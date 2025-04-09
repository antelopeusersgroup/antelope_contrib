/***************************************************************************
 * network.c
 *
 * Network communication routines for SeedLink
 *
 * Originally based on the SeedLink interface of the modified Comserv in
 * SeisComP written by Andres Heinloo.
 *
 * This file is part of the SeedLink Library.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * Copyright (C) 2024:
 * @author Chad Trabant, EarthScope Data Services
 ***************************************************************************/

#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "libslink.h"

#include "mbedtls/include/mbedtls/debug.h"
#include "mbedtls/include/mbedtls/net_sockets.h"
#include "mbedtls/include/mbedtls/ssl.h"
#include "mbedtls/include/mbedtls/entropy.h"
#include "mbedtls/include/mbedtls/ctr_drbg.h"
#include "mbedtls/include/mbedtls/error.h"

/* Some portable macros to test error conditions */
#if defined(SLP_WIN)
#define IS_EINTR(X) ((X) == SOCKET_ERROR && WSAGetLastError () == WSAEINTR)
#define IS_EWOULDBLOCK() (WSAGetLastError () == WSAEWOULDBLOCK)
#define IS_ECONNRESET() (WSAGetLastError () == WSAECONNRESET)
#else
#define IS_EINTR(X) ((X) == -1 && errno == EINTR)
#define IS_EWOULDBLOCK() (errno == EWOULDBLOCK || errno == EAGAIN)
#define IS_ECONNRESET() (errno == ECONNRESET)
#endif

/* Functions only used in this source file */
static int sayhello_int (SLCD *slconn);
static int batchmode_int (SLCD *slconn);
static int negotiate_uni_v3 (SLCD *slconn);
static int negotiate_multi_v3 (SLCD *slconn);
static int negotiate_v4 (SLCD *slconn);
static int sockstartup_int (void);
static int sockconnect_int (SOCKET sock, struct sockaddr *inetaddr, int addrlen);
static int socknoblock_int (SOCKET sock);
static int setsocktimeo_int (SOCKET socket, int timeout);
static int load_ca_certs (SLCD *slconn);

/* Data structures for TLS connection context */
/* @cond */
typedef struct TLSCTX
{
  mbedtls_net_context server_fd;
  mbedtls_ssl_context ssl;
  mbedtls_ssl_config conf;
  mbedtls_ctr_drbg_context ctr_drbg;
  mbedtls_entropy_context entropy;
  mbedtls_x509_crt cacert;
} TLSCTX;
/* @endcond */

/* Debug output for TLS to stderr */
void
tls_debug (void *ctx, int level, const char *file, int line, const char *str)
{
  ((void)level);
  ((void)ctx);

  fprintf (stderr, "%s:%04d: %s", file, line, str);
}

/* Configure TLS on connection, return 0 on success */
int
tls_configure (SLCD *slconn, const char *nodename)
{
  TLSCTX *tlsctx         = NULL;
  const char *seed_value = (slconn->clientname) ? slconn->clientname : "SeedLink Client";
  char *evalue           = NULL;
  uint32_t flags;
  int debug_level = 0;
  int ret;
  psa_status_t status;

  sl_log_r (slconn, 1, 1, "[%s] Configuring TLS\n", slconn->sladdr);

  /* Allocate TLS data structure context */
  if ((slconn->tlsctx = (TLSCTX *)malloc (sizeof (TLSCTX))) == NULL)
  {
    sl_log_r (slconn, 2, 0, "cannot allocate memory for TLS context\n");
    return -1;
  }

  tlsctx = (TLSCTX *)slconn->tlsctx;

  /* Set debug level from environment variable if set */
  if ((evalue = getenv ("LIBSLINK_TLS_DEBUG")) != NULL)
  {
    debug_level = (int)strtol (evalue, NULL, 10);
    sl_log_r (slconn, 1, 0, "[%s] configuring debug level %d (from LIBSLINK_TLS_DEBUG)\n",
              slconn->sladdr, debug_level);

    if (debug_level > 0)
    {
      mbedtls_debug_set_threshold (debug_level);
    }
  }

  tlsctx->server_fd.fd = slconn->link;
  mbedtls_ssl_init (&tlsctx->ssl);
  mbedtls_ssl_config_init (&tlsctx->conf);
  mbedtls_x509_crt_init (&tlsctx->cacert);
  mbedtls_ctr_drbg_init (&tlsctx->ctr_drbg);
  mbedtls_entropy_init (&tlsctx->entropy);

  if ((status = psa_crypto_init ()) != PSA_SUCCESS)
  {
    sl_log_r (slconn, 2, 0, "[%s] Failed to initialize PSA Crypto implementation: %d\n",
              slconn->sladdr, (int)status);
    return -1;
  }

  if ((ret = mbedtls_ctr_drbg_seed (&tlsctx->ctr_drbg, mbedtls_entropy_func,
                                    &tlsctx->entropy,
                                    (const unsigned char *)seed_value,
                                    strlen (seed_value))) != 0)
  {
    sl_log_r (slconn, 2, 0, "mbedtls_ctr_drbg_seed() returned %d\n", ret);
    return -1;
  }

  /* Load Certificate Authority certificates */
  if (load_ca_certs(slconn) == 0)
  {
    sl_log_r (slconn, 1, 0, "[%s] No trusted CA certificates found, connections may not work\n", slconn->sladdr);
    sl_log_r (slconn, 1, 0, "[%s]   CA cert locations can be specified with the following environment variables:\n", slconn->sladdr);
    sl_log_r (slconn, 1, 0, "[%s]   LIBSLINK_TLS_CERT_FILE and LIBSLINK_TLS_CERT_PATH\n", slconn->sladdr);
  }

  if ((ret = mbedtls_ssl_config_defaults (&tlsctx->conf,
                                          MBEDTLS_SSL_IS_CLIENT,
                                          MBEDTLS_SSL_TRANSPORT_STREAM,
                                          MBEDTLS_SSL_PRESET_DEFAULT)) != 0)
  {
    sl_log_r (slconn, 2, 0, "[%s] mbedtls_ssl_config_defaults() returned %d\n",
              slconn->sladdr, ret);
    return -1;
  }

  mbedtls_ssl_conf_authmode (&tlsctx->conf, MBEDTLS_SSL_VERIFY_OPTIONAL);
  mbedtls_ssl_conf_ca_chain (&tlsctx->conf, &tlsctx->cacert, NULL);
  mbedtls_ssl_conf_rng (&tlsctx->conf, mbedtls_ctr_drbg_random, &tlsctx->ctr_drbg);
  mbedtls_ssl_conf_dbg (&tlsctx->conf, tls_debug, NULL);

  if ((ret = mbedtls_ssl_setup (&tlsctx->ssl, &tlsctx->conf)) != 0)
  {
    sl_log_r (slconn, 2, 0, "[%s] mbedtls_ssl_setup() returned %d\n",
              slconn->sladdr, ret);
    return -1;
  }

  if ((ret = mbedtls_ssl_set_hostname (&tlsctx->ssl, nodename)) != 0)
  {
    sl_log_r (slconn, 2, 0, "[%s] mbedtls_ssl_set_hostname() returned %d\n",
              slconn->sladdr, ret);
    return -1;
  }

  mbedtls_ssl_set_bio (&tlsctx->ssl, &tlsctx->server_fd,
                       mbedtls_net_send, mbedtls_net_recv, NULL);

  sl_log_r (slconn, 1, 2, "[%s] Starting TLS handshake\n", slconn->sladdr);

  while ((ret = mbedtls_ssl_handshake (&tlsctx->ssl)) != 0)
  {
    if (ret != MBEDTLS_ERR_SSL_WANT_READ &&
        ret != MBEDTLS_ERR_SSL_WANT_WRITE &&
        ret != MBEDTLS_ERR_SSL_CRYPTO_IN_PROGRESS)
    {
      sl_log_r (slconn, 2, 0, " [%s] mbedtls_ssl_handshake() returned -0x%x\n",
                slconn->sladdr, (unsigned int)-ret);
      return -1;
    }

    /* Wait for socket availability for 1 second */
    sl_poll (slconn, 1, 1, 1000);
  }

  sl_log_r (slconn, 1, 2, "[%s] Verifying TLS server certificate\n", slconn->sladdr);

  if ((flags = mbedtls_ssl_get_verify_result (&tlsctx->ssl)) != 0)
  {
    /* Check if continuing despite cerfication failure */
    if ((evalue = getenv ("LIBSLINK_CERT_UNVERIFIED_OK")) != NULL)
    {
      sl_log_r (slconn, 1, 0, "[%s] Continuing with unverified cert [LIBSLINK_CERT_UNVERIFIED_OK]\n",
                slconn->sladdr);
    }
    else
    {
      char vrfy_buf[512];

      sl_log_r (slconn, 2, 0, "[%s] Certificate verification failed\n", slconn->sladdr);

      mbedtls_x509_crt_verify_info (vrfy_buf, sizeof (vrfy_buf), "  ! ", flags);
      sl_log_r (slconn, 2, 0, "VERIFY INFO: %s\n", vrfy_buf);

      sl_log_r (slconn, 1, 0, "[%s] Connection refused due to certificate verification failure\n",
                slconn->sladdr);
      return -1;
    }
  }

  sl_log_r (slconn, 1, 1, "[%s] TLS connection established\n", slconn->sladdr);

  return 0;
} /* End of TLS configuration */

/**********************************************************************/ /**
 * @brief Connect to a SeedLink server
 *
 * The main SeedLink client entry point is \a sl_collect(), you probably
 * should not call this directly.
 *
 * The SLCD.sladdr value is expected to be in 'host:port' format.  Either
 * the host, port or both are optional.  If the host is not specified
 * 'localhost' is assumed.  If the port is not specified '18000' is assumed.
 *
 * If \a sayhello is true, commands will be sent to the server
 * to determine server features and set features supported by the
 * library.  This includes upgrading the protocol to the maximum
 * version supported by both server and client.  Unless you wish to do
 * low level negotiation independently, always set this to 1.
 *
 * If a permanent error is detected (invalid port specified) the
 * slconn->terminate flag will be set so the sl_collect() family of
 * routines will not continue trying to connect.
 *
 * @param slconn The ::SLCD connection to connect
 * @param sayhello If true, send HELLO command to server
 *
 * @returns -1 on errors otherwise the socket descriptor created.
 *
 * @sa sl_collect()
 ***************************************************************************/
SOCKET
sl_connect (SLCD *slconn, int sayhello)
{
  struct addrinfo *addr0 = NULL;
  struct addrinfo *addr  = NULL;
  struct addrinfo hints;
  int valueone = 1;
  int sockstat;
  int timeout;

  if (!slconn)
    return -1;

  if (slconn->sladdr == NULL)
  {
    sl_log_r (slconn, 2, 0, "no server address specified\n");
    return -1;
  }

  /* Parse server host and port if needed */
  if (slconn->slhost == NULL || slconn->slport == NULL)
  {
    if (sl_set_serveraddress (slconn, slconn->sladdr))
    {
      sl_log_r (slconn, 2, 0, "server address not in in recognized format: %s\n",
                slconn->sladdr);
      return -1;
    }
  }

  if (sockstartup_int ())
  {
    sl_log_r (slconn, 2, 0, "could not initialize network sockets\n");
    return -1;
  }

  /* Resolve for either IPv4 or IPv6 (PF_UNSPEC) for a TCP stream (SOCK_STREAM) */
  memset (&hints, 0, sizeof (hints));
  hints.ai_family   = PF_UNSPEC;
  hints.ai_socktype = SOCK_STREAM;

  /* Resolve server address */
  if (getaddrinfo (slconn->slhost, slconn->slport, &hints, &addr0))
  {
    sl_log_r (slconn, 2, 0, "cannot resolve hostname %s\n", slconn->slhost);
    return -1;
  }

  /* Traverse address results trying to connect */
  slconn->link = -1;
  for (addr = addr0; addr != NULL; addr = addr->ai_next)
  {
    /* Create socket */
    if ((slconn->link = socket (addr->ai_family, addr->ai_socktype, addr->ai_protocol)) < 0)
    {
      continue;
    }

    /* Set socket I/O timeouts if possible */
    if (slconn->iotimeout)
    {
      timeout = (slconn->iotimeout > 0) ? slconn->iotimeout : -slconn->iotimeout;

      if (setsocktimeo_int (slconn->link, timeout) == 1)
      {
        /* Negate timeout to indicate socket timeouts are set */
        slconn->iotimeout = -timeout;
      }
    }

    /* Connect socket */
    if ((sockconnect_int (slconn->link, addr->ai_addr, addr->ai_addrlen)))
    {
      sl_disconnect (slconn);
      continue;
    }

    break;
  }

  if (slconn->link < 0)
  {
    sl_log_r (slconn, 2, 0, "[%s] Cannot connect: %s\n", slconn->sladdr, sl_strerror ());
    sl_disconnect (slconn);
    freeaddrinfo (addr0);
    return -1;
  }

  freeaddrinfo (addr0);

  if (slconn->iotimeout < 0)
  {
    sl_log_r (slconn, 1, 2, "[%s] using system socket timeouts\n", slconn->sladdr);
  }

  /* Set non-blocking IO */
  if (socknoblock_int (slconn->link))
  {
    sl_log_r (slconn, 2, 0, "[%s] Error setting socket to non-blocking\n", slconn->sladdr);
    sl_disconnect (slconn);
    return -1;
  }

  /* Wait up to 10 seconds for the socket to be connected */
  if ((sockstat = sl_poll (slconn, 0, 1, 10000)) <= 0)
  {
    if (sockstat < 0 && slconn->terminate == 0)
    {
      sl_log_r (slconn, 2, 1, "[%s] socket connect error\n", slconn->sladdr);
    }
    else if (sockstat == 0)
    {
      sl_log_r (slconn, 2, 1, "[%s] socket connect time-out (10s)\n",
                slconn->sladdr);
    }

    sl_disconnect (slconn);
    return -1;
  }

  if (slconn->terminate) /* Check that terminate has not been requested */
  {
    sl_disconnect (slconn);
    return -1;
  }

  sl_log_r (slconn, 1, 1, "[%s] network socket connected\n", slconn->sladdr);

  /* Set the SO_KEEPALIVE socket option, although not really useful */
  if (setsockopt (slconn->link, SOL_SOCKET, SO_KEEPALIVE, (void *)&valueone, sizeof (valueone)) < 0)
    sl_log_r (slconn, 1, 1, "[%s] cannot set SO_KEEPALIVE socket option\n",
              slconn->sladdr);

  /* Make sure enabled batch mode is in an initial state */
  if (slconn->batchmode)
    slconn->batchmode = 1;

  /* Configure TLS */
  if (slconn->tls && tls_configure (slconn, slconn->slhost))
  {
    sl_log_r (slconn, 2, 0, "[%s] error configuring TLS\n", slconn->sladdr);
    sl_disconnect (slconn);
    return -1;
  }

  /* Everything should be connected, get capabilities, etc. */
  if (sayhello)
  {
    if (sayhello_int (slconn) == -1)
    {
      sl_disconnect (slconn);
      return -1;
    }
  }

  return slconn->link;
} /* End of sl_connect() */

/**********************************************************************/ /**
 * @brief Configure and negotiate data selections with a SeedLink server
 *
 * The main SeedLink client entry point is \a sl_collect(), you probably
 * should not call this directly.
 *
 * Negotiation will be either uni or multi-station depending on the value
 * of SLCD.multistation.
 *
 * @param slconn The ::SLCD connection to configure
 *
 * @returns -1 on errors, otherwise returns the link descriptor.
 *
 * @sa sl_collect()
 ***************************************************************************/
SOCKET
sl_configlink (SLCD *slconn)
{
  SOCKET ret = slconn->link;

  if (slconn->protocol & SLPROTO40)
  {
    ret = negotiate_v4 (slconn);
  }
  else if (slconn->protocol & SLPROTO3X)
  {
    if (slconn->multistation)
    {
      ret = negotiate_multi_v3 (slconn);
    }
    else
    {
      ret = negotiate_uni_v3 (slconn);
    }
  }

  return ret;
} /* End of sl_configlink() */

/**********************************************************************/ /**
 * @brief Send a request for the specified INFO level
 *
 * The main SeedLink client entry point is \a sl_collect(), you probably
 * should not call this directly.
 *
 * The verbosity level can be specified, allowing control of when the
 * request should be logged.
 *
 * @param slconn Send INFO to the connection associated with the ::SLCD
 * @param infostr The INFO command to request
 * @param verbose The verbosity level to use when logging the request
 *
 * @returns -1 on errors, otherwise the socket descriptor.
 *
 * @sa sl_collect()
 ***************************************************************************/
int
sl_send_info (SLCD *slconn, const char *infostr, int verbose)
{
  char sendstr[100]; /* A buffer for command strings */

  snprintf (sendstr, sizeof (sendstr), "INFO %s\r\n", infostr);

  sl_log_r (slconn, 1, verbose, "[%s] requesting INFO %s\n",
            slconn->sladdr, infostr);

  if (sl_senddata (slconn, (void *)sendstr, strlen (sendstr),
                   slconn->sladdr, (void *)NULL, 0) < 0)
  {
    sl_log_r (slconn, 2, 0, "[%s] error sending INFO request\n", slconn->sladdr);
    return -1;
  }

  return slconn->link;
} /* End of sl_send_info() */

/**********************************************************************/ /**
 * @brief Close a connction to a SeedLink server
 *
 * The network socket associated with ::SLCD is closed and all memory
 * allocated for the TLS context is freed.
 *
 * @param slconn Close the connection associated with the ::SLCD
 *
 * @returns -1, historically used to set the old descriptor
 ***************************************************************************/
int
sl_disconnect (SLCD *slconn)
{
  if (slconn->link != -1)
  {
#if defined(SLP_WIN)
    return closesocket (slconn->link);
#else
    return close (slconn->link);
#endif

    slconn->link = -1;

    sl_log_r (slconn, 1, 1, "[%s] network socket closed\n", slconn->sladdr);
  }

  if (slconn->tlsctx)
  {
    TLSCTX *tlsctx = (TLSCTX *)slconn->tlsctx;

    mbedtls_x509_crt_free (&tlsctx->cacert);
    mbedtls_ssl_free (&tlsctx->ssl);
    mbedtls_ssl_config_free (&tlsctx->conf);
    mbedtls_ctr_drbg_free (&tlsctx->ctr_drbg);
    mbedtls_entropy_free (&tlsctx->entropy);
    mbedtls_psa_crypto_free();

    free (slconn->tlsctx);
    slconn->tlsctx = NULL;
  }

  return -1;
} /* End of sl_disconnect() */


/**********************************************************************/ /**
 * @brief Connect to a SeedLink server, issue HELLO and parse response
 *
 * This function serves as a convienence function to "ping" a SeedLink
 * server and get the server ID and site/organization strings.  This
 * verifies that the connection is possible and that the most basic
 * SeedLink command is suppported.
 *
 * The server ID and site/organization strings in the response are
 * copied into serverid and site strings which should have 100 bytes
 * of space each.
 *
 * @param[in] slconn The ::SLCD connection to ping
 * @param[out] serverid Destination for the server ID string, NULL if undesired
 * @param[out] site Destination for the site/organization string, NULL if undesired
 *
 * @retval  0  Success
 * @retval -1  Connection opened but invalid response to 'HELLO'
 * @retval -2  Could not open network connection
 ***************************************************************************/
int
sl_ping (SLCD *slconn, char *serverid, char *site)
{
  char sendstr[100] = {0}; /* A buffer for command strings */
  char servstr[100] = {0}; /* The remote server ident */
  char sitestr[100] = {0}; /* The site/data center ident */

  /* Open network connection to server */
  if (sl_connect (slconn, 0) == -1)
  {
    sl_log_r (slconn, 2, 1, "Could not connect to server\n");
    return -2;
  }

  /* Send HELLO */
  snprintf (sendstr, sizeof (sendstr), "HELLO\r\n");

  sl_log_r (slconn, 1, 2, "[%s] sending: %.*s\n", slconn->sladdr,
            (int)strcspn (sendstr, "\r\n"), sendstr);

  sl_senddata (slconn, (void *)sendstr, strlen (sendstr), slconn->sladdr,
               NULL, 0);

  /* Recv the two lines of response */
  if (sl_recvresp (slconn, (void *)servstr, (size_t)sizeof (servstr) - 1,
                   sendstr, slconn->sladdr) < 0)
  {
    return -1;
  }

  if (sl_recvresp (slconn, (void *)sitestr, (size_t)sizeof (sitestr) - 1,
                   sendstr, slconn->sladdr) < 0)
  {
    return -1;
  }

  /* Copy the response strings into the supplied strings */
  if (serverid)
    strcpy (serverid, servstr);

  if (site)
    strcpy (site, sitestr);

  slconn->link = sl_disconnect (slconn);

  return 0;
} /* End of sl_ping() */

/**********************************************************************/ /**
 * @brief Send specified data to a SeedLink server
 *
 * The main SeedLink client entry point is \a sl_collect(), you probably
 * should not call this directly.
 *
 * Send \a buflen bytes from \a buffer to server.  The \a ident value is
 * a string to include in error messages for identification, usually
 * the address of the remote server.  If \a resp is not NULL then read
 * up to \a resplen bytes into \a resp after sending \a buffer.  This is
 * only designed for small pieces of data, specifically the server
 * responses to commands terminated by `"\r\n"`.
 *
 * @param slconn The ::SLCD connection to send data
 * @param buffer The data to send
 * @param buflen The length of the data to send
 * @param ident A string to include in error messages for identification
 * @param resp A buffer to store the response
 * @param resplen The length of the response buffer
 *
 * @retval  -1 on error
 * @retval   0 for success when no response requested
 * @retval >=0 the number of bytes read if response requested
 *
 * @sa sl_collect()
 ***************************************************************************/
int
sl_senddata (SLCD *slconn, void *buffer, size_t buflen,
             const char *ident, void *resp, int resplen)
{
  int bytesread = 0; /* bytes read into resp */
  int64_t byteswritten;

  if (slconn->tlsctx != NULL)
  {
    TLSCTX *tlsctx = (TLSCTX *)slconn->tlsctx;

    while ((byteswritten = mbedtls_ssl_write (&tlsctx->ssl, buffer, buflen)) <= 0)
    {
      if (byteswritten != MBEDTLS_ERR_SSL_WANT_READ &&
          byteswritten != MBEDTLS_ERR_SSL_WANT_WRITE &&
          byteswritten != MBEDTLS_ERR_SSL_CRYPTO_IN_PROGRESS)
      {
        break;
      }

      /* Wait for socket write availability for 1 second */
      sl_poll (slconn, 0, 1, 1000);
    }
  }
  else
  {
    byteswritten = send (slconn->link, buffer, buflen, 0);
  }

  if (byteswritten < 0)
  {
    sl_log_r (slconn, 2, 0, "[%s] error sending '%.*s'\n",
              ident,
              (int)strcspn ((char *)buffer, "\r\n"),
              (char *)buffer);
    return -1;
  }

  /* If requested collect the response */
  if (resp != NULL)
  {
    memset (resp, 0, resplen);
    bytesread = sl_recvresp (slconn, resp, resplen, buffer, ident);
  }

  return bytesread;
} /* End of sl_senddata() */

/**********************************************************************/ /**
 * @brief Receive data from a SeedLink server
 *
 * The main SeedLink client entry point is \a sl_collect(), you probably
 * should not call this directly.
 *
 * Read \a maxbytes data from connection into a specified \a buffer.
 * The \a ident is a string to be included in error messages for
 * identification, usually the address of the remote server.
 *
 * @param slconn The ::SLCD connection to receive data from
 * @param buffer The buffer to store the received data
 * @param maxbytes The maximum number of bytes to read
 * @param ident A string to include in error messages for identification
 *
 * @retval -1 on error
 * @retval 0  for no available data
 * @retval >0 the number of bytes read on success.
 *
 * @sa sl_collect()
 ***************************************************************************/
int64_t
sl_recvdata (SLCD *slconn, void *buffer, size_t maxbytes,
             const char *ident)
{
  int64_t bytesread = 0;

  if (buffer == NULL)
  {
    return -1;
  }

  if (slconn->tlsctx != NULL)
  {
    TLSCTX *tlsctx = (TLSCTX *)slconn->tlsctx;

    do
    {
      bytesread = mbedtls_ssl_read (&tlsctx->ssl, buffer, maxbytes);

      if (bytesread == MBEDTLS_ERR_SSL_RECEIVED_NEW_SESSION_TICKET)
      {
        sl_log_r (slconn, 1, 3, "[%s] TLS 1.3 New Session Ticket received\n", slconn->sladdr);
        continue;
      }

      break;
    } while (1);
  }
  else
  {
    bytesread = recv (slconn->link, buffer, maxbytes, 0);
  }

  if (bytesread == 0) /* Indicates TCP FIN or EOF, connection closed */
  {
    /* Set termination flag to initial state if connection was closed */
    slconn->terminate = 1;
    return 0;
  }
  else if (bytesread < 0)
  {
    /* Return 0 when no data for nonblocking IO */
    if ((slconn->tlsctx && (bytesread == MBEDTLS_ERR_SSL_WANT_READ ||
                            bytesread == MBEDTLS_ERR_SSL_WANT_WRITE)) ||
        IS_EWOULDBLOCK ())
    {
      return 0;
    }

    /* Set termination flag to initial state on connection reset */
    if ((slconn->tlsctx && bytesread == MBEDTLS_ERR_NET_CONN_RESET) ||
        IS_ECONNRESET ())
    {
      slconn->terminate = 1;
    }
    /* Handle all other errors */
    else
    {
      if (slconn->tlsctx)
      {
        char error_message[100];
        mbedtls_strerror (bytesread, error_message, sizeof (error_message));

        sl_log_r (slconn, 2, 0, "[%s] %s(): %" PRId64 " (-0x%" PRIx64 "): %.*s\n",
                  (ident) ? ident : "", __func__, bytesread, -bytesread,
                  (int)sizeof (error_message), error_message);

        return -1;
      }
      else
      {
        sl_log_r (slconn, 2, 0, "[%s] %s(): %" PRId64 ": %s\n",
                  (ident) ? ident : "", __func__, bytesread, sl_strerror ());

        return -1;
      }
    }
  }

  return (bytesread < 0) ? -1 : bytesread;
} /* End of sl_recvdata() */

/**********************************************************************/ /**
 * @brief Receive a response to a command from a SeedLink server
 *
 * The main SeedLink client entry point is \a sl_collect(), you probably
 * should not call this directly.
 *
 * To receive a response to a command read one byte at a time until
 * `"\r\n"` or up to \a maxbytes is received and written into a
 * specified \a buffer.  The function will wait up to 30 seconds for a
 * response to be recv'd.  \a command is a string to be included in
 * error messages indicating which command the response is
 * for. \a ident is a string to be included in error messages for
 * identification, usually the address of the remote server.
 *
 * It should not be assumed that the populated buffer contains a
 * terminated string.
 *
 * @param slconn The ::SLCD connection to receive data from
 * @param buffer The buffer to store the received data
 * @param maxbytes The maximum number of bytes to read
 * @param command A string to include in error messages indicating the command
 * @param ident A string to include in error messages for identification
 *
 * @retval -1  on error/EOF
 * @retval >=0 the number of bytes read on success.
 *
 * @sa sl_collect()
 ***************************************************************************/
int
sl_recvresp (SLCD *slconn, void *buffer, size_t maxbytes,
             const char *command, const char *ident)
{
  size_t bytesread = 0; /* total bytes read */

  int recvret = 0;     /* return from sl_recvdata */
  int ackcnt  = 0;     /* counter for the read loop */
  int ackpoll = 50000; /* poll at 0.05 seconds for reading */

  if (buffer == NULL)
  {
    return -1;
  }

  /* Clear the receiving buffer */
  memset (buffer, 0, maxbytes);

  /* Recv a byte at a time and wait up to 30 seconds for a response */
  while (bytesread < maxbytes)
  {
    recvret = sl_recvdata (slconn, (char *)buffer + bytesread, 1, ident);

    /* Trap door for termination */
    if (slconn->terminate)
    {
      return 0;
    }

    if (recvret > 0)
    {
      bytesread += recvret;
    }
    else if (recvret < 0)
    {
      sl_log_r (slconn, 2, 0, "[%s] bad response to '%.*s'\n",
                ident,
                (int)strcspn (command, "\r\n"),
                command);
      return -1;
    }

    /* Done if '\r\n' is recv'd */
    if (bytesread >= 2 &&
        *(char *)((char *)buffer + bytesread - 2) == '\r' &&
        *(char *)((char *)buffer + bytesread - 1) == '\n')
    {
      return (int)bytesread;
    }

    /* Trap door if 30 seconds has elapsed, (ackpoll x 600) */
    if (ackcnt > 600)
    {
      sl_log_r (slconn, 2, 0, "[%s] timeout waiting for response to '%.*s'\n",
                ident,
                (int)strcspn (command, "\r\n"),
                command);
      return -1;
    }

    /* Delay if no data received */
    if (recvret == 0)
    {
      sl_usleep (ackpoll);
      ackcnt++;
    }
  }

  return (int)bytesread;
} /* End of sl_recvresp() */

/**********************************************************************/ /**
 * @brief Poll the network connection associated with the ::SLCD
 *
 * Poll the connected socket for read and/or write ability using select()
 * for a specified amount of time.
 *
 * The timeout is specified in milliseconds.
 *
 * Interrupted select() calls are retried until the timeout expires
 * unless the connection termination flag is set (slconn->terminate).
 *
 * @param slconn The ::SLCD connection to poll
 * @param readability If true, poll for readability
 * @param writability If true, poll for writability
 * @param timeout_ms The timeout in milliseconds
 *
 * @retval >=1 : success
 * @retval   0 : if time-out expires
 * @retval  <0 : errors
 ***************************************************************************/
int
sl_poll (SLCD *slconn, int readability, int writability, int timeout_ms)
{
  fd_set readset;
  fd_set writeset;
  struct timeval to;
  int retries = 0;
  int ret;

  if (!slconn)
    return -1;

  if (timeout_ms < 0)
    return -1;

  FD_ZERO (&readset);
  FD_ZERO (&writeset);

  if (readability)
    FD_SET (slconn->link, &readset);

  if (writability)
    FD_SET (slconn->link, &writeset);

  to.tv_sec  = timeout_ms / 1000;
  to.tv_usec = (timeout_ms % 1000) * 1000;

  do
  {
    ret = select (slconn->link + 1, &readset, &writeset, NULL, &to);

    /* Limit retries to 100 */
    if (retries++ > 100)
      break;
  } while (IS_EINTR (ret) && slconn->terminate == 0);

  return ret;
}

/***************************************************************************
 * sayhello_int:
 *
 * Send the HELLO and other commands to determine server capabilities.
 *
 * The connection is promoted to the highest version supported by both
 * server and client.
 *
 * Returns -1 on errors, 0 on success.
 ***************************************************************************/
static int
sayhello_int (SLCD *slconn)
{
  int ret = 0;
  char sendstr[1024]; /* A buffer for command strings */
  char servstr[200];  /* The remote server ident */
  char sitestr[200];  /* The site/data center ident */
  char *capptr;       /* Pointer to capabilities flags */
  char capflag   = 0; /* CAPABILITIES command is supported by server */
  size_t servcnt = 0;
  size_t sitecnt = 0;

  uint8_t server_major = 0;
  uint8_t server_minor = 0;

  int bytesread = 0;
  char readbuf[1024];

  /* Send HELLO */
  snprintf (sendstr, sizeof (sendstr), "HELLO\r\n");

  sl_log_r (slconn, 1, 2, "[%s] sending: %.*s\n", slconn->sladdr,
            (int)strcspn (sendstr, "\r\n"), sendstr);

  ret = sl_senddata (slconn, (void *)sendstr, strlen (sendstr), slconn->sladdr,
                     NULL, 0);

  if (ret < 0)
  {
    sl_log_r (slconn, 2, 0, "[%s] error sending HELLO\n", slconn->sladdr);
    return -1;
  }

  /* Recv the two lines of response: server ID and site installation ID */
  if (sl_recvresp (slconn, (void *)servstr, (size_t)sizeof (servstr),
                   sendstr, slconn->sladdr) < 0)
  {
    return -1;
  }

  if (sl_recvresp (slconn, (void *)sitestr, (size_t)sizeof (sitestr),
                   sendstr, slconn->sladdr) < 0)
  {
    return -1;
  }

  if (slconn->terminate)
  {
    sl_log_r (slconn, 2, 0, "[%s] Connection closed\n", slconn->sladdr);
    return -1;
  }

  /* Terminate on first "\r" character or at one character before end of buffer */
  servcnt = strcspn (servstr, "\r");
  if (servcnt > (sizeof (servstr) - 2))
  {
    servcnt = (sizeof (servstr) - 2);
  }
  servstr[servcnt] = '\0';

  sitecnt = strcspn (sitestr, "\r");
  if (sitecnt > (sizeof (sitestr) - 2))
  {
    sitecnt = (sizeof (sitestr) - 2);
  }
  sitestr[sitecnt] = '\0';

  /* Search for capabilities flags in server ID by looking for "::"
   * The expected format of the complete server ID is:
   * "SeedLink v#.# <optional text> <:: optional capability flags>"
   */
  capptr = strstr (servstr, "::");
  if (capptr)
  {
    /* Truncate server ID portion of string */
    *capptr = '\0';

    /* Move pointer to beginning of flags */
    capptr += 2;

    /* Move capptr up to first non-space character */
    while (*capptr == ' ')
      capptr++;

    if (slconn->capabilities)
      free (slconn->capabilities);
    if (slconn->caparray)
      free (slconn->caparray);

    slconn->capabilities = strdup(capptr);
    slconn->caparray = NULL;
  }

  /* Report server details */
  sl_log_r (slconn, 1, 1, "[%s] connected to: %s\n", slconn->sladdr, servstr);
  sl_log_r (slconn, 1, 1, "[%s] organization: %s\n", slconn->sladdr, sitestr);

  /* Validate that the server ID starts with "SeedLink" */
  if (strncasecmp (servstr, "SEEDLINK", 8))
  {
    sl_log_r (slconn, 2, 0,
              "[%s] unrecognized server identification: '%s'\n",
              slconn->sladdr, servstr);
    return -1;
  }

  /* Check capability flags included in HELLO response */
  capptr = slconn->capabilities;
  while (capptr && *capptr)
  {
    while (*capptr == ' ')
      capptr++;

    if (strncmp (capptr, "SLPROTO:", 8) == 0)
    {
      ret = sscanf (capptr, "SLPROTO:%" SCNu8 ".%" SCNu8,
                    &server_major,
                    &server_minor);

      if (ret < 1)
      {
        sl_log_r (slconn, 1, 1,
                  "[%s] could not parse protocol version from SLPROTO flag: %s\n",
                  slconn->sladdr, capptr);
      }
      else if (server_major == 3)
      {
        slconn->server_protocols |= SLPROTO3X;
      }
      else if (server_major == 4 && server_minor == 0)
      {
        slconn->server_protocols |= SLPROTO40;
      }
      /* Fallback to protocol v4.0 for unrecognized (future) minor versions */
      else if (server_major == 4)
      {
        sl_log_r (slconn, 1, 1, "[%s] using protocol v4.0 instead of (unsupported) v%d.%d\n",
                  slconn->sladdr, server_major, server_minor);

        slconn->server_protocols |= SLPROTO40;
      }

      capptr += 8;
    }
    else if (strncmp (capptr, "CAP", 3) == 0)
    {
      capptr += 3;
      capflag = 1;
    }

    capptr++;
  }

  /* Default to SeedLink 3.x if no protocols advertised by server are recognized */
  if (slconn->server_protocols == 0)
  {
    sl_log_r (slconn, 1, 1, "[%s] no recognized protocol version, defaulting to 3.x\n", slconn->sladdr);

    slconn->server_protocols = SLPROTO3X;
  }

  /* Promote protocol if supported by server */
  if (slconn->server_protocols & SLPROTO40)
  {
    snprintf (sendstr, sizeof (sendstr), "SLPROTO 4.0\r\n");

    /* Send SLPROTO and recv response */
    sl_log_r (slconn, 1, 2, "[%s] sending: %.*s\n", slconn->sladdr,
              (int)strcspn (sendstr, "\r\n"), sendstr);

    bytesread = sl_senddata (slconn, (void *)sendstr, strlen (sendstr), slconn->sladdr,
                             readbuf, sizeof (readbuf));

    if (bytesread < 0)
    { /* Error from sl_senddata() */
      return -1;
    }

    /* Check response to SLPROTO */
    if (!strncmp (readbuf, "OK\r", 3) && bytesread >= 4)
    {
      sl_log_r (slconn, 1, 2, "[%s] %.*s accepted\n", slconn->sladdr,
                (int)strcspn (sendstr, "\r\n"), sendstr);
    }
    else if (!strncmp (readbuf, "ERROR", 5) && bytesread >= 6)
    {
      char *cp = readbuf + (bytesread-1);

      /* Trim space, \r, and \n while terminating response string */
      while (*cp == ' ' || *cp == '\r' || *cp == '\n')
        *cp-- = '\0';

      sl_log_r (slconn, 1, 2, "[%s] %.*s not accepted: %s\n", slconn->sladdr,
                (int)strcspn (sendstr, "\r\n"), sendstr,
                readbuf + 6);
      return -1;
    }
    else
    {
      sl_log_r (slconn, 2, 0,
                "[%s] invalid response to SLPROTO command: %.*s\n", slconn->sladdr,
                bytesread, readbuf);
      return -1;
    }

    slconn->protocol = SLPROTO40;
  }
  /* Otherwise use SeedLink 3.x if supported */
  else if (slconn->server_protocols & SLPROTO3X)
  {
    slconn->protocol = SLPROTO3X;
  }
  else
  {
    sl_log_r (slconn, 2, 0, "[%s] no supported protocol found\n", slconn->sladdr);
    sl_log_r (slconn, 1, 1, "[%s] server ID: %s\n", slconn->sladdr, servstr);
    sl_log_r (slconn, 1, 1, "[%s] capabilities: %s\n", slconn->sladdr,
              (slconn->capabilities) ? slconn->capabilities : "");
    return -1;
  }

  /* Report server capabilities */
  if (slconn->capabilities)
    sl_log_r (slconn, 1, 1, "[%s] server capabilities: %s\n", slconn->sladdr,
              (slconn->capabilities) ? slconn->capabilities : "");

  /* Send CAPABILITIES flags if supported by server and protocol 3.x */
  if (capflag && slconn->protocol & SLPROTO3X)
  {
    char *term1, *term2;
    char *extreply = 0;

    /* Send EXTREPLY capability flag */
    snprintf (sendstr, sizeof (sendstr), "CAPABILITIES EXTREPLY\r\n");

    /* Send CAPABILITIES and recv response */
    sl_log_r (slconn, 1, 2, "[%s] sending: %.*s\n", slconn->sladdr,
              (int)strcspn (sendstr, "\r\n"), sendstr);

    bytesread = sl_senddata (slconn, (void *)sendstr, strlen (sendstr), slconn->sladdr,
                             readbuf, sizeof (readbuf));

    if (bytesread < 0)
    { /* Error from sl_senddata() */
      return -1;
    }

    /* Search for 2nd "\r" indicating extended reply message present */
    extreply = 0;
    if ((term1 = memchr (readbuf, '\r', bytesread)))
    {
      if ((term2 = memchr (term1 + 1, '\r', bytesread - (readbuf - term1) - 1)))
      {
        *term2   = '\0';
        extreply = term1 + 1;
      }
    }

    /* Check response to CAPABILITIES */
    if (!strncmp (readbuf, "OK\r", 3) && bytesread >= 4)
    {
      sl_log_r (slconn, 1, 2, "[%s] capabilities OK %s%s%s\n", slconn->sladdr,
                (extreply) ? "{" : "", (extreply) ? extreply : "", (extreply) ? "}" : "");
    }
    else if (!strncmp (readbuf, "ERROR\r", 6) && bytesread >= 7)
    {
      sl_log_r (slconn, 1, 2, "[%s] CAPABILITIES not accepted %s%s%s\n", slconn->sladdr,
                (extreply) ? "{" : "", (extreply) ? extreply : "", (extreply) ? "}" : "");
      return -1;
    }
    else
    {
      sl_log_r (slconn, 2, 0,
                "[%s] invalid response to CAPABILITIES command: %.*s\n",
                slconn->sladdr, bytesread, readbuf);
      return -1;
    }
  }

  /* Send USERAGENT if protocol >= v4 */
  if (slconn->protocol & SLPROTO40)
  {
    /* Create USERAGENT, optional client name and version */
    snprintf (sendstr, sizeof (sendstr),
              "USERAGENT %s%s%s libslink/%s\r\n",
              (slconn->clientname) ? slconn->clientname : "",
              (slconn->clientname && slconn->clientversion) ? "/" : "",
              (slconn->clientname && slconn->clientversion) ? slconn->clientversion : "",
              LIBSLINK_VERSION);

    /* Send USERAGENT and recv response */
    sl_log_r (slconn, 1, 2, "[%s] sending: %.*s\n", slconn->sladdr,
              (int)strcspn (sendstr, "\r\n"), sendstr);

    bytesread = sl_senddata (slconn, (void *)sendstr, strlen (sendstr), slconn->sladdr,
                             readbuf, sizeof (readbuf));

    if (bytesread < 0)
    { /* Error from sl_senddata() */
      return -1;
    }

    /* Check response to USERAGENT */
    if (!strncmp (readbuf, "OK\r", 3) && bytesread >= 4)
    {
      sl_log_r (slconn, 1, 2, "[%s] USERAGENT accepted\n",
                slconn->sladdr);
    }
    else if (!strncmp (readbuf, "ERROR", 5) && bytesread >= 6)
    {
      char *cp = readbuf + (bytesread-1);

      /* Trim space, \r, and \n while terminating response string */
      while (*cp == ' ' || *cp == '\r' || *cp == '\n')
        *cp-- = '\0';

      sl_log_r (slconn, 1, 2, "[%s] USERAGENT not accepted: %s\n", slconn->sladdr,
                readbuf+6);
      return -1;
    }
    else
    {
      sl_log_r (slconn, 2, 0,
                "[%s] invalid response to USERAGENT command: %.*s\n",
                slconn->sladdr, bytesread, readbuf);
      return -1;
    }
  }

  /* Send AUTH if auth_value() callback set and protocol >= v4 */
  if (slconn->auth_value &&
      slconn->protocol & SLPROTO40)
  {
    /* Call user-supplied callback function that returns authentication value */
    const char *auth_value = slconn->auth_value (slconn->sladdr, slconn->auth_data);

    if (strlen(auth_value) > sizeof (sendstr) - 10)
    {
      sl_log_r (slconn, 2, 0, "[%s] authentication value too large (%d bytes), maximum: %d bytes\n",
                slconn->sladdr, (int)strlen (auth_value), (int)sizeof (sendstr) - 10);

      if (slconn->auth_finish)
        slconn->auth_finish (slconn->sladdr, slconn->auth_data);

      return -1;
    }

    /* Create full AUTH command */
    snprintf (sendstr, sizeof (sendstr),
              "AUTH %s\r\n",
              auth_value);

    /* Call user-supplied finish callback function */
    if (slconn->auth_finish)
      slconn->auth_finish (slconn->sladdr, slconn->auth_data);

    /* Send AUTH and recv response */
    sl_log_r (slconn, 1, 2, "[%s] sending: AUTH ...\n", slconn->sladdr);

    bytesread = sl_senddata (slconn, (void *)sendstr, strlen (sendstr), slconn->sladdr,
                             readbuf, sizeof (readbuf));

    /* Clear memory with authentication value */
    memset (sendstr, 0, sizeof (sendstr));

    if (bytesread < 0)
    { /* Error from sl_senddata() */
      return -1;
    }

    /* Check response to AUTH */
    if (!strncmp (readbuf, "OK\r", 3) && bytesread >= 4)
    {
      sl_log_r (slconn, 1, 2, "[%s] AUTH accepted\n",
                slconn->sladdr);
    }
    else if (!strncmp (readbuf, "ERROR", 5) && bytesread >= 6)
    {
      char *cp = readbuf + (bytesread-1);

      /* Trim space, \r, and \n while terminating response string */
      while (*cp == ' ' || *cp == '\r' || *cp == '\n')
        *cp-- = '\0';

      sl_log_r (slconn, 1, 2, "[%s] AUTH not accepted: %s\n", slconn->sladdr,
                readbuf+6);
      return -1;
    }
    else
    {
      sl_log_r (slconn, 2, 0,
                "[%s] invalid response to AUTH command: %.*s\n",
                slconn->sladdr, bytesread, readbuf);
      return -1;
    }
  }

  /* Send BATCH if v3 and set */
  if (slconn->protocol & SLPROTO3X &&
      slconn->batchmode)
  {
    if (batchmode_int (slconn) < 0)
    {
      return -1;
    }
  }

  return 0;
} /* End of sayhello_int() */

/***************************************************************************
 * batchmode_int:
 *
 * Send the BATCH command to switch the connection to batch command
 * mode, in this mode the server will not send acknowledgments (OK or
 * ERROR) after recognized commands are submitted.
 *
 * Returns -1 on errors, 0 on success (regardless if the command was accepted).
 * Sets slconn->batchmode accordingly.
 ***************************************************************************/
static int
batchmode_int (SLCD *slconn)
{
  char sendstr[100]; /* A buffer for command strings */
  char readbuf[100]; /* A buffer for server reply */
  int bytesread = 0;

  if (!slconn)
    return -1;

  /* Send BATCH and recv response */
  snprintf (sendstr, sizeof (sendstr), "BATCH\r\n");

  sl_log_r (slconn, 1, 2, "[%s] sending: %.*s\n", slconn->sladdr,
            (int)strcspn (sendstr, "\r\n"), sendstr);

  bytesread = sl_senddata (slconn, (void *)sendstr, strlen (sendstr), slconn->sladdr,
                           readbuf, sizeof (readbuf));

  if (bytesread < 0)
  { /* Error from sl_senddata() */
    return -1;
  }

  /* Check response to BATCH */
  if (!strncmp (readbuf, "OK\r", 3) && bytesread >= 4)
  {
    sl_log_r (slconn, 1, 2, "[%s] BATCH accepted\n", slconn->sladdr);
    slconn->batchmode = 2;
  }
  else if (!strncmp (readbuf, "ERROR\r", 6) && bytesread >= 7)
  {
    sl_log_r (slconn, 1, 2, "[%s] BATCH not accepted\n", slconn->sladdr);
  }
  else
  {
    sl_log_r (slconn, 2, 0,
              "[%s] invalid response to BATCH command: %.*s\n",
              slconn->sladdr, bytesread, readbuf);
    return -1;
  }

  return 0;
} /* End of batchmode_int() */

/***************************************************************************
 * negotiate_uni_v3:
 *
 * Negotiate stream details with protocol 3 in uni-station mode and
 * issue the DATA command.  This is compatible with SeedLink Protocol
 * version 2 or greater.
 *
 * If \a selectors is set then the string is parsed on space and each
 * selector is sent.
 *
 * If \a seqnum is not `SL_UNSETSEQUENCE` and the SLCD \a resume flag is
 * true then data is requested starting at seqnum.
 *
 * Returns -1 on errors, otherwise returns the link descriptor.
 ***************************************************************************/
static SOCKET
negotiate_uni_v3 (SLCD *slconn)
{
  int sellen    = 0;
  int bytesread = 0;
  int acceptsel = 0; /* Count of accepted selectors */
  char *selptr;
  char *extreply = 0;
  char *term1, *term2;
  char start_time[31] = {0};
  char end_time[31]   = {0};
  char sendstr[100]; /* A buffer for command strings */
  char readbuf[100]; /* A buffer for responses */
  SLstream *curstream;

  /* Generate V3, legacy SeedLink style date-time strings */
  if (slconn->start_time)
  {
    if (sl_commadatetime (start_time, slconn->start_time) == NULL)
    {
      sl_log_r (slconn, 2, 0, "%s(): Start time string cannot be parsed '%s'\n",
                __func__, slconn->start_time);
      return -1;
    }
  }
  if (slconn->end_time)
  {
    if (sl_commadatetime (end_time, slconn->end_time) == NULL)
    {
      sl_log_r (slconn, 2, 0, "%s(): End time string cannot be parsed '%s'\n",
                __func__, slconn->end_time);
      return -1;
    }
  }

  curstream = slconn->streams;

  /* Send the selector(s) and check the response(s) */
  if (curstream->selectors)
  {
    selptr = curstream->selectors;
    sellen = 0;

    while (1)
    {
      /* Parse space-separated selectors and submit individually */
      selptr += sellen;
      selptr += strspn (selptr, " ");
      sellen = strcspn (selptr, " ");

      if (sellen == 0)
      {
        break; /* end of while loop */
      }
      else
      {

        /* Build SELECT command, send it and receive response */
        snprintf (sendstr, sizeof (sendstr), "SELECT %.*s\r\n", sellen, selptr);

        sl_log_r (slconn, 1, 2, "[%s] sending: %.*s\n", slconn->sladdr,
                  (int)strcspn (sendstr, "\r\n"), sendstr);

        bytesread = sl_senddata (slconn, (void *)sendstr,
                                 strlen (sendstr), slconn->sladdr,
                                 readbuf, sizeof (readbuf));
        if (bytesread < 0)
        { /* Error from sl_senddata() */
          return -1;
        }

        /* Search for 2nd "\r" indicating extended reply message present */
        extreply = 0;
        if ((term1 = memchr (readbuf, '\r', bytesread)))
        {
          if ((term2 = memchr (term1 + 1, '\r', bytesread - (readbuf - term1) - 1)))
          {
            *term2   = '\0';
            extreply = term1 + 1;
          }
        }

        /* Check response to SELECT */
        if (!strncmp (readbuf, "OK\r", 3) && bytesread >= 4)
        {
          sl_log_r (slconn, 1, 2, "[%s] selector %.*s is OK %s%s%s\n", slconn->sladdr,
                    sellen, selptr, (extreply) ? "{" : "", (extreply) ? extreply : "", (extreply) ? "}" : "");
          acceptsel++;
        }
        else if (!strncmp (readbuf, "ERROR\r", 6) && bytesread >= 7)
        {
          sl_log_r (slconn, 1, 2, "[%s] selector %.*s not accepted %s%s%s\n", slconn->sladdr,
                    sellen, selptr, (extreply) ? "{" : "", (extreply) ? extreply : "", (extreply) ? "}" : "");
        }
        else
        {
          sl_log_r (slconn, 2, 0,
                    "[%s] invalid response to SELECT command: %.*s\n",
                    slconn->sladdr, bytesread, readbuf);
          return -1;
        }
      }
    }

    /* Fail if none of the given selectors were accepted */
    if (!acceptsel)
    {
      sl_log_r (slconn, 2, 0, "[%s] no data stream selector(s) accepted\n",
                slconn->sladdr);
      return -1;
    }
    else
    {
      sl_log_r (slconn, 1, 2, "[%s] %d selector(s) accepted\n",
                slconn->sladdr, acceptsel);
    }
  } /* End of selector processing */

  /* Issue the DATA, FETCH or TIME action commands.  A specified start (and
     optionally, stop time) takes precedence over the resumption from any
     previous sequence number. */
  if (start_time[0])
  {
    if (end_time[0])
    {
      snprintf (sendstr, sizeof (sendstr), "TIME %.31s %.31s\r\n", start_time, end_time);
    }
    else
    {
      snprintf (sendstr, sizeof (sendstr), "TIME %.31s\r\n", start_time);
    }

    sl_log_r (slconn, 1, 1, "[%s] requesting specified time window\n",
              slconn->sladdr);
  }
  else if (curstream->seqnum != SL_UNSETSEQUENCE && slconn->resume)
  {
    char cmd[10];

    if (slconn->dialup)
    {
      snprintf (cmd, sizeof (cmd), "FETCH");
    }
    else
    {
      snprintf (cmd, sizeof(cmd), "DATA");
    }

    /* Append the last packet time if the feature is enabled */
    if (slconn->lastpkttime &&
        strlen (curstream->timestamp))
    {
      char timestr[31] = {0};

      if (sl_commadatetime (timestr, curstream->timestamp) == NULL)
      {
        sl_log_r (slconn, 2, 0, "%s(): Stream time string cannot be parsed '%s'\n",
                  __func__, curstream->timestamp);
        return -1;
      }

      /* Increment sequence number by 1 */
      snprintf (sendstr, sizeof (sendstr), "%s %0" PRIX64 " %.31s\r\n", cmd,
                (curstream->seqnum + 1), timestr);

      sl_log_r (slconn, 1, 1,
                "[%s] resuming data from %0" PRIX64 " (Dec %" PRIu64 ") at %.31s\n",
                slconn->sladdr, (curstream->seqnum + 1),
                (curstream->seqnum + 1), timestr);
    }
    else
    {
      /* Increment sequence number by 1 */
      snprintf (sendstr, sizeof (sendstr), "%s %0" PRIX64 "\r\n", cmd,
                (curstream->seqnum + 1));

      sl_log_r (slconn, 1, 1,
                "[%s] resuming data from %0" PRIX64 " (Dec %" PRIu64 ")\n",
                slconn->sladdr, (curstream->seqnum + 1),
                (curstream->seqnum + 1));
    }
  }
  else
  {
    if (slconn->dialup)
    {
      snprintf (sendstr, sizeof (sendstr), "FETCH\r\n");
    }
    else
    {
      snprintf (sendstr, sizeof (sendstr), "DATA\r\n");
    }

    sl_log_r (slconn, 1, 1, "[%s] requesting next available data\n", slconn->sladdr);
  }

  if (sl_senddata (slconn, (void *)sendstr, strlen (sendstr),
                   slconn->sladdr, (void *)NULL, 0) < 0)
  {
    sl_log_r (slconn, 2, 0, "[%s] error sending DATA/FETCH/TIME request\n", slconn->sladdr);
    return -1;
  }

  return slconn->link;
} /* End of negotiate_uni_v3() */

/***************************************************************************
 * negotiate_multi_v3:
 *
 * Negotiate stream selection with protocol 3 in multi-station mode
 * and issue the END command to start streaming.
 *
 * If \a curstream->selectors is set then the string is parsed on spaces
 * and each selector is sent.
 *
 * If \a curstream->seqnum is not `SL_UNSETSEQUENCE` and the SLCD \a resume
 * flag is true then data is requested starting at seqnum.
 *
 * Returns -1 on errors, otherwise returns the link descriptor.
 ***************************************************************************/
static SOCKET
negotiate_multi_v3 (SLCD *slconn)
{
  int sellen    = 0;
  int bytesread = 0;
  int acceptsta = 0; /* Count of accepted stations */
  int acceptsel = 0; /* Count of accepted selectors */
  char *selptr;
  char *term1, *term2;
  char *extreply      = 0;
  char start_time[31] = {0};
  char end_time[31]   = {0};
  char sendstr[100]; /* A buffer for command strings */
  char readbuf[100]; /* A buffer for responses */
  SLstream *curstream;

  char net[22];
  char *sta;

  /* Generate V3, legacy SeedLink style date-time strings */
  if (slconn->start_time)
  {
    if (sl_commadatetime (start_time, slconn->start_time) == NULL)
    {
      sl_log_r (slconn, 2, 0, "%s(): Start time string cannot be parsed '%s'\n",
                __func__, slconn->start_time);
      return -1;
    }
  }
  if (slconn->end_time)
  {
    if (sl_commadatetime (end_time, slconn->end_time) == NULL)
    {
      sl_log_r (slconn, 2, 0, "%s(): End time string cannot be parsed '%s'\n",
                __func__, slconn->end_time);
      return -1;
    }
  }

  curstream = slconn->streams;

  /* Loop through the stream list */
  while (curstream != NULL)
  {
    /* Generate independent network and station strings from NET_STA */
    strncpy (net, curstream->stationid, sizeof(net));
    if ((sta = strchr (net, '_')))
      *sta++ = '\0';

    /* Send the STATION command */
    snprintf (sendstr, sizeof (sendstr), "STATION %s %s\r\n", (sta) ? sta : "", net);

    sl_log_r (slconn, 1, 2, "[%s] sending: %.*s\n",
              curstream->stationid,
              (int)strcspn (sendstr, "\r\n"), sendstr);

    bytesread = sl_senddata (slconn, (void *)sendstr, strlen (sendstr),
                             curstream->stationid,
                             (slconn->batchmode == 2) ? (void *)NULL : readbuf,
                             sizeof (readbuf));

    if (bytesread < 0)
    {
      return -1;
    }
    else if (bytesread == 0 && slconn->batchmode == 2)
    {
      acceptsta++;
    }
    else
    {
      /* Search for 2nd "\r" indicating extended reply message present */
      extreply = 0;
      if ((term1 = memchr (readbuf, '\r', bytesread)))
      {
        if ((term2 = memchr (term1 + 1, '\r', bytesread - (readbuf - term1) - 1)))
        {
          *term2   = '\0';
          extreply = term1 + 1;
        }
      }

      /* Check the response */
      if (!strncmp (readbuf, "OK\r", 3) && bytesread >= 4)
      {
        sl_log_r (slconn, 1, 2, "[%s] station is OK %s%s%s\n", curstream->stationid,
                  (extreply) ? "{" : "", (extreply) ? extreply : "", (extreply) ? "}" : "");
        acceptsta++;
      }
      else if (!strncmp (readbuf, "ERROR\r", 6) && bytesread >= 7)
      {
        sl_log_r (slconn, 2, 0, "[%s] station not accepted %s%s%s\n", curstream->stationid,
                  (extreply) ? "{" : "", (extreply) ? extreply : "", (extreply) ? "}" : "");
        /* Increment the loop control and skip to the next stream */
        curstream = curstream->next;
        continue;
      }
      else
      {
        sl_log_r (slconn, 2, 0, "[%s] invalid response to STATION command: %.*s\n",
                  curstream->stationid, bytesread, readbuf);
        return -1;
      }
    }

    /* Send the selector(s) and check the response(s) */
    if (curstream->selectors)
    {
      selptr = curstream->selectors;
      sellen = 0;

      while (1)
      {
        /* Parse space-separated selectors and submit individually */
        selptr += sellen;
        selptr += strspn (selptr, " ");
        sellen = strcspn (selptr, " ");

        if (sellen == 0)
        {
          break; /* end of while loop */
        }
        else
        {
          /* Build SELECT command, send it and receive response */
          snprintf (sendstr, sizeof (sendstr), "SELECT %.*s\r\n", sellen, selptr);

          sl_log_r (slconn, 1, 2, "[%s] sending: SELECT %.*s\n",
                    curstream->stationid,
                    (int)strcspn (sendstr, "\r\n"), sendstr);

          bytesread = sl_senddata (slconn, (void *)sendstr, strlen (sendstr),
                                   curstream->stationid,
                                   (slconn->batchmode == 2) ? (void *)NULL : readbuf,
                                   sizeof (readbuf));

          if (bytesread < 0)
          {
            return -1;
          }
          else if (bytesread == 0 && slconn->batchmode == 2)
          {
            acceptsel++;
          }
          else
          {
            /* Search for 2nd "\r" indicating extended reply message present */
            extreply = 0;
            if ((term1 = memchr (readbuf, '\r', bytesread)))
            {
              if ((term2 = memchr (term1 + 1, '\r', bytesread - (readbuf - term1) - 1)))
              {
                *term2   = '\0';
                extreply = term1 + 1;
              }
            }

            /* Check response to SELECT */
            if (!strncmp (readbuf, "OK\r", 3) && bytesread >= 4)
            {
              sl_log_r (slconn, 1, 2, "[%s] selector %.*s is OK %s%s%s\n",
                        curstream->stationid, sellen, selptr,
                        (extreply) ? "{" : "", (extreply) ? extreply : "", (extreply) ? "}" : "");
              acceptsel++;
            }
            else if (!strncmp (readbuf, "ERROR\r", 6) && bytesread >= 7)
            {
              sl_log_r (slconn, 2, 0, "[%s] selector %.*s not accepted %s%s%s\n",
                        curstream->stationid, sellen, selptr,
                        (extreply) ? "{" : "", (extreply) ? extreply : "", (extreply) ? "}" : "");
            }
            else
            {
              sl_log_r (slconn, 2, 0,
                        "[%s] invalid response to SELECT command: %.*s\n",
                        curstream->stationid, bytesread, readbuf);
            return -1;
            }
          }
        }
      }

      /* Fail if none of the given selectors were accepted */
      if (!acceptsel)
      {
        sl_log_r (slconn, 2, 0, "[%s] no data stream selector(s) accepted\n",
                  curstream->stationid);
        return -1;
      }
      else
      {
        sl_log_r (slconn, 1, 2, "[%s] %d selector(s) accepted\n",
                  curstream->stationid, acceptsel);
      }

      acceptsel = 0; /* Reset the accepted selector count */

    } /* End of selector processing */

    /* Issue the DATA, FETCH or TIME action commands.  A specified start (and
       optionally, stop time) takes precedence over the resumption from any
       previous sequence number. */
    if (start_time[0])
    {
      if (end_time[0] == '\0')
      {
        snprintf (sendstr, sizeof (sendstr), "TIME %.31s\r\n", start_time);
      }
      else
      {
        snprintf (sendstr, sizeof (sendstr), "TIME %.31s %.31s\r\n", start_time, end_time);
      }
      sl_log_r (slconn, 1, 1, "[%s] requesting specified time window\n",
                curstream->stationid);
    }
    else if (curstream->seqnum != SL_UNSETSEQUENCE && slconn->resume)
    {
      char cmd[10];

      if (slconn->dialup)
      {
        snprintf (cmd, sizeof (cmd), "FETCH");
      }
      else
      {
        snprintf (cmd, sizeof (cmd), "DATA");
      }

      /* Append the last packet time if the feature is enabled */
      if (slconn->lastpkttime &&
          strlen (curstream->timestamp))
      {
        char timestr[31] = {0};

        if (sl_commadatetime (timestr, curstream->timestamp) == NULL)
        {
          sl_log_r (slconn, 2, 0, "%s(): Stream time string cannot be parsed '%s'\n",
                    __func__, curstream->timestamp);
          return -1;
        }

        /* Increment sequence number by 1 */
        snprintf (sendstr, sizeof (sendstr), "%s %0" PRIX64 " %.31s\r\n", cmd,
                  (curstream->seqnum + 1), timestr);

        sl_log_r (slconn, 1, 1,
                  "[%s] resuming data from %0" PRIX64 " (Dec %" PRIu64 ") at %.31s\n",
                  slconn->sladdr, (curstream->seqnum + 1),
                  (curstream->seqnum + 1), timestr);
      }
      else
      { /* Increment sequence number by 1 */
        snprintf (sendstr, sizeof (sendstr), "%s %0" PRIX64 "\r\n", cmd,
                  (curstream->seqnum + 1));

        sl_log_r (slconn, 1, 1,
                  "[%s] resuming data from %0" PRIX64 " (Dec %" PRIu64 ")\n",
                  curstream->stationid,
                  (curstream->seqnum + 1),
                  (curstream->seqnum + 1));
      }
    }
    else
    {
      if (slconn->dialup)
      {
        snprintf (sendstr, sizeof (sendstr), "FETCH\r\n");
      }
      else
      {
        snprintf (sendstr, sizeof (sendstr), "DATA\r\n");
      }

      sl_log_r (slconn, 1, 1, "[%s] requesting next available data\n",
                curstream->stationid);
    }

    /* Send the TIME/DATA/FETCH command and receive response */
    bytesread = sl_senddata (slconn, (void *)sendstr, strlen (sendstr),
                             curstream->stationid,
                             (slconn->batchmode == 2) ? (void *)NULL : readbuf,
                             sizeof (readbuf));

    if (bytesread < 0)
    {
      return -1;
    }
    else if (bytesread > 0)
    {
      /* Search for 2nd "\r" indicating extended reply message present */
      extreply = 0;
      if ((term1 = memchr (readbuf, '\r', bytesread)))
      {
        if ((term2 = memchr (term1 + 1, '\r', bytesread - (readbuf - term1) - 1)))
        {
          *term2   = '\0';
          extreply = term1 + 1;
        }
      }

      /* Check response to DATA/FETCH/TIME request */
      if (!strncmp (readbuf, "OK\r", 3) && bytesread >= 4)
      {
        sl_log_r (slconn, 1, 2, "[%s] DATA/FETCH/TIME command is OK %s%s%s\n",
                  curstream->stationid,
                  (extreply) ? "{" : "", (extreply) ? extreply : "", (extreply) ? "}" : "");
      }
      else if (!strncmp (readbuf, "ERROR\r", 6) && bytesread >= 7)
      {
        sl_log_r (slconn, 2, 0, "[%s] DATA/FETCH/TIME command is not accepted %s%s%s\n",
                  curstream->stationid,
                  (extreply) ? "{" : "", (extreply) ? extreply : "", (extreply) ? "}" : "");
      }
      else
      {
        sl_log_r (slconn, 2, 0, "[%s] invalid response to DATA/FETCH/TIME command: %.*s\n",
                  curstream->stationid, bytesread, readbuf);
        return -1;
      }
    }

    curstream = curstream->next;

  } /* End of stream and selector config (end of stream list). */

  /* Fail if no stations were accepted */
  if (!acceptsta)
  {
    sl_log_r (slconn, 2, 0, "[%s] no station(s) accepted\n", slconn->sladdr);
    return -1;
  }
  else
  {
    sl_log_r (slconn, 1, 1, "[%s] %d station(s) accepted\n",
              slconn->sladdr, acceptsta);
  }

  /* Issue END action command */
  snprintf (sendstr, sizeof (sendstr), "END\r\n");

  sl_log_r (slconn, 1, 2, "[%s] sending: %.*s\n", slconn->sladdr,
            (int)strcspn (sendstr, "\r\n"), sendstr);

  if (sl_senddata (slconn, (void *)sendstr, strlen (sendstr),
                   slconn->sladdr, (void *)NULL, 0) < 0)
  {
    sl_log_r (slconn, 2, 0, "[%s] error sending END command\n", slconn->sladdr);
    return -1;
  }

  return slconn->link;
} /* End of negotiate_multi_v3() */

/***************************************************************************
 * negotiate_v4:
 *
 * Negotiate stream selection with protocol 4 and issue the END
 * command to start streaming.
 *
 * If a stream's \a selectors is set then the string is parsed on spaces
 * and each selector is sent.
 *
 * If a streams's \a seqnum is set to SL_ALLDATASEQUENCE all data will
 * be requested.  Otherwise, if it is not set to SL_UNSETSEQUENCE and the
 * SLCD.resume flag is true then data is requested starting at seqnum.
 *
 * Returns -1 on errors, otherwise returns the link descriptor.
 ***************************************************************************/
static SOCKET
negotiate_v4 (SLCD *slconn)
{
  int stationcnt = 0; /* Station count */
  int errorcnt   = 0; /* Error count */
  int bytesread  = 0;
  size_t sellen  = 0;
  char *selptr;
  char *cp;
  char *cmd_selector;
  char selector[32]   = {0};
  char v4selector[32] = {0};
  char start_time[31] = {0};
  char end_time[31]   = {0};
  char sendstr[32];  /* A buffer for small command strings */
  char readbuf[200]; /* A buffer for responses */
  SLstream *curstream;

  struct cmd_s
  {
    char cmd[100];
    char nsid[22];
    struct cmd_s *next;
  };

  struct cmd_s *cmdlist = NULL;
  struct cmd_s *cmdtail = NULL;
  struct cmd_s *cmdptr = NULL;

  if (!slconn)
    return -1;

  /* Generate V4, ISO compatible date-time strings */
  if (slconn->start_time)
  {
    if (sl_isodatetime (start_time, slconn->start_time) == NULL)
    {
      sl_log_r (slconn, 2, 0, "%s(): Start time string cannot be converted '%s'\n",
                __func__, slconn->start_time);
      return -1;
    }
  }
  if (slconn->end_time)
  {
    if (sl_isodatetime (end_time, slconn->end_time) == NULL)
    {
      sl_log_r (slconn, 2, 0, "%s(): End time string cannot be converted '%s'\n",
                __func__, slconn->end_time);
      return -1;
    }
  }

  curstream = slconn->streams;

  /* Loop through the stream list */
  while (curstream != NULL)
  {
    /* Allocate new command in list */
    if ((cmdptr = (struct cmd_s *)malloc(sizeof(struct cmd_s))) == NULL)
    {
      sl_log_r (slconn, 2, 0, "%s() Cannot allocate memory\n", __func__);
      while (cmdlist)
      {
        cmdptr = cmdlist->next;
        free (cmdlist);
        cmdlist = cmdptr;
      }

      return -1;
    }

    if (!cmdlist)
    {
      cmdlist = cmdptr;
      cmdtail = cmdptr;
    }
    else
    {
      cmdtail->next = cmdptr;
      cmdtail = cmdptr;
    }

    strcpy (cmdtail->nsid, curstream->stationid);
    cmdtail->next = NULL;

    /* Generate STATION command */
    snprintf (cmdtail->cmd, sizeof (cmdtail->cmd),
              "STATION %s\r",
              curstream->stationid);

    stationcnt++;

    /* Send the selector(s) */
    if (curstream->selectors)
    {
      selptr = curstream->selectors;
      sellen = 0;

      while (1)
      {
      /* Parse space-separated selectors and submit individually */
        selptr += sellen;
        selptr += strspn (selptr, " ");
        sellen = strcspn (selptr, " ");

        if (sellen >= sizeof (selector))
        {
          sl_log_r (slconn, 2, 0, "%s() Selector too long: %s\n", __func__, selptr);

          return -1;
        }

        if (sellen == 0)
          break; /* end of while loop */
        else
        {
          strncpy (selector, selptr, sellen);
          selector[sellen] = '\0';

          /* Convert selector from v3 to v4 if necessary */
          if (sl_v3to4selector (v4selector, sizeof (v4selector), selector))
          {
            cmd_selector = v4selector;
          }
          else
          {
            cmd_selector = selector;
          }

          /* Allocate new command in list */
          if ((cmdptr = (struct cmd_s *)malloc (sizeof (struct cmd_s))) == NULL)
          {
            sl_log_r (slconn, 2, 0, "%s() Cannot allocate memory\n", __func__);
            while (cmdlist)
            {
              cmdptr = cmdlist->next;
              free (cmdlist);
              cmdlist = cmdptr;
            }

            return -1;
          }

          cmdtail->next = cmdptr;
          cmdtail = cmdptr;
          strcpy (cmdtail->nsid, curstream->stationid);
          cmdtail->next = NULL;

          /* Generate SELECT command */
          snprintf (cmdtail->cmd, sizeof (cmdtail->cmd),
                    "SELECT %s\r",
                    cmd_selector);
        }
      }
    } /* End of selector processing */

    /* Allocate new command in list */
    if ((cmdptr = (struct cmd_s *)malloc(sizeof(struct cmd_s))) == NULL)
    {
      sl_log_r (slconn, 2, 0, "%s() Cannot allocate memory\n", __func__);
      while (cmdlist)
      {
        cmdptr = cmdlist->next;
        free (cmdlist);
        cmdlist = cmdptr;
      }

      return -1;
    }

    cmdtail->next = cmdptr;
    cmdtail = cmdptr;
    strcpy (cmdtail->nsid, curstream->stationid);
    cmdtail->next = NULL;

    /* Generate DATA command with _incremented_ sequence number */
    if (start_time[0])
    {
      if (curstream->seqnum != SL_UNSETSEQUENCE)
      {
        snprintf (cmdtail->cmd, sizeof (cmdtail->cmd),
                  "DATA %" PRIu64 "%s%s%s\r",
                  (curstream->seqnum + 1),
                  start_time,
                  (end_time[0]) ? " " : "",
                  (end_time[0]) ? end_time : "");
      }
      else
      {
        snprintf (cmdtail->cmd, sizeof (cmdtail->cmd),
                  "DATA ALL %s%s%s\r",
                  start_time,
                  (end_time[0]) ? " " : "",
                  (end_time[0]) ? end_time : "");
      }
    }
    else
    {
      if (curstream->seqnum == SL_UNSETSEQUENCE)
      {
        snprintf (cmdtail->cmd, sizeof (cmdtail->cmd),
                  "DATA\r");
      }
      else if (curstream->seqnum == SL_ALLDATASEQUENCE)
      {
        snprintf (cmdtail->cmd, sizeof (cmdtail->cmd),
                  "DATA ALL\r");
      }
      else
      {
        snprintf (cmdtail->cmd, sizeof (cmdtail->cmd),
                  "DATA %" PRIu64 "\r",
                  (curstream->seqnum + 1));
      }
    }

    curstream = curstream->next;
  } /* End of stream and selector config */

  /* Send all generated commands */
  cmdptr = cmdlist;
  while (cmdptr)
  {
    sl_log_r (slconn, 1, 2, "[%s] sending: %s\n",
              cmdptr->nsid, cmdptr->cmd);

    bytesread = sl_senddata (slconn, (void *)cmdptr->cmd,
                             strlen (cmdptr->cmd), cmdptr->nsid,
                             (void *)NULL, 0);

    if (bytesread < 0)
    {
      while (cmdlist)
      {
        cmdptr = cmdlist->next;
        free (cmdlist);
        cmdlist = cmdptr;
      }

      return -1;
    }

    cmdptr = cmdptr->next;
  }

  /* Receive all responses */
  cmdptr = cmdlist;
  while (cmdptr)
  {
    bytesread = sl_recvresp (slconn, readbuf, sizeof (readbuf),
                             NULL, curstream->stationid);

    if (bytesread < 0)
    {
      while (cmdlist)
      {
        cmdptr = cmdlist->next;
        free (cmdlist);
        cmdlist = cmdptr;
      }

      return -1;
    }

    /* Terminate command and response at first carriage return */
    if ((cp = strchr(cmdptr->cmd, '\r')))
      *cp = '\0';
    if ((cp = strchr(readbuf, '\r')))
      *cp = '\0';

    if (bytesread >= 2 && !strncmp (readbuf, "OK", 2))
    {
      sl_log_r (slconn, 1, 2, "[%s] Command OK (%s)\n",
                cmdptr->nsid, cmdptr->cmd);
    }
    else if (bytesread >= 5 && !strncmp (readbuf, "ERROR", 5))
    {
      sl_log_r (slconn, 2, 0, "[%s] Command not accepted (%s): %s\n",
                cmdptr->nsid, cmdptr->cmd, readbuf);
      errorcnt++;
    }
    else
    {
      sl_log_r (slconn, 2, 0, "[%s] invalid response to command (%s): %s\n",
                cmdptr->nsid, cmdptr->cmd, readbuf);
      errorcnt++;
    }

    cmdptr = cmdptr->next;
  }

  if (errorcnt == 0)
  {
    sl_log_r (slconn, 1, 1, "[%s] %d station(s) accepted\n",
              slconn->sladdr, stationcnt);

    /* Issue END or ENDFETCH command to finalize stream selection and start streaming */
    snprintf (sendstr, sizeof (sendstr), (slconn->dialup) ? "ENDFETCH\r\n" : "END\r\n");

    sl_log_r (slconn, 1, 2, "[%s] sending: %.*s\n", slconn->sladdr,
              (int)strcspn (sendstr, "\r\n"), sendstr);

    if (sl_senddata (slconn, (void *)sendstr, strlen (sendstr),
                     slconn->sladdr, (void *)NULL, 0) < 0)
    {
      sl_log_r (slconn, 2, 0, "[%s] error sending END command\n", slconn->sladdr);
      errorcnt++;
    }
  }

  /* Free command list */
  while (cmdlist)
  {
    cmdptr = cmdlist->next;
    free (cmdlist);
    cmdlist = cmdptr;
  }

  return (errorcnt) ? -1 : slconn->link;
} /* End of negotiate_v4() */

/***************************************************************************
 * Startup the network socket layer.  At the moment this is only meaningful
 * for the WIN platform.
 *
 * Returns -1 on errors and 0 on success.
 ***************************************************************************/
static int
sockstartup_int (void)
{
#if defined(SLP_WIN)
  WORD wVersionRequested;
  WSADATA wsaData;

  /* Check for Windows sockets version 2.2 */
  wVersionRequested = MAKEWORD (2, 2);

  if (WSAStartup (wVersionRequested, &wsaData))
    return -1;

#endif

  return 0;
}

/***************************************************************************
 * Connect a network socket.
 *
 * Returns -1 on errors and 0 on success.
 ***************************************************************************/
static int
sockconnect_int (SOCKET sock, struct sockaddr *inetaddr, int addrlen)
{
#if defined(SLP_WIN)
  if ((connect (sock, inetaddr, addrlen)) == SOCKET_ERROR)
  {
    if (WSAGetLastError () != WSAEINPROGRESS && WSAGetLastError () != WSAEWOULDBLOCK)
      return -1;
  }
#else
  if ((connect (sock, inetaddr, addrlen)) == -1)
  {
    if (errno != EINPROGRESS && errno != EWOULDBLOCK)
      return -1;
  }
#endif

  return 0;
}

/***************************************************************************
 * Set a network socket to non-blocking.
 *
 * Returns -1 on errors and 0 on success.
 ***************************************************************************/
static int
socknoblock_int (SOCKET sock)
{
#if defined(SLP_WIN)
  u_long flag = 1;

  if (ioctlsocket (sock, FIONBIO, &flag) == -1)
    return -1;

#else
  int flags = fcntl (sock, F_GETFL, 0);

  flags |= O_NONBLOCK;
  if (fcntl (sock, F_SETFL, flags) == -1)
    return -1;

#endif

  return 0;
}

/***********************************************************************/ /**
 * @brief Set socket I/O timeout
 *
 * Set socket I/O timeout if such an option exists.  On WIN and
 * other platforms where `SO_RCVTIMEO` and `SO_SNDTIMEO` are defined this
 * sets the `SO_RCVTIMEO` and `SO_SNDTIMEO` socket options using
 * setsockopt() to the @a timeout value (specified in seconds).
 *
 * Solaris does not implelement socket-level timeout options.
 *
 * @param socket Network socket descriptor
 * @param timeout Alarm timeout in seconds
 *
 * @return -1 on error, 0 when not possible and 1 on success.
 ***************************************************************************/
static int
setsocktimeo_int (SOCKET socket, int timeout)
{
#if defined(SLP_WIN)
  int tval = timeout * 1000;

  if (setsockopt (socket, SOL_SOCKET, SO_RCVTIMEO, (char *)&tval, sizeof (tval)))
  {
    return -1;
  }
  tval = timeout * 1000;
  if (setsockopt (socket, SOL_SOCKET, SO_SNDTIMEO, (char *)&tval, sizeof (tval)))
  {
    return -1;
  }

#else
/* Set socket I/O timeouts if socket options are defined */
#if defined(SO_RCVTIMEO) && defined(SO_SNDTIMEO)
  struct timeval tval;

  tval.tv_sec  = timeout;
  tval.tv_usec = 0;

  if (setsockopt (socket, SOL_SOCKET, SO_RCVTIMEO, &tval, sizeof (tval)))
  {
    return -1;
  }
  if (setsockopt (socket, SOL_SOCKET, SO_SNDTIMEO, &tval, sizeof (tval)))
  {
    return -1;
  }
#else
  return 0;
#endif

#endif

  return 1;
}

/***************************************************************************
 * Load Certificate Authority certs for TLS connection cert verification.
 *
 * CA certs are loaded from the following locations (in order):
 * - Environment variable LIBSLINK_TLS_CERT_FILE
 * - Environment variable LIBSLINK_TLS_CERT_PATH (all files in path)
 * - Known CA cert files and paths
 *
 * Returns number of CA certs files/paths loaded.
 ***************************************************************************/
static int
load_ca_certs (SLCD *slconn)
{
  TLSCTX *tlsctx = (TLSCTX *)slconn->tlsctx;
  int ca_loaded  = 0;
  char *evalue   = NULL;
  int ret;

  /* Common locations for Certificate Authority files on Linux/BSD systems */
  char *ca_known_files[] = {
      "/etc/ssl/cert.pem",
      "/etc/ssl/certs/ca-certificates.crt",
      "/etc/pki/tls/certs/ca-bundle.crt",
      "/etc/ssl/ca-bundle.pem",
      "/etc/pki/tls/cacert.pem",
      "/etc/pki/ca-trust/extracted/pem/tls-ca-bundle.pem"};

  char *ca_known_paths[] = {
      "/etc/ssl/certs",
      "/etc/pki/tls/certs"};

  /* Read trusted CA file */
  if ((evalue = getenv ("LIBSLINK_CA_CERT_FILE")) != NULL)
  {
    sl_log_r (slconn, 1, 2, "[%s] Reading TLS CA cert(s) [LIBSLINK_CA_CERT_FILE] from (%s)\n",
              slconn->sladdr, evalue);

    if ((ret = mbedtls_x509_crt_parse_file (&tlsctx->cacert, evalue)) != 0)
    {
      sl_log_r (slconn, 2, 0, "[%s] mbedtls_x509_crt_parse_file() returned -0x%x\n",
                slconn->sladdr, (unsigned int)-ret);
      return -1;
    }

    ca_loaded += 1;
  }

  /* Read trusted CA files from directory */
  if ((evalue = getenv ("LIBSLINK_CA_CERT_PATH")) != NULL)
  {
    sl_log_r (slconn, 1, 2, "[%s] Reading TLS CA cert(s) [LIBSLINK_CA_CERT_PATH] from path (%s)\n",
              slconn->sladdr, evalue);

    if ((ret = mbedtls_x509_crt_parse_path (&tlsctx->cacert, evalue)) != 0)
    {
      sl_log_r (slconn, 2, 0, "[%s] mbedtls_x509_crt_parse_path() returned -0x%x\n",
                slconn->sladdr, (unsigned int)-ret);
      return -1;
    }

    ca_loaded += 1;
  }

  /* If no CA certs loaded yet, search for known locations and load */
  if (ca_loaded == 0)
  {
    /* Search known CA file locations, stop after finding one */
    for (size_t i = 0; i < sizeof(ca_known_files) / sizeof(ca_known_files[0]); i++)
    {
      if (access (ca_known_files[i], R_OK) != 0)
      {
        continue;
      }

      sl_log_r (slconn, 1, 2, "[%s] Reading TLS CA cert file (%s)\n",
                slconn->sladdr, ca_known_files[i]);

      if ((ret = mbedtls_x509_crt_parse_file (&tlsctx->cacert, ca_known_files[i])) != 0)
      {
        sl_log_r (slconn, 2, 0, "[%s] mbedtls_x509_crt_parse_file(%s) returned -0x%x\n",
                  slconn->sladdr, ca_known_files[i], (unsigned int)-ret);
        return -1;
      }

      ca_loaded += 1;
      break;
    }

    /* Search known CA cert path locations, read all locations */
    for (size_t i = 0; i < sizeof(ca_known_paths) / sizeof(ca_known_paths[0]); i++)
    {
      if (access (ca_known_paths[i], R_OK) != 0)
      {
        continue;
      }

      sl_log_r (slconn, 1, 2, "[%s] Reading TLS CA cert files from path (%s)\n",
                slconn->sladdr, ca_known_paths[i]);

      if ((ret = mbedtls_x509_crt_parse_path (&tlsctx->cacert, ca_known_paths[i])) != 0)
      {
        sl_log_r (slconn, 2, 0, "[%s] mbedtls_x509_crt_parse_path(%s) returned -0x%x\n",
                  slconn->sladdr, ca_known_paths[i], (unsigned int)-ret);
        return -1;
      }

      ca_loaded += 1;
    }
  }

  return ca_loaded;
}
