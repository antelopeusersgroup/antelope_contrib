/***********************************************************************/ /**
 * @file network.c
 *
 * Network communication routines for DataLink
 *
 * This file is part of the DataLink Library.
 *
 * Copyright (c) 2020 Chad Trabant, IRIS Data Management Center
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
 ***************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "libdali.h"
#include "portable.h"

/***********************************************************************/ /**
 * @brief Connect to a DataLink server
 *
 * Open a network socket connection to a Datalink server and set
 * 'dlconn->link' to the new descriptor.  Expects 'dlconn->addr' to be
 * in 'host:port' or 'host\@port' format.  Either the host, port or
 * both are optional, if the host is not specified 'localhost' is
 * assumed, if the port is not specified '16000' is assumed, if
 * neither is specified (only a separator) then 'localhost' and port
 * '16000' are assumed.
 *
 * If a permanent error is detected (invalid port specified) the
 * dlconn->terminate flag will be set so the dl_collect() family of
 * routines will not continue trying to connect.
 *
 * @param dlconn DataLink Connection Parameters
 *
 * @return the socket descriptor created.
 * @retval -1 on errors
 ***************************************************************************/
SOCKET
dl_connect (DLCP *dlconn)
{
  struct addrinfo *addr0 = NULL;
  struct addrinfo *addr = NULL;
  struct addrinfo hints;
  SOCKET sock;
  long int nport;
  char nodename[300];
  char nodeport[100];
  char *ptr, *tail;
  int timeout;
  int socket_family = -1;

  if (dlp_sockstartup ())
  {
    dl_log_r (dlconn, 2, 0, "could not initialize network sockets\n");
    return -1;
  }

  /* Search address host-port separator, first for '@', then ':' */
  if ((ptr = strchr (dlconn->addr, '@')) == NULL && (ptr = strchr (dlconn->addr, ':')))
  {
    /* If first ':' is not the last, this is not a separator */
    if (strrchr (dlconn->addr, ':') != ptr)
      ptr = NULL;
  }

  /* If address begins with the separator */
  if (dlconn->addr == ptr)
  {
    if (dlconn->addr[1] == '\0')  /* Only a separator */
    {
      strcpy (nodename, LD_DEFAULT_HOST);
      strcpy (nodeport, LD_DEFAULT_PORT);
    }
    else /* Only a port */
    {
      strcpy (nodename, LD_DEFAULT_HOST);
      strncpy (nodeport, dlconn->addr + 1, sizeof (nodeport));
    }
  }
  /* Otherwise if no separator, use default port */
  else if (ptr == NULL)
  {
    strncpy (nodename, dlconn->addr, sizeof (nodename));
    strcpy (nodeport, LD_DEFAULT_PORT);
  }
  /* Otherwise separate host and port */
  else if ((ptr - dlconn->addr) < sizeof (nodename))
  {
    strncpy (nodename, dlconn->addr, (ptr - dlconn->addr));
    nodename[(ptr - dlconn->addr)] = '\0';
    strncpy (nodeport, ptr + 1, sizeof (nodeport) - 1);
  }

  /* Sanity test the port number */
  nport = strtoul (nodeport, &tail, 10);
  if (*tail || (nport <= 0 || nport > 0xffff))
  {
    dl_log_r (dlconn, 2, 0, "server port specified incorrectly\n");
    dlconn->terminate = 1;
    return -1;
  }

  /* Resolve for either IPv4 or IPv6 (PF_UNSPEC) for a TCP stream (SOCK_STREAM) */
  memset (&hints, 0, sizeof (hints));
  hints.ai_family   = PF_UNSPEC;
  hints.ai_socktype = SOCK_STREAM;

  /* Resolve server address */
  if (getaddrinfo (nodename, nodeport, &hints, &addr0))
  {
    dl_log_r (dlconn, 2, 0, "cannot resolve hostname %s\n", nodename);
    return -1;
  }

  /* Traverse addresses trying to connect */
  sock = -1;
  for (addr = addr0; addr != NULL; addr = addr->ai_next)
  {
    /* Create socket */
    if ((sock = socket (addr->ai_family, addr->ai_socktype, addr->ai_protocol)) < 0)
    {
      continue;
    }

    /* Set socket I/O timeouts if possible */
    if (dlconn->iotimeout)
    {
      timeout = (dlconn->iotimeout > 0) ? dlconn->iotimeout : -dlconn->iotimeout;

      if (dlp_setsocktimeo (sock, timeout) == 1)
      {
        /* Negate timeout to indicate socket timeouts are set */
        dlconn->iotimeout = -timeout;
      }
    }

    /* Connect socket */
    if ((dlp_sockconnect (sock, addr->ai_addr, addr->ai_addrlen)))
    {
      dlp_sockclose (sock);
      sock = -1;
      continue;
    }

    socket_family = addr->ai_family;
    break;
  }

  if (sock < 0)
  {
    dl_log_r (dlconn, 2, 0, "[%s] Cannot connect: %s\n", dlconn->addr, dlp_strerror ());
    dlp_sockclose (sock);
    freeaddrinfo (addr0);
    return -1;
  }

  freeaddrinfo(addr0);

  if (dlconn->iotimeout < 0)
  {
    dl_log_r (dlconn, 1, 2, "[%s] using system socket timeouts\n", dlconn->addr);
  }

  /* Set socket to non-blocking */
  if (dlp_socknoblock (sock))
  {
    dl_log_r (dlconn, 2, 0, "Error setting socket to non-blocking\n");
    dlp_sockclose (sock);
    return -1;
  }

  /* Socket connected */
  dl_log_r (dlconn, 1, 1, "[%s] network socket opened ", dlconn->addr);
  switch (socket_family)
  {
  case PF_INET:
    dl_log_r (dlconn, 1, 1, "(IPv4)\n");
    break;
  case PF_INET6:
    dl_log_r (dlconn, 1, 1, "(IPv6)\n");
    break;
  default:
    dl_log_r (dlconn, 1, 1, "(Unknown protocol)\n");
  }

  dlconn->link = sock;

  /* Everything should be connected, exchange IDs */
  if (dl_exchangeIDs (dlconn, 1) == -1)
  {
    dlp_sockclose (sock);
    return -1;
  }

  return sock;
} /* End of dl_connect() */

/***********************************************************************/ /**
 * @brief Disconnect a DataLink connection
 *
 * Close the network socket associated with connection and set
 * 'dlconn->link' to -1.
 *
 * @param dlconn DataLink Connection Parameters
 ***************************************************************************/
void
dl_disconnect (DLCP *dlconn)
{
  if (dlconn->link >= 0)
  {
    dlp_sockclose (dlconn->link);
    dlconn->link = -1;

    dl_log_r (dlconn, 1, 1, "[%s] network socket closed\n", dlconn->addr);
  }
} /* End of dl_disconnect() */

/***********************************************************************/ /**
 * @brief Send arbitrary data to a DataLink server
 *
 * This fundamental routine is used by other library routines to send
 * data via a DataLink connection.  Before data is sent the socket to
 * set to blocking mode and back to non-blocking before returning
 * unless there was an error in which case the socket should be
 * disconnected.
 *
 * If a user specified network I/O timeout was not applied at the
 * system socket level this routine will implement the timeout using
 * an alarm timer to interrupt the blocked send.
 *
 * @param dlconn DataLink Connection Parameters
 * @param buffer Buffer containing data to send
 * @param sendlen Number of bytes to send from buffer
 *
 * @retval 0 on success
 * @retval -1 on error.
 ***************************************************************************/
int
dl_senddata (DLCP *dlconn, void *buffer, size_t sendlen)
{
  /* Set socket to blocking */
  if (dlp_sockblock (dlconn->link))
  {
    dl_log_r (dlconn, 2, 0, "[%s] error setting socket to blocking\n",
              dlconn->addr);
    return -1;
  }

  /* Set timeout alarm if needed */
  if (dlconn->iotimeout > 0)
  {
    if (dlp_setioalarm (dlconn->iotimeout))
    {
      dl_log_r (dlconn, 2, 0, "[%s] error setting network I/O timeout\n",
                dlconn->addr);
    }
  }

  /* Send data */
  if (send (dlconn->link, buffer, sendlen, 0) != (int64_t)sendlen)
  {
    dl_log_r (dlconn, 2, 0, "[%s] error sending data\n", dlconn->addr);
    return -1;
  }

  /* Cancel timeout alarm if set */
  if (dlconn->iotimeout > 0)
  {
    if (dlp_setioalarm (0))
    {
      dl_log_r (dlconn, 2, 0, "[%s] error cancelling network I/O timeout\n",
                dlconn->addr);
    }
  }

  /* Set socket to non-blocking */
  if (dlp_socknoblock (dlconn->link))
  {
    dl_log_r (dlconn, 2, 0, "[%s] error setting socket to non-blocking\n",
              dlconn->addr);
    return -1;
  }

  return 0;
} /* End of dl_senddata() */

/***********************************************************************/ /**
 * @brief Create and send a DataLink packet
 *
 * Send a DataLink packet created by combining an appropriate
 * preheader with @a headerbuf and, optionally, @a databuf.
 *
 * The header length must be larger than 0 but the packet length can
 * be 0 resulting in a header-only packet, commonly used for sending
 * commands.
 *
 * If the response buffer @a respbuf is not NULL then read up to @a
 * resplen bytes into @a respbuf using dl_recvheader() after sending
 * the packet.  This is only designed for small pieces of data,
 * specifically the server acknowledgement to a command, which are a
 * header-only packets.
 *
 * @param dlconn DataLink Connection Parameters
 * @param headerbuf Buffer containing DataLink packet header
 * @param headerlen Length of header buffer to send
 * @param databuf Buffer containing DataLink packet data
 * @param datalen Length of data buffer to send
 * @param respbuf Buffer to place response from server
 * @param resplen Length of response buffer
 *
 * @return number of bytes of response received
 * @retval 0 on success and @a respbuf is NULL
 * @retval -1 on error
 ***************************************************************************/
int
dl_sendpacket (DLCP *dlconn, void *headerbuf, size_t headerlen,
               void *databuf, size_t datalen,
               void *respbuf, int resplen)
{
  int bytesread = 0; /* bytes read into resp buffer */
  char wirepacket[MAXPACKETSIZE];

  if (!dlconn || !headerbuf)
    return -1;

  /* Sanity check that the header is not too large or zero */
  if (headerlen > 255 || headerlen == 0)
  {
    dl_log_r (dlconn, 2, 0, "[%s] packet header size is invalid: %" PRIsize_t "\n",
              dlconn->addr, headerlen);
    return -1;
  }

  /* Sanity check that the header + packet data is not too large */
  if ((3 + headerlen + datalen) > MAXPACKETSIZE)
  {
    dl_log_r (dlconn, 2, 0, "[%s] packet is too large (%" PRIsize_t "), max is %d\n",
              dlconn->addr, (headerlen + datalen), MAXPACKETSIZE);
    return -1;
  }

  /* Set the synchronization and header size bytes */
  wirepacket[0] = 'D';
  wirepacket[1] = 'L';
  wirepacket[2] = (uint8_t)headerlen;

  /* Copy header into the wire packet */
  memcpy (wirepacket + 3, headerbuf, headerlen);

  /* Copy packet data into the wire packet if supplied */
  if (databuf && datalen > 0)
    memcpy (wirepacket + 3 + headerlen, databuf, datalen);

  /* Send data */
  if (dl_senddata (dlconn, wirepacket, (3 + headerlen + datalen)) < 0)
  {
    /* Check for a message from the server */
    if ((bytesread = dl_recvheader (dlconn, respbuf, resplen, 0)) > 0)
    {
      dl_log_r (dlconn, 2, 0, "[%s] %*s", dlconn->addr, bytesread, (char *)respbuf);
    }

    return -1;
  }

  /* If requested collect the response (packet header only) */
  if (respbuf != NULL)
  {
    if ((bytesread = dl_recvheader (dlconn, respbuf, resplen, 1)) < 0)
    {
      if (bytesread < -1)
        dl_log_r (dlconn, 2, 0, "[%s] error receiving data\n", dlconn->addr);

      return -1;
    }
  }

  return bytesread;
} /* End of dl_sendpacket() */

/***********************************************************************/ /**
 * @brief Receive arbitrary data from a DataLink server
 *
 * This fundamental routine is used by other library routines to
 * receive data from a DataLink server.  Up to @a readlen bytes of
 * received data is placed into @a buffer.
 *
 * If @a blockflag is true (1) this function will block until @a
 * readlen bytes have been read.  If @a blockflag is false (0) and no
 * data is available for reading this function will immediately
 * return.  If @a blockflag is false and some initial data is received
 * the function will block until @a readlen bytes have been read.
 *
 * If a user specified network I/O timeout was not applied at the
 * system socket level this routine will implement the timeout using
 * an alarm timer to interrupt the blocked send.
 *
 * @param dlconn DataLink Connection Parameters
 * @param buffer Buffer for received data
 * @param readlen Number of bytes to read and place into @a buffer
 * @param blockflag Flag to control use of blocking versus non-blocking mode
 *
 * @return number of bytes read on success
 * @retval 0 when no data available on non-blocking socket
 * @retval -1 on connection shutdown
 * @retval -2 on error.
 ***************************************************************************/
int
dl_recvdata (DLCP *dlconn, void *buffer, size_t readlen, uint8_t blockflag)
{
  int nrecv;
  int nread  = 0;
  char *bptr = buffer;

  if (!buffer)
  {
    return -2;
  }

  /* Set socket to blocking if requested */
  if (blockflag)
  {
    if (dlp_sockblock (dlconn->link))
    {
      dl_log_r (dlconn, 2, 0, "[%s] Error setting socket to blocking: %s\n",
                dlconn->addr, dlp_strerror ());
      return -2;
    }
  }

  /* Set timeout alarm if needed */
  if (dlconn->iotimeout > 0)
  {
    if (dlp_setioalarm (dlconn->iotimeout))
    {
      dl_log_r (dlconn, 2, 0, "[%s] error setting network I/O timeout\n",
                dlconn->addr);
    }
  }

  /* Recv until readlen bytes have been read */
  while (nread < (int64_t)readlen)
  {
    if ((nrecv = recv (dlconn->link, bptr, readlen - nread, 0)) < 0)
    {
      /* The only acceptable error is no data on non-blocking */
      if (!blockflag && !dlp_noblockcheck ())
      {
        /* Only break out if no data has yet been received */
        if (nread == 0)
          break;

        /* Corner case: if some data has been received in non-blocking
		 mode we will loop forever until readlen bytes are read */
      }
      else
      {
        dl_log_r (dlconn, 2, 0, "[%s] recv(%d): %d %s\n",
                  dlconn->addr, dlconn->link, nrecv, dlp_strerror ());
        nread = -2;
        break;
      }
    }

    /* Peer completed an orderly shutdown */
    if (nrecv == 0)
    {
      nread = -1;
      break;
    }

    /* Update recv pointer and byte count */
    if (nrecv > 0)
    {
      bptr += nrecv;
      nread += nrecv;
    }
  }

  /* Cancel timeout alarm if set */
  if (dlconn->iotimeout > 0)
  {
    if (dlp_setioalarm (0))
    {
      dl_log_r (dlconn, 2, 0, "[%s] error cancelling network I/O timeout\n",
                dlconn->addr);
    }
  }

  /* Set socket to non-blocking if set to blocking */
  if (blockflag)
  {
    if (dlp_socknoblock (dlconn->link))
    {
      dl_log_r (dlconn, 2, 0, "[%s] Error setting socket to non-blocking: %s\n",
                dlconn->addr, dlp_strerror ());
      return -2;
    }
  }

  return nread;
} /* End of dl_recvdata() */

/***********************************************************************/ /**
 * @brief Receive DataLink packet header
 *
 * Receive a DataLink packet header and place it into @a buffer up to
 * @a buflen bytes in length.
 *
 * The header body returned in @a buffer will always be NULL
 * terminated.  The buffer must be at least 255 bytes in size.  The
 * maximum header length is effectively 254 bytes.
 *
 * @return number of bytes read on success
 * @retval 0 when no data available on non-blocking socket
 * @retval -1 on connection shutdown
 * @retval -2 on error.
 ***************************************************************************/
int
dl_recvheader (DLCP *dlconn, void *buffer, size_t buflen, uint8_t blockflag)
{
  int bytesread = 0;
  int headerlen;
  char *cbuffer = buffer;

  if (!dlconn || !buffer)
  {
    return -2;
  }

  if (buflen < 255)
  {
    dl_log_r (dlconn, 2, 0, "[%s] dl_recvheader(): buffer length to small (%zd)\n",
              dlconn->addr, buflen);
    return -2;
  }

  /* Receive synchronization bytes and header length */
  if ((bytesread = dl_recvdata (dlconn, buffer, 3, blockflag)) != 3)
  {
    /* Bytes read but not 3 is an error */
    if (bytesread > 0)
      return -2;
    else
      return bytesread;
  }

  /* Test synchronization bytes */
  if (cbuffer[0] != 'D' || cbuffer[1] != 'L')
  {
    dl_log_r (dlconn, 2, 0, "[%s] No DataLink packet detected\n",
              dlconn->addr);
    return -2;
  }

  /* 3rd byte is the header length */
  headerlen = (uint8_t)cbuffer[2];

  /* Receive header payload blocking until completely received */
  if ((bytesread = dl_recvdata (dlconn, buffer, headerlen, 1)) != headerlen)
  {
    /* Bytes read but not headerlen is an error */
    if (bytesread > 0)
      return -2;
    else
      return bytesread;
  }

  /* Make sure reply is NULL terminated */
  if (bytesread == (int64_t)buflen)
    cbuffer[bytesread - 1] = '\0';
  else
    cbuffer[bytesread] = '\0';

  return bytesread;
} /* End of dl_recvheader() */
