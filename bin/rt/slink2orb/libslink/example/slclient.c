/***************************************************************************
 * slclient.c
 * An example SeedLink client demonstrating the use of libslink.
 *
 * Connects to a SeedLink server, configures a connection. collects data,
 * and prints a summary of the received packets.
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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include <libslink.h>

#define PACKAGE "slclient"
#define VERSION LIBSLINK_VERSION

static short int verbose  = 0;
static short int ppackets = 0;
static char *statefile    = 0; /* state file for saving/restoring state */

static void packet_handler (SLCD *slconn, const SLpacketinfo *packetinfo,
                            const char *payload, uint32_t payloadlen);
static int parameter_proc (SLCD *slconn, int argcount, char **argvec);
static const char *auth_value (const char *server, void *data);
static void auth_finish  (const char *server, void *data);
static void usage (void);

static char auth_buffer[1024] = {0};

int
main (int argc, char **argv)
{
  SLCD *slconn = NULL; /* connection parameters */

  const SLpacketinfo *packetinfo = NULL; /* packet information */

  char *plbuffer = NULL;
  uint32_t plbuffersize = 16384;
  int status;

  /* Allocate and initialize a new connection description */
  slconn = sl_initslcd (PACKAGE, VERSION);

  /* Process given parameters (command line and parameter file) */
  if (parameter_proc (slconn, argc, argv) < 0)
  {
    fprintf (stderr, "Parameter processing failed\n\n");
    fprintf (stderr, "Try '-h' for detailed help\n");
    return -1;
  }

  /* Set signal handlers to trigger clean connection shutdown */
  if (sl_set_termination_handler (slconn) < 0)
  {
    sl_log (2, 0, "Failed to set termination handler\n");
    return -1;
  }

  /* Allocate payload buffer */
  if ((plbuffer = (char *)malloc (plbuffersize)) == NULL)
  {
    sl_log (2, 0, "Memory allocation failed\n");
    return -1;
  }

  /* Loop with the connection manager */
  while ((status = sl_collect (slconn, &packetinfo,
                               plbuffer, plbuffersize)) != SLTERMINATE)
  {
    if (status == SLPACKET)
    {
      /* Here we do something with the packet */
      packet_handler (slconn, packetinfo, plbuffer, packetinfo->payloadcollected);
    }
    else if (status == SLTOOLARGE)
    {
      /* Here we could increase the payload buffer size to accommodate if desired.
       * If you wish to increase the buffer size be sure to copy any data that might
       * have already been collected from the old buffer to the new.  realloc() does this. */
      sl_log (2, 0, "received payload length %u too large for max buffer of %u\n",
              packetinfo->payloadlength, plbuffersize);

      break;
    }
    else if (status == SLNOPACKET)
    {
      /* Here should only occur when non-blocking, i.e. slconn->noblock == 1 */
      sl_log (0, 2, "sleeping after receiving no data from sl_collect()\n");
      sl_usleep(500000);
    }

    /* Here we could send an in-stream INFO request with sl_request_info() */
  }

  /* Make sure everything is shut down and save the state file */
  sl_disconnect (slconn);

  if (statefile)
    sl_savestate (slconn, statefile);

  sl_freeslcd (slconn);
  free (plbuffer);

  return 0;
} /* End of main() */

/***************************************************************************
 * packet_handler():
 * Process a received packet based on packet type.
 ***************************************************************************/
static void
packet_handler (SLCD *slconn, const SLpacketinfo *packetinfo,
                const char *payload, uint32_t payloadlength)
{
  char payloadsummary[128] = {0};
  double dtime;   /* Epoch time */
  double secfrac; /* Fractional part of epoch time */
  time_t itime;   /* Integer part of epoch time */
  char timestamp[30] = {0};
  struct tm *timep;
  int printed;

  /* Build a current local time string */
  dtime   = sl_dtime (void);
  secfrac = (double)((double)dtime - (int)dtime);
  itime   = (time_t)dtime;
  timep   = localtime (&itime);

  printed = snprintf (timestamp, sizeof (timestamp), "%04d-%03dT%02d:%02d:%02d.%01.0f",
                      timep->tm_year + 1900, timep->tm_yday + 1, timep->tm_hour,
                      timep->tm_min, timep->tm_sec, secfrac);

  if (printed >= sizeof (timestamp))
  {
    sl_log (1, 0, "%s() Time string overflow\n", __func__);
  }

  sl_log (0, 1, "%s, seq %" PRIu64 ", Received %u bytes of payload format %s\n",
          timestamp, packetinfo->seqnum, payloadlength,
          sl_formatstr(packetinfo->payloadformat, packetinfo->payloadsubformat));

  /* Print summary of the payload */
  if (sl_payload_summary (slconn->log, packetinfo, payload, payloadlength,
                          payloadsummary, sizeof (payloadsummary)) != -1)
  {
    sl_log (1, 1, "%s\n", payloadsummary);
  }
  else
  {
    sl_log (1, 1, "%s() Error generating payload summary\n", __func__);
  }

} /* End of packet_handler() */

/***************************************************************************
 * parameter_proc:
 *
 * Process the command line parameters.
 *
 * Returns 0 on success, and -1 on failure
 ***************************************************************************/
static int
parameter_proc (SLCD *slconn, int argcount, char **argvec)
{
  int optind;
  int error = 0;

  char *server_address = NULL;
  char *streamfile     = NULL;
  char *multiselect    = NULL;
  char *selectors      = NULL;

  if (argcount <= 1)
    error++;

  /* Process all command line arguments */
  for (optind = 1; optind < argcount; optind++)
  {
    if (strcmp (argvec[optind], "-V") == 0)
    {
      fprintf (stderr, "%s version: %s\n", PACKAGE, VERSION);
      exit (0);
    }
    else if (strcmp (argvec[optind], "-h") == 0)
    {
      usage ();
      exit (0);
    }
    else if (strncmp (argvec[optind], "-v", 2) == 0)
    {
      verbose += strspn (&argvec[optind][1], "v");
    }
    else if (strcmp (argvec[optind], "-p") == 0)
    {
      ppackets = 1;
    }
    else if (strcmp (argvec[optind], "-Ap") == 0)
    {
      sl_set_auth_params (slconn, auth_value, auth_finish, NULL);
    }
    else if (strcmp (argvec[optind], "-nt") == 0)
    {
      sl_set_idletimeout (slconn, atoi (argvec[++optind]));
    }
    else if (strcmp (argvec[optind], "-nd") == 0)
    {
      sl_set_reconnectdelay (slconn, atoi (argvec[++optind]));
    }
    else if (strcmp (argvec[optind], "-k") == 0)
    {
      sl_set_keepalive (slconn, atoi (argvec[++optind]));
    }
    else if (strcmp (argvec[optind], "-l") == 0)
    {
      streamfile = argvec[++optind];
    }
    else if (strcmp (argvec[optind], "-s") == 0)
    {
      selectors = argvec[++optind];
    }
    else if (strcmp (argvec[optind], "-S") == 0)
    {
      multiselect = argvec[++optind];
    }
    else if (strcmp (argvec[optind], "-x") == 0)
    {
      statefile = argvec[++optind];
    }
    else if (strncmp (argvec[optind], "-", 1) == 0)
    {
      fprintf (stderr, "Unknown option: %s\n", argvec[optind]);
      exit (1);
    }
    else if (server_address == NULL)
    {
      server_address = argvec[optind];
    }
    else
    {
      fprintf (stderr, "Unknown option: %s\n", argvec[optind]);
      exit (1);
    }
  }

  /* Make sure a server was specified */
  if (server_address == NULL)
  {
    fprintf (stderr, "%s version: %s\n\n", PACKAGE, VERSION);
    fprintf (stderr, "No SeedLink server specified\n\n");
    fprintf (stderr, "Usage: %s [options] [host][:port]\n", PACKAGE);
    fprintf (stderr, "Try '-h' for detailed help\n");
    exit (1);
  }

  sl_set_serveraddress (slconn, server_address);

  /* Initialize the verbosity for the sl_log function */
  sl_loginit (verbose, NULL, NULL, NULL, NULL);

  /* Report the program version */
  sl_log (0, 1, "%s version: %s\n", PACKAGE, VERSION);

  /* If errors then report the usage message and quit */
  if (error)
  {
    usage ();
    exit (1);
  }

  /* Load the stream list from a file if specified */
  if (streamfile)
    sl_add_streamlist_file (slconn, streamfile, selectors);

  /* Parse the 'multiselect' string following '-S' */
  if (multiselect)
  {
    if (sl_add_streamlist (slconn, multiselect, selectors) == -1)
      return -1;
  }
  else if (!streamfile)
  { /* No 'streams' array, assuming all-station mode */
    sl_set_allstation_params (slconn, selectors, SL_UNSETSEQUENCE, NULL);
  }

  /* Attempt to recover sequence numbers from state file */
  if (statefile)
  {
    if (sl_recoverstate (slconn, statefile) < 0)
    {
      sl_log (2, 0, "state recovery failed\n");
    }
  }

  return 0;
} /* End of parameter_proc() */

/***************************************************************************
 * auth_value:
 *
 * A callback function registered at SLCD.auth_value() that should return
 * a string to be sumitted with the SeedLink AUTH command.
 *
 * In this case, the function prompts the user for a username and password
 * for interactive input.
 *
 * Returns authorization value string on success, and NULL on failure
 ***************************************************************************/
static const char *
auth_value (const char *server, void *data)
{
  (void)data; /* User-supplied data is not used in this case */
  char username[256] = {0};
  char password[256] = {0};
  int printed;

  fprintf (stderr, "Enter username for %s: ", server);
  fgets (username, sizeof (username), stdin);
  username[strlen (username) - 1] = '\0';

  fprintf (stderr, "Enter password: ");
  fgets (password, sizeof (password), stdin);
  password[strlen (password) - 1] = '\0';

  /* Create AUTH value of "USERPASS <username> <password>" */
  printed = snprintf (auth_buffer, sizeof (auth_buffer),
                      "USERPASS %s %s",
                      username, password);

  if (printed >= sizeof (auth_buffer))
  {
    fprintf (stderr, "%s() Auth value is too large (%d bytes)\n", __func__, printed);

    return NULL;
  }

  return auth_buffer;
}

/***************************************************************************
 * auth_finish:
 *
 * A callback function registered at SLCD.auth_finish() that is called
 * after the AUTH command has been sent to the server.
 *
 * In this case, the function clears the memory used to store the
 * username and password populated by auth_value().
 ***************************************************************************/
static void
auth_finish (const char *server, void *data)
{
  (void)server; /* Server address is not used in this case */
  (void)data;   /* User-supplied data is not used in this case */

  /* Clear memory used to store auth value */
  memset (auth_buffer, 0, sizeof (auth_buffer));
}

/***************************************************************************
 * usage:
 * Print the usage message and exit.
 ***************************************************************************/
static void
usage (void)
{
  fprintf (stderr, "\nUsage: %s [options] [host][:port]\n\n", PACKAGE);
  fprintf (stderr,
           " ## General program options ##\n"
           " -V             report program version\n"
           " -h             show this usage message\n"
           " -v             be more verbose, multiple flags can be used\n"
           " -p             print details of data packets\n"
           " -Ap            prompt for authentication details (v4 only)\n"
           "\n"
           " -nd delay      network re-connect delay (seconds), default 30\n"
           " -nt timeout    network timeout (seconds), re-establish connection if no\n"
           "                  data/keepalives are received in this time, default 600\n"
           " -k interval    send keepalive packets this often (seconds)\n"
           " -x statefile   save/restore stream state information to this file\n"
           "\n"
           " ## Data stream selection ##\n"
           " -l listfile    read a stream list from this file for multi-station mode\n"
           " -s selectors   selectors for all-station or default for multi-station\n"
           " -S streams     select streams for multi-station\n"
           "   'streams' = 'stream1[:selectors1],stream2[:selectors2],...'\n"
           "        'stream' is in NET_STA format, for example:\n"
           "        -S \"IU_COLA:BHE BHN,GE_WLF,MN_AQU:HH?\"\n"
           "\n"
           " [host][:port]  Address of the SeedLink server in host:port format\n"
           "                  if host is omitted (i.e. ':18000'), localhost is assumed\n"
           "                  if :port is omitted (i.e. 'localhost'), 18000 is assumed\n"
           "\n");
} /* End of usage() */
