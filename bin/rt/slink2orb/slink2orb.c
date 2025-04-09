/***************************************************************************
 * slink2orb.c
 *
 * A SeedLink to Antelope ORB module
 *
 * Written by Chad Trabant,
 *       ORFEUS/EC-Project MEREDIAN
 * then: IRIS Data Management Center
 *  now: EarthScope Data Services
 ***************************************************************************/

#include <signal.h>

#include <Pkt.h>
#include <arrays.h>
#include <bury.h>
#include <coords.h>
#include <orb.h>
#include <tr.h>

#include <libslink.h>
#include <libmseed.h>

#include "mseed2orbpkt.h"

static char *version = "5.0 (2025.098 DEV)";
static char *package = "slink2orb";
static char verbose  = 0;
static char remap    = 0; /* remap sta and chan from SEED tables */

static int orb         = -1;             /* the ORB descriptor */
static char *statefile = NULL;           /* state file */
static char *paramfile = "slink2orb.pf"; /* parameter file with default */
static char *orbaddr   = NULL;           /* the host:port of the destination ORB */
static char *mappingdb = NULL;           /* the database for SEED name mapping */
static char *calibdb   = NULL;           /* the database for calibration info */
static char *selectors = NULL;           /* default SeedLink selectors */
static int stateint    = 100;            /* interval to save the state file (pkts) */
char auth_buffer[1024] = {0};            /* Buffer for SeedLink authentication */

static SLCD *slconn = NULL;

static void packet_handler (const SLpacketinfo *packetinfo, const char *payload);
static int parameter_proc (int argcount, char **argvec);
static void report_environ ();
static void elog_printlog (char const *msg);
static void elog_printerr (char const *msg);
static void usage (void);

int
main (int argc, char **argv)
{
  Dbptr dbase;
  static char plbuffer[SL_MAX_PAYLOAD];  /* payload buffer */
  const SLpacketinfo *packetinfo = NULL; /* packet information */
  int retval;
  int packetcnt = 0;

  /* Allocate and initialize a new connection description */
  slconn = sl_initslcd (package, version);

  /* Process given parameters (command line and parameter file) */
  if (parameter_proc (argc, argv) < 0)
  {
    sl_log (2, 0, "Parameter processing failed.\n");
    return -1;
  }

  /* Set signal handlers for terminating connections */
  if (sl_set_termination_handler (slconn))
  {
    sl_log (2, 0, "sl_set_termination_handler() failed\n");
    return -1;
  }

  /* Print important parameters if verbose enough */
  if (verbose >= 3)
    report_environ ();

  /* ORB setup */
  orb = orbopen (orbaddr, "w&");
  if (orb < 0)
  {
    sl_log (2, 0, "%s: orbopen() error for %s\n", package, orbaddr);
    exit (1);
  }

  /* Database setup */
  if (mappingdb)
  {
    if (dbopen (mappingdb, "r+", &dbase) == dbINVALID)
    {
      sl_log (2, 0, "dbopen(%s) failed\n", mappingdb);
      exit (1);
    }
    finit_db (dbase);
  }

  /* Loop with the connection manager */
  while ((retval = sl_collect (slconn, &packetinfo,
                               plbuffer, (uint32_t)sizeof (plbuffer))) != SLTERMINATE)
  {
    /* Check if a packet arrived */
    if (retval == SLPACKET)
    {
      packet_handler (packetinfo, plbuffer);

      /* Save intermediate state */
      if (statefile && stateint)
      {
        if (++packetcnt >= stateint)
        {
          sl_savestate (slconn, statefile);
          packetcnt = 0;
        }
      }
    }
    /* Otherwise log unexpected */
    else
    {
      if (retval == SLTOOLARGE)
        sl_log (2, 0, "%s: Error - packet too large for receiving buffer\n", package);
      else if (retval == SLNOPACKET)
        sl_log (2, 0, "%s: Error - sl_collect() returned SLNOPACKET unexpectedly\n", package);
      else
        sl_log (2, 0, "%s: Error - sl_collect() returned unexpected value: %d\n", package, retval);

      break;
    }
  }

  /* Shutdown */
  sl_disconnect (slconn);

  if (orb != -1)
    orbclose (orb);

  if (statefile)
    sl_savestate (slconn, statefile);

  /* flush the error log just in case */
  elog_print (stdout, 0);

  return 0;
} /* End of main() */

/***************************************************************************
 * packet_handler():
 * Process a received packet based on blocktype
 ***************************************************************************/
void
packet_handler (const SLpacketinfo *packetinfo, const char *payload)
{
  static char *packet = NULL;
  static int packetsz = 0;
  char srcname[ORBSRCNAME_SIZE];
  double time;
  int retval = 0;
  int nbytes = 0;

  if (verbose >= 2)
    sl_log (0, 2, "Received %u bytes of payload for %.*s, format %s, SeedLink sequence: %" PRIu64 "\n",
            packetinfo->payloadlength,
            (int)packetinfo->stationidlength,
            packetinfo->stationid,
            sl_formatstr (packetinfo->payloadformat, packetinfo->payloadsubformat),
            packetinfo->seqnum);

  /* Process miniSEED records and send them on */
  if (packetinfo->payloadformat == SLPAYLOAD_MSEED2 ||
      packetinfo->payloadformat == SLPAYLOAD_MSEED3)
  {
    retval = mseed2orbpkt (packetinfo->payloadformat, payload, packetinfo->payloadlength,
                           calibdb, mappingdb, remap,
                           srcname, &time, &packet, &nbytes, &packetsz, verbose);

    if (retval == 0)
    {
      if (verbose >= 3)
      {
        char *timestr = strydtime (time);
        sl_log (0, 3, "Putting %s, %s, %d bytes\n", srcname, timestr, nbytes);
        free (timestr);
      }

      if (orbput (orb, srcname, time, packet, nbytes))
        sl_log (2, 0, "orbput() failed: %s(%d)\n", srcname, nbytes);
    }
    else if (retval < 0)
    {
      sl_log (2, 0, "%s record not forwarded, mseed2orbpkt() returned: %d\n",
              packetinfo->stationid, retval);
    }
  }
  else
  {
    sl_log (2, 0, "%s: Error - unhandled payload format: %s\n",
            package,
            sl_formatstr (packetinfo->payloadformat, packetinfo->payloadsubformat));
  }
} /* End of packet_handler() */

/***************************************************************************
 * auth_value:
 *
 * A callback function returning the auth_buffer, which is a string to
 * be sumitted with the SeedLink AUTH command.
 *
 * Returns authorization value string on success, and NULL on failure
 ***************************************************************************/
const char *
auth_value (const char *server, void *data)
{
  (void)server; /* Server name is not used in this case */
  (void)data;   /* User-supplied data is not used in this case */

  return auth_buffer;
}

/***************************************************************************
 * parameter_proc():
 * Process the command line and parameter file parameters.
 *
 * Returns 0 on success, and -1 on failure
 ***************************************************************************/
static int
parameter_proc (int argcount, char **argvec)
{
  char *stationid;
  char *staselector;
  char *seedlinkaddr = NULL;
  char *key;
  char *tptr;
  Pf *pf        = NULL;
  Arr *stations = NULL;
  Tbl *stakeys;

  int netto_argv     = 0;
  int netdly_argv    = 0;
  int keepalive_argv = 0;

  if (argcount <= 2)
    usage ();

  /* Process all but last 2 command line arguments */
  for (optind = 1; optind < argcount; optind++)
  {
    if (strncmp (argvec[optind], "-v", 2) == 0)
    {
      verbose += strspn (&argvec[optind][1], "v");
    }
    else if (strcmp (argvec[optind], "-S") == 0)
    {
      statefile = argvec[++optind];
    }
    else if (strcmp (argvec[optind], "-pf") == 0)
    {
      paramfile = argvec[++optind];
    }
    else if (strcmp (argvec[optind], "-nt") == 0)
    {
      slconn->netto = atoi (argvec[++optind]);
      netto_argv    = 1; /* parameter file will not override */
    }
    else if (strcmp (argvec[optind], "-nd") == 0)
    {
      slconn->netdly = atoi (argvec[++optind]);
      netdly_argv    = 1; /* parameter file will not override */
    }
    else if (strcmp (argvec[optind], "-k") == 0)
    {
      slconn->keepalive = atoi (argvec[++optind]);
      keepalive_argv    = 1; /* parameter file will not override */
    }
    else if (strcmp (argvec[optind], "-dc") == 0)
    {
      calibdb = argvec[++optind];
    }
    else if (strcmp (argvec[optind], "-dm") == 0)
    {
      mappingdb = argvec[++optind];
    }
    else if (strcmp (argvec[optind], "-mbi") == 0)
    {
      elog_complain (0, "Argument -mbi is no longer supported, ignoring\n");
    }
    else if (strcmp (argvec[optind], "-r") == 0)
    {
      remap = 1;
    }
    else
    {
      if (seedlinkaddr == NULL)
      {
        seedlinkaddr = argvec[optind];
      }
      else if (orbaddr == NULL)
      {
        orbaddr = argvec[optind];
      }
      else
      {
        elog_complain (0, "Unknown argument: %s\n", argvec[optind]);
        usage ();
      }
    }
  }

  /* Initialize the Antelope elog printing system */
  Program_Name = argvec[0];
  elog_init (argcount, argvec);

  /* Initialize the verbosity for the libslink functions */
  sl_loginit (verbose, &elog_printlog, " ", &elog_printerr, "!");

  sl_log (0, 0, "%s version %s\n", package, version);

  if (seedlinkaddr == NULL || orbaddr == NULL)
  {
    if (seedlinkaddr == NULL)
      sl_log (2, 0, "SeedLink address not specified, try -h for usage\n");

    if (orbaddr == NULL)
      sl_log (2, 0, "ORB address not specified, try -h for usage\n");

    exit (1);
  }

  sl_set_serveraddress (slconn, seedlinkaddr);

  /* Read parameter file */
  if ((pfread (paramfile, &pf)) < 0)
  {
    sl_log (2, 0, "error reading parameter file: %s\n", paramfile);
    exit (1);
  }
  else
  {
    /* Only read network timeout if not set on command line */
    if (!netto_argv)
      if ((tptr = pfget_string (pf, "nettimeout")))
        slconn->netto = atoi ((char *)tptr);

    /* Only read network reconnect delay if not set on command line */
    if (!netdly_argv)
      if ((tptr = pfget_string (pf, "netdelay")))
        slconn->netdly = atoi ((char *)tptr);

    /* Only read keepalive interval if not set on command line */
    if (!keepalive_argv)
      if ((tptr = pfget_string (pf, "keepalive")))
        slconn->keepalive = atoi ((char *)tptr);

    if ((tptr = pfget_string (pf, "stateint")))
      stateint = atoi ((char *)tptr);

    if ((tptr = pfget_string (pf, "selectors")) && strlen (tptr) > 0)
      selectors = strdup ((char *)tptr);

    /* 'stations' == 0 if not found */
    stations = pfget_arr (pf, "stations");

    slconn->tls = (pfget_boolean (pf, "tls") == -1) ? 1 : 0;

    if ((tptr = pfget_string (pf, "tls_ca_file")))
    {
      if (setenv ("LIBSLINK_CA_CERT_FILE", tptr, 1))
      {
        sl_log (2, 0, "Error setting LIBSLINK_CA_CERT_FILE environment variable\n");
      }
    }

    if ((tptr = pfget_string (pf, "tls_ca_path")))
    {
      if (setenv ("LIBSLINK_CA_CERT_PATH", tptr, 1))
      {
        sl_log (2, 0, "Error setting LIBSLINK_CA_CERT_PATH environment variable\n");
      }
    }

    if ((tptr = pfget_string (pf, "tls_debug")))
    {
      if (setenv ("LIBSLINK_TLS_DEBUG", tptr, 1))
      {
        sl_log (2, 0, "Error setting LIBSLINK_TLS_DEBUG environment variable\n");
      }
    }

    if ((tptr = pfget_string (pf, "tls_unverified_cert_ok")))
    {
      if (setenv ("LIBSLINK_CERT_UNVERIFIED_OK", tptr, 1))
      {
        sl_log (2, 0, "Error setting LIBSLINK_CERT_UNVERIFIED_OK environment variable\n");
      }
    }

    if ((tptr = pfget_string (pf, "userpass")))
    {
      char user[512] = {0};
      char pass[512] = {0};

      if (sscanf (tptr, "%511s %511s", user, pass) == 2)
      {
        snprintf (auth_buffer, sizeof (auth_buffer), "USERPASS %s %s", user, pass);
        sl_set_auth_params (slconn, auth_value, NULL, NULL);
      }
      else
      {
        sl_log (2, 0, "userpass parameter: User and Pass fields are not both specified\n");
        sl_log (2, 0, "userpass value: '%s'\n", tptr);
        return -1;
      }
    }
  }

  /* Translate the 'stations' Arr, if given */
  if (stations)
  {
    slconn->multistation = 1;
    stakeys              = keysarr (stations);
    if (maxtbl (stakeys) <= 0)
    {
      sl_log (2, 0, "'stations' array in pf is empty!\n");
      return -1;
    }

    /* Go through each entry in the 'stations' Arr */
    while ((key = (char *)shifttbl (stakeys)) != NULL)
    {
      stationid = strdup (key);

      /* Retrieve selectors for this station, if none use default */
      tptr = (char *)getarr (stations, key);
      if (strlen (tptr) == 0)
      {
        staselector = selectors;
      }
      else
      {
        staselector = tptr;
      }

      /* Add stationID */
      sl_add_stream (slconn, stationid, staselector, SL_UNSETSEQUENCE, NULL);

      free (stationid);
    }

    /* Free the Arr and Tbl */
    freearr (stations, 0);
    freetbl (stakeys, 0);
  }
  else
  { /* No 'stations' array, assuming all-station mode */
    sl_set_allstation_params (slconn, selectors, SL_UNSETSEQUENCE, NULL);
  }

  /* Free the parameter file structure */
  if (paramfile)
  {
    pffree (pf);
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
 * report_environ():
 * Report the state of global variables, for testing.
 ***************************************************************************/
static void
report_environ ()
{
  int stacount;
  SLstream *curstation;

  if (statefile)
    sl_log (0, 0, "statefile: %s\n", statefile);
  else
    sl_log (0, 0, "'statefile' not defined\n");

  sl_log (0, 0, "stateint: %d\n", stateint);

  if (paramfile)
    sl_log (0, 0, "paramfile: %s\n", paramfile);
  else
    sl_log (0, 0, "'paramfile' not defined\n");

  if (mappingdb)
    sl_log (0, 0, "mappingdb: %s\n", mappingdb);
  else
    sl_log (0, 0, "'mappingdb' not defined\n");

  if (calibdb)
    sl_log (0, 0, "calibdb: %s\n", calibdb);
  else
    sl_log (0, 0, "'calibdb' not defined\n");

  sl_log (0, 0, "verbose: %d\n", verbose);
  sl_log (0, 0, "remap: %d\n", remap);
  sl_log (0, 0, "nettimeout: %d\n", slconn->netto);
  sl_log (0, 0, "netdelay: %d\n", slconn->netdly);
  sl_log (0, 0, "keepalive: %d\n", slconn->keepalive);
  sl_log (0, 0, "tls: %d\n", slconn->tls);

  sl_log (0, 0, "LIBSLINK_CA_CERT_FILE: %s\n",
          getenv ("LIBSLINK_CA_CERT_FILE") ? getenv ("LIBSLINK_CA_CERT_FILE") : "[not set]");

  sl_log (0, 0, "LIBSLINK_CA_CERT_PATH: %s\n",
          getenv ("LIBSLINK_CA_CERT_PATH") ? getenv ("LIBSLINK_CA_CERT_PATH") : "[not set]");

  sl_log (0, 0, "LIBSLINK_TLS_DEBUG: %s\n",
          getenv ("LIBSLINK_TLS_DEBUG") ? getenv ("LIBSLINK_TLS_DEBUG") : "[not set]");

  sl_log (0, 0, "LIBSLINK_CERT_UNVERIFIED_OK: %s\n",
          getenv ("LIBSLINK_CERT_UNVERIFIED_OK") ? getenv ("LIBSLINK_CERT_UNVERIFIED_OK") : "[not set]");

  if (auth_buffer[0] != 0)
  {
    sl_log (0, 0, "auth value: %s\n", auth_buffer);
  }
  else
  {
    sl_log (0, 0, "'auth value' not defined\n");
  }

  if (selectors)
    sl_log (0, 0, "selectors: %s\n", selectors);
  else
    sl_log (0, 0, "'selectors' not defined\n");

  if (orbaddr)
    sl_log (0, 0, "orbaddr: %s\n", orbaddr);
  else
    sl_log (0, 0, "'orbaddr' not defined\n");

  if (slconn->sladdr)
    sl_log (0, 0, "sladdr: %s\n", slconn->sladdr);
  else
    sl_log (0, 0, "'slconn->sladdr' not defined\n");

  sl_log (0, 0, "link: %d\n", slconn->link);

  sl_log (0, 0, "slconn->multistation: %d\n", slconn->multistation);

  stacount   = 0;
  curstation = slconn->streams;

  sl_log (0, 0, "'stations' array:\n");
  while (curstation != NULL)
  {
    sl_log (0, 0, "  %d - station ID: %s\n",
            stacount, curstation->stationid);

    sl_log (0, 0, "  %d - selectors: %s\n",
            stacount, (curstation->selectors) ? curstation->selectors : "[not set]");

    if (curstation->seqnum == SL_UNSETSEQUENCE)
      sl_log (0, 0, "  %d - seqnum: [not set]\n", stacount);
    else
      sl_log (0, 0, "  %d - seqnum: %" PRIu64 "\n", stacount, curstation->seqnum);

    sl_log (0, 0, "  %d - timestamp: '%s'\n", stacount, curstation->timestamp);

    stacount++;
    curstation = curstation->next;
  }
} /* End of report_environ() */

/***************************************************************************
 * elog_printlog():
 * A hook to re-direct sl_log() (libslink) log messages to elog_notify.
 ***************************************************************************/
static void
elog_printlog (char const *msg)
{
  if (msg)
    elog_notify (0, "%s", msg + 1);
}

/***************************************************************************
 * elog_printerr():
 * A hook to re-direct sl_log() (libslink) error messages to elog_complain.
 ***************************************************************************/
static void
elog_printerr (char const *msg)
{
  if (msg)
  {
    if (msg[0] == '!')
      elog_complain (0, "%s", msg + 1);
    else
      elog_notify (0, "%s", msg + 1);
  }
}

/***************************************************************************
 * usage():
 * Print the usage message and exit.
 ***************************************************************************/
static void
usage (void)
{
  printf ("\n%s version %s\n", package, version);
  printf ("\n"
          "Usage: slink2orb [-dc database] [-dm database] [-nd delay] [-nt timeout]\n"
          "                 [-k interval] [-pf parameterfile] [-S statefile]\n"
          "                 [-mbi] [-r] [-v] SeedLink ORB\n"
          "\n"
          "Antelope Contributed Software\n"
          "\n"
          "Chad Trabant\n"
          "ORFEUS/EC-Project MEREDIAN\n"
          "IRIS Data Management Center\n"
          "EarthScope Data Services\n"
          "\n"
          "Please report problems at https://github.com/antelopeusersgroup/antelope_contrib/issues\n"
          "\n");

  exit (1);
}
