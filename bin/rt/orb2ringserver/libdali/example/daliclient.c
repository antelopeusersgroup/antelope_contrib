/***************************************************************************
 * daliclient.c
 *
 * An example DataLink client demonstrating the use of libdali.
 *
 * Connects to a DataLink server, configures a connection and collects
 * data.  Detailed information about the data received can be printed.
 *
 * Written by Chad Trabant, IRIS Data Management Center
 *
 * modified 2013.210
 ***************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include <libdali.h>

#ifndef WIN32
  #include <signal.h>

  static void term_handler (int sig);
#else
  #define strcasecmp _stricmp
#endif

#define PACKAGE "daliclient"
#define VERSION LIBDALI_VERSION

static short int verbose   = 0;
static char *statefile     = 0;	    /* State file for saving/restoring state */
static char *matchpattern  = 0;	    /* Source ID matching expression */
static char *rejectpattern = 0;	    /* Source ID rejecting expression */
static char *infotype      = 0;	    /* INFO type to request */

static DLCP *dlconn;	            /* Connection parameters */

static int parameter_proc (int argcount, char **argvec);
static void usage (void);

int
main (int argc, char **argv)
{
  DLPacket dlpack;
  char packetdata[MAXPACKETSIZE];
  char timestr[50];
  char *infobuf = 0;
  int infolen;
  int endflag = 0;
  
#ifndef WIN32
  /* Signal handling, use POSIX calls with standardized semantics */
  struct sigaction sa;
  
  sigemptyset (&sa.sa_mask);
  sa.sa_flags   = SA_RESTART;
  
  sa.sa_handler = term_handler;
  sigaction (SIGINT, &sa, NULL);
  sigaction (SIGQUIT, &sa, NULL);
  sigaction (SIGTERM, &sa, NULL);
  
  sa.sa_handler = SIG_IGN;
  sigaction (SIGHUP, &sa, NULL);
  sigaction (SIGPIPE, &sa, NULL);
#endif
  
  /* Process given parameters (command line and parameter file) */
  if ( parameter_proc (argc, argv) < 0 )
    {
      fprintf (stderr, "Parameter processing failed\n\n");
      fprintf (stderr, "Try '-h' for detailed help\n");
      return -1;
    }
  
  /* Connect to server */
  if ( dl_connect (dlconn) < 0 )
    {
      fprintf (stderr, "Error connecting to server\n");
      return -1;
    }
  
  /* Reposition connection */
  if ( dlconn->pktid > 0 )
    {
      if ( dl_position (dlconn, dlconn->pktid, dlconn->pkttime) < 0 )
	return -1;
    }
  
  /* Send match pattern if supplied */
  if ( matchpattern )
    {
      if ( dl_match (dlconn, matchpattern) < 0 )
	return -1;
    }
  
  /* Send reject pattern if supplied */
  if ( rejectpattern )
    {
      if ( dl_reject (dlconn, rejectpattern) < 0 )
	return -1;
    }

  /* Request INFO and print returned XML */
  if ( infotype )
    {
      if ( (infolen = dl_getinfo (dlconn, infotype, matchpattern, &infobuf, 0)) < 0 )
	{
	  dl_log (2, 0, "Problem requesting INFO from server\n");
	  return -1;
	}
      
      printf ("%.*s\n", infolen, infobuf);
      
      if ( infobuf )
	free (infobuf);
    }
  /* Otherwise collect packets in STREAMing mode */
  else
    {
      /* Collect packets in streaming mode */
      while ( dl_collect (dlconn, &dlpack, packetdata, sizeof(packetdata), endflag) == DLPACKET )
	{
	  dl_dltime2seedtimestr (dlpack.datastart, timestr, 1);
	  
	  dl_log (0, 0, "Received %s (%lld), %s, %d\n",
		  dlpack.streamid, dlpack.pktid, timestr, dlpack.datasize);
	}
    }
  
  /* Make sure everything is shut down and save the state file */
  if ( dlconn->link != -1 )
    dl_disconnect (dlconn);
  
  /* Save the state file */
  if ( statefile )
    dl_savestate (dlconn, statefile);

  if ( dlconn )
    dl_freedlcp (dlconn);
  
  return 0;
}  /* End of main() */


/***************************************************************************
 * parameter_proc:
 *
 * Process the command line parameters.
 *
 * Returns 0 on success, and -1 on failure
 ***************************************************************************/
static int
parameter_proc (int argcount, char **argvec)
{
  char *address = 0;
  int keepalive = -1;
  int optind;
  
  /* Process all command line arguments */
  for (optind = 1; optind < argcount; optind++)
    {
      if (strcmp (argvec[optind], "-V") == 0)
	{
	  fprintf(stderr, "%s version: %s\n", PACKAGE, VERSION);
	  exit (0);
	}
      else if (strcmp (argvec[optind], "-h") == 0)
	{
	  usage();
	  exit (0);
	}
      else if (strncmp (argvec[optind], "-v", 2) == 0)
	{
	  verbose += strspn (&argvec[optind][1], "v");
	}
      else if (strcmp (argvec[optind], "-k") == 0)
	{
	  keepalive = strtoul (argvec[++optind], NULL, 10);
	}
      else if (strcmp (argvec[optind], "-m") == 0)
	{
	  matchpattern = argvec[++optind];
	}
      else if (strcmp (argvec[optind], "-r") == 0)
	{
	  rejectpattern = argvec[++optind];
	}
      else if (strcmp (argvec[optind], "-i") == 0)
	{
	  infotype = argvec[++optind];
	}
      else if (strcmp (argvec[optind], "-x") == 0)
	{
	  statefile = argvec[++optind];
	}
      else if (strncmp (argvec[optind], "-", 1 ) == 0)
	{
	  fprintf(stderr, "Unknown option: %s\n", argvec[optind]);
	  exit (1);
	}
      else if ( ! address )
	{
	  address = argvec[optind];
	}
      else
	{
	  fprintf(stderr, "Unknown option: %s\n", argvec[optind]);
	  exit (1);
	}
    }
  
  /* Make sure a server was specified */
  if ( ! address )
    {
      fprintf(stderr, "No DataLink server specified\n\n");
      fprintf(stderr, "%s version: %s\n\n", PACKAGE, VERSION);
      fprintf(stderr, "Usage: %s [options] [host][:][port]\n", PACKAGE);
      fprintf(stderr, "Try '-h' for detailed help\n");
      exit (1);
    }
  
  /* Allocate and initialize a new connection description */
  dlconn = dl_newdlcp (address, argvec[0]);
  
  /* Set keepalive parameter, allow for valid value of 0 */
  if ( keepalive >= 0 )
    dlconn->keepalive = keepalive;
  
  /* Initialize the verbosity for the dl_log function */
  dl_loginit (verbose, NULL, NULL, NULL, NULL);
  
  /* Report the program version */
  dl_log (0, 1, "%s version: %s\n", PACKAGE, VERSION);
  
  /* Load the match stream list from a file if the argument starts with '@' */
  if ( matchpattern && *matchpattern == '@' )
    {
      char *filename = matchpattern + 1;
      
      if ( ! (matchpattern = dl_read_streamlist (dlconn, filename)) )
	{
	  dl_log (2, 0, "Cannot read matching list file: %s\n", filename);
	  exit (1);
	}
    }

  /* Load the reject stream list from a file if the argument starts with '@' */
  if ( rejectpattern && *rejectpattern == '@' )
    {
      char *filename = rejectpattern + 1;
      
      if ( ! (rejectpattern = dl_read_streamlist (dlconn, filename)) )
	{
	  dl_log (2, 0, "Cannot read rejecting list file: %s\n", filename);
	  exit (1);
	}
    }
  
  /* Recover from the state file and reposition */
  if ( statefile )
    {
      if ( dl_recoverstate (dlconn, statefile) < 0 )
	{
	  dl_log (2, 0, "Error reading state file\n");
	  exit (1);
	}
    }
  
  return 0;
}  /* End of parameter_proc() */


/***************************************************************************
 * usage:
 * Print the usage message and exit.
 ***************************************************************************/
static void
usage (void)
{
  fprintf (stderr, "\nUsage: %s [options] [host][:][port]\n\n", PACKAGE);
  fprintf (stderr,
	   " ## General program options ##\n"
	   " -V             report program version\n"
	   " -h             show this usage message\n"
	   " -v             be more verbose, multiple flags can be used\n"
	   " -k secs        specify keepalive interval in seconds\n"
	   " -m match       specify stream ID matching pattern\n"
	   " -r reject      specify stream ID rejecting pattern\n"
	   " -i type        request INFO type, print XML and exit\n"
	   " -x statefile   save/restore stream state information to this file\n"
	   "\n"
	   " [host][:][port]  Address of the DataLink server in host:port format\n"
	   "                  if host is omitted (i.e. ':16000'), localhost is assumed\n"
	   "                  if :port is omitted (i.e. 'localhost'), 16000 is assumed\n\n");
  
}  /* End of usage() */

#ifndef WIN32
/***************************************************************************
 * term_handler:
 * Signal handler routine. 
 ***************************************************************************/
static void
term_handler (int sig)
{
  dl_terminate (dlconn);
}
#endif

