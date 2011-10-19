/***************************************************************************
 * rrp2orb.c:
 *
 * A small client to collect data from an NEIC RRP ring file and
 * inject the records as SEED-type packets into an ORB.  The received
 * records must contain blockette 1000s.
 *
 * Written by Chad Trabant, IRIS Data Management Center
 ***************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <stdarg.h>
#include <time.h>
#include <signal.h>
#include <regex.h>

#include <orb.h>
#include <Pkt.h>

#include "ring_reader.h"
#include "seedutils.h"
#include "mseed2orbpkt.h"

#define PACKAGE "rrp2orb"
#define VERSION "2010.292"

static void process_record (char *record, int reclen);
static int parameter_proc (int argcount, char **argvec);
static char *getoptval (int argcount, char **argvec, int argopt);
int lprintf (int level, const char *fmt, ...);
static void term_handler (int sig);
static void usage ();

static int   verbose     = 0;    /* Verbosity level */
static int   modulus     = 100;  /* Status file update modulus */
static int   shutdownsig = 0;    /* Shutdown signal flag */
static regex_t *match    = 0;    /* Filename match regex */
static char *netcode     = 0;    /* Forced network code, all records restamped */ 
static char *ringfile    = 0;    /* Name of RRP ring file */
static char *extstr      = 0;    /* Extention for ring reading state file */
static int   orb         = -1;   /* the ORB descriptor */
static char  remap       = 0;    /* remap sta and chan from SEED tables */
static char *orbaddr     = 0;    /* the host:port of the destination ORB */
static char *mappingdb   = 0;    /* the database for SEED name mapping */
static char *calibdb     = 0;    /* the database for calibration info */

static char  buf[4096];  /* The data buffer */

int
main (int argc,char **argv)
{
  Dbptr dbase;
  int reclen;
  int expected;
  int nout;

  /* Signal handling, use POSIX calls with standardized semantics */
  struct sigaction sa;
  
  sa.sa_flags = SA_RESTART;
  sigemptyset (&sa.sa_mask);
  
  sa.sa_handler = term_handler;
  sigaction (SIGINT, &sa, NULL);
  sigaction (SIGQUIT, &sa, NULL);
  sigaction (SIGTERM, &sa, NULL);
  
  sa.sa_handler = SIG_IGN;
  sigaction (SIGHUP, &sa, NULL);
  sigaction (SIGPIPE, &sa, NULL);
  
  /* Process given parameters (command line and parameter file) */
  if (parameter_proc (argc, argv) < 0)
    return -1;
  
  /* Initialize RRP ring */
  if ( rrp_init(ringfile, modulus, extstr) )
    return -1;
  
  /* ORB setup */
  orb = orbopen (orbaddr, "w&");
  if ( orb < 0 )
    {
      lprintf (0, "%s: orbopen() error for %s\n", Program_Name, orbaddr);
      exit (1);
    }
  
  /* Database setup */
  if ( mappingdb )
    {
      if ( dbopen(mappingdb, "r+", &dbase) == dbINVALID )
        {
          lprintf (0, "dbopen() failed for %s\n", mappingdb);
          exit (1);
        }
      
      finit_db(dbase);
    }
  
  expected = rrp_get_nextout();
  
  /* Main loop, collect and archive records */
  while ( ! shutdownsig )
    {
      nout = rrp_get_nextout();
      
      /* Sanity check: expected should be the nextout */
      if ( expected != nout )
	{
	  lprintf (0, "Not getting expected block. expected=%d nextout=%d last=%d\n",
		   expected, nout, rrp_get_lastseq());
	  expected = nout;
	}
      
      /* Get next packet */
      reclen = rrp_get_next(buf, &shutdownsig);
      
      if ( ! reclen )  /* Unknown record length, could assume 512 but let's not */
	{
	  lprintf (0, "rrp_get_next() returned a zero length record, skipping\n");
	}
      else if ( reclen < 0 )  /* Error or shutdown */
	{
	  shutdownsig = 1;
	}
      else  /* Received record, process it */
	{
	  if ( reclen != 512 && reclen != 4096 )
	    {
	      lprintf (0, "Unexpected record length: %d (seqno: %d)\n",
		       reclen, nout);
	    }
	  
	  /* Process record */
	  process_record ((char *) buf, reclen);
	}
      
      /* Increment and roll expected sequence number if needed */
      expected++;
      if ( expected > MAXSEQNUM )
	expected=0;
    }
  
  /* Shutdown ORB connection */
  if ( verbose > 1 )
    lprintf (0, "Shutting down ORB connection\n");
  if (orb != -1)
    orbclose(orb);
  
  /* Shutdown procedures for the ring reading */
  if ( verbose > 1 )
    lprintf (0, "Shutting down ring reading\n");
  rrp_shutdown();
  
  exit(0);
}

/***************************************************************************
 * process_record:
 *
 * Process received records by parsing the record header, creating a
 * "source name", performing any needed matching, and sending to the
 * archving routine (ds_streamproc).
 *
 ***************************************************************************/
static void
process_record (char *record, int reclen)
{
  /* Define some structures to pass to parse_record() */
  static struct s_fsdh *fsdh = NULL;
  static struct s_blk_1000 *blk_1000 = NULL;
  static char *packet = NULL;
  static int bufsize = 0;

  char  prtnet[4], prtsta[7];
  char  prtloc[4], prtchan[5];
  char *spcptr, *encoding;
  char  order[4], sourcename[50];
  int   parsed = 0;
  int   seedreclen;
  int   i,p;
  int   sendflag = 1;
  double time;
  char srcname[ORBSRCNAME_SIZE];
  int retval = 0;
  int nbytes = 0;
  
  /* Simple verification of a data record */
  if ( record[6] != 'D' && record[6] != 'R' && record[6] != 'Q' && record[6] != 'M' ) {
    lprintf (1, "Record header/quality indicator unrecognized!\n");
    return;
  }

  /* Make sure there is room for these structs */
  fsdh = (t_fsdh *) realloc(fsdh, sizeof(t_fsdh));
  blk_1000  = (t_blk_1000 *) realloc(blk_1000, sizeof(t_blk_1000));
  
  parsed = parse_record( (char *) record, reclen,
			 (t_fsdh *) fsdh, (t_blk_1000 *) blk_1000);
  
  if ( ! parsed ) {
    lprintf (1, "1000 blockette was NOT found!\n");
    return;
  }
  
  /* Most of this monkey business is so the data stream naming convention
     will be consistent even with oddly filled fields */
  strncpy(prtnet, fsdh->network_code, 2); prtnet[2] = '\0';
  strncpy(prtsta, fsdh->station_code, 5); prtsta[5] = '\0';
  strncpy(prtloc, fsdh->location_id, 2); prtloc[2] = '\0';
  strncpy(prtchan, fsdh->channel_id, 3); prtchan[3] = '\0';
  
  /* Cut trailing spaces. Assumes left justified fields */
  if ( (spcptr = strstr(prtnet, " ")) != NULL ) *spcptr = '\0';
  if ( (spcptr = strstr(prtsta, " ")) != NULL ) *spcptr = '\0';
  if ( (spcptr = strstr(prtloc, " ")) != NULL ) *spcptr = '\0';
  if ( (spcptr = strstr(prtchan, " ")) != NULL ) *spcptr = '\0';
  
  if (prtnet[0] != '\0') strcat(prtnet, "_");
  if (prtsta[0] != '\0') strcat(prtsta, "_");
  if (prtloc[0] != '\0') strcat(prtloc, "_");
  
  /* Build the source name string */
  sprintf( sourcename, "%.3s%.6s%.3s%.3s",
	   prtnet, prtsta, prtloc, prtchan );
  
  /* Calculate record size in bytes as 2^(blk_1000->rec_len) */
  for (p=1, i=1; i <= blk_1000->rec_len; i++) p *= 2;
  seedreclen = p;
  
  if (seedreclen != reclen) {
    lprintf (1, "Record was not expected size: %d, dropping\n", reclen);    
    sendflag = 0;
  }
  
  /* Big or little endian reported by the 1000 blockette? */
  if (blk_1000->word_swap == 0) strcpy(order, "LE");
  else if (blk_1000->word_swap == 1) strcpy(order, "BE");
  else strcpy(order, "??");
  
  /* Get a description of the encoding format */
  encoding = get_encoding (blk_1000->encoding);
  
  /* Force network code if supplied */
  if ( netcode ) {
    /* Update binary record */
    strncpy ( record+18, netcode, 2);
    
    /* Update parsed FSDH */
    strncpy ( fsdh->network_code, netcode, 2);
  }
  
  /* Do regex matching if an expression was specified */
  if ( match != 0 )
    if ( regexec (match, sourcename, (size_t) 0, NULL, 0) != 0 )
      sendflag = 0;
  
  if ( sendflag )
    {      
      if ( netcode )
	lprintf (2, "%s (Forced net: '%s'): %s, %d bytes, %s\n",
		 sourcename, netcode, order, seedreclen, encoding);
      else
	lprintf (2, "%s: %s, %d bytes, %s\n",
		 sourcename, order, seedreclen, encoding);
      
      /* Create SEED-type ORB packet */
      retval = mseed2orbpkt(record, reclen, calibdb, mappingdb,
			    remap, &srcname[0], &time, &packet,
			    &nbytes, &bufsize);
      
      if ( retval == 0 )
	{
	  /* Send data record to the ORB */
	  if ( orbput(orb, srcname, time, packet, nbytes) )
	  {
	    lprintf (0, "orbput() failed: %s(%d)\n", srcname, nbytes);
	    return;
	  }
	}
    }
  else
    {
      lprintf (2, "DROPPED %s: %s, %d bytes, %s\n",
	       sourcename, order, seedreclen, encoding);
    }
  
  return;
}


/***************************************************************************
 * parameter_proc:
 * Process the command line parameters.
 *
 * Returns 0 on success, and -1 on failure
 ***************************************************************************/
static int
parameter_proc (int argcount, char **argvec)
{
  char *matchstr = 0;
  int optind;
  
  /* Process all command line arguments */
  for (optind = 1; optind < argcount; optind++)
    {
      if (strcmp (argvec[optind], "-V") == 0)
        {
          lprintf (0, "%s version: %s\n", PACKAGE, VERSION);
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
      else if (strcmp (argvec[optind], "-m") == 0)
        {
	  matchstr = getoptval(argcount, argvec, optind++);
        }
      else if (strcmp (argvec[optind], "-n") == 0)
        {
	  char *opt = getoptval(argcount, argvec, optind++);
	  
	  if ( strlen (opt) > 2 ) {
	    lprintf (0,"Network code can only be 1 or 2 characters\n");
	    exit (1);
	  }
	  netcode = (char *) malloc (3);
	  sprintf (netcode, "%-2.2s", opt);
	  netcode[2] = '\0';
	  break;
	}
      else if (strcmp (argvec[optind], "-ring") == 0)
        {
	  ringfile = getoptval(argcount, argvec, optind++);
        }
      else if (strcmp (argvec[optind], "-mod") == 0)
        {
	  modulus = strtol (getoptval(argcount, argvec, optind++), NULL, 10);
        }
      else if (strcmp (argvec[optind], "-ext") == 0)
        {
	  extstr = getoptval(argcount, argvec, optind++);
        }
      else if (strcmp (argvec[optind], "-MD") == 0)
        {
          mappingdb = getoptval(argcount, argvec, optind++);
        }
      else if (strcmp (argvec[optind], "-CD") == 0)
        {
          calibdb = getoptval(argcount, argvec, optind++);
        }
      else if (strcmp (argvec[optind], "-r") == 0)
        {
          remap = 1;
        }
      else if (strncmp (argvec[optind], "-", 1) == 0)
        {
          lprintf (0, "Unknown option: %s\n", argvec[optind]);
          exit (1);
        }
      else
	{
	  if ( ! orbaddr )
	    orbaddr = argvec[optind];
	  else
	    lprintf (0, "Unknown option: %s\n", argvec[optind]);
	}
    }
  
  /* Initialize the Antelope elog printing system */
  Program_Name = argvec[0];
  elog_init (argcount, argvec);
  
  /* Report the program version */
  lprintf (0, "%s version: %s\n", PACKAGE, VERSION);
  
  /* Check that an ORB was specified */
  if ( ! orbaddr )
    {
      lprintf (0, "Error: no ORB specified\n");
      lprintf (0, "Try the -h option for help\n");
      exit (1);
    }
  
  /* Check that an input ring was specified */
  if ( ! ringfile )
    {
      lprintf (0, "No ring file specified\n");
      lprintf (0, "Try the -h option for help\n");
      exit (1);
    }
  
  /* Compile the match regex if specified */
  if ( matchstr )
    {
      match = (regex_t *) malloc (sizeof (regex_t));
      
      if ( regcomp(match, matchstr, REG_EXTENDED|REG_NOSUB ) != 0 )
	{
	  lprintf (0, "Cannot compile regular expression '%s'", matchstr);
	  exit (1);
	}
    }
  
  return 0;
}  /* End of parameter_proc() */


/***************************************************************************
 * getoptval:
 * Return the value to a command line option; checking that the value is 
 * itself not an option (starting with '-') and is not past the end of
 * the argument list.
 *
 * argcount: total arguments in argvec
 * argvec: argument list
 * argopt: index of option to process, value is expected to be at argopt+1
 *
 * Returns value on success and exits with error message on failure
 ***************************************************************************/
static char *
getoptval (int argcount, char **argvec, int argopt)
{
  if ( argvec == NULL || argvec[argopt] == NULL ) {
    lprintf (0, "getoptval(): NULL option requested\n");
    exit (1);
  }
  
  if ( (argopt+1) < argcount && *argvec[argopt+1] != '-' )
    return argvec[argopt+1];
  
  lprintf (0, "Option %s requires a value\n", argvec[argopt]);
  exit (1);
}  /* End of getoptval() */


/* A generic log message print handler */
int
lprintf (int level, const char *fmt, ...)
{
  int r = 0;
  char message[200];
  char timestr[100];
  va_list argptr;
  time_t loc_time;
  
  if ( level <= verbose )
    {
      va_start(argptr, fmt);
      
      /* Build local time string and cut off the newline */
      time (&loc_time);
      strcpy (timestr, asctime(localtime(&loc_time)));
      timestr[strlen(timestr) - 1] = '\0';
      
      r = vsnprintf (message, sizeof(message), fmt, argptr);
      
      fprintf(stderr, "%s - %s", timestr, message);
      fflush(stdout);
    }
  
  return r;
}


/***************************************************************************
 * term_handler:
 * Signal handler routine. 
 ***************************************************************************/
static void
term_handler (int sig)
{
  lprintf (0, "Shutting down\n");
  shutdownsig = 1;
}


/***************************************************************************
 * usage:
 * Print the usage message and exit.
 ***************************************************************************/
static void
usage ()
{
  fprintf (stderr, "%s version: %s\n\n", PACKAGE, VERSION);
  fprintf (stderr, "Collect Mini-SEED data from a NEIC RRP ring send to an ORB\n\n");
  fprintf (stderr, "Usage: %s [options] -ring <ringfile> ORB\n\n", PACKAGE);
  fprintf (stderr,
           " ## Options ##\n"
           " -V              Report program version\n"
           " -h              Show this usage message\n"
           " -v              Be more verbose, multiple flags can be used (-vv or -v -v)\n"
	   " -m regex        Regular expression to match source name using\n"
	   "                   a 'NET_STA_LOC_CHAN' convention, for example:\n"
	   "                   'IU_KONO_00_BH?' or 'GE_WLF_.*'\n"
	   " -n net          Change network code (after matching), 1 or 2 chars\n"
           "\n"
	   " # RRP options #\n"
           " -ring ringfile  Ring file name, required\n"
	   " -mod nnn        Write status file interval in packets (default 100)\n"
	   " -ext string     Client reader ID, must be unique, default: .ringstat\n"
           "\n"
           " # ORB options #\n"
	   " -MD mappingdb  Specify database which contains mapping of SEED to CSS names\n"
	   " -CD calibdb    Specify database containing calib information\n"
	   " -r             Remap SEED names to sta and chan codes using mapping db\n"
           "\n"
	   " ORB            Address of destination ORB in host:port format\n"
	   "\n");
}  /* End of usage() */
