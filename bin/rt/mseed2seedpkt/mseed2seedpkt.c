/***************************************************************************
 * mseed2seedpkt.c:
 *
 * An Antelope ORB module to reformat ORB packets which include Mini-SEED
 * records into ORB SEED types.
 *
 * Chad Trabant, IRIS Data Management Center
 ***************************************************************************/

#include <stdio.h>
#include <signal.h>
#include <stdarg.h>
#include <orb.h>
#include <stock.h>

#include "seedutil.h"

static char *version   = "2004.237";
static char  verbose   = 0;   /* Verbosity flag */
static int orbin       = 0;   /* Input ORB descriptor */
static int orbout      = 0;   /* Output ORB descriptor */
static int stopsig     = 0;   /* Stop signal flag */
static int offset      = 8;   /* Offset into raw ORB packet to Mini-SEED */

static char *database  = 0;   /* DB for calib, calper and segtype */
static char *statefile = 0;   /* A file to save the ORB packet id */

/* Wraps Mini-SEED to make an Antelope/SEED packet (see mseed2orbpkt.c) */
extern int  mseed2orbpkt(char *, int, char *, char *, double *,
                         char **, int *, int *);

static void packethandler (char *srcname, char *rawpacket, int nbytes, int offset);
static int isalldig (char *check);
static int lprintf (int level, const char *fmt, ...);
static void term_sighandler (int sig);
static void usage ();


int
main (int argc, char **argv)
{
  char *select     = 0;      /* Packets to match */
  char *reject     = 0;      /* Packets to reject */
  char *orbinaddr  = 0;      /* Location of the iput ORB, in host:port format */
  char *orboutaddr = 0;      /* Location of the ouput ORB, in host:port format */
  char *startat    = 0;      /* Starting ORB position */

  int pktcountdown = -1;     /* How many packets to process */

  Dbptr dbase;
  double tepoch;
  int errflag = 0;
  int orbret;
  int i;
  
  /* For use with the statefile */
  char *statestr      = 0;
  char *stateinterstr = 0;
  int stateinter      = 0;   /* Interval to bury the pktid (packets recv'd) */
  int pktcount        = 0;
  int last_pktid;
  double last_pkttime;

  /* Parameters filled/used by orbreap() */
  int pktid;
  char srcname[200];
  double time;
  char *rawpacket = NULL;
  int nbytes = 0;
  int bufsize = 0;

  struct sigaction sa;

  /* Signal handling, use POSIX calls with standardized semantics */
  sa.sa_flags = SA_RESTART;
  sigemptyset(&sa.sa_mask);

  sa.sa_handler = term_sighandler;
  sigaction(SIGINT, &sa, NULL);
  sigaction(SIGQUIT, &sa, NULL);
  sigaction(SIGTERM, &sa, NULL);

  sa.sa_handler = SIG_IGN;
  sigaction(SIGHUP, &sa, NULL);
  sigaction(SIGPIPE, &sa, NULL);

  if (argc < 3)
    usage();

  while ((i = getopt(argc, argv, "vd:m:r:n:s:S:o:")) != EOF)
    switch (i) {
    case 'v':
      verbose++;
      break;
    case 'd':
      database = strdup(optarg);
      break;
    case 'm':
      select = strdup(optarg);
      break;
    case 'r':
      reject = strdup(optarg);
      break;
    case 'n':
      pktcountdown = atoi(optarg);
      break;
    case 's':
      startat = strdup(optarg);
      break;
    case 'S':
      statestr = strdup(optarg);
      break;
    case 'o':
      offset = atoi(optarg);
      break;
    }
  
  if ( optind < argc )
    orbinaddr = strdup(argv[optind++]);
  
  if ( optind < argc )
    orboutaddr = strdup(argv[optind]);
  
  /* Initialize the Antelope elog system */
  elog_init(argc, argv);

  lprintf (0, "mseed2seedpkt version %s\n", version);

  if ( ! orbinaddr )
    {
      elog_complain (0, "No input ORB location specified\n");
      errflag++;
    }
  
  if ( ! orboutaddr )
    {
      elog_complain (0, "No output ORB location specified\n");
      errflag++;
    }

  /* Process the statestr, separate file name from time interval */
  if ( statestr && (stateinterstr = strchr(statestr, ':')) != NULL )
    {   /* Interval specified */   
      *stateinterstr++ = '\0';  /* Split the strings */
      
      if ( strlen(stateinterstr) == 0 ) /* Nothing after ':' */
	{
	  elog_complain (0, "No state saving interval specified after ':'\n");
	  errflag++; 
	}
      
      i = strlen(statestr);
      statefile = (char *) malloc( i + 1 );
      strncpy(statefile, statestr, i);  /* State file */
      *(statefile + i) = '\0';
      
      stateinter = atoi(stateinterstr); /* Recv'd packet interval */
    }
  else  /* No interval specified */
    {
      statefile = statestr;
    }

  /* Exit with usage message if any errors */
  if ( errflag )
    exit (1);
  
  /* Database setup */
  if (database)
    {
      if (dbopen(database, "r+", &dbase) == dbINVALID)
        {
          elog_complain (0, "dbopen(%s) failed\n", database);
          exit(1);
        }
      finit_db(dbase);
    }
  
  /* Input/Output ORB setup */
  lprintf (1, "Connecting to input ORB %s\n", orbinaddr);
  if ( (orbin = orbopen(orbinaddr, "r&")) < 0 )
    {
      elog_complain (0, "Error connecting to input ORB %s\n", orbinaddr);
      exit(1);
    }

  lprintf (1, "Connecting to output ORB %s\n", orboutaddr);
  if ( (orbout = orbopen(orboutaddr, "w&")) < 0 )
    {
      elog_complain (0, "Error connecting to output ORB %s\n", orboutaddr);
      orbclose (orbin);
      exit(1);
    }

  /* Read/setup the state file if supplied */
  if ( statefile ) {
    if ( (exhume(statefile, 0, 0, 0)) < 0 )
      elog_complain (0, "exhume() of '%s' failed\n", statefile);
    if ( (orbresurrect(orbin, &last_pktid, &last_pkttime)) >= 0 )
      lprintf (0, "Input ORB position resurrected\n");
    else
      lprintf (0, "Input ORB position resurrection unsuccessful\n");
  }

  /* Init the ORB position */
  if ( startat != 0 && isalldig(startat) ) {  /* If all digits assume pktid */
    if ( orbseek(orbin, atoi(startat)) == -1 )
      elog_complain (0, "orbseek() error for '%s'\n", startat);
    else
      lprintf (0, "ORB positioned to pktid: %d\n", atoi(startat));
  }
  else if ( startat != 0 ) {
    if ( is_epoch_string(startat, &tepoch) )
      {
	orbposition(orbin, startat);
	lprintf (0, "ORB positioned to time: %s\n", startat);
      }
  }
  
  /* Set the ORB select and reject if provided */
  if ( select )
    orbselect(orbin, select);
  if ( reject )
    orbreject(orbin, reject);
  
  /* Start the primary loop  */
  while ( stopsig == 0 && pktcountdown != 0 )
    {
      
      orbret = orbreap( orbin, &pktid, srcname, &time,
			&rawpacket, &nbytes, &bufsize );
      
      if ( orbret )
     	{
	  elog_complain (0, "orbreap() error\n");
	  break;
	}
      else           /* Process the packet */
	{
	  lprintf (3, "Recv'd %s, pktid: %d\n", srcname, pktid);
	  
	  /* Send the data to the packet handler */
	  packethandler (srcname, rawpacket, nbytes, offset);
	  
	  last_pktid = pktid;
	  last_pkttime = time;

	  if ( pktcountdown > 0 ) pktcountdown--;
	  
	  /* Save the ORB position every 'stateinter' # of packets */
	  if ( statefile && stateinter )
	    {
	      pktcount++;
	      if ( pktcount >= stateinter )
		{
		  pktcount = 0;
		  
		  if ( (bury()) < 0 )
		    elog_complain (0, "Error saving ORB position to state file\n");
		  else
		    lprintf (2, "ORB position saved to state file\n");
		}
	    }
	}
    } /* End of primary while loop */
  
  /* Bury the state information */
  if ( statefile ) {
    if ( (bury()) < 0 )
      elog_complain (0, "Error saving ORB position to state file\n");
    else
      lprintf (1, "ORB position saved to state file\n");
  }
  
  if ( orbin == orbout )
    {
      orbclose(orbin);
    }
  else
    {
      orbclose(orbin);
      orbclose(orbout);
    }
  
  lprintf (1, "Terminating mseed2seedpkt\n");

  elog_print (stdout, 0);

  return 0;
} /* End of main() */


/*********************************************************************
 * packethandler:
 *
 * Package the MiniSEED record into an ORB SEED packet.
 *
 * Returns 0 on success and -1 on error.
 *********************************************************************/
static void
packethandler (char *srcname, char *rawpacket, int nbytes, int offset)
{
  static char *packet;
  static int bufsize = 0;
  int mseedret  = 0;
  int noutbytes = 0;
  int reclen;
  double time;

  /* Find the Mini-SEED record length and generally test for a
     Mini-SEED record */
  reclen = find_reclen (rawpacket + offset, nbytes - offset);
  
  if ( reclen == -1 )
    {
      elog_complain (0, "No Mini-SEED record found at offset %d for %s (%d bytes)\n",
		     offset, srcname, nbytes);
      return;
    }
  else if ( reclen == 0 )
    {
      elog_complain (0, "No Blockette 1000 found in packet for %s (%d bytes)\n",
		     srcname, nbytes);
      return;
    }
  
  lprintf (1, "Received %s (%d bytes), reclen %d\n",
	   srcname, nbytes, reclen);
  
  mseedret = mseed2orbpkt(rawpacket + offset, reclen, database, srcname,
			  &time, &packet, &noutbytes, &bufsize);
  
  lprintf (3, "Sending %s (%d bytes) to output ORB\n", srcname, noutbytes);
  
  if ( mseedret == 0 )
    {
      if ( orbput(orbout, srcname, time, packet, noutbytes) )
	elog_complain (0, "orbput() failed: %s (%d bytes)\n", srcname, nbytes);
    }
  else
    {
      elog_complain (0, "%s: packet not forwared, mseed2orbpkt returned: %d\n",
		     srcname, mseedret);
    }
}


/*********************************************************************
 * isalldig:
 *
 * Simple check for a string of all digits.
 *
 * Returns 1 if string was all digits and 0 if not.
 *********************************************************************/
static int
isalldig (char *check)
{
  int i,len;

  len = strlen(check);

  for ( i=0; i < len; i++ )
    {
      if ( strchr("0123456789", check[i]) == NULL )
	return 0;
    }

  return 1;
}


/*********************************************************************
 * lprintf:
 *
 * A generic log message print handler.
 *
 * Returns the number of characters in final message or a negative
 * value on error.
 *********************************************************************/
static int
lprintf (int level, const char *fmt, ...)
{
  int r = 0;
  char message[100];
  va_list argptr;

  if ( level <= verbose )
    {
      va_start(argptr, fmt);
      
      r = vsnprintf(message, sizeof(message), fmt, argptr);
      
      elog_notify (0, message);
    }

  return r;
}


/***************************************************************************
 * term_sighandler:
 *
 * Signal handler routine for termination.
 ***************************************************************************/
static void
term_sighandler (int sig)
{
  /* Bury the state information */
  if ( statefile ) {
    if ( (bury()) < 0 )
      elog_complain (0, "error saving ORB position to state file\n");
    else
      lprintf (1, "ORB position saved to state file\n");
  }
  
  if ( orbin == orbout )
    {
      orbclose(orbin);
    }
  else
    {
      orbclose(orbin);
      orbclose(orbout);
    }
  
  lprintf (1, "Terminating mseed2seedpkt\n");

  elog_print (stdout, 0);

  exit (0);
}


/***************************************************************************
 * usage():
 * Print the usage message and exit.
 ***************************************************************************/
static void
usage()
{
  fprintf(stderr,"\nmseed2seedpkt: version %s\n", version);
  fprintf(stderr,"\nUsage: mseed2seedpkt [-v] [-m match] [-r reject] [-n number] [-s pktid|time]\n");
  fprintf(stderr,"                     [-S statefile[:pkts]] in out\n\n");
  fprintf(stderr,"  -v                  Verbosity, up to 3 levels.\n");
  fprintf(stderr,"  -d                  Insert calib, calper and segtype from this database\n");
  fprintf(stderr,"  -m match            Regular expression to match ORB packets,\n");
  fprintf(stderr,"  -r reject           Regular expression to reject ORB packets.\n");
  fprintf(stderr,"  -n number           Number of packets, otherwise continuous.\n");
  fprintf(stderr,"  -s pktid|time       Start position in ORB, pktid or time,\n");
  fprintf(stderr,"                        default is next packet.\n");
  fprintf(stderr,"  -S statefile[:pkts] File for saving/restoring the ORB position,\n");
  fprintf(stderr,"                        restored ORB position is overridden by '-s'.\n");
  fprintf(stderr,"                        The position will  be saved every 'pkts' number\n");
  fprintf(stderr,"                        of packets received, if provided.\n");
  fprintf(stderr,"  -o offset           Offset into raw packet to Mini-SEED record, default: %d\n\n", offset);
  fprintf(stderr,"  in                  Required, the input ORB in host:port format.\n");
  fprintf(stderr,"  out                 Required, the output ORB in host:port format.\n\n");
  fprintf(stderr,"Antelope Contributed Software\n\n");
  fprintf(stderr,"Chad Trabant\nIRIS Data Management Center\n\n");

  exit (1);
}
