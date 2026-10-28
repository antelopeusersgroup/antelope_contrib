/***************************************************************************
 * orb_plugin.c:
 * A SeedLink plugin to read data from an Antelope ORB.
 *
 * This software requires the Antelope libraries and needs an Antelope
 * build environment to compile.  But, of course, it is a SeedLink plugin
 * and should be run by the SeedLink daemon as such.
 *
 * Chad Trabant, ORFEUS/EC-Project MEREDIAN
 * Niko Horn, ZAMG messed up the code to work with 64-bit Antelope 5.0+
 ***************************************************************************/

#include <signal.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <stdarg.h>
#include <libgen.h>
#include "orb.h"
#include "Pkt.h"
#include "msd.h"

#include "plugin.h"
#include "seedutil.h"

static char *version   = "2015.187";
static int verbose     = 0;
static int trim        = 0;
static int orb         = 0;
static int stopsig     = 0;
static char *statefile = 0;      /* A file to save the ORB packet id */
static Tbl  *chaninfo;           /* Table to keep track of channel information */
static int   repackage = 0;      /* Re-packge original 512-byte miniSEED records */

/* To keep track of unique data streams and associated details */
typedef struct ChanData {
  char       *streamname;
  Msd        *msd;
  double      starttime;  /* start time of the stream */
  double      samprate;
  double      nexttime;   /* next sample should start at this time */
  char        blk1001[8]; /* An 8 byte 1001 blockette for QCDAT */
} ChanData;


int findstream (char *streamname);
int addstream (char *streamname, PktChannel *pktchan);
int packstream (char *streamname, char *typename, PktChannel *pktchan,
		char *rawpacket, int nbytes);
int save_record (Msd *msd, long n0, long n1);
int flushstream (char *streamname);
Msd *initnewmsd (PktChannel *pktchan);
int isalldig (char *check);
int lprintf (int level, const char *fmt, ...);
void mortician ();

/* Exists in qcdatutil.c */
extern void *qcdat_to_deb(char *qcdat_record, int next_offset, int data_frames);

static void
usage(char *binname)
{
  fprintf(stderr,"\n%s: version %s\n", binname, version);
  fprintf(stderr,"\nUsage: %s [-v] [-t] [-m match] [-r reject] [-n number] [-s pktid|time]\n", binname);
  fprintf(stderr,"                  [-S statefile[:pkts]] -o orb\n\n");
  fprintf(stderr,"  -v                  Verbosity, up to 3 levels.\n");
  fprintf(stderr,"  -p                  Re-package original 512-byte miniSEED records\n");
  fprintf(stderr,"  -t                  Trim partially overlapping data.\n");
  fprintf(stderr,"  -m match            Regular expression to match ORB packets,\n");
  fprintf(stderr,"                        default is all waveform data.\n");
  fprintf(stderr,"  -r reject           Regular expression to reject ORB packets.\n");
  fprintf(stderr,"  -n number           Number of packets, otherwise continuous.\n");
  fprintf(stderr,"  -s pktid|time       Start position in ORB, pktid or time,\n");
  fprintf(stderr,"                        default is next packet.\n");
  fprintf(stderr,"  -S statefile[:pkts] File for saving/restoring the ORB position,\n");
  fprintf(stderr,"                        restored ORB position is overridden by '-s'.\n");
  fprintf(stderr,"                        The position will  be saved every 'pkts' number\n");
  fprintf(stderr,"                        of packets received, if provided.\n");
  fprintf(stderr,"  -o orb              Required, location of the ORB in host:port format.\n\n");

  exit (1);
}

int
main (int argc, char **argv)
{
  char *select  = 0;        /* Objects to match */
  char *reject  = 0;        /* Objects to reject */
  char *orbaddr = 0;        /* Location of the ORB, in host:port format */
  char *startat = 0;        /* Starting ORB position */

  int pktcountdown = -1;    /* How many packets to process */
  int errflag = 0;
  int orbret;
  int retval;
  int c, ichan;
  double tepoch;

  extern char *optarg;
  extern int optind;

  /* For use with the statefile */
  char *statestr      = 0;
  char *stateinterstr = 0;
  int stateinter      = 0;   /* Interval to bury the pktid (packets recv'd) */
  int pktcount        = 0;
  int last_pktid;
  double last_pkttime;

  /* Parameters filled/used by orbreap() and unstuffPkt() */
  int pktid;
  char srcname[200];
  char streamname[200];
  double time;
  char *rawpacket = NULL;
  int nbytes = 0;
  int bufsize = 0;
  struct Packet *pkt = NULL;
  
  struct PktChannel *pktchan = NULL;
  
  /* Signal handling, use POSIX calls with standardized semantics */
  struct sigaction sa;
  sa.sa_flags = SA_RESTART;
  sigemptyset(&sa.sa_mask);
  
  sa.sa_handler = mortician;
  sigaction(SIGINT, &sa, NULL);
  sigaction(SIGQUIT, &sa, NULL);
  sigaction(SIGTERM, &sa, NULL);
  
  sa.sa_handler = SIG_IGN;
  sigaction(SIGHUP, &sa, NULL);
  sigaction(SIGPIPE, &sa, NULL);
  
  if (argc < 2)
    errflag++;
  
  while ((c = getopt(argc, argv, "vtm:r:n:s:S:o:")) != EOF)
    switch (c) {
    case 'v':
      verbose++;
      break;
    case 'p':
      repackage = 1;
      break;
    case 't':
      trim = 1;
      break;
    case 'm':
      select = (char *) strdup(optarg);
      break;
    case 'r':
      reject = (char *) strdup(optarg);
      break;
    case 'n':
      pktcountdown = atoi(optarg);
      break;
    case 's':
      startat = (char *) strdup(optarg);
      break;
    case 'S':
      statestr = (char *) strdup(optarg);
      break;
    case 'o':
      orbaddr = (char *) strdup(optarg);
      break;
    }

  if ( orbaddr == 0 )
    {
      lprintf (0, "%s: No ORB location given, '-o' flag is required!\n", argv[0]);
      errflag++;
    }
  
  /* Process the statestr, separate file name from time interval */
  if ( statestr && (stateinterstr = strchr(statestr, ':')) != NULL )
    {  /* Interval provided */
      
      *stateinterstr++ = '\0';  /* Split the strings */
      
      if ( strlen(stateinterstr) == 0 ) errflag++; /* Nothing after ':' */
      
      c = strlen(statestr);
      statefile = (char *) malloc( c + 1 );
      strncpy(statefile, statestr, c);  /* State file */
      *(statefile+c) = '\0';
      
      stateinter = atoi(stateinterstr); /* Recv'd packet interval */
    }
  else /* No interval provided */
    {
      statefile = statestr;
    }

  /* Exit with usage message if any errors */
  if ( errflag )
    usage(basename(argv[0]));
  
  /* ORB setup */
  if ( (orb = orbopen(orbaddr, "r&")) < 0 )
    {
      lprintf (0, "%s: orbopen() error for %s (%d)\n", argv[0], orbaddr, orb);
      exit(1);
    }
  
  /* Read/setup the state file and setup signal handlers */  
  if ( statefile ) {
    if ( verbose )
      lprintf (0, "Resuming ORB position from state file\n");
    
    if ( (exhume(statefile, 0, 0, 0)) < 0 )
      lprintf (0, "exhume() of '%s' failed\n", statefile);

    if ( (orbresurrect(orb, &last_pktid, &last_pkttime)) >= 0 )
      lprintf (0, "ORB position resurrected\n");
    else
      lprintf (0, "ORB position resurrection unsuccessful\n");
  }
  
  /* Init channel info table */
  chaninfo = newtbl(0);
  
  /* Init the ORB position */
  if ( startat )
    {
      if ( verbose )
	lprintf (0, "Initializing ORB position to '%s'\n", startat);

      if ( isalldig (startat) )  /* if all digits assume pktid */
	{
	  if ( orbseek (orb, atoi(startat)) == -1 )
	    lprintf (0, "orbseek() error for '%s'\n", startat);
	  else 
	    lprintf (0, "ORB positioned to pktid: %d\n", atoi(startat));
	}
      else if ( is_epoch_string (startat, &tepoch) )
	{
	  orbposition (orb, startat);
	  lprintf (0, "ORB positioned to time: %s\n", startat);
	}
    }

  /* Set the ORB selector and rejector if provided */
  if ( select )
    orbselect(orb, select);
  if ( reject )
    orbreject(orb, reject);
  
  /* Start the primary loop  */
  while ( stopsig == 0 && pktcountdown != 0 )
    {
      
      orbret = orbreap ( orb, &pktid, srcname, &time,
			 &rawpacket, &nbytes, &bufsize );
      
      if ( orbret < 0 )
	{
	  lprintf(0, "orbreap() error (%d)\n", orbret);
	  stopsig = 1;
	}
      
      else           /* Process the packet */
	{
	  lprintf (3, "Recv'd %s, pktid %d\n", srcname, pktid);
	  
	  retval = unstuffPkt( srcname, time, rawpacket, nbytes, &pkt );
	  
	  if ( retval < 0 )
	    lprintf(0,"Unstuff failure for %s\n", srcname);
	  
	  /* If waveform data process it */
	  if ( retval == Pkt_wf )
	    {
	      char *timestr = 0;

	      for( ichan = 0; ichan < pkt->nchannels; ichan++ )
		{
		  pktchan = gettbl( pkt->channels, ichan );
		  
		  snprintf (streamname, sizeof(streamname), "%s_%s_%s_%s",
			    pktchan->net, pktchan->sta, pktchan->loc, pktchan->chan);
		  
		  if ( verbose >= 3 )
		    timestr = strydtime (pktchan->time);
		  lprintf (3, "Channel: %s nsamp: %d @ %s\n",
			   streamname, pktchan->nsamp, timestr);
		  if ( timestr )
		    free (timestr);

		  /* Send the data to the packager */
		  packstream( streamname, pkt->pkttype->name, pktchan, rawpacket, nbytes );
		}
	    }
	  
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
		    lprintf(0, "error saving ORB position to state file\n");
		  else
		    lprintf(2, "ORB position saved to state file\n");
		}
	    }
	} /* End of packet processing */
    } /* End of primary while loop */
  
  /* Getting ready to exit, this will almost never be reached as the 
   * signal handler routine will usually do approriate shutdown and exit. */
  
  if ( statefile ) 
    {
      if ( (bury()) < 0 )
	lprintf (0, "error saving ORB position to state file\n");
      else
	lprintf (1, "ORB position saved to state file\n");
    }
  
  flushstream (NULL);
  orbclose (orb);
  
  lprintf (1, "Terminating orb_plugin\n");
  
  return 0;
} /* End of main() */


/*********************************************************************
 * findstream:
 *
 * Simple function to find the index of a stream.
 *
 * Returns -1 if stream was not found, otherwise the index of the
 * stream.
 *********************************************************************/
int
findstream (char *streamname)
{
  int idx;
  int found = -1;
  ChanData *chandata;

  /* Search the chaninfo table for this streamname */
  for (idx=0; idx < maxtbl(chaninfo); idx++) {
    chandata = gettbl(chaninfo, idx);
    if ( !strcmp(chandata->streamname, streamname) ) {
      found = idx;
      break;
    }
  }
  
  return found;
}


/*********************************************************************
 * addstream:
 *
 * Add a stream.
 *
 * Returns -1 if the stream was already present, otherwise the index
 * of the added stream.
 *********************************************************************/
int
addstream (char *streamname, PktChannel *pktchan)
{
  int found;
  ChanData *newchan;

  /* Search the chaninfo table for this streamname */
  found = findstream(streamname);

  /* Complain if stream definition already exists */
  if ( found != -1 ) {
    lprintf (0, "Stream %s already defined at index %d!\n",
	     streamname, found);
  }
  /* Else, initialize and add to table */
  else {
    newchan = (ChanData *) malloc(sizeof(ChanData));
    newchan->streamname = strdup(streamname);
    newchan->msd = initnewmsd(pktchan);
    newchan->starttime = pktchan->time;
    newchan->samprate = pktchan->samprate;
    newchan->blk1001[0] = 0;

    found = pushtbl(chaninfo, newchan);

    lprintf (1, "Adding stream %s\n", streamname);
  }

  return found;
}


/*********************************************************************
 * packstream:
 *
 * Package the data into miniSEED records.
 *
 * Return the positive index of the stream on success and -1 on error.
 *********************************************************************/
int
packstream (char *streamname, char *typename, PktChannel *pktchan,
	    char *rawpacket, int nbytes)
{
  int nerr, index;
  int contiguous, reset;
  long datasamp, offset;
  int *databuf; /* Assuming correct data type, 32-bit ints */
  double endtime, timediff;
  char *blk1001;
  Msd *curmsd;
  ChanData *chandata;
  
  /* Init the data buffer and number of samples */
  databuf = pktchan->data;
  datasamp = pktchan->nsamp;
  
  /* If the stream information does not exist add it */
  if ( (index = findstream(streamname)) == -1 ) {
    index = addstream (streamname, pktchan);
    chandata = gettbl (chaninfo, index);
  }
  /* Else check for data duplication, overlap, & gaps */
  else {
    reset = 0;
    chandata = gettbl (chaninfo, index);
    contiguous = TRSAMETIME (chandata->nexttime,
			     pktchan->time, pktchan->samprate);

    if ( !contiguous && pktchan->time < chandata->nexttime ) {
      /* Do we have all of this data? */
      endtime = ENDTIME (pktchan->time, pktchan->samprate, pktchan->nsamp);

      /* Yes, duplicate data */
      if ( endtime < chandata->nexttime ) {
        timediff = SAMP2TIME (pktchan->time, pktchan->samprate,
			      pktchan->nsamp);
	timediff -= pktchan->time;
        lprintf (1,"Duplicate data: %s, %f sec, dropping\n",
		 streamname, timediff);
	return -1;
      }

      /* No, partially overlapping data */
      if ( endtime >= chandata->nexttime ) {
	timediff = chandata->nexttime - pktchan->time;

	if ( ! trim ) {    /* Do not cut out the overlap */
	  lprintf (1,"Partial overlap: %s, %f sec, resetting stream\n",
		   streamname, timediff);
	  reset++;
	}
	else {             /* Trim the overlap */
	  lprintf (1,"Partial overlap: %s, %f sec, trimming\n",
		   streamname, timediff);
	  offset = NSAMP (pktchan->time, pktchan->samprate,
			  chandata->nexttime);
	  offset -= 2;     /* Make the ends meet */

	  /* Adjust the current databuf and datasamp */
	  databuf = &databuf[offset];
	  datasamp -= (offset + 1);
	}
      }
    }

    /* Check for data gap */
    if ( !contiguous && pktchan->time > chandata->nexttime ) {
      timediff = pktchan->time - chandata->nexttime;
      lprintf (1,"Data gap: %s, %fsec, resetting stream\n", streamname, timediff);
      reset++;
    }

    if (reset) {
      flushstream (streamname);
      chandata->msd->hook = 0;
      msdfree (chandata->msd);
      chandata->msd = initnewmsd (pktchan);
      chandata->starttime = pktchan->time;
      chandata->samprate = pktchan->samprate;
    }
  }  /* end of timing checks */
  
  /* Keep track of the next expected sample time */
  chandata->nexttime = SAMP2TIME (pktchan->time, pktchan->samprate,
				  pktchan->nsamp);
  
  /* Msd structure for this stream */
  curmsd = chandata->msd;
  
  /* If this is QCDAT data, generate a network byte order 1001 blockette */
  if ( strcmp(typename, "QCDAT") == 0 ) {
    blk1001 = qcdat_to_deb (rawpacket, 0, 7);
    if ( blk1001 != 0 ) {
      memcpy (chandata->blk1001, blk1001, 8);
    }
    else {
      chandata->blk1001[0] = 0;
      lprintf (0, "Could not generate a blk1001 for %s\n", streamname);
    }
  }
  
  /* If this packet is a 512-byte (PLUGIN_MSEED_SIZE) miniSEED record
   * send the original to the SeedLink server, otherwise send it to
   * cmsd() for packaging.  (Re)Packaging of all data can be forced
   * with the '-p' command line option.
   *
   * As of Antelope 4.5 the miniSEED record starts 14 bytes from the
   * beginning of a SEED type packet, this is unlikely to change
   * anytime soon.
   */
  if ( !repackage && strcmp(typename, "SEED") == 0 ) {
    if ( find_reclen (rawpacket+14, (nbytes-14)) == PLUGIN_MSEED_SIZE ) {
      send_mseed (curmsd->sdh.sta, rawpacket+14, PLUGIN_MSEED_SIZE);
      
      /* DEBUG, Write the record to file descriptor */
      /*
      write (60, rawpacket+14, PLUGIN_MSEED_SIZE);
      */
    }
  }
  else {
    /* Loop back into the Msd structure for use in save_record(),
     * mis-typing the hook member on purpose. */
    curmsd->hook = (Hook *) chandata;
    
    /* Squish it */
    if ( (nerr = cmsd (curmsd, save_record, databuf, datasamp)) ) {
      lprintf (0, "%d Steim compression errors for %s\n", nerr, streamname);
    }
  }
  
  return index;
}


/*********************************************************************
 * save_record:
 *
 * Routine called by cmsd() when a full record of compressed data is
 * ready.
 *
 * Returns 0
 *********************************************************************/
int
save_record ( Msd *msd, long n0, long n1 )
{
  ChanData *chandata;
  
  /* Unwrap the ChanData pointer */
  chandata = (ChanData *) msd->hook;
  
  /* Fill in the fixed header */
  pack_msdhdr (msd, n0, n1);
  
  /* Insert the 1001 blockette here if present */
  if ( chandata->blk1001[0] != 0 ) {
    insert_blkt (msd->record, msd->record_size, chandata->blk1001, 8);
  }
  
  lprintf (3, "Forwarding %s_%s_%s_%s, (%d) %d samps\n",
	   msd->sdh.net, msd->sdh.sta, msd->sdh.loc, msd->sdh.chan,
	   msd->record_size, msd->sdh.nsamp);
  
  /* Send the record to SeedLink */
  send_mseed (msd->sdh.sta, msd->record, msd->record_size);
  
  /* DEBUG, Write the record to file descriptor */
  /*
  write (60, msd->record, msd->record_size);
  */
  
  /* Bump the sequence number (with wrap) and record count */
  if (msd->sdh.seq == 999999) msd->sdh.seq = 1;
  else msd->sdh.seq++;
  msd->cnt++;
  
  return 0;
}


/*********************************************************************
 * flushstream:
 *
 * Package remaining data in buffer(s) into miniSEED records.  If
 * streamname is NULL all streams will be flushed, otherwise only the
 * specified stream will be flushed.
 *
 * Returns 0 if all streams were flushed, a positive index if the
 * specified stream streamname was found and -1 if streamname was not
 * found.
 *********************************************************************/
int
flushstream (char *streamname)
{
  int idx, found, nerr;
  Msd *curmsd;
  ChanData *chandata = 0;
  
  found = (streamname == NULL) ? 0 : -1;

  for (idx=0; idx < maxtbl(chaninfo); idx++) {
    chandata = gettbl(chaninfo, idx);
    
    if ( streamname != NULL ) {   /* if streamname was specified */
      if ( !strcmp(chandata->streamname, streamname) ) {
	chandata = gettbl(chaninfo, idx);
	curmsd = chandata->msd;
	curmsd->hook = (Hook *) chandata;
	
	lprintf (3, "Flushing stream %s\n", chandata->streamname);
	if ( (nerr = cmsd(curmsd, save_record, 0, 0)) ) {
	  lprintf (0, "%d Steim compression errors\n", nerr);
	}
	found = idx;
        break;
      }
    }
    else {                     /* if flushing all streams */
      chandata = gettbl(chaninfo, idx);
      curmsd = chandata->msd;
      curmsd->hook = (Hook *) chandata;
      
      lprintf (3, "Flushing stream %s\n", chandata->streamname);
      
      if ( (nerr = cmsd(curmsd, save_record, 0, 0)) ) {
	lprintf (0, "%d Steim compression errors\n", nerr);
      }
    }
  }
  
  if ( streamname != NULL && found == -1 ) {
    lprintf (0, "Could not find stream %s for flushing!\n",
	     chandata->streamname);
  }
  
  return found;
}


/*********************************************************************
 * initnewmsd:
 *
 * Initialize a new Msd structure and return it.
 *********************************************************************/
Msd *
initnewmsd (PktChannel *pktchan)
{
  Msd *newmsd;
  
  newmsd = msdnew();
  
  /*
  fprintf (stderr, "%s_%s_%s_%s\n", pktchan->net, pktchan->sta,
	   pktchan->chan, pktchan->loc);
  */

  msdput ( newmsd, MSD_LEVEL, 2, 0 );          /* Steim 2 */
  msdput ( newmsd, MSD_SPARC_ORDER, 1, 0);     /* Network byte-order record */
  msdput ( newmsd, MSD_RECORD_SIZE, 512, 0 );  /* 512-byte record */
  msdput ( newmsd, MSD_HAS_B100, 0, 0 );       /* No 100 Blockette */
  
  msdput ( newmsd,
	   MSD_NET, pktchan->net,
	   MSD_STA, pktchan->sta,
	   MSD_CHAN, pktchan->chan,
	   MSD_LOC, pktchan->loc,
	   MSD_TIME, pktchan->time,
	   MSD_SAMPRATE, pktchan->samprate,
	   0 ) ;
  
  return newmsd;
}


/*********************************************************************
 * isalldig:
 *
 * Return 1 if the specified string is all digits and 0 otherwise.
 *********************************************************************/
int
isalldig (char *check)
{
  int i,len;
  len = strlen(check);

  for ( i=0; i < len; i++ ) {
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
 * Return the number of characters formatted for message, otherwise 0.
 *********************************************************************/
int
lprintf (int level, const char *fmt, ...)
{
  int r = 0;
  char  message[100];
  char  timestr[100];
  va_list argptr;
  time_t loc_time;
  
  if ( level <= verbose ) {
    
    va_start (argptr, fmt);
    
    /* Build local time string and cut off the newline */
    loc_time = time (NULL);
    strcpy (timestr, ctime(&loc_time));
    timestr[strlen(timestr) - 1] = '\0';
    
    r = vsnprintf (message, sizeof(message), fmt, argptr);
    
    printf ("%s - orb_plugin: %s", timestr, message);
    fflush (stdout);
  }

  return r;
}


/*********************************************************************
 * moritician:
 *
 * Do termination sequence and exit.
 *********************************************************************/
void
mortician ()
{
  /* Flush all remaining data streams and close the ORB connection */
  flushstream (NULL);
  orbclose (orb);
  
  /* Bury state file */
  if ( statefile ) 
    {
      if ( (bury()) < 0 )
	lprintf (0, "error saving ORB position to state file\n");
      else
	lprintf (1, "ORB position saved to state file\n");
    }
  
  lprintf (0, "Terminating orb_plugin\n");
  exit (0);
}
