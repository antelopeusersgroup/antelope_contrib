/***************************************************************************
 * orb2ringserver.c:
 *
 * Transmit data from an Antelope ORB to a ringserver (via DataLink)
 *
 * This software requires the Antelope libraries and needs an Antelope
 * build environment to compile.
 *
 * Chad Trabant, IRIS Data Managment Center
 *
 * Modified: 2015.042
 ***************************************************************************/

#include <stdio.h>
#include <string.h>
#include <signal.h>
#include <errno.h>

#include <libdali.h>
#include <libmseed.h>

#include "orb.h"
#include "Pkt.h"

#define PACKAGE   "orb2ringserver"
#define VERSION   "1.4"

static int verbose     = 0;
static char *match     = 0;        /* ORB streams to match */
static char *reject    = 0;        /* ORB streams to reject */
static char *startat   = 0;        /* Starting ORB position */

static int stopsig     = 0;        /* 1: termination requested, 2: termination and no flush */
static char *statefile = 0;        /* A file to save the ORB packet id */
static int stateinter  = 0;        /* Interval to bury the pktid (packets recv'd) */
static int reconnectinterval = 60; /* Interval to wait between reconnection attempts */
static int flushlatency = 500;     /* Flush data buffers if not updated for latency in seconds */

static char *orbaddr   = 0;
static char *rsaddr    = 0;
static int orb         = 0;
static DLCP *dlcp      = 0;

static MSTraceGroup *mstg = 0;

/* Per-trace statistics */
typedef struct tracestats_s
{
  hptime_t earliest;
  hptime_t latest;
  hptime_t update;
  hptime_t xmit;
  int64_t pktcount;
  int64_t reccount;
} TraceStats;

static int handlestream (char *streamname, char *typename, PktChannel *pktchan,
			 char *rawpacket, int nbytes);
static int packtraces (MSTrace *mst, int flush, hptime_t flushtime);
static void sendrecord (char *record, int reclen, void *handlerdata);
static void logmststats ( MSTrace *mst );
static int isalldig (char *check);
static void mortician ();
static int  parameter_proc (int argcount, char **argvec);
static char *getoptval (int argcount, char **argvec, int argopt);
static void elog_printlog (char *msg);
static void elog_printerr (char *msg);

static void
usage(void)
{
  fprintf(stderr,"\n%s: version %s\n", PACKAGE, VERSION);
  fprintf(stderr,"\nUsage: %s <options> ORB ringserver\n\n", PACKAGE);
  fprintf(stderr,"  -v                  Verbosity, up to 3 levels.\n");
  fprintf(stderr,"  -f latency          Flush data buffers with at least latency in seconds\n");
  fprintf(stderr,"  -R interval         Reconnect to ringserver at this interval in seconds\n");
  fprintf(stderr,"  -m match            Regular expression to match ORB packets,\n");
  fprintf(stderr,"                        default is all waveform data.\n");
  fprintf(stderr,"  -r reject           Regular expression to reject ORB packets.\n");
  fprintf(stderr,"  -s pktid|time       Start position in ORB, pktid or time,\n");
  fprintf(stderr,"                        default is next packet.\n");
  fprintf(stderr,"  -S statefile[:pkts] File for saving/restoring the ORB position,\n");
  fprintf(stderr,"                        restored ORB position is overridden by '-s'.\n");
  fprintf(stderr,"                        The position will  be saved every 'pkts' number\n");
  fprintf(stderr,"                        of packets received, if provided.\n");
  fprintf(stderr,"\n");

  exit (1);
}

int
main (int argc, char **argv)
{
  int orbret;
  int retval;
  int ichan;
  double tepoch;
  
  /* For use with the statefile */
  int pktcount = 0;
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
  
  /* Process specified parameters */
  if ( parameter_proc (argc, argv) < 0 )
    {
      ms_log (2, "Argument processing failed\n");
      ms_log (2, "Try '-h' for detailed help\n");
      return -1;
    }
  
  /* Initialize trace buffer */
  if ( ! (mstg = mst_initgroup ( mstg )) )
    {
      ms_log (2, "Cannot initialize MSTraceList\n");
      exit(1);
    }
  
  /* ORB setup */
  if ( (orb = orbopen(orbaddr, "r&")) < 0 )
    {
      ms_log (2, "%s: orbopen() error for %s (%d)\n", argv[0], orbaddr, orb);
      exit(1);
    }

  if ( verbose )
    ms_log (0, "Connected to orbserver at %s\n", orbaddr);
  
  /* Init the ORB position */
  if ( startat )
    {
      if ( verbose )
	ms_log (0, "Initializing ORB position to '%s'\n", startat);
      
      if ( isalldig (startat) )  /* if all digits assume pktid */
	{
	  if ( orbseek (orb, atoi(startat)) == -1 )
	    ms_log (2, "orbseek() error for '%s'\n", startat);
	  else if ( verbose )
	    ms_log (0, "ORB positioned to pktid: %d\n", atoi(startat));
	}
      else if ( is_epoch_string (startat, &tepoch) )
	{
	  orbposition (orb, startat);
	  if ( verbose )
	    ms_log (0, "ORB positioned to time: %s\n", startat);
	}
    }
  
  /* Read/setup the state file */
  if ( statefile )
    {
      if ( verbose )
	ms_log (0, "Resuming ORB position from state file\n");
      
      if ( (exhume(statefile, 0, 0, 0)) < 0 )
	ms_log (2, "exhume() of '%s' failed\n", statefile);
      
      if ( (orbresurrect(orb, &last_pktid, &last_pkttime)) < 0 )
	{
	  ms_log (2, "ORB position resurrection unsuccessful\n");
	}
    }
  
  /* Set the ORB selector and rejector if provided */
  if ( match )
    orbselect(orb, match);
  if ( reject )
    orbreject(orb, reject);
  
  /* Allocate and initialize DataLink connection description */
  if ( ! (dlcp = dl_newdlcp (rsaddr, PACKAGE)) )
    {
      fprintf (stderr, "Cannot allocation DataLink descriptor\n");
      exit (1);
    }
  
  /* Connect to destination DataLink server */
  if ( dl_connect (dlcp) < 0 )
    {
      ms_log (2, "Error connecting to DataLink server: %s\n", dlcp->addr);
      exit(1);
    }
  
  if ( verbose )
    ms_log (0, "Connected to ringserver at %s\n", dlcp->addr);
  
  /* Start the primary loop  */
  while ( ! stopsig )
    {
      orbret = orbreap_timeout ( orb, 1, &pktid, srcname, &time,
				 &rawpacket, &nbytes, &bufsize );
      
      if ( orbret < 0 && orbret != ORB_INCOMPLETE )
	{
	  ms_log (2, "orbreap() error (%d)\n", orbret);
	  stopsig = 1;
	}
      else if ( orbret != ORB_INCOMPLETE ) /* Process the packet */
	{
	  if ( verbose >= 3 )
	    ms_log (0, "Recv'd %s, pktid %d\n", srcname, pktid);
	  
	  retval = unstuffPkt( srcname, time, rawpacket, nbytes, &pkt );
	  
	  if ( retval < 0 )
	    ms_log (2, "Unstuff failure for %s\n", srcname);
	  
	  /* If waveform data process it */
	  if ( retval == Pkt_wf )
	    {
	      char *timestr = 0;
	      
	      for ( ichan = 0; ichan < pkt->nchannels; ichan++ )
		{
		  pktchan = gettbl ( pkt->channels, ichan );
		  
		  snprintf (streamname, sizeof(streamname), "%s_%s_%s_%s",
			    pktchan->net, pktchan->sta, pktchan->loc, pktchan->chan);
		  
		  if ( verbose >= 3 )
		    {
		      timestr = strydtime (pktchan->time);
		      ms_log (0, "Channel: %s nsamp: %d @ %s\n",
			      streamname, pktchan->nsamp, timestr);

		      if ( timestr )
			free (timestr);
		    }
		  
		  /* Send the data to the packager */
		  handlestream ( streamname, pkt->pkttype->name, pktchan, rawpacket, nbytes );
		}
	    }
	  
	  last_pktid = pktid;
	  last_pkttime = time;
	  
	  /* Save the ORB position every 'stateinter' # of packets */
	  if ( statefile && stateinter )
	    {
	      pktcount++;
	      if ( pktcount >= stateinter )
		{
		  pktcount = 0;
		  if ( (bury()) < 0 )
		    ms_log (2, "error saving ORB position to state file\n");
		  else if ( verbose >= 2 )
		    ms_log (0, "ORB position saved to state file\n");
		}
	    }
	} /* End of packet processing */
      
      /* Flush data buffers not updated for flushlatency seconds */
      if ( flushlatency )
	{
	  if ( packtraces (NULL, 0, (dlp_time() - MS_EPOCH2HPTIME(flushlatency))) < 0 )
	    {
	      ms_log (2, "Cannot pack trace buffers or send records!\n");
	    }
	}
      
    } /* End of main client loop */
  
  /* Flush all remaining data streams and close the connections */
  packtraces (NULL, 1, HPTERROR);
  
  orbclose (orb);
  
  if ( dlcp->link != -1 )
    dl_disconnect (dlcp);
  
  /* Bury state file */
  if ( statefile ) 
    {
      if ( (bury()) < 0 )
	ms_log (2, "error saving ORB position to state file\n");
      else if ( verbose )
	ms_log (0, "ORB position saved to state file\n");
    }
  
  if ( verbose )
    {
      MSTrace *mst = mstg->traces;
      
      while ( mst )
	{
	  logmststats (mst);
	  
	  mst = mst->next;
	}
    }
  
  ms_log (0, "Terminating %s\n", PACKAGE);
  
  return 0;
}  /* End of main() */


/*********************************************************************
 * handlestream:
 *
 * Add the packet data to the trace buffer, pack the buffer and send
 * it on.  If the original packet is a 512-byte miniSEED record it
 * will be sent instead unless re-packaging is specifically requested.
 *
 * Returns the number of records packed/transmitted on success and -1
 * on error.
 *********************************************************************/
static int
handlestream (char *streamname, char *typename, PktChannel *pktchan,
	      char *rawpacket, int nbytes)
{
  static MSRecord *msr = NULL;
  MSTrace *mst = NULL;
  int origreclen = 0;
  int recordspacked = 0;
  
  if ( ! (msr = msr_init (msr)) )
    {
      ms_log (2, "Could not (re)initialize MSRecord\n");
      return -1;
    }
  
  /* Populate an MSRecord */
  ms_strncpclean (msr->network, pktchan->net, 2);
  ms_strncpclean (msr->station, pktchan->sta, 5);
  ms_strncpclean (msr->location, pktchan->loc, 2);
  ms_strncpclean (msr->channel, pktchan->chan, 3);
  
  msr->starttime = (hptime_t)(MS_EPOCH2HPTIME(pktchan->time) + 0.5);
  msr->samprate = pktchan->samprate;
  
  msr->datasamples = pktchan->data;
  msr->numsamples = pktchan->nsamp;
  msr->samplecnt = pktchan->nsamp;
  msr->sampletype = ( pktchan->isfloat ) ? 'f' : 'i';
  
  /* If this packet is a 512-byte miniSEED record send the original to
   * the SeedLink server, otherwise add it to the trace buffer packaging.
   *
   * As of Antelope 4.5 the miniSEED record starts 14 bytes from the
   * beginning of a SEED type packet, this is unlikely to change
   * anytime soon.
   */
  
  origreclen = ms_detect (rawpacket+14, (nbytes-14));
  
  if ( origreclen == 512 )
    {
      sendrecord (rawpacket+14, 512, NULL);
      recordspacked = 1;
    }
  else
    {
      /* Add data to trace buffer, creating new entry or extending as needed */
      if ( ! (mst = mst_addmsrtogroup (mstg, msr, 1, -1.0, -1.0)) )
	{
	  ms_log (2, "Cannot add packet data to trace buffer!\n");
	  return -1;
	}
      
      /* To keep small variations in the sample rate or time base from accumulating
       * to large errors, re-base the time of the buffer by back projecting from
       * the endtime, which is calculated from the tracebuf starttime and number
       * of samples.  In essence, this maintains a time line based on the starttime
       * of received tracebufs.  It also retains the variations of the sample rate
       * and other characteristics of the original data stream to some degree. */
      
      mst->starttime = mst->endtime - (hptime_t) (((double)(mst->numsamples - 1) / mst->samprate * HPTMODULUS) + 0.5);
      
      /* Allocate & init per-trace stats structure if needed */
      if ( ! mst->prvtptr )
	{
	  if ( ! (mst->prvtptr = malloc (sizeof(TraceStats))) )
	    {
	      ms_log (2, "Cannot allocate buffer for trace stats!\n");
	      return -1;
	    }
	  
	  ((TraceStats *)mst->prvtptr)->earliest = HPTERROR;
	  ((TraceStats *)mst->prvtptr)->latest = HPTERROR;
	  ((TraceStats *)mst->prvtptr)->update = HPTERROR;
	  ((TraceStats *)mst->prvtptr)->xmit = HPTERROR;
	  ((TraceStats *)mst->prvtptr)->pktcount = 0;
	  ((TraceStats *)mst->prvtptr)->reccount = 0;
	}
      
      ((TraceStats *)mst->prvtptr)->update = dlp_time();
      ((TraceStats *)mst->prvtptr)->pktcount += 1;
      
      if ( (recordspacked = packtraces (mst, 0, HPTERROR)) < 0 )
	{
	  ms_log (2, "Cannot pack trace buffer or send records!\n");
	  return -1;
	}
    }
  
  return recordspacked;
}  /* End of handlestream() */


/*********************************************************************
 * packtraces:
 *
 * Package remaining data in buffer(s) into miniSEED records.  If mst
 * is NULL all streams will be packed, otherwise only the specified
 * stream will be packed.
 *
 * If the flush argument is true the stream buffers will be flushed
 * completely, otherwise records are only packed when enough samples
 * are available to fill a record.
 *
 * Returns the number of records packed on success and -1 on error.
 *********************************************************************/
static int
packtraces (MSTrace *mst, int flush, hptime_t flushtime)
{
  static struct blkt_1000_s Blkt1000;
  static struct blkt_1001_s Blkt1001;
  static MSRecord *mstemplate = NULL;

  MSTrace *prevmst;
  void *handlerdata = mst;
  int trpackedrecords = 0;
  int packedrecords = 0;
  int flushflag = flush;
  int encoding;
  
  /* Set up MSRecord template, include blockette 1000 and 1001 */
  if ( (mstemplate = msr_init (mstemplate)) == NULL )
    {
      ms_log (2, "Cannot initialize packing template (out of memory?)\n");
      return -1;
    }
  else
    {
      mstemplate->dataquality = 'D';
      
      /* Add blockettes 1000 & 1001 to template */
      memset (&Blkt1000, 0, sizeof(struct blkt_1000_s));
      msr_addblockette (mstemplate, (char *) &Blkt1000,
                        sizeof(struct blkt_1001_s), 1000, 0);
      memset (&Blkt1001, 0, sizeof(struct blkt_1001_s));
      msr_addblockette (mstemplate, (char *) &Blkt1001,
                        sizeof(struct blkt_1001_s), 1001, 0);
    }

  if ( mst )
    {
      encoding = (mst->sampletype == 'f') ? DE_FLOAT32 : DE_STEIM2;
      
      strcpy (mstemplate->network, mst->network);
      strcpy (mstemplate->station, mst->station);
      strcpy (mstemplate->location, mst->location);
      strcpy (mstemplate->channel, mst->channel);
      
      trpackedrecords = mst_pack (mst, sendrecord, handlerdata, 512,
    				  encoding, 1, NULL, flushflag,
 				  verbose-2, mstemplate);
      
      if ( trpackedrecords == -1 )
 	return -1;
      
      packedrecords += trpackedrecords;
    }
  else
    {
      mst = mstg->traces;
      prevmst = NULL;
      
      while ( mst && stopsig != 2 )
	{
	  if ( mst->numsamples > 0 )
	    {
	      encoding = (mst->sampletype == 'f') ? DE_FLOAT32 : DE_STEIM2;
	      
	      /* Flush data buffer if update time is less than flushtime */
	      if ( flush == 0 && mst->prvtptr && flushtime != HPTERROR )
		if (((TraceStats *)mst->prvtptr)->update < flushtime )
		  {
                    if ( verbose >= 1 )
                      ms_log (0, "Flushing data buffer for %s_%s_%s_%s\n",
                              mst->network, mst->station, mst->location, mst->channel);
		    flushflag = 1;
		  }
	      
              strcpy (mstemplate->network, mst->network);
              strcpy (mstemplate->station, mst->station);
              strcpy (mstemplate->location, mst->location);
              strcpy (mstemplate->channel, mst->channel);

	      trpackedrecords = mst_pack (mst, sendrecord, handlerdata, 512,
					  encoding, 1, NULL, flushflag,
					  verbose-2, mstemplate);
	      
	      if ( trpackedrecords == -1 )
		return -1;
	      
	      packedrecords += trpackedrecords;
	    }
	  
	  /* Remove trace buffer entry if no samples remaining */
	  if ( mst->numsamples <= 0 )
	    {
	      MSTrace *nextmst = mst->next;
	      
	      if ( verbose )
		logmststats (mst);
	      
	      if ( ! prevmst )
		mstg->traces = mst->next;
	      else
		prevmst->next = mst->next;
	      
	      mst_free (&mst);
	      
	      mst = nextmst;
	    }
	  else
	    {
	      prevmst = mst;
	      mst = mst->next;
	    }
	}
    }
  
  return packedrecords;
}  /* End of packtraces() */


/*********************************************************************
 * sendrecord:
 *
 * Routine called to send a record to the DataLink server.
 *
 * Returns 0
 *********************************************************************/
static void
sendrecord (char *record, int reclen, void *handlerdata)
{
  static MSRecord *msr = NULL;
  MSTrace *mst = handlerdata;
  TraceStats *stats;
  hptime_t endtime;
  char streamid[100];
  int writeack = 0;
  int rv;
  
  if ( ! record )
    return;
  
  /* Parse Mini-SEED header */
  if ( (rv = msr_unpack (record, reclen, &msr, 0, 0)) != MS_NOERROR )
    {
      ms_recsrcname (record, streamid, 0);
      ms_log (2, "sendrecord(): Error unpacking %s: %s", streamid, ms_errorstr(rv));
      return;
    }
  
  /* Generate stream ID for this record: NET_STA_LOC_CHAN/MSEED */
  msr_srcname (msr, streamid, 0);
  strcat (streamid, "/MSEED");
  
  /* Determine high precision end time */
  endtime = msr_endtime (msr);
  
  if ( verbose >= 2 )
    ms_log (0, "Sending %s\n", streamid);
  
  /* Send record to server, loop */
  while ( dl_write (dlcp, record, reclen, streamid, msr->starttime, endtime, writeack) < 0 )
    {
      if ( dlcp->link == -1 )
	dl_disconnect (dlcp);
      
      if ( stopsig )
	{
	  ms_log (2, "Termination signal with no connection to DataLink, the data buffers will be lost");
	  stopsig = 2;
	  break;
	}
      
      if ( ! reconnectinterval )
	{
	  stopsig = 2;
	  break;
	}
      else if ( dl_connect (dlcp) < 0 )
	{
	  ms_log (2, "Error re-connecting to DataLink server: %s, sleeping\n", dlcp->addr);
	  dlp_usleep (reconnectinterval * 1e6);
	}
    }
  
  /* Update stats */
  if ( mst )
    {
      stats = (TraceStats *)mst->prvtptr;
      
      if ( stats->earliest == HPTERROR || stats->earliest > msr->starttime )
	stats->earliest = msr->starttime;
      
      if ( stats->latest == HPTERROR || stats->latest < endtime )
	stats->latest = endtime;
      
      stats->xmit = dlp_time();
      stats->reccount += 1;
    }
}  /* End of sendrecord() */


/*********************************************************************
 * logmststats:
 *
 * Log MSTrace stats.
 *********************************************************************/
static void
logmststats ( MSTrace *mst )
{
  TraceStats *stats;
  char etime[50];
  char ltime[50];
  char utime[50];
  char xtime[50];
  
  stats = (TraceStats *) mst->prvtptr;
  ms_hptime2mdtimestr (stats->earliest, etime, 1);
  ms_hptime2mdtimestr (stats->latest, ltime, 1);
  ms_hptime2mdtimestr (stats->update, utime, 1);
  ms_hptime2mdtimestr (stats->xmit, xtime, 1);
  
  ms_log (0, "%s_%s_%s_%s, earliest: %s, latest: %s\n",
	  mst->network, mst->station, mst->location, mst->channel,
	  (stats->earliest == HPTERROR) ? "NONE":etime,
	  (stats->latest == HPTERROR) ? "NONE":ltime);
  ms_log (0, "  last update: %s, xmit time: %s\n",
	  (stats->update == HPTERROR) ? "NONE":utime,
	  (stats->xmit == HPTERROR) ? "NONE":xtime);
  ms_log (0, "  pktcount: %lld, reccount: %lld\n",
	  (long long int) stats->pktcount,
	  (long long int) stats->reccount); 
}  /* End of logmststats() */


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
}  /* End of isalldig() */


/*********************************************************************
 * moritician:
 *
 * Do termination sequence and exit.
 *********************************************************************/
void
mortician ()
{
  stopsig = 1;
}


/***************************************************************************
 * parameter_proc:
 *
 * Process the command line parameters.
 *
 * Returns 0 on success, and -1 on failure.
 ***************************************************************************/
static int
parameter_proc (int argcount, char **argvec)
{
  char *tptr = NULL;
  int errflag = 0;
  
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
	  usage();
          exit (0);
        }
      else if (strncmp (argvec[optind], "-v", 2) == 0)
	{
	  verbose += strspn (&argvec[optind][1], "v");
	}
      else if (strcmp (argvec[optind], "-f") == 0)
	{
	  flushlatency = strtol (getoptval(argcount, argvec, optind++), NULL, 10);
	}
      else if (strcmp (argvec[optind], "-R") == 0)
	{
	  reconnectinterval = strtol (getoptval(argcount, argvec, optind++), NULL, 10);
	}
      else if (strcmp (argvec[optind], "-m") == 0)
	{
	  match = getoptval(argcount, argvec, optind++);
	}
      else if (strcmp (argvec[optind], "-r") == 0)
	{
	  reject = getoptval(argcount, argvec, optind++);
	}
      else if (strcmp (argvec[optind], "-s") == 0)
	{
	  startat = getoptval(argcount, argvec, optind++);
	}
      else if (strcmp (argvec[optind], "-S") == 0)
	{
	  statefile = getoptval(argcount, argvec, optind++);
	}
      else if (strncmp (argvec[optind], "-", 1) == 0)
	{
	  fprintf (stderr, "Unknown option: %s\n", argvec[optind]);
	  exit (1);
	}
      else if ( ! orbaddr )
        {
          orbaddr = argvec[optind];
        }
      else if ( ! rsaddr )
        {
          rsaddr = argvec[optind];
        }
      else
	{
	  fprintf (stderr, "Unknown option: %s\n", argvec[optind]);
	  exit (1);
	}
    }
  
  /* Initialize the Antelope elog system */
  elog_init(argcount, argvec);
  
  /* Initialize the logging for the dl_log family */
  dl_loginit (verbose-1, &elog_printlog, NULL, &elog_printerr, NULL);
  
  /* Initialize the logging for the ms_log family */
  ms_loginit (&elog_printlog, NULL, &elog_printerr, NULL);  
  
  /* Make sure a source ORB server was specified */
  if ( ! orbaddr )
    {
      ms_log (2, "No source ORB server specified\n");
      errflag++;
    }
  
  /* Make sure a destination DataLink server was specified */
  if ( ! rsaddr )
    {
      ms_log (2, "No destination DataLink server specified\n");
      errflag++;
    }
  
  /* Separate state file name from time interval if needed */
  if ( statefile && (tptr = strchr(statefile, ':')) != NULL )
    {
      *tptr++ = '\0';
      
      if ( strlen(tptr) == 0 || isalldig(tptr) == 0 )
	{
	  ms_log (2, "Unrecognized state file saving interval: '%s'\n", tptr);
	  errflag++;
	}
      
      stateinter = (int) strtol(tptr, NULL, 10);
    }
  
  if ( errflag )
    {
      ms_log (0, "Usage: %s [options] ORB ringserver\n", PACKAGE);
      ms_log (0, "Try '-h' for detailed help\n");
      exit (1);
    }
  
  /* Report the program version */
  ms_log (0, "%s version: %s\n", PACKAGE, VERSION);

  return 0;
}  /* End of parameter_proc() */


/***************************************************************************
 * getoptval:
 *
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
    fprintf (stderr, "getoptval(): NULL option requested\n");
    exit (1);
  }

  if ( (argopt+1) < argcount && *argvec[argopt+1] != '-' )
    return argvec[argopt+1];

  fprintf (stderr, "Option %s requires a value\n", argvec[argopt]);
  exit (1);
}  /* End of getoptval() */


/***************************************************************************
 * elog_printlog():
 * A hook to re-direct sl_log() (libslink) log messages to elog_notify.
 ***************************************************************************/
static void
elog_printlog (char *msg)
{
  elog_notify(0, "%s", msg);
}


/***************************************************************************
 * elog_printerr():
 * A hook to re-direct sl_log() (libslink) error messages to elog_complain.
 ***************************************************************************/
static void
elog_printerr (char *msg)
{
  elog_complain(0, "%s", msg);
}
