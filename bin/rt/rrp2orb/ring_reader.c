/* ring_reader.c  -  Read in a ring file created by the RRP system.
   Author D.C. Ketchum May 2007
	 

	 Changes :
	 28-May-2007     Initial version
	 
	 This code allows a user's application to get data from a Ring Buffer
	 created by the RRP or ANSS OutputInfrastructure.  It also creates a 
	 companion to the ring file where status information about the using process's
	 progress in reading the ring.
	 
	 Overall concept - Each block in a ring file has a sequence number which
	 can be computed from its position in the ring file and the next sequence
	 number to be put in the file and the ring size.  These sequence numbers
	 run from 0 - 1,999,999,999 (MAXSEQNUM).  The position of a sequence in the ring file
	 is (seq % ringsize)+1.  The first block is reserved for the status informat
	 in the file.  The ringstruct below represents this ondisk status of the
	 ringwriter and of the user of this process (nextout and maxsize). 
	 
	 To use the unit test code, uncomment the MAIN declaration.  
	 
	 Function calls :
	 
rrp_init(argc, argv)
	 
	 This call initializes access to a ring.  The following are the command line
	 parameters used to configure :
	 -ring  filename   The file name (absolute or relative path) to the ring file
	                   (default="ringbuffer.ring")
	 -mod   nnn        nnn is the modulus causing the process status file updates
	 -ext   string		The string is added to the ring file name and is used as the
	                  file for tracking this process's progress through the file.
										(default=.ringstat).  Use this if separate processes will process
										the ring file.
										
		Non-user configuration:
		-dbg					Turn on more output
		-dbgbin				Use debug mode where seq # are placed over the miniseed sequences
		              to allow debug tracking of sequences (DEBUG ONLY)
		
		Notes:  
		1) if the status file is new, the status nextout is set to newest seq in the ring.
		2) if the nextout in status is out of the ring (is further away than maxsize), the
		   status file nextout is set to the oldest data still in the ring+ 1% of ring size.

rrp_shutdown()

	This call should be made by the user program when it exits.  It close the file and 
	insures the status file is up-to-date with the last sequence processed
	
int rrp_get_next(buf)

	This routine gets the next block out of the ring buffer.  This routine blocks if 
	all of the data from the ring has already been read (ring and status nextout are equal).
	The buf must be as big as the largest possible mini-seed to be returned by this routine.
	The return value is the length of the mini-seed if the mini-seed contains a blockette
	1000.  If no blockette 1000 is present, 0 is return and buf contains 512 new bytes.
	
	
	NOTES: 
	1) This routine updates the status file whenever it is called with no more data available
	    and whenever seq % modulus == 0.
	2) If the desired sequence is out of the ring, the nextout will be adjusted to 
	   oldest data in the ring + 1% of ring size.  If this happens, the user program
		 is not processing the data fast enough!  
	3) The creator of the ring buffer might be configured to return only 512 byte miniseed.
	   if so, any longer packets received at the NOC are broken up into 512 byte miniseed.  
		 You can tell this has happened because the upper most miniseed sequence number will be
		 "6" and the remainder of the sequence number will be of the data returned.  Each 
		 additional block will be higher (e.g. a 4096 with seq=020002 will return 9 blocks
		 with sequences 620002-620010)
		 
		 
int rrp_get_nextout() 

	Return the status files next block to process sequence number.
int rrp_get_size() 		

	Return the ring files size in blocks (does not count the header block)
	
int rrp_get_lastseq() 

	Return the sequence number of the next block to be written into the ring file.
	
	 Changes : Chad Trabant
	 29-June-2007 - Rework:
	                No exiting, but return errors
			Remove verbose status messages
	                Close some potential overflows
			More error checking for system calls, fix error messages
			rrp_init() takes specific arguments instead of command line
			rrp_get_next() takes a int pointer to monitor for shutdown
*/
#include <stdio.h>
#include <stdlib.h>
#include <strings.h>

#include <errno.h>
#include <time.h>
#include <termios.h>
#include <memory.h>
#include <string.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <fcntl.h>

#include "ring_reader.h"
#include "sdr.h"

/* Generic log printing, implemented in rpp2archive.c */
extern int lprintf (int level, const char *fmt, ...);

int stat_modulus=100;	 /* how many blocks between status file updates */
int dbg,dbgbin;

/* Following variables relate to the ring file and its corresponding status file*/
char filename[200]="ringbuffer.ring";	/* if in ring buffer mode, the file name */
char status_ext[20] =".ringstat";       /* extension for status file */
char statfilename[200];	                /* if in ring buffer mode, the file name */
int ringfile;                           /* unit number of open input file */
int statfile;                           /* unit number of open stat file */ 
int dumpstdout;

struct ringstruct {   /* the basic structure of hdr block for a ring */
  int nextout;  /* next sequence number to write out (0-1,999,999,999) */
  int maxsize;  /* size of ring in blocks (2,000,000,000 % maxsize == 0) */
};
struct ringstruct ringhdr;  /* structure for reading 1st block of ring file*/
struct ringstruct stathdr;  /* structure for reading 1st block of ring file*/


/* Initialize the ring reader
 *
 * Returns 0 on success and -1 on error
 */
int rrp_init (char *ringfilename, int modulus, char *statusext)
{
  int i,err;
  struct stat statbuf;
  
  if ( ! ringfilename )
    {
      lprintf (0, "rrp_int(): ringfile is not specified\n");
      return -1;
    }
  
  /* Set initial values */
  ringfile=-1;
  dbg=0;
  dbgbin=0;
  dumpstdout=0;
  
  if ( ringfilename )
    snprintf (filename, sizeof(filename), "%s", ringfilename);
  
  stat_modulus = ( modulus == 0 ) ? 100 : modulus;
  
  if ( statusext )
    snprintf (status_ext, sizeof(status_ext), "%s", statusext);	
  
  /* open the ring file, read only */
  ringfile=open(filename,(O_RDONLY));
  if ( ringfile < 0 )
    {
      lprintf (0, "Error opening ring file=%d %s\n",errno, strerror(errno));
      return -1;
    }
  
  /* read in hdr */
  if ( rrp_read_ring_hdr() )
    return -1;
  
  /* open the status file read/write */
  snprintf (statfilename, sizeof(statfilename), "%s%s", filename, status_ext);
  
  statfile = open(statfilename,(O_RDWR | O_CREAT),0777);
  if ( statfile < 0 )
    {
      lprintf (0, "Failed to open a stat file Permissions? errno=%d %s\n",
	       errno,strerror(errno));
      return -1;
    }
  
  if ( fstat(statfile, &statbuf) )
    {
      lprintf (0, "Failed to fstat the stat file Permissions? errno=%d %s\n",
	       errno,strerror(errno));
      return -1;
    }
  
  if ( statbuf.st_size == 0 )
    {
      stathdr.nextout=ringhdr.nextout-100; /* if we just opened the file, use current value*/
      stathdr.maxsize=ringhdr.maxsize;
      err=write(statfile, &stathdr, 8);
      if ( err == -1 ) 
	{
	  lprintf (0, "rrp_init(): Cannot write to stat file, errno=%d %s\n",
		   errno,strerror(errno));
	  return -1;
	}
    }
  else
    {
      err=read(statfile, &stathdr, 8);
      if (err == -1)
	{
	  lprintf (0, "rrp_init(): Cannot read stat file, errno=%d %s\n",
		   errno,strerror(errno));
	  return -1;
	}
      
      /* If the ring file has changed since we last wrote our status, reset our pointer */
      if ( stathdr.maxsize != ringhdr.maxsize )
	{
	  lprintf (0, "rrp_init(): Stat file size has been changed ring=%d stat=%d reset next=%d\n",
		   ringhdr.maxsize, stathdr.maxsize, ringhdr.nextout);
	  
	  stathdr.nextout=ringhdr.nextout;
	  stathdr.maxsize=ringhdr.maxsize;
	  
	  err=write(statfile, &stathdr, 8);
	  if (err == -1)
	    {
	      lprintf (0, "rrp_init(): Cannot write to stat file, errno=%d %s\n",
		       errno,strerror(errno));
	      return -1;
	    }
	  else
	    {
	      /* Compute if the status nextout is still in the ring, if not set it to oldest+1% */
	      i = ringhdr.nextout - stathdr.nextout;
	      while (i < 0 && i > -100000) {
		usleep(100000);
		rrp_read_ring_hdr();
		i = ringhdr.nextout - stathdr.nextout;
	      }
	      if (i < 0) i += MAXSEQNUM;
	      if (i > ringhdr.maxsize || i < 0)   /* the desired block is not in the ring, use oldest */
		{
		  stathdr.nextout = ringhdr.nextout - ringhdr.maxsize + (ringhdr.maxsize/100);
		  if (stathdr.nextout < 0) stathdr.nextout += MAXSEQNUM;
		}
	      
	      /* Ensure stathdr.nextout will be at least one higher when read next time 
		 This prevents a dead lock where one block of bad data is constantly repeated*/
	      i = stathdr.nextout;
	      stathdr.nextout++;
	      if ( stathdr.nextout >= MAXSEQNUM )
		{
		  lprintf (0, "rrp_init(): Ring buffer sequence wrap %d\n",stathdr.nextout);
		  stathdr.nextout=0;
		}
	      
	      /* position and write out the stat file */
	      if ( rrp_write_stat() )
		return -1;;
	      
	      stathdr.nextout=i;
	    }
	}
    }
  
  return 0;
}  /* End of rrp_init() */

int rrp_get_nextout() {return stathdr.nextout;}
int rrp_get_size()    {return ringhdr.maxsize;}
int rrp_get_lastseq() {return ringhdr.nextout;}

/* shutdown the ring reader, close files and write out last status */
int rrp_shutdown()
{
  rrp_write_stat();
  close (ringfile);
  close (statfile);
  return 0;
}

/* write out the current state to the status file
 *
 * Returns 0 on success and -1 on error */
int rrp_write_stat()
{
  int err;
  
  /* position and write the stat file header */
  err = lseek (statfile, 0, SEEK_SET);/* position stat file */
  if ( err == -1 ) {
    lprintf (0, "rrp_write_stat(): lseek in statfile failed errno=%d %s\n",
	     errno, strerror(errno));
    return -1;
  }
  
  err = write (statfile, &stathdr.nextout, 8);	/* write current status */
  if ( err == -1 ) {
    lprintf (0, "rrp_write_stat(): write update to statfile failed errno=%d %s\n",
	     errno, strerror(errno));
    return -1;
  }
  
  return 0;
}

/* read in the ring buffer header
 *
 * Returns 0 on success and -1 on error */
int rrp_read_ring_hdr()
{
  int err;
  
  /* need to get data off the disk, are we caught up?  if so, update the stat file read ring hdr*/
  
  /* position and read the ring file header */
  err = lseek (ringfile,0, SEEK_SET);
  if (err == -1) {
    lprintf (0, "rrp_read_ring_hdr(): lseek in ring buf failed errno=%d %s\n",
	     errno, strerror(errno));
    return -1;
  }
  
  err = read (ringfile, &ringhdr, 8);
  if (err == -1) {
    lprintf (0, "rrp_read_ring_hdr(): read in ring buf got hft failed errno=%d %s\n",
	     errno, strerror(errno));
    return -1;
  }
  
  return 0;
}

/* read in the next data block.  The mini-seed is returned in buf.
 * function monitors the value of shutdown and will exit when it is non-zero.
 * return value is the mini-seed length in bytes or -1 on error and shutdown */
int rrp_get_next (char *buf, int *shutdown)
{
  struct _sdr_hdr * pnt;				/* point to seed header */
  int i,err, nread, nused;
  
  struct _blockette_1000 *blk1000;	/* point to data only seed blockette*/
  
  /* make buf pointer and check out header */
  pnt = (struct _sdr_hdr *) &buf[0];

 top:
  
  /* If there are not blocks ready to go, sleep and check again */
  while (stathdr.nextout == ringhdr.nextout && ! *shutdown)
    {
      if ( rrp_write_stat() )
	return -1;
      usleep(500000);
      if ( rrp_read_ring_hdr() )
	return -1;
    }

  if ( *shutdown )
    return -1;
  
  /* check to see we are still in the window, if not reset our chasing pointer */
  nused = ringhdr.nextout - stathdr.nextout;
  
  /* if we are waiting for the ring to retransmit a little data on a restart that
     has already been processed, wait for our sequence to come in */
  while ( nused < 0 && nused > -100000 ) {
    usleep(10000000);
    rrp_read_ring_hdr();
    nused = ringhdr.nextout - stathdr.nextout;
  }
  
  if ( nused < 0 )
    nused += MAXSEQNUM;
  
  /* check that we are still in the ring buffer window, if the user is too slow, move up! */
  if ( nused > ringhdr.maxsize )
    {		/* we are no longer in the ring buffer window */
      lprintf (0, "rrp_get_next(): **** Out of ring window Stat.nxout=%d ring.nxout=%d nused=%d max=%d\n",
	       stathdr.nextout, ringhdr.nextout, nused, ringhdr.maxsize); 
      stathdr.nextout = ringhdr.nextout - ringhdr.maxsize+ ringhdr.maxsize/100;
      
      if (stathdr.nextout < 0)
	stathdr.nextout += MAXSEQNUM;
      
      if ( rrp_write_stat() )
	return -1;
    }
  
  /* There is data to process, do so and hand it back to user */
  /* position and read the next data from the ring file */
  err = lseek (ringfile, ((stathdr.nextout % ringhdr.maxsize)+1)*512, SEEK_SET);
  if (err == -1) {
    lprintf (0, "rrp_get_next(): lseek in ring buf failed errno=%d %s\n",
	     errno, strerror(errno));
    return -1;
  }
  err = read (ringfile, buf, 512);
  if (err == -1) {
    lprintf (0, "rrp_get_next(): read in ring buf failed errno=%d %s\n",
	     errno, strerror(errno));
    return -1;
  }
  
  stathdr.nextout++;
  if (stathdr.nextout >= MAXSEQNUM)
    {
      lprintf (0, "rrp_get_next(): ring buffer sequence wrap %d\n", stathdr.nextout);
      stathdr.nextout=0;
    }
  
  blk1000=(struct _blockette_1000 *)&buf[pnt->first_blockette];
  if (buf[6] != 'D') goto top;	/* find a data record */
  nread=512;
  
  /* if its not a 512 byte block, read in the rest */
  if (blk1000->data_rec_len > 9)
    {
      nread = 1 << blk1000->data_rec_len;
      i = 512;
      while (i < nread)
	{
	  if (stathdr.nextout == ringhdr.nextout) 
	    lprintf (0, "rrp_get_next(): ******** ring >512 will lap ringhdr (never happen!) stat=%d ring=%d\n",
		     stathdr.nextout, ringhdr.nextout);
	  err = read (ringfile, &buf[i], 512);
	  if (err == -1) {
	    lprintf (0, "rrp_get_next(): read in ring buf+ failed err=%d errno=%d %s\n",
		     errno, strerror(errno));
	    return -1;
	  }
	  
	  stathdr.nextout++;
	  
	  if (stathdr.nextout >= MAXSEQNUM) {
	    lprintf (0, "rrp_get_next(): ring buffer sequence wrap2 %d\n", stathdr.nextout);
	    stathdr.nextout=0;
	  }

	  i+=512;
	}
    }
  
  /* if we have processed modulus blocks, update the stat file */
  if ( stathdr.nextout % stat_modulus == 0 )
    if ( rrp_write_stat() )
      return -1;
  
  return nread;			/* return length of MiniSeed record! */
}						/* end of subroutine */


/* This main was used to  unit test the ring_reader code including the dbgbin mode.
   Users may use its example, but do not use the dbgbin as this corrupts the sequence
	 headers of the data blocks.
	 
	 example command :
	 ring_reader -ring IRIS_DMC.ring -ext status
*/
#ifdef MAIN
	int main(int argc, char **argv) {
		struct _sdr_hdr * pnt;				/* point to seed header */
		struct _blockette_1000 *blk1000;	/* point to data only seed blockette*/
		int len, i, nout, expected, seq;
		char buf[4096];
		double rate;
		int printout=0;
		rrp_init(argc, argv);
		fprintf(logout,"nextout=%d next=%d size=%d\n",rrp_get_nextout(),rrp_get_lastseq(),
				rrp_get_size());
		expected = rrp_get_nextout();
		for(;;) {
			nout = rrp_get_nextout();
			
			/*  expected should be the nextout */
			if(expected != nout) {
				fprintf(logout,"Not getting expected block.  expected=%d nextout=%d last=%d\n",
						expected, nout, rrp_get_lastseq());
				expected = nout;
			}
			pnt=(struct _sdr_hdr *) &buf[0];
			len = rrp_get_next(buf);
			
			/* Print out the Mini-Seed, convert the sample rate and round to whole number */
			if(printout) {
				if(pnt->sample_rate_factor > 0) rate=pnt->sample_rate_factor;
				else rate=1./(double) -pnt->sample_rate_factor;
				if(pnt->sample_rate_mult > 0) rate=rate * (double) pnt->sample_rate_mult;
				else rate = rate / (double) -pnt->sample_rate_mult;
				rate = (int) (rate + 0.1);
				blk1000=(struct _blockette_1000 *)&buf[pnt->first_blockette];/* assume b1000 is first */
				fprintf(logout,
				"rrp:%d %d %d stn=%c%c%c%c %c%c%c %c%c| sq=%c%c nbk=%d rtn=%d rt=%7.3f fdat=%d fblk=%d ",
					stathdr.nextout, ringhdr.nextout,ringhdr.maxsize,
					pnt->station_id[0],	pnt->station_id[1],	pnt->station_id[2],	
					pnt->station_id[3], pnt->channel_id[0], pnt->channel_id[1],
					pnt->channel_id[2],	pnt->location_id[0], pnt->location_id[1],
					pnt->seq_no[4],pnt->seq_no[5],
					pnt->num_blockettes, nout, rate,
					pnt->first_data, pnt->first_blockette); fflush(logout);
	  			fprintf(logout,"blk100 frm=%d ord=%d len=%d\n",blk1000->format,
	  				blk1000->word_order, blk1000->data_rec_len);
			}

			if(len != 512 && len != 4096) fprintf(logout,"unusual block size=%d seq=%d\n",
					len, nout);
			if(dumpstdout) fwrite(buf, 1, len, stdout);
			if(dbgbin) {
				memcpy(&seq, buf, 4);			/* but binary seq in an int */
				if(seq != expected) {
					if(seq % rrp_get_size() != expected % rrp_get_size()) 
						fprintf(logout," ### Data seq does not match expected in test mode expected=%d got=%d nout=%d\n",
								expected, seq, nout);
					else
						fprintf(logout," ### Data seq does not match but is in right position expected=%d got=%d nout=%d\n",
								expected, seq, nout);
					expected = seq;
				}
			}
			expected++;
			if(expected > MAXSEQNUM) expected=0;
			if(expected % 10000 == 0) fprintf(logout,"%s Processing %d sq=%d r.nout=%d\n",
				asctim(),expected,seq, ringhdr.nextout);
			
		}
	}
#endif
