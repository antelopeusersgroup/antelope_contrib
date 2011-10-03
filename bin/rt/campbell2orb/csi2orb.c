#include <unistd.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <termios.h>
#include <time.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <ctype.h>
#include <errno.h>
#include <strings.h>
#include <termios.h>
#include <time.h>
#include <orb.h>
#include <coords.h>
#include <stock.h>
#include <Pkt.h>

/*
 Copyright (c) 2003 - 2006 The Regents of the University of California
 All Rights Reserved

 Permission to use, copy, modify and distribute any part of this software for
 educational, research and non-profit purposes, without fee, and without a
 written agreement is hereby granted, provided that the above copyright
 notice, this paragraph and the following three paragraphs appear in all
 copies.

 Those desiring to incorporate this software into commercial products or use
 for commercial purposes should contact the Technology Transfer Office,
 University of California, San Diego, 9500 Gilman Drive, La Jolla, CA
 92093-0910, Ph: (858) 534-5815.

 IN NO EVENT SHALL THE UNIVESITY OF CALIFORNIA BE LIABLE TO ANY PARTY FOR
 DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES, INCLUDING
 LOST PROFITS, ARISING OUT OF THE USE OF THIS SOFTWARE, EVEN IF THE UNIVERSITY
 OF CALIFORNIA HAS BEEN ADIVSED OF THE POSSIBILITY OF SUCH DAMAGE.

 THE SOFTWARE PROVIDED HEREIN IS ON AN "AS IS" BASIS, AND THE UNIVERSITY OF
 CALIFORNIA HAS NO OBLIGATION TO PROVIDE MAINTENANCE, SUPPORT, UPDATES,
 ENHANCEMENTS, OR MODIFICATIONS.  THE UNIVERSITY OF CALIFORNIA MAKES NO
 REPRESENTATIONS AND EXTENDS NO WARRANTIES OF ANY KIND, EITHER IMPLIED OR
 EXPRESS, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE, OR THAT THE USE OF THE
 SOFTWARE WILL NOT INFRINGE ANY PATENT, TRADEMARK OR OTHER RIGHTS.

   This code was created as part of the ROADNet project.
   See http://roadnet.ucsd.edu/

   Based on code written by: Rock Yuen-Wong 6/2/2003
   This code by: Todd Hansen 12/18/2003
   Last Updated By: Todd Hansen 4/24/2006

   Moved to Contrib CVS: Todd Hansen 9/9/2006
*/

#define VERSION "$Revision$"
#define UNSUCCESSFUL -9999

#define MAXCHANNELS 300
#define MAXRESP 5000

#define DEFAULT_PROG_VS_CHAN 1
#define DEFAULT_YEAR_CHAN 2
#define DEFAULT_DAY_CHAN 3
#define DEFAULT_HOURMIN_CHAN 4
#define DEFAULT_SEC_CHAN 5

#define MAXCAMDELAY 10

#define min(a,b)  (a<b?a:b)

int verbose=0, printprog=0;
double starttime=-1, endtime=-1;
char *ipaddress=NULL;
char *serialport=NULL;
char *port="4000";
char *statefile=NULL;
char *configfile=NULL;
char *orbname=":";
char *srcname="test_sta1";
char *camtimezone="";

int orbfd;
int interval=0;
int OldMemPtr=-1;
int NextMemPtr=1;  /* state pointers */
double previoustimestamp=-1; /* state pointers */
int previousyearstamp=0;
int previousdaystamp=0;
int previoushrstamp=0;
int previoussecstamp=0;
int force=0;
int secondsfield=0;
int chan_length_override=0;
int slop;
int checktime=0;
int kickstatefile=0;
int versioncheck=-1;
int jitterenable=0;
int skewlog=-1;
char *initstr=NULL;
int skewlogvalid=0;
double samintlog=-1;
int samintlogvalid=0;
int settime=0;

struct crack_time_ret_struct
{
    double timestamp;
    int saminterval;
    int prog_vs;
    int prog_ch;
    int year_ch;
    int day_ch;
    int hour_min_ch;
    int sec_ch;
};

void init_serial(char *file_name, struct termios *orig_termios, int *fd, int speed);
int getAttention(int *fd);
void flushOut(int *fd);
int flushUntil(int *fd,char c);
void printProgram(int *fd);
int setMemPtr(int *fd, int location);
int readline(int *fd, char *rebuf);
int initConnection(char *host, char *port);
int dataIntegrityCheck(char *completeResponse);
int stuffline(Tbl *r, char *readbuf);
void getTime(int *fd);
void setTime(int *fd);
int find_speed(char *val);
struct crack_time_ret_struct* crack_timing (Tbl *r, Pf *configpf, char *readbuf);

void usage (void)
{
  cbanner(VERSION,"[-v] [-V] [-d] [-f] [-q] [-x] [-j] [-w] [-l] {[-p serialport] | [-a ipaddress] [-n portnumber]} [-s statefile [-k]] [-t starttime] [-e endtime] [-c net_sta] [-g configfile] [-i interval] [-r serialspeed] [-m arrayid] [-z timezone] [-o $ORB] [-S modeminitstring]","Todd Hansen","UCSD ROADNet Project","tshansen@ucsd.edu");
}

int main(int argc,char *argv[])
{
  int ch;
  int fd=0;
  int speed=B9600;
  int fpass=0;
  struct termios orig_termios;
  char readbuf[MAXRESP];
  char readbuf2[MAXRESP];
  Relic relic;
  int sleeptime;

  elog_init(argc,argv);

  while((ch=getopt(argc,argv,"Vvfjqxlkdwp:a:n:S:m:i:s:t:r:e:c:g:o:z:"))!=-1)
    {
      switch(ch)
	{
	case 'V':
	  usage();
	  exit(-1);
	  break;
	case 'v':
	  verbose=1;
	  break;
	case 'f':
	  force=1;
	  break;
	case 'l':
	  chan_length_override=1;
	  break;
	case 'x':
	  checktime=1;
	  break;
	case 'j':
	  jitterenable=1;
	  break;
	case 'q':
	  secondsfield=1;
	  break;
	case 'w':
	  settime=1;
	  break;
	case 'k':
	  kickstatefile=1;
	  break;
	case 'p':
	  serialport=optarg;
	  break;
	case 'a':
	  ipaddress=optarg;
	  break;
	case 'i':
	  interval=atoi(optarg);
	  break;
	case 'n':
	  port=optarg;
	  break;
	case 'm':
	  versioncheck=atoi(optarg);
	  elog_notify(0,"runtime restricted looking for campbell program version (%d arrayid)\n",versioncheck);
	  break;
	case 's':
	  statefile=optarg;
	  break;
	case 'r':
	  speed=find_speed(optarg);
	  break;
	case 't':
	  starttime=atoi(optarg);
	  elog_notify(0,"runtime restricted looking for data newer than (%.2f)\n",starttime);
	  break;
	case 'e':
	  endtime=atoi(optarg);
	  elog_notify(0,"runtime restricted looking for data older than (%.2f)\n",versioncheck);
	  break;
	case 'c':
	  srcname=optarg;
	  break;
	case 'd':
	  printprog=1;
	  break;
	case 'g':
	  configfile=optarg;
	  break;
	case 'z':
	  camtimezone=optarg;
	  break;
	case 'o':
	  orbname=optarg;
	  break;
	case 'S':
	  initstr=optarg;
	  break;
	default:
	  elog_complain(0,"Invalid argument: %d\n", ch);
	  usage();
	  exit(-1);
	}
    }
  elog_notify(0,"csi2orb %s\n",VERSION);
  if (jitterenable && (!checktime || !(interval>0) || !configfile))
    {
      elog_notify(0,"jitter enable (-j) has no effect unless you specify (-x, -g configfile, and  -i maxinterval)\n");
      jitterenable=0;
    }

  if ((serialport==NULL && ipaddress==NULL) || (serialport!=NULL && ipaddress!=NULL))
    {
      elog_complain(0,"missing or vague arguments, please use one argument of either -a or -p\n\n");
      usage();
      exit(-1);
    }

  if (settime && interval)
  {
      elog_complain(0,"you can't set the time (-w) on the campbell when you set the repeat interval (-i interval), ignoring repeat interval.\n\n");
      interval=0;
  }

  if(statefile!=NULL)
    {
      ch=exhume(statefile,NULL,0,0);
      if (ch<0)
	{
	  elog_complain(1,"exhume failed, returned %d.\n",ch);
	  exit(-1);
	}

      if (ch==0)
	elog_notify(0,"no saved state file found. starting from scratch\n");

      relic.dp=&previoustimestamp;
      if(resurrect("previousTimestamp",relic,DOUBLE_RELIC)==0)
	fprintf(stderr,"resurrected previousTimestamp %f\n",previoustimestamp);
      else
	previoustimestamp=-1;

      relic.ip=&NextMemPtr;
      if(resurrect("NextMemPtr",relic,INT_RELIC)==0)
	fprintf(stderr,"resurrected NextMemPtr %d\n",NextMemPtr);
      else
	NextMemPtr=1;
    }

  if (kickstatefile>0)
    {
      NextMemPtr=1;
      previoustimestamp=-1;
      if (statefile!=NULL)
	elog_notify(0,"ignoring state file's state since you specified the -k option, we will start at the beginning of the data logger and update the state file as we download new data\n");
    }

  if ((orbfd=orbopen(orbname,"w&"))<0)
    {
      elog_complain(0,"orbopen failed");
      return(-1);
    }

  fd=-1;
  while (1)
    {
      if (fd<0)
	{
	  if (verbose)
	    elog_notify(0,"connecting to remote station\n");

	  if (ipaddress)
	    fd=initConnection(ipaddress,port);
	  else
	    {
	      init_serial(serialport, &orig_termios, &fd, speed);
	    }

	  if (initstr && fd>0)
	  {
	      if (write(fd,initstr,strlen(initstr))<strlen(initstr))
	      {
		  elog_complain(1,"write(\"%s\") initstring failed",initstr);
		  close(fd);
		  fd=-1;
	      }
	  }

	  if (fd>0)
	    {
	      if (getAttention(&fd)==UNSUCCESSFUL)
		{
		  close(fd);
		  fd=-1;
		}
	      else
		fpass=1;
	    }
	}

      slop=1;
      if (fd>=0)
	{
	  if (printprog)
	    {
	      printProgram(&fd);
	      break;
	    }
	  if (fpass && checktime)
	    getTime(&fd);

	  if (fpass)
	    fpass=0;

	  if (fd < 0 || setMemPtr(&fd,NextMemPtr)==UNSUCCESSFUL)
	    {
	      elog_complain(0,"setMemPtr(&fd,%d) failed\n",NextMemPtr);
	      close(fd);
	      fd=-1;
	    }
	  else
	    {
	      /*flushOut(&fd);*/
	      if (write(fd,"D\r",2)<2)
		{
		  elog_complain(1,"write(\"D\\r\") failed");
		  close(fd);
		  fd=-1;
		}
	      else
		{
		  if (readline(&fd,readbuf)!=UNSUCCESSFUL)
		  {
		    if (dataIntegrityCheck(readbuf)!=UNSUCCESSFUL)
		    {
			strncpy(readbuf2,readbuf,MAXRESP);
			readbuf2[MAXRESP-1]='\0';
			strtr(readbuf,"\n"," ");
			slop=stuffline(split(readbuf,' '),readbuf2); /* update local pointers and bury if applicable */
		    }
		    else
		      break;
		  }
		}
	    }
	}

      if (slop && settime)
	  setTime(&fd);


      /* if slop then we ran out of data to get */
      if (interval>0 && slop)
	{
	    if (fd>=0)
		if (write(fd,"E\r",2)!=2)
		{
		    elog_complain(1,"write error: while telling campbell we are leaving:");
		    close(fd);
		    fd=-1;
		}

	    close(fd);
	    fd=-2;
	    if (jitterenable && samintlogvalid && skewlogvalid)
	    {
		sleeptime=(int)((previoustimestamp+samintlog+skewlog)-now());
		if (sleeptime>interval || sleeptime<0)
		    sleeptime=interval;
		else if (verbose)
		    elog_notify(0,"sleep shorted. (sleeping for %d sec, interval=%d)\n",sleeptime,interval);

		sleep(sleeptime);
	    }
	    else
		sleep(interval);
	}
      else if (slop)
	break;
    }

  if (fd>=0)
  {
      if (write(fd,"E\r",2)!=2)
      {
	  elog_complain(1,"write error: while telling campbell to sleep before csi2orb exits:");
      }
      close(fd);
  }

  orbclose(orbfd);
  return(0);
}

int stuffline(Tbl *r, char *readbuf)
{
  int channels=0;
  char *c;
  Packet *orbpkt;
  PktChannel *pktchan;
  int ret;
  static char *packet=NULL;
  static int packetsz=0;
  char pfsearch[255], *channame;
  int nbytes;
  int lcv;
  struct crack_time_ret_struct *crack_time_ret=NULL, fake;
  static Pf *configpf=NULL;
  char generatedSourceName[500];
  char channame_cpy[500];
  Srcname srcparts;
  Tbl *chantab;

  /* clean up Tbl */
  c=gettbl(r,0);
  if (c)
      while (strncmp("01",c,2))
      {
	  if (!deltbl(r,0))
	  {
	      elog_complain(0,"this error: stuffline(): deltbl() returns NULL should never occur.\n");
	      exit(-1);
	  }

	  c=gettbl(r,0);
	  if (c==NULL)
	  {
	      freetbl(r,0);
	      if (verbose)
		  elog_notify(0,"No 01 column in response.\n\tAre we done?");
	      return(1);
	  }
      }
  else
  {
      freetbl(r,0);
      if(verbose)
	  elog_notify(0,"No columns in response.\n\tAre we done?");
      return(1);
  }

  /* check for config file */
  if (configfile!=NULL)
    {
      if ((ret=pfupdate(configfile,&configpf))<0)
	{
	  elog_complain(1,"pfupdate(%s,configpf)",configfile);
	  exit(-1);
	}
      else if (ret==1)
	elog_notify(0,"updated config file loaded %s\n",configfile);
    }

  /* find packet format and timestamp */
  crack_time_ret=crack_timing(r, configpf, readbuf);
  if (crack_time_ret==NULL && versioncheck==-1)
  {
      elog_complain(0,"Failed to parse timing in stuffline().\n");
      exit(-1);
  }
  else if (crack_time_ret==NULL)
  {
      elog_complain(0,"Failed to parse timing in stuffline().\nSince you specified, the -m arrayid argument, I am going to ignore the packet.\n\n");

      /* load some values that will allow it to skip through the rest of this code */
      /* if we exit here instead then we don't update the state file with the current mem location */
      fake.timestamp=now();
      fake.saminterval=0;
      fake.prog_vs=-1;
      fake.year_ch=1;
      fake.day_ch=1;
      fake.hour_min_ch=1;
      fake.sec_ch=1;
      crack_time_ret=&fake;
  }

  /* ok, lets stuff */
  orbpkt=newPkt();
  orbpkt->pkttype=suffix2pkttype("MGENC");
  orbpkt->nchannels=0;
  orbpkt->time=crack_time_ret->timestamp;
  split_srcname(srcname,&srcparts);

  while((c=shifttbl(r))!=NULL)
    {
	while(*c!='\0' && !isdigit(*c) && *c != 'A' && *c != 'L')
	    c++;

	if (c[0]=='A' || c[0]=='L')
	{
	    if (*c != 'L')
	    {
		c=shifttbl(r);

		while(*c!='\0' && !isdigit(*c) && *c != 'L')
		    c++;
	    }

	    if (channels<4 || (secondsfield && channels < 5))
	    {
		if (channels != 0 || verbose)
		    elog_complain(0,"this memory location (%d) did not contain enough data elements (%d)\n",OldMemPtr,channels);

		/* don't do it */
		freePkt(orbpkt);
		freetbl(r,0);
		if (channels==0)
		{
		    if (verbose)
			elog_notify(0,"are we done yet?\n");
		    return(1);
		}

		exit(-1);
	    }

	    OldMemPtr=NextMemPtr;
	    NextMemPtr=atoi(c+1);
	    if (verbose)
		elog_notify(0,"NextMemPtr updated (now=%d verbose=%s)\n",NextMemPtr,c);
	    break;
	}
	else if (c[0]!='\0' && (channels+1!=crack_time_ret->year_ch) && (channels+1!=crack_time_ret->day_ch) && (channels+1!=crack_time_ret->hour_min_ch) && (!secondsfield || (channels+1!=crack_time_ret->sec_ch)))
	{
	    chantab=NULL;
	    pktchan = newPktChannel();

	    if (configpf != NULL)
	    {
		sprintf(pfsearch,"%s{%d}{ch%d}",srcname,crack_time_ret->prog_vs,channels+1);
		channame=pfget_string(configpf,pfsearch);

		if (channame != NULL)
		{
		    if (verbose)
			elog_notify(0,"channame=\"%s\"\n",channame);
		    strncpy(channame_cpy,channame,499);
		    channame_cpy[499]='\0';
		    lcv=0;
		    while(channame_cpy[lcv]!='\0')
		    {
			if (isspace(channame_cpy[lcv]))
			    channame_cpy[lcv]=' ';
			lcv++;
		    }

		    chantab=split(channame_cpy,' ');
		    strncpy(pktchan->chan,gettbl(chantab,0),PKT_TYPESIZE);

		    if (!chan_length_override && strlen(pktchan->chan)>8)
		    {
                        /* to make Steve Foley happy */
		        /* (keeps chan names short enough to fit in CSS3.0 */

			elog_complain(0,"The channel name for ch%d is longer than 8 characters (%s).\nYou should fix this or I will take a long walk off a short pier.\n",channels+1,pktchan->chan);
			exit(-1);
		    }
		    else if (verbose && strlen(pktchan->chan)>8)
		    {
			elog_notify(0,"The channel name for ch%d is longer than 8 characters (%s).\nI'll allow this since the override flag is set\n",channels+1,pktchan->chan);
		    }
		}
		else if (orbpkt->time<starttime || (versioncheck!=-1 && versioncheck!=crack_time_ret->prog_vs))
		{ /* we aren't going to write it, so lets set a channel name */
		    sprintf(pktchan->chan,"%d",channels+1);
		}
		else
		{
		    elog_complain(0,"can't add channel %d, no channel name, ignoring packet at postion %d and timestamp %f (verbose=%s)\ncsi2orb is shutting down\n",channels+1,NextMemPtr,orbpkt->time,c);
		    elog_notify(0,"I was searching at this location: %s{%d}{ch%d}\n",srcname,crack_time_ret->prog_vs,channels+1);
		    freePktChannel(pktchan);
		    freePkt(orbpkt);
		    if (chantab!=NULL)
			freetbl(chantab,0);
		    freetbl(r,0);
		    exit(-1);
		}
	    }
	    else
		sprintf(pktchan->chan,"%d",channels+1);

	    pktchan->datasz = 1;
	    pktchan->data=malloc(4);
	    if (pktchan->data==NULL)
	    {
		elog_complain(1,"stuffline(): malloc");
		exit(-1);
	    }

	    if (chantab && maxtbl(chantab)>1)
		pktchan->data[0]=atof(c+2)*atof(gettbl(chantab,1));
	    else
		pktchan->data[0]=atof(c+2)*1000;

	    pktchan->time=orbpkt->time;
	    strncpy(pktchan->net,srcparts.src_net,PKT_TYPESIZE);
	    strncpy(pktchan->sta,srcparts.src_sta,PKT_TYPESIZE);
	    *(pktchan->loc)='\0';
	    pktchan->nsamp=1;

	    if (chantab && maxtbl(chantab)>2)
		strncpy(pktchan->segtype,gettbl(chantab,2),4);
	    else
		strncpy(pktchan->segtype,"c",2);

	    if (chantab && maxtbl(chantab)>1)
		pktchan->calib=1.0/atof(gettbl(chantab,1));
	    else
		pktchan->calib=0.001;

	    pktchan->calper=-1;

	    if (crack_time_ret->saminterval>0)
		pktchan->samprate=1.0/crack_time_ret->saminterval;
	    else
		pktchan->samprate=0;

	    pushtbl(orbpkt->channels,pktchan);
	    orbpkt->nchannels++;

	    if (verbose)
		fprintf(stderr,"adding channel %s (%d) %f\n",pktchan->chan,channels,pktchan->data[0]*pktchan->calib);

	    if (chantab)
	    {
		freetbl(chantab,0);
		chantab=NULL;
	    }
	}

	if (c[0]!='\0')
	    ++channels;
    }

  freetbl(r,0);

  pktchan = newPktChannel();

  sprintf(pktchan->chan,"memloc");

  pktchan->datasz = 1;
  pktchan->data=malloc(4);
  if (pktchan->data==NULL)
  {
      elog_complain(0,"malloc");
      exit(-1);
  }

  pktchan->data[0]=OldMemPtr;
  pktchan->time=orbpkt->time;
  strncpy(pktchan->net,srcparts.src_net,PKT_TYPESIZE);
  strncpy(pktchan->sta,srcparts.src_sta,PKT_TYPESIZE);
  *(pktchan->loc)='\0';
  pktchan->nsamp=1;
  strncpy(pktchan->segtype,"c",2);
  pktchan->calib=1;
  pktchan->calper=-1;
  pktchan->samprate=1.0/crack_time_ret->saminterval;
  pushtbl(orbpkt->channels,pktchan);
  orbpkt->nchannels++;

  previoustimestamp=crack_time_ret->timestamp;

  if (previoustimestamp>endtime && endtime>-0.2)
    {
      elog_complain(0,"current packet (%d) timestamp %s exceeds the endtime of %s - all data downloaded, exiting without sending current packet\n",NextMemPtr,strtime(orbpkt->time),strtime(endtime));
      freePkt(orbpkt);
      orbclose(orbfd);
      exit(0);
    }

  if (previoustimestamp<starttime)
    {
      if (verbose)
	elog_notify(0,"current packet (%d @ %s) is prior to starttime (%s) - skipping packet\n",NextMemPtr,strtime(orbpkt->time),strtime(starttime));
    }
  else if ((versioncheck!=-1) && (versioncheck!=crack_time_ret->prog_vs))
    {
      if (verbose)
	elog_notify(0,"current packet (%d @ %s prog_vs=%d) is not the desired program_vs or array ID (%d) - skipping packet\n",NextMemPtr,strtime(orbpkt->time),crack_time_ret->prog_vs,versioncheck);
    }
  else
    {
      stuffPkt(orbpkt,generatedSourceName,&(crack_time_ret->timestamp),&packet,&nbytes,&packetsz);
      if (verbose)
	showPkt(0,generatedSourceName,crack_time_ret->timestamp,packet,nbytes,stderr,PKT_UNSTUFF);
      orbput(orbfd,generatedSourceName,crack_time_ret->timestamp,packet,nbytes);
    }

  bury();

  freePkt(orbpkt);
  return(0);
}

int dataIntegrityCheck(char *completeResponse)
{
  char checksum[5];
  int loop=0,
    runningChecksum=0,
    cells=0;
  int lc;

  lc=0;
  while(completeResponse[loop]!='C')
    {
      if(completeResponse[loop]=='L')
        lc=1;
      if(lc==0 && completeResponse[loop]=='.')
        cells++;

      runningChecksum+=(unsigned int)completeResponse[loop++];
    }

  runningChecksum+=(int)'C';
  runningChecksum%=8192;

  loop++;
  checksum[0]=completeResponse[loop++];
  checksum[1]=completeResponse[loop++];
  checksum[2]=completeResponse[loop++];
  checksum[3]=completeResponse[loop];
  checksum[4]='\0';
  /* fprintf(stderr,"checksum %d\n",atoi(checksum)); */

  if(runningChecksum!=atoi(checksum))
    {
      elog_complain(0,"dataIntegrityCheck = Checksum error (runningChecksum=%i,checksum=%s\n",runningChecksum,checksum);
      return UNSUCCESSFUL;
    }
  else
    return cells;
}

struct crack_time_ret_struct* crack_timing (Tbl *r, Pf *configpf, char *readbuf)
/* ret = 0 for success, <>0 for failure */
{
    static struct crack_time_ret_struct crack_time_ret;
    Tbl *valsarr=NULL;
    Pf *subpf=NULL;
    char *prog_vs_ch=NULL;
    char *tmp_idx=NULL;
    char pfsearch[255], *val_ch;
    unsigned char exit_flag=0;
    int prog_vs;

    if (configpf == NULL)
    {
	crack_time_ret.prog_vs=0;
	crack_time_ret.prog_ch=DEFAULT_PROG_VS_CHAN;
	crack_time_ret.year_ch=DEFAULT_YEAR_CHAN;
	crack_time_ret.day_ch=DEFAULT_DAY_CHAN;
	crack_time_ret.hour_min_ch=DEFAULT_HOURMIN_CHAN;
	crack_time_ret.sec_ch=DEFAULT_SEC_CHAN;
    }
    else
    {
	if (pfget(configpf,srcname,(void **)&subpf)==PFINVALID)
	{
	    elog_complain(0,"parameter file mis-formed. Variable: %s should be an array.\n",srcname);
	    return(NULL);
	}

	valsarr=pfkeys(subpf);
	if (!valsarr)
	{
	    elog_complain(0,"keysarr() failed. PF Variable: %s should be an array.\n",srcname);
	    return(NULL);
	}

	while (!exit_flag && (prog_vs_ch=poptbl(valsarr)))
	{
	    prog_vs=atoi(prog_vs_ch);

	    sprintf(pfsearch,"%s{%s}{prog_vs_chan}",srcname,prog_vs_ch);
	    if ((tmp_idx=pfget_string(configpf,pfsearch))==NULL)
	    {
		crack_time_ret.prog_ch=DEFAULT_PROG_VS_CHAN;
		if (verbose)
		    elog_notify(0,"prog_vs_chan not defined for %s in parameter file\n",pfsearch);
	    }
	    else
	    {
		sscanf(tmp_idx,"ch%d",&crack_time_ret.prog_ch);
		if (verbose)
		    elog_notify(0,"prog_vs_chan defined as %d (atoi(%s)) for %s\n",crack_time_ret.prog_ch, tmp_idx, pfsearch);
	    }

	    if ((val_ch=gettbl(r, crack_time_ret.prog_ch-1))==NULL)
	    {
		elog_complain(0,"gettbl(r, crack_time_ret.prog_ch-1) failed to return a value\n");
	    }
	    else if (atoi(val_ch+2)==prog_vs)
	    {
		exit_flag=1; /* wohoo */
		if (verbose)
		    elog_notify(0,"found program version (array id) %d\n",prog_vs);
	    }
	    else if (verbose)
		elog_notify(0,"no match: gettbl() returned %s\n",val_ch);
	}

	if (!exit_flag)
	{
	    elog_complain(0,"parameter file entry not found for current data logger array:\n %s\n",readbuf);
	    freetbl(valsarr,0);
	    return(NULL);
	}

	/* now that we have the correct array def, don't forget until we finish unstuff() */
	crack_time_ret.prog_vs=prog_vs;

	sprintf(pfsearch,"%s{%s}{year_chan}", srcname, prog_vs_ch);
	if ((tmp_idx=pfget_string(configpf,pfsearch))==NULL)
	    crack_time_ret.year_ch=DEFAULT_YEAR_CHAN;
	else
	{
	    sscanf(tmp_idx,"ch%d",&crack_time_ret.year_ch);
	    if (verbose)
		elog_notify(0,"year chan = %d\n",crack_time_ret.year_ch);
	}

	sprintf(pfsearch,"%s{%s}{day_chan}", srcname, prog_vs_ch);
	if ((tmp_idx=pfget_string(configpf,pfsearch))==NULL)
	    crack_time_ret.day_ch=DEFAULT_DAY_CHAN;
	else
	{
	    sscanf(tmp_idx,"ch%d",&crack_time_ret.day_ch);
	    if (verbose)
		elog_notify(0,"day chan = %d\n",crack_time_ret.day_ch);
	}

	sprintf(pfsearch,"%s{%s}{hour_min_chan}", srcname, prog_vs_ch);
	if ((tmp_idx=pfget_string(configpf,pfsearch))==NULL)
	    crack_time_ret.hour_min_ch=DEFAULT_HOURMIN_CHAN;
	else
	{
	    sscanf(tmp_idx,"ch%d",&crack_time_ret.hour_min_ch);
	    if (verbose)
		elog_notify(0,"hour_min chan = %d\n",crack_time_ret.hour_min_ch);
	}

	sprintf(pfsearch,"%s{%s}{sec_chan}", srcname, prog_vs_ch);
	if ((tmp_idx=pfget_string(configpf,pfsearch))==NULL)
	{
	    crack_time_ret.sec_ch=DEFAULT_SEC_CHAN;
	}
	else
	{
	    sscanf(tmp_idx,"ch%d",&crack_time_ret.sec_ch);
	    if (verbose)
		elog_notify(0,"sec chan = %d\n",crack_time_ret.sec_ch);
	}

	freetbl(valsarr,0);
    }

    /* find timestamp */
    if ((val_ch=gettbl(r, crack_time_ret.year_ch-1))==NULL)
    {
	elog_complain(0,"year field not found (expected in column #%d) in output array matching program version %d\n",crack_time_ret.year_ch, crack_time_ret.prog_vs);
	return(NULL);
    }
    previousyearstamp=atoi(val_ch+2);

    if ((val_ch=gettbl(r, crack_time_ret.day_ch-1))==NULL)
    {
	elog_complain(0,"day field not found (expected in column #%d) in output array matching program version %d\n",crack_time_ret.day_ch, crack_time_ret.prog_vs);
	return(NULL);
    }
    previousdaystamp=atoi(val_ch+2);

    if ((val_ch=gettbl(r, crack_time_ret.hour_min_ch-1))==NULL)
    {
	elog_complain(0,"hour_min field not found (expected in column #%d) in output array matching program version %d\n",crack_time_ret.hour_min_ch, crack_time_ret.prog_vs);
	return(NULL);
    }
    previoushrstamp=atoi(val_ch+2);

    if (secondsfield)
    {
	if ((val_ch=gettbl(r, crack_time_ret.sec_ch-1))==NULL)
	{
	    elog_complain(0,"second field not found (expected in column #%d) in output array matching program version %d\n",crack_time_ret.sec_ch, crack_time_ret.prog_vs);
	    return(NULL);
	}
	previoussecstamp=atoi(val_ch+2);
    }

    /* check timestamp */
    if (secondsfield)
	sprintf(pfsearch,"%d-%03d %d:%02d:%02d %s",previousyearstamp,previousdaystamp,previoushrstamp/100,previoushrstamp%100,previoussecstamp,camtimezone);
    else
	sprintf(pfsearch,"%d-%03d %d:%d %s",previousyearstamp,previousdaystamp,previoushrstamp/100,previoushrstamp%100,camtimezone);

    crack_time_ret.timestamp=str2epoch(pfsearch);
    if (verbose)
	elog_notify(0,"timestamp: %s -> %s\n",pfsearch,strtime(crack_time_ret.timestamp));

    sprintf(pfsearch,"%s{%d}{sampleinterval}",srcname,prog_vs);
    if (configpf != NULL && !(crack_time_ret.timestamp<starttime) && (versioncheck==-1 || prog_vs == versioncheck))
    {
	crack_time_ret.saminterval=pfget_int(configpf,pfsearch);
	samintlog=crack_time_ret.saminterval;
	samintlogvalid=1;

	if (previoustimestamp>-0.2)
	{
	    if (crack_time_ret.timestamp-previoustimestamp>crack_time_ret.saminterval+crack_time_ret.saminterval*0.05 || crack_time_ret.timestamp-previoustimestamp<crack_time_ret.saminterval-crack_time_ret.saminterval*0.05)
	    {
		if (force)
		    elog_complain(0,"sample interval out of tolerance, ignoring failure (%f should be %d with a tolerance of %f)\n",previoustimestamp-crack_time_ret.timestamp,crack_time_ret.saminterval,crack_time_ret.saminterval*0.05);
		else
		{
		    elog_complain(0,"sample interval out of tolerance, failing, using -f to force this to work (%f should be %d with a tolerance of %f)\n",previoustimestamp-crack_time_ret.timestamp,crack_time_ret.saminterval,crack_time_ret.saminterval*0.05);
		    return(NULL);
		}
	    }
	}

    }
    else
    {
	crack_time_ret.saminterval=0;
	if (verbose)
	{
	    if (versioncheck!=-1 && prog_vs != versioncheck)
		elog_notify(0,"program version not matched, so I won't check for data gaps\n");
	    else if (!(crack_time_ret.timestamp<starttime))
		elog_notify(0,"no config file, so I won't check for data gaps\n");
	    else
		elog_notify(0,"timestamp earlier than start time, so no reason to check for data gaps\n");
	}
    }

    return(&crack_time_ret);
}

void printProgram(int *fd)
{
  char program[10000];

  bzero(program,10000);
  getAttention(fd);
  write(*fd,"7H\r",3);
  flushUntil(fd,'>');
  write(*fd,"*D\r",3);
  sleep(3);
  write(*fd,"1A\r",3);
  sleep(5);
  read(*fd,program,10000);
  fprintf(stderr,"%s\n",program);
  write(*fd,"*0",2);
  write(*fd,"E\r",2);
  flushOut(fd);
  close(*fd);
  *fd=-1;

  exit(0);
}

void getTime(int *fd)
{
  char program[10000];
  int lcv=0;
  double samtime;
  double camtime;
  int year;
  int day;
  Packet *orbpkt;
  PktChannel *pktchan;
  static char *packet=NULL;
  static int packetsz=0;
  int nbytes;
  Srcname srcparts;
  int hr;
  int val;
  int min;
  int sec;
  char pfs[500];
  char generatedSourceName[500];
  fd_set readfd;
  fd_set except;
  int selret;
  int brk;
  struct timeval timeout;

  bzero(program,10000);

  flushOut(fd);

  if (write(*fd,"C\r",2)<2)
    {
      elog_complain(1,"get_time() write()");
      close(*fd);
      *fd=-1;
      return;
    }
  samtime=now();

  val=fcntl(*fd,F_GETFL,0);

  if (val==-1)
  {
      elog_complain(1,"getTime: fcntl(F_GETFL) failed:");
      close(*fd);
      *fd=-1;
      return;
  }

  val|=O_NONBLOCK;

  if (fcntl(*fd,F_SETFL,val)==-1)
  {
      elog_complain(1,"getTime: fcntl(F_SETFL,O_NONBLOCK) failed:");
      close(*fd);
      *fd=-1;
      return;
  }

  do {
        /* prepare for select */
        timeout.tv_sec=MAXCAMDELAY;
        timeout.tv_usec=0;

        FD_ZERO(&readfd);
        FD_SET(*fd,&readfd);

        FD_ZERO(&except);
        FD_SET(*fd,&except);

        selret=select(*fd+1,&readfd,NULL,&except,&timeout);

        if (selret<0)
        {
            elog_complain(1,"getTime: select() on read failed");
            close(*fd);
            *fd=-1;
            return;
        }
        else if (!selret)
        {
            elog_complain(0,"getTime: timed out (%d seconds) in select()\n",MAXCAMDELAY);
            close(*fd);
            *fd=-1;
            return;
        }
	else
	{
	    brk=0;
	    do {
		if (read(*fd,program+lcv,1)<1)
		{
		    if (errno!=EAGAIN)
		    {
			elog_complain(1,"getTime: read()");
			val&=~O_NONBLOCK;
			if (fcntl(*fd,F_SETFL,val)==-1)
			{
			    elog_complain(1,"getTime: fcntl(F_SETFL,blocking) failed:");
			    close(*fd);
			    *fd=-1;
			    return;
			}

			close(*fd);
			*fd=-1;
			return;
		    }
		    else
		    {
			brk=1;
			lcv--;
		    }
		}
		else if (program[lcv]=='\r' || program[lcv]=='\n')
		    program[lcv]='J';

		lcv++;
	    }
	    while (!brk || program[lcv-1]!='*');
	}
  }
  while (program[lcv-1]!='*');


  val&=~O_NONBLOCK;
  if (fcntl(*fd,F_SETFL,val)==-1)
  {
      elog_complain(1,"getTime: fcntl(F_SETFL,blocking) failed:");
      close(*fd);
      *fd=-1;
      return;
  }

  program[lcv-1]='0';
  sscanf(program,"CJJ Y%2d D%4d T%d:%d:%d",&year,&day,&hr,&min,&sec);
  sprintf(pfs,"20%02d-%03d %d:%02d:%02d %s",year,day,hr,min,sec,camtimezone);
  if (zstr2epoch(pfs,&camtime))
  {
      elog_complain(0,"unable to convert time, perhaps we have a formatting issue (str: \"%s\" had %d errors). Disconnecting from Campbell.\n",pfs,-zstr2epoch(pfs,&camtime));
      close(*fd);
      *fd=-1;
  }

  if (verbose)
    elog_notify(0,"time check resp=%s\ttimediff=%d seconds (campbell=%s)\n",pfs,(int)(samtime-camtime),program);

  orbpkt=newPkt();
  orbpkt->time=samtime;
  orbpkt->pkttype=suffix2pkttype("MGENC");
  orbpkt->nchannels=1;
  split_srcname(srcname,&srcparts);
  pktchan = newPktChannel();
  strncpy(pktchan->chan,"timeskew",PKT_TYPESIZE);
  pktchan->datasz = 1;
  pktchan->data=malloc(4);
  pktchan->data[0]=(int)(samtime-camtime);
  pktchan->time=orbpkt->time;
  strncpy(pktchan->net,srcparts.src_net,PKT_TYPESIZE);
  strncpy(pktchan->sta,srcparts.src_sta,PKT_TYPESIZE);
  *(pktchan->loc)='\0';
  pktchan->nsamp=1;
  strncpy(pktchan->segtype,"T",2);
  pktchan->calib=1.0;
  pktchan->calper=-1;
  if (jitterenable && samintlogvalid)
    pktchan->samprate=1.0/samintlog;
  else if (interval>0)
    pktchan->samprate=1.0/interval;
  else
    pktchan->samprate=1;
  pushtbl(orbpkt->channels,pktchan);

  stuffPkt(orbpkt,generatedSourceName,&samtime,&packet,&nbytes,&packetsz);

  split_srcname(generatedSourceName,&srcparts);
  strncpy(srcparts.src_subcode,"stat",PKT_TYPESIZE);
  join_srcname(&srcparts,generatedSourceName);

  if (verbose)
    showPkt(0,generatedSourceName,samtime,packet,nbytes,stderr,PKT_UNSTUFF);
  orbput(orbfd,generatedSourceName,samtime,packet,nbytes);

  freePkt(orbpkt);

  skewlog=(int)(samtime-camtime);
  skewlogvalid=1;

  flushOut(fd);
}

int setMemPtr(int *fd,int location)
{
  char moveCmd[50];
  int moveCmdSize=0;

  if (verbose)
      elog_notify(0,"setting memory pointer to location: %d\n",location);

  if(location==-1)
    moveCmdSize=sprintf(moveCmd,"B\r");
  else
    moveCmdSize=sprintf(moveCmd,"%dG\r",location);

  if (write(*fd,moveCmd,moveCmdSize)<moveCmdSize)
    {
      elog_complain(1,"setMemPtr: write failed:");
      close(*fd);
      *fd=-1;
      return(UNSUCCESSFUL);
    }

  return flushUntil(fd,'*');
}

int getAttention(int *fd)
{
  int loop=0,
    val;
  int ret;
  char prompt[4];

  bzero(prompt,4);

  flushOut(fd);

  val=fcntl(*fd,F_GETFL,0);

  if (val==-1)
  {
      elog_complain(1,"getAttention: fcntl(F_GETFL) failed:");
      close(*fd);
      *fd=-1;
      return UNSUCCESSFUL;
  }

  val|=O_NONBLOCK;

  if (fcntl(*fd,F_SETFL,val)==-1)
  {
      elog_complain(1,"getAttention: fcntl(F_SETFL,O_NONBLOCK) failed:");
      close(*fd);
      *fd=-1;
      return UNSUCCESSFUL;
  }

  while(loop++<10)
    {

	if (write(*fd,"\r",1)!=1)
	{
	    elog_complain(1,"getAttention: write 1 failed:");
	    close(*fd);
	    *fd=-1;
	    return UNSUCCESSFUL;
	}

	sleep(2);

	if (verbose)
	  elog_notify(0,"waiting for ** prompt");

	while ((ret=read(*fd,prompt,4))>0)
        {

	    if(prompt[0]=='*'||prompt[1]=='*'||prompt[2]=='*'||prompt[3]=='*')
            {
		val&=~O_NONBLOCK;
		if (fcntl(*fd,F_SETFL,val)==-1)
		{
		    elog_complain(1,"getAttention: fcntl(F_SETFL,blocking) failed:");
		    close(*fd);
		    *fd=-1;
		    return UNSUCCESSFUL;
		}

		if (verbose)
		    elog_notify(0,"got attention");
		return 0;
            }
        }

	if (ret<0 && errno!=EAGAIN)
	{
	    perror("getAttention(read)");
	    close(*fd);
	    *fd=-1;
	    return(UNSUCCESSFUL);
	}
    }

  val&=~O_NONBLOCK;
  if (fcntl(*fd,F_SETFL,val)==-1)
  {
      elog_complain(1,"getAttention: fcntl(F_SETFL,blocking) failed:");
      close(*fd);
      *fd=-1;
      return UNSUCCESSFUL;
  }

  elog_complain(0,"getAttention() = Could not get attention (prompt[0]=%c,prompt[1]=%c,prompt[2]=%c,prompt[3]=%c)\n",prompt[0],prompt[1],prompt[2],prompt[3]);

  close(*fd);
  *fd=-1;

  return UNSUCCESSFUL;
}

void flushOut(int *fd)
{
  char c;
  int val;
  int read_count;

  if ((val=fcntl(*fd,F_GETFL,0))==-1)
  {
      elog_complain(1,"flushOut: fcntl(F_GETFL) failed:");
      close(*fd);
      *fd=-1;
      return;
  }

  val|=O_NONBLOCK;
  if (fcntl(*fd,F_SETFL,val)==-1)
  {
      elog_complain(1,"flushOut: fcntl(F_SETFL,non-blocking) failed:");
      close(*fd);
      *fd=-1;
      return;
  }

  sleep(6);

  while(read(*fd,&c,1)>0)
  {
   /* BAD hack for SMER GORGE. Not sure what causes ASCII 94 char (foley) */
      if (read_count == 20)
      {
         elog_complain(0, "too many characters read in flushOut(), dying with char:^%c^ and errno: ^%d^...",c,errno);
         close(*fd);
         *fd=-1;
         return;
      }
      /* elog_notify(0,"flushOut() read: %c", c); */
      read_count++;
      /* fprintf(stderr,"%c\n",c); */
  }

  val&=~O_NONBLOCK;
  if (fcntl(*fd,F_SETFL,val)==-1)
  {
      elog_complain(1,"flushOut: fcntl(F_SETFL,blocking) failed:");
      close(*fd);
      *fd=-1;
      return;
  }
}

int flushUntil(int *fd,char c)
{
  char prompt=0;
  int loop=0;
  int val;
  int selret;
  int brk;
  fd_set readfd;
  fd_set except;
  struct timeval timeout;

  if ((val=fcntl(*fd,F_GETFL,0))==-1)
  {
      elog_complain(1,"flushUntil: fcntl(F_GETFL) failed:");
      close(*fd);
      *fd=-1;
      return UNSUCCESSFUL;
  }

  val|=O_NONBLOCK;
  if (fcntl(*fd,F_SETFL,val)==-1)
  {
      elog_complain(1,"flushUntil: fcntl(F_SETFL,non-blocking) failed:");
      close(*fd);
      *fd=-1;
      return UNSUCCESSFUL;
  }

  while(loop<3000)
    {
	/* prepare for select */
	timeout.tv_sec=MAXCAMDELAY;
	timeout.tv_usec=0;

	FD_ZERO(&readfd);
	FD_SET(*fd,&readfd);

	FD_ZERO(&except);
	FD_SET(*fd,&except);

	selret=select(*fd+1,&readfd,NULL,&except,&timeout);

	if (selret<0)
	{
	    elog_complain(1,"flushUntil: select() on read failed");
	    close(*fd);
	    *fd=-1;
	    return UNSUCCESSFUL;
	}
	else if (!selret)
	{
	    elog_complain(0,"flushUntil: timed out (%d seconds) in select()\n",MAXCAMDELAY);
	    close(*fd);
	    *fd=-1;
	    return UNSUCCESSFUL;
	}
	else
	{
	    brk=0;
	    while (!brk && loop++<3000)
	    {
		prompt=0;
		if (read(*fd,&prompt,1)<1)
		{
		    if (errno!=EAGAIN)
		    {
			elog_complain(1,"flushUntil: read");

			val&=~O_NONBLOCK;
			if (fcntl(*fd,F_SETFL,val)==-1)
			{
			    elog_complain(1,"flushUntil: fcntl(F_SETFL,blocking) failed:");
			    close(*fd);
			    *fd=-1;
			    return UNSUCCESSFUL;
			}

			close(*fd);
			*fd=-1;
			return UNSUCCESSFUL;
		    }
		    else
		    {
			brk=1;
			loop--;
		    }
		}
		else if(prompt==c)
		{
		    if (fcntl(*fd,F_SETFL,val)==-1)
		    {
			elog_complain(1,"flushUntil: fcntl(F_SETFL,blocking) failed:");
			close(*fd);
			*fd=-1;
			return UNSUCCESSFUL;
		    }

		    return loop;
		}
	    }
	}
    }

  val&=~O_NONBLOCK;
  if (fcntl(*fd,F_SETFL,val)==-1)
  {
      elog_complain(1,"flushUntil: fcntl(F_SETFL,blocking) failed:");
      close(*fd);
      *fd=-1;
      return UNSUCCESSFUL;
  }

  elog_complain(0,"flushUntil() = overflow in flushUntil (c=%c)\n",c);
  close(*fd);
  *fd=-1;
  return UNSUCCESSFUL;
}

int readline(int *fd, char *rebuf)
{
  int loop=0;
  int val;
  int selret;
  int brk;
  fd_set readfd;
  fd_set except;
  struct timeval timeout;

  val=fcntl(*fd,F_GETFL,0);
  if (val==-1)
  {
      elog_complain(1,"readline: fcntl(F_GETFL) failed:");
      close(*fd);
      *fd=-1;
      return UNSUCCESSFUL;
  }

  val|=O_NONBLOCK;

  if (fcntl(*fd,F_SETFL,val)==-1)
  {
      elog_complain(1,"readline: fcntl(F_SETFL,O_NONBLOCK) failed:");
      close(*fd);
      *fd=-1;
      return UNSUCCESSFUL;
  }

  while(loop<5000)
    {
	/* prepare for select */
	timeout.tv_sec=MAXCAMDELAY;
	timeout.tv_usec=0;

	FD_ZERO(&readfd);
	FD_SET(*fd,&readfd);

	FD_ZERO(&except);
	FD_SET(*fd,&except);

	selret=select(*fd+1,&readfd,NULL,&except,&timeout);

	if (selret<0)
	{
	    elog_complain(1,"readline: select() on read failed");
	    close(*fd);
	    *fd=-1;
	    return UNSUCCESSFUL;
	}
	else if (!selret)
	{
	    elog_complain(0,"readline: timed out (%d seconds) in select()\n",MAXCAMDELAY);
	    close(*fd);
	    *fd=-1;
	    return UNSUCCESSFUL;
	}
	else
	{
	    brk=0;
            while (!brk && loop++<5000)
            {
		if (read(*fd,&(rebuf[loop-1]),1)<1)
		{
		    if (errno!=EAGAIN)
		    {
			elog_complain(1,"readline: read()");

			val&=~O_NONBLOCK;
			if (fcntl(*fd,F_SETFL,val)==-1)
			{
			    elog_complain(1,"readline: fcntl(F_SETFL,blocking) failed:");
			    close(*fd);
			    *fd=-1;
			    return UNSUCCESSFUL;

			}

			close(*fd);
			*fd=-1;
			return UNSUCCESSFUL;
		    }
		    else
		    {
			brk=1;
			loop--;
		    }
		}
		else if(rebuf[loop-1]=='*')
		{
		    rebuf[loop]='\0';

		    if (verbose)
			elog_notify(0,"campbell resp: %s\n",rebuf);

		    val&=~O_NONBLOCK;

		    if (fcntl(*fd,F_SETFL,val)==-1)
		    {
			elog_complain(1,"readline: fcntl(F_SETFL,blocking) failed:");
			close(*fd);
			*fd=-1;
			return UNSUCCESSFUL;
		    }

		    return loop;
		}
	    }
	}
    }

  val&=~O_NONBLOCK;
  if (fcntl(*fd,F_SETFL,val)==-1)
  {
      elog_complain(1,"readline: fcntl(F_SETFL,blocking) failed:");
      close(*fd);
      *fd=-1;
      return UNSUCCESSFUL;
  }

  elog_complain(0,"readline() = overflow in readline (c=%c)\n",rebuf[loop-1]);
  close(*fd);
  *fd=-1;
  return UNSUCCESSFUL;
}

int find_speed(char *val)
{
  int l;

  l=atoi(val);
  if (l==50)
    return B50;
  if (l==75)
    return B75;
  if (l==110)
    return B110;
  if (l==134)
    return B134;
  if (l==150)
    return B150;
  if (l==200)
    return B200;
  if (l==300)
    return B300;
  if (l==600)
    return B600;
  if (l==1200)
    return B1200;
  if (l==1800)
    return B1800;
  if (l==2400)
    return B2400;
  if (l==4800)
    return B4800;
  if (l==9600)
    return B9600;
  if (l==19200)
    return B19200;
  if (l==38400)
    return B38400;
  if (l==57600)
    return B57600;
  if (l==115200)
    return B115200;
#ifdef B230400
  if (l==230400)
    return B230400;
#endif
#ifdef B460800
  if (l==460800)
    return B460800;
#endif

  elog_complain(0,"speed %s is not supported see: /usr/include/sys/termios.h for supported values. Using default: 9600 bps\n",val);
  return B9600;
}


void init_serial(char *file_name, struct termios *orig_termios, int *fd, int speed)
{
  struct termios tmp_termios;

  *fd=open(file_name,O_RDWR);
  if (*fd<0)
    {
      perror("open serial port");
    }

  if (tcgetattr(*fd,&tmp_termios)<0)
    {
      perror("get serial attributes");
    }

  *orig_termios=tmp_termios;

  cfsetispeed(&tmp_termios,speed);
  cfsetospeed(&tmp_termios,speed);
  tmp_termios.c_lflag &= ~(ECHO|ICANON|IEXTEN|ISIG);

  tmp_termios.c_iflag &= ~(BRKINT|ICRNL|INPCK|ISTRIP|IXON);
  tmp_termios.c_cflag &= ~(CSIZE|PARENB);
  tmp_termios.c_cflag |= CS8;
  tmp_termios.c_oflag &= ~OPOST;

  tmp_termios.c_cc[VMIN]=1;
  tmp_termios.c_cc[VTIME]=0;
  if (tcsetattr(*fd,TCSANOW,&tmp_termios)<0)
    {
      perror("set serial attributes");
    }
}

int initConnection(char *host, char *port)
{
  int fd;
  unsigned long ina;
  int nconnected=1;
  struct hostent *host_ent;
  struct sockaddr_in addr;
  int val;

  if (verbose)
     elog_notify(0,"in initConnection host ^%s^ port ^%s^\n",host,port);

  if ( (ina=inet_addr(host)) != -1 )
    {
      memcpy(&addr.sin_addr, &ina,min(sizeof(ina), sizeof(addr.sin_addr)));
    }
  else
    {
      host_ent = gethostbyname(host);

      if ( host_ent == NULL )
	{
	  elog_complain(0,"initConnection = Could not resolve address (host=%s)\n",host);
	  return UNSUCCESSFUL;
	}

      memcpy(&addr.sin_addr, host_ent->h_addr,min(host_ent->h_length, sizeof(addr.sin_addr)));
    }

  /* make socket */
  if( (fd=socket(AF_INET, SOCK_STREAM, 0)) == -1 )
    {
      elog_complain(0,"initConnection = Could not make socket\n");
      return UNSUCCESSFUL;
    }

  /* create address from host ent */
  addr.sin_family = AF_INET;
  addr.sin_port = htons(atoi(port));

  nconnected=connect(fd, (struct sockaddr *) &addr, sizeof(addr));

  if (nconnected)
    {
      elog_complain(1,"initConnection = connect failed\n");
      close(fd);
      return UNSUCCESSFUL;
    }

  val=1;
  if (setsockopt(fd,SOL_SOCKET,SO_KEEPALIVE,&val,sizeof(int)))
    {
      perror("setsockopt(SO_KEEPALIVE)");
      exit(-1);
    }

  if (verbose)
     elog_notify(0, "initConnection successful");

  return fd;
}


void setTime(int *fd)
{ /* defunct */
  char year[6],
    dayOfYear[6],
    hhmm[6],
    sec[6];
  double t;
  int lt;

  getAttention(fd);

  write(*fd,"7H\r",3);
  flushUntil(fd,'>');

  lt=now();
  if (lt%60<55)
  {
      elog_notify(0,"sleeping until close to end of minute, waking at 55 sec  (%d sec)",55-lt%60);
      sleep(55-lt%60);
  }

  t=now()+60;

  sprintf(year,"%.4d",atoi(epoch2str(t,"%Y")));
  sprintf(dayOfYear,"%.4d",atoi(epoch2str(t,"%j")));
  sprintf(hhmm,"%.2d%.2d",atoi(epoch2str(t,"%H")),atoi(epoch2str(t,"%M")));
  sprintf(sec,"%.2d",atoi(epoch2str(t,"%S")));
  sprintf(sec,"%.2d",00);
  elog_notify(0,"setting time to: %s-%s %s %s\n",year,dayOfYear,hhmm,sec);
  write(*fd,"*5",2);
  write(*fd,"A",1);
  write(*fd,year,4);
  write(*fd,"A",1);
  write(*fd,dayOfYear,4);
  write(*fd,"A",1);
  write(*fd,hhmm,4);
  write(*fd,"A",1);
  write(*fd,sec,2);
  write(*fd,"A",1);
  write(*fd,"*0",2);
  write(*fd,"E\r",2);
  flushOut(fd);
  close(*fd);

  fprintf(stderr,"time reset to UTC, exiting\n");

  exit(0);
}

