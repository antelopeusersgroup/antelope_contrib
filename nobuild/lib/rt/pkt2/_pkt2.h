/* $Name $Revision$ $Date$  */ 
/*======================================================================
 *
 *  _pkt2.h
 *
 *  Include file of constants, macros, function declarations, etc 
 *  for RAW packets.
 *
 *  Author: Marina Harkins-Glushko
 *  	    UCSD, IGPP
 *	    glushko@ucsd.edu
 *
 *====================================================================*/
#ifndef _pkt2_h_included
#define _pkt2_h_included
#include <sys/types.h>
#include <stdio.h>
#include "db.h"
#include "coords.h"
#include "stock.h"
#include "pf.h"
#include "tr.h"

#define TRGAP_VALUE_S4  2147483647

#define valINT -1111
#define valSTR -2222
#define valFLT -3333

/*
BBARRAY network.
The following is a structure which defines the DAS data packets.
The data portion is of variable length and is determined by
the "byte_count" field. The byte count field includes the data
bytes and the header. The header is 62 bytes long.
*/
 
/* typedef unsigned long ulong ; */ 
 
typedef struct ethrhdr_bba {
    ushort_t sync;            /* SW sync pattern 0xABCD = 100 SPS */
                              /* SW sync pattern 0xABDE =  40 SPS */
                              /* SW sync pattern 0xABEF =  Status */
    ushort_t checksum;        /* block checksum (16 bit XOR of packet)*/
    ushort_t byte_count;      /* packet length */
    ushort_t unitid;          /* unit id number */
    ushort_t buf_id;          /* buffer id for rexmit*/
    ulong  time_tag;          /* time tag in sec since year begin */
    ulong  xmt_seconds;       /* xmit time tag in seconds */
    uchar_t  send_backlog;    /* send buffer backlog count */
    uchar_t   rexmit;         /* # of rexmit for this buffer */
    uchar_t  smprat;          /* Sample rate */
    uchar_t  gain;            /* channel gain codes */
    ushort_t battery;         /* battery voltage */
    ushort_t temperature;     /* DAS temp reading */
    ushort_t reset_count;     /* DAS reset Count */
    ushort_t radio_reset;     /* Frewave reset count */
    ushort_t overflow_cnt;    /* Buffer overflow count */
    ushort_t last_command;    /* Last received command (blank on reset) */
    ushort_t comm_err_count;  /* command error count */
    ushort_t sync_latch;      /* msecs from DSP packet */
    uchar_t  lock_flag;       /* Locked=1 Unlocked=0 */
    uchar_t  lat_dd;          /* Latitude degress */
    uchar_t  lat_mm;          /* Latitude minutes */
    uchar_t  lat_ss;          /* Latitude seconds */
    uchar_t  lat_hh;          /* Latitude hundreths */
    uchar_t  NS;              /* GPS position */
    ushort_t long_dd;         /* Longitude degress */
    uchar_t  long_mm;         /* Longitude minutes */
    uchar_t  long_ss;         /* Longitude seconds */
    uchar_t  long_hh;         /* Longitude hundreths */
    uchar_t  EW;              /* GPS position */
    short altitude;           /* GPS altitude */
    uchar_t  spare[10];
   
} PACKHDR_BBA;

/*
the following is a packet structure for the status packet from the
remote 44 fo ANZA type network. It is sent every 5 seconds.
*/
 
#if 0
struct dc_status_struct
    {
     struct dchdr_status_struct {
	 ushort_t ID;                      /* ID = "SP" */
	 ushort_t checksum;                /* block checksum */
	 ushort_t byte_count;              /* packet length */
	 ushort_t unitid;                  /* unit id number */
	 ushort_t buf_id;                  /* buffer id for rexmit*/
	 ulong  tag_seconds;             /* time tag in seconds since year begin */
	 ushort_t dc_num_das;              /* number of reporting statio ns */
	 ushort_t dc_reset_count;          /* count of DC resets */
	 ushort_t dc_tcp_reconnect_count;  /* count of reconnects to TCP socket */
	 ulong  dc_tcp_send_count;       /* TCP socket sends */
	 ushort_t dc_tcp_delay_count;      /* TCP send backlog */
	 ushort_t dc_battery;              /* DC battery */
	 uchar_t dc_clk_stat[2];           /* SEE DESCRIPTION BELOW  */
	 ushort_t dc_stat_flag;            /* SEE DESCRIPTION BELOW  */
         ulong  dc_last_command_time;    /* Time of last DC command */
         uchar_t  dc_last_command[40];     /* DC Last command */
     } dc;

     struct das_status_struct  {
        ushort_t port;                    /* SCC port number */
        ushort_t byte_count;              /* DAS buffer length */
        ushort_t unitid;                  /* DAS unit number */
        ushort_t buf_id;                  /* DAS packet ID */
        ulong  tag_seconds;             /* DAS time tag */
        ushort_t send_backlog;            /* DAS packet backup */
        ushort_t battery;                 /* DAS battery voltage */
        ushort_t temp;                    /* DAS temp */
        ushort_t reset_count;             /* DAS reset count */
        ushort_t radio_reset;             /* DAS radio reset count */
        ushort_t overflow_cnt;            /* DAS buffer overflow count */
        ushort_t comm_err_count;          /* Command receive errors */
        ushort_t pkt_cnt;                 /* DAS packets recieved */
        ushort_t chk_cnt;                 /* DAS packet checksum errors */
        ushort_t rtx_req;                 /* DC to DAS rtx requests */
        ushort_t rtx_rcv;                 /* DAS to DC rtx receieves */
        ushort_t rtx_skp;                 /* DC rtx skips */
        ulong  sample[3];               /* DAS sample data */
     } das[40];
} dc_status;
#endif

/*
dc_clock_stat: 
    --------------
    This parameter is 2 bytes long; the 1st byte represents the MAIN clock, the
    2nd byte represents the AUX clock:
	 
	- if the byte is an ASCII "space", the clock is locked with +/- 1uS worse-case accuracy
	- if the byte is an ASCII ".", the clock is locked with +/- 10uS worse-case accuracy
	- if the byte is an ASCII "*", the clock is locked with +/- 100uS worse-case accuracy
	- if the byte is an ASCII "#", the clock is locked with +/- 1000uS worse-case accuracy
	- if the byte is an ASCII "?", the clock is unlocked
	- if the byte is an ASCII "!", the DC is not receiving serial data from the clock
	 
	 
dc_stat_flag:
--------------
    This parameter is 2 bytes long and each bit represents specific info about
    DC:
		 
	bit#1 -  MAINS FAIL; 0 = AC power is present, 1 = AC power is absent (failed)
	bit#2 -  HAZARD; 0 = hazard condition is absent, 1 = hazard condition is present
	bit#3 -  M1 OC* (Master Radio #1 Over-Current failure); 0 = failure, 1 = normal
	bit#4 -  M2 OC* (Master Radio #2 Over-Current failure); 0 = failure, 1 = normal
	bit#5 -  M3 OC* (Master Radio #3 Over-Current failure); 0 = failure, 1 = normal
	bit#6 -  MAIN CLOCK OC* (Main Clock Over-Current failure); 0 = failure, 1 = normal
	bit#6 -  AUX CLOCK OC* (Aux Clock Over-Current failure); 0 = failure, 1 = normal
	bit#8 - reserved
	bit#9 - MAIN CLOCK ON; 0 = off, 1 =  on
	bit#10 - AUX CLOCK ON; 0 = off, 1 =  on
	bit#11 - MAIN/AUX* CLOCK; 0 = DC is using AUX clock, 1 = DC is using MAIN clock
	bit#12 - reserved
	bit#13 - reserved
	bit#14 - reserved
	bit#15 - reserved
	bit#16 - reserved
	 
Note = reference of "*" in 'dc_stat_flag' indicate "active low".

*/

/* PASSCAL: Common Input packet parameters  */

typedef struct PsclPreHdr {
	uchar_t pkttype[2];
	uchar_t experiment[1];
 	uchar_t year[1];
	uchar_t unit[2];
	uchar_t bcdtime[6];
	uchar_t pktsize[2];
	uchar_t pktnum[2];
} PSCLPREHDR;

/* PASSCAL: Calibration Definition Packet - CD  */
 
typedef struct PsclCD {
	struct PsclPreHdr prehdr;
	uchar_t stime[14];
	uchar_t interv[8];
	uchar_t ninterv[4];
	uchar_t length[8];
	uchar_t on_off[4];
	uchar_t period[8];
	uchar_t size[8];
	uchar_t amp[8];
	unsigned char output[4];
} PCD;

/* PASSCAL: Data Stream Packet - DS  */

typedef struct DSInfo {
	char streamnum[2];
	char stream_name[24];
	char channels[16];
	char samprate[4];
	char datatype[2];
	char blank[16];
	char trgtype[4];
	char trginfo[162];
} DSINFO;

typedef struct PsclDS {
	struct PsclPreHdr prehdr;
	struct DSInfo dstreams[4];
} PDS;

/* PASSCAL: DaTa Packet - DT  */

typedef struct PsclDT {
	struct PsclPreHdr prehdr;
	uchar_t eventnum[2];
	uchar_t streamid[1];
	uchar_t chan[1];
	uchar_t nsampl[2];
	uchar_t blank[1];
	uchar_t datatype[1];
	uchar_t data[1000];
} PDT;

/* PASSCAL: Event Header/Trailer Packet - EH, ET  */

typedef struct PsclEH_ET {
	struct PsclPreHdr prehdr;
	uchar_t eventnum[2];
	uchar_t streamid[1];
	uchar_t blank[4];
	uchar_t datatype[1];
	char tmmsg[33];
	char filler[7];
	char stream[24];
	char  samprate[4];
	char trgtype[4];
	char trgtime[16];
	char fsmp_time[16];
	char detrgtim[16];
	char lsmp_time[16];
	char weight[128];
} PEH;


/* PASSCAL: Operating Mode Packet  - OM  */

typedef struct wake_up {
	uchar_t num[2];
	uchar_t stime[12];
 	uchar_t pow_dur[6];
	uchar_t interv[6];
	uchar_t interv_num[2];
	uchar_t blank[36];
} WAKEUP;


typedef struct PsclOM {
        struct PsclPreHdr prehdr;
	uchar_t power_state[2];
	uchar_t recmode[2];
	uchar_t blank[28];
	struct wake_up wake[8];
} POM;

/* PASSCAL: Station-Cahnnel Packet - SC  */

typedef struct PsclChan {
	uchar_t chid[2];
	uchar_t name[10];
	uchar_t azimuth[10];
	uchar_t inclination[10];
	uchar_t xcoord[10];
	uchar_t ycoord[10];
	uchar_t zcoord[10];
	uchar_t xytype[4];
	uchar_t ztype[4];
	uchar_t gain[4];
	char sens_model[12];
	char sensid[12];
	char comment[40];
	uchar_t volt[8];
} PSCLCHAN;

typedef struct PsclSC {
        struct PsclPreHdr prehdr;
	uchar_t expnum[2];
	char expname[24];
	char comment[40];
	uchar_t staid[4];
	char sta[24];
	char sta_comment[40];
	char dastype[12];
	char dasid[12];
	char start[14];
	char clktype[4];
	char clkid[10];
	struct PsclChan ch[5];
} PSC;

/* PASSCAL: State Health Packet  - SH  */

typedef struct PsclSH {
        struct PsclPreHdr prehdr;
	uchar_t blank[8];
	uchar_t info[1000];
} PSH;

typedef struct DASPar {
	int dasid;
	FILE *daslog_fp;
	char daslog[64];
} DASPar;
 
typedef struct Stream {
 	double stime;
	double etime;
	int dasid;
	int id;
        int ev_num;
        int samprate;
	int datatype;
        int nsamp;
        int pktnum;
        int nchan;
} Stream;

typedef struct PktPar {
      float srate;           /* Sample rate  */
      int  nsamp;              /* Packet size in bytes  */
      int  size;              /* Packet size in bytes  */
      int hdrsiz;             /* Size of the header before data points */
      int timtag;             /* Time Tag offset  */
      int nsta;               /* Number stations */
      int nchan;              /* Number channels */
      char datatype[4];
      char pkttype[12];       /* Pkt type - UCSDDP,UCSDSP, UCSDCP,etc*/
      char hdrtype[12];       /* Pkt type - UCSDDP,UCSDSP, UCSDCP,etc*/
      char net_type[32];      /* Net type  */
} PktPar;
 
/* Definition for reading&trimming&writing the structure  */
 
#define PKT_RVL(SP) \
    &(SP)->size, &(SP)->timtag, (SP)->pkttype, (SP)->hdrtype, \
    &(SP)->hdrsiz,&(SP)->nsta, &(SP)->nchan, &(SP)->srate, \
    &(SP)->nsamp, (SP)->datatype,(SP)->net_type
#define PKT_TRIM(SP) \
    TRIM((SP)->datatype,3);TRIM((SP)->pkttype, 11); \
    TRIM((SP)->hdrtype, 11); TRIM((SP)->net_type,31)
#define PKT_SCS "%d %d %s %s %d %d %d %f %d %s %s [^\n] \n"
 
/* Site parameters  */
typedef struct Site {
   double calib;           /* calibration coef  */
   int sid;                /* DAS ID  */
   int sensid;             /* Sensor ID  */
   char pkttype[12];       /* Packet type - CBBHS, CBBLS, etc */
   char name[8];           /* Site name */
   char sens[12];          /* Sensor name  */
} Site;
 
/* Definition for reading&trimming&writing the structure  */
 
#define STE_RVL(SP)  \
(SP)->pkttype,&(SP)->sid,(SP)->name,&(SP)->sensid,(SP)->sens, &(SP)->calib
 
#define STE_TRIM(SP) \
TRIM((SP)->pkttype,11);TRIM((SP)->name,7); TRIM((SP)->sens,11)
 
#define STE_SCS " %s %d %s %d %s %lf[^\n] \n"
 
/* Statistic parameters  */
typedef struct Stat {
   int id;                /* DAS ID  */
   char name[16];           /* Site name */
} Stat;
 
/* Definition for reading&trimming&writing the structure  */
 
#define STAT_RVL(SP) &(SP)->id,(SP)->name
 
#define STAT_TRIM(SP) TRIM((SP)->name,15);
 
#define STAT_SCS " %d %s [^\n] \n"
 
extern char *DASPF;
extern Arr *DasID;       
extern Arr *DcID;       
extern Arr *RTXID;       
extern Arr *StaName;       
extern Arr *StaCh;       
extern Arr *StaID;       
extern Arr *Packets ;       

#endif


/* $Id$ */
