/* @(#)header.h	1.2 12/26/96  */
/******************************************************************
 *
 *  header.h 
 *
 ********************************************************************/
#ifndef header_h_included
#define header_h_included

#include "pkt.h"
#define MAXNUMPAR_BBA2  20
#define MAXNUMPAR  21
#define NDAS 16
 
/* Common header block defenitions for ANZA network   */
 
#define TTAG         (8)  /* Time Tag                        */
#define LCMD        (442)  /* Host checksum count           */
 
#define PPSJ         (40)  /* Remote 1PPS jerk count          */
#define TSE          (42)  /* Remote Time Sequence Error cnt  */
#define CCODE        (46)  /* Remote Clock status code (1=NO1HZ, 2=NOCLO CK) */
#define HAZARD       (47)  /* remote hazard or 0xAB = reset */
#define BUFDEL       (48)  /* Count of unlock transitions     */
#define LLOCK        (50)  /* Seconds since last lock */
#define CRC          (54)  /* DAS crc error count */
#define CHKSUM       (86)  /* DAS checksum error count */
#define DASRES      (118)  /* DAS reset                     */
#define DREC        (150)  /* DAS command receive timeout count */        
#define DSECERR     (182)  /* DAS second compare error count */
#define DMSECER     (214)  /* DAS msec error count */
#define DRTXSND     (246)  /* DAS rtx send count */
#define DRTXREC     (278)  /* DAS rtx receive count */
#define DRTSKP      (310)  /* DAS rtx skip count */
#define RTXNUM      (346)  /* DAS no xmit error count */
#define XMTTAG      (378)  /* DAS xmit time error count */
#define OVRFL       (410)  /* buffer overflow count         */
#define HRTXREQ     (508)  /* Host rtx request count */
#define REMBKLG     (510)  /* remote buffer backlog */
#define HRTXSKP     (512)  /* Host rtx request skip count */
 
  
/* Common header block defenitions for bbarray network   */
 
#define PSYNC_BBA2         (0)  /* Packet Sequence Number           */
#define CHSUM_BBA2        (2)  /* Checksum                         */
#define BYTES_BBA2        (4)  /* Total bytes in this packet       */
#define UID_BBA2          (6)  /* Unit ID                          */
#define BUFID_BBA2        (8) /* Buffer ID                         */
#define TTAG_BBA2         (10)  /* Time Tag                        */
#define XMTTAG_BBA2       (14)  /* xmit time tag in seconds       */
#define BUFDEL_BBA2       (18)  /* Remout 44 Unlock count        */
#define RTXNUM_BBA2       (19)  /* # of rexmit for this buffer   */
#define SRATE_BBA2        (20)  /* Sample rate                   */
#define GAIN_BBA2         (21)  /* Gain                          */
#define BATT_BBA2         (22)  /* DAS battery voltage           */
#define TEMP_BBA2         (24)  /* DAS Temperature               */
#define DASRES_BBA2       (26)  /* DAS reset                     */
#define RADRES_BBA2       (28)  /* RADIO RESET                   */
#define OVRFL_BBA2        (30)  /* buffer overflow count         */
#define LCMD_BBA2         (32)  /* Host checksum count           */
#define CMD_ERR           (34)  /* time of last GPS lock         */
#define SYNC_BBA2         (36)  /* time of last GPS lock         */
#define LLOCK_BBA2        (38)  /* time of last GPS lock         */
#define LATDD_BBA2        (39)  /* GPS position                  */
#define LATMM_BBA2        (40)  /* GPS position                  */
#define LATSS_BBA2        (41)  /* GPS position                  */
#define LATHH_BBA2        (42)  /* GPS position                  */
#define NS_BBA2           (43)  /* GPS position                  */
#define LONDD_BBA2        (44)  /* GPS position                  */
#define LONMM_BBA2        (46)  /* GPS position                  */
#define LONSS_BBA2        (47)  /* GPS position                  */
#define LONHH_BBA2        (48)  /* GPS position                  */
#define EW_BBA2           (49)  /* GPS position                  */
#define ALTD_BBA2         (50)  /* Host format error             */
#define spare		  (52)  /*
/* ANZA parameters  */

static char *comname="COMM";

static int PAR_OFF[MAXNUMPAR] = {
PPSJ, TSE, CCODE, HAZARD, BUFDEL, LLOCK, CRC, 
CHKSUM, DASRES, DREC, DSECERR, DMSECER, DRTXSND, DRTXREC,
DRTSKP, RTXNUM, XMTTAG, OVRFL, HRTXREQ, REMBKLG, HRTXSKP
};
 
/*  The extantions of file name for all  network parameters  */
 
static char *FILE_NAME[MAXNUMPAR] = {
"PPSJ", "TSE", "CCODE", "HAZARD", "BUFDEL", "LLOCK", "CRC", 
"CHKSUM", "DASRES", "DREC", "DSECERR", "DMSECER", "DRTXSND", "DRTXREC",
"DRTSKP", "RTXNUM", "XMTTAG", "OVRFL", "HRTXREQ", "REMBKLG", "HRTXSKP"
};
 
static int PAR_BYTE[MAXNUMPAR] = {2,2,1,1,2,4,2,
                                  2,2,2,2,2,2,2,
                                  2,2,2,2,2,2,2 };
 
static int PAR_DAS[MAXNUMPAR] =  {0,0,0,0,0,0,1,
                                  1,1,1,1,1,1,1,
                                  1,1,1,1,2,2,2 };
 
/*   BBARRAY parameters  */
 
static int PAR_OFF_BBA2[MAXNUMPAR_BBA2] = {
PSYNC_BBA2, CHSUM_BBA2, BYTES_BBA2, UID_BBA2, BUFID_BBA2, TTAG_BBA2, XMTTAG_BBA2,
BUFDEL_BBA2, RTXNUM_BBA2, SRATE_BBA2, GAIN_BBA2, BATT_BBA2, TEMP_BBA2, DASRES_BBA2,
RADRES_BBA2, OVRFL_BBA2, LCMD_BBA2, CMD_ERR, SYNC_BBA2 };
/*, 
LLOCK_BBA2, LATDD_BBA2, LATMM_BBA2, LATSS_BBA2, LATHH_BBA2, NS_BBA2,
LONDD_BBA2, LONMM_BBA2, LONSS_BBA2, LONHH_BBA2, EW_BBA2, ALTD_BBA2 
};
*/ 
/*  The extantions of file name for all  network parameters  */
 
static char *FILE_NAME_BBA2[MAXNUMPAR_BBA2] = {
"PSYNC", "CHSUM", "BYTES", "UID", "BUFID", "TTAG", "XMTTAG",
"BUFDEL", "RTXNUM", "SRATE", "GAIN", "BATT:V", "TEMP:F", "DASRES",
"RADRES", "OVRFL", "LCMD", "CMD", "SYNC", "LLOCK" };

/*
, 
"LATDD","LADMM", "LATSS", "LATHH", "NS", "LONDD", "LONMM","LONSS",
"LONHH", "EW", "ALTD" 
 
};
*/ 
static int PAR_BYTE_BBA2[MAXNUMPAR_BBA2] = {2,2,2,2,2,4,4,
                                  1,1,1,1,2,2,2,
                                  2,2,2,2,2,1};
				  /*,
				  1,1,1,1,1,2,1,1,1,1,2 };  */

typedef struct Pval {
  double time;
  int val;
  FILE *fp;
  char name[128];
} Pval;
  
Pval *DiagPar;

extern Ascii;
extern int Intv;
int aparameter[MAXNUMPAR*NDAS];
int bparameter[MAXNUMPAR_BBA2];
char Staid[16][PKT_NAMESIZE];


#endif
