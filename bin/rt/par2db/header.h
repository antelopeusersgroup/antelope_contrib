/* @(#)header.h	1.2 12/26/96  */
/******************************************************************
 *
 *  header.h 
 *
 ********************************************************************/
#ifndef header_h_included
#define header_h_included

#include "pkt.h"
#define MAXNUMPAR_BBA2  7
#define NDAS 16
 
/* Common header block defenitions for bbarray network   */
 
#define PSYNC_BBA2         (0)  /* Packet tag           */
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


/*   BBARRAY parameters  */
 
static int PAR_OFF_BBA2[MAXNUMPAR_BBA2] = {
BUFDEL_BBA2, BATT_BBA2, DASRES_BBA2,
RADRES_BBA2, OVRFL_BBA2, SYNC_BBA2, LLOCK_BBA2 };
/*, 
LLOCK_BBA2, LATDD_BBA2, LATMM_BBA2, LATSS_BBA2, LATHH_BBA2, NS_BBA2,
LONDD_BBA2, LONMM_BBA2, LONSS_BBA2, LONHH_BBA2, EW_BBA2, ALTD_BBA2 
};
*/ 
/*  The extantions of file name for all  network parameters  */
 
static char *FILE_NAME_BBA2[MAXNUMPAR_BBA2] = {
 "BUFDEL",  "BATT:V", "DASRES",
"RADRES", "OVRFL",  "RXERR", "LLOCK" };

/*
, 
"LATDD","LADMM", "LATSS", "LATHH", "NS", "LONDD", "LONMM","LONSS",
"LONHH", "EW", "ALTD" 
 
};
*/ 
static int PAR_BYTE_BBA2[MAXNUMPAR_BBA2] = { 1,2,2,2,
                                  2,2,1};
				  /*,
				  1,1,1,1,1,2,1,1,1,1,2 };  */

typedef struct Pval {
  double time;
  int val;
  FILE *fp;
  char name[128];
} Pval;
  
Pval *DiagPar;

extern int Intv;
int bparameter[MAXNUMPAR_BBA2];


#endif
