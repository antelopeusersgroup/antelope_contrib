#ifndef dcsp_h_included
#define dcsp_h_included

#include "pkt.h"

#define MAX_DCPAR  4
#define MAXDAS 4
 
/* DC parametrs offset  */
 
#define CHSUM_DC        (2)  /* Checksum                         */
#define BYTES_DC        (4)  /* Total bytes in this packet       */
#define UID_DC          (6)  /* Unit ID                          */
#define BUFID_DC        (8) /* Buffer ID                         */
#define TTAG_DC         (10)  /* Time Tag                        */
#define DASNUM          (14)  /* Number of DASes                 */
#define RESET_DC        (16)  /* DC RESET                        */
#define RECNCT_DC       (18)  /* count of reconnects to TCPsocket */
#define TCPSEND_DC      (20)  /* TCP send count                  */
#define TCPDEL_DC       (24)  /* TCP delay count                 */
#define DCBATT          (26)  /* TDC battery                     */
#define DCCLOCK         (28)  /* DC A/C fail count               */
#define DCCMD_TIM       (32)  /* DC Last Command Time            */
#define DCCMD           (36)  /* DC Last Command                 */

/* DAS parameter offsets  */

#define PORT_DAS       (0)  /* xmit time tag in seconds      */
#define BYTES_DAS      (2)  /* DAS buffer length             */
#define UID_DAS        (4)  /* Unit ID                       */
#define BUFID_DAS      (6) /* Buffer ID                      */
#define TTAG_DAS       (8)  /* Time Tag                      */
#define BKLOG_DAS         (12)  /* DAS packet backlog            */
#define BATT_DAS      (14)  /* DAS battery voltage           */
#define TEMP_DAS      (16)  /* DAS Temperature               */
#define DASRES_DAS        (18)  /* DAS reset                     */
#define RADRES_DAS        (20)  /* DAS radio reset               */
#define OVRFL_DAS     (22)  /* buffer overflow count         */
#define CMDERR_DAS    (24)  /* Command receive errorsd       */
#define PKT_DAS       (26)  /* DASpacket recieved            */
#define CHKSUM_DAS        (28)  /* DAS checksum  errors          */
#define RTXREQ_DAS    (30)  /* DAS rtx request               */
#define RTXREC_DAS    (32)  /* DAS rtx receive               */
#define RTXSKP_DAS    (34)  /* DAS rtx skip                  */
#define SAMP1         (36)  /* DAS 1st sample                */
#define SAMP2         (40)  /* DAS 2nd sample                */
#define SAMP3         (44)  /* DAS 3d  sample                */

#define DCSIZ       76
#define DASSIZ      48

/* DC parameters  */

static char *comname="DC";

/*  The extantions of file name for all  network parameters  */

static char *DCFILE_NAME[MAX_DCPAR] = {
"RECNCT" , "TCPSND", "TCPDEL", "BATT"
};
 
static int DCPAR_OFF[MAX_DCPAR] = {
RECNCT_DC , TCPSEND_DC, TCPDEL_DC, DCBATT
};
 
 
static int DCPAR_BYTE[MAX_DCPAR] = {2,4,2,2};


/*   DAS Parameters  */
 
static int PAR_OFF_DAS[MAXDAS] = {
 CHKSUM_DAS, RTXREQ_DAS, RTXREC_DAS, RTXSKP_DAS 
};

/*  The extantions of file name for all  network parameters  */
 
static char *FILE_NAME_DAS[MAXDAS] = {
  "RTXREQ", "RTXREC" 
};

static int PAR_BYTE_DAS[MAXDAS] = {2,2,2,2 };

typedef struct DcStat {
   double time;
   short status;
			/*
   			" ", the clock is locked with +/- 1uS
 			".", the clock is locked with +/- 10uS 
 			"*", the clock is locked with +/- 100uS
 			"#", the clock is locked with +/- 1000uS
 			"?", the clock is unlocked
 			"!", the DC is not receiving serial data from the clock  
 			*/ 

   short ac_failed;  	/* 0 ==> power is present; 1 ==> no pwer  */
   short hazard;  	/* 0 ==> no hazard condition; 1 => hazard condition is */
   short m1oc;  	/* Master Radio #1 Over_current; 0 => failure and 1 => normal */
   short m2oc;          /* Master Radio #2 Over_current; 0 => failure and 1 => normal */
   short m3oc;          /* Master Radio #3 Over_current; 0 => failure and 1 => normal */
   short oc;            /* Main clock over-current; 0 = failure and 1 => normal  */
   char  clock[6];
} DcStat;
    
#endif
