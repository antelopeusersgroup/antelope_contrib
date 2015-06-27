#ifndef diagp_h_included
#define diagp_h_included

#include <sys/time.h>
#include <sys/types.h>
#include <regex.h>
#include "tcl.h"
#include "tk.h"
#include "arrays.h"
#include "defunctpkt.h"


#define ERRR  "error_MSG"
#define ERR_PROC  "error_msg"
#define MSG_PROC  "msg_msg"

#define DASNUM      14    /* DAS # tag in BSP packet    */
#define UID_DC       6    /*  DC ID tag in BSP packet   */
#define UID_DAS      4    /*  DAS ID tag in BSP packet  */
#define DCCMD_TIM   32    /* DC Last Command Time       */
#define DCCMD       36    /* DC Last Command            */

#define BBADAS	0x11
#define BBADC	0x22
#define ANZAPAR	0x33

#define MAXBATT  16.0
#define MINBATT  11.0
#define MAXTEMP  120.0
#define MINTEMP	 50.0
#define MAXUNLOCK  3600.0

typedef struct batrecord {
   Tbl *vpipe;
   int avr;
   char *key;
} Batrecord;

typedef struct tclorb {
	double lcmd_time;
        int orbid;
	int pktid;
	char cmdname[8];
	char orbname[ORBSRCNAME_SIZE];
	int verbose;
	Arr *dpars;
	char *alarmcall;
	char *balarmcall;
	char *lcmdcall;
	char *parcallback;
	char lcmdwidget[256];
	char timwidget[256];
	char clcsel[256];
	Tcl_Interp *interp;
} TclOrb;

typedef struct dpars{
        double ptime;
	double last_lock;
        float crnt_val;
        float min_val;
        float max_val;
        int alert;
        char dpar[ORBSRCNAME_SIZE];
	char widget[256];
	char minwidget[256];
	char maxwidget[256];
} DPars;

	/* parameters, constants, values, etc. for ANZA system   */

#define TINT  5         /* Time interval in sec between packets  */
#define ANZA_DAS     2
#define ANZA_DCDC    5
#define ANZA_DCDAS   9
#define DasNum       16  /* there can be up to 16 Dases maximun  */

static char *ANZA_DAS_NAME[ANZA_DAS] = {
  "BUFDEL", "BATT"
};

static char *ANZA_DCDC_NAME[ANZA_DCDC] = {
  "1PPS", "SEQERR", "STAT", "UNLOCK", "LLOCK" 
};

static char *ANZA_DCDAS_NAME[ANZA_DCDAS] = {
  "CRC", "CHKSUM", "NOCMD", "SEC", "MSEC", "RTXREQ", "RTXRCV", "RTXSKP", "DASDN"
};

static int ANZA_DAS_OFF[ANZA_DAS] = {
  44, 50
};

static int ANZA_DCDC_OFF[ANZA_DCDC] = {
   40, 42, 46, 48, 50
};

static int ANZA_DCDAS_OFF[ANZA_DCDAS] = {
   54, 86, 150, 182, 214, 246, 278, 310, 342  
};

	/* parameters, constants, values, etc. for BBA system   */

#define DCSIZ 		76
#define DASSIZ 		48
#define BBA_DAS  	 7
#define BBA_DCDC 	 4
#define BBA_DCDAS 	 4
#define DCCLOCK  	28   /* DC CLOCK status      */
#define DCCMD_TIM       32   /* DC Last Command Time */
#define DCCMD           36   /* DC Last Command      */

#define NCHAN        21
#define GAIN_OFF     52

static int BBA_DAS_OFF[BBA_DAS] = {
	18, 22, 26, 28, 30, 36, 38 };

static char *BBA_DAS_NAME[BBA_DAS] = {
	"BUFDEL",  "BATT", "DASRES", "RADRES", "OVRFL", "RXERR", "LLOCK" };

static int BBA_DCDAS_OFF[BBA_DCDAS] = {
	28, 30, 32, 34 };

static char *BBA_DCDAS_NAME[BBA_DCDAS] = {
	"CHKSUM", "RTXREQ", "RTXRCV", "RTXSKP" };

static int BBA_DCDC_OFF[BBA_DCDC] = {
	18, 20, 24, 26 };

static char *BBA_DCDC_NAME[BBA_DCDC] = {
	"RECNCT", "TCPSND", "TCPDEL", "BATT" };


extern regex_t dasmatch;
extern regex_t dcmatch;

#endif
