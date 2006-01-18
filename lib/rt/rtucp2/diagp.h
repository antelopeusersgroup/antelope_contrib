#ifndef diagp_h_included
#define diagp_h_included

#include <sys/time.h>
#include <sys/types.h>
#include <regex.h>

#include "tcl.h"
#include "tk.h"
#include "arrays.h"
#include <Pkt.h>


#define ERRR  "error_MSG"
#define ERR_PROC  "error_msg"
#define MSG_PROC  "msg_msg"

#define UID_DC      10    /*  DC ID tag in BSP packet   */
#define UID_DAS     10    /*  DAS ID tag in BSP packet  */

#define NCHAN       23    /* offset to # of channels */

#if 0
#define DCCMD_TIM   32    /* DC Last Command Time       */
#define DCCMD       36    /* DC Last Command            */
#endif

#define BBADAS	0x11
#define BBADC	0x22

#define MAXBATT  16.0
#define MINBATT  11.0
#define MAXTEMP  120.0
#define MINTEMP	 50.0
#define MAXUNLOCK  3600.0

static Packet *unstuff=0;

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
	char orbname[64];
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
        char dpar[64];
	char widget[256];
	char minwidget[256];
	char maxwidget[256];
} DPars;


extern regex_t dasmatch;
extern regex_t dcmatch;

#endif
