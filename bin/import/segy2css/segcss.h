/* $Name $Revision$ $Date$  */
/*======================================================================
 *
 *  segy2css/segcss.h
 *
 *  Include file of constants, macros, function declarations, etc.
 *
 *====================================================================*/
#ifndef segcss_h_included
#define segcss_h_included
#include <sys/types.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <assert.h>
#include <errno.h>
#include <time.h>
#include <unistd.h>
#include "util.h"
#include <fcntl.h>
#include <stdio.h>


#define MODE  (0664)
#define  DATAF "YYDDDHHMMSS.w"
#define  WD_FNAME   "YYDDDHHMMSS.wfdisc"
#define  BUF_SIZE  120
#define DATA_OFF  (240)

#define CH_OFF     7
#define  YR_OFF    78
#define SMPL_OFF  (57)
#define RATE_OFF  (58)
#define RATE_LONG_OFF (100)
#define DFORMAT    102
#define MSEC_OFF  (103)
#define SN_OFF     112

int Wfid;    /* Waveform identifier. Look css 3.0 Database structure  */
int Byevent; /* if Yes, then for all SEGY data files from one directory
                make one wfdisc file, even data have different start time
                In other case for each set of data with different
                start time build different 'wfdisc' files  */
FILE *Fp_wfd;
int Fp_out;
int Channel;       /* Channel number  */
long Data_bytes;       /* Number of data samples   */
long Sample;       /* Number of data samples   */
long DOFFSET;       /* current offset for foff wfdisc field  */
int Dformat;
int Wfid;

typedef struct conver {
  int sernum;
  char stime[20];
  char etime[20];
  char sname[8];

} CONVER;

static struct conver conver_null = {
	-999,
	"_",
	"_",
	"_"
};

#define CONVER_SCS "%d %s %s %s[^\n] \n"
#define CONVER_RVL(SP) &(SP)->sernum, (SP)->stime, (SP)->etime, (SP)->sname
#define CONVER_TRM(SP) TRIM((SP)->stime,19); TRIM((SP)->etime, 19); TRIM((SP)->sname, 7) 

        struct data {
            int yr;
            int day;
            int hour;
            int min;
            int sec;
            short msec;
        } ;

        struct name  {
            char sta[8];
            char chan[8];
            char *dataf;
            char *fwd;
            int cnt;
        } ;

#ifndef leap_year
#define leap_year(i) ((i % 4 == 0 && i % 100 != 0) || i % 400 == 0)
#endif
#ifndef dysize
#define dysize(i) (365 + leap_year(i))
#endif
#define SPM    (long)(60)
#define SPH    (long)(3600)
#define SPD    (long)(86400)
#define MSPS (double)(1000)

/* Extern functions  */

extern void usage();
extern char *decode();

#endif

/* $Id$ */
