#include <stdio.h>
#include <string.h>
#include <sys/termios.h>
#include <fcntl.h> 
#include <errno.h>
#include <time.h>
#include <signal.h>
        
#include <sys/param.h>
#include <sys/types.h>
#include <xview/xview.h>
#include <xview/panel.h>
#include <xview/textsw.h>
#include <xview/xv_xrect.h>
#include <xview/notice.h>
#include "pkt.h"
 
#define DATA_PORT   "/dev/ttyb"
#define CMND_PORT   "/dev/ttya"

#define isaleap(year) ((((year)%100 != 0) && ((year)%4 == 0)) || ((year)%400 == 0))

#define PLEN	80    /* B3S2 packets are 80 bytes long  */
#define ORBERR  100   /* max error 3 of the send2orb  */ 

FILE           *outfile[3], *wfdiscfile;        /* output file pointer */

char 		*orbname; 
int Log;
int             cmnd_port;   /* file handle for modem tty port */
    
int 		orbfp, orb;  /* orb file descriptor */
int             packet_index, silent_flag;
   
int             capture_flag, capture_count, capture_index, capture_length, capture[20]; 
struct tm       capture_time, current_time;
unsigned int    sec_tear, msec_tear, das_reset_count;
long             time_sec;
struct tm      *ltime;
int             emla_set, cal_set;
   
void catchint();

