/* %W% %G% */
/*===========================================================================
 *
 *
 *    segy2css/wrt_segwfd.c
 *
 *    Write wfdisc file with the name YYDDDHHMMSS.wfdisc'                                      
 *                                                  
 *
 =========================================================================*/
#include <time.h>
#include "coords.h"
#include "wfdiscio.h"
#include "segcss.h"
#include "stock.h" /* for declaration of std_now() */


int wrt_segwfd(buffer, dates, names,dname)
short *buffer;                            /* Buffer contains SEGY header information  */
struct data *dates;              /* Structure with the start time    */
struct name *names;              /* Structure with the names of the current SEGY file */
char *dname;                     /* Name of the file in SEGY format   */

{

        struct wfdisc wfd;
        double rate_long;
        double dtime;
        long *lpntr, l_rate;
        long ldate;
        float rate;
	int i, j, k, smp_size;


    if( (lpntr = (long *) malloc(8) ) == NULL )  {
       perror("segy2css/wrt_segwfd(): malloc");
       return 0;
    }

/* Write data file in css 3.0 format to the disk  */
 
     if(!wrt_segdata(dname, names) ) return 0; 
   
/*  Initialize wfdisc structure  */

    wfd = wfdisc_null;

/*  Get parameters from SEGY files header and fill wfdisk structure  */    

    rate   = (float) *(buffer + RATE_OFF); 
    if(rate == 1)  {
       memcpy((char *)lpntr, buffer + RATE_LONG_OFF, 8);
       l_rate = (long) *lpntr;
       wfd.samprate = 1000000.0/l_rate;
    }  else wfd.samprate = 1000000.0/rate;  

    ldate = dates->yr * 1000 + dates->day;
    dtime = dtoepoch( ldate ) +
            3600.0*dates->hour + 
            60.0*dates->min + 
            dates->sec + (double)dates->msec/1000.0;
 
    strcpy(wfd.sta, names->sta );
    strcpy(wfd.chan, names->chan );
    ucase(wfd.sta);
    ucase(wfd.chan);

    wfd.jdate = ldate;
    wfd.wfid = Wfid++;
    wfd.time = dtime;
    wfd.nsamp = Sample;
    wfd.foff = DOFFSET;
    wfd.endtime = dtime + (Sample-1)/wfd.samprate;
    strcpy(wfd.instype , "RefTek");
    strcpy(wfd.segtype, "o");
    if(buffer[DFORMAT] == 0 ) strcpy(wfd.datatype, "s2");
    else if(buffer[DFORMAT] == 1 ) strcpy(wfd.datatype, "s4");
    wfd.chanid = 0;
    strcpy(wfd.dir,"./");
    strcpy(wfd.dfile, names->dataf); 
    DOFFSET += Sample * Dformat;

/* Find current time  */
 
    dtime = std_now();
    wfd.lddate = dtime;

/*  Write wfdisc  file  */

    WFDISC_TRM(&wfd);
    if( fprintf(Fp_wfd,   WFDISC_WCS,  WFDISC_WVL(&wfd)) <= 0)   {
         return 0;
    }  

    return 1;
}
 

