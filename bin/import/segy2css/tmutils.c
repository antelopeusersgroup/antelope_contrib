/* @(#)tmutils.c	1.1 03/20/96  */
/************************************************************************
 *
 *    Time utilities some were written by staff from JSPC at Colorado
 *    some are locally developed.
 *
 ***********************************************************************/
#include <ctype.h>
#include <time.h>

/* Macros */
 
#define MIN(a,b)  ( (a) < (b) ? (a) : (b) )
 
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
 

static char *month_name[] =
{"Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"};

static int days_in_month[] = {31,28,31,30,31,30,31,31,30,31,30,31,31};

int dtsplit(dtime, year, day, hour, min, sec, msec)
double dtime;
int *year, *day, *hour, *min, *sec, *msec;
{
long  ltime;
int   imsc;
float fmsc;
struct tm *tm;

    ltime = (long) dtime;
    tm    = gmtime(&ltime);
    fmsc  = (float) ((dtime - (double) ltime)) * 1000.0;
    imsc  = (int) fmsc;
    if (fmsc - (float) imsc >= 0.5) imsc++;

    *year = 1900 + tm->tm_year;
    *day  = ++tm->tm_yday;
    *hour = tm->tm_hour;
    *min  = tm->tm_min;
    *sec  = tm->tm_sec;
    *msec = imsc;

}

 /* return todays date in as string YYYYDDDHHMMSS */
 
char *sdate(out)
  char ** out;
{
         struct tm *gmt,*gmtime();
         time_t crnt;
         char str[17];
         int i;


         memset(str, " ", 17);
         crnt = time(0);          /* get epoch time (in GMT) */
         gmt = gmtime(&crnt);
         sprintf(str,"%4d%03d%02d%02d%02d\0", gmt->tm_year+1900, gmt->tm_yday+1, gmt->tm_hour, gmt->tm_min, gmt->tm_sec);
         strcpy(*out, str);
  }
