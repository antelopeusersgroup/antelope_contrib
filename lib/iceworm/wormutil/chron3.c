#include <stdio.h>
#include <time.h>
#include <sys/types.h>
#include "chron3.h"
/*
   chron3.c : Time/date conversion routines.

   91May07 CEJ Version 1.0 - chron.c, chron.h

   Modified by W. Kohler, March 8, 1993.
   Source file name changed from chron.c to chron2.c.
   Include file name changed from chron.h to chron2.h.
   Routine datime added to calculate gregorian date and time from
   julian seconds.  Seconds added to structure Greg in file chron2.h.

   Modified by L. Dietz,  March 30, 1995.
   Source file name changed from chron2.c to chron3.c.
   Include file name changed from chron2.h to chron3.h.
   Routines date15 and julsec15 added to convert between time in
   julian seconds and character strings.
   Added a define statement to set the century (#define CENTURY 1900)

   Modified by L. Dietz, January 30, 1995.
   Added routine epochsec15 to convert from time given in a
   15-character string and seconds since 1970-01-01 00:00:00.0 GMT.

   Modified by L. Dietz, October, 1998.
   Changed make the library Y2K-compliant. 
   + Removed the CENTURY definition from chron3.h! 
   + Removed all functions that dealt with 2-digit-year date strings 
     and replaced them with 4-digit-year counterparts.
        date15     -> date17
        date18     -> date20
        julsec15   -> julsec17
        epochsec15 -> epochsec17
        timegm     -> timegm (fixed to call epochsec17)

   Modified by L. Dietz, November, 1998.
   Changed to make function MT-safe.  
   Eliminated the file-global "struct Greg G" workspace and had each function 
   declare its own "struct Greg" variable, if needed.  Changed function
   arguments for grg(), gregor(), and datime() to include a pointer to 
   struct Greg for returning information to calling function.

*/

/*********************C O P Y R I G H T   N O T I C E ***********************/
/* Copyright 1991 by Carl Johnson.  All rights are reserved. Permission     */
/* is hereby granted for the use of this product for nonprofit, commercial, */
/* or noncommercial publications that contain appropriate acknowledgement   */
/* of the author. Modification of this code is permitted as long as this    */
/* notice is included in each resulting source module.                      */
/****************************************************************************/

int mo[] = {   0,  31,  59,  90, 120, 151, 181, 212, 243, 273, 304, 334,
               0,  31,  60,  91, 121, 152, 182, 213, 244, 274, 305, 335};
char *cmo[] = {"Jan", "Feb", "Mar", "Apr", "May", "Jun",
               "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" };

/*
 * Calculate julian minute from gregorian date and time.
 */
long julmin( struct Greg *pg )
{
        return(1440L * (julian(pg) - 2305448L) + 60L * pg->hour + pg->minute);
}

/*
 * Calculate gregorian date and time from julian minutes.
 */
struct Greg *grg( long min, struct Greg *pg )
{
        long j;
        long m;

        j = min/1440L;
        m = min-1440L*j;
        j += 2305448L;
        gregor(j,pg);
        pg->hour = m/60;
        pg->minute = m - 60 * pg->hour;
        return(pg);
}

/*
 * julian : Calculate julian date from gregorian date.
 */
long julian( struct Greg *pg )
{
        long jul;
        int leap;
        int year;
        int n;

        jul = 0;
        year = pg->year;
        if(year < 1) goto x110;
        year--;
        jul = 365;

/* four hundred year rule */
        n = year / 400;
        jul += n * 146097L;
        year -= n * 400;

/* hundred year rule */
        n = year / 100;
        jul += n * 36524L;
        year -= n * 100;

/* four year rule */
        n = year / 4;
        jul += n * 1461L;
        year -= n * 4;

/* one year rule */
        jul += year * 365L;

/* Handle days in current year */
x110:
        leap = 0;
        if(pg->year % 4   == 0) leap = 12;
        if(pg->year % 100 == 0) leap = 0;
        if(pg->year % 400 == 0) leap = 12;
        jul += mo[pg->month + leap - 1] + pg->day + 1721060L;
        return(jul);
}

/*
 * gregor : Calculate gregorian date from julian date.
 */
struct Greg *gregor( long min, struct Greg *pg )
{
        long test;
        long check;
        int leap;
        int left;
        int imo;

        pg->year = (min - 1721061L) / 365L;
        pg->month = 1;
        pg->day = 1;
        test = julian(pg);
        if(test <= min) goto x110;

x20:
        pg->year--;
        test = julian(pg);
        if(test > min) goto x20;
        goto x210;

x105:
        pg->year++;
        test = julian(pg);

x110:
        check = test - min - 366L;
        if(check < 0) goto x210;
        if(check > 0) goto x105;

        if(pg->year % 400 == 0) goto x210;
        if(pg->year % 100 == 0) goto x105;
        if(pg->year %   4 == 0) goto x210;
        goto x105;

x210:
        left = min - test;
        leap = 0;
        if(pg->year %   4 == 0) leap = 12;
        if(pg->year % 100 == 0) leap = 0;
        if(pg->year % 400 == 0) leap = 12;
        for(imo=1; imo<12; imo++) {
                if(mo[imo+leap] <= left)
                        continue;
                pg->month = imo;
                pg->day = left - mo[imo+leap-1] + 1;
                return(pg);
        }
        pg->month = 12;
        pg->day = left - mo[11+leap] + 1;
        return(pg);
}

/*
 * date20 : Create 20 char date string in the form 1988Jan23 1234 12.21
 *          from the julian seconds.  Remember to leave space for the
 *          string termination (NULL).
 *          Replaces non-Y2K-compliant date18() function
 *          Added to chron3.c 10/28/98 by LDD.
 */
void date20( double secs, char *c20)
{
        struct Greg g;
        long minute;
        double sex;
        int hrmn;

        minute = (long) (secs / 60.0);
        sex = secs - 60.0 * minute;
        grg(minute, &g);
        hrmn = 100 * g.hour + g.minute;
        sprintf(c20, "%04d%3s%2d %4d%6.2f",
                g.year, cmo[g.month-1], g.day, hrmn, sex);
}

/*
 * tnow : Returns current system time for time stamping
 */
double tnow( void )
{
        struct Greg g;
/*      struct timeb q; */
        time_t tsecs;
        double secs;

        g.year   = 1970;
        g.month  = 1;
        g.day    = 1;
        g.hour   = 0;
        g.minute = 0;
/* original code by Carl; ftime() not supported on Sparc C compiler 3.0.1 */
/*      ftime(&q);                                              */
/*      secs = 60.0 * julmin(&g) + q.time +  0.001 * q.millitm; */
        time(&tsecs);                                   /*950501:ldd.*/
        secs = 60.0 * julmin(&g) + (double) tsecs;      /*950501:ldd.*/
        return secs;
}

/*
 * Calculate gregorian date and time from julian seconds.
 */
struct Greg *datime( double secs, struct Greg *pg )
{
        long j, m, min;

        min = (long) (secs / 60.0);
        j = min/1440L;
        m = min-1440L*j;
        j += 2305448L;
        gregor(j,pg);
        pg->hour = m/60;
        pg->minute = m - 60 * pg->hour;
        pg->second = (float) (secs - 60.0 * min);
        return(pg);
}

/*
 * date17 : Build a 17 char date string in the form 19880123123412.21
 *          from the julian seconds.  Remember to leave space for the
 *          string termination (NULL).
 *          Replaces the non-Y2K-compliant date15() function.
 *          Added to chron3.c on 10/28/98 by LDD
 */
void date17( double secs, char *c17 )
{
        struct Greg g;
        long minute;
        double sex;

        minute = (long) (secs / 60.0);
        sex = secs - 60.0 * (double) minute;
        grg(minute,&g);
        sprintf(c17, "%04d%02d%02d%02d%02d%05.2f\0",
                g.year, g.month, g.day, g.hour, g.minute, sex);
}

/*
 * julsec17 : Calculate time in julian seconds from a character string
 *            of the form 19880123123412.21
 *            Replaces the non-Y2K-compliant julsec15() function.
 *            Added to chron3.c on 10/28/98 by LDD
 */
double julsec17( char *c17 )
{
        struct Greg  g;
        double       jsecs;
        int          narg, i;
        int          isec, hsec;

/*** Make sure there are no blanks in the time part of the pick ***/
        for(i=0; i<17; i++)
        {
                if( c17[i] == ' ' )  c17[i] = '0';
        }

/***  Read character string  ***/
        narg = sscanf( c17, "%4d%2d%2d%2d%2d%2d.%2d",
                        &g.year, &g.month, &g.day,
                        &g.hour, &g.minute, &isec, &hsec);

        if ( narg < 7 )  return( 0.0 );


/***  Calculate julian seconds ***/
        jsecs   = 60.0 * (double) julmin(&g) +
                         (double) isec +
                         (double) hsec / 100.0;

        return( jsecs );
}

/*
 * epochsec17 :  Convert time in a character string form of
 *               ccyymmddhhmmss.ff (19880231010155.23) to
 *               seconds since 1970-01-01 00:00:00.0
 *               Replaces the non-Y2K-compliant epochsec15() function.
 *               Added to chron3.c on 10/28/98 by LDD
 */
int epochsec17( double *sec, char *tstr )
{
   double jsec;
   double sec1970 = 11676096000.00;  /* # seconds between Carl Johnson's     */
                                     /* time 0 and 1970-01-01 00:00:00.0 GMT */

   jsec = julsec17( tstr );
   if( jsec==0.0 )
   {
      *sec=0.0;
      return ( -1 );
   }

   *sec = jsec-sec1970;
   return ( 0 );
}

/*
 * timegm :  Convert time as a struct tm to seconds since 1970-01-01 00:00:00.0
 *           This function is equivalent to timegm() in SunOS 4.x.
 *           Added to chron3.c on 2/27/98 by WMK
 *           Modified to be Y2K compliant 10/28/98 by LDD
 */
time_t timegm( struct tm *tm )
{
   char   tstr[18];
   double dsec;

   sprintf( tstr, "%04d%02d%02d%02d%02d%02d.00",
            tm->tm_year + 1900,
            tm->tm_mon + 1,
            tm->tm_mday,
            tm->tm_hour,
            tm->tm_min,
            tm->tm_sec );

   epochsec17( &dsec, tstr );
   return( (time_t)dsec );
}

