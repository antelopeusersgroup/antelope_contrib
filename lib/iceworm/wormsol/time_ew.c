
/*
 *   THIS FILE IS UNDER RCS - DO NOT MODIFY UNLESS YOU HAVE
 *   CHECKED IT OUT USING THE COMMAND CHECKOUT.
 *
 *    $Id$
 *
 *    Revision history:
 *     $Log$
 *     Revision 1.3  2003/06/01 08:25:38  lindquis
 *     Upgrade Iceworm libraries to Earthworm6.2. Add some rudimentary man
 *     pages. Preparation for the rewritten ew2orb.
 *
 *     Revision 1.4  2001/05/10 21:39:46  davidk
 *     Added a little comment about high performance timer
 *     availability in solaris.
 *
 *     Revision 1.3  2001/01/23 16:46:53  dietz
 *     Corrected roundoff problem in datestr23 and datestr23_local
 *
 *     Revision 1.2  2000/09/14 19:25:12  lucky
 *     Added datestr23_local which returns time string in local time
 *
 *     Revision 1.1  2000/02/14 18:46:17  lucky
 *     Initial revision
 *
 *
 */

     /********************************************************
      *              time_ew.c   Solaris version             *
      *                                                      *
      *  This file contains earthworm multi-thread safe      *
      *  versions of time routines                           *
      *  Needs to be linked with -lposix4                    *
      ********************************************************/


/***************************************************
 * Note to developers:
 * Clock Resolution better than 10ms
 *
 * Starting with Solaris 2.6, this (better clock res.)
 * can be achieved with the following entry in /etc/system: 
 *       set hires_tick = 1
 *
 * This will set the system hz value to 1000. 
 *
 * In principle, you can also set "hz" directly, 
 * but that is not supported nor recommended: 
 *
 * Get 0.1 ms clock resolution/timer granularity    
 *       set hz = 10000
 *
 * Solaris 8 introduces the cyclic subsystem; this 
 * allows for timers of much better granularity 
 * without burdening the system with a high 
 * interrupt rate. High resolution timers are available 
 * to root only using timer_create(3rt) with a clock_id 
 * of CLOCK_HIGHRES. 
 * 
 * DavidK from http://www.science.uva.nl/pub/solaris/solaris2/Q7.5.html
 ***************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <time_ew.h>

/********************************************************
 *  gmtime_ew() converts time in seconds since 1970 to  *
 *  a time/date structure expressed as UTC (GMT)        *
 ********************************************************/
struct tm *gmtime_ew( const time_t *epochsec, struct tm *res )
{
    gmtime_r( epochsec, res );
    return( res );
}

/********************************************************
 *                      timegm_ew()                     *
 * Convert struct tm to time_t using GMT as time zone   *
 ********************************************************/
#define MAXENV 128         
char    envTZ[MAXENV];  /* Space where environment variable TZ will be */
                        /* stored after the first call to timegm_ew()  */
         
time_t timegm_ew( struct tm *stm )
{
   char  *tz;
   time_t tt;
   char   TZorig[MAXENV];

/* Save current TZ setting locally
 *********************************/
   tz = getenv("TZ");
   if( tz != (char *) NULL )
   {
      if( strlen(tz) > MAXENV-4 ) 
      {
         printf("timegm_ew: unable to store current TZ environment variable.\n");
         return( -1 );
      }
   }
   sprintf( TZorig, "TZ=%s", tz );
  
/* Change time zone to GMT; do conversion
 ****************************************/
   sprintf( envTZ, "TZ=GMT" );
   if( putenv( envTZ ) != 0 )  
   {
      printf("timegm_ew: putenv: unable to set TZ environment variable.\n" );
      return( -1 );
   }
   tt = mktime( stm ); 

/* Restore original TZ setting
 *****************************/
   sprintf( envTZ, "%s", TZorig );
   if( putenv( envTZ ) != 0 )  
   {
     printf("timegm_ew: putenv: unable to restore TZ environment variable.\n" );
   }
   tzset();

   return( tt );
}

/********************************************************
 *  localtime_ew() converts time in seconds since 1970  *
 *  to a time/date structure expressed as local time    *
 *  (using time zone and daylight savings corrections)  *
 ********************************************************/
struct tm *localtime_ew( const time_t *epochsec, struct tm *res )
{
    localtime_r( epochsec, res );
    return( res );
}

/********************************************************
 *  ctime_ew() converts time in seconds since 1970 to   *
 *  a 26 character string expressed as local time       *
 *  (using time zone and daylight savings corrections)  *
 *   Example:  "Fri Sep 13 00:00:00 1986\n\0"           *
 ********************************************************/
char *ctime_ew( const time_t *epochsec, char *buf, int buflen )
{
    char *rc;

    ctime_r( epochsec, buf, buflen );

    if (rc == (char *) NULL)  return( rc );
    return( buf );
}

/********************************************************
 *  asctime_ew() converts time/date structure to        *
 *  a 26 character string                               *
 *   Example:  "Fri Sep 13 00:00:00 1986\n\0"           *
 ********************************************************/
char *asctime_ew( const struct tm *tm, char *buf, int buflen )
{
    char *rc;

    rc = asctime_r( tm, buf, buflen );

    if (rc == (char *) NULL)  return( rc );
    return( buf );
}

/*******************************************************
 * hrtime_ew() returns a high-resolution system clock  *
 *             time as a double                        *
 *******************************************************/
double hrtime_ew( double *tnow )
{
    struct timespec t;

    if( clock_gettime( CLOCK_REALTIME, &t ) == 0 ) {
       *tnow = (double) t.tv_sec + (double)t.tv_nsec*0.000000001;
    }
    else {   
       *tnow = 0;
    }
    return( *tnow );
}


/**********************************************************
 * Converts time (double, seconds since 1970:01:01) to    *
 * a 22-character, null-terminated string in the form of  *
 *            yyyy/mm/dd hh:mm:ss.ss                      *
 * Time is displayed in UTC                               *
 * Target buffer must be 23-chars long to have room for   *
 * null-character                                         *
 **********************************************************/ 
char *datestr23( double t, char *pbuf, int len )
{  
   time_t    tt;       /* time as time_t                  */
   struct tm stm;      /* time as struct tm               */
   int       t_hsec;   /* hundredths-seconds part of time */

/* Make sure target is big enough
 ********************************/
   if( len < DATESTR23 ) return( (char *)NULL );

/* Convert double time to other formats 
 **************************************/
   t += 0.005;  /* prepare to round to the nearest 100th */
   tt     = (time_t) t;
   t_hsec = (int)( (t - tt) * 100. );
   gmtime_ew( &tt, &stm );

/* Build character string
 ************************/
   sprintf( pbuf, 
           "%04d/%02d/%02d %02d:%02d:%02d.%02d",
            stm.tm_year+1900,
            stm.tm_mon+1,
            stm.tm_mday,
            stm.tm_hour,
            stm.tm_min,
            stm.tm_sec,            
            t_hsec );
 
   return( pbuf );
}



/**********************************************************
 * Converts time (double, seconds since 1970:01:01) to    *
 * a 22-character, null-terminated string in the form of  *
 *            yyyy/mm/dd hh:mm:ss.ss                      *
 * Time is displayed in LOCAL time                        *
 * Target buffer must be 23-chars long to have room for   *
 * null-character                                         *
 **********************************************************/ 
char *datestr23_local( double t, char *pbuf, int len )
{  
   time_t    tt;       /* time as time_t                  */
   struct tm stm;      /* time as struct tm               */
   int       t_hsec;   /* hundredths-seconds part of time */

/* Make sure target is big enough
 ********************************/
   if( len < DATESTR23 ) return( (char *)NULL );

/* Convert double time to other formats 
 **************************************/
   t += 0.005;  /* prepare to round to the nearest 100th */
   tt     = (time_t) t;
   t_hsec = (int)( (t - tt) * 100. );
   localtime_ew( &tt, &stm );

/* Build character string
 ************************/
   sprintf( pbuf, 
           "%04d/%02d/%02d %02d:%02d:%02d.%02d",
            stm.tm_year+1900,
            stm.tm_mon+1,
            stm.tm_mday,
            stm.tm_hour,
            stm.tm_min,
            stm.tm_sec,            
            t_hsec );
 
   return( pbuf );
}
