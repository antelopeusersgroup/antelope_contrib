
     /********************************************************
      *              time_ew.c   Solaris version             *
      *                                                      *
      *  This file contains earthworm multi-thread safe      *
      *  versions of time routines                           *
      *  Needs to be linked with -lposix4                    *
      ********************************************************/

#include <time.h>

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
