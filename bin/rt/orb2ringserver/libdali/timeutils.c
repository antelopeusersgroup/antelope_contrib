/***********************************************************************//**
 * @file timeutils.c
 *
 * General time utility routines and routines for dealing with libdali
 * time values of dltime_t type.
 *
 * @author Chad Trabant, IRIS Data Management Center
 *
 * modified: 2011.003
 ***************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "libdali.h"

static dltime_t dl_time2dltime_int (int year, int day, int hour,
				    int min, int sec, int usec);

static struct tm *dl_gmtime_r (int64_t *timep, struct tm *result);


/***********************************************************************//**
 * @brief Compute the month and day-of-month from day-of-year
 *
 * Compute the month and day-of-month from a year and day-of-year.
 *
 * Year is expected to be in the range 1900-2100, jday is expected to
 * be in the range 1-366, month will be in the range 1-12 and mday
 * will be in the range 1-31.
 *
 * @param year Year (1900 - 2100)
 * @param jday Day-of-year, "Julian" day (1 - 366)
 * @param month Returned month (1 - 12)
 * @param mday Returned day-of-month (1 - 31)
 *
 * @return 0 on success and -1 on error.
 ***************************************************************************/
int
dl_doy2md (int year, int jday, int *month, int *mday)
{
  int idx;
  int leap;
  int days[] = {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
  
  /* Sanity check for the supplied year */
  if ( year < 1900 || year > 2100 )
    {
      dl_log (2, 0, "dl_doy2md(): year (%d) is out of range\n", year);
      return -1;
    }
  
  /* Test for leap year */
  leap = ( ((year%4 == 0) && (year%100 != 0)) || (year%400 == 0) ) ? 1 : 0;

  /* Add a day to February if leap year */
  if ( leap )
    days[1]++;

  if (jday > 365+leap || jday <= 0)
    {
      dl_log (2, 0, "dl_doy2md(): day-of-year (%d) is out of range\n", jday);
      return -1;
    }
    
  for ( idx=0; idx < 12; idx++ )
    {
      jday -= days[idx];

      if ( jday <= 0 )
	{
	  *month = idx + 1;
	  *mday = days[idx] + jday;
	  break;
	}
    }

  return 0;
}  /* End of dl_doy2md() */


/***********************************************************************//**
 * @brief Compute the day-of-year from year, month and day-of-month
 *
 * Compute the day-of-year from a year, month and day-of-month.
 *
 * Year is expected to be in the range 1900-2100, month is expected to
 * be in the range 1-12, mday is expected to be in the range 1-31 and
 * jday will be in the range 1-366.
 *
 * @param year Year (1900 - 2100)
 * @param month Month (1 - 12)
 * @param mday Day-of-month (1 - 31)
 * @param jday Returned day-of-year, "Julian" day (1 - 366)
 *
 * @return 0 on success and -1 on error.
 ***************************************************************************/
int
dl_md2doy (int year, int month, int mday, int *jday)
{
  int idx;
  int leap;
  int days[] = {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
  
  /* Sanity check for the supplied parameters */
  if ( year < 1900 || year > 2100 )
    {
      dl_log (2, 0, "dl_md2doy(): year (%d) is out of range\n", year);
      return -1;
    }
  if ( month < 1 || month > 12 )
    {
      dl_log (2, 0, "dl_md2doy(): month (%d) is out of range\n", month);
      return -1;
    }
  if ( mday < 1 || mday > 31 )
    {
      dl_log (2, 0, "dl_md2doy(): day-of-month (%d) is out of range\n", mday);
      return -1;
    }
  
  /* Test for leap year */
  leap = ( ((year%4 == 0) && (year%100 != 0)) || (year%400 == 0) ) ? 1 : 0;
  
  /* Add a day to February if leap year */
  if ( leap )
    days[1]++;
  
  /* Check that the day-of-month jives with specified month */
  if ( mday > days[month-1] )
    {
      dl_log (2, 0, "dl_md2doy(): day-of-month (%d) is out of range for month %d\n",
	       mday, month);
      return -1;
    }

  *jday = 0;
  month--;
  
  for ( idx=0; idx < 12; idx++ )
    {
      if ( idx == month )
	{
	  *jday += mday;
	  break;
	}
      
      *jday += days[idx];
    }
  
  return 0;
}  /* End of dl_md2doy() */


/***********************************************************************//**
 * @brief Generate an ISO time string from a dltime_t
 *
 * Build a time string in ISO recommended format from a high precision
 * epoch time, dltime_t, value.
 *
 * The provided isostimestr must have enough room for the resulting time
 * string of 27 characters, i.e. '2001-07-29T12:38:00.000000' + NULL.
 *
 * The 'subseconds' flag controls whenther the sub second portion of the
 * time is included or not.
 *
 * @param dltime The dltime_t time value
 * @param isotimestr Returned ISO time string, must have room for 27 characters
 * @param subseconds Flag to control the inclusion of subseconds
 *
 * @return A pointer to the resulting string or NULL on error.
 ***************************************************************************/
char *
dl_dltime2isotimestr (dltime_t dltime, char *isotimestr, int8_t subseconds)
{
  struct tm tms;
  int64_t isec;
  int ifract;
  int ret;

  if ( isotimestr == NULL )
    return NULL;

  /* Reduce to Unix/POSIX epoch time and fractional seconds */
  isec = DL_DLTIME2EPOCH(dltime);
  ifract = (dltime_t) dltime - (isec * DLTMODULUS);
  
  /* Adjust for negative epoch times */
  if ( dltime < 0 && ifract != 0 )
    {
      isec -= 1;
      ifract = DLTMODULUS - (-ifract);
    }

  if ( ! (dl_gmtime_r (&isec, &tms)) )
    return NULL;
  
  if ( subseconds )
    /* Assuming ifract has at least microsecond precision */
    ret = snprintf (isotimestr, 27, "%4d-%02d-%02dT%02d:%02d:%02d.%06d",
                    tms.tm_year + 1900, tms.tm_mon + 1, tms.tm_mday,
                    tms.tm_hour, tms.tm_min, tms.tm_sec, ifract);
  else
    ret = snprintf (isotimestr, 20, "%4d-%02d-%02dT%02d:%02d:%02d",
                    tms.tm_year + 1900, tms.tm_mon + 1, tms.tm_mday,
                    tms.tm_hour, tms.tm_min, tms.tm_sec);

  if ( ret != 26 && ret != 19 )
    return NULL;
  else
    return isotimestr;
}  /* End of dl_dltime2isotimestr() */


/***********************************************************************//**
 * @brief Generate an time string in month-day format from a dltime_t
 *
 * Build a time string in month-day format from a high precision
 * epoch time.
 *
 * The provided mdtimestr must have enough room for the resulting time
 * string of 27 characters, i.e. '2001-07-29 12:38:00.000000' + NULL.
 *
 * The 'subseconds' flag controls whenther the sub second portion of the
 * time is included or not.
 *
 * @param dltime The dltime_t time value
 * @param mdtimestr Returned time string, must have room for 27 characters
 * @param subseconds Flag to control the inclusion of subseconds
 *
 * @return A pointer to the resulting string or NULL on error.
 ***************************************************************************/
char *
dl_dltime2mdtimestr (dltime_t dltime, char *mdtimestr, int8_t subseconds)
{
  struct tm tms;
  int64_t isec;
  int ifract;
  int ret;

  if ( mdtimestr == NULL )
    return NULL;

  /* Reduce to Unix/POSIX epoch time and fractional seconds */
  isec = DL_DLTIME2EPOCH(dltime);
  ifract = (dltime_t) dltime - (isec * DLTMODULUS);
  
  /* Adjust for negative epoch times */
  if ( dltime < 0 && ifract != 0 )
    {
      isec -= 1;
      ifract = DLTMODULUS - (-ifract);
    }
  
  if ( ! (dl_gmtime_r (&isec, &tms)) )
    return NULL;
  
  if ( subseconds )
    /* Assuming ifract has at least microsecond precision */
    ret = snprintf (mdtimestr, 27, "%4d-%02d-%02d %02d:%02d:%02d.%06d",
                    tms.tm_year + 1900, tms.tm_mon + 1, tms.tm_mday,
                    tms.tm_hour, tms.tm_min, tms.tm_sec, ifract);
  else
    ret = snprintf (mdtimestr, 20, "%4d-%02d-%02d %02d:%02d:%02d",
                    tms.tm_year + 1900, tms.tm_mon + 1, tms.tm_mday,
                    tms.tm_hour, tms.tm_min, tms.tm_sec);
  
  if ( ret != 26 && ret != 19 )
    return NULL;
  else
    return mdtimestr;
}  /* End of dl_dltime2mdtimestr() */


/***********************************************************************//**
 * @brief Generate an time string in SEED format from a dltime_t
 *
 * Build a SEED (day-of-year) time string from a high precision epoch time.
 *
 * The provided seedtimestr must have enough room for the resulting time
 * string of 25 characters, i.e. '2001,195,12:38:00.000000\n'.
 *
 * The 'subseconds' flag controls whenther the sub second portion of the
 * time is included or not.
 *
 * @param dltime The dltime_t time value
 * @param seedtimestr Returned time string, must have room for 25 characters
 * @param subseconds Flag to control the inclusion of subseconds
 *
 * @return A pointer to the resulting string or NULL on error.
 ***************************************************************************/
char *
dl_dltime2seedtimestr (dltime_t dltime, char *seedtimestr, int8_t subseconds)
{
  struct tm tms;
  int64_t isec;
  int ifract;
  int ret;
  
  if ( seedtimestr == NULL )
    return NULL;
  
  /* Reduce to Unix/POSIX epoch time and fractional seconds */
  isec = DL_DLTIME2EPOCH(dltime);
  ifract = (dltime_t) dltime - (isec * DLTMODULUS);
  
  /* Adjust for negative epoch times */
  if ( dltime < 0 && ifract != 0 )
    {
      isec -= 1;
      ifract = DLTMODULUS - (-ifract);
    }

  if ( ! (dl_gmtime_r (&isec, &tms)) )
    return NULL;
  
  if ( subseconds )
    /* Assuming ifract has at least microsecond precision */
    ret = snprintf (seedtimestr, 25, "%4d,%03d,%02d:%02d:%02d.%06d",
		    tms.tm_year + 1900, tms.tm_yday + 1,
		    tms.tm_hour, tms.tm_min, tms.tm_sec, ifract);
  else
    /* Assuming ifract has at least microsecond precision */
    ret = snprintf (seedtimestr, 18, "%4d,%03d,%02d:%02d:%02d",
                    tms.tm_year + 1900, tms.tm_yday + 1,
                    tms.tm_hour, tms.tm_min, tms.tm_sec);
  
  if ( ret != 24 && ret != 17 )
    return NULL;
  else
    return seedtimestr;
}  /* End of dl_dltime2seedtimestr() */


/***************************************************************************
 * Convert specified time values to a dltime_t value, internal-only
 *
 * Convert specified time values to a high precision epoch time
 * (1/DLTMODULUS second ticks from the epoch).  This is an internal
 * routine which does no range checking, it is assumed that checking
 * the range for each value has already been done.  The algorithm used
 * is a specific version of a generalized function in GNU glibc.
 *
 * Returns dltime_t time value on success and DLTERROR on error.
 ***************************************************************************/
static dltime_t
dl_time2dltime_int (int year, int day, int hour, int min, int sec, int usec)
{
  dltime_t dltime;
  int32_t shortyear;
  int32_t a4, a100, a400;
  int32_t intervening_leap_days;
  int32_t days;
  
  shortyear = year - 1900;
  
  a4 = (shortyear >> 2) + 475 - ! (shortyear & 3);
  a100 = a4 / 25 - (a4 % 25 < 0);
  a400 = a100 >> 2;
  intervening_leap_days = (a4 - 492) - (a100 - 19) + (a400 - 4);
  
  days = (365 * (shortyear - 70) + intervening_leap_days + (day - 1));
  
  dltime = (dltime_t ) (60 * (60 * (24 * days + hour) + min) + sec) * DLTMODULUS
    + (dltime_t) usec * (1000000 / DLTMODULUS);
  
  return dltime;
}  /* End of dl_time2dltime_int() */


/***********************************************************************//**
 * @brief Convert specified time values to a dltime_t value
 *
 * Convert specified time values to a high precision epoch time, a
 * dltime_t value.  The routine will range check all the input
 * parameters.
 *
 * @param year Year (1900 - 2100)
 * @param day  Day  (1 - 366)
 * @param hour Hour (0 - 23)
 * @param min  Minute (0 - 59)
 * @param sec  Second (0 - 60)
 * @param usec Microsecond (0 - 999999)
 *
 * @return dltime_t time value on success and DLTERROR on error.
 ***************************************************************************/
dltime_t
dl_time2dltime (int year, int day, int hour, int min, int sec, int usec)
{
  if ( year < 1900 || year > 2100 )
    {
      dl_log (2, 0, "dl_time2dltime(): Error with year value: %d\n", year);
      return DLTERROR;
    }
  
  if ( day < 1 || day > 366 )
    {
      dl_log (2, 0, "dl_time2dltime(): Error with day value: %d\n", day);
      return DLTERROR;
    }
  
  if ( hour < 0 || hour > 23 )
    {
      dl_log (2, 0, "dl_time2dltime(): Error with hour value: %d\n", hour);
      return DLTERROR;
    }
  
  if ( min < 0 || min > 59 )
    {
      dl_log (2, 0, "dl_time2dltime(): Error with minute value: %d\n", min);
      return DLTERROR;
    }
  
  if ( sec < 0 || sec > 60 )
    {
      dl_log (2, 0, "dl_time2dltime(): Error with second value: %d\n", sec);
      return DLTERROR;
    }
  
  if ( usec < 0 || usec > 999999 )
    {
      dl_log (2, 0, "dl_time2dltime(): Error with microsecond value: %d\n", usec);
      return DLTERROR;
    }
  
  return dl_time2dltime_int (year, day, hour, min, sec, usec);
}  /* End of dl_time2dltime() */


/***********************************************************************//**
 * @brief Convert a SEED time string to a dltime_t value
 * 
 * Convert a SEED time string to a high precision epoch time.  SEED
 * time format is "YYYY[,DDD,HH,MM,SS.FFFFFF]", the delimiter can be a
 * comma [,], colon [:] or period [.] except for the fractional
 * seconds which must start with a period [.].
 *
 * The time string can be "short" in which case the omitted values are
 * assumed to be zero (with the exception of DDD which is assumed to
 * be 1): "YYYY,DDD,HH" assumes MM, SS and FFFF are 0.  The year is
 * required, otherwise there wouldn't be much for a date.
 *
 * Ranges are checked for each time value.
 *
 * @param seedtimestr SEED time string to convert
 *
 * @return dltime_t time value on success and DLTERROR on error.
 ***************************************************************************/
dltime_t
dl_seedtimestr2dltime (char *seedtimestr)
{
  int fields;
  int year = 0;
  int day  = 1;
  int hour = 0;
  int min  = 0;
  int sec  = 0;
  float fusec = 0.0;
  int usec = 0;
  
  fields = sscanf (seedtimestr, "%d%*[,:.]%d%*[,:.]%d%*[,:.]%d%*[,:.]%d%f",
		   &year, &day, &hour, &min, &sec, &fusec);
  
  /* Convert fractional seconds to microseconds */
  if ( fusec != 0.0 )
    {
      usec = (int) (fusec * 1000000.0 + 0.5);
    }
  
  if ( fields < 1 )
    {
      dl_log (2, 0, "dl_seedtimestr2dltime(): Error converting time string: %s\n", seedtimestr);
      return DLTERROR;
    }
  
  if ( year < 1900 || year > 3000 )
    {
      dl_log (2, 0, "dl_seedtimestr2dltime(): Error with year value: %d\n", year);
      return DLTERROR;
    }

  if ( day < 1 || day > 366 )
    {
      dl_log (2, 0, "dl_seedtimestr2dltime(): Error with day value: %d\n", day);
      return DLTERROR;
    }
  
  if ( hour < 0 || hour > 23 )
    {
      dl_log (2, 0, "dl_seedtimestr2dltime(): Error with hour value: %d\n", hour);
      return DLTERROR;
    }
  
  if ( min < 0 || min > 59 )
    {
      dl_log (2, 0, "dl_seedtimestr2dltime(): Error with minute value: %d\n", min);
      return DLTERROR;
    }
  
  if ( sec < 0 || sec > 60 )
    {
      dl_log (2, 0, "dl_seedtimestr2dltime(): Error with second value: %d\n", sec);
      return DLTERROR;
    }
  
  if ( usec < 0 || usec > 999999 )
    {
      dl_log (2, 0, "dl_seedtimestr2dltime(): Error with fractional second value: %d\n", usec);
      return DLTERROR;
    }
  
  return dl_time2dltime_int (year, day, hour, min, sec, usec);
}  /* End of dl_seedtimestr2dltime() */


/***********************************************************************//**
 * @brief Convert a time string to a dltime_t value
 * 
 * Convert a generic time string to a high precision epoch time.
 * SEED time format is "YYYY[/MM/DD HH:MM:SS.FFFF]", the delimiter can
 * be a dash [-], slash [/], colon [:], or period [.] and between the
 * date and time a 'T' or a space may be used.  The fracttional
 * seconds must begin with a period [.].
 *
 * The time string can be "short" in which case the omitted values are
 * assumed to be zero (with the exception of month and day which are
 * assumed to be 1): "YYYY/MM/DD" assumes HH, MM, SS and FFFF are 0.
 * The year is required, otherwise there wouldn't be much for a date.
 *
 * Ranges are checked for each time value.
 *
 * @param timestr Time string to convert
 *
 * @return dltime_t time value on success and DLTERROR on error.
 ***************************************************************************/
dltime_t
dl_timestr2dltime (char *timestr)
{
  int fields;
  int year = 0;
  int mon  = 1;
  int mday = 1;
  int day  = 1;
  int hour = 0;
  int min  = 0;
  int sec  = 0;
  float fusec = 0.0;
  int usec = 0;
    
  fields = sscanf (timestr, "%d%*[-/:.]%d%*[-/:.]%d%*[-/:.T ]%d%*[-/:.]%d%*[- /:.]%d%f",
		   &year, &mon, &mday, &hour, &min, &sec, &fusec);
  
  /* Convert fractional seconds to microseconds */
  if ( fusec != 0.0 )
    {
      usec = (int) (fusec * 1000000.0 + 0.5);
    }

  if ( fields < 1 )
    {
      dl_log (2, 0, "dl_timestr2dltime(): Error converting time string: %s\n", timestr);
      return DLTERROR;
    }
  
  if ( year < 1900 || year > 3000 )
    {
      dl_log (2, 0, "dl_timestr2dltime(): Error with year value: %d\n", year);
      return DLTERROR;
    }
  
  if ( mon < 1 || mon > 12 )
    {
      dl_log (2, 0, "dl_timestr2dltime(): Error with month value: %d\n", mon);
      return DLTERROR;
    }

  if ( mday < 1 || mday > 31 )
    {
      dl_log (2, 0, "dl_timestr2dltime(): Error with day value: %d\n", mday);
      return DLTERROR;
    }

  /* Convert month and day-of-month to day-of-year */
  if ( dl_md2doy (year, mon, mday, &day) )
    {
      return DLTERROR;
    }
  
  if ( hour < 0 || hour > 23 )
    {
      dl_log (2, 0, "dl_timestr2dltime(): Error with hour value: %d\n", hour);
      return DLTERROR;
    }
  
  if ( min < 0 || min > 59 )
    {
      dl_log (2, 0, "dl_timestr2dltime(): Error with minute value: %d\n", min);
      return DLTERROR;
    }
  
  if ( sec < 0 || sec > 60 )
    {
      dl_log (2, 0, "dl_timestr2dltime(): Error with second value: %d\n", sec);
      return DLTERROR;
    }
  
  if ( usec < 0 || usec > 999999 )
    {
      dl_log (2, 0, "dl_timestr2dltime(): Error with fractional second value: %d\n", usec);
      return DLTERROR;
    }
  
  return dl_time2dltime_int (year, day, hour, min, sec, usec);
}  /* End of dl_timestr2dltime() */


/***************************************************************************
 * dl_gmtime_r:
 *
 * An internal version of gmtime_r() that is 64-bit compliant and
 * works with years beyond 2038.
 *
 * The original was called pivotal_gmtime_r() by Paul Sheer, all
 * required copyright and other hoohas are below.  Modifications were
 * made to integrate the original to this code base, avoid name
 * collisions and formatting so I could read it.
 * 
 * Returns a pointer to the populated tm struct on success and NULL on error.
 ***************************************************************************/

/* pivotal_gmtime_r - a replacement for gmtime/localtime/mktime
                      that works around the 2038 bug on 32-bit
                      systems. (Version 4)

   Copyright (C) 2009  Paul Sheer

   Redistribution and use in source form, with or without modification,
   is permitted provided that the above copyright notice, this list of
   conditions, the following disclaimer, and the following char array
   are retained.

   Redistribution and use in binary form must reproduce an
   acknowledgment: 'With software provided by http://2038bug.com/' in
   the documentation and/or other materials provided with the
   distribution, and wherever such acknowledgments are usually
   accessible in Your program.

   This software is provided "AS IS" and WITHOUT WARRANTY, either
   express or implied, including, without limitation, the warranties of
   NON-INFRINGEMENT, MERCHANTABILITY or FITNESS FOR A PARTICULAR
   PURPOSE. THE ENTIRE RISK AS TO THE QUALITY OF THIS SOFTWARE IS WITH
   YOU. Under no circumstances and under no legal theory, whether in
   tort (including negligence), contract, or otherwise, shall the
   copyright owners be liable for any direct, indirect, special,
   incidental, or consequential damages of any character arising as a
   result of the use of this software including, without limitation,
   damages for loss of goodwill, work stoppage, computer failure or
   malfunction, or any and all other commercial damages or losses. This
   limitation of liability shall not apply to liability for death or
   personal injury resulting from copyright owners' negligence to the
   extent applicable law prohibits such limitation. Some jurisdictions
   do not allow the exclusion or limitation of incidental or
   consequential damages, so this exclusion and limitation may not apply
   to You.

*/

const char pivotal_gmtime_r_stamp_ld[] =
  "pivotal_gmtime_r. Copyright (C) 2009  Paul Sheer. Terms and "
  "conditions apply. Visit http://2038bug.com/ for more info.";

static const int tm_days[4][13] = {
  {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31},
  {31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31},
  {0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365},
  {0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366},
};

#define TM_LEAP_CHECK(n) ((!(((n) + 1900) % 400) || (!(((n) + 1900) % 4) && (((n) + 1900) % 100))) != 0)
#define TM_WRAP(a,b,m)   ((a) = ((a) <  0  ) ? ((b)--, (a) + (m)) : (a))

static struct tm *
dl_gmtime_r (int64_t *timep, struct tm *result)
{
  int v_tm_sec, v_tm_min, v_tm_hour, v_tm_mon, v_tm_wday, v_tm_tday;
  int leap;
  long m;
  int64_t tv;
  
  if ( ! timep || ! result )
    return NULL;
  
  tv = *timep;
  
  v_tm_sec = ((int64_t) tv % (int64_t) 60);
  tv /= 60;
  v_tm_min = ((int64_t) tv % (int64_t) 60);
  tv /= 60;
  v_tm_hour = ((int64_t) tv % (int64_t) 24);
  tv /= 24;
  v_tm_tday = tv;
  
  TM_WRAP (v_tm_sec, v_tm_min, 60);
  TM_WRAP (v_tm_min, v_tm_hour, 60);
  TM_WRAP (v_tm_hour, v_tm_tday, 24);
  
  if ( (v_tm_wday = (v_tm_tday + 4) % 7) < 0 )
    v_tm_wday += 7;
  
  m = (long) v_tm_tday;
  
  if ( m >= 0 )
    {
      result->tm_year = 70;
      leap = TM_LEAP_CHECK (result->tm_year);
      
      while ( m >= (long) tm_days[leap + 2][12] )
	{
	  m -= (long) tm_days[leap + 2][12];
	  result->tm_year++;
	  leap = TM_LEAP_CHECK (result->tm_year);
	}
      
      v_tm_mon = 0;
      
      while ( m >= (long) tm_days[leap][v_tm_mon] )
	{
	  m -= (long) tm_days[leap][v_tm_mon];
	  v_tm_mon++;
	}
    }
  else
    {
      result->tm_year = 69;
      leap = TM_LEAP_CHECK (result->tm_year);
      
      while ( m < (long) -tm_days[leap + 2][12] )
	{
	  m += (long) tm_days[leap + 2][12];
	  result->tm_year--;
	  leap = TM_LEAP_CHECK (result->tm_year);
	}
      
      v_tm_mon = 11;
      
      while ( m < (long) -tm_days[leap][v_tm_mon] )
	{
	  m += (long) tm_days[leap][v_tm_mon];
	  v_tm_mon--;
	}
      
      m += (long) tm_days[leap][v_tm_mon];
    }
  
  result->tm_mday = (int) m + 1;
  result->tm_yday = tm_days[leap + 2][v_tm_mon] + m;
  result->tm_sec = v_tm_sec;
  result->tm_min = v_tm_min;
  result->tm_hour = v_tm_hour;
  result->tm_mon = v_tm_mon;
  result->tm_wday = v_tm_wday;
  
  return result;
}  /* End of dl_gmtime_r() */
