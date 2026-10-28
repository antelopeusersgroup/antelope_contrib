/***********************************************************************/ /**
 * @file timeutils.c
 *
 * General time utility routines and routines for dealing with libdali
 * time values of dltime_t type.
 *
 * This file is part of the DataLink Library.
 *
 * Copyright (c) 2020 Chad Trabant, IRIS Data Management Center
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 ***************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "gmtime64.h"
#include "libdali.h"

static dltime_t time2dltime_int (int year, int day, int hour,
                                 int min, int sec, int usec);

/***********************************************************************/ /**
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
  if (year < 1900 || year > 2100)
  {
    dl_log (2, 0, "dl_doy2md(): year (%d) is out of range\n", year);
    return -1;
  }

  /* Test for leap year */
  leap = (((year % 4 == 0) && (year % 100 != 0)) || (year % 400 == 0)) ? 1 : 0;

  /* Add a day to February if leap year */
  if (leap)
    days[1]++;

  if (jday > 365 + leap || jday <= 0)
  {
    dl_log (2, 0, "dl_doy2md(): day-of-year (%d) is out of range\n", jday);
    return -1;
  }

  for (idx = 0; idx < 12; idx++)
  {
    jday -= days[idx];

    if (jday <= 0)
    {
      *month = idx + 1;
      *mday  = days[idx] + jday;
      break;
    }
  }

  return 0;
} /* End of dl_doy2md() */

/***********************************************************************/ /**
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
  if (year < 1900 || year > 2100)
  {
    dl_log (2, 0, "dl_md2doy(): year (%d) is out of range\n", year);
    return -1;
  }
  if (month < 1 || month > 12)
  {
    dl_log (2, 0, "dl_md2doy(): month (%d) is out of range\n", month);
    return -1;
  }
  if (mday < 1 || mday > 31)
  {
    dl_log (2, 0, "dl_md2doy(): day-of-month (%d) is out of range\n", mday);
    return -1;
  }

  /* Test for leap year */
  leap = (((year % 4 == 0) && (year % 100 != 0)) || (year % 400 == 0)) ? 1 : 0;

  /* Add a day to February if leap year */
  if (leap)
    days[1]++;

  /* Check that the day-of-month jives with specified month */
  if (mday > days[month - 1])
  {
    dl_log (2, 0, "dl_md2doy(): day-of-month (%d) is out of range for month %d\n",
            mday, month);
    return -1;
  }

  *jday = 0;
  month--;

  for (idx = 0; idx < 12; idx++)
  {
    if (idx == month)
    {
      *jday += mday;
      break;
    }

    *jday += days[idx];
  }

  return 0;
} /* End of dl_md2doy() */

/***********************************************************************/ /**
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

  if (isotimestr == NULL)
    return NULL;

  /* Reduce to Unix/POSIX epoch time and fractional seconds */
  isec   = DL_DLTIME2EPOCH (dltime);
  ifract = (int)(dltime - (isec * DLTMODULUS));

  /* Adjust for negative epoch times */
  if (dltime < 0 && ifract != 0)
  {
    isec -= 1;
    ifract = DLTMODULUS - (-ifract);
  }

  if (!(dl_gmtime64_r (&isec, &tms)))
    return NULL;

  if (subseconds)
    /* Assuming ifract has at least microsecond precision */
    ret = snprintf (isotimestr, 27, "%4d-%02d-%02dT%02d:%02d:%02d.%06d",
                    tms.tm_year + 1900, tms.tm_mon + 1, tms.tm_mday,
                    tms.tm_hour, tms.tm_min, tms.tm_sec, ifract);
  else
    ret = snprintf (isotimestr, 20, "%4d-%02d-%02dT%02d:%02d:%02d",
                    tms.tm_year + 1900, tms.tm_mon + 1, tms.tm_mday,
                    tms.tm_hour, tms.tm_min, tms.tm_sec);

  if (ret != 26 && ret != 19)
    return NULL;
  else
    return isotimestr;
} /* End of dl_dltime2isotimestr() */

/***********************************************************************/ /**
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

  if (mdtimestr == NULL)
    return NULL;

  /* Reduce to Unix/POSIX epoch time and fractional seconds */
  isec   = DL_DLTIME2EPOCH (dltime);
  ifract = (int)(dltime - (isec * DLTMODULUS));

  /* Adjust for negative epoch times */
  if (dltime < 0 && ifract != 0)
  {
    isec -= 1;
    ifract = DLTMODULUS - (-ifract);
  }

  if (!(dl_gmtime64_r (&isec, &tms)))
    return NULL;

  if (subseconds)
    /* Assuming ifract has at least microsecond precision */
    ret = snprintf (mdtimestr, 27, "%4d-%02d-%02d %02d:%02d:%02d.%06d",
                    tms.tm_year + 1900, tms.tm_mon + 1, tms.tm_mday,
                    tms.tm_hour, tms.tm_min, tms.tm_sec, ifract);
  else
    ret = snprintf (mdtimestr, 20, "%4d-%02d-%02d %02d:%02d:%02d",
                    tms.tm_year + 1900, tms.tm_mon + 1, tms.tm_mday,
                    tms.tm_hour, tms.tm_min, tms.tm_sec);

  if (ret != 26 && ret != 19)
    return NULL;
  else
    return mdtimestr;
} /* End of dl_dltime2mdtimestr() */

/***********************************************************************/ /**
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

  if (seedtimestr == NULL)
    return NULL;

  /* Reduce to Unix/POSIX epoch time and fractional seconds */
  isec   = DL_DLTIME2EPOCH (dltime);
  ifract = (int)(dltime - (isec * DLTMODULUS));

  /* Adjust for negative epoch times */
  if (dltime < 0 && ifract != 0)
  {
    isec -= 1;
    ifract = DLTMODULUS - (-ifract);
  }

  if (!(dl_gmtime64_r (&isec, &tms)))
    return NULL;

  if (subseconds)
    /* Assuming ifract has at least microsecond precision */
    ret = snprintf (seedtimestr, 25, "%4d,%03d,%02d:%02d:%02d.%06d",
                    tms.tm_year + 1900, tms.tm_yday + 1,
                    tms.tm_hour, tms.tm_min, tms.tm_sec, ifract);
  else
    /* Assuming ifract has at least microsecond precision */
    ret = snprintf (seedtimestr, 18, "%4d,%03d,%02d:%02d:%02d",
                    tms.tm_year + 1900, tms.tm_yday + 1,
                    tms.tm_hour, tms.tm_min, tms.tm_sec);

  if (ret != 24 && ret != 17)
    return NULL;
  else
    return seedtimestr;
} /* End of dl_dltime2seedtimestr() */


/***************************************************************************
 * INTERNAL Convert specified date-time values to a high precision epoch time.
 *
 * This is an internal version which does no range checking, it is
 * assumed that checking the range for each value has already been
 * done.
 *
 * Returns dltime_t time value.
 ***************************************************************************/
static dltime_t
time2dltime_int (int year, int yday, int hour, int min, int sec, int usec)
{
  dltime_t dltime;
  int32_t shortyear;
  int32_t a4, a100, a400;
  int32_t intervening_leap_days;
  int32_t days;

  shortyear = year - 1900;

  a4                    = (shortyear >> 2) + 475 - !(shortyear & 3);
  a100                  = a4 / 25 - (a4 % 25 < 0);
  a400                  = a100 >> 2;
  intervening_leap_days = (a4 - 492) - (a100 - 19) + (a400 - 4);

  days = (365 * (shortyear - 70) + intervening_leap_days + (yday - 1));

  dltime = (dltime_t) (60 * (60 * ((dltime_t)24 * days + hour) + min) + sec) * DLTMODULUS + usec;

  return dltime;
} /* End of time2dltime_int() */

/***********************************************************************/ /**
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
  if (year < 1900 || year > 2100)
  {
    dl_log (2, 0, "dl_time2dltime(): Error with year value: %d\n", year);
    return DLTERROR;
  }

  if (day < 1 || day > 366)
  {
    dl_log (2, 0, "dl_time2dltime(): Error with day value: %d\n", day);
    return DLTERROR;
  }

  if (hour < 0 || hour > 23)
  {
    dl_log (2, 0, "dl_time2dltime(): Error with hour value: %d\n", hour);
    return DLTERROR;
  }

  if (min < 0 || min > 59)
  {
    dl_log (2, 0, "dl_time2dltime(): Error with minute value: %d\n", min);
    return DLTERROR;
  }

  if (sec < 0 || sec > 60)
  {
    dl_log (2, 0, "dl_time2dltime(): Error with second value: %d\n", sec);
    return DLTERROR;
  }

  if (usec < 0 || usec > 999999)
  {
    dl_log (2, 0, "dl_time2dltime(): Error with microsecond value: %d\n", usec);
    return DLTERROR;
  }

  return time2dltime_int (year, day, hour, min, sec, usec);
} /* End of dl_time2dltime() */

/***********************************************************************/ /**
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
  int year    = 0;
  int day     = 1;
  int hour    = 0;
  int min     = 0;
  int sec     = 0;
  float fusec = 0.0;
  int usec    = 0;

  fields = sscanf (seedtimestr, "%d%*[,:.]%d%*[,:.]%d%*[,:.]%d%*[,:.]%d%f",
                   &year, &day, &hour, &min, &sec, &fusec);

  /* Convert fractional seconds to microseconds */
  if (fusec != 0.0)
  {
    usec = (int)(fusec * 1000000.0 + 0.5);
  }

  if (fields < 1)
  {
    dl_log (2, 0, "dl_seedtimestr2dltime(): Error converting time string: %s\n", seedtimestr);
    return DLTERROR;
  }

  if (year < 1900 || year > 3000)
  {
    dl_log (2, 0, "dl_seedtimestr2dltime(): Error with year value: %d\n", year);
    return DLTERROR;
  }

  if (day < 1 || day > 366)
  {
    dl_log (2, 0, "dl_seedtimestr2dltime(): Error with day value: %d\n", day);
    return DLTERROR;
  }

  if (hour < 0 || hour > 23)
  {
    dl_log (2, 0, "dl_seedtimestr2dltime(): Error with hour value: %d\n", hour);
    return DLTERROR;
  }

  if (min < 0 || min > 59)
  {
    dl_log (2, 0, "dl_seedtimestr2dltime(): Error with minute value: %d\n", min);
    return DLTERROR;
  }

  if (sec < 0 || sec > 60)
  {
    dl_log (2, 0, "dl_seedtimestr2dltime(): Error with second value: %d\n", sec);
    return DLTERROR;
  }

  if (usec < 0 || usec > 999999)
  {
    dl_log (2, 0, "dl_seedtimestr2dltime(): Error with fractional second value: %d\n", usec);
    return DLTERROR;
  }

  return time2dltime_int (year, day, hour, min, sec, usec);
} /* End of dl_seedtimestr2dltime() */

/***********************************************************************/ /**
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
  int year    = 0;
  int mon     = 1;
  int mday    = 1;
  int day     = 1;
  int hour    = 0;
  int min     = 0;
  int sec     = 0;
  float fusec = 0.0;
  int usec    = 0;

  fields = sscanf (timestr, "%d%*[-/:.]%d%*[-/:.]%d%*[-/:.T ]%d%*[-/:.]%d%*[- /:.]%d%f",
                   &year, &mon, &mday, &hour, &min, &sec, &fusec);

  /* Convert fractional seconds to microseconds */
  if (fusec != 0.0)
  {
    usec = (int)(fusec * 1000000.0 + 0.5);
  }

  if (fields < 1)
  {
    dl_log (2, 0, "dl_timestr2dltime(): Error converting time string: %s\n", timestr);
    return DLTERROR;
  }

  if (year < 1900 || year > 3000)
  {
    dl_log (2, 0, "dl_timestr2dltime(): Error with year value: %d\n", year);
    return DLTERROR;
  }

  if (mon < 1 || mon > 12)
  {
    dl_log (2, 0, "dl_timestr2dltime(): Error with month value: %d\n", mon);
    return DLTERROR;
  }

  if (mday < 1 || mday > 31)
  {
    dl_log (2, 0, "dl_timestr2dltime(): Error with day value: %d\n", mday);
    return DLTERROR;
  }

  /* Convert month and day-of-month to day-of-year */
  if (dl_md2doy (year, mon, mday, &day))
  {
    return DLTERROR;
  }

  if (hour < 0 || hour > 23)
  {
    dl_log (2, 0, "dl_timestr2dltime(): Error with hour value: %d\n", hour);
    return DLTERROR;
  }

  if (min < 0 || min > 59)
  {
    dl_log (2, 0, "dl_timestr2dltime(): Error with minute value: %d\n", min);
    return DLTERROR;
  }

  if (sec < 0 || sec > 60)
  {
    dl_log (2, 0, "dl_timestr2dltime(): Error with second value: %d\n", sec);
    return DLTERROR;
  }

  if (usec < 0 || usec > 999999)
  {
    dl_log (2, 0, "dl_timestr2dltime(): Error with fractional second value: %d\n", usec);
    return DLTERROR;
  }

  return time2dltime_int (year, day, hour, min, sec, usec);
} /* End of dl_timestr2dltime() */
