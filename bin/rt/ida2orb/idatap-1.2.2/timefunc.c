/* @(#)timefunc.c	2.7 02/21/97 */
/*======================================================================
 *
 *  Misc. time related functions.
 *
 * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * Copyright (c) 1997 Regents of the University of California.
 * All rights reserved.
 * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 *
 *  util_attodt:
 *  Convert a string of the form "yyyy:ddd-hh:mm:ss:msc" to double time.
 *  Can truncate fields from the right.
 *
 *----------------------------------------------------------------------
 *
 *  util_dttostr:
 *  Given a double time value and format code, make a string of one of
 *  the following formats:
 *
 *  Format code   Output string
 *       0        yyyy:ddd-hh:mm:ss.msc
 *       1        Mon dd, year hh:mm:ss:msc
 *       2        yy:ddd-hh:mm:ss.msc, where input time is an interval
 *       3        yyyydddhhmmssmsc
 *       4        yyyyddd
 *       5        Day Mon dd, year
 *       6        yyyymmddhhmmss
 *       7        yyyy mm dd hh mm ss
 *       8        ddd-hh:mm:ss.msc, where input time is an interval
 *
 *  No newline is appended.
 *
 *  Returns a pointer to static space, therefore a call of the form:
 *
 *  printf("%s %s\n",
 *      util_dttostr(dtime1, form1), util_dttostr(dtime2, form1)
 *  );
 *
 *  will result in two duplicate time strings, even if dtime1 != dtime2
 *  or form1 != form2.  In such a case you should make distinct calls to
 *  printf.  For example:
 *
 *  printf("%s ",   util_dttostr(dtime1, form1));
 *  printf(" %s\n", util_dttostr(dtime2, form1));
 *
 *----------------------------------------------------------------------
 *
 *  util_lttostr:
 *  Given a long time value and format code, make a string of one of
 *  the following formats:
 *
 *  Format code   Output string
 *       0        yyyy:ddd-hh:mm:ss
 *       1        Mon dd, year hh:mm:ss
 *       2        yy:ddd-hh:mm:ss, where input time is an interval
 *       3        yydddhhmmss
 *       4        yyyyddd
 *       5        Day Mon dd, year
 *       6        yyyymmddhhmmss
 *       7        yyyy mm dd hh mm ss
 *       8        ddd-hh:mm:ss, where input time is an interval
 *
 *  No newline is appended.
 *
 *  Returns a pointer to static space, therefore the same words of
 *  caution from util_dttostr() above apply here as well.
 *
 *----------------------------------------------------------------------
 *
 *  util_tsplit:
 *  Split a double time to yyyy, ddd, hh, mm, ss, msc.
 *
 *----------------------------------------------------------------------
 *
 *  util_ydhmsmtod:
 *  Given year, day, hour, minutes, seconds, and milliseconds, return
 *  a double containing the seconds since 00:00:00.000 Jan 1, 1970.
 *
 *  Only works for times after Jan 1, 1970!
 *
 *----------------------------------------------------------------------
 *
 *  util_jdtomd:
 *  Given year and day of year, determine month and day of month.
 *
 *----------------------------------------------------------------------
 *
 *  util_ymdtojd:
 *  Given year, month, and day determine day of year.
 *
 *----------------------------------------------------------------------
 *
 *  util_today:
 *  Returns today's date in YYYYDDD form.
 *
 *====================================================================*/
#include <stdio.h>
#include <time.h>
#include <ctype.h>
#include <string.h>
#include <sys/types.h>
#include "util.h"

#ifndef leap_year
#define leap_year(i) ((i % 4 == 0 && i % 100 != 0) || i % 400 == 0)
#endif

static char _util_daytab[2][13] = {
    {0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31},
    {0, 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31},
};

static char *_util_month[] = {
    "Jan", "Feb", "Mar", "Apr", "May", "Jun", 
    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
};

static char *_util_day[] = {
    "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"
};

static char *_util_bad_call = "Bad util_dttostr input!";

#define _UTIL_MIN_CODE 0
#define _UTIL_MAX_CODE 8

static char *_util_bad_str[_UTIL_MAX_CODE-_UTIL_MIN_CODE+1] = {
    "                     ",
    "                         ",
    "                   ",
    "                ",
    "       ",
    "                ",
    "              ",
    "                   ",
    "                "
};

static char _util_out_str[] = "xxxxxxxxxxxxxxxxxxxxxxxxx";

static char copy[1024];

#define SPM (       60L)
#define SPH (SPM *  60L)
#define SPD (SPH *  24L)
#define SPY (SPD * 365L)

/***********************************************************************/

double util_attodt(string)
char *string;
{
char *token[6];
int errors, ntoken, yr, da, hr, mn, sc, ms;
static char *fid = "util_attodt";

/*  Check for special case ("present")  */

    if (strcasecmp(string, "present") == 0) return (double) 2147483647;

/*  Parse (copy of) string  */

    strcpy(copy, string);

    ntoken = util_parse(copy, token, "-/.:(),;", 6, 0);

    yr = hr = mn = sc = ms = 0; da = 1;

/* Decode the various pieces */

    switch (ntoken) {

      case 6:
        ms = atoi(token[5]);
      case 5:
        sc = atoi(token[4]);
      case 4:
        mn = atoi(token[3]);
      case 3:
        hr = atoi(token[2]);
      case 2:
        da = atoi(token[1]);
      case 1:
        yr = atoi(token[0]) + ((strlen(token[0]) == 2) ? 1900 : 0);
        break;
      default:
        fprintf(stderr, "%s: illegal string '%s'\n", fid, string);
        util_log(1, "%s: illegal string '%s'", fid, string);
        return -1.0;
    }

    errors = 0;
    if (yr < 1970) {
        util_log(1, "%s: illegal yr `%d'", fid, yr);
        ++errors;
    }
    if (da < 1 || da > 366) {
        util_log(1, "%s: illegal da `%d'", fid, da);
        ++errors;
    }
    if (hr < 0 || hr >  23) {
        util_log(1, "%s: illegal hr `%d'", fid, hr);
        ++errors;
    }
    if (mn < 0 || mn >  59) {
        util_log(1, "%s: illegal mn `%d'", fid, mn);
        ++errors;
    }
    if (sc < 0 || sc >  59) {
        util_log(1, "%s: illegal sc `%d'", fid, sc);
        ++errors;
    }
    if (ms < 0 || ms > 999) {
        util_log(1, "%s: illegal ms `%d'", fid, ms);
        ++errors;
    }
    
    if (errors) {
        fprintf(stderr, "%s: illegal value in string '%s'\n", fid, string);
        util_log(1, "%s: illegal value in string '%s'\n", fid, string);
        return -2.0;
    }

    return util_ydhmsmtod(yr, da, hr, mn, sc, ms);

}

/***********************************************************************/

char *util_dttostr(dtime, code)
double dtime;
int code;
{
struct tm *tm;
long  ltime;
float ffrac;
int   ifrac, yr, da, hr, mn, sc, ms;

    if (code < _UTIL_MIN_CODE || code > _UTIL_MAX_CODE) return _util_bad_call;

    ltime = (long) dtime;
    ffrac = (float) (dtime - (double) ltime) * 1000.0;
    ifrac = (int) ffrac;
    if (ffrac - (float) ifrac >= 0.5) ifrac++;

/* Deal with the intervals */

    if (code == 2 || code == 8) {
        yr = ltime / SPY; ltime -= yr * SPY;
        da = ltime / SPD; ltime -= da * SPD;
        hr = ltime / SPH; ltime -= hr * SPH;
        mn = ltime / SPM; ltime -= mn * SPM;
        sc = ltime;
        ms = ifrac;
        if (code == 2) {
            sprintf(_util_out_str,"%2.2d:%3.3d-%2.2d:%2.2d:%2.2d.%3.3d",
                yr, da, hr, mn, sc, ms
            );
        } else {
            sprintf(_util_out_str,"%3.3d-%2.2d:%2.2d:%2.2d.%3.3d",
                da, hr, mn, sc, ms
            );
        }
        return _util_out_str;
    }

    if ((tm = gmtime(&ltime)) == NULL) return _util_bad_str[code];
    tm->tm_year += 1900;
    tm->tm_yday += 1;
    tm->tm_mon  += 1;

    switch (code) {
        case 0:
            sprintf(_util_out_str,"%4.4d:%3.3d-%2.2d:%2.2d:%2.2d.%3.3d",
                   tm->tm_year, tm->tm_yday, tm->tm_hour,
                   tm->tm_min, tm->tm_sec, ifrac);
            break;
        case 1:
            sprintf(_util_out_str, "%s %2.2d, %4.4d %2.2d:%2.2d:%2.2d.%3.3d",
              _util_month[tm->tm_mon], tm->tm_mday,tm->tm_year + 1900,
              tm->tm_hour, tm->tm_min, tm->tm_sec, ifrac);
            break;
        case 3:
            sprintf(_util_out_str,"%4.4d%3.3d%2.2d%2.2d%2.2d%3.3d",
                   tm->tm_year, tm->tm_yday, tm->tm_hour,
                   tm->tm_min, tm->tm_sec, ifrac);
            break;
        case 4:
            sprintf(_util_out_str,"%4.4d%3.3d",
                   tm->tm_year, tm->tm_yday);
            break;
        case 5:
            sprintf(_util_out_str, "%s %2.2d/%2.2d/%4.4d",
              _util_day[tm->tm_wday], tm->tm_mon, tm->tm_mday, tm->tm_year);
            break;
        case 6:
            sprintf(_util_out_str,"%4.4d%2.2d%2.2d%2.2d%2.2d%2.2d",
                   tm->tm_year, tm->tm_mon, tm->tm_mday,
                   tm->tm_hour, tm->tm_min, tm->tm_sec);
            break;
        case 7:
            sprintf(_util_out_str,"%4.4d %2.2d %2.2d %2.2d %2.2d %2.2d",
                   tm->tm_year, tm->tm_mon, tm->tm_mday,
                   tm->tm_hour, tm->tm_min, tm->tm_sec);
            break;
        default: 
            strcpy(_util_out_str, _util_bad_call);
            break;
    }

    return _util_out_str;
}

/***********************************************************************/

char *util_lttostr(ltime, code)
long ltime;
int code;
{
char *buffer;

    buffer = util_dttostr((double) ltime, code);
    if (code >= 4 && code <= 7) return buffer;

    if (code != 3) {
        buffer[strlen(buffer)-strlen(".msc")] = 0;
    } else {
        buffer[strlen(buffer)-strlen("msc")] = 0;
    }

    return buffer;
}

/***********************************************************************/

void util_tsplit(dtime, yr, da, hr, mn, sc, ms)
double dtime;
int *yr, *da, *hr, *mn, *sc, *ms;
{
long  ltime;
int   imsc;
double dmsc;
struct tm *tiempo;

    ltime = (long) dtime;
    dmsc = ((dtime - (double) ltime)) * (double) 1000.0;
    imsc = (int) dmsc;
    if (dmsc - (double) imsc >= (double) 0.5) imsc++;
    if (imsc == 1000) {
        ++ltime;
        imsc = 0;
    }

    tiempo = gmtime(&ltime);
    *yr = 1900 + tiempo->tm_year;
    *da = ++tiempo->tm_yday;
    *hr = tiempo->tm_hour;
    *mn = tiempo->tm_min;
    *sc = tiempo->tm_sec;
    *ms = imsc;

}

/***********************************************************************/

double util_ydhmsmtod(yr, da, hr, mn, sc, ms)
int yr, da, hr, mn, sc, ms;
{
int i, days_in_year_part;
long   secs;

    days_in_year_part = 0;
    for (i = 1970; i < yr; i++) days_in_year_part += daysize(i);
    secs = (long) days_in_year_part * SPD;

    secs += (long)(da-1)*SPD + (long)hr*SPH + (long)mn*SPM + (long)sc;

    return (double) secs + ((double) (ms)/1000.0);

}

/***********************************************************************/

int util_jdtomd(year, day, m_no, d_no)
int year, day, *m_no, *d_no;
{
int i, leap;

    leap = leap_year(year);
    
    for (i = 1; day > _util_daytab[leap][i]; i++) day -= _util_daytab[leap][i];

    *m_no = i;
    *d_no = day;
}

/***********************************************************************/

int util_ymdtojd(year, mo, da)
int year, mo, da;
{
int jd, m, leap;

    leap = leap_year(year);
    for (jd = 0, m = 1; m < mo; m++) jd += _util_daytab[leap][m];
    jd += da;

    return jd;
}

/***********************************************************************/

long util_today()
{
time_t now;
struct tm *current;

    now = time(NULL);
    current = localtime(&now);
    current->tm_year += 1900;
    ++current->tm_yday;

    return (1000 * current->tm_year) + current->tm_yday;

}
