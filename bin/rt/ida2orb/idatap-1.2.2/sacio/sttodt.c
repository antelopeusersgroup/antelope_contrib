/* @(#)sttodt.c	1.5 7/12/95 */
/*======================================================================
 *
 *  sacio/sacttodt.c
 *
 *  Convert SAC reference time into double.
 *
 *====================================================================*/
#include "sacio.h"
 
#ifndef leap_year
#define leap_year(i) ((i % 4 == 0 && i % 100 != 0) || i % 400 == 0)
#endif
 
#ifndef daysize
#define daysize(i) (365 + leap_year(i))
#endif

#define SPM (      60L) /* seconds per minute */
#define SPH (SPM * 60L) /* seconds per hour   */
#define SPD (SPH * 24L) /* seconds per day    */

double sacio_ydhmsmtod(yr, da, hr, mn, sc, ms)
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

double sacio_sttodt(hdr)
struct sac_header *hdr;
{
int i, yr, da, hr, mn, sc, ms;
double dtmp;

    if (hdr == NULL) return (double) -12345;

    if (hdr->nzyear < 0) return (double) -12345;
    if (hdr->nzjday < 0) return (double) -12345;
    if (hdr->nzhour < 0) return (double) -12345;
    if (hdr->nzmin  < 0) return (double) -12345;
    if (hdr->nzsec  < 0) return (double) -12345;
    if (hdr->nzmsec < 0) return (double) -12345;

    yr = (int) hdr->nzyear;
    da = (int) hdr->nzjday;
    hr = (int) hdr->nzhour;
    mn = (int) hdr->nzmin;
    sc = (int) hdr->nzsec;
    ms = (int) hdr->nzmsec;

    if (
        yr < 1970 || yr > 1999 ||
        da <    1 || da >  366 ||
        hr <    0 || hr >   23 ||
        mn <    0 || mn >   59 ||
        sc <    0 || sc >   59 ||
        ms <    0 || ms >  999
    ) return (double) -12345;

    return sacio_ydhmsmtod(yr,da,hr,mn,sc,ms);

}
