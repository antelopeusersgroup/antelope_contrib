/* @(#)jdate.c	1.2 01/28/97 */
/*======================================================================
 *
 *  Given an epoch time, return the equivalent jdate.
 * 
 * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * Copyright (c) 1997 Regents of the University of California.
 * All rights reserved.
 *====================================================================*/
#include <time.h>
#include <sys/types.h>
#include "cssio.h"

long cssio_jdate(dtime)
double dtime;
{
long  ltime;
struct tm *tiempo;

    ltime = (long) dtime;
    tiempo = gmtime(&ltime);
    return ((1900 + tiempo->tm_year) * 1000) + tiempo->tm_yday + 1;
}
