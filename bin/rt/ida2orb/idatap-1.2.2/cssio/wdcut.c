/* @(#)wdcut.c	1.11 01/28/97 */
/*======================================================================
 * 
 *  wdcut.c
 *
 *  Given a wfdisc record ptr and a start/end time, produce a new
 *  wfdisc record which points to only those data which fall into
 *  the given time window.  Return pointer to this new wfdisc record
 *  or NULL if the data do intersect the window.
 *
 * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * Copyright (c) 1997 Regents of the University of California.
 * All rights reserved.
 *====================================================================*/
#include <errno.h>
#include "cssio.h"

struct wfdisc *wdcut(input, beg, end)
struct wfdisc *input;
double beg;
double end;
{
int  wrdsiz;
long nskip, ncopy;
double span;
static struct wfdisc output;
struct { double start; double stop; } want, actual;

    if (beg >= end) {
        errno = EINVAL;
        return NULL;
    }

    want.start   = beg;
    want.stop    = end;
    actual.start = input->time;
    actual.stop  = input->endtime;

    if (actual.stop <= actual.start) return NULL;
    span = actual.stop - actual.start;

    if (want.start >= actual.stop || want.stop <= actual.start) return NULL;

    if (input->nsamp <= 0) return NULL;
    if (input->smprate <= (float) 0) return NULL;
    if (input->time == wfdisc_null.time) return NULL;
    if ((wrdsiz = cssio_wrdsize(input->datatype)) <= 0)return NULL;

    output = *input;
    nskip = (long) ((want.start - actual.start) * (double) output.smprate);

    if (output.nsamp < nskip) return NULL;

    if (nskip > 0) {
        output.nsamp -= nskip;
        output.time  += (double) nskip / (double) output.smprate;
        output.jdate  = atol(util_dttostr(output.time, 4));
    } else {
        nskip = 0;
    }

    ncopy = (long) ((want.stop - output.time) * (double) output.smprate);
    if (ncopy < output.nsamp) output.nsamp = ncopy;

    output.endtime = output.time + ((double) (output.nsamp - 1) /
                                    (double) output.smprate);
    output.foff += nskip * wrdsiz;

    return &output;
}
