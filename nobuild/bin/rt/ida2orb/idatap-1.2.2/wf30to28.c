/* @(#)wf30to28.c	1.5 01/28/97 */
/*======================================================================
 *
 *  cssio/wf30to28->c
 *
 *  Convert a 3.0 wfdisc record to 2.8.
 *
 * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * Copyright (c) 1997 Regents of the University of California.
 * All rights reserved.
 *====================================================================*/
#include <memory.h>
#include "cssio.h"

void wf30to28(wfdisc28, wfdisc30)
struct wfdisc28  *wfdisc28;
struct wfdisc *wfdisc30;
{

    *wfdisc28 = wfdisc28_null;

    memcpy(wfdisc28->sta,    wfdisc30->sta,      6); wfdisc28->sta[6]    = 0;
    memcpy(wfdisc28->chan,   wfdisc30->chan,     2); wfdisc28->chan[2]   = 0;
    memcpy(wfdisc28->instyp, wfdisc30->instype,  6); wfdisc28->instyp[6] = 0;
    memcpy(wfdisc28->dattyp, wfdisc30->datatype, 2); wfdisc28->dattyp[2] = 0;
    memcpy(wfdisc28->dir,    wfdisc30->dir,     30); wfdisc28->dir[30]   = 0;
    memcpy(wfdisc28->file,   wfdisc30->dfile,   20); wfdisc28->file[20]  = 0;

    wfdisc28->date    = wfdisc30->jdate;
    wfdisc28->time    = wfdisc30->time;
    wfdisc28->nsamp   = wfdisc30->nsamp;
    wfdisc28->smprat  = wfdisc30->smprate;
    wfdisc28->calib   = wfdisc30->calib;
    wfdisc28->calper  = wfdisc30->calper;
    wfdisc28->segtyp  = wfdisc30->segtype;
    wfdisc28->clip    = wfdisc30->clip;
    wfdisc28->chid    = wfdisc30->chanid;
    wfdisc28->wfid    = wfdisc30->wfid;
    wfdisc28->foff    = wfdisc30->foff;

}
