/* @(#)wf28to30.c	1.6 01/28/97 */
/*======================================================================
 *
 *  cssio/wf28tod0.c
 *
 *  Convert a 2.8 wfdisc record to 3.0.
 *
 * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * Copyright (c) 1997 Regents of the University of California.
 * All rights reserved.
 *====================================================================*/
#include "cssio.h"

void wf28to30(wfdisc30, wfdisc28)
struct wfdisc *wfdisc30;
struct wfdisc28 *wfdisc28;
{
    *wfdisc30 = wfdisc_null;

    strcpy(wfdisc30->sta,      wfdisc28->sta);
    strcpy(wfdisc30->chan,     wfdisc28->chan);
           wfdisc30->time    = wfdisc28->time;
           wfdisc30->wfid    = wfdisc28->wfid;
           wfdisc30->chanid  = wfdisc28->chid;
           wfdisc30->jdate   = wfdisc28->date;
           wfdisc30->nsamp   = wfdisc28->nsamp;
           wfdisc30->smprate = wfdisc28->smprat;
           wfdisc30->calib   = wfdisc28->calib;
           wfdisc30->calper  = wfdisc28->calper;
    strcpy(wfdisc30->instype,  wfdisc28->instyp);
           wfdisc30->segtype = wfdisc28->segtyp;
    strcpy(wfdisc30->datatype, wfdisc28->dattyp);
           wfdisc30->clip    = wfdisc28->clip;
    strcpy(wfdisc30->dir,      wfdisc28->dir);
    strcpy(wfdisc30->dfile,    wfdisc28->file);
           wfdisc30->foff    = wfdisc28->foff;

    wfdisc30->endtime = wfdisc30->time + (wfdisc30->nsamp-1)/wfdisc30->smprate;
}
