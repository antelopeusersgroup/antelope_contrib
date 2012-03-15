/* @(#)cssio.h	1.15 01/31/97 */
/*======================================================================
 *
 *  include/cssio.h
 *
 *  Include file for cssio library routines.
 *
 * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * Copyright (c) 1997 Regents of the University of California.
 * All rights reserved.
 *====================================================================*/
#ifndef cssio_h_included
#define cssio_h_included

#include <stdio.h>
#include "css/2.8/wfdiscio.h"
#include "css/3.0/wfdiscio.h"
#include "sacio.h"

#define CSS_28 0
#define CSS_30 1

/*  cssio library function prototypes  */

#ifdef __STDC__

int css2sac(struct wfdisc *, long, int, struct sac_header *, int);
int rwfdrec(FILE *, struct wfdisc *);
int wwfdisc(FILE *, struct wfdisc *);
int wwfdisc28(FILE *, struct wfdisc28 *);
char *cssio_datatype(int, unsigned long, int);
long cssio_jdate(double);
int  cssio_wrdsize(char *);

long rwfdisc(FILE *, struct wfdisc **);

void wf28to30(struct wfdisc *, struct wfdisc28 *);
void wf30to28(struct wfdisc28 *, struct wfdisc *);

struct wfdisc *wdcut(struct wfdisc *, double, double);

char *wdtoa(struct wfdisc *);

#else

int css2sac();
int rwfdrec();
int wwfdisc();
int wwfdisc28();
char *cssio_datatype();
long cssio_jdate();
int  cssio_wrdsize();

long rwfdisc();

void wf28to30();
void wf30to28();

struct wfdisc *wdcut();

char *wdtoa();

#endif /* ifdef __STDC__ */

#endif
