/* @(#)rwfdrec.c	1.6 01/28/97 */
/*======================================================================
 *
 *  cssio/rwfdrec.c
 *
 *  Read a single wfdisc record.  2.8 format wfdiscs are detected and
 *  converted to 3.0 format silently and automatically.
 *
 *  Records with incorrect length are silently ignored.
 *
 * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * Copyright (c) 1997 Regents of the University of California.
 * All rights reserved.
 *====================================================================*/
#include <stdio.h>
#include <stdlib.h>
#include "cssio.h"
#include "util.h"

int rwfdrec(fp, wfdisc)
FILE *fp;                 
struct wfdisc *wfdisc;  
{
static char line[WFDISC_SIZE+2];
struct wfdisc28 wfdisc28;

    if (SETMODE(fileno(fp), O_BINARY) == -1) {
        perror("library routine rwfdrec: setmode");
        return -1;
    }

    while (1) {

        if (fgets(line, WFDISC_SIZE+1, fp) == NULL) return -1;

        if (strlen(line) == WFDISC28_SIZE) {
            sscanf(line, WFDISC28_SCS, WFDISC28_RVL(&wfdisc28));
            WFDISC28_TRM(&wfdisc28);
            wf28to30(wfdisc, &wfdisc28);
            return 0;
        } else if (strlen(line) == WFDISC_SIZE) {
            sscanf(line, WFDISC_SCS, WFDISC_RVL(wfdisc));
            WFDISC_TRM(wfdisc);
            return 0;
        }
    }
}
