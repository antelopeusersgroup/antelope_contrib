/* @(#)wwfdisc.c	1.10 01/28/97 */
/*======================================================================
 *
 *  cssio/wwfdisc.c
 *
 *  wwfdisc():   Write a 3.0 wfdisc record to the specified file.
 *  wwfdisc28(): Write a 2.8 wfdisc record to the specified file.
 *
 * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * Copyright (c) 1997 Regents of the University of California.
 * All rights reserved.
 *====================================================================*/
#include <stdio.h>
#include <errno.h>
#include "cssio.h"
#include "util.h"

char *wdtoa(wfdisc)
struct wfdisc *wfdisc;
{
static char buffer[WFDISC_SIZE+1];

    if (wfdisc == NULL) {
        errno = EINVAL;
        return NULL;
    }

    sprintf(buffer, "%-6.6s ",   util_ucase(wfdisc->sta));
    sprintf(buffer+strlen(buffer), "%-8.8s ",   util_lcase(wfdisc->chan));
    sprintf(buffer+strlen(buffer), "%17.5f ",   wfdisc->time);
    sprintf(buffer+strlen(buffer), "%8ld ",     wfdisc->wfid);
    sprintf(buffer+strlen(buffer), "%8ld ",     wfdisc->chanid);
    sprintf(buffer+strlen(buffer), "%8ld ",     wfdisc->jdate);
    sprintf(buffer+strlen(buffer), "%17.5f ",   wfdisc->endtime);
    sprintf(buffer+strlen(buffer), "%8ld ",     wfdisc->nsamp);
    sprintf(buffer+strlen(buffer), "%11.7f ",   wfdisc->smprate);
    sprintf(buffer+strlen(buffer), "%16.6f ",   wfdisc->calib);
    sprintf(buffer+strlen(buffer), "%16.6f ",   wfdisc->calper);
    sprintf(buffer+strlen(buffer), "%-6.6s ",   wfdisc->instype);
    sprintf(buffer+strlen(buffer), "%c ",       wfdisc->segtype);
    sprintf(buffer+strlen(buffer), "%-2.2s ",   wfdisc->datatype);
    sprintf(buffer+strlen(buffer), "%c ",       wfdisc->clip);
    sprintf(buffer+strlen(buffer), "%-64.64s ", wfdisc->dir);
    sprintf(buffer+strlen(buffer), "%-32.32s ", wfdisc->dfile);
    sprintf(buffer+strlen(buffer), "%10ld ",    wfdisc->foff);
    sprintf(buffer+strlen(buffer), "%8ld ",     wfdisc->commid);
    sprintf(buffer+strlen(buffer), "%-17.17s",  wfdisc->lddate);

    return buffer;
}

int wwfdisc(fp, wfdisc)
FILE *fp;
struct wfdisc *wfdisc;
{
char *string;

    if (SETMODE(fileno(fp), O_BINARY) == -1) {
        perror("library routine wwfdisc: setmode");
        exit(1);
    }

    string = wdtoa(wfdisc);
    fprintf(fp, "%s\n", string != NULL ? string : "illegal wfdisc record");

    return ferror(fp) ? -1 : 0;
}

int wwfdisc28(fp, wfdisc28)
FILE *fp;
struct wfdisc28 *wfdisc28;
{
char *format = "%?.?f ";

    if (SETMODE(fileno(fp), O_BINARY) == -1) {
        perror("library routine wwfdisc28: setmode");
        exit(1);
    }

    fprintf(fp, "%8ld ",      wfdisc28->date);
    fprintf(fp, "%15.3f ",    wfdisc28->time);
    fprintf(fp, "%-6.6s ",    wfdisc28->sta);
    fprintf(fp, "%-2.2s ",    wfdisc28->chan);
    fprintf(fp, "%8ld ",      wfdisc28->nsamp);
    fprintf(fp, "%11.7f ",    wfdisc28->smprat);
    strcpy(format, "%9.6f ");
    if (wfdisc28->calib < 0) {
        if (wfdisc28->calib > -10.0) 
            strcpy(format, "%9.6f ");
        else if (wfdisc28->calib > -100.0)
            strcpy(format, "%9.5f ");
        else if (wfdisc28->calib > -1000.0)
            strcpy(format, "%9.4f ");
        else if (wfdisc28->calib > -10000.0)
            strcpy(format, "%9.3f ");
        else if (wfdisc28->calib > -100000.0)
            strcpy(format, "%9.2f ");
        else if (wfdisc28->calib > -1000000.0)
            strcpy(format, "%9.1f ");
    } else {
        if (wfdisc28->calib < 100.0) 
            strcpy(format, "%9.6f ");
        else if (wfdisc28->calib < 1000.0)
            strcpy(format, "%9.5f ");
        else if (wfdisc28->calib < 10000.0)
            strcpy(format, "%9.4f ");
        else if (wfdisc28->calib < 100000.0)
            strcpy(format, "%9.3f ");
        else if (wfdisc28->calib < 1000000.0)
            strcpy(format, "%9.2f ");
        else if (wfdisc28->calib < 10000000.0)
            strcpy(format, "%9.1f ");
    }
    fprintf(fp, format,       wfdisc28->calib);
    if (wfdisc28->calper < 100.0) {
        strcpy(format, "%7.4f ");
    } else if (wfdisc28->calper < 1000.0) {
        strcpy(format, "%7.3f ");
    }
    fprintf(fp, format,       wfdisc28->calper);
    fprintf(fp, "%-6.6s ",    wfdisc28->instyp);
    fprintf(fp, "%c ",        wfdisc28->segtyp);
    fprintf(fp, "%-2.2s ",    wfdisc28->dattyp);
    fprintf(fp, "%c ",        wfdisc28->clip);
    fprintf(fp, "%8ld ",      wfdisc28->chid);
    fprintf(fp, "%8ld ",      wfdisc28->wfid);
    fprintf(fp, "%-30.30s ",  wfdisc28->dir);
    fprintf(fp, "%-20.20s ",  wfdisc28->file);
    fprintf(fp, "%10ld ",     wfdisc28->foff);
    fprintf(fp, "%8ld ",      wfdisc28->adate);
    fprintf(fp, "%-30.30s\n", wfdisc28->remark);

    return ferror(fp) ? -1 : 0;
}
