/* @(#)rwfdisc.c	1.6 01/28/97 */
/*======================================================================
 *
 *  cssio/rwfdisc.c
 *
 *  Read an entire wfdisc file into an array.  2.8 wfdiscs are detected
 *  and converted to 3.0 format silently and automatically.
 *
 * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * Copyright (c) 1997 Regents of the University of California.
 * All rights reserved.
 *====================================================================*/
#include <stdio.h>
#include <stdlib.h>
#include "cssio.h"

long rwfdisc(fp, output)
FILE *fp;                 
struct wfdisc **output;  
{
long i, nrec = 0;

struct list {
    struct wfdisc wfdisc;
    struct list *next;
} head, *crnt, *new, *next;

struct wfdisc28 wfdisc28;
struct wfdisc wfdisc, *array;

/*  Read entire file into linked list  */

    head.next = NULL;
    crnt      = &head;

    nrec = 0;
    while (rwfdrec(fp, &wfdisc) == 0) {
        new = (struct list *) malloc(sizeof(struct list));
        if (new == NULL) return -1;
        new->wfdisc = wfdisc;
        new->next   = NULL;
        crnt->next  = new;
        crnt        = crnt->next;
        ++nrec;
    }
    if (ferror(fp)) return -2;

/*  Copy from linked list into array  */

    array = (struct wfdisc *) malloc(nrec*sizeof(struct wfdisc));
    if (array == NULL) return -3;

    i = 0; crnt = head.next;
    while (crnt != NULL) {
        next = crnt->next;
        array[i++] = crnt->wfdisc;
        free(crnt);
        crnt = next;
    }
    if (i != nrec) return -4;

/*  Assign array to user provided pointer and return number of elements  */

    *output = array;
    return nrec;
}
