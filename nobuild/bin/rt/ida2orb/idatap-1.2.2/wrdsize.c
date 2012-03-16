/* @(#)wrdsize.c	1.4 01/28/97 */
/*======================================================================
 *
 *  Returns wordsize in bytes for given datatype.  -1 means unrecognized
 *  datatype.
 *
 * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * Copyright (c) 1997 Regents of the University of California.
 * All rights reserved.
 *====================================================================*/
#include <stdio.h>
#include "cssio.h"

int cssio_wrdsize(datatype)
char *datatype;
{
    if (strcmp(datatype, "a0") == 0) {
        return 12;
    } else if (strcmp(datatype, "b0") == 0) {
        return 15;
    } else if (strcmp(datatype, "c0") == 0) {
        return 24;
    } else if (strcmp(datatype, "t4") == 0) {
        return 4;
    } else if (strcmp(datatype, "t8") == 0) {
        return 8;
    } else if (strcmp(datatype, "s4") == 0) {
        return 4;
    } else if (strcmp(datatype, "s2") == 0) {
        return 2;
    } else if (strcmp(datatype, "f4") == 0) {
        return 4;
    } else if (strcmp(datatype, "f8") == 0) {
        return 8;
    } else if (strcmp(datatype, "i4") == 0) {
        return 4;
    } else if (strcmp(datatype, "i2") == 0) {  /* not officially recognized */
        return 2;
    } else if (strcmp(datatype, "g2") == 0) {
        return 2;
    } else {
        return -1;
    }
}
