/* @(#)getline.c	2.4 01/28/97 */
/*======================================================================
 *
 *  Read a single line from the given file, stripping out comments and
 *  blank lines.
 *
 *  The processed line will be a NULL terminated string and without
 *  the trailing newline.
 *
 *  Return values: 0 => success
 *                 1 => EOF
 *                 2 => read error
 *                 3 => other error
 *
 * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * Copyright (c) 1997 Regents of the University of California.
 * All rights reserved.
 *====================================================================*/
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include "util.h"

int util_getline(
	FILE *fp,     /* input stream              */
	char *buffer, /* buffer to hold line       */
	int  buflen,  /* length of buffer          */
	char comment, /* comment character         */
	int  *lineno  /* current line number in fp */
) {
int i;

    if (fp == NULL || buffer == NULL || buflen < 2) {
        errno = EINVAL;
        return -1;
    }

    clearerr(fp);

    buffer[0] = 0;
    do {

        /*  Read the next line in the file  */

        if (fgets(buffer, buflen-1, fp) == NULL) {
            buffer[0] = 0;
            return feof(fp) ? 1 : -1;
        }
        if (lineno != NULL) ++*lineno;
        
        /*  Truncate everything after comment token  */

        if (comment != (char) 0) {
            i = 0;
            while (i < strlen(buffer) && buffer[i++] != comment);
            buffer[--i] = 0;
        }

        /*  Remove trailing blanks  */

        i = strlen(buffer) - 1;
        while (i >= 0 && (buffer[i] == ' ' || buffer[i] == '\n')) --i;
        buffer[++i] = 0;
        
    } while (strlen(buffer) <= 0);

    return 0;
}
