/* @(#)parse.c	2.3 01/28/97 */
/*======================================================================
 *
 *  parse.c
 *
 *  sparse: parse a string (original version)
 *  parse:  parse a string (new version with quote support)
 *
 * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * Copyright (c) 1997 Regents of the University of California.
 * All rights reserved.
 *====================================================================*/
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include "util.h"

int util_sparse(
	char *input,
	char *argv[],
	char *delimiters,
	int  max_tokens
) {
int i = 0;

    if (max_tokens < 1) {
        fprintf(stderr,"sparse: illegal 'max_tokens'\n");
        return -1;
    }

    i = 0;
    if ((argv[i] = strtok(input, delimiters)) == NULL) return 0;
    for (i = 1; i < max_tokens; i++) {
        if ((argv[i] = strtok(NULL, delimiters)) == NULL) return i;
    }

    return i;
}

int util_parse(
	char *input,     /* input string                */
	char **argv,     /* output array to hold tokens */
	char *delimiters,/* token delimiters            */
	int  max_tokens, /* max number of tokens (number of elements in argv) */
	char quote       /* quote character for strings */
){
char *ptr;
int i = 0, nquote = 0;

    if (max_tokens < 1) {
        errno = EINVAL;
        return -1;
    }

/* Save embedded blanks inside quoted strings */

    if (quote != 0) {
        for (ptr = input; *ptr != (char) 0; ptr++) {
            if (*ptr == quote) {
                if (++nquote == 2) nquote = 0;
            } else {
                if (nquote == 1 && *ptr == ' ') *ptr = (char) -1;
            }
        }
    }

/* Parse the string, restoring blanks if required */

    if ((argv[0] = strtok(input, delimiters)) == NULL) return 0;
    
    i = 1;
    do {
        if ((argv[i] = strtok(NULL, delimiters)) != NULL && quote != 0) {
            for (ptr = argv[i]; *ptr != (char) 0; ptr++) {
                if (*ptr == (char) -1) *ptr = ' ';
            }
        }
    } while (argv[i] != NULL && ++i < max_tokens);

/* Return the number of tokens */

    return i;
}

#define MAX_INPUT (1024)

int parse(fp, argv, delimiters, max_tokens)
FILE *fp;
char *argv[];
char *delimiters;
int  max_tokens;
{
static char input[MAX_INPUT];
int i = 0;

    if (max_tokens < 1) {
        fprintf(stderr,"parse: illegal 'max_tokens'\n");
        return -1;
    }

    if (fgets(input, MAX_INPUT-1, fp) == NULL) {
        if (feof(fp)) return -1;
        perror("lib_src/util/parse");
        return -1;
    }

    i = 0;
    if ((argv[i] = strtok(input, delimiters)) == NULL) return 0;
    for (i = 1; i < max_tokens; i++) {
        if ((argv[i] = strtok(NULL, delimiters)) == NULL) return i;
    }

    return i;
}
