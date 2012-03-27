/* @(#)lenprt.c	2.2 07/05/96 */
/*======================================================================
 *
 *  lenprt.c
 *
 *  Print ascii text with configured line length.
 *
 *====================================================================*/
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include "util.h"

int util_lenprt(
	FILE *fp,
	char *text,
	int len,
	char cont
) {
int i, count, status = 0;

    if (cont) --len;

    if (fp == NULL) {
        status = -1;
    } else if (text == NULL) {
        status = -2;
    } else if (len < 1) {
        status = -3;
    } else if (cont != 0 && !isprint(cont)) {
        status = -4;
    }

    if (status) {
        errno = EINVAL;
        return status;
    }

    for (i = 0, count = 0; i < strlen(text); i++) {
        if (count && count % len == 0) {
            if (cont != 0) fprintf(fp, "%c", cont);
            fprintf(fp, "\n");
            count = 0;
        }
        fprintf(fp, "%c", text[i]);
        if (text[i] == '\n') {
            count = 0;
        } else {
            ++count;
        }
    }
    if (count != 0) fprintf(fp, "\n");
    fflush(fp);

    return ferror(fp) ? -5 : 0;
}

#ifdef DEBUG_TEST

#define BUFLEN 1024

main(argc, argv)
int argc;
char *argv[];
{
char buffer[BUFLEN];
int nbytes, status, linlen;
char cont = '\\';

    if (argc != 2) {
        fprintf(stderr, "usage: %s line_len < input\n", argv[0]);
        exit(1);
    }

    linlen = atoi(argv[1]);

    while (fgets(buffer, BUFLEN-1, stdin) != NULL) {
        status = util_lenprt(stdout, buffer, linlen, cont);
        if (status != 0) {
            perror("util_lenprt");
            exit(1);
        }
    }

    exit(0);
}

#endif /* DEBUG_TEST */
