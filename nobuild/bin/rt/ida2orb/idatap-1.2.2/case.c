/* @(#)case.c	2.1 10/04/95 */
/*======================================================================
 *
 *  util_lcase: lower case a string
 *  util_ucase: upper case a string
 *
 *====================================================================*/
#include <ctype.h>
#include <errno.h>
#include "util.h"

char *util_lcase(c)
char *c;
{
int i;

    if (c == NULL) {
        errno = EINVAL;
        return NULL;
    }

    for (i=0;i<strlen(c);i++) if (isupper(c[i])) c[i] = tolower(c[i]);
    return c;

}

char *util_ucase(c)
char *c;
{
int    i;

    if (c == NULL) {
        errno = EINVAL;
        return NULL;
    }

    for (i=0;i<strlen(c);i++) if (islower(c[i])) c[i] = toupper(c[i]);
    return c;

}
