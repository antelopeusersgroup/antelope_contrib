/* @(#)string.c	2.2 4/4/96 */
/*======================================================================
 *
 *  Misc. string operations.
 *
 *  util_strpad:
 *  Replace all characters from current end-of-string to specified
 *  length with constant character.  User is responsible for memory.
 *
 *  Pointer to begining of padded string is returned.
 *
 *----------------------------------------------------------------------
 *
 *  util_strtrm:
 *  Truncate a string, removing trailing blanks.  Truncated string
 *  is returned.
 *
 *====================================================================*/
#include <stdio.h>
#include <string.h>
#include "util.h"

char *util_strpad(input, maxlen, padchar)
char *input;
int  maxlen;
char padchar;
{
int i;

    if (strlen(input) == maxlen) return input;
    for (i = strlen(input); i < maxlen-1; i++) input[i] = padchar;
    input[maxlen-1] = 0;

    return input;

}

char *util_strtrm(input)
char *input;
{
int n;

    n = strlen(input) - 1;
    while (n >= 0 && input[n] == ' ') --n;
    input[++n] = 0;

    return input;

}
