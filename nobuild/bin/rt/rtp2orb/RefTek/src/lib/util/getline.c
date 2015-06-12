#pragma ident "$Id$"
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
 *====================================================================*/
#include "util.h"

UINT16 util_getline(
    FILE *fp,     /* input stream              */
    CHAR *buffer, /* buffer to hold line       */
    INT32 buflen,  /* length of buffer          */
    CHAR comment, /* comment character         */
    INT32 *lineno  /* current line number in fp */
) {
INT32 i;

    if (fp == (FILE *) NULL || buffer == (CHAR *) NULL || buflen < 2) {
        errno = EINVAL;
        return 3;
    }

    clearerr(fp);

    buffer[0] = 0;
    do {

        /*  Read the next line in the file  */

        if (fgets(buffer, buflen-1, fp) == NULL) {
            buffer[0] = 0;
            return feof(fp) ? 1 : 2;
        }
        if (lineno != NULL) ++*lineno;
        
        /*  Truncate everything after comment token  */

        if (comment != (char) 0) {
            i = 0;
            while (i < (INT32) strlen(buffer) && buffer[i++] != comment);
            buffer[--i] = 0;
        }

        /*  Remove trailing blanks  */

        i = strlen(buffer) - 1;
        while (i >= 0 && (buffer[i] == ' ' || buffer[i] == '\n')) --i;
        buffer[++i] = 0;
        
    } while (strlen(buffer) <= 0);

    return 0;
}

/* Revision History
 *
 * $Log$
 * Revision 1.1  2004/03/09 18:28:23  vernon
 * Initial revision
 *
 * Revision 1.2  2002/01/18 17:51:44  nobody
 * replaced WORD, BYTE, LONG, etc macros with size specific equivalents
 *
 * Revision 1.1.1.1  2000/06/22 19:13:09  nobody
 * Import existing sources into CVS
 *
 */
