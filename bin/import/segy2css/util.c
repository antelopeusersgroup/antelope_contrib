/* $Name $Revision$ $Date$  */
/*********************************************************************
 *
 *  pathfrname.c
 *
 *  Extract path name from full path.
 *
 **********************************************************************/
#include <stdio.h>
#include <ctype.h>
#include "segcss.h"
#include "wfdiscio.h"


void pathfrname(path, name)
char *path;
char *name;
{
int i;

        for(i = strlen(path)-1; i >= 0; i --)
             if (path[i] == '/') break;
        if(i < 0) *name = NULL;
        else  {
             strncpy(name, path, i);
             name[i] = '\0';
        }
}

/***********************************************************************
 *
 *  ucase.c
 *
 *  Upper case a string
 *
 **********************************************************************/

char *ucase(string)
char *string;
{
int i;

    for (i = 0; i < strlen(string); i++) 
        if (islower(string[i])) string[i] = toupper(string[i]);

    return string;

}

/* $Id$ */
