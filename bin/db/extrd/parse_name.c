
/* $Name $Revision$ $Date$ */
/*======================================================================
 *
 *  /lib/util/parse_name.c
 *  
 *  bsname(), pathfrname(), fexten():
 *  Get base name, path, extention from full file name.
 *
 *====================================================================*/
#include <string.h>


void bsname(fname, name, exten)
char *fname;                 /* Full file name  */
char *name;                  /* Basename of the file  */
char *exten;                 /* extention to be cut from name */
{
int i, j;
char *tmp_name;       

        name[0] = '\0';
        tmp_name = (char *) malloc(132);
 
/* Get file name from full name     */

        for(j = 0,i = strlen(fname)-1; i >= 0; j++,i --)
             if (fname[i] == '/') break;
       
        strncpy(tmp_name, fname + i + 1, j);
        tmp_name[j] = '\0';
        strcpy(name, tmp_name);
        
/* Cut extention if such is specified */

        if(exten != NULL)  {
           tmp_name = strstr(name, exten);
           if(strcmp(tmp_name, exten) == 0)
              name[strlen(name)-strlen(exten)] = '\0';
        }

  free(tmp_name);       
}

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

void fexten(fname, name)
char *fname;                 /* Full file name  */
char *name;                  /* File name exention  */
{
int i;
 
 
        for(i = strlen(fname); i > 0; i --)
             if (fname[i] == '.') break;
        if( i == 0 )  {
           name[0] = '\0';
        }  else  {
           strncpy(name, fname + i + 1, strlen(fname) - i - 1);
           name[strlen(fname) - i - 1] = '\0';
        }
        
}
void namefrpath(path, name)
char *path;
char *name;
{
int i, j;
 
        for(j = 0,i = strlen(path)-1; i >= 0; j++,i --)
             if (path[i] == '/') break;
        strncpy(name, path + i + 1, j);
        name[j] = '\0';
}


/* $Id$ */
