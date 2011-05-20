/* %W% %G% */
/*======================================================================
 *
 *  lib_src/util/Find_path.c
 *
 *  Find a real path, take care of the soft links and automount points.
 *
 *====================================================================*/
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>
#include <stdio.h>

#include "stock.h"
 
#define DELIMITERS  "/"
#define NTOKEN  (20)   /* Max number of the subdirectories. Hope it's enough */
 
int Find_path(path, real_path)
char *path;
char **real_path;
 
{
struct stat buf;
char *tmp[NTOKEN];
char *orig_path;
char *subpath;
char *link_path;
int ntoken, link_size, size, i;
 
  ntoken = 0; size = 256;
  subpath = (char *) malloc(256);
  link_path = (char *) malloc(256);
  orig_path = (char *) malloc(256);
  
  strcpy(orig_path, path);
  
  tmp[ntoken++] = strtok(orig_path,DELIMITERS);
  while ((tmp[ntoken] = strtok(NULL,DELIMITERS)) != NULL)
           ntoken++;
  if(path[0] == '/')  {
      strcpy(subpath, "/");
      strcpy(*real_path, "/");
  }  else {
      strcpy(subpath, "");
      strcpy(*real_path, "");
  } 
  for(i = 0; i < ntoken; i++)  {
    strcat(subpath, tmp[i]);
    if(lstat(subpath, &buf) != 0)  {
         elog_complain(1,"Find_path:can't stat. \n");
         return -1;
    } else if(S_ISLNK(buf.st_mode) != 0)  {
       link_size = readlink(subpath, (void *)link_path, size);
       if(link_size < 0)  {
           perror(subpath);
           return 0;
       }
       link_path[link_size] = '\0';
       if(link_path[0] == '/') { 
          strcpy(*real_path, link_path);
          strcat(*real_path,"/\0");
          strcpy(subpath, *real_path);
       } else {
          strncpy(*real_path, subpath, strlen(subpath) - strlen(tmp[i]));
          strcat(*real_path, link_path);
          strcat(*real_path,"/\0");
          strcpy(subpath, *real_path);
       }
   } else if(S_ISDIR(buf.st_mode) != 0)  {
      strcat(subpath,"/");
      strcpy(*real_path, subpath);
   } else {
       elog_complain( 0, "%s is not a path to the data file\n", subpath);
       free(subpath); free(link_path); free(orig_path);
       return 0;
   }
}
    
free(subpath); free(link_path); free(orig_path);
 
return 1;
}

