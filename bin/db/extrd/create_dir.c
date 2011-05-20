/*********************************************************************
 *
 *
 *   lib/create_dir
 *
 *   Try to create dir spcified by path.
 *
 *
 ********************************************************************/
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>

#include "stock.h"
 
#define DELIMITERS  "/"
#define NTOKEN  (20)   /* Max number of the subdirectories. Hope it's enough */

extern  char *getcwd(); 
int create_dir(fname)
char *fname;

{
struct stat buf;
char *tmp[NTOKEN];
char *real_path;
char *subpath;
char *link_path;
char *tmp_str;
char *home, *new_home;
int ntoken, link_size, i;
 
  ntoken = 0;
  subpath = (char *) malloc(256);
  link_path = (char *) malloc(256);
  tmp_str = (char *) malloc(256);
  real_path = (char *) malloc(256);

  strcpy(tmp_str, fname);

/* Get all subdirectories  */
 
  tmp[ntoken++] = strtok(tmp_str,DELIMITERS);
  while ((tmp[ntoken] = strtok(NULL,DELIMITERS)) != NULL)
           ntoken++;
 
  if(fname[0] == '/')  {
      strcpy(subpath, "/");
  }  else {
      strcpy(subpath, "");
  } 

/* Stat each subdirectory; if doesn't exist try to create  */
 
  for(i = 0; i < ntoken; i++)  {
    strcat(subpath, tmp[i]);
    if(stat(subpath, &buf) != 0)  {
        if(ENOENT)  {	/* Doesn't exist; create */
          if(mkdir(subpath, 0775) == -1)  {
             elog_complain(1, "Can't create directory %s\n", subpath);
             return -1;
          }
        }  else  {
           elog_complain( 1, "create_dir():can't stat. \n");
           return -1;
       }
    } else if(S_ISLNK(buf.st_mode) != 0)  {	/* It's link; find the real path  */
       link_size = readlink(subpath, link_path, 512);
       if(link_size < 0)  {
           elog_complain(1, "read link error %s\n", subpath);
           return -1;
       }
       link_path[link_size] = '\0';
       if(link_path[0] == '/') { 
          sprintf(real_path, "%s/\0", link_path); 
          strcpy(subpath, real_path);
       } else {
          strncpy(real_path, subpath, strlen(subpath) - strlen(tmp[i]));
          strcat(real_path, link_path);
          strcat(real_path,"/\0");
          strcpy(subpath, real_path);
       }
   } else if(S_ISDIR(buf.st_mode) != 0)  {   /* It's directory  */
      strcat(subpath,"/");
      strcpy(real_path, subpath);
   } else if(S_ISREG(buf.st_mode) != 0)  {  /* Regular file */
      elog_complain( 0, "%s is a REGULAR file and already exist!\n", subpath);
            return 0;
   } else if(S_ISCHR(buf.st_mode))  {
      elog_complain( 0, " %s is a CHR device!\n", subpath);
            return 0;
      
 
   }  else {
       elog_complain( 0,"%s is not a path to the data file\n", subpath);
       free(tmp_str); free(subpath); free(link_path); free(real_path);
       return -1;
   }
}
/*  get absolute path to the data file(s)  */
 
    if ((home = getcwd((char *)NULL, 256)) == NULL) {
        elog_complain(1, " getcwd failed\n");
        return 0;
    }
    if(chdir(subpath) != 0) {
       elog_complain(1," chdir error:%s \n", subpath );
        return 0;
    }
    if ((new_home = getcwd((char *)NULL, 256)) == NULL) {
        elog_complain(1, "getcwd error\n");
        return 0;
    }
    if(chdir(home) != 0) {
       elog_complain(1," chdir error:%s \n", home );
        return 0;
    }


strcpy(fname, new_home);    
free(tmp_str); free(subpath); free(link_path); free(real_path);
return 1;
}
