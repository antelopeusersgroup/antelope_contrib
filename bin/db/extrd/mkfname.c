
/* $Name $Revision$ $Date$  */
/************************************************************************
 *
 *    extrd/exist.c
 * Make output directory with the name YYDDDHHMMSS                
 * 
 ************************************************************************/
#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <fcntl.h>
#include "util.h"
#include <errno.h>

#define MODE (0664)

/* Global variables  */

int Fp_data;
char *Data_file;
char *Wfd_name;


int mkfname(time, out_dir)
double time;
char *out_dir;
{

    struct stat buf;
    int yr,day, hour, min, sec, msec;

    if( (Data_file = (char *) malloc(512) ) == NULL )  {
          perror("extrd/mkfname(): malloc");
          exit(1);         
    }
   
    if( (Wfd_name = (char *) malloc(512) ) == NULL )  {
          perror("extrd/mkfname(): malloc");
          exit(1);         
    }

/* Make directory name for new data subset from data start time   */
   
   dtsplit(time, &yr, &day, &hour, &min, &sec, &msec); yr -= 1900;
   if(out_dir != NULL)
     sprintf(Data_file,"%s/%2.2d%3.3d%2.2d%2.2d%2.2d\0", out_dir, yr, day, hour, min, sec);
   else
     sprintf(Data_file,"%2.2d%3.3d%2.2d%2.2d%2.2d\0", yr, day, hour, min, sec);
  
/* Make new wfdisc name from data start time  */
 
   strcpy(Wfd_name, Data_file);
   strcat(Wfd_name, ".wfdisc");
   strcat(Data_file, ".w");
 
/*  Make directory for data subset from start time  */
     
       if(stat(Data_file, &buf) != 0)  {
           if (!ENOENT)  {
               fprintf(stderr,"extrd():");
               perror(Data_file);
               return 0;
            }  
       } else  {
           fprintf(stderr, "File %s already exist\n", Data_file);
           fprintf(stderr, "Can't overwrite existing file.\n");
           return 0;
      }
      if ((Fp_data = open(Data_file, O_CREAT | O_WRONLY, MODE)) <= 0 )  {
            fprintf(stderr,"extract_data():Can't open file %s\n", Data_file);
            perror(Data_file);
            return 0;
      }

   return 1;    
}

/* $Id$ */
