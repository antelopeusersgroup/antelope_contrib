/* $Name $Revision$ $Date$  */ 
/**************************************************************************
 *
 * 
 *      extrd/get_data.c
 * Get portion of data from current data file.
 *
 *
 *************************************************************************/
#include <math.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <fcntl.h>
#include "util.h"
#include "wfdiscio.h"


#define CLEAN "rm -rf .all.wfdisc"
#define MIN(a,b)  ( (a) < (b) ? (a) : (b) )
#define MAX_NUM  (500000)

/* Global parameters  */

extern int Factor;

int get_data(wfd, path, etime, extr_pnts, crnt_time, fp)
struct wfdisc *wfd;
char *path;
double etime;
long *extr_pnts;
double *crnt_time;
int fp;

{
     char *buffer;
     char *ifname;
     char tmp_path[256];
     double wfd_time;
     long skip_pnts, tmp_pnts, pnts;
     long bytes, wrt_pnts;
     int fd;

     wfd_time = 0.0; 

/* If  specified "start time" for new set of data less then start time 
   in first wfdisc file then take "wfdisc start time" for beginning, 
   otherwise skip number of points for specified "start time"  
*/
     if(*crnt_time > etime) return FALSE;
     if(*crnt_time > wfd->time) 
        skip_pnts = floor( (*crnt_time-wfd->time)*(double) wfd->samprate + 0.5);
      else  skip_pnts = 0;

     if(skip_pnts >= wfd->nsamp) return FALSE;
     else  {
        if(skip_pnts != 0)
           *crnt_time = (wfd->time + (double) (skip_pnts) /(double)wfd->samprate);

        else if( wfd->time > *crnt_time ) *crnt_time = wfd->time; 
 
       if(*crnt_time > etime) return FALSE;
        pnts =  (etime - *crnt_time)*(double) wfd->samprate;
        *extr_pnts =  (long) MIN(pnts, (wfd->nsamp-skip_pnts) );
        pnts = *extr_pnts;
        if(pnts == 0) return FALSE;
       
        if( (buffer = (char *) malloc(132) ) == NULL)  {
           printf("extrd/get_data(): malloc error\n");
           perror(buffer);
           system(CLEAN);
           exit(1);
        }
        if( (ifname = (char *) malloc(132) ) == NULL)  {
           printf("extrd/get_data(): malloc error\n");
           perror(ifname);
           system(CLEAN);
           exit(1);           
        }

        if(wfd->dir[0] == '/')  {  
           sprintf(tmp_path,"%s\0", wfd->dir);
           Find_path(tmp_path, &ifname);
           strcat(ifname, wfd->dfile);
        } else if(strlen(path) > 0)  {
              sprintf(tmp_path,"%s/%s\0", path, wfd->dir);
              Find_path(tmp_path, &ifname);
              strcat(ifname, wfd->dfile);
           } else { 
              sprintf(tmp_path,"%s\0", wfd->dir);
              Find_path(tmp_path, &ifname);
              strcat(ifname, wfd->dfile);
              sprintf(tmp_path,"%s\0", wfd->dir);
          }
        if ((fd = open(ifname,O_RDONLY) ) <= 0)  {
           fprintf(stderr,"extrd/get_data():Can't open file %s \n", ifname);
           perror(ifname);
           system(CLEAN);
           exit(1);           
        }
        skip_pnts = skip_pnts*Factor + wfd->foff;
        if(lseek(fd, skip_pnts, SEEK_SET) < 0)  {
               printf("extrd/get_data():Can't skip data from file %s\n", ifname);
               perror("extrd/get_data():"); 
           system(CLEAN);
               exit(1);           
        } 

/* Extract specified number of the data points or the rest data points 
   from current data file  */
        while(pnts > 0)  {
           tmp_pnts = MIN(pnts,MAX_NUM);
           pnts = pnts - tmp_pnts;
           buffer = (char *) realloc((void *) buffer, (size_t) tmp_pnts*Factor);
           if(buffer == NULL )  {
              printf("extrd/get_data(): realloc error\n");
              perror(buffer);
              system(CLEAN);
              exit(1);           
           }
           bytes = read(fd, buffer, tmp_pnts*Factor);
           if (bytes != tmp_pnts*Factor)  {
              printf("extrd/get_data():Can't extract data from file %s\n", ifname);
              perror("extrd/get_data():"); 
              system(CLEAN);
              exit(1);           
           } else  {
               wrt_pnts = write(fp, buffer, tmp_pnts*Factor);
               if (wrt_pnts != tmp_pnts*Factor)  {
                   printf("extrd/get_data():write error for sta:%s chan:%s \n", wfd->sta, wfd->chan);
                   printf("write %ld instead of %ld \n", wrt_pnts, tmp_pnts*Factor);
                   perror("extrd/get_data():"); 
                    system(CLEAN);
                   exit(1);           
               } 
          }
        }
        close(fd); 
     }
     free(ifname); free(buffer);
  return TRUE;
}


/* $Id$ */
