
/* @(#)getfdcss.c       8/20/91 */
#define PM 0644         /* Mode r&w for owner, r for everyone else */
#include <stdio.h>
#include <fcntl.h>
#include <sys/errno.h>

/* For a file name, filna, getdfcss, returns the fd attached to it.*/

getfdcss(filnam,iact)

int *iact;
char filnam[80];
{
        int niact;
        int ity;

        niact=*iact;

        /*open for read only*/
        if (niact==0)
        {
         ity=open(filnam,O_RDONLY);
         if(ity==-1){
            perror(filnam);
            fprintf(stderr,"File %s could not be opened.\n",filnam);
            fprintf(stderr,"Read permission not available - program aborts...\n");
            exit(15);
         }
        }

        /*open for RW*/
        if (niact==1 || niact==-1)
        {
        ity=open(filnam,O_RDWR|O_CREAT|O_TRUNC,PM);
        if(ity==-1){
         fprintf(stderr,"Could not create file %s. Program aborts.\n",filnam);
         exit(15);
         }
        }

        return(ity);
}



/* $Id$ */
