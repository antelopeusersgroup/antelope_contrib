/* @(#)css2io.c 8/20/91 */
#include <stdio.h>
css2io_(list,rlx,rhx,filnam,nrecx)
/*Fortran callable routine for reading and writing short integers on disk */

/*Identical to shrtio in the nio library except that files are specified
by name rather than number*/

char filnam[80];
short list[];
long *rlx,*rhx;
int *nrecx;
{
   int ity,iloc,lun,nb,nbr,nrec;
   long lastp,rl,rh,sp;
   int i;
   
   rl = *rlx;
   rh = *rhx;
   nrec = *nrecx;

   for(i =0; i < 80; i++) 
     if((filnam[i] == ' ') || (filnam[i] == '\0') || (filnam[i] == '\n') || (filnam[i] == '\t') ) break;
   filnam[i] = '\0';

   lun = getfdcss(filnam,&nrec);
   /* find place to start, and seek to there from start of file */
   sp = 2*(rl-1);       /*start byte in short integer file*/
   nb = 2*(rh-rl+1);            /*number of bytes to read*/
   if (lseek(lun,sp,0) == -1)
        perror("css2io lseek");

   if(nrec!=0){
      nbr = write(lun,list,nb);         /*write short integers*/
      if(nb!=nbr){
         fprintf(stderr,"Error in css2io in writing to file %s\n",filnam);
         fprintf(stderr,"Terms to be written were %ld through %ld, or %d bytes.\n",rl,rh,nb);
         fprintf(stderr,"%d bytes actually written. Program aborts.\n",nbr);
         exit(15);
      }
   }
   else {
      nbr = read(lun,list,nb);          /*read short integers*/
      if(nb!=nbr){
         lastp = (sp+nbr)/2;
         ioredercss(filnam,rl,rh,nbr,lastp);
         /*exit(15);*/
      }
   }
   close(lun);
   return;
}



/* $Id$ */
