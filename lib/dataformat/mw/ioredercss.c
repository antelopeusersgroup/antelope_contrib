#include <stdio.h>
ioredercss(fname,rl,rh,nbr,lastp)
/*  Prints out error message on read; if >0 bytes are read, gives the  */
/*  probable last term of the file, otherwise prints out most likely   */
/*  source of error */
int nbr;
char fname[80];
long rl,rh,lastp;
{
#ifndef ERRNO
   extern int errno;
   extern char *sys_errlist[];
#endif
   fprintf(stderr,"Error in reading from file %s\n",fname);
   fprintf(stderr,"Terms to be read were %ld through %ld.\n",rl,rh);
   if(nbr>0){
      fprintf(stderr,"Partial read - last point read was %d\n",lastp);
/*      fprintf(stderr,"Program aborts...\n");*/
   }
   else if (nbr==0)
	{
      fprintf(stderr,"Attempt to read after EOF. Program aborts...\n");
	exit(-15);
	}
   else {
#ifndef ERRNO
	{
      fprintf(stderr,"Bad read: ( %s ), program aborts...\n",sys_errlist[errno]);
	exit(-15);
	}
#else
	{
      perror("Bad read; program aborts");
	exit(-15);
	}
#endif
   }
}

/* $Id$ */
