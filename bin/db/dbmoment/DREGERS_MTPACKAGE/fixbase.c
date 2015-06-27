#include <stdio.h>
#include <sys/file.h>

fixbase(x,len,scale)
  float *x,scale;
  int len;
  {
  float sum;
  int i,con2;

        con2 = (int)(scale*len);
	  if ( con2 > len)
	     {
	     fprintf(stderr,"Error in fixbase\n");
	     exit(-1);
	     }
        sum=0.0;
	for(i=0 ; i < con2 ; i++) /*Find baseline from data*/
	   sum += x[i];
        sum /= (float)con2;

	  for(i=0 ; i < len ; i++)/*Remove baseline*/
	     x[i] -= sum;


}
