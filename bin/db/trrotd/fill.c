/*  myfill  fills in gaps as called by trsplice */
#include <stdio.h>
#include <math.h>
#include "db.h"
#include "stock.h"
#include "tr.h"

int
fill ( Dbptr *tr, Trsample *data, int *i0p, int *i1p, int *imaxp)
{
   int i, n, i0, i1, imax;
   float fval;

   i0 = *i0p;
   i1 = *i1p;
   imax = *imaxp;
   /* set default value to average of data pts on either side of gap */
   if (i0 <= 0) {
     fval = data[i1];
   } else if (i1 >= imax) {
     fval = data[i0-1];
   } else {
     fval = 0.5*(data[i0-1] + data[i1]);
   }
   printf("trsplice/fill: i0 %d i1 %d imax %d fval %.1f\n",i0,i1,imax,fval); 

   for (i=i0; i<i1; i++ ) {
      data[i] = fval;
   }
   return 0;
}	
