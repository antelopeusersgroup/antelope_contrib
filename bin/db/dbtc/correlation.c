/*************************************************************************
 *
 *  Evaluate correlation value for data array, find MAX value,
 *  and calculate time offset for this value
 *
 ***********************************************************************/
#include <stdio.h>
#include "dbtc.h"  

#define TRBADS4(X)        ((X)>=2147470000||(X)<=-2147400000)
 
/**   Compute mean value   **/
 
int mean( float *data, int num, double *meanval )
 
{
   double val;
   int i;
 
 
   for(i = 0, val=0.0; i < num; i++)  {
        if( TRBADS4( (int) data[i] ) ) return 0; 
	val += (double) data[i];
   }
 
   *meanval = ( val / (double) num);
 
   return 1;
 
}
/**   Compute deviation  **/
 
 
int deviation( float *data, int num, double mean_val, double *dev )
{
   int i;
   double val, tmp;
 
   val = 0.0; tmp = 0.0;
 
     for(i = 0; i < num; i++)  {
        tmp = ((double) data[i]- mean_val );
        val += tmp*tmp;
     }
     
     *dev = sqrt(val);
 
     return 1;
 
 
}
 
int correlation( Dset *gset, Dset *bset, Param *par, double *tcorr )
 
{
   double val ;
   double *correl, chk=-1.0;
   double gmean, gdev, bmean, bdev;
   int i, j, ncorr;
   int pos;
   float *gdata, *bdata, *fptr;


/*  Calculate correlatin value for data array   */
 
   gdata = gset->data;
   bdata = bset->data;

   /* draw( gset->data, bdata, bset->data, gset->npts );  */
 
   allot( double *, correl, par->ncorr );
   val = 0.0;
   memset( correl, 0, par->ncorr*sizeof(double) );
   if( !mean( gdata, par->ncorr, &gmean ))  return 0;
   deviation( gdata, par->ncorr, gmean, &gdev );

   gset->mean = gmean;
   gset->dev = gdev;

/*
fprintf( stderr, "good mean:%lf dev:%lf %d\n", gset->mean, gset->dev, par->ncorr);
*/
 
   for( j = 0, ncorr = 0; j < par->ncorr; j++)  {
      if( bset->npts - j < par->ncorr ) break;
      fptr = (float *) ( bdata +j);
      
      if(!mean( fptr, par->ncorr, &bmean )) return 0;
      deviation( fptr, par->ncorr, bmean, &bdev );

      bset->mean = bmean;
      bset->dev = bdev;

/*
fprintf( stderr, "bad mean:%lf dev:%lf %d\n", bset->mean, bset->dev, par->ncorr);
*/

      for(i = 0, val=0.0; i < par->ncorr; i++)  {
         val += ((double) gdata[i] - gset->mean ) *
                 ((double) fptr[i] - bset->mean ); 
      }
      ncorr++;
      correl[j] =  val/(gset->dev*bset->dev) ;

/*
fprintf( stderr, "pos:%d val:%lf cor:%lf\n", j, val, correl[j] );
*/

   }

   if( ncorr == 0 ) return 0;

/*  Find MAX value  */
 
    for( i = 0, pos = 0; i < par->ncorr; i++)  {
/*  
fprintf( stderr, "%lf   ", correl[i]);  
*/
       if( fabs( correl[i]) > chk )  {
          chk = fabs( correl[i]);
          pos = i;
       }   
  }
 
/*  Calculate time offset time  */
 
  *tcorr = (double) pos/(double) gset->srate; 
 
/*
fprintf( stderr, "pos:%d cor:%lf timoff:%lf\n", pos, chk, tcorr );
fflush(stderr);
*/

  free(correl);
  return 1;
}
 

