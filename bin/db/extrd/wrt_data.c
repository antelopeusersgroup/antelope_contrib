/**************************************************************************
 *
 * Get portion of data from current data file.
 *
 *
 *************************************************************************/
#include "extrd.h"

extern int save_seed();

int wrt_data( 
    SegData *segment,
    double dtime,
    long npts,
    int *data
)
{
     long wrt_pnts = 0;
     int code;
     
             
     if( segment->dcode == trSEED )  {
         if( (code = csteim ( segment->steim, save_seed, data, npts) )  < 0 ) 
              elog_die( 1, " steim compression error for %s_%s at %lf\n", 
                       segment->sta, segment->chan, dtime ) ;
        else if ( code > 0 ) 
            elog_complain( 1, " steim compression problems for %s_%s at %lf\n", 
                       segment->sta, segment->chan, dtime ) ;

     }  else  {
         wrt_pnts = fwrite( data, sizeof(int), npts,  Df );
         if (wrt_pnts != npts)  {
               elog_complain( 1, "write error for %s_%s at %lf \n", 
                         segment->sta, segment->chan, dtime);
               elog_complain(  0, "write %ld samples insted of %ld \n", 
                          wrt_pnts, npts);
               return 0;
         }
         segment->nsamp += wrt_pnts;
	 fflush( Df );
         Foff += wrt_pnts * sizeof( int );
         flush_db( segment );
     }

  return 1;
}

