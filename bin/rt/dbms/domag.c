/*********************************************************************
 *
 *
 *  Author: Marina Harkins-Glushko
 *  	    UCSD, IGPP
 *	    glushko@ucsd.edu
 *******************************************************************/
#include "dbms.h"
  
int domag( float *data, 
           double ts,
	   double srate, 
           int nsamp, 
           int pos, 
           double calib, 
           Event *event )

{

   double logam, logd;
   float delta, max_val, min_val;
   int i, isave=1; 
          

   delta = 1.0/ srate; 
   max_val = -2147483647.0; min_val = 2147483647.0;
 
   for(i = 0; i <  nsamp; i++)  
        if( TRBADS4( (int) data[i] ) )  
            return 0;


/* Filter the data */
           
    setbfl_(&(flt.lf), &(flt.lo), &(flt.hf), &(flt.ho), &delta);
    inifil_( data);
    filrec_(&nsamp,  data, &isave,  data);

/* Integrate waveform  */

    vel2dis( data,  nsamp, srate, calib );

    for(i = pos; i < nsamp; i++)  {    
      if( TRBADS4( (int) data[i] ) ) continue;  
      if( data[i] > max_val) max_val = data[i];
      if( data[i] < min_val) min_val =  data[i];
    }

/* Find an amplitude  */
 
    event->amp = fabs((double) max_val) > fabs((double) min_val) ?
          fabs((double) max_val) : fabs((double) min_val);

    if( TRBADS4( (int) event->amp ) ) return 0;
    event->ms = log10( event->amp ) + 1.66 * log10(event->delta) + 2.0 ;

/*
fprintf( stderr, "%s  => %lf %lf ( %f %f )\n",   
event->sta, event->amp, event->ms, min_val, max_val);
*/
        return 1;
 
}
