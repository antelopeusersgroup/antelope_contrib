/******************************************************************
 *
 *  dbms/util.c
 *
 *
 *  Author: Marina Harkins-Glushko
 *          UCSD, IGPP
 *          glushko@ucsd.edu
 *
 *******************************************************************/
#include "dbms.h"


void usage ()
{
    fprintf (stderr, 
            "Usage: %s [-a auth] [-f filter] [-n net] [-o orid] [-t twin] [-v]  dbin \n", 
            Program_Name);
            exit (1);
}
 

Event *new_ev( ) { 

    Event *new;
    allot( Event *, new, 1 );

    return new;
}

EvRec *new_id( int record, int id )
{
   EvRec *new;
   allot( EvRec *, new, 1 );

   new->record = record;
   new->evid =  id;
   return new;
}
 
void vel2dis( float *data, long nsamp, double smprate, double calib )

{
    double  delta;
    double  val0,
            val1,
            sum=0.0; 
    int     i; 

 
    delta = 1.0/(2.0*smprate); 
    
    val0= data[0] * calib / 1000.0;
    data[0] = 0.0;

    for ( i = 1; i < nsamp; i++) {
       val1 = data[i] * calib / 1000.0 ; 
       sum += ( val0 + val1 ) * delta ;
       data[i] = sum ; 
       val0 = val1 ; 
    }
    
}

