/*
 *
*/
#include "dbtc.h"
 
int get_data( Dset *set, Param *par )
{
    Dbptr db;
    double ts, te ;
    double stime, endtime, calib;
    int nsamp, npts;
    float *buf=0;
    char sta[8];
 
 
    db = set->db;
 
    for( db.record = 0 ; db.record < set->dbrec; db.record++)   {
 
       if( (dbgetv( db, 0,
          "time", &stime,
          "endtime", &endtime,
          "calib", &calib,
          "samprate", &(set->srate),
          "nsamp", &nsamp,
          "sta", sta, 0 )) == dbINVALID )
           elog_die( 0, "dbgetv faild for record #%d\n", db.record );
 
/*
fprintf( stderr, "%s: %lf %lf %d %lf \n", sta, stime, endtime, nsamp,set->stime );
*/
       if( strncmp( set->sta, sta, strlen(sta) )) continue;
       if( set->stime <= 0.0 )  {
          set->stime += stime;
          set->etime = stime + par->datseg;
       }
 
       if( stime > set->etime )   {
           elog_complain( 0, 
	      "can't get data for %s %lf - %lf; first sample is at %lf.\n", 
               set->key, set->stime, set->etime, stime );
           return 0;
       }
       if( endtime <= set->stime ) continue;
     
       if( buf != 0 && *buf != 0 ) free( buf );
       buf = trgetwf( db, 0, 0, 0, set->stime, set->etime, &ts, &te, &npts, 0, 0) ; 
       if( buf == 0 || npts <= 0 )  {
           elog_complain( 0, "can't get data for %s from %lf to %lf\n", 
                        set->key, set->stime, set->etime ); 
           buf = 0;
           continue;
       }
/*
fprintf( stderr, "%lf %lf - %lf %lf %d \n", 
         set->stime, set->etime, ts, te, npts );
*/ 

       if( te <= set->stime ) continue;
       if( ts > set->etime ) {
           elog_complain( 0, "returned time is more than requested etime. %lf > %lf\n",
                     ts, set->etime );
           return 0;
       }  else {
 
           set->stime = ts;
           set->etime = te;
           set->data = buf;
           set->npts = npts;
           return 1;
       }
  }
  return -1;
 
}

