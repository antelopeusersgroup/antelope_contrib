/*
 *
 *  extrd/get_data.c
 *
 *  Read wfdisc file; for each station/channel extract specified 
 *  number of data points.
 *
 *
 *************************************************************************/
#include "dbms.h"

int proc_sta( Dbptr db, int nrec, Event *event )
{
    double ts, te, et ;
    double stime, endtime, calib;
    double samprate;
    int pos, retcode;
    int nsamp, arid, npts;
    float *buf=0;
    char sta[8], key[64];



    for( db.record = 0 ; db.record < nrec; db.record++)   {

       if( (dbgetv( db, 0,
	  "time", &stime,
          "endtime", &endtime,
          "calib", &calib,
          "samprate", &samprate,
          "arid", &arid,
          "nsamp", &nsamp,
          "sta", sta,
           0 )) == dbINVALID )
           elog_die( 0, "dbgetv faild for record #%d\n", db.record );

/*
fprintf( stderr, "%s: %lf %lf %d %lf \n", sta, stime, endtime, nsamp,event->otime );
*/
       if( strncmp( event->sta, sta, strlen(sta) )) continue;

       if( stime >= event->etime )   {
           elog_complain( 0, "can't get data for %s %lf - %lf.\n", 
                         sta, event->otime, event->etime);
           return 0;
       }
       if( endtime <= event->stime ) continue;
       if( arid != event->arid ) continue;
      
       trgetwf(
           db, 0, &buf, 0, event->otime, event->etime, &ts, &te, &npts, 0, 0) ; 
       if( npts <= 0 )  {
           if(buf != 0) free(buf);
	   buf = 0;
           continue;
       }

/*
fprintf( stderr, "%lf %lf - %lf %lf %d \n", 
         event->otime, event->etime, ts, te, npts );

*/
       if( buf == 0 )  { 
           elog_complain( 0, "can't get data for %s from %lf to %lf\n", 
                        event->sta, event->otime, event->etime ); 
           continue;
       }
       if( te <= event->otime ) continue;
       if( ts > event->stime ) {
           elog_complain( 0, "trace start time is more than SWA time. %lf > %lf\n",
                     ts, event->stime );
           if(buf) {
	    	free(buf);
		buf = 0;
	   }
	   return 0;
       }  else {
           if( ( pos = (int) ( event->stime - ts ) * samprate) >= npts ) {
               elog_complain( 0,  
               "no data for %s from %lf to %lf.\n", sta, event->stime, event->etime ); 
           	if(buf) {
	 	   free(buf);
		   buf = 0;
		}
                continue;
           }

           if( !(retcode = domag( buf, ts, samprate, npts, pos, calib, event)) ) 
               elog_complain( 0,  
               "can't calculate MS for %s. ( no data or too many FULL scale values).\n", sta); 
           if( buf != 0 && *buf != 0 ) free( buf );
           return retcode;
       }
  }

}
