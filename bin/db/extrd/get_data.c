/*
 *
 *  extrd/get_data.c
 *
 *  Read wfdisc file; for each station/channel extract specified 
 *  number of data points.
 *
 *
 *************************************************************************/
#include "extrd.h"

int get_data( double stime, double etime )
{
    SegData segment, new;
    double ts, te, et ;
    double crnt_time;
    float *buf=0;
    int skip, npts; 
    int done, bytes, i;
    int save_npts, extra_npts;
    int nrec;
    int saved = 0;
    char inrec[512];
    int First = 1;

    Foff = 0; 
    dbquery (db, dbRECORD_COUNT, &nrec);
    
    if (nrec < 1)   {
        complain (0, "No records selected\n" ) ;
        return 0;
    } 

    crnt_time = stime; 
    segment.steim = 0;
    mydata.c = 0;

    for( db.record = 0; db.record < nrec; db.record++)   {
       if( dbget( db, inrec) == dbINVALID )
           die( 0, "dbget faild for record #%d\n", db.record );

       if( (dbgetv( db, 0,
	  "time", &(new.time),
          "endtime", &(new.endtime),
          "calib", &(new.calib),
          "calper", &(new.calper),
          "samprate", &(new.samprate),
          "nsamp", &(new.nsamp),
          "sta", new.sta,
          "chan", new.chan,
          "datatype", new.datatype,
          "instype", new.instype,
          "segtype", new.segtype,
           0 )) == dbINVALID )
           die( 0, "dbgetv faild for record #%d\n", db.record );

#ifdef DEBUG
fprintf( stderr, "%s_%s: %lf %lf %d \n", 
new.sta, new.chan, new.time, new.endtime, new.nsamp );
#endif

       if( stime >= new.endtime ) continue;
       if( etime <= new.time ) continue;
       new.dcode = trdatacode ( new.datatype);
       done = skip = 0;
       if( segment.time <= 0.0 ) segment.time = new.time;
       if(strcmp(new.sta, segment.sta)!= 0 || 
          strcmp(new.chan, segment.chan) != 0) { 
           if( First ) segment.sbytes = 0;
           else if ( saved )  flush_db( &segment );

           saved = 0;
	   update_segdata( &segment, &new );
           crnt_time = stime;          
           if( new.endtime <= crnt_time ) continue;
       } else  {

         if( new.endtime <= crnt_time ) continue;
         check_param( &segment, &new );
       }
      
       skip = TIME2SAMP( new.time, new.samprate, crnt_time );
       if( skip >= new.nsamp) continue;

       while( !done )  {
	   if( skip < 0 ) skip = 0;
           if( new.nsamp-skip > MAX_NSAMP ) {
               new.nsamp -= MAX_NSAMP;
               te = ENDTIME( crnt_time, new.samprate, MAX_NSAMP);  
               et = MIN(etime, te );
           }  else {
               et = MIN(etime, new.endtime);
               done = 1;
           }
   
           if( buf != 0 && *buf != 0 ){
	      free( buf );
	   }
           buf = trgetwf(db, 0, 0, 0, crnt_time, et, 
                     &ts, &te, &npts, 0, 0) ; 
           if( npts <= 0 )  {
              buf = 0;
              continue;
           }



#ifdef DEBUG
fprintf( stderr, "ET: %lf %lf - %lf %lf ( %lf %lf ) %d \n", 
crnt_time, et, ts, te, stime, etime, npts );
#endif


           if( buf == 0 )  { 
              complain( 0, "can't get data for %s_%s from %lf to %lf\n", 
                        new.sta, new.chan, crnt_time, et ); 
              continue;
           }
           if( te <= crnt_time ) continue;
           if( ( skip = TIME2SAMP( ts, new.samprate, crnt_time ) ) > 0 )  {

                 complain( 0, "overlapped traces %s_%s: last time = %lf\tnew_t0=%lf new_te=%lf\n", 
                           new.sta, new.chan, crnt_time, ts, te );
                 complain( 0, "will skip %d points\n", skip);

           } else  skip = 0;

           if( ( extra_npts = ( te - et )*new.samprate-1) < 1 )
	      extra_npts = 0;
	   else te = et;

           save_npts = npts - extra_npts - skip ;
	  


#ifdef DEBUG
fprintf( stderr, "%d - %d - %d = %d\n", npts, skip, extra_npts, save_npts );
#endif

 
           if( fabs( ts - crnt_time ) > 1.0/new.samprate )  {

#ifdef DEBUG
	      complain( 0, "problem get data. Ask %lf -  got %lf\n", 
                            crnt_time, ts );
#endif

              crnt_time = ts;
	   }

           if( segment.time <= 0.0 ) {
	       segment.time = crnt_time;
	       segment.etime = etime;
	   }

	   First = 0;
	   crnt_time = te;
	   segment.endtime = crnt_time - 1.0 /segment.samprate;
	 
	   bytes = save_npts * 4; if ( bytes == 0 ) continue;
           if( mydata.c == 0 ) {
		 allot (char *, mydata.c, bytes);
	   }  else 
		 reallot (char *, mydata.c, bytes);
           for (i = 0; i < save_npts; i++)
	       mydata.i[i] = buf[i+skip];
								   
           if(!wrt_data( &segment, crnt_time, save_npts, mydata.i ))   {
               complain(0, "can't save data for %s_%s at %lf.\n",
                  segment.sta, segment.chan, crnt_time);
               fflush(Df);
               fclose(Df);
               if( mydata.c != 0 ) free( mydata.c);
	       return 0;
           } else saved = 1;


#ifdef DEBUG
fprintf( stderr, "ST:%lf ET:%lf NSMP:%d\n", 
segment.time, segment.endtime, segment.nsamp );
#endif


           if( etime <= crnt_time ) done = 1;
      }  
  }
  if( segment.dcode == trSEED ) flush_db( &segment );

  if( mydata.c != 0 ) free( mydata.c);
  if( buf != 0 ) free( buf);
  return 1;
}
