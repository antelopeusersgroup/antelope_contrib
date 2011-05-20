/*********************************************************************
 *
 *    parse packet; get time&stream&das&chan;
 *    if we want packet then archive it;
 *
 *
 ********************************************************************/
#include "ref2db.h"

extern Arr *StaCh;
extern Arr *PsclSTRM;
extern Arr *Chan;
extern ChArr *anewchan();
extern int VLog;

int valid_pkt( unsigned char **data,
	char *srcname,
	double *epoch,
	int *psize,
	int length,
        int byevent )
{  

   int  ptype;
   Raw *raw;
   PktPar *pkt;
   ushort_t *val;
   ushort_t code;
   char *parse;
   int  i, nelem;
   char key[64];
   char das[8], *dasname;
 
   val = ( ushort_t *)  (*data);
   code = *val;
 
   Par.raw.pkttype =  code;
   if( RawPkts == NULL) init_RawPkts();
   sprintf( key, "%d\0", code );
   if( code == 0 )  {
        elog_complain( 0, " valid_pkt(): Can't recognize a packet type %s\n", key );
        return -1;
   }
 
   raw = ( Raw *) getarr( RawPkts, (char *) &key[0] );
   if( raw == NULL ) {
        elog_complain( 0, " valid_pkt(): Can't get RawPkts info for %s\n", key );
        return -1;
   }
   memcpy( (char *) &Par.raw, (char *) raw, sizeof( Raw ) );


   switch(  parse_raw( *data, raw->parse, raw->pkttype ) ) {
 
        case -1:
          elog_complain( 0, " valid_pkt(): Can't get RawPkts info for %s\n", key );
          return -1;
        case 0:
          return 0;
        case 1:
          return (int) raw->pkttype;
   }

   if( VLog ) {
       fprintf( stderr, "dasid %d time %lf\n", Par.staid, Par.time );
       fflush( stderr );
   }
   if( Dases != 0 )  {
        sprintf( das, "%d\0", Par.staid );
        if( (dasname = getarr( Dases, das) ) == 0 ) return 0 ;
   }
 
   if( raw->pkttype == PSCLDT )  {
      update_site( ); 
      if( !check_dt( *data, byevent ) ) return -1;
   } else if( raw->pkttype == PSCLEH || raw->pkttype == PSCLET )  {
      if( !record_eh_et( *data, raw->pkttype )) return -1;
   }

   *epoch = Par.time;

    if( length > 0 ) Par.packet.size = length; 
    if( !(*psize = hdr2packet( (char **) data, Par.hdrtype,  srcname )) )  {
	elog_complain( 0, "valid_pkt(): Not a valid packet. Wrong Header?\n");
	return -1;
    } else return 1;

 
}

int record_eh_et( uchar_t *packet , int pkttype )

{

      struct PsclEH_ET pkt;
      Stream *stream;
      double stime;
      int yr, day, hr, min, sec, msec;
      int streamid; 
      int dasid;  
      char sstring[64];
      char key[16];

     memcpy( (char *) &pkt, packet, sizeof( struct PsclEH_ET ) );
 
     dasid = bcd2hex(  pkt.prehdr.unit ,  4 );
 
     sprintf( &key[0], "%d\0", dasid );
   
     streamid = bcd2hex(  pkt.streamid, 2 ); 
     streamid++;
 
     sprintf( key, "%d_%d\0", dasid, streamid );
        
     if( PsclSTRM == 0 ) PsclSTRM = newarr(0); 
     if( ( stream = (Stream *) getarr( PsclSTRM, key ) ) == 0 )
         stream = ( Stream *) new_stream( dasid, streamid );

     if( !timerr )
         if ((timerr = fopen (timerr_fname, "a+")) == 0) {
         elog_die(1, "Can't open time errors log file %s.\n", timerr_fname );
     }
 
     
     switch( pkttype )  {
        case PSCLEH:
            if( stream->etime == 0.0 ) 
                elog_complain( 0, "There is no ET record for event #%d.\n", stream->ev_num ); 
            if(  pkt.fsmp_time[0] != ' ')  {
               sscanf( pkt.fsmp_time, "%4d%3d%2d%2d%2d%3d", &yr, &day, &hr, &min, &sec, &msec);
               sprintf( sstring,"%04d%03d:%02d:%02d:%02d.%03d\0", yr, day, hr, min, sec, msec);
              stream->stime = str2epoch(&sstring[0]);
            }  else  {
              elog_complain(0, "Can't get First Sample Time ( %.6s )\n", &pkt.fsmp_time[0] );
              return 0;
            }

            stream->ev_num = bcd2hex( &packet[16], 4 );
            fprintf(timerr, "Event #%d starts at %lf\n", stream->ev_num, stream->stime);
            fflush( timerr);
 
            if( stream->ev_num > 1 )  {

                 stream->samprate = atoi(pkt.samprate);
                 stime = stream->etime + 1.0/stream->samprate;

/*
printf(" %lf %lf (%lf)\n", stream->etime, stream->stime, stime );
fflush(stdout);
*/
 
                 if( fabs(stream->stime - stime)*stream->samprate > 0.5  )  {
                    fprintf(timerr, "Event gap: previous event over at %lf and current event starts at %lf\n.", 
                    stime, stream->stime );
                    fflush( timerr);
                 }

            }
         break;

       case PSCLET: 

          if( stream->stime == 0.0 ) 
                elog_complain( 0, "There is no EH record for event #%d.\n", stream->ev_num ); 
          if(  pkt.lsmp_time[0] != ' ')  {
             sscanf( pkt.lsmp_time, "%4d%3d%2d%2d%2d%3d", &yr, &day, &hr, &min, &sec, &msec);
             sprintf( sstring,"%04d%03d:%02d:%02d:%02d.%03d\0", yr, day, hr, min, sec, msec);
             stream->etime = str2epoch( &sstring[0] );
          } else  {
             elog_complain(0, "Can't get last sample time.\n");
             return 0;
          }

          fprintf(timerr, "Event #%d is over at %lf\n\n", stream->ev_num, stream->etime);
          fflush( timerr);

          stream->stime = 0.0;
       break;
    }
    setarr( PsclSTRM, key, (char *) stream );

    return 1;
}

int check_dt( uchar_t *packet, int byevent )  
{
      double new_time, tim_diff;
      Stream *stream;
      ChArr *comp;
      int event, streamid, tshift;
      char key[16], str_key[16];

   event = bcd2hex( &packet[16], 4 );
   streamid = bcd2hex( &packet[18], 2 ); 
   streamid++;
     
   sprintf( str_key, "%d_%d\0", Par.staid, streamid );
         
   if( ( stream = (Stream *) getarr( PsclSTRM, str_key ) ) == 0 )  {
         elog_complain( 0, "Can't get %s stream info\n",str_key);
         return 0;
   }

   sprintf( key, "%d_%d_%d\0", Par.staid, Par.chan, streamid );
 
   if( ( comp = (ChArr *) getarr( Chan, key ) ) == 0 )  {
         comp = (ChArr *) anewchan(stream);   
         comp->stream = streamid;
         comp->str_time = stream->stime;
         comp->stime = stream->stime;
         comp->srate = Par.packet.srate;
   }

   
   if( comp->str_time != stream->stime  )  {
      comp->time_diff = 0;
      comp->nsamp = 0;
      comp->str_time = stream->stime;
      comp->stime = stream->stime;
      if( byevent ) rec_end_of_event();
   } 
 
   new_time = SAMP2TIME(comp->stime, comp->srate, comp->nsamp);
   tim_diff =   new_time - Par.time;  
   if( tim_diff < 0 ) 
      tshift = ceil(tim_diff-0.5) * 1000.0;
   else 
      tshift = floor(tim_diff+0.5) * 1000.0;


/*
printf("%s %lf =>  %lf %lf %d  \n", 
key, Par.time, new_time, tim_diff, tshift );
fflush(stdout);
*/


   if( tshift )  {
          if( !timerr )
             if ((timerr = fopen (timerr_fname, "a+")) == 0) {
                elog_die(1, "Can't open time errors log file %s.\n", timerr_fname );
             }
 

    /*    if( fabs(tim_diff - comp->time_diff)*comp->srate > 0.5  ) {  */
             
         fprintf( timerr, "Data gap in event %d: %d_%d_%d at %lf - time was shift on %d msec \n",
                  event, comp->stream, comp->sta, comp->chan, new_time, tshift);

         fflush( timerr);
         Par.time=new_time;
          
    }

    if( !TRSAMERATE( comp->srate, Par.packet.srate ) )  {
      elog_complain( 0, "samprate changed from %f to %f\n", comp->srate, Par.packet.srate);
      comp->srate = Par.packet.srate;
    }
    comp->nsamp += Par.packet.nsamp;
    stream->nsamp = comp->nsamp;
    setarr (Chan, key, (char *) comp);
    stream->etime = ENDTIME( Par.time, Par.packet.nsamp, Par.packet.srate);
    setarr(PsclSTRM, str_key, stream);
 
   return 1;
} 

ChArr *anewchan( int id )

{

     ChArr *new;
     allot( ChArr *, new, 1 );
    
     new->stream = id;
     new->sta = Par.staid;
     new->chan = Par.chan;
     new->nsamp = 0;

     return new;
}
