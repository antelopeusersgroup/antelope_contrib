/* $Name $Revision$ $Date$  */ 
/**************************************************************************
 *
 *    read_psclIP.c  read Information PASSCAL packets.
 *
 *
 *  Author: Marina Harkins-Glushko
 *  	    UCSD, IGPP
 *	    glushko@ucsd.edu
 *
 ***********************************************************************/
#include "defunctpkt.h"

int PsclLog=0;
Arr *DasPar = 0;
extern Arr *PsclSTRM ;

Stream *new_stream( int dasid, int id )

{
        Stream *str;
    
	if( PsclSTRM == 0 ) PsclSTRM = newarr(0);

	allot( Stream *, str, 1 );
        str->stime = -1.0;
        str->etime = -1.0;
        str->dasid = dasid;
        str->id = id;
        str->ev_num = -1;
        str->samprate = -1;
        str->datatype = -1;
        str->nchan = -1;
        str->nsamp = 0;
        str->pktnum = 0;
       
	return str;
}

int match_stream( Stream **saved, Stream *old )

{
       Stream *new;

       new = *saved;

        if(  old->id != -1 )
           if( new->id != -1 &&  old->id != new->id ) {
              elog_complain( 0, "id missmatch: new %d != old %d.\n", 
	                new->id, old->id);

              return 0; 
	   }
	   
        if(  old->dasid != -1 )
           if( new->dasid != -1 &&  old->dasid != new->dasid ) {
              elog_complain( 0, "dasid missmatch: new %d != old %d.\n", 
	                new->dasid, old->dasid);

              return 0;          
	   }
	   

        if(  old->ev_num != -1 )  
           if( new->ev_num != -1 &&  old->ev_num != new->ev_num )  {
              elog_complain( 0, "ev_num missmatch: new %d != old %d.\n",
	                new->ev_num, old->ev_num);
              new->nsamp = 0;
	   }
	   

        if(  old->samprate != -1 )  
           if( new->samprate != -1 &&  old->samprate != new->samprate )  {
              elog_complain( 0, "samprate missmatch: new %d != old %d.\n", 
	                new->samprate, old->samprate);
              return 0;            
	   }
	   

        if(  old->datatype != -1 )  
           if( new->datatype != -1 &&  old->datatype != new->datatype )  {
              elog_complain( 0, "datatype missmatch: new %d != old %d\n",
	                new->datatype, old->datatype );
              return 0;             
	   }
	   

        if(  old->nchan != -1 )  
           if( new->nchan != -1 &&  old->nchan != new->nchan )  {
              elog_complain( 0, "nchan missmatch: new %d != old %d. \n",
	                new->nchan, old->nchan );
             return 0;              
	   }
	   
 
     return 1; 
        
}

int set_Stream( 
	int id,
	int dasid,
	Stream *stream )

{

   char key[64];
   Stream *tmp;
   
   sprintf( key, "%d_%d\0", dasid, id );
 
   tmp = ( Stream * ) getarr( PsclSTRM, key );
   if( tmp != 0 )  {
      if( !match_stream( &stream, tmp ) ) return 0;
   }  
   setarr( PsclSTRM, key, (char *) stream ); 
   return 1;
}


DASPar *getDAS( int dasid )
{

   DASPar *tmpdas;
   char key[24];

   if( DasPar == 0 )  DasPar = newarr( 0 );

   tmpdas = ( DASPar *) malloc( sizeof(DASPar ));
   if( tmpdas == 0 ) {
      elog_complain( 1, "malloc error \n");
      return 0;
   }
   sprintf( tmpdas->daslog, "%d_log\0", dasid );
   tmpdas->dasid = dasid;
 
   if( (tmpdas->daslog_fp = fopen( tmpdas->daslog, "a")) == 0 ) {
        elog_complain( 0, "Can't open LOG file for DAS# %s\n", tmpdas->daslog);
        return 0;
   }
   sprintf( &key[0], "%d\0", dasid ); 
   setarr( DasPar, (char *) &key[0], (char *) tmpdas );
  
   return tmpdas;
   

}

 
int read_pscl_AD( 
    double time, 
    char *srcid, 
    uchar_t *packet, 
    Packet **Pkt, 
    void *par )
{
    return 2;
}

int read_pscl_CD( 
    double time, 
    char *srcid, 
    uchar_t *packet, 
    Packet **Pkt, 
    void *par )
{

      struct PsclCD pkt;
      DASPar *das;
      int dasid;  
      char key[24];
      struct DataPar *hdr;
      int hdrsize = 0;

     if( !PsclLog )  return 2;

     if( par != 0 )  {
	hdr = ( struct DataPar *) par;
	hdrsize = hdr->hdrsize;
     }  
        
      memcpy( (char *) &pkt, packet+hdrsize, sizeof( struct PsclCD ) );

      dasid = bcd2hex(  pkt.prehdr.unit , 4 );
   
    sprintf( &key[0], "%d\0", dasid );
    if( DasPar == 0 )  DasPar = newarr( 0 );
    das = ( DASPar *) getarr( DasPar, (char *) &key[0] );
  
    if( das == 0 )  {
        das = ( DASPar *) getDAS( dasid );
        if( das == 0 ) {
           elog_complain( 0, "Can't record CD for DAS#%d to the LOG file.\n", dasid);
           return 0;
	}
    } 
    
     fprintf(das->daslog_fp, "\nCalibration Definition \n");
     fprintf(das->daslog_fp, "  Start: Year %.4s Day %.3s %.2s:%.2s:%.2s \n",
         &( pkt.stime[0]), &( pkt.stime[4]), &( pkt.stime[7]), 
         &( pkt.stime[9]), &( pkt.stime[11]));
     fprintf(das->daslog_fp, "  Repeat Interval Days %.2s %.2s:%.2s:%.2s", 
         &( pkt.interv[0]), &( pkt.interv[2]), &( pkt.interv[4]), &( pkt.interv[6]));
     fprintf(das->daslog_fp, "  Number of Repeats %.4s\n", &( pkt.ninterv[0]));
     fprintf(das->daslog_fp, "  Length of CAL (seconds) %.8s\n", &( pkt.length[0]));
  
    /* Is STEP ON?   */

     if ( !strncmp( (char *) &( pkt.on_off[0]), "ON", 2)) {
        /* step turned on  */
        fprintf(das->daslog_fp, "  Step ON\n");
        fprintf(das->daslog_fp, "    Period (seconds) %.8s Size (seconds) %.8s \n", 
            &( pkt.period[0]), &( pkt.size[0]));
        fprintf(das->daslog_fp, "    Amplitude (volts) %.8s Output to %.4s\n",
            &( pkt.amp[0]), &( pkt.output[0]));
    }
    else {
        fprintf(das->daslog_fp, "  Step OFF\n");   
    }
 
/*
 
    if ( !strncmp( (char *) &( pkt.freq[0]), "ON", 2)) {
        fprintf(das->daslog_fp, "  Freq ON\n");
        fprintf(das->daslog_fp, "    Start (hertz) %.8s ", &( pkt.freq_start[0]));
        fprintf(das->daslog_fp, "   Stop (hertz) %.8s \n", &( pkt.freq_stop[0]));
        fprintf(das->daslog_fp, "    Amplitude (volts) %.8s", &( pkt.freq_ampl[0]));
        fprintf(das->daslog_fp, "   Output to %.4s\n", &( pkt.freq_output[0]));
    }
    else {
        fprintf(das->daslog_fp, "  Freq OFF\n");    
    }
  
    if ( !strncmp((char *) &( pkt.noise[0]), "ON", 2) ) {
  
        fprintf(das->daslog_fp, "  Noise ON\n");
        fprintf(das->daslog_fp, "    Amplitude (volts) %.8s", &( pkt.noise_ampl[0]));
        fprintf(das->daslog_fp, "   Output to %.4s\n", &( pkt.noise_output[0]));
    }
    else {
        fprintf(das->daslog_fp, "  Noise OFF\n");    
    }
 */ 

      fflush( das->daslog_fp ); 
      return 2;

}

int read_pscl_DS( 
    double time, 
    char *srcid, 
    uchar_t *packet, 
    Packet **Pkt, 
    void *par )
{

      Stream *stream;
      struct PsclDS pkt;
      DASPar *das;
      int dasid, id, yr, day, hr, min, sec, msec;  
      int ch, nchan=0, i;
      char key[16];
      char string[16];
      struct DataPar *hdr;
      int hdrsize = 0;


     if( par != 0 )  {
	hdr = ( struct DataPar *) par;
	hdrsize = hdr->hdrsize;
     }  
        

     memcpy( (char *) &pkt, packet+hdrsize, sizeof( struct PsclDS ) );

    dasid = bcd2hex(  pkt.prehdr.unit , 4 );
    yr = bcd2hex(  pkt.prehdr.year , 2 );
    sprintf(string, "%02x%02x%02x%02x%02x%02x", 
       pkt.prehdr.bcdtime[0],  pkt.prehdr.bcdtime[1], pkt.prehdr.bcdtime[2], 
       pkt.prehdr.bcdtime[3], pkt.prehdr.bcdtime[4], pkt.prehdr.bcdtime[5]);
    sscanf( string, "%3d%2d%2d%2d%3d", &day, &hr, &min, &sec, &msec);
   
    if( PsclLog )  {
	sprintf( &key[0], "%d\0", dasid );
        if( DasPar == 0 )  DasPar = newarr( 0 );
        das = ( DASPar *) getarr( DasPar, (char *) &key[0] );
  
        if( das == 0 )  {
            das = ( DASPar *) getDAS( dasid );
            if( das == 0 ) {
               elog_complain( 0, "Can't record CD for DAS#%d to the LOG file.\n", dasid);
               return 0;
	    }
        } 
    
        fprintf( das->daslog_fp, "\nDATA STREAM DEFINITION\n\n");
    }

    for( i = 0; i < 4; i++ )  {
      if(  pkt.dstreams[i].streamnum[0] == ' ' ) continue;
      if( PsclLog  )  {
	  if(yr < 50 ) 
	      fprintf( das->daslog_fp, 
	          " 20%.2d-%3d:%2d:%2d:%2d.%3d: STREAM ID %.2s NAME: %.24s\n",
                    yr, day, hr, min, sec, msec,  
                    pkt.dstreams[i].streamnum,  pkt.dstreams[i].stream_name );  
	  else
	      fprintf( das->daslog_fp, 
	          " 19%.2d-%3d:%2d:%2d:%2d.%3d: STREAM ID %.2s NAME: %.24s\n",
                    yr, day, hr, min, sec, msec,  
                    pkt.dstreams[i].streamnum,  pkt.dstreams[i].stream_name );  
          fprintf( das->daslog_fp, 
	            "	Sample Rate: %.4s Data Type: %.2s Channels: ",
                    pkt.dstreams[i].samprate,  pkt.dstreams[i].datatype ); 
          for( ch = 0; ch < 16; ch++)  
            if(  pkt.dstreams[i].channels[ch] != 0x20) {
               nchan++;
               fprintf( das->daslog_fp, "%c ", pkt.dstreams[i].channels[ch]);
            }
          fprintf( das->daslog_fp, "	Trigger Type: %.4s \n", 
                   pkt.dstreams[i].trgtype); 
          if( !strncmp( pkt.dstreams[i].trgtype, "CON", strlen("CON")) )  {
                fprintf(das->daslog_fp, "  Record Length (seconds) %.8s\n",
                &( pkt.dstreams[i].trginfo[0]));

          }  else if( !strncmp( pkt.dstreams[i].trgtype, "CRS", strlen("CRS")) )  {
               fprintf(das->daslog_fp, "  Cross Trigger from Stream %.2s\n", 
                   &( pkt.dstreams[i].trginfo[0]));
               fprintf(das->daslog_fp, "  Pretrigger Length (seconds)  %.8s\n", 
                   &( pkt.dstreams[i].trginfo[2]));
               fprintf(das->daslog_fp, "  Record Length (seconds) %.8s\n", 
                   &( pkt.dstreams[i].trginfo[10]));
   
          }  else if( !strncmp( pkt.dstreams[i].trgtype, "EVT", strlen("EVT")) )  {
               fprintf(das->daslog_fp, "  Trigger Channels %.16s\n",
                   &( pkt.dstreams[i].trginfo[0]));
               fprintf(das->daslog_fp, "  Minimum Number of channels %.2s",
                   &( pkt.dstreams[i].trginfo[16]));
               fprintf(das->daslog_fp, "   Trigger Window (seconds) %.8s\n",
                   &( pkt.dstreams[i].trginfo[18]));
               fprintf(das->daslog_fp, "  Pretrigger Length (seconds)  %.8s\n",
                   &( pkt.dstreams[i].trginfo[26]));
               fprintf(das->daslog_fp, "  Posttrigger Length  (seconds) %.8s\n",
                   &( pkt.dstreams[i].trginfo[34]));
               fprintf(das->daslog_fp, "  Record Length (seconds) %.8s\n",
                   &( pkt.dstreams[i].trginfo[42]));
               fprintf(das->daslog_fp, "  STA (seconds) %.8s ",
                   &( pkt.dstreams[i].trginfo[58]));
               fprintf(das->daslog_fp, "  LTA (seconds) %.8s \n",
                   &( pkt.dstreams[i].trginfo[66]));
               fprintf(das->daslog_fp, "  Mean Average Length %.8s\n",
                   &( pkt.dstreams[i].trginfo[74]));
               fprintf(das->daslog_fp, "  Trigger Ratio %.8s ",
                   &( pkt.dstreams[i].trginfo[82]));
               fprintf(das->daslog_fp, "  Detrigger Ratio %.8s\n",
                   &( pkt.dstreams[i].trginfo[90]));
               fprintf(das->daslog_fp, "  LTA Hold Flag %.4s",
                   &( pkt.dstreams[i].trginfo[98]));

          }  else if( !strncmp( pkt.dstreams[i].trgtype, "EXT", strlen("EXT")) )  {
            fprintf(das->daslog_fp, "  Pretrigger Length (seconds ) %.8s\n", 
                &( pkt.dstreams[i].trginfo[0]));
            fprintf(das->daslog_fp, "  Record Length (seconds) %.8s\n",
                &( pkt.dstreams[i].trginfo[8]));
               

         }  else if( !strncmp( pkt.dstreams[i].trgtype, "LVL", strlen("LVL")) )  {
            fprintf(das->daslog_fp, "  Trigger Level (counts ) %.8s\n",
                &( pkt.dstreams[i].trginfo[0]));
            fprintf(das->daslog_fp, "  Pretrigger Length (seconds ) %.8s\n",
                &( pkt.dstreams[i].trginfo[8]));
            fprintf(das->daslog_fp, "  Record Length (seconds) %.8s\n",
                &( pkt.dstreams[i].trginfo[16]) );

         }  else if( !strncmp( pkt.dstreams[i].trgtype, "RAD", strlen("RAD")) )  {
            fprintf(das->daslog_fp, "  Pretrigger Length (seconds ) %.8s\n",
                &( pkt.dstreams[i].trginfo[0]));
            fprintf(das->daslog_fp, "  Record Length (seconds) %.8s\n", 
                &( pkt.dstreams[i].trginfo[8]));

         }  else if( !strncmp( pkt.dstreams[i].trgtype, "TIM", strlen("TIM")) )  {
            fprintf(das->daslog_fp, "  Start Time : Year %.4s ", 
                &( pkt.dstreams[i].trginfo[0]));
            fprintf(das->daslog_fp, "   Day %.3s ",
                &( pkt.dstreams[i].trginfo[4]));
            fprintf(das->daslog_fp, "  %.2s:%.2s:%.2s\n", 
                &( pkt.dstreams[i].trginfo[7]),
                &( pkt.dstreams[i].trginfo[9]),
                &( pkt.dstreams[i].trginfo[11]));
            fprintf(das->daslog_fp, "  Repeat Interval Days %.2s ", 
                &( pkt.dstreams[i].trginfo[14]));
            fprintf(das->daslog_fp, "  %.2s:%.2s:%.2s ", 
                &( pkt.dstreams[i].trginfo[16]),
                &( pkt.dstreams[i].trginfo[18]),
                &( pkt.dstreams[i].trginfo[20]));
            fprintf(das->daslog_fp, "  Number of Repeats %.4s\n", 
                &( pkt.dstreams[i].trginfo[22]));
            fprintf(das->daslog_fp, "  Pretrigger Length (seconds) %.8s\n",
                &( pkt.dstreams[i].trginfo[26]));
            fprintf(das->daslog_fp, "  Record Length (seconds) %.8s\n", 
                &( pkt.dstreams[i].trginfo[34]));
 
         }  
         fflush( das->daslog_fp ); 
      } 
      id = atoi( pkt.dstreams[i].streamnum );

      sprintf( key, "%d_%d\0", dasid, id );
      if( PsclSTRM == 0 ) PsclSTRM = newarr(0);
      if( ( stream = (Stream *) getarr( PsclSTRM, key ) ) == 0 )  
         stream = ( Stream *) new_stream( dasid, id );
       
       stream->samprate = atoi( &( pkt.dstreams[i].samprate[0]) ); 
   
       if(   pkt.dstreams[i].datatype[0] == '1' )
          stream->datatype = 2;
       else if(  pkt.dstreams[i].datatype[0] == '3' )
          stream->datatype = 4;
       else if(  pkt.dstreams[i].datatype[0] == 'C' )
          stream->datatype = 0;
       else stream->datatype = -1;
 
       stream->nchan = nchan;
        
       if( !set_Stream( id, dasid, stream ) ) return 0; 
   } 
  
   return 2;
      
}


int read_pscl_EH(
    double time, 
    char *srcid, 
    uchar_t *packet, 
    Packet **Pkt, 
    void *par )
{
      struct PsclEH_ET pkt;
      Stream *stream;
      DASPar *das;
      int dasid;  
      int yr, day, hr, min, sec, msec;
      int event_num, streamid; 
      int off, i;
      char sstring[64];
      char key[16];
      struct DataPar *hdr;
      int hdrsize = 0;

     if( par != 0 )  {
	hdr = ( struct DataPar *) par;
	hdrsize = hdr->hdrsize;
     }  
        
     memcpy( (char *) &pkt, packet+hdrsize, sizeof( struct PsclEH_ET ) );

    dasid = bcd2hex(  pkt.prehdr.unit ,  4 );
   
    sprintf( &key[0], "%d\0", dasid );
   
    if( PsclLog )  {
	if( DasPar == 0 )  DasPar = newarr( 0 );
        das = ( DASPar *) getarr( DasPar, (char *) &key[0] );
        if( das == 0 )  {
            das = ( DASPar *) getDAS( dasid );
            if( das == 0 ) {
               elog_complain( 0, "Can't record CD for DAS#%d to the LOG file.\n", dasid);
               return 0;
	    }
        } 
    } 
    event_num = bcd2hex(  pkt.eventnum, 4 ); 
    streamid = bcd2hex(  pkt.streamid, 2 ); 
    streamid++;

    sprintf( key, "%d_%d\0", dasid, streamid );
    if( PsclSTRM == 0 ) PsclSTRM = newarr(0);
    if( ( stream = (Stream *) getarr( PsclSTRM, key ) ) == 0 )
        stream = ( Stream *) new_stream( dasid, streamid );
	
    if(  pkt.fsmp_time[0] != ' ')  {
       sscanf( pkt.fsmp_time, "%4d%3d%2d%2d%2d%3d", &yr, &day, &hr, &min, &sec, &msec);
       sprintf( sstring,"%04d%03d:%02d:%02d:%02d.%03d\0", yr, day, hr, min, sec, msec);
      stream->stime = str2epoch(&sstring[0]);
    }  else  {
      elog_complain(0, "Can't get First Sample Time ( %.6s )\n", &pkt.fsmp_time[0] );
      return 0;
    } 
    stream->ev_num = event_num; 
    if( pkt.samprate[0] != ' ') 
      stream->samprate = atoi(pkt.samprate);
    
    if(  pkt.datatype[0] == 0x16 )
          stream->datatype = 2;
    if(  pkt.datatype[0] == 0x32 )
          stream->datatype = 4;
    if(  pkt.datatype[0] == 0xc0 )
          stream->datatype = 0;
    else stream->datatype = -1;

    stream->etime = -1; 
    if( !set_Stream( streamid, dasid, stream ) ) return 0; 
  
    if( PsclLog  )  {

       fprintf( das->daslog_fp, "\nEvent: %d ", event_num);
       fprintf( das->daslog_fp, "  Das ID: %d  Stream: %d", dasid, streamid);
       if(  pkt.tmmsg[0] != ' ')
           fprintf( das->daslog_fp, "  Start: %.24s\n",  pkt.tmmsg);
/*
       for( i = 0, off=0; i < 16; i++  )  {
         if(  pkt.weight[off] != 0x20 )
             fprintf( das->daslog_fp, "	Channel: %d with  %.8s (volts/count) \n", 
                i, &( pkt.weight[off]));
         off += 8;
       } 
*/
       if( stream->stime > 0 )
          fprintf( das->daslog_fp, "  First sample: %s ( %lf )  ", sstring, stream->stime);
    
       fprintf( das->daslog_fp, "\n\n");
       fflush( das->daslog_fp );
    }
    return 2;
}


int read_pscl_ET( 
    double time, 
    char *srcid, 
    uchar_t *packet, 
    Packet **Pkt, 
    void *par )
{
      double etime;
      struct PsclEH_ET pkt;
      Stream *stream;
      DASPar *das;
      int dasid;
      int yr, day, hr, min, sec, msec;
      int event_num, streamid; 
      int off, i;
      char skey[64], estring[64];
      char key[16];
      struct DataPar *hdr;
      int hdrsize = 0;

     if( par != 0 )  {
	hdr = ( struct DataPar *) par;
	hdrsize = hdr->hdrsize;
     }  
        
     memcpy( (char *) &pkt, packet+hdrsize, sizeof( struct PsclEH_ET ) );

    dasid = bcd2hex(  &packet[4+hdrsize], 4 );
  
    sprintf( &key[0], "%d\0", dasid );
    if( PsclLog )  {
	if( DasPar == 0 )  DasPar = newarr( 0 );
        das = ( DASPar *) getarr( DasPar, (char *) &key[0] );
  
        if( das == 0 )  {
            das = ( DASPar *) getDAS( dasid );
            if( das == 0 ) {
               elog_complain( 0, "Can't record ET for DAS#%d to the LOG file.\n", dasid);
               return 0;
	    }
        } 
    }

    event_num = bcd2hex(  pkt.eventnum, 4 ); 
    streamid = bcd2hex(  pkt.streamid, 2 ); 
    streamid++;
   
    if( PsclSTRM == 0 ) return 2; 
    sprintf( key, "%d_%d\0", dasid, streamid );
    if( ( stream = (Stream *) getarr( PsclSTRM, key ) ) == 0 ) return 2;

    if(  pkt.lsmp_time[0] != ' ')  {
       sscanf( pkt.lsmp_time, "%4d%3d%2d%2d%2d%3d", &yr, &day, &hr, &min, &sec, &msec);
       sprintf( estring,"%04d%03d:%02d:%02d:%02d.%03d\0", yr, day, hr, min, sec, msec);
       etime = str2epoch( &estring[0] );
    } else  {
       elog_complain(0, "Can't get last sample time.\n");
       return 0;
    }
    if( PsclLog  )  {
	if( etime > 0 )
           fprintf( das->daslog_fp, "  Last Sample: %s ( %lf ) \n", estring, etime);
    
       fprintf( das->daslog_fp, "\nEvent %d Das: %d Stream: %d is Over at :", 
                event_num, dasid, streamid );
       fprintf( das->daslog_fp, " %.24s\n",  pkt.tmmsg);
       fprintf( das->daslog_fp, "\n\n");
       fflush( das->daslog_fp );
    } 
    stream->stime = -1;
    stream->ev_num = -1;
    if( !set_Stream( streamid, dasid, stream ) ) return 0; 
  
    return 2;
}

int read_pscl_OM( 
    double time, 
    char *srcid, 
    uchar_t *packet, 
    Packet **Pkt, 
    void *par )
{

      struct PsclOM pkt;
      DASPar *das;
      int dasid;  
      int i;
      char key[16];
      struct DataPar *hdr;
      int hdrsize = 0;

     if( par != 0 )  {
	hdr = ( struct DataPar *) par;
	hdrsize = hdr->hdrsize;
     }  
    
     if( !PsclLog ) return 2;

     memcpy( (char *) &pkt, packet+hdrsize,sizeof( struct PsclOM ) );
    
    dasid = bcd2hex(  pkt.prehdr.unit , 4 );
   
    sprintf( &key[0], "%d\0", dasid );
    if( DasPar == 0 )  DasPar = newarr( 0 );
    das = ( DASPar *) getarr( DasPar, (char *) &key[0] );
  
    if( das == 0 )  {
        das = ( DASPar *) getDAS( dasid );
        if( das == 0 ) {
           elog_complain( 0, "Can't record CD for DAS#%d to the LOG file.\n", dasid);
           return 0;
	}
    } 
    
    fprintf( das->daslog_fp, "\nWake-up Sequence Definition:\n   State:%.2s\n   Recording_Mode:%.2s\n", 
         pkt.power_state,  pkt.recmode);
    fflush( das->daslog_fp);
    
    for( i = 0; i < 8; i++)  {

        if (  pkt.wake[i].stime[0] == ' ')
            continue;
  
        fprintf(das->daslog_fp, "  Start: %.4s-%03d:%02d:%02d:%02d ", 
            pkt.wake[i].stime,
           str2int(&( pkt.wake[i].stime[4]),3),
           str2int(&( pkt.wake[i].stime[7]),2), 
           str2int(&( pkt.wake[i].stime[9]),2), 
           str2int(&( pkt.wake[i].stime[11]),2));
        fprintf(das->daslog_fp, "\n  Sequence # : %.2s\n",  pkt.wake[i].num);
        fprintf(das->daslog_fp, "  Start time : %.12s ",  pkt.wake[i].stime);
        fprintf(das->daslog_fp, "  Repeat Duration Days %02d%02d:%02d:%02d\n ", 
           str2int( pkt.wake[i].pow_dur,2),
           str2int(&( pkt.wake[i].pow_dur[2]),2), 
           str2int(&( pkt.wake[i].pow_dur[4]),2), 
           str2int(&( pkt.wake[i].pow_dur[6]),2));
        fprintf(das->daslog_fp, "  Repeat Interval Days %02d%02d:%02d:%02d\n ", 
           str2int(&( pkt.wake[i].interv[0]),2),
           str2int(&( pkt.wake[i].interv[2]),2), 
           str2int(&( pkt.wake[i].interv[4]),2), 
           str2int(&( pkt.wake[i].interv[6]),2));
        fprintf(das->daslog_fp, "  Number of Intervals %04d\n", 
           str2int( pkt.wake[i].interv_num,4));

    }
    fflush( das->daslog_fp );
    return 2;
    
}


int read_pscl_SH( 
    double time, 
    char *srcid, 
    uchar_t *packet, 
    Packet **Pkt, 
    void *par )
{

      struct PsclSH pkt;
      DASPar *das;
      int dasid;  
      uchar_t *cline, *line, cr, lf;
      char key[16];
      struct DataPar *hdr;
      int hdrsize = 0;
    
      cr = 13; lf = 10;

      if( !PsclLog )  return 2;

     if( par != 0 )  {
	hdr = ( struct DataPar *) par;
	hdrsize = hdr->hdrsize;
     }  
        
     memcpy( (char *) &pkt, packet+hdrsize, sizeof( struct PsclSH ) );
    
    dasid = bcd2hex(  pkt.prehdr.unit , 4 );
   
    sprintf( &key[0], "%d\0", dasid );
    if( DasPar == 0 )  DasPar = newarr( 0 );
    das = ( DASPar *) getarr( DasPar, (char *) &key[0] );
    if( das == 0 )  {
        das = ( DASPar *) getDAS( dasid );
        if( das == 0 ) {
           elog_complain( 0, "Can't record CD for DAS#%d to the LOG file.\n", dasid);
           return 0;
	}
    } 
    
    line =  pkt.info;
    fprintf( das->daslog_fp, "SH:DAS# %d\n ", dasid ); 
    while( (int) line < (int) &packet[1023+hdrsize])  {
        cline = (uchar_t *) strchr((char*) line, cr );
        if( cline == 0 ) break;
        if ( *(cline + 1) == lf ) {
            *cline = 0;
            fprintf( das->daslog_fp, "  %s\n ", line ); 
            line  = (cline + 2);
        } else {
                elog_complain(0, "Warning: CR/LF missing in SOH block\n");
            line  = (cline + 2);
        }
    }
    fflush( das->daslog_fp );
    return 2;

}


int read_pscl_SC( 
    double time, 
    char *srcid, 
    uchar_t *packet, 
    Packet **Pkt, 
    void *par )
{

      struct PsclSC pkt;
      DASPar *das;
      int dasid, id;  
      int  i;
      char key[16];
      struct DataPar *hdr;
      int hdrsize = 0;

      if( !PsclLog )  return 2;

     if( par != 0 )  {
	hdr = ( struct DataPar *) par;
	hdrsize = hdr->hdrsize;
     }  
        
     memcpy( (char *) &pkt, packet+hdrsize, sizeof( struct PsclSC ) );


    dasid = bcd2hex(  pkt.prehdr.unit , 4 );
   
    sprintf( &key[0], "%d\0", dasid );
    if( DasPar == 0 )  DasPar = newarr( 0 );
    das = ( DASPar *) getarr( DasPar, (char *) &key[0] );
  
    if( das == 0 )  {
        das = ( DASPar *) getDAS( dasid );
        if( das == 0 ) {
           elog_complain( 0, "Can't record CD for DAS#%d to the LOG file.\n", dasid);
           return 0;
	}
    } 
    
    fprintf( das->daslog_fp, "\n Station-Channel Definition \n");
    fprintf( das->daslog_fp, " Experiment: %.2s  Name: %.24s Comments: %.40s \n", 
      &( pkt.expnum[0]), &( pkt.expname[0]),&( pkt.comment[0]));
     fprintf( das->daslog_fp, "  Start: %.14s\n", &( pkt.start[0]));
     fprintf( das->daslog_fp, "  Station ID: %.4s Name: %.24s Comments: %.40s\n", 
       &( pkt.staid[0]), &( pkt.sta[0]),&( pkt.sta_comment[0]));
     fprintf( das->daslog_fp, "  Das Serial Number: %.12s Model: %.12s\n ", 
              &(pkt.dasid[0]), &( pkt.dastype[0]));
     fprintf( das->daslog_fp, "  Clock SN#:%.10s Type: %.4s\n",
              &(pkt.clkid[0]), &(pkt.clktype[0]));
     for( i = 0; i < 5; i++)  
       if(   pkt.ch[i].chid[0] != ' ' )  {
          fprintf( das->daslog_fp, "  \nChannel ID: %.2s Channel Name: %.10s \n", 
             &( pkt.ch[i].chid[0]), &( pkt.ch[i].name[0]));
          fprintf( das->daslog_fp, 
             "  AZ:%.10s INC:%.10s X-%.10s Y:-.10s XYType:%.4s Z-%.10s-ZType:%.4s\n", 
             &( pkt.ch[i].azimuth[0]),&( pkt.ch[i].inclination[0]),
             &( pkt.ch[i].xcoord[0]), &( pkt.ch[i].ycoord[0]), &( pkt.ch[i].xytype[0]),
             &( pkt.ch[i].zcoord[0]),  &( pkt.ch[i].ztype[0]));
         fprintf( das->daslog_fp, "  Gain: %.4s Sensor: %.12s Model: %.12s\n", 
             &(pkt.ch[i].gain[0]), &(pkt.ch[i].sensid[0]), &(pkt.ch[i].sens_model[0]));
         fprintf( das->daslog_fp, "  Volts per BIT:%.8s\n", &( pkt.ch[i].volt[0]));
         fprintf( das->daslog_fp, "  Comments:%.40s\n\n", &( pkt.ch[i].comment[0]));
     } 
     fflush( das->daslog_fp );
     
   return 2;
    
}

 

/* $Id$ */
