#include "ref2db.h"

extern Arr *wantcmp;

Ch_data *new_chan (PktChannel *src, SpecPar *params)
{
    Ch_data      *buf;
    int	maxsamp ;

    allot (Ch_data *, buf, 1);

    buf->stime = src->time;
    buf->crnt_time = src->time;
    buf->samprate = src->samprate;
    buf->nsamp = 0 ;
    buf->ev_over = 0 ;
 
    
    buf->data = 0;
    
    strncpy (buf->net, src->net, PKT_NAMESIZE);
    strncpy (buf->sta, src->sta, PKT_NAMESIZE);
    strncpy (buf->chan, src->chan, PKT_NAMESIZE);

    buf->params = params;
    if( buf->params->segsiz ) 
       buf->tmax = buf->stime + buf->params->segsiz ;
    else
       buf->tmax = BIG_NUMBER ;

    buf->db = params->db;
    buf->file = 0 ;
    buf->path = 0 ;


    if ( params->datacode == trSEED ) {
	buf->steim = stinit (buf);
    } else {
	buf->steim = 0 ; 
    }


    return buf ;
}


int new_dfile (Ch_data *buf, PktChannel *new, double crnt_time ) 
{
    int  num;

/*
fprintf(stdout, "new_dbrecord: %s_%s %lf\n", buf->sta, buf->chan, new->time );
fflush(stdout); 
*/

    fclose( buf->file );
    buf->db.record = dbALL ;
    if( buf->path != 0 )  {
        free(buf->path) ;
        buf->path = 0 ;
    }
    if ((buf->db.record = dbaddnull (buf->db)) < 0) {
	elog_log(0, "Couldn't add new record.\n");
	return 0; 
    } else {
	if (dbputv (buf->db, 0,
	    "sta", buf->sta,
	    "chan", buf->chan,
	    "time", crnt_time,   
	    "endtime", crnt_time,   
	    "nsamp", 0,
	    "samprate", new->samprate,        
	    "datatype", buf->params->datatype,
	    0) < 0) {
	    elog_log(0, "Couldn't write to table\n");
	    return 0; 
	} else if (trwfname (buf->db, buf->params->wfname, &(buf->path)) < 0) {
	    return 0; 
	} else if ( (buf->file = fopen (buf->path, "w+")) == 0) {
	    elog_log(1, "Can't open %s data file.\n", 
		buf->path);
	    return 0; 
	}
	buf->nsamp = 0 ; 
	buf->stime = crnt_time;
	buf->crnt_time = crnt_time;
	buf->ev_over = 0;           
        if( buf->params->segsiz ) 
           buf->tmax = buf->stime + buf->params->segsiz ;
        else
           buf->tmax = BIG_NUMBER ;
    
        if ( buf->steim  ) 
	    buf->steim->s100.samprate = buf->samprate;
    }

    return 1;
}
int new_dbrecord (Ch_data *buf, PktChannel *new, double crnt_time ) 
{
    int  num;
    int foff;
    char dir[512], dfile[512];
    struct stat sbuf;

/*
fprintf(stdout, "new_dbrecord: %s_%s %lf\n", buf->sta, buf->chan, new->time);
fflush(stdout); 
*/

    dbgetv( buf->db, 0,
            "dir", dir,
            "dfile", dfile,
            0) ;

    if(stat(buf->path, &sbuf) == 0)  
       foff = sbuf.st_size;                 
    else  {
        elog_complain(1,"Can't stat %s\n",buf->path);
      return 0;
    }

    if ((buf->db.record = dbaddnull (buf->db)) < 0) {
	elog_log(0, "Couldn't add new record.\n");
	return 0; 
    } else {
	if (dbputv (buf->db, 0,
	    "sta", buf->sta,
	    "chan", buf->chan,
	    "time", crnt_time,
	    "endtime", crnt_time,   
	    "nsamp", 0,
	    "foff", foff,
	    "dir", dir,
	    "dfile", dfile,
	    "samprate", new->samprate,        
	    "datatype", buf->params->datatype,
	    0) < 0) {
	    elog_log(0, "Couldn't write to table\n");
	    return 0; 
	} 
	buf->nsamp = 0 ; 
	buf->samprate = new->samprate ;
	buf->stime = crnt_time;
	buf->crnt_time = crnt_time;
	
        if ( buf->steim  ) 
	    buf->steim->s100.samprate = buf->samprate;
    }
    return 1;
}

int rec_end_of_event( )
     
{
         
  Site site;
  Ch_data *buf ;
  char key[64];
   
  if( wantcmp == 0 ) return 1;

  if( Par.staid < 0  )  {
      elog_complain( 0, "Wrong STAID - %d\n", Par.staid);
      return 0;   
  }
 
  if( !get_site( Par.packet.pkttype, Par.staid, Par.chan, &site))  {
       elog_complain( 0, "can't get site info for STAID:%d CHID:%d PKTTYPE:%s\n",
                 Par.staid, Par.chan, Par.packet.pkttype);
       return 0;
  }
 
  sprintf (key, "%s_%s_%s", Par.packet.net_type, site.name, site.sens);
  buf = (Ch_data *) getarr ( wantcmp, key );
  if( buf == 0 )  return 1;
  else {

      buf->ev_over = 1;
  }
  setarr( wantcmp, key, buf );

  return 1;
 
}

