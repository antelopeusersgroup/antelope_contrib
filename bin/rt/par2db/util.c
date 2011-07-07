#include <sys/stat.h>
#include "par2db.h"

extern Steim *stinit();

Source *new_source ( int npkt )
{
    Source *source ;

    allot ( Source *, source, 1 ) ;
    source->apipe = new_orbpipe ( npkt );
    source->last = 0.0 ;
    return source ;
}

Db_buffer  *new_buf (PktChannel *src, Save_params *params)
{
    Db_buffer      *buf;
    int	maxsamp ;

    allot (Db_buffer *, buf, 1);

    strncpy (buf->net, src->net, PKT_NAMESIZE);
    strncpy (buf->sta, src->sta, PKT_NAMESIZE);
    strncpy (buf->chan, src->chan, PKT_NAMESIZE);
 
    buf->stime = src->time;
    buf->crnt_time = src->time;
    buf->samprate = src->samprate;
    buf->nsamp = 0 ;
    buf->data = 0;

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

int
new_dfile (Db_buffer *buf, PktChannel *new, double crnt_time ) 
{
    int             n;

    fclose( buf->file );
    buf->db.record = dbALL ;
    if( buf->path != 0 )  {
       free(buf->path) ;
       buf->path = 0 ;
    }
    if ((buf->db.record = dbaddnull (buf->db)) < 0) {
	elog_log(0, "Couldn't add new record to table\n");
	return 0; 
    } else {
	if (dbputv (buf->db, 0,
	    "sta", buf->sta,
	    "chan", buf->chan,
	    "time", crnt_time,
	    "endtime", crnt_time,
	    "nsamp", 0,
	    "samprate", new->samprate,
	    "datatype",  buf->params->datatype,
	    0) < 0) {
	    elog_log(0, "Couldn't write to table\n");
	    return 0; 
	} else if (trwfname (buf->db, buf->params->wfname, &(buf->path)) < 0) {
	    return 0;
	} else if ( (buf->file = fopen (buf->path, "w+")) == 0) {
            elog_die(1, "Can't open %s data file.\n", buf->path);
        }
        if ( fclose ( buf->file ) != 0 ) {
             elog_die( 1, "Couldn't close output file '%s'\n", buf->path ) ; 
        }
        buf->file = 0;

							    
	buf->nsamp = 0 ; 
	buf->stime = crnt_time ;
	buf->crnt_time = crnt_time ;
	if( buf->params->segsiz )
	   buf->tmax = buf->stime + buf->params->segsiz ;
        else 
	   buf->tmax = BIG_NUMBER ;

	if ( buf->steim  ) 
           buf->steim->s100.samprate = buf->samprate;

    }
/*
fprintf( stderr,"filename %s new tmax %lf\n", buf->path,  buf->tmax );
*/

    return 1;
}

int new_dbrecord ( Db_buffer *buf, PktChannel *new, double stime ) 
{
    int  num;
    int foff;
    char dir[512], dfile[512];
    struct stat sbuf;
		     
     dbgetv( buf->db, 0, "dir", dir, "dfile", dfile, 0) ;

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
	     "time", stime,     
	     "endtime", stime,       
	     "nsamp", 0, 
	     "foff", foff, 
	     "dir", dir, 
	     "dfile", dfile, 
	     "samprate", new->samprate,        
	     "datatype", buf->params->datatype, 0) < 0) {
	    
	      elog_log(0, "Couldn't write to table\n");
	      return 0; 
	  }
          buf->nsamp = 0 ; 
          buf->samprate = new->samprate ;
          buf->stime = stime;       
          buf->crnt_time = stime;       
				          
          if ( buf->steim  ) 
              buf->steim->s100.samprate = buf->samprate;
     }
     return 1;
}



double get_last_dbtimes (Dbptr db)
{
    Tbl *keys ; 
    int nstachan ;
    Dbptr dbs, dbg ; 
    Expression *expr ;
    double after, min_after ;
    char sta[25], chan[25] ;

    dbquery ( db, dbRECORD_COUNT, &nstachan ) ; 
    if ( nstachan < 1 ) 
	return 0.0 ; 

    keys = strtbl( "sta", "chan", 0 ) ; 
    dbs = dbsort ( db, keys, 0, 0 ) ; 
    dbg = dbgroup ( dbs, keys, 0, 1 ) ; 

    min_after = BIG_NUMBER ;
    dbquery ( dbg, dbRECORD_COUNT, &nstachan ) ; 
    dbex_compile ( dbg, "max(endtime)", &expr, dbREAL ) ;
    for ( dbg.record = 0 ; dbg.record < nstachan ; dbg.record++ ) {
	dbgetv ( dbg, 0, "sta", &sta, "chan", &chan, 0 ) ; 
	if ( dbex_eval(dbg, expr, 0, &after ) < 0 ) 
	    elog_die( 0, "failed to read maximum endtime for %s_%s\n", 
		sta, chan ) ; 
	min_after = MIN(after, min_after) ; 
    }

    dbex_free ( expr ) ; 
    freetbl(keys, 0) ;
    return min_after ;
}

void usage ()
{
    fprintf (stderr, "usage: %s [-d datatype] [-g] [-m srcmatch] [-i interval] [-v] orb db [start-time [window]]\n", Program_Name);
    exit (1);
}
