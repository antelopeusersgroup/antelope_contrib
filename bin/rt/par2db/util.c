#include "par2db.h"

Source *
new_source ( int npkt )
{
    Source *source ;

    allot ( Source *, source, 1 ) ;
    source->apipe = new_orbpipe ( npkt );
    source->last = 0.0 ;
    return source ;
}

Data_segment   *
new_data_segment (int maxsamp)
{
    Data_segment   *aseg;

    allot (Data_segment *, aseg, 1);
    aseg->t0 = 0.0;
    aseg->samprate = 0.0;
    if ( maxsamp > 0 ) 
	allot (int *, aseg->data, maxsamp);
    else 
	aseg->data = 0 ;
    aseg->nsamp = 0;
    aseg->maxsamp = maxsamp;
    return aseg;
}

static int      encoding_format[] = {0, 10, 11, 20};

Db_buffer      *
new_db_buffer (PktChannel *src, Save_params *params)
{
    Db_buffer      *buf;
    int	maxsamp ;

    allot (Db_buffer *, buf, 1);

    strncpy (buf->net, src->net, PKT_NAMESIZE);
    strncpy (buf->sta, src->sta, PKT_NAMESIZE);
    strncpy (buf->chan, src->chan, PKT_NAMESIZE);

    maxsamp = TIME2SAMP (0.0, src->samprate, params->memsize ) ; 
    buf->mem = new_data_segment (maxsamp);
    buf->disk = new_data_segment (0);

    buf->params = params;

    buf->db = params->db;
    buf->file = 0 ;
    buf->path = 0 ;

    init_mem_segment ( src, buf) ;

    buf->tmax = VERY_LARGE_DOUBLE ;

    return buf ;
}

int
new_dbrecord (Db_buffer *buf) 
{
    int             n, retcode = 0;

    if ((buf->db.record = dbaddnull (buf->db)) < 0) {
	register_error (0, "Couldn't add new record to table\n");
	retcode++;
    } else {
	if (dbputv (buf->db, 0,
	    "sta", buf->sta,
	    "chan", buf->chan,
	    "time", buf->mem->t0,
	    "nsamp", 0,
	    "samprate", buf->mem->samprate,
	    "datatype", "s4",
	    0) < 0) {
	    register_error (0, "Couldn't write to table\n");
	    retcode++;
	} else if (trwfname (buf->db, buf->params->wfname, &(buf->path)) < 0) {
	    retcode++;
	} else 
	
	buf->file = 0;
	buf->disk->nsamp = 0 ; 
	buf->disk->samprate = buf->mem->samprate ;
	buf->disk->t0 = buf->mem->t0 ;
	n = buf->disk->t0 / buf->params->segment_size ; 
	buf->tmax = (n+1) * buf->params->segment_size ;
    }
    return retcode;
}

double 
get_last_dbtimes (Dbptr db)
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

    min_after = VERY_LARGE_DOUBLE ;
    dbquery ( dbg, dbRECORD_COUNT, &nstachan ) ; 
    dbex_compile ( dbg, "max(endtime)", &expr, dbREAL ) ;
    for ( dbg.record = 0 ; dbg.record < nstachan ; dbg.record++ ) {
	dbgetv ( dbg, 0, "sta", &sta, "chan", &chan, 0 ) ; 
	if ( dbex_eval(dbg, expr, 0, &after ) < 0 ) 
	    die ( 0, "failed to read maximum endtime for %s_%s\n", 
		sta, chan ) ; 
	min_after = MIN(after, min_after) ; 
    }

    dbex_free ( expr ) ; 
    freetbl(keys, 0) ;
    return min_after ;
}

void usage ()
{
    fprintf (stderr, "usage: %s [-O] [-c] [-g] [-m srcmatch] [-i interval] [-v] orb db [start-time [window]]\n", Program_Name);
    exit (1);
}
