/*  @(#)new_free_util.c	1.1 12/26/96  */

#include "par2db.h"

Data_segment   *
new_data_segment (int maxsamp)
{
    Data_segment   *aseg;

    allot (Data_segment *, aseg, 1);
    aseg->t0 = 0.0;
    aseg->samprate = 0.0;
    if ( maxsamp > 0 ) 
	allot (Segsample *, aseg->data, maxsamp);
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
	    /* "net", buf->net, */
	    "sta", buf->sta,
	    "chan", buf->chan,
	    "time", buf->mem->t0,
	    "nsamp", 0,
	    "samprate", buf->mem->samprate,
	    "datatype", buf->params->datatype,
	    0) < 0) {
	    register_error (0, "Couldn't write to table\n");
	    retcode++;
	} else if (trwfname (buf->db, buf->params->wfname, &(buf->path)) < 0) {
	    retcode++;
	} else 
	/*
	if ((buf->file = fopen (buf->path, "a+")) == 0) {
	    register_error (1, "Can't open %s to write trace data.\n", 
		buf->path);
	    retcode++;
	}
	*/
	buf->file = 0;

	buf->disk->nsamp = 0 ; 
	buf->disk->samprate = buf->mem->samprate ;
	buf->disk->t0 = buf->mem->t0 ;
	n = buf->disk->t0 / buf->params->segment_size ; 
	buf->tmax = (n+1) * buf->params->segment_size ;
    }
    return retcode;
}

void
free_data_segment (Data_segment * aseg)
{
    if ( aseg->data != 0)
	free (aseg->data);
    free (aseg);
}

void
free_db_buffer (Db_buffer * abuf)
{
    flush2db ( abuf, 1 ) ;
    free_data_segment (abuf->mem);
    free_data_segment (abuf->disk);
    free (abuf);
}
