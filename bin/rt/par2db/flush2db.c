/* @(#)flush2db.c	1.1 12/26/96  */

#include "par2db.h"

void 
flushrecord ( Db_buffer *buf ) 
{
    buf->db.record = dbALL ;
    if ( fclose ( buf->file ) != 0 ) {
	die ( 1, "Couldn't close output file '%s'\n", buf->path ) ; 
    }
    free(buf->path) ; 
    buf->path = 0 ; 
}

int
flush2db (Db_buffer *buf, int finish)
{
    int             written;
    Data_segment    *mem, *disk ; 
    double	     t1 ;

    mem = buf->mem ;
    disk = buf->disk ;
    if ( mem->nsamp < 1 ) 
	return 0 ; 

    if (buf->db.record < 0) {
	if ( new_dbrecord (buf) ) 
	    die (0, "Couldn't add new record to database.\n" ) ; 
    } else { 
	char *s ;
	if (!TRSAMERATE (disk->samprate, mem->samprate)
	 || !TRSAMETICKS (disk->t0, mem->t0, disk->samprate)
	 || !TRCONTIGUOUS(disk->t0, mem->t0, disk->samprate, disk->nsamp)){
	    if (!TRSAMERATE (disk->samprate, mem->samprate)) {
		complain ( 0, "sample rate changed from %.3f to %.3f for %s_%s_%s at %s\n", 
			disk->samprate, mem->samprate, 
			buf->net, buf->sta, buf->chan, s=strtime(mem->t0) ) ;
		free(s) ;
	    }
/*
	    if ( !TRSAMETICKS (disk->t0, mem->t0, disk->samprate) ) { 
		complain ( 0, "tick registration changed for %s_%s_%s at %s\n", 
			buf->net, buf->sta, buf->chan, s=strtime(mem->t0) ) ;
		free(s) ;
	    }
*/
	    flushrecord ( buf ) ; 
	    if ( new_dbrecord (buf) ) 
		die (0, "Couldn't add new record to database.\n" ) ; 
	 }
    }

    switch (buf->params->datacode) {

    case trINT:
	if ((written = fwrite (buf->mem->data, sizeof(Segsample), 
		buf->mem->nsamp, buf->file)) != buf->mem->nsamp) {
	    die (1, 
		"Could only write %d of %d samples to %s.\n",
		  written, buf->mem->nsamp, buf->path ) ; 
	}
	if ( fflush(buf->file) != 0 ) 
	    die ( 1, "Can't flush %s\n", buf->path ) ; ;
	break;

    default:
	die (0, "Unsupported or unrecognized datacode %d\n", 
	    buf->params->datacode);
	break;
    }

    disk->nsamp += mem->nsamp ; 

    if ( dbputv ( buf->db, 0, 
	"nsamp", disk->nsamp,
	"endtime", ENDTIME(disk->t0, disk->samprate, disk->nsamp), 
	0 ) < 0 ) 
	die (0, "Couldn't write to database\n") ; 

    t1 = SAMP2TIME ( disk->t0, disk->samprate, disk->nsamp ) ; 
    if ( finish || t1 >= buf->tmax ) {
	flushrecord ( buf ) ; 
    }

    return 0;
}
