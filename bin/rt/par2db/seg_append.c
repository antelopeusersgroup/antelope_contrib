/*  @(#)seg_append.c	1.1 12/26/96  */

#include "par2db.h"
extern int Verbose;

void
init_mem_segment ( PktChannel *new, Db_buffer *buf )
{
    buf->mem->t0 = new->time;
    buf->mem->samprate = new->samprate;
    buf->mem->ngapmax = new->samprate * buf->params->gapmax ;
    buf->mem->nsamp = 0 ;
}

int
seg_append (PktChannel *new, Db_buffer *buf ) 
{
    int             i, 
		    bufindex, bufmax;
    Data_segment *mem ;

    mem = buf->mem ;
    bufindex = TIME2SAMP (mem->t0, mem->samprate, new->time);
    bufmax = MIN(bufindex, mem->maxsamp ) ;

    if (!TRSAMERATE (new->samprate, mem->samprate)
     || !TRSAMETICKS (mem->t0, new->time, mem->samprate)
     || (bufindex - mem->nsamp > mem->ngapmax)) {
	flush2db(buf, 0) ; 
	init_mem_segment ( new, buf ) ; 
	bufindex = 0 ;
     } else {

	for (i = mem->nsamp ; i < bufmax ; i++)
	    mem->data[i] = buf->params->gap_value ;
	mem->nsamp = i ; 

	while ( (bufindex + new->nsamp) > mem->maxsamp ) {
	    flush2db (buf, 0) ;
	    mem->t0 = SAMP2TIME(mem->t0, mem->samprate, mem->nsamp) ; 
	    mem->nsamp = 0 ;
	    bufindex = TIME2SAMP (mem->t0, mem->samprate, new->time);
	    bufmax = MIN(bufindex, mem->maxsamp ) ;
	    for (i = mem->nsamp; i < bufmax ; i++)
		mem->data[i] = buf->params->gap_value ;
	    mem->nsamp = i ; 
	}
     }

    /* copy new data */
    memcpy (mem->data + bufindex, new->data, new->nsamp * sizeof (Segsample));
    mem->nsamp += new->nsamp ; 
    return 0 ;
}
