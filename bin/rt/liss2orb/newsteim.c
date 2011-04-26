/*  Copyright (c) 1997 Boulder Real Time Technologies, Inc.           */
/*                                                                    */
/*  This software module is wholly owned by Boulder Real Time         */
/*  Technologies, Inc. Any use of this software module without        */
/*  express written permission from Boulder Real Time Technologies,   */
/*  Inc. is prohibited.                                               */


#include "tr.h"
#include "ant_steim.h"

Steim *newsteim () {
    Steim *conf ;
    allot(Steim *, conf, 1) ; 
    conf->frames_per_record = 62 ;
    conf->firstframe = 0 ; 
    conf->level = 2; 
    conf->byteswap = 0 ; 
    conf->data_offset = 128 ;
    conf->nrecords = 0 ; 
    conf->n0 = 0 ; 
    conf->n1 = 0 ;
    conf->record_size = 4096 ;
    conf->record = 0 ; 
    conf->nerr = 0 ; 
    conf->ffp = 0 ; 
    conf->x0 = 0 ; 
    conf->xn = 0 ;
    conf->fp = 0 ; 
    conf->new_record_flag = 1 ;
    conf->fcnt = 0 ;
    conf->wp = 0 ;
    conf->wcnt = 0 ;
    conf->data = 0 ; 
    conf->last_x = 0 ;
    conf->cbuf = 0 ;
    conf->maxclass = 0 ; 
    conf->code = 0 ; 
    conf->steimdef = 0 ;
    conf->nsteimdef = 0 ; 
    conf->pvt = 0 ; 
    memset((void *) &conf->sdh, 0, sizeof(conf->sdh)) ; 
    memset((void *) &conf->s1000, 0, sizeof(conf->s1000)) ; 
    memset((void *) &conf->s100, 0, sizeof(conf->s100)) ; 
    conf->s1000.type = 1000 ; 
    conf->s1000.next_blockette_offset = 64 ; 
    conf->s1000.log2_record_length = 12 ;
    conf->s100.type = 100 ;
    conf->has_s100 = 1 ; 
    conf->has_s1000 = 1 ; 
    return conf ; 
}

void freesteim(conf) 
Steim *conf ;
{
    if ( conf->record ) 
	free (conf->record) ; 
    if ( conf->data ) 
	free ( conf->data ) ; 
    if ( conf->cbuf ) 
	freetbl ( conf->cbuf, 0 ) ; 
    free(conf) ; 
}

/* $Id$ */
