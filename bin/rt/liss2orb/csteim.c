/*  Copyright (c) 1997 Boulder Real Time Technologies, Inc.           */
/*                                                                    */
/*  This software module is wholly owned by Boulder Real Time         */
/*  Technologies, Inc. Any use of this software module without        */
/*  express written permission from Boulder Real Time Technologies,   */
/*  Inc. is prohibited.                                               */


#include <stdio.h>
#include <math.h>
#include <errno.h>

#include "tr.h"
#include "ant_steim.h"
#include "swapbytes.h"

#define gettbl(TBL,INDEX) ((TBL)->base.cp+(INDEX)*(TBL)->entry_size)

extern void right_growtbl (Tbl * tbl, long incr) ;

static char * 
ppushtbl ( Tbl *tbl ) 
{
    int index ;
    if ( tbl->active_right >= tbl->right )
        right_growtbl ( tbl, 1 ) ;
    index = tbl->active_right++ ;
    return tbl->base.cp+index*tbl->entry_size ; 
}

static void *
sshifttbl ( tbl ) 
Tbl *tbl ;
{
    char *retval ;

    retval =  tbl->base.cp; 
    
    tbl->active_right-- ; 
    tbl->right-- ; 
    tbl->left-- ; 
    tbl->base.cp += tbl->entry_size ; 

    return retval ;
}

Steimdef steim2[] = { 
/*       bits cnt  code dnib   min           max       mask      sign    extend */
        {  4,   7,   3,   2, ~((1<< 3)-1), (1<< 3)-1, (1<< 4)-1, 1<< 3, ~((1<< 4)-1) },
        {  5,   6,   3,   1, ~((1<< 4)-1), (1<< 4)-1, (1<< 5)-1, 1<< 4, ~((1<< 5)-1) },
        {  6,   5,   3,   0, ~((1<< 5)-1), (1<< 5)-1, (1<< 6)-1, 1<< 5, ~((1<< 6)-1) },
        {  8,   4,   1,   0, ~((1<< 7)-1), (1<< 7)-1, (1<< 8)-1, 1<< 7, ~((1<< 8)-1) },
        { 10,   3,   2,   3, ~((1<< 9)-1), (1<< 9)-1, (1<<10)-1, 1<< 9, ~((1<<10)-1) },
        { 15,   2,   2,   2, ~((1<<14)-1), (1<<14)-1, (1<<15)-1, 1<<14, ~((1<<15)-1) },
        { 30,   1,   2,   1, ~((1<<29)-1), (1<<29)-1, (1<<30)-1, 1<<29, ~((1<<30)-1) }
} ; 
int nsteim2 = sizeof(steim2) / sizeof(Steimdef) ; 
 
Steimdef steim1[] = { 
/*       bits cnt  code dnib   min           max       mask      sign    extend */
        {  8,   4,   1,   0, ~((1<< 7)-1), (1<< 7)-1, (1<< 8)-1, 1<< 7, ~((1<< 8)-1) },
        { 16,   2,   2,   0, ~((1<<15)-1), (1<<15)-1, (1<<16)-1, 1<<15, ~((1<<16)-1) },
	{ 32,   1,   3,   0, 0x80000000,  0x7fffffff, 0xffffffff, 0x80000000, 0 }  
} ; 
int nsteim1 = sizeof(steim1) / sizeof(Steimdef) ; 

static
int stuffw ( Steim *conf, Steimdef *adef, int (*save_record)(), int flush ) 
{
    int i, w ;
    int ncbuf, nbuf ; 
    Sample *asample=0 ;
    int retcode = 0 ;

    if ( conf->new_record_flag ) {
	conf->new_record_flag = 0 ;
	memset ( conf->record, 0, conf->record_size ) ;
	conf->fp = conf->ffp ;
	conf->wp = conf->fp + 3 ;
	conf->wcnt = 3 ;
	asample = (Sample *) gettbl(conf->cbuf, 0) ; 
	*(conf->x0) = asample->x ;
	conf->n0 = conf->n1 ;
    }

#if 0
    printf ( "record=%x ffp=%x fp=%x wp=%x cbuf=%x wcnt=%d wp-fp=%d fcnt=%d (fp-ffp)/16=%d\n", 
	    conf->record, conf->ffp, conf->fp, conf->wp, conf->cbuf, 
	    conf->wcnt,
	    conf->wp-conf->fp, 
	    conf->fcnt, (conf->fp - conf->ffp)/16 ) ; 
    insist ( conf->fp - conf->ffp == 16 * conf->fcnt ) ; 
    insist ( conf->wp-conf->fp == conf->wcnt ) ;
#endif 

    ncbuf = maxtbl(conf->cbuf) ; 
    if ( ncbuf ) {
	nbuf = MIN(ncbuf, adef->cnt ) ; 
	conf->n1 += nbuf ;

	w = 0 ; 
	for(i=0 ; i<nbuf ; i++ ) {
	    asample = (Sample *) sshifttbl ( conf->cbuf ) ;
	    w <<= adef->bits ; 
	    w |= (asample->dx & adef->mask ) ;
	}
	for( ; i<adef->cnt ; i++ ) 
	    w <<= adef->bits ; 

	w |= (adef->dnib << 30) ; 

	conf->code |= (adef->code) << (2*(15-conf->wcnt)) ;

	*((conf->wp)++) = w ;

    }

    if ( ++(conf->wcnt) >= 16 || flush ) {
	conf->wcnt = 1 ; 
	*(conf->fp) = conf->code ;
	conf->fp = (conf->wp)++ ;
	conf->code = 0 ; 
	if ( ++(conf->fcnt) >= conf->frames_per_record || flush ) {
	    if ( asample != 0 ) 
		*(conf->xn) = asample->x ; 
	    else
		/* only happens sometimes when flushing last record */
		*(conf->xn) = conf->last_x ; 

	    conf->fcnt = conf->firstframe ; 

#ifndef WORDS_BIGENDIAN
	    {
	    	int nbytes;
    	    	nbytes = conf->record_size - conf->data_offset;
    	    	H2N4 (conf->record+conf->data_offset, conf->record+conf->data_offset, nbytes/4);
	    }
#endif
	    if (save_record (conf, conf->n0, conf->n1)) {
		elog_log(1, 
		    "Couldn't save record for samples %ld to %ld for %s_%s_%s_%s\n",
		    conf->n0, conf->n1, 
		    conf->sdh.net, conf->sdh.sta, conf->sdh.chan, conf->sdh.loc );
		retcode = -1 ;
	    }
	    conf->new_record_flag = 1 ;
	    (conf->nrecords)++ ; 
	}
    }

    nbuf = maxtbl(conf->cbuf) ; 
    conf->maxclass = 0 ;
    for(i=0 ; i<nbuf ; i++ ) {
	asample = (Sample *) gettbl ( conf->cbuf, i ) ;
	conf->maxclass = MAX (conf->maxclass, asample->aclass) ;
    }

    return retcode ;

}

int
csteim (Steim *conf, int (*save_record)(), int *data, long npts)
{
    Sample	    *asample ;
    long 	    i, j, n ;
    int		    dx ;
    int		    retcode = 0 ; 

    if (conf->record == 0) {
	conf->record_size = STEIM_BYTES_PER_FRAME * conf->frames_per_record 
				+ conf->data_offset;
	allot ( char *, conf->record, conf->record_size ) ; 
	conf->ffp = (int *) (conf->record + conf->data_offset + STEIM_BYTES_PER_FRAME * conf->firstframe) ;
	conf->x0 = conf->ffp + 1 ; 
	conf->xn = conf->ffp + 2 ; 
	conf->cbuf = inittbl ( 0, 0, 1, 0, sizeof(struct Sample)) ;
	switch ( conf->level ) {
	    case 1 : conf->steimdef = steim1 ;
		     conf->nsteimdef = nsteim1 ;
		     break ;

	    case 2 : conf->steimdef = steim2 ;
		     conf->nsteimdef = nsteim2 ;
		     break ;

	    default: elog_log( 0, "Steim level must be 1 or 2, not %d\n", conf->level ) ;
		return -1 ; 
	}

    } 

    if (npts > 0)  {
	for (i=0 ; i<npts ; i++ ) {
	    asample = (Sample *) ppushtbl(conf->cbuf) ;
	    asample->x = data[i] ; 
	    asample->dx = dx = data[i] - conf->last_x ; 

	    for(j=0 ; j<conf->nsteimdef ; j++ ) {
		if ( dx >= conf->steimdef[j].mn && dx <= conf->steimdef[j].mx ) 
		    break ; 
	    }
	    if ( j>= conf->nsteimdef ) {
		if ( conf->nerr == 0 ) 
		    elog_log(0, "The difference %d between %d and %d near sample #%ld cannot be represented in a Steim level 2 compressed record\n", 
		    dx, conf->last_x, asample->x, conf->n1 ) ;
		conf->nerr++ ;
		j-- ;
		/* try to do something reasonable instead of just quitting 
		 * since we can't get there all in one jump, do the best we can
		 * and let the next difference try to make up the difference.
		 */
		if ( asample->x < conf->last_x ) 
		    asample->dx = conf->steimdef[j].mn ; 
		else
		    asample->dx = conf->steimdef[j].mx ; 
		asample->x = conf->last_x + asample->dx ;
	    }
	    conf->last_x = asample->x ;
	    asample->aclass = j ;

	    conf->maxclass = MAX(conf->maxclass, asample->aclass) ;
	    /* pushtbl ( conf->cbuf, asample ) ; */

	    if ( maxtbl(conf->cbuf) >= conf->steimdef[conf->maxclass].cnt ) {
		retcode |= stuffw ( conf, &(conf->steimdef[conf->maxclass]), save_record, 0 ) ;
	    }
	}
    } else {
	while ( (n = maxtbl(conf->cbuf)) > 0 ) {
	    for ( i=conf->maxclass ; i<conf->nsteimdef ; i++ ) {
		if ( n >= conf->steimdef[i].cnt ) {
		    retcode |= stuffw ( conf, &(conf->steimdef[i]), save_record, n == conf->steimdef[i].cnt ) ;
		    break ;
		}
	    }
	}
	if ( ! conf->new_record_flag ) 
		retcode |= stuffw ( conf, &(conf->steimdef[0]), save_record, 1 ) ;
    }

    if ( retcode == 0 ) {
	retcode = conf->nerr ;
	conf->nerr = 0 ; 
    }
    return retcode ;
}
