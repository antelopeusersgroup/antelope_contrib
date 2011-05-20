
/* This attempts to put all the awful seed related routines in one 
   file.
*/

#include <stdio.h>
#include <math.h>
#include "coords.h"
#include "tr.h"
#include "ant_steim.h"

static int
ascii2int (int *result, char *s, int n)
{
    char            buf[101];
    strncpy (buf, s, MIN (n, 100));
    buf[MIN (n, 100)] = 0;
    *result = atoi (buf);
    return 0;
}

static int
buf2float (float *dst, char *src)
{
    union {
	int i ;
	float f ; 
    } copy ;

    copy.i = 0 ; 

    copy.i |= ((*src++) & 0xff) << 24 ; 
    copy.i |= ((*src++) & 0xff) << 16 ; 
    copy.i |= ((*src++) & 0xff) << 8 ; 
    copy.i |= ((*src) & 0xff) ; 

    *dst = copy.f ; 
    return 0 ; 
}

static int
buf2int_unsigned (int *dst, char *src, int n)
{
    int copy ;

    copy = 0 ; 
    switch (n) {
	case 4:
	    copy |= ((*src++) & 0xff) << 24 ; 
	    copy |= ((*src++) & 0xff) << 16 ; 
	    copy |= ((*src++) & 0xff) << 8 ; 
	    copy |= ((*src) & 0xff) ; 
	    break ;

	case 3:
	    copy |= ((*src++) & 0xff) << 16 ; 
	    copy |= ((*src++) & 0xff) << 8 ; 
	    copy |= ((*src) & 0xff) ; 
	    break ;

	case 2 :
	    copy |= ((*src++) & 0xff) << 8 ; 
	    copy |= ((*src) & 0xff) ; 
	    break ;
	
	case 1 :
	    copy |= ((*src) & 0xff) ; 
	    break ;

	default : 
	    elog_die( 0, "Bad call to buf2int_unsigned: n=%d\n", n ) ; 
    }
    *dst = copy ; 
    return 0 ; 
}

static int
buf2int (int *dst, char *src, int n)
{
    int copy ;

    copy = 0 ; 
    switch (n) {
	case 4:
	    copy |= ((*src++) & 0xff) << 24 ; 
	    copy |= ((*src++) & 0xff) << 16 ; 
	    copy |= ((*src++) & 0xff) << 8 ; 
	    copy |= ((*src) & 0xff) ; 
	    break ;

	case 3:
	    copy |= ((*src++) & 0xff) << 16 ; 
	    copy |= ((*src++) & 0xff) << 8 ; 
	    copy |= ((*src) & 0xff) ; 
	    if ( copy & 0x800000 ) 
		copy |= ~ 0xffffff ;
	    break ;

	case 2 :
	    copy |= ((*src++) & 0xff) << 8 ; 
	    copy |= ((*src) & 0xff) ; 
	    if ( copy & 0x8000 ) 
		copy |= ~ 0xffff ;
	    break ;
	
	case 1 :
	    copy |= ((*src) & 0xff) ; 
	    if ( copy & 0x80 ) 
		copy |= ~ 0xff ;
	    break ;

	default : 
	    elog_die( 0, "Bad call to buf2int: n=%d\n", n ) ; 
    }
    *dst = copy ; 
    return 0 ; 
}

static int 
int2buf (char *dst, int value, int n)
{
    switch (n) {
	case 4:
	    *dst++ = (unsigned char) (value >> 24 & 0xff ) ; 
	    /*FALLTHRU*/

	case 3:
	    *dst++ = (unsigned char) (value >> 16 & 0xff ) ; 
	    /*FALLTHRU*/

	case 2 :
	    *dst++ = (unsigned char) (value >> 8 & 0xff ) ; 
	    /*FALLTHRU*/
	
	case 1 :
	    *dst++ = (unsigned char) (value & 0xff ) ; 
	    break ;

	default : 
	    elog_die( 0, "Bad call to int2buf: n=%d\n", n ) ; 
    }
    return 0 ; 
}

static int 
float2buf (char *dst, float value)
{
    union {
	float f ; 
	int   i ; 
    } v ; 
    v.f = value ;

    *dst++ = (unsigned char) (v.i >> 24 & 0xff ) ; 
    *dst++ = (unsigned char) (v.i >> 16 & 0xff ) ; 
    *dst++ = (unsigned char) (v.i >> 8 & 0xff ) ; 
    *dst++ = (unsigned char) (v.i & 0xff ) ; 

    return 0 ; 
}

#if 0
#define MAX2BYTES 32767

int
samprate2seed (double samprate, int *factor, int *multiplier)
{
    double          top,
                    bottom ; 
    int             scale;
    double          rate2;

    if (samprate < 1.) {
	top = 1.0 / samprate;
    } else {
	top = samprate;
    }
    bottom = 1.0 ;

    if ( top > MAX2BYTES ) { 
	scale = top/MAX2BYTES ;
	top /= scale ; 
	bottom = scale ;

	if ( samprate < 1 ) { 
	    *multiplier = -bottom ;
	    *factor = -top ;
	} else { 
	    *multiplier = bottom ;
	    *factor = top ;
	}
    } else { 
	scale = MAX2BYTES/top ;

	top *= scale ;
	bottom *= scale ;

	if ( samprate < 1 ) { 
	    *multiplier = bottom ;
	    *factor = -top ;
	} else { 
	    *multiplier = -bottom ;
	    *factor = top ;
	}

    }

    if (seed2samprate (*factor, *multiplier, &rate2)
	    || ABS(1.0 - rate2/samprate) > .0001) {
	elog_complain(0, "Bad sample rate conversion: samprate = %10.3f factor=%d multiplier=%d new rate=%10.3f\n",
		  samprate, *factor, *multiplier, rate2);
	return -1;
    }
    return 0;
}

int
seed2samprate (int factor, int multiplier, double *samprate)
{
    if ((factor > 0) && (multiplier > 0))
	*samprate = (double) factor *(double) multiplier;
    else if ((factor > 0) && (multiplier < 0))
	*samprate = -(double) factor / (double) multiplier;
    else if ((factor < 0) && (multiplier > 0))
	*samprate = -(double) multiplier / (double) factor;
    else if ((factor < 0) && (multiplier < 0))
	*samprate = 1.0 / ((double) multiplier * (double) factor);
    else {
	elog_log(0, "seed2samprate: bad values: factor=%d, multiplier=%d\n",
			factor, multiplier);
	*samprate = 1.0;
	return -1;
    }
    return 0;
}

#endif

int
fill_SDH (SDH *blk, char *buffer)
{
    int             retcode = 0;

    sprintf (buffer,
	     "%06dD %-5.5s%-2.2s%-3.3s%-2.2s",
	     blk->seq,
	     blk->sta,
	     blk->loc,
	     blk->chan,
	     blk->net);
    buffer += 20;

    int2buf (buffer, blk->year, 2);
    buffer += 2;
    int2buf (buffer, blk->doy, 2);
    buffer += 2;
    int2buf (buffer, blk->hour, 1);
    buffer++;
    int2buf (buffer, blk->minutes, 1);
    buffer++;
    int2buf (buffer, blk->sec, 1);
    buffer++;
    memset (buffer, 0, 1);
    buffer++;
    int2buf (buffer, blk->msec_x_10, 2);
    buffer += 2;

    int2buf (buffer, blk->nsamp, 2);
    buffer += 2;
    int2buf (buffer, blk->samprate_factor, 2);
    buffer += 2;
    int2buf (buffer, blk->samprate_multiplier, 2);
    buffer += 2;
    int2buf (buffer, blk->activity_flags, 1);
    buffer++;
    int2buf (buffer, blk->io_flags, 1);
    buffer++;
    int2buf (buffer, blk->quality_flags, 1);
    buffer++;
    int2buf (buffer, blk->blks_following, 1);
    buffer++;
    int2buf (buffer, blk->time_correction, 4);
    buffer += 4;
    int2buf (buffer, blk->data_offset, 2);
    buffer += 2;
    int2buf (buffer, blk->next_blockette_offset, 2);
    buffer += 2;
    return retcode;
}

int
fill_S1000 (S1000 *blk, char *buffer)
{
    int             retcode = 0;

    int2buf (buffer, 1000, 2);
    buffer += 2;
    int2buf (buffer, blk->next_blockette_offset, 2);
    buffer += 2;
    int2buf (buffer, blk->dataformat, 1);
    buffer++;
    int2buf (buffer, blk->sparc_order, 1);
    buffer++;
    int2buf (buffer, blk->log2_record_length, 1);
    buffer++;

    return retcode;
}

int
fill_S100 (S100 *blk, char *buffer)
{
    int             retcode = 0;

    int2buf (buffer, 100, 2);
    buffer += 2;
    int2buf (buffer, blk->next_blockette_offset, 2);
    buffer += 2;
    float2buf (buffer, blk->samprate);
    buffer += 4;
    int2buf (buffer, blk->flags, 1);
    buffer += 1 ;
    int2buf (buffer, blk->reserved, 3);
    buffer += 3 ;

    return retcode;
}

int
parse_SDH (char *buffer, SDH *blk)
{
    int             retcode = 0;
    double	    sec ;

    retcode += ascii2int (&blk->seq, buffer, 6);
    buffer += 6;
    retcode += ((*buffer == 'D' || *buffer == 'R' || *buffer == 'Q') ? 0 : 1);
    buffer++;
    retcode += ((*buffer == ' ') ? 0 : 1);
    buffer++;
    copystrip (blk->sta, buffer, STA_LEN);
    buffer += STA_LEN;
    copystrip (blk->loc, buffer, LOC_LEN);
    buffer += LOC_LEN;
    copystrip (blk->chan, buffer, CHAN_LEN);
    buffer += CHAN_LEN;
    copystrip (blk->net, buffer, NET_LEN);
    buffer += NET_LEN;

    buf2int_unsigned (&blk->year, buffer, 2);
    buffer += 2;
    buf2int_unsigned (&blk->doy, buffer, 2);
    buffer += 2;
    buf2int_unsigned (&blk->hour, buffer, 1);
    buffer++;
    buf2int_unsigned (&blk->minutes, buffer, 1);
    buffer++;
    buf2int_unsigned (&blk->sec, buffer, 1);
    buffer++;
    buffer++;
    buf2int_unsigned (&blk->msec_x_10, buffer, 2);
    buffer += 2;

    buf2int_unsigned (&blk->nsamp, buffer, 2);
    buffer += 2;
    buf2int (&blk->samprate_factor, buffer, 2);
    buffer += 2;
    buf2int (&blk->samprate_multiplier, buffer, 2);
    buffer += 2;
    buf2int_unsigned (&blk->activity_flags, buffer, 1);
    buffer++;
    buf2int_unsigned (&blk->io_flags, buffer, 1);
    buffer++;
    buf2int_unsigned (&blk->quality_flags, buffer, 1);
    buffer++;
    buf2int_unsigned (&blk->blks_following, buffer, 1);
    buffer++;
    buf2int_unsigned (&blk->time_correction, buffer, 4);
    buffer += 4;
    buf2int_unsigned (&blk->data_offset, buffer, 2);
    buffer += 2;
    buf2int (&blk->next_blockette_offset, buffer, 2);
    buffer += 2;

    sec = (double) blk->sec + blk->msec_x_10 / 10000.0 ; 
    blk->epoch = h2e ( blk->year,
			blk->doy, 
			blk->hour, 
			blk->minutes, 
			sec) ;

    if ( seed2samprate (blk->samprate_factor, blk->samprate_multiplier, 
	&blk->samprate )) retcode++ ;
    return retcode;
}

int
parse_S1000 (char *buffer, S1000 *blk)
{
    int             retcode = 0;

    buf2int_unsigned (&blk->type, buffer, 2);
    buffer += 2;
    if (blk->type != 1000) {
	retcode++;
    } else { 
	buf2int_unsigned (&blk->next_blockette_offset, buffer, 2);
	buffer += 2;
	buf2int_unsigned (&blk->dataformat, buffer, 1);
	buffer++;
	buf2int_unsigned (&blk->sparc_order, buffer, 1);
	buffer++;
	if (blk->sparc_order != 1)
	    retcode++;
	buf2int_unsigned (&blk->log2_record_length, buffer, 1);
	buffer++;
    }
    return retcode;
}

int
parse_S100 (char *buffer, S100 *blk)
{
    int             retcode = 0;
    float	    samprate ;

    buf2int_unsigned (&blk->type, buffer, 2);
    buffer += 2;
    if (blk->type != 100) {
	retcode++;
    } else {
	buf2int_unsigned (&blk->next_blockette_offset, buffer, 2);
	buffer += 2;
	buf2float (&samprate, buffer);
	blk->samprate = samprate ;
	buffer += 4;
	buf2int_unsigned (&blk->flags, buffer, 1);
	buffer++;
	buf2int_unsigned (&blk->reserved, buffer, 3);
	buffer++;
    }
    return retcode;
}

int
fill_seed_header (Steim *conf, long n0, long n1)
{
    double          time;
    double          sec;
    SeedDataHeader *pvt;
    int		    offset ;

    pvt = (SeedDataHeader *) conf->pvt ;

    conf->sdh.nsamp = n1 - n0;
    time = SAMP2TIME(pvt->time, pvt->samprate, n0) ;

    time = d4round ( time ) ; 
    e2h (time, 
	    &conf->sdh.year,
	    &conf->sdh.doy, 
	    &conf->sdh.hour, 
	    &conf->sdh.minutes, 
	    &sec) ;

    conf->sdh.sec = (int) sec;
    conf->sdh.msec_x_10 = (int) ((sec - conf->sdh.sec) * 1.0e4 + 0.5 ) ; 

    samprate2seed (pvt->samprate, 
	&conf->sdh.samprate_factor, &conf->sdh.samprate_multiplier);

    offset = MSD_FIXED_DATA_SIZE ; 
    conf->sdh.blks_following = (conf->has_s1000 ? 1 : 0) + (conf->has_s100 ? 1 : 0) ;
    if ( conf->sdh.blks_following ) {
	conf->sdh.next_blockette_offset = offset ; 
    }
    fill_SDH (&conf->sdh, conf->record) ;

    if ( conf->has_s1000 ) { 
	if ( conf->has_s100 ) { 
	    offset += 16 ; 
	    conf->s1000.next_blockette_offset = offset ;
	} else { 
	    conf->s1000.next_blockette_offset = 0 ; 
	}
	fill_S1000 (&conf->s1000, conf->record+MSD_FIXED_DATA_SIZE) ;
    }
    if ( conf->has_s100 ) { 
	conf->s1000.next_blockette_offset = 0 ;
	fill_S100 (&conf->s100, conf->record+64) ;
    }

    return 0;
}

int parse_seed_data_header ( Steim *conf ) 
{
    int retcode = 0 ; 
    int next_blockette_offset, type ;

    retcode = parse_SDH (conf->record, &conf->sdh ) ;
    conf->has_s100 = conf->has_s1000 = 0 ; 
    next_blockette_offset = conf->sdh.next_blockette_offset ;
    while ( next_blockette_offset > 0 
	&& next_blockette_offset < conf->record_size-100 ) {
	buf2int_unsigned (&type, conf->record+next_blockette_offset, 2);
	switch ( type ) { 
	    case 1000 :
		conf->has_s1000 = 1;
		retcode |= parse_S1000 (conf->record+next_blockette_offset, 
				&conf->s1000 ) ;
		conf->record_size = 
		    pow(2.0, (double) conf->s1000.log2_record_length) ;
		conf->frames_per_record = 
		    (int) (conf->record_size - conf->sdh.data_offset)
				/ 64.0 ; 
		break ;

	    case 100 :
		conf->has_s100 = 1;
		retcode |= parse_S100 (conf->record+next_blockette_offset, 
				&conf->s100 ) ;
		conf->sdh.samprate = conf->s100.samprate ;
		break ;

	    default :
		break ;
	}
	buf2int_unsigned (&next_blockette_offset, 
			    conf->record+next_blockette_offset+2, 2);
    }

    if ( conf->has_s1000 ) { 
	switch (conf->s1000.dataformat) {
	    case 10 :  conf->level = 1; break ;
	    case 11 :  conf->level = 2; break ;
	    case 19 :  conf->level = 3; break ;
	    default :  conf->level = -1 ; break ; /* not steim encoded */
	}
    } else { 
	retcode = - 1; 
    }

    return retcode;
}

void 
show_seed_conf ( FILE *file, Steim *conf ) 
{

    char *s ; 
    fprintf(file, "record_size = %d\n", conf->record_size ) ;
    fprintf(file, "header: seq = %d time=%s samprate=%.3f\n"
	    "net='%s' sta='%s' chan='%s' loc='%s'\n" 
	    "year=%d doy=%d hour=%d minutes=%d sec=%d msec=%d\n" 
	    "nsamp=%d samprate_factor=%d samprate_multiplier=%d\n"
	    "quality=0x%x blks following=%d time_correction=%d\n",
    	conf->sdh.seq, s=strydtime(conf->sdh.epoch), conf->sdh.samprate,
	conf->sdh.net,
	conf->sdh.sta,
	conf->sdh.chan,
	conf->sdh.loc,
	conf->sdh.year,
	conf->sdh.doy,
	conf->sdh.hour,
	conf->sdh.minutes,
	conf->sdh.sec,
	conf->sdh.msec_x_10,
	conf->sdh.nsamp,
	conf->sdh.samprate_factor,
	conf->sdh.samprate_multiplier,
	conf->sdh.quality_flags,
	conf->sdh.blks_following,
	conf->sdh.time_correction ) ;
    free(s) ;

    if ( conf->has_s1000 ) { 
	fprintf(file, "S1000: dataformat=%d sparc_order=%d log2_record_length=%d\n",
	    conf->s1000.dataformat,
	    conf->s1000.sparc_order,
	    conf->s1000.log2_record_length) ;
    } else { 
	fprintf(file, "no s1000 blockette\n" ) ;
    }

    if ( conf->has_s100 ) { 
	fprintf(file, "S100: samprate=%.3f flags=0x%x\n", 
		conf->s100.samprate, conf->s100.flags ) ; 
    } else { 
	fprintf(file, "no s100 blockette\n" ) ;
    }
}

