/**************************************************************************
 *
 *
 *
 *************************************************************************/
#include "extrd.h"

static int      comp_format[] = {0, 10, 11, 20};
extern int Seq;

Steim *init_steim( SegData *segment)
{
    Steim *steim ;
 
    steim = newsteim ();
    steim->level = 2;  
    steim->pvt = (void *) segment;
 
    szcopy (steim->sdh.sta, segment->sta, STA_LEN);
    szcopy (steim->sdh.chan, segment->chan, CHAN_LEN);
    szcopy (steim->sdh.loc, "", LOC_LEN) ;
    szcopy (steim->sdh.net, segment->net, NET_LEN);
 
    steim->sdh.seq = Seq;
    steim->sdh.activity_flags = 0;
    steim->sdh.io_flags = 0;
    steim->sdh.quality_flags = 0;
    steim->sdh.time_correction = 0;
    steim->sdh.blks_following = 2;
    steim->sdh.next_blockette_offset = 48;
    steim->sdh.data_offset = 128;
 
    steim->s1000.next_blockette_offset = 64;
    steim->s1000.dataformat = comp_format[steim->level];
    steim->s1000.sparc_order = 1;
    steim->s1000.log2_record_length =
        (int) (log ((double) steim->record_size) / log (2.0) + 0.5);
 
    steim->s100.type = 100;
    steim->s100.next_blockette_offset = 0;
    steim->s100.samprate = segment->samprate ;
 
    return steim ;

}

static int fill_header (conf, first, last, segment)
Steim   *conf;
int     first;
int     last;
SegData   *segment;
{                  
    double  dtime;
    double  sec;
 
    conf->sdh.nsamp = last - first;
    dtime = SAMP2TIME(segment->time, segment->samprate, first ) ;
 
    e2h ( dtime, &conf->sdh.year, &conf->sdh.doy,
            &conf->sdh.hour, &conf->sdh.minutes, &sec) ;
 
    conf->sdh.sec = (int) sec;
    conf->sdh.msec_x_10 = (int) ((sec - conf->sdh.sec) * 1.0e4 + 0.5 ) ;
  
    samprate2seed ( segment->samprate,
        &conf->sdh.samprate_factor, &conf->sdh.samprate_multiplier);
 
    fill_SDH (&conf->sdh, conf->record) ;
    fill_S1000 (&conf->s1000, conf->record+48) ;
    fill_S100 (&conf->s100, conf->record+64) ;

    return 0;
}


int
save_seed ( Steim *conf, int first, int last)
{
    SegData  *segment;
    int nbytes = 0;
    int extra=0;
    
    segment = ( SegData *) conf->pvt;

    fill_header (conf, first, last, segment);

    if ( (nbytes = fwrite (conf->record, 1, conf->record_size, segment->fp )) <= 0) {
	elog_complain(1, "Couldn't save seed data\n");
	return -1;
    }
    if ( fflush(segment->fp) ) 
	elog_die( 1, "Can't flush data file.\n" ) ;
    
    conf->sdh.seq++;
    Seq = conf->sdh.seq;
    segment->sbytes += nbytes;                 
    Foff += nbytes;                 

#ifdef DEBUG
fprintf( stderr, "steim: %s_%s_%s: %lf %lf %d\n", 
segment->net, segment->sta, segment->chan, segment->time, segment->endtime, last );
#endif

    segment->nsamp = last;

    if( segment->new ) {
       segment->new = 0;
       new_db( segment );
    }
    if ( dbputv ( segment->db, 0, 
	"time", segment->time,
	"nsamp", segment->nsamp,
	"endtime", segment->endtime, 
	0 ) < 0 ) 
	elog_die(0, "Couldn't write to database\n") ; 

    return 0;
}

