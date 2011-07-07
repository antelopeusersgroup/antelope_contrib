/************************************************************************
 *
 *  routines to fill mini-seed headers and write seed volumes  
 *
 *
 *************************************************************************/
#include "ref2db.h"

static int      encoding_format[] = {0, 10, 11, 20};
 

Steim *stinit (Ch_data *buf)
{   
    Steim *steim ;  
    
    steim = newsteim ();
    steim->level = 2;
    steim->pvt = (void *) buf;
        
    szcopy (steim->sdh.sta, buf->sta, STA_LEN);  
    szcopy (steim->sdh.chan, buf->chan, CHAN_LEN);
    szcopy (steim->sdh.loc, "", LOC_LEN) ;
    szcopy (steim->sdh.net, buf->net, NET_LEN);
 
    steim->sdh.seq = 1;
    steim->sdh.activity_flags = 0;
    steim->sdh.io_flags = 0;
    steim->sdh.quality_flags = 0;
    steim->sdh.time_correction = 0;
    steim->sdh.blks_following = 2;
    steim->sdh.next_blockette_offset = 48;
    steim->sdh.data_offset = 128;
 
    steim->s1000.next_blockette_offset = 64;
    steim->s1000.dataformat = encoding_format[steim->level];
    steim->s1000.sparc_order = 1;
    steim->s1000.log2_record_length =
        (int) (log ((double) steim->record_size) / log (2.0) + 0.5);
 
    steim->s100.type = 100;
    steim->s100.next_blockette_offset = 0;
    steim->s100.samprate = 0.0 ;
 
    return steim ;
}

 
static int fill_header ( Steim     *conf,
			 int       n0,
			 int       n1,
			 Ch_data   *abuf )
       
{                  
    double     time;
    double     sec;
 
    conf->sdh.nsamp = n1 - n0;
    time = SAMP2TIME(abuf->stime, abuf->samprate, n0) ;
 
    e2h (time, &conf->sdh.year, &conf->sdh.doy, &conf->sdh.hour, &conf->sdh.minutes, &sec) ;
 
    conf->sdh.sec = (int) sec;
    conf->sdh.msec_x_10 = (int) ((sec - conf->sdh.sec) * 1.0e4 + 0.5 ) ;
  
    samprate2seed (abuf->samprate, &conf->sdh.samprate_factor, &conf->sdh.samprate_multiplier);
 
    fill_SDH (&conf->sdh, conf->record) ;
    fill_S1000 (&conf->s1000, conf->record+48) ;
    fill_S100 (&conf->s100, conf->record+64) ;

    return 0;
}


int save_seed ( Steim *conf, 
                int    n0,
                int    n1)
{
    Ch_data      *abuf;


    abuf = (Ch_data *) conf->pvt;
    fill_header (conf, n0, n1, abuf);
    
    if (fwrite (conf->record, conf->record_size, 1, abuf->file) != 1) {
	elog_log(1, "Couldn't save seed data\n");
	return -1;
    }

    if ( fflush(abuf->file) ) 
	elog_die( 1, "Can't flush %s\n", abuf->path ) ;
   
    conf->sdh.seq++;

    if ( dbputv ( abuf->db, 0, 
	"nsamp", n1,
	"endtime", ENDTIME(abuf->stime, abuf->samprate, n1), 
	0 ) < 0 ) 
	elog_die(0, "Couldn't write to database\n") ; 

    return 0;
}

