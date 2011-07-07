#include <stdlib.h>
#include <unistd.h>

#include "liss2orb.h"

extern int UNSEED ( char *seed, int size, Steim **confp, double *time, double *samprate, int *nsamp, int **outp, int *datasz );

int
liss2orbpkt ( char *seed, int size, char *database, int remap, 
	char *srcname, double *time, char **packet, int *nbytes, int *bufsize ) 
{
    int zero = 0, two=2 ;
    int needed ;
    char *cp ;
    double samprate ;
    double calib, calper ; 
    float fcalib, fcalper; 
    char segtype[4] ;
    char sta[16], chan[16] ;
    static int *data, datasz=0 ;
    int nsamp ;
    Steim *conf ;
    Srcname parts ;
    int retcode = 0 ;

    if ( strncmp(seed, "000000                                                          ", 64 ) == 0 ) { 
	return -5 ; /* some kind of empty packet to keep connection open? */
    }

    retcode = UNSEED ( seed, size, &conf, time, &samprate, &nsamp, 
		    &data, &datasz ) ; 
    switch ( retcode ) { 

	case 0:
	    needed = 2*size ;
	    SIZE_BUFFER ( char *, *packet, *bufsize, needed ) ;
	    cp = *packet ;

	    strcpy ( parts.src_net, conf->sdh.net ) ;
	    strcpy ( parts.src_sta, conf->sdh.sta ) ;
	    strcpy ( parts.src_chan, conf->sdh.chan ) ;
	    strcpy ( parts.src_loc, conf->sdh.loc ) ;
	    strcpy ( parts.src_suffix, "LISS" ) ; 
	    *parts.src_subcode = 0 ;

	    N2H4 (cp, &zero, 1);
	    cp += 4 ;
	    N2H4 (cp, &two, 1);
	    cp += 4 ;

	    HI2NS (cp, &nsamp, 1);
	    cp += 2 * 1;

	    cp += 2;  
	    HD2NF (cp, &samprate, 1);
	    cp += 4 ;

	    if (database) {
	    	if (map_seed_netsta ( conf->sdh.net, conf->sdh.sta, sta )  < 0) {
			retcode = -1;
			break;
	    	}
	    	if (map_seed_chanloc ( sta, conf->sdh.chan, conf->sdh.loc, chan )  < 0) {
			retcode = -1;
			break;
	    	}
	    }
	    if ( remap ) {
		strcpy ( parts.src_sta, sta ) ;
		strcpy ( parts.src_chan, chan ) ;
		strcpy ( parts.src_loc, "" ) ;
	    }
	    join_srcname ( &parts, srcname) ; 

	    if (database) dbget_calib ( sta, chan, *time, database, &calib, &calper, segtype );
	    else {
		calib = 0.0;
		calper = -1.0;
		strcpy (segtype, "V");
	    }
	    fcalib = calib ;
	    N2H4 (cp, &fcalib, 1);
	    cp += 4 * 1;
	    fcalper = calper ;
	    N2H4 (cp, &fcalper, 1);
	    cp += 4 * 1;   
	    memcpy (cp, segtype, 1);
	    cp += 1 * 1;

	    memcpy (cp, seed, size) ;
	    cp += size ;
	    *nbytes = cp-*packet ; 
	    freesteim(conf) ;
	    break ;

	case -2:  /* got garbage */
	    if (   conf->sdh.samprate_factor != 0 
		|| conf->sdh.samprate_multiplier != 0) {
		elog_complain( 0, "Can't decode this packet:"  ) ;
		hexdump ( stderr, seed, size ) ;
	    }
	    break; 

	default:
	case -1:
	    elog_complain( 0, "failed to decode packet." ) ; 
	    hexdump ( stderr, seed, size ) ;
	    break; 

	case -3: /* unsupported formats */
	case -4: /* log data */
	    break ;

    }
    return retcode ;
}
