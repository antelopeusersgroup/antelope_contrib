#include <stdlib.h>
#include <unistd.h>

#include "Pkt.h"
#include "stock.h"
#include "coords.h"
#include "xtra.h"
#include "tr.h"

extern int UNSEED ( char *seed, int size, Steim **confp, double *time, double *samprate, int *nsamp, int **outp, int *datasz );

int
liss2orbpkt ( char *seed, int size, char *database, 
	char *srcname, double *time, char **packet, int *nbytes, int *bufsize ) 
{
    int zero = 0, two=2 ;
    int needed ;
    char *cp ;
    double samprate ;
    static Bns *bns = 0 ;
    double calib, calper ; 
    char segtype[4] ;
    char sta[16], chan[16] ;
    static int *data, datasz=0 ;
    int nsamp ;
    Steim *conf ;
    Srcname parts ;
    int retcode = 0 ;

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
	    join_srcname ( &parts, srcname) ; 

	    N2H4 (cp, &zero, 1);
	    cp += 4 ;
	    N2H4 (cp, &two, 1);
	    cp += 4 ;

	    HI2NS (cp, &nsamp, 1);
	    cp += 2 * 1;

	    cp += 2;  
	    HD2NF (cp, &samprate, 1);
	    cp += 4 ;

	    map_seed_netsta ( conf->sdh.net, conf->sdh.sta, sta ) ;
	    map_seed_chanloc ( sta, conf->sdh.chan, conf->sdh.loc, chan ) ;

	    dbget_calib ( sta, chan, *time, database, &calib, &calper, segtype );
	    N2H4 (cp, &calib, 1);
	    cp += 4 * 1;
	    N2H4 (cp, &calper, 1);
	    cp += 4 * 1;   
	    memcpy (cp, segtype, 1);
	    cp += 1 * 1;

	    if ( bns == 0 ) {
		bns = bnsnew(-1, 1024) ;
	    }
	    memcpy (cp, seed, size) ;
	    cp += size ;
	    *nbytes = cp-*packet ; 
	    freesteim(conf) ;
	    break ;

	case -2:  /* got garbage */
	    complain ( 0, "Can't decode this packet:"  ) ;
	    hexdump ( stderr, seed, size ) ;
	    break; 

	default:
	case -1:
	    complain ( 0, "failed to decode packet." ) ; 
	    hexdump ( stderr, seed, size ) ;
	    break; 

	case -4: /* log data */
	    break ;

    }
    return retcode ;
}
