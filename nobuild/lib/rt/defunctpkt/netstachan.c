/*  Copyright (c) 1997 Boulder Real Time Technologies, Inc.           */
/*                                                                    */
/*  This software may be used freely in any way as long as            */
/*  the copyright statement above is not removed.                     */

#include <string.h>

#include "defunctpkt.h"
#include "orb.h"
#include "stock.h"

int 
netstachan ( int orb, Arr **channels, Arr **srcnames ) 
{
    int i, nsrc, j ;
    Orbsrc *record, *source ;
    double when ;
    char *chanstr ; 
    int sZchanstr ; 
    Packet         *unstuffed=0;
    int		    rpktid, nbytes, bufsize = 0 ; 
    char	   *packet ;
    char	    pktsrcname[ORBSRCNAME_SIZE] ;
    char            net_sta_chan[ORBSRCNAME_SIZE * 2];
    double	    pkttime ; 
    PktChannel     *achan ;

    if ( orbsources ( orb, &when, &source, &nsrc ) )
	return -1 ; 

    *channels = newarr(0) ;
    *srcnames = newarr(0) ;

    sZchanstr = STRSZ ;
    allot ( char *, chanstr, sZchanstr ) ;

    for (i=0 ; i<nsrc ; i++ ) {
        record = source + i ;
	if (orbget (orb, record->slatest,
		&rpktid, pktsrcname, &pkttime, &packet, &nbytes, &bufsize) == 0 
	    && strcmp(pktsrcname, record->srcname ) == 0 ) {

	    switch (unstuffpkt (pkttime, pktsrcname, packet, &unstuffed)) {
		case 1:
		    *chanstr = 0 ;
		    for (j = 0; j < unstuffed->nchannels; j++) {
			achan = (PktChannel *) gettbl (unstuffed->chan, j);
			sprintf ( net_sta_chan, "%s_%s_%s", 
			    achan->net, achan->sta, achan->chan ) ;
			setarr(*srcnames, net_sta_chan, strdup(pktsrcname) ) ;
			if ( strlen(chanstr) > sZchanstr ) {
			    sZchanstr *= 2 ; 
			    reallot (char *, chanstr, sZchanstr ); 
			}
			if ( j > 0 ) {
			    strcat ( chanstr, " " ) ; 
			}
			strcat (chanstr, net_sta_chan ) ;
		    }
		    setarr ( *channels, pktsrcname, strdup(chanstr)) ;
		    break ;

		default :
		    break ;
	    }
	}
    }
    if ( bufsize > 0 ) {
	free(packet) ; 
    }
    if ( unstuffed != 0 )   
	freePkt(unstuffed) ;
    free(chanstr) ;
    return 0 ; 
}

/* $Id$ */
