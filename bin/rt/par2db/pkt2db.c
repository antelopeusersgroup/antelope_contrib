/*  @(#)pkt2db.c	1.1 12/26/96  */

#include "par2db.h"

Arr *Selected = 0;

int
pkt2db ( Packet **Pkt,
        char *srcname,
	double pkttime,
	char *packet,
	Save_params * params)
{
    int i ; 
    PktChannel *achan ;
    Packet *unstuffed ;
    Db_buffer *buffer ;
    char acomp[64];

    if( Selected == 0 )
      Selected = newarr( 0 );

    switch ( unstuffpar (packet, pkttime, Pkt, srcname )) {
	case 1:
	    unstuffed = *Pkt;
	    for (i = 0; i < unstuffed->nchannels; i++) {
		achan = (PktChannel *) gettbl (unstuffed->chan, i);
		sprintf( &acomp[0], "%s_%s_%s\0", 
		   achan->net, achan->sta, achan->chan);
		buffer = (Db_buffer *) getarr( Selected, acomp );
		if (buffer != 0) 
		    seg_append (achan, buffer) ;
		else {
		    buffer = new_db_buffer( achan, params);
		    setarr( Selected, acomp, buffer );
		}
	    }
	    break;

	default:
	    break;
    }

    return 0;
}
