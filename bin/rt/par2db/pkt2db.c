/*  @(#)pkt2db.c	1.1 12/26/96  */

#include "par2db.h"

int
pkt2db (char *srcname,
	double pkttime,
	char *packet,
	int sortcode,
	struct re_pattern_buffer * acc_re,
	Save_params * params)
{
    int i, unstuff ; 
    PktChannel *achan ;
    static Packet *unstuffed=0 ;
    Db_buffer *buffer ;
    int retcode = 0 ;
    char *s ;

    switch (unstuff = unstuffpar (packet, pkttime, &unstuffed, srcname )) {
	case 1:
	    for (i = 0; i < unstuffed->nchannels; i++) {
		achan = (PktChannel *) gettbl (unstuffed->chan, i);
		buffer = get_match (achan, params, acc_re ) ; 
		if (buffer != 0) 
		    seg_append (achan, buffer) ;
	    }
	    break;

	case 2:			       /* status packet */
	    break;

	default:
	    retcode = -4;
	    complain (0,
		      "unknown return code %d from unstuffpkt for %s at %s\n",
		      unstuff, srcname, s = strtime (pkttime));
	    free (s);
	    break;
    }

    return retcode;
}
