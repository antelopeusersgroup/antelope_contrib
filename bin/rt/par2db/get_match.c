/* @(#)get_match.c	1.1 12/26/96  */

#include "par2db.h"
Arr    *Selected = 0,
       *Rejected;

Db_buffer *
get_match (PktChannel *achan,
	   Save_params *params,
	   struct re_pattern_buffer *acc_re)
{
    char            net_sta_chan[ORBSRCNAME_SIZE * 2];
    Db_buffer      *buffer;
    struct re_registers *regs = 0;

    if (Selected == 0) {
	Selected = newarr (0);
	Rejected = newarr (0);
    }
    sprintf (net_sta_chan, "%s_%s_%s", achan->net, achan->sta, achan->chan);

    buffer = (Db_buffer *) getarr (Selected, net_sta_chan);
    if (buffer == 0) {
	if (!getarr (Rejected, net_sta_chan)) {
	    int len ;
	    len = strlen (net_sta_chan);
	    if (acc_re == 0
		  || re_match (acc_re, net_sta_chan, len, 0, regs) == len) {
		buffer = new_db_buffer (achan, params);
		setarr (Selected, net_sta_chan, buffer) ;
	    } else {
		setarr (Rejected, net_sta_chan, (void *) -1);
	    }
	}
    }
    return buffer;

}
