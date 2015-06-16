#include "diagp.h"


struct DasPreHdr {
    short           hdrsiz;            /* header size */
    short           pktsiz;            /* raw packet size */
    ushort_t        hdrtype;           /* header type  */
    ushort_t        pkttype;           /* packet type tag  */
};

void shrt_swap( ushort_t *input, int number)
{
uchar_t byte[2];
uchar_t temp;
int i;
 
    for (i = 0; i < number; i++) {
        memcpy(byte, input + i, 2);
        temp = byte[0];
        byte[0] = byte[1];
        byte[1] = temp;
        memcpy(input + i, byte, 2);
    }
 
}

#define CHDATA_OFF (4)     /* size of the pre-channel header */
#define CHDATA_LEN_OFF (2) /* offset to a "channel data size" value */


int DAS_gain( ClientData clientData ,
                char *srcname,
                double pkttime,
                uchar_t *packet )
{
    char cmdstr[256]; 
    char errmsg[512], msg[512];
    char key[256];
    int nchan;
    struct DasPreHdr hdr;
    DPars *dp;
    int i, skip_bytes, doff, chbytes;
    uchar_t *tmphdr;
    ushort_t gain, chid, val;
    short tmpval;
    TclOrb *tclorb = (TclOrb *) clientData;
    Srcname parts ; 

	split_srcname(srcname, &parts) ;
     
        memcpy( &key[0], packet, 256);
        tmphdr = (uchar_t *) &key[0];
 
        memcpy( &tmpval, tmphdr, sizeof(short ));
        tmphdr += sizeof(short );
        hdr.hdrsiz =  ntohs(  tmpval );

/*
printf("src=%s size=%d\n", srcname, hdr.hdrsiz);
fflush(stdout);
*/ 
        /* skip to the 'doff'(offset to real data) value in orb header */
        /* skipping: hdrtype+pkttype+pktsize+
		     calib+srate+
                     datatype+nsamp+nchan */      
        skip_bytes = 0;
	skip_bytes =  sizeof(short)+sizeof(short)+sizeof(short)+
                      sizeof(float)+ sizeof(float)+
                      sizeof(short)+sizeof(short)+sizeof(short);
 
        tmphdr += sizeof(short); /* skip hdrtype valuse */
        tmphdr += sizeof(short); /* skip pkttype valuse */
        tmphdr += sizeof(short); /* skip pktsize valuse */
        tmphdr += sizeof(float); /* skip calib valuse */
        tmphdr += sizeof(float); /* skip samprate valuse */
        tmphdr += sizeof(short); /* skip datatype valuse */
        tmphdr += sizeof(short); /* skip nsamp valuse */
        tmphdr += sizeof(short); /* skip nchan valuse */
        
        /* get doff value */
        memcpy( &tmpval, tmphdr, sizeof( short ));
        doff =  ntohs(  tmpval );

        nchan = packet[hdr.hdrsiz+NCHAN];

/*
printf("doff=%d ch=%d\n", doff, nchan);
fflush(stdout);
*/
	if (nchan <= 0 || nchan > 9  ) {
	    sprintf( errmsg, "Wrong number of channels - %d (%0x)!!!", nchan, packet[hdr.hdrsiz+NCHAN] );
	    sprintf( &msg[0], "%s {%s} \0", ERR_PROC, errmsg);
	    Tcl_Eval ( tclorb->interp, msg );
	    while ( Tk_DoOneEvent (TK_DONT_WAIT) )
	         ;
	    return 0;
        }
        doff += hdr.hdrsiz;
 
        for ( i = 0; i < nchan; i++ )  { 
 
           chid = (uchar_t) packet[doff];
           gain = (uchar_t) packet[doff+1];
            
           val = (uchar_t)packet[doff+2]*256 + (uchar_t)packet[doff+3]; 
           chbytes = val;

/*
printf("id=%d gain=%d byte=%d\n", chid, gain, chbytes);
fflush(stdout);
*/
           sprintf (key, "%s_chan%d\0", parts.src_sta, chid );
           dp = ( DPars *) getarr (tclorb->dpars, key);
           if ( dp != 0) {

               if(gain == 2)  
                   sprintf ( &cmdstr[0], 
		   "%s {%s} {x32} {none}\0", tclorb->parcallback, dp->widget  );
               else if(gain == 1) 
                   sprintf ( &cmdstr[0], 
		   "%s {%s} {x1} {none}\0", tclorb->parcallback, dp->widget  );
               else {
	           sprintf( errmsg, "Unknown gain value - %d for channel %d!", gain, chid);
	           sprintf( &msg[0], "%s {%s} \0", ERR_PROC, errmsg);
	           Tcl_Eval ( tclorb->interp, msg );
	           while ( Tk_DoOneEvent (TK_DONT_WAIT) )
	                ;
	           return 0;

               }
               if (tclorb->verbose > 0) {
	             printf ("%s\n", cmdstr );
	             fflush (stdout);
               }
               Tcl_Eval (tclorb->interp, cmdstr );
            }
            doff += CHDATA_OFF+chbytes; 
        }

     return 1;  

}
