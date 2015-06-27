#include "diagp.h"

extern Arr *Das_ID_toName;
extern Arr *DPOrbs;
extern Arr *DP;
extern Arr *BatRec;

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

int DAS_gain( ClientData clientData ,
                char *srcname,
                double pkttime,
                uchar_t *packet )
{
    char cmdstr[256]; 
    char errmsg[512], msg[512];
    char net[32], sta[12], key[64];
    int nchan;
    struct PreHdr hdr;
    DPars *dp;
    int i;
    uchar_t *tmphdr, pkt[2];
    ushort_t gain, sptr[2];
    short tmpval;
    TclOrb *tclorb = (TclOrb *) clientData;
 

        parse_srcname( srcname, &net[0], &sta[0], 0, 0 );
     
        memcpy( &key[0], packet, 64);
        tmphdr = (uchar_t *) &key[0];
 
        memcpy( &tmpval, tmphdr, sizeof(short ));
        tmphdr += sizeof(short );
        hdr.hdrsiz =  ntohs(  tmpval );
 
        memcpy( &tmpval, tmphdr, sizeof( short ));
        tmphdr += sizeof( short );
        hdr.pktsiz =  ntohs(  tmpval );
 
        nchan = packet[hdr.hdrsiz+NCHAN];
	if (nchan <= 0 || nchan > 9  ) {
	    sprintf( errmsg, "Wrong number of channels - %d (%0x)!!!", nchan, packet[hdr.hdrsiz+NCHAN] );
	    sprintf( &msg[0], "%s {%s} \0", ERR_PROC, errmsg);
	    Tcl_Eval ( tclorb->interp, msg );
	    while ( Tk_DoOneEvent (TK_DONT_WAIT) )
	         ;
	    return 0;
        }

        memcpy( (char *) sptr, packet+hdr.hdrsiz+GAIN_OFF, 2); 
        shrt_swap( sptr, 1 );
        gain = *sptr;

        for ( i = 0; i < nchan; i++ )  { 
 
           sprintf (key, "%s_chan%d\0", sta, i+1 );
           dp = ( DPars *) getarr (tclorb->dpars, key);
           if ( dp != 0) {

               if( ( gain >> i) & 0x1 )  
                   sprintf ( &cmdstr[0], 
		   "%s {%s} {x32} {none}\0", tclorb->parcallback, dp->widget  );
               else 
                   sprintf ( &cmdstr[0], 
		   "%s {%s} {x1} {none}\0", tclorb->parcallback, dp->widget  );
               if (tclorb->verbose > 0) {
	             printf ("%s\n", cmdstr );
	             fflush (stdout);
               }
               Tcl_Eval (tclorb->interp, cmdstr );
            }
        }

     return 1;  

}
