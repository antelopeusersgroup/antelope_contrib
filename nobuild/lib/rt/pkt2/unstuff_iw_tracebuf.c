/* Routine to unstuff Iceworm trace-buf packets to Datascope orb packets 
 *
 * Kent Lindquist, with instructions from Danny Harvey
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * March, 1997
 */

#include <sys/types.h>
#include <netinet/in.h>
 
#include "pkt2.h"

#define STREQ(a, b) (strcmp((a), (b)) == 0)

  
double htond();
double ntohd();

static int
split_netstachan (netstachan, net, sta, chan)

char *            netstachan;
char *                        net;
char *                             sta;
char *                                  chan;

{
	char *ptr;
	int i;

	*net = '\0';
	*sta = '\0';
	*chan = '\0';
	ptr = net;
	for (i=0; i<strlen(netstachan); i++) if (netstachan[i] == '_') break; else {*ptr = netstachan[i]; ptr++;}
	*ptr = '\0';
	ptr = sta;
	for (i++; i<strlen(netstachan); i++) if (netstachan[i] == '_') break; else {*ptr = netstachan[i]; ptr++;}
	*ptr = '\0';
	ptr = chan;
	for (i++; i<strlen(netstachan); i++) if (netstachan[i] == '_') break; else {*ptr = netstachan[i]; ptr++;}
	*ptr = '\0';
}

int unstuff_iw_tracebuf( time, srcid, packet, Pkt, par )
double time;
char *srcid;
char *packet;
struct Packet **Pkt;
void *par;

{
    struct PreHdr *prehdr;    
    struct PktChannel *pktchan;
    int new_pktchannel;
    int i;
    int size;
    union {
	char *c;
	int  *i;
	short *s;
	float *f;
    } data_in;
    int *data_out;
    char *iwhdr;
    float samprate, calib;
    unsigned short pinno, nsamp, quality;
    char datatype[4];

    prehdr = ( struct PreHdr * ) packet;
    iwhdr = (char *)  ( packet + sizeof( struct PreHdr ) );
    
    if( *Pkt == 0 ) *Pkt = newpkt();
 
    (*Pkt)->pkttype = ntohs (prehdr->pkttype);
    (*Pkt)->hdrtype = ntohs (prehdr->hdrtype);
    (*Pkt)->nchannels = 1;
    size = ntohs (prehdr->pktsiz) - ntohs (prehdr->hdrsiz);

    if( (*Pkt)->chan == 0 ) (*Pkt)->chan = newtbl( 0 );
    if( maxtbl( (*Pkt)->chan ) == 0 )
    {
       allot ( PktChannel *, pktchan, 1) ;
       settbl( (*Pkt)->chan, 0, (char *) pktchan ) ;
       new_pktchannel = 1;
       strcpy (pktchan->segtype, "V");
    }
    else
    {
       pktchan = ( struct PktChannel * ) gettbl( (*Pkt)->chan, 0 );
       new_pktchannel = 0;
    }

    memcpy (&samprate, iwhdr, sizeof(float));
    iwhdr += sizeof(float);
    memcpy (&calib, iwhdr, sizeof(float));
    iwhdr += sizeof(float);
    memcpy (&pinno, iwhdr, sizeof(unsigned short));
    iwhdr += sizeof(unsigned short);
    memcpy (&nsamp, iwhdr, sizeof(unsigned short));
    iwhdr += sizeof(unsigned short);
    memcpy (datatype, iwhdr, 2);
    datatype[2] = '\0';
    iwhdr += 2;
    memcpy (&quality, iwhdr, sizeof(unsigned short));
    iwhdr += sizeof(unsigned short);

    data_in.c = iwhdr;

    split_netstachan (srcid, pktchan->net, pktchan->sta, pktchan->chan);
    pktchan->time = time;
    ntohfp ( &samprate, &samprate );
    pktchan->samprate = samprate;
    ntohfp ( &calib, &calib );
    pktchan->calib = calib;
    pktchan->nsamp = ntohs( nsamp );

    pktchan->datatype = trINT;

    if( new_pktchannel )
    {
       allot ( int *, data_out, pktchan->nsamp );
       pktchan->data = (void *) data_out;
       pktchan->nbytes = pktchan->nsamp * sizeof( int );
    }
    else if( pktchan->nbytes < pktchan->nsamp * sizeof( int ) )
    {
       data_out = (int *) pktchan->data;
       reallot( int *, data_out, pktchan->nsamp );
       pktchan->data = (void *) data_out;
       pktchan->nbytes = pktchan->nsamp * sizeof( int );
    }
    else
    {
       data_out = (int *) pktchan->data;
       /* leave pktchan->nbytes untouched */
    }


    if( STREQ( datatype, "s4" ) )
    {
       for( i = 0; i < pktchan->nsamp; i++ )
       {
	  data_out[i] = ntohl( data_in.i[i] );
       }
    }
    else if( STREQ( datatype, "s2" ) )
    {
       for( i = 0; i < pktchan->nsamp; i++ )
       {
	  data_out[i] = (int) ntohs( data_in.s[i] );
       }
    }
    else if( STREQ( datatype, "t4" ) )
    {
       for( i = 0; i < pktchan->nsamp; i++ )
       {
	  data_out[i] = (int) ntohl( data_in.f[i] );
       }
    }
    else if( STREQ( datatype, "gc" ) )
    {
    	if (genuncompress (&data_out, &(pktchan->nsamp), &(pktchan->nbytes), (unsigned char *) data_in.c, size) < 0) {
    		elog_log(0, "unstuff_iw_tracebuf: genuncompress() error.\n");
    		return 0;
    	}
	pktchan->data = (void *) data_out;

    	for (i=1; i<pktchan->nsamp; i++) data_out[i] += data_out[i-1];
    }

    return 1; 
}

/* $Id$ */
