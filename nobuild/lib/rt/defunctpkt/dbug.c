/* $Name $Revision$ $Date$  */ 
#include <stdio.h>

#include "_pkt.h"
#include "defunctpkt.h"

int 
st_dbug ( unstuffed, stuffed, nalloc, srcname, time, size ) 
Packet *unstuffed ;
char **stuffed ; 
int *nalloc ;
char *srcname ;
double *time ; 
int *size ;
{
    char *cp ;
    PktChannel *achan ; 
    unsigned short hdrsize, datasize, type;
    int i, perchan ;
    double mintime=1e99 ;

    perchan = 3 * PKT_NAMESIZE + 3 * sizeof(double) + 3 * sizeof(int);
    hdrsize = 3 * sizeof(int) + perchan * unstuffed->nchannels ;
    datasize = 0 ; 
    type = ORBDBUG;
    for (i=0 ; i<unstuffed->nchannels ; i++ ) {
	achan = (PktChannel *) gettbl(unstuffed->chan, i) ;
	datasize += sizeof(int) * achan->nsamp ; 
	mintime = MIN  ( mintime, achan->time ) ; 
    }

    if ( *nalloc < hdrsize + datasize ) {
	*nalloc = hdrsize + datasize ;
	allot ( char *, *stuffed, *nalloc ) ;
    }

    cp = *stuffed ;
    H2N2 (cp, &hdrsize, 1);
    cp += 2;
    H2N2 (cp, &datasize, 1);
    cp += 2;
    H2N2 (cp, &type, 1);
    cp += 2;
    H2N2 (cp, &type, 1);
    cp += 2;
    H2N4 (cp, &(unstuffed->nchannels), 1);
    cp += 4;

    if ( unstuffed->nchannels == 1 ) {
	sprintf ( srcname, "%s_%s_%s/%s", 
	    achan->net, achan->sta, achan->chan, "dbug" ) ;
    } else {
	sprintf ( srcname, "%s_%s/%s", 
	    achan->net, achan->sta, "dbug" ) ;
    }

    for (i=0 ; i<unstuffed->nchannels ; i++ ) {
	achan = (PktChannel *) gettbl(unstuffed->chan, i) ;
	memcpy(cp, achan->net, PKT_NAMESIZE ) ; 
	cp += PKT_NAMESIZE ;
	memcpy(cp, achan->sta, PKT_NAMESIZE ) ; 
	cp += PKT_NAMESIZE ;
	memcpy(cp, achan->chan, PKT_NAMESIZE ) ; 
	cp += PKT_NAMESIZE ;
	H2N8 (cp, &(achan->time), 1);
	cp += 8;
	H2N8 (cp, &(achan->samprate), 1);
	cp += 8;
	H2N8 (cp, &(achan->calib), 1);
	cp += 8;
	H2N4 (cp, &(achan->nsamp), 1);
	cp += 4;
	H2N4 (cp, &(achan->nbytes), 1);
	cp += 4;
	H2N4 (cp, &(achan->datatype), 1);
	cp += 4;
    }
	
    for (i=0 ; i<unstuffed->nchannels ; i++ ) {
	achan = (PktChannel *) gettbl(unstuffed->chan, i) ;
	if (achan->datatype == trINT) {
		H2N4 (cp, achan->data, achan->nsamp);
	} else {
		memcpy(cp, achan->data, achan->nsamp * sizeof(int) ) ; 
	}
	cp += achan->nsamp * sizeof(int) ; 
    }
   
    *size = cp - *stuffed ;
    *time = mintime ;
    return 0 ; 
}

Packet *
newpkt ()
{
    Packet *pkt ;
    allot (Packet *, pkt, 1) ;
    pkt->nchannels = 0 ; 
    pkt->pkttype = -1 ;
    pkt->hdrtype = -1 ;
    pkt->chan = newtbl(0) ; 
    return pkt ; 
}



int 
un_dbug ( double time, 
          char *srcid, 
	  char *stuffed, 
          Packet **unstuffedp ,
	  void *private )

{
    Packet *unstuffed ;
    char *cp ;
    PktChannel *achan ; 
    unsigned short hdrsize, datasize, type;
    int i, perchan ;
    int retcode = 1 ;
    int *data ; 
    int nbytes ;

    if ( *unstuffedp == 0 ) 
	*unstuffedp = newpkt() ; 
    unstuffed = *unstuffedp ;

    perchan = 3 * PKT_NAMESIZE + 3 * sizeof(double) + 3 * sizeof(int);

    cp = (char *) stuffed ;
    N2H2 (&hdrsize, cp, 1);
    cp += 2;
    N2H2 (&datasize, cp, 1);
    cp += 2;
    N2H2 (&type, cp, 1);
    cp += 2;
    N2H2 (&type, cp, 1);
    cp += 2;
    N2H4 (&(unstuffed->nchannels), cp, 1);
    cp += 4;
    unstuffed->pkttype = type ;
    unstuffed->hdrtype = type ;

    for (i=0 ; i<unstuffed->nchannels ; i++ ) {
	achan = (PktChannel *) gettbl(unstuffed->chan, i) ;
	if ( achan == 0 ) {
	    allot ( PktChannel *, achan, 1 ) ;
	    settbl(unstuffed->chan, i, achan ) ; 
	    achan->nbytes = 0 ; 
	    achan->data = 0 ;
	    strcpy (achan->segtype, "V");
	}
	memcpy(achan->net, cp, PKT_NAMESIZE ) ; 
	cp += PKT_NAMESIZE ;
	memcpy(achan->sta, cp, PKT_NAMESIZE ) ; 
	cp += PKT_NAMESIZE ;
	memcpy(achan->chan, cp, PKT_NAMESIZE ) ; 
	cp += PKT_NAMESIZE ;
	N2H8 (&(achan->time), cp, 1);
	cp += 8;
	N2H8 (&(achan->samprate), cp, 1);
	cp += 8;
	N2H8 (&(achan->calib), cp, 1);
	cp += 8;
	N2H4 (&(achan->nsamp), cp, 1);
	cp += 4;
	cp += 4;
	N2H4 (&(achan->datatype), cp, 1);
	cp += 4;
    }
	
    for (i=0 ; i<unstuffed->nchannels ; i++ ) {
	achan = (PktChannel *) gettbl(unstuffed->chan, i) ;
	nbytes = sizeof(int) * achan->nsamp ; 
	if ( achan->nbytes < nbytes ) { 
	    data = achan->data;
	    if (data) reallot ( int *, data, achan->nsamp ) ;
	    else allot ( int *, data, achan->nsamp);
	    achan->data = data;
	    achan->nbytes = nbytes ; 
	}
	if (achan->datatype == trINT) {
		N2H4 (achan->data, cp, achan->nsamp);
	} else {
		memcpy(achan->data, cp, nbytes ) ;
	}
	cp += nbytes ;
    }
    if ( cp - stuffed != hdrsize + datasize ) {
	elog_log( 0, "packet sizes don't add up: %d != %d + %d\n",
	    cp - stuffed, hdrsize, datasize ) ; 
	retcode = 0 ;
    }
   
    return retcode ; 
}

/* $Id$ */
