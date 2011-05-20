/*
 * ida2orb
 *
 * Kent Lindquist, stealing from David Chavez's IDA-connection examples
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * June, 1997
 *
 * Modified by M. Glushko:
      
      - stuff_iw_tracebuf  doesn't exist in latest version of pkt
        library. It was replaced by mystuff_iw_tracebuf;
      - originally was beg=end=XFER_YNGEST, this made Xfer_Read
        bring only one packets. beg is set to XFER_OLDEST and 'end'
	to a  9.999e99;

 * Modified by K. Lindquist: 

      - IDA channel name changed to 5 characters in 2002, with 
	the last two being the equivalent of the SEED Loc code. 
	parse that out and handle it appropriately in source-name.

 */
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/timeb.h>
#include <time.h>
#include "util.h"
#include "xfer.h"
#include "stock.h"
#include "coords.h"
#include "orb.h"
#include "Pkt.h"
#include "tr.h"


#define STREQ(a, b) (strcmp((a), (b)) == 0)

void xfer_packet_to_orb_pktchan( char *, struct xfer_packet *, struct PktChannel **, int );
int Log = 0;

main( int argc, char **argv )
{
	char	orbname[STRSZ];
	char	idahost[STRSZ];
	char	importstring[STRSZ];
	int	optind;
	static char net[] = "II";		/* HARD-WIRE */
	int	orbfd;
	char	c;
	XFER	*xp;
	char	*tmp;
	char	*sc   = NULL;
	int	keepup = 1;
	int	retry  = 1;
	double	beg = XFER_YNGEST;
	double	end = XFER_YNGEST;
	int	status;
	struct	xfer_packet xf_packet;
	struct PktChannel *pktchan = 0;
	char	*packet = 0;
	int	bufsize = 0;
	int	nbytes = 0;
	char	srcid[STRSZ];
	int	upper = 0;
	int	reject_future_packets = 0;
	double	reject_future_packets_sec;
	struct timespec tp;
	double  tdelta;
	double	endtime;
	int	rc;
	Packet *pkt ;
	char	*s;

	elog_init (argc, argv) ;
	elog_notify ( 0, "%s : $Revision$ $Date$\n", argv[0] ) ;

	pkt = newPkt() ;
	pkt->pkttype = suffix2pkttype("CD1S");

	while ((c = getopt(argc, argv, "ulr:")) != -1)
	{
		switch (c)
		{
        	case 'u':
            		upper = 1;
            		break;
        	case 'l':
            		Log = 1;
            		break;
		case 'r':
			reject_future_packets = 1;
			reject_future_packets_sec = atof( optarg );
			fprintf( stderr, 
			"Rejecting packets more than %g seconds in the future\n", 
				reject_future_packets_sec );
			break;
		default:
			elog_die( 1,
			 "Usage: %s [-r seconds] [-u] orbname idahost importstring\n",
			 argv[0] );
            		break;
		}
	}
	if( upper && reject_future_packets ) {
		optind = 4;
	} else if( reject_future_packets ) {
		optind = 3;
	} else if( upper ) {
		optind = 2;
	} else {
		optind = 1;
	}
	if( argc - optind != 3 )
	elog_die( 1, "Usage: %s [-r seconds] [-u] orbname idahost importstring\n", argv[0] );

	strcpy( orbname, argv[optind] );
	strcpy( idahost, argv[optind + 1] );
	strcpy( importstring, argv[optind + 2] );

	orbfd = orbopen( orbname, "w&" );
	if( orbfd < 0 )
	{
		elog_clear_register( 1 );
		exit( 1 );
	}

	xp = Xfer_Open(idahost, importstring, beg, end, keepup, retry );
	if (xp == (XFER *) NULL) {
		fprintf(stderr, "%s: can't connect with server: %s\n",
				argv[0], Xfer_ErrStr() );
		exit(1);
	}

	status = XFER_OK;

	while ( status == XFER_OK) 
	{
	        status = Xfer_Read(xp, &xf_packet);             
		if( status == XFER_OK )  {
		    double pkttime ;
		    xfer_packet_to_orb_pktchan( net, &xf_packet, &pktchan, upper );
		    strcpy (pkt->parts.src_net, pktchan->net ) ;
		    strcpy (pkt->parts.src_sta, pktchan->sta ) ;
		    strcpy (pkt->parts.src_chan, pktchan->chan ) ;
		    strcpy (pkt->parts.src_loc, pktchan->loc ) ;
		    pkt->nchannels =1 ; 
		    settbl(pkt->channels, 0, pktchan ) ;
		    if ( stuffPkt(  pkt, srcid, &pkttime, &packet, &nbytes, &bufsize ) < 0 ) { 
			elog_die( 0, "can't stuff packet" ) ;
		    }
		    if( Log )  {
			fprintf( stderr, "%s %lf \n", srcid, pktchan->time );
		        fflush( stderr);
		    }

		    endtime = ENDTIME( pktchan->time, 
				       pktchan->samprate, 
				       pktchan->nsamp );
		    tdelta = endtime - now() ;

		    if( reject_future_packets && tdelta > reject_future_packets_sec ) {
			elog_complain( 1,
				"ida2orb: rejecting packet from %s, which ends %s into the future\n", 	
				srcid, ( s = strtdelta( tdelta ) ) );
			free( s );
		    } else {
		    	rc = orbput( orbfd, srcid, pktchan->time,
				     packet, nbytes );
		    	orbflush( orbfd );
			if( rc ) elog_complain( 1,
				"ida2orb: orbput failed for %s\n",
				srcid );
		    }
		    status = XFER_OK;  
		 }  else elog_complain( 0, "Xfer_Read return code is %d\n", status );
	}
	
	Xfer_Close( xp );
	orbclose( orbfd );
 
	if ( status != XFER_FINISHED ) {
		fprintf(stderr, "%s: Xfer_Read failed: %s\n",
			 argv[0], Xfer_ErrStr() );
		exit(1);
	}
 
	exit(0);
}

void
xfer_packet_to_orb_pktchan( char *net,
			    struct xfer_packet *xf_packet,
			    struct PktChannel **pktchan,
			    int upper )
{
	int	new_pktchannel = 0;
	int     *data_out;
	int	i;

	if( (*pktchan) == (struct PktChannel *) NULL )
	{
		*pktchan = newPktChannel() ;
		new_pktchannel++;
	}

        if ( Log )  
            fprintf( stderr, "%lf %s_%s %d %f\n", 
            xf_packet->beg, xf_packet->sname, xf_packet->cname, 
	    xf_packet->nsamp, xf_packet->sint);
 
	(*pktchan)->time = xf_packet->beg;
	(*pktchan)->samprate = 1./xf_packet->sint;
	(*pktchan)->calib = xf_packet->calib;
	(*pktchan)->calper = xf_packet->calper;
	(*pktchan)->nsamp = xf_packet->nsamp;
	strcpy( (*pktchan)->net, net );
	strcpy( (*pktchan)->sta, xf_packet->sname );

	if( strlen( xf_packet->cname ) <= 3 ) {
		strcpy( (*pktchan)->chan, xf_packet->cname );
		strcpy( (*pktchan)->loc, "" );
	} else {
		for( i=0; i<3; i++ )
		{
			(*pktchan)->chan[i] = xf_packet->cname[i];
		}
		(*pktchan)->chan[3] = 0;
		for( i=3; i<5; i++ )
		{
			(*pktchan)->loc[i-3] = xf_packet->cname[i];
		}
		(*pktchan)->loc[2] = 0;
	}

	if( upper )
	{
		for( i=0; i<strlen((*pktchan)->sta); i++ )
		{
			(*pktchan)->sta[i] = toupper( (*pktchan)->sta[i] );
		}
		for( i=0; i<strlen((*pktchan)->chan); i++ )
		{
			(*pktchan)->chan[i] = toupper( (*pktchan)->chan[i] );
		}
		for( i=0; i<strlen((*pktchan)->loc); i++ )
		{
			(*pktchan)->loc[i] = toupper( (*pktchan)->loc[i] );
		}
	}
 
	if( new_pktchannel )
	{
		allot ( int *, data_out, (*pktchan)->nsamp );
		(*pktchan)->data = (void *) data_out;
	}
	else if( (*pktchan)->datasz < (*pktchan)->nsamp )
	{
		data_out = (int *) (*pktchan)->data;
		reallot( int *, data_out, (*pktchan)->nsamp );
		(*pktchan)->data = (void *) data_out;
		(*pktchan)->datasz = (*pktchan)->nsamp ;
	}
	else
	{
		data_out = (int *) (*pktchan)->data;
	}
 
	memcpy( (char *) data_out, (char *) xf_packet->data, 
				(*pktchan)->nsamp * sizeof( int ) );
}
