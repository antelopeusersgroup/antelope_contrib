#include "pkt.h"
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

 */
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/timeb.h>
#include "util.h"
#include "xfer.h"
#include "stock.h"
#include "orb.h"
#include "pkt.h"


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
	int	rc;

	while ((c = getopt(argc, argv, "ul")) != -1)
	{
		switch (c)
		{
        	case 'u':
            		upper = 1;
            		break;
        	case 'l':
            		Log = 1;
            		break;
		default:
			die( 1,
			 "Usage: %s [-u] orbname idahost importstring\n",
			 argv[0] );
            		break;
		}
	}
	if( upper ) { optind = 2;} else {optind = 1;}
	if( argc - optind != 3 )
	die( 1, "Usage: %s [-u] orbname idahost importstring\n", argv[0] );

	strcpy( orbname, argv[optind] );
	strcpy( idahost, argv[optind + 1] );
	strcpy( importstring, argv[optind + 2] );

	orbfd = orbopen( orbname, "w&" );
	if( orbfd < 0 )
	{
		clear_register( 1 );
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
		    xfer_packet_to_orb_pktchan( net, &xf_packet, &pktchan, upper );
		    mystuff_iw_tracebuf( 0, 0, pktchan, &packet, &nbytes, &bufsize );
		    sprintf( srcid, "%s_%s_%s", net, pktchan->sta, pktchan->chan );
		    if( Log )  {
			fprintf( stderr, "%s %lf \n", srcid, pktchan->time );
		        fflush( stderr);
		    }
		    rc = orbput( orbfd, srcid, pktchan->time, packet, nbytes );
		    orbflush( orbfd );
		    status = XFER_OK;  
		 }  else complain( 0, "Xfer_Read return code is %d\n", status );
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
		allot( PktChannel *, (*pktchan), 1 );
		new_pktchannel++;
	}

        if ( Log )  
            fprintf( stderr, "%lf %s_%s %d %f\n", 
            xf_packet->beg, xf_packet->sname, xf_packet->cname, 
	    xf_packet->nsamp, xf_packet->sint);
 
	(*pktchan)->time = xf_packet->beg;
	(*pktchan)->samprate = 1./xf_packet->sint;
	(*pktchan)->calib = xf_packet->calib;
	(*pktchan)->nsamp = xf_packet->nsamp;
	(*pktchan)->datatype = trINT;
	strcpy( (*pktchan)->net, net );
	strcpy( (*pktchan)->sta, xf_packet->sname );
	strcpy( (*pktchan)->chan, xf_packet->cname );

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
	}
 
	if( new_pktchannel )
	{
		allot ( int *, data_out, (*pktchan)->nsamp );
		(*pktchan)->data = (void *) data_out;
		(*pktchan)->nbytes = (*pktchan)->nsamp * sizeof( int );
	}
	else if( (*pktchan)->nbytes < (*pktchan)->nsamp * sizeof( int ) )
	{
		data_out = (int *) (*pktchan)->data;
		reallot( int *, data_out, (*pktchan)->nsamp );
		(*pktchan)->data = (void *) data_out;
		(*pktchan)->nbytes = (*pktchan)->nsamp * sizeof( int );
	}
	else
	{
		data_out = (int *) (*pktchan)->data;
	}
 
	memcpy( (char *) data_out, (char *) xf_packet->data, 
				(*pktchan)->nsamp * sizeof( int ) );
}
