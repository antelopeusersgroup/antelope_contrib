/*
 * guralp2orb
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1998
 *
 */
#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include "stock.h"
#include "orb.h"
#include "pf.h"
#include "pkt.h"
#include "mygcf.h"
#include "receiver.h"

extern int socketInit();
extern int receiver();

#define REREAD_PF_AFTER_NPACKETS 10

#define NULL_NET "NULL"
#define NULL_PINNO 0
#define NULL_CALIB 0.0

static char pffile[FILENAME_MAX];
static int verbose = 0;

static int 
packet_convert( char *packet,
		char **orbpacket,
		int *nbytes,
		int *bufsize,
		double *time,
		double *endtime,
		char *srcid )
{
	PktChannel pktchan;
	TGCFpacket pkt;
	static char *null_net = NULL_NET;
	static int pf_reread_countdown = 0;
	static Pf *pf = 0;
	Pf	*system;
	Pf	*channels;
	Arr	*channel;
	char	*net;
	char	*sta;
	char	*chan;
	int	pinno;
	double	calib;
	void	*p;
	int	rc;
	int	retcode = 0;

	rc = decompressGCF_UDPmessage( packet, &pkt );

	if( rc ) {
		return rc;
	}

	net = null_net;
	sta = &pkt.system_id[0];
	chan = &pkt.stream_id[0];
	pinno = NULL_PINNO;
	calib = NULL_CALIB;

	if( ! pf_reread_countdown ) {

		if( pf ) pffree( pf );

		if( ( rc = pfread( pffile, &pf ) ) < 0 ) {
			pf = 0;
		}

		pf_reread_countdown = REREAD_PF_AFTER_NPACKETS;

	} else {

		pf_reread_countdown--;

	}

	if( pf && ( pfget( pf, pkt.system_id, (void **) &system )
			!= PFINVALID ) ) {

		if( ( p = pfget_string( system, "net" ) ) != NULL )
							net = (char *) p;

		if( ( p = pfget_string( system, "sta" ) ) != NULL )
							sta = (char *) p;

		if( pfget( system, "channels", (void **) &channels ) != PFINVALID ) {
			if(channel = pfget_arr(channels,pkt.stream_id)) {
				if( p = getarr( channel, "chan" ) ) 
					chan = (char *) p;
				if( p = getarr( channel, "pinno" ) ) 
					pinno = atoi( (char *) p );
				if( p = getarr( channel, "calib" ) ) 
					calib = atof( (char *) p );
			}
		}
	}

	if( strcmp( chan, "Ignore" ) == 0 ) {

		if( verbose ) fprintf( stderr, 
				"Ignoring station %s stream_id %s\n",
				sta, pkt.stream_id );
		return -1;

	} else {

		(void) strcpy( pktchan.net, net ); 
		(void) strcpy( pktchan.sta, sta ); 
		(void) strcpy( pktchan.chan, chan );

		(void) sprintf( srcid, "%s_%s_%s", net, sta, chan );

		if( verbose ) fprintf( stderr,
					"Converting %s %s as %s\n", 
			 		pkt.system_id,
			 		pkt.stream_id,
			 		srcid );
	}

	pktchan.time = pkt.epoch;
	*time = pktchan.time;

	pktchan.samprate = 1.0 * pkt.samprate;
	pktchan.calib = calib;
	pktchan.nsamp = pkt.nsamp;
	pktchan.datatype = trINT;
	pktchan.data = (void *) pkt.data; 

	*endtime = ENDTIME( *time, pktchan.samprate, pktchan.nsamp );

	mystuff_iw_tracebuf( 0, pinno, &pktchan, orbpacket, nbytes, bufsize );

	return retcode;
}

main( argc, argv )
int argc;
char **argv;
{
	char	orbname[STRSZ];
	int	orbfd;
	char	*inaddr = "0.0.0.0";
	int	inport;
	int	packetLen;
	char	packet[PACKET_SIZE];
	char    srcid[STRSZ];
	double	timestamp;
	int	reject_future_packets = 0;
	double	reject_future_packets_sec = 0;
	double	tdelta;
	double	endtime;
	struct timespec tp;
	char	*orbpacket = 0;
	int	bufsize = 0;
	int	nbytes;
	int	optind;
	int	rc;
	char	*c;
	char	*s;
	int	i;

	strcpy( pffile, "guralp2orb" );

    	for (optind=1 ; optind < argc ; optind++ ) {

        	if ( *(argv[optind]) != '-' || strcmp(argv[optind], "-") == 0 )
            	break ;
	 
        	if (strcmp(argv[optind], "-v") == 0) {
			verbose = 1;
        	}

        	if (strcmp(argv[optind], "-pf") == 0) {
			strcpy( pffile, argv[++optind] );
        	}

        	if (strcmp(argv[optind], "-r") == 0) {

			reject_future_packets = 1;
			reject_future_packets_sec = atof( argv[++optind] );

			fprintf( stderr, "%s%s%s\n",
			  "guralp2orb: rejecting all packets that are ",
			  ( s = strtdelta( reject_future_packets_sec ) ),
			  " or more into the future" );
			free( s );
        	}
	}

	if( argc - optind != 2 )
		die( 1, "Usage: %s [-v] [-pf pffile] orbname port_number\n",
			argv[0] );

	strcpy( orbname, argv[optind++] );
	inport = atoi( argv[optind++] );

	orbfd = orbopen( orbname, "w&" );
	if( orbfd < 0 ) {
		clear_register( 1 );
		exit( 1 );
	}

	rc = socketInit(inaddr, inport);
	if( rc ) {
		fprintf( stderr, "Error initializing socket\n" );
		exit(1);
	}

	for (;;) {

		packetLen = receiver( packet );
		if ( packetLen == -1 ) {
			(void) fprintf(stderr,
				"Was not able to get udp packet\n");
			continue;
		} 

		rc = packet_convert( packet, 
				     &orbpacket, 
				     &nbytes, 
				     &bufsize,
				     &timestamp,
				     &endtime,
				     srcid );
		if( rc ) {
			continue;
		}

		clock_gettime( CLOCK_REALTIME, &tp );
		tdelta = endtime - tp.tv_sec+tp.tv_nsec/1e9;

		if( reject_future_packets &&
		    tdelta > reject_future_packets_sec ) {
			complain( 1, 
	"guralp2orb: rejecting packet from %s, which ends %s into future\n",
			 srcid, ( s = strtdelta( tdelta ) ) );
			 free( s );
			clear_register( 1 );

			 continue;
		}

		rc = orbput( orbfd, srcid,
			     timestamp, orbpacket, nbytes );

		if( rc != 0 ) clear_register( 1 );

		orbflush( orbfd );

		clear_register( 1 );
	}

	return(0);
}
