#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <unistd.h>
#include <errno.h>

#include "Pkt.h"
#include "orb.h"
#include "pf.h"
#include "coords.h"
#include "stock.h"

#include "pforbstat.h"

static void
usage ()
{
    fprintf (stderr, "\nUsage: %s [-c] [-s] [-m match] [-r reject] [-v] [-f filename] [-n srcname] [-o orbout] orbname [secs]\n", Program_Name );
    exit (1);
}

int
main (int argc, char **argv)
{
	Pf 	*pf;
	Packet	*pkt;
	FILE	*fp = NULL;
    	int	c;
	int	nargs;
	int	nsources;
	int	errflg = 0;
	char	*orbname;
	char	*orboutname = 0;
	char	*filename = 0;
    	int 	orbin = 0;
	int	orbout = 0;
	int	verbose = 0;
	char	*match = 0;
	char	*reject = 0;
	int	seconds = 0;
	int	flags = PFORBSTAT_SERVER; /* Necessary for packet time */
	int	nbytes = 0;
	int	packetsz = 0;
	char	*packet = 0;
	double	pkttime;
	char	srcname[ORBSRCNAME_SIZE] = "/pf/orbstat";
	int	rc;

    	elog_init (argc, argv);
    	elog_notify (0, "%s $Revision$ $Date$\n",
		 Program_Name);

	while ((c = getopt (argc, argv, "o:n:f:scm:r:vV")) != -1) {
		switch (c) {
		case 'o':
			orboutname = strdup( optarg );
			break;

		case 'n':
			strcpy( srcname, optarg );
			break;

		case 'f':
			filename = strdup( optarg );
			break;

		case 's':
			flags |= PFORBSTAT_SOURCES;
			break;

		case 'c':
			flags |= PFORBSTAT_CLIENTS;
			break;

		case 'm':
			match = strdup( optarg );
			break;

		case 'r':
			reject = strdup( optarg );
			break;

		case 'v':
			verbose++;
			break;

		case 'V':
			usage ();
			break;

		case '?':
			complain( 1, "Bad option %c\n", c );
			errflg++;
		}
	}

	nargs = argc - optind;

	if (errflg || nargs < 1 || nargs > 2 )
		usage ();

	orbname = argv[optind++];

	if( nargs > 1 ) {
		seconds = atoi( argv[optind++] );
	}

	pkt = newPkt();

	split_srcname( srcname, &pkt->parts );

	pkt->pkttype = suffix2pkttype( pkt->parts.src_suffix );

	if( pkt->pkttype->content != Pkt_pf ) {
		die( 1, 
		     "Source-name code \"%s\" is not of pf type\n",
		     pkt->parts.src_suffix );
	}

	if ((orbin = orbopen (orbname, "r&")) < 0) {
		die (0, "Can't open input '%s'\n", orbname);
	}

	if ( orboutname && ( orbout = orbopen (orboutname, "w&") ) < 0) {
		die( 0, "Can't open output '%s'\n", orboutname );
	} 

	if( match ) {
		nsources = orbselect( orbin, match );
	}

	if( reject ) {
		nsources = orbreject( orbin, reject );
	}

	if( verbose && (match || reject) ) {

		fprintf( stderr, 
			 "pforbstat: %d sources selected\n", 
			 nsources );
	}

	for( ;; ) {

		pf = pforbstat( orbin, flags );

		if( orbout ) {

			pkt->pf = pf;
			pkt->time = pfget_double( pf, "server{when}" );

			rc = stuffPkt( pkt, 
				       srcname, 
				       &pkttime, 
				       &packet,
				       &nbytes,
				       &packetsz );

			if( rc < 0 ) {

				complain( 0, 
					  "stuffPkt failed for %s\n",
					  pkt->pkttype->name );
			}

			if( rc >= 0 &&
			    orbput( orbout,
				    srcname, 
				    pkttime, 
				    packet, 
				    nbytes ) < 0 ) {

				complain( 0,
				 	"Couldn't send packet to %s\n",
				 	orboutname);
			}
		}

		if( filename ) {

			if( pfwrite( filename, pf ) < 0 ) {

				complain( 1, "pfwrite failed\n" );
			}
		}

		if( verbose || 
		    ( filename == NULL && orbout == 0 ) ) {

			pfout( stdout, pf );
		}

		pffree( pf );

		if( seconds ) {
			sleep( seconds );
		} else {
			break;
		}
	}


	if (orbclose (orbin)) {
		complain (1, "error closing read orb\n");
	}

	if ( orbout && orbclose (orbout)) {
		complain (1, "error closing write orb\n");
	}

	return 0;
}
