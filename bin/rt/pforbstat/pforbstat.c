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

#define SRCNAME_DEFAULT "/pf/orbstat"

static void
usage ()
{
    fprintf (stderr, "\nUsage: %s [-a] [-c] [-s] [-m match] [-r reject] [-v] [-f filename] [-n srcname] [-o orbout] orbname [secs]\n", Program_Name );
    exit (1);
}

int
main (int argc, char **argv)
{
	Pf 	*pf;
	Packet	*pkt;
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
	char	srcname_in[ORBSRCNAME_SIZE] = SRCNAME_DEFAULT;
	char	srcname[ORBSRCNAME_SIZE] = SRCNAME_DEFAULT;
	int	rc;

    	elog_init (argc, argv);
    	elog_notify (0, "%s $Revision$ $Date$\n",
		 Program_Name);

	while ((c = getopt (argc, argv, "ao:n:f:scm:r:vV")) != -1) {
		switch (c) {
		case 'o':
			orboutname = strdup( optarg );
			break;

		case 'n':
			strcpy( srcname_in, optarg );
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

		case 'a':
			flags |= PFORBSTAT_CONNECTIONS;
			flags |= PFORBSTAT_DATABASES;
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
			elog_complain( 1, "Bad option %c\n", c );
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

	if( orboutname == NULL && strcmp( srcname, SRCNAME_DEFAULT ) ) {
		elog_complain( 1, "Useless specification of srcname (-n) without -o\n" );
	}

	pkt = newPkt();

	split_srcname( srcname_in, &pkt->parts );

	pkt->pkttype = suffix2pkttype( pkt->parts.src_suffix );

	if( pkt->pkttype == NULL || pkt->pkttype->content != Pkt_pf ) {
		elog_die( 1, 
		     "Source-name code \"%s\" is not of pf type\n",
		     pkt->parts.src_suffix );
	}

	if ( orboutname && ( orbout = orbopen (orboutname, "w&") ) < 0) {
		elog_die( 0, "Can't open output '%s'\n", orboutname );
	} 

	for( ;; ) {

		if ((orbin = orbopen (orbname, "r&")) < 0) {
			elog_die(0, "Can't open input '%s'\n", orbname);
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

		pf = pforbstat( orbin, flags );

		elog_clear_register( 1 );

		if( orbout ) {

			pkt->pf = pf;
			pkt->time = str2epoch( pfget_string( pf, "server{when}" ) );

			rc = stuffPkt( pkt, 
				       srcname, 
				       &pkttime, 
				       &packet,
				       &nbytes,
				       &packetsz );

			if( rc < 0 ) {

				elog_complain( 0, 
					  "stuffPkt failed for %s\n",
					  pkt->pkttype->name );
			}

			if( strcmp( srcname_in, SRCNAME_DEFAULT ) ) {

				strcpy( srcname, srcname_in );

			} else {
				
				sprintf( srcname, "%s:%ld%s",
					 pfget_string( pf, "server{address}" ),
					 pfget_int( pf, "server{port}" ),
					 SRCNAME_DEFAULT );
			}

			if( rc >= 0 &&
			    orbput( orbout,
				    srcname, 
				    pkttime, 
				    packet, 
				    nbytes ) < 0 ) {

				elog_complain( 0,
				 	"Couldn't send packet to %s\n",
				 	orboutname);
			}
		}

		if( filename ) {

			if( pfwrite( filename, pf ) < 0 ) {

				elog_complain( 1, "pfwrite failed\n" );
			}
		}

		if( verbose || 
		    ( filename == NULL && orbout == 0 ) ) {

			pfout( stdout, pf );
		}

		pffree( pf );

		if (orbclose (orbin)) {
			elog_complain(1, "error closing read orb\n");
		}

		if( seconds ) {
			sleep( seconds );
		} else {
			break;
		}
	}

	if ( orbout && orbclose (orbout)) {
		elog_complain(1, "error closing write orb\n");
	}

	return 0;
}
