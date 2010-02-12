#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "stock.h"
#include "orb.h"
#include "brttpkt.h"

Arr	*Rrd_files = 0;
double	Status_stepsize_sec = 0;
char	*Rrdfile_pattern = 0;
int	Verbose = 0;
int 	VeryVerbose = 0;
FILE	*Rrdfp;

static void
usage( void )
{
	cbanner( "$Date$",
		"[-vV] [-s statefile] [-p pffile] [-m match] [-f from]  orbname dbcache\n",
		"Dr. Kent Lindquist",
		"Lindquist Consulting, Inc.",
		"kent@lindquistconsulting.com" );

	exit( 1 );
}

static void
archive_dlsvar( Dbptr db, char *net, char *sta, char *dls_var, char *dsparams, Tbl *rras, double time, double val )
{
	char	key[STRSZ];
	char	*rrd;
	double	start_time;
	Dbptr	dbt;
	char	datasource[STRSZ];
	char	command[STRSZ];
	char	response[STRSZ];
	char	*resp_ptr;
	int	i;

	sprintf( key, "%s:%s:%s", net, sta, dls_var );

	rrd = getarr( Rrd_files, key );

	if( rrd == NULL || ! is_present( rrd ) ) {

		start_time = time - Status_stepsize_sec;

		dbt = db;
		dbt.record = dbaddnull( db );

		dbputv( dbt, 0, "net", net,
				"sta", sta,
				"rrdvar", dls_var,
				"time", start_time,
				0 );

		trwfname( dbt, Rrdfile_pattern, &rrd );

		sprintf( datasource, "DS:%s:%s", dls_var, dsparams );

		if( Verbose ) {

			elog_notify( 0, "Creating rrdfile %s\n", rrd );
		}

		sprintf( command, "create %s -b %d -s %f %s", 
			rrd, (int) floor( start_time ), Status_stepsize_sec, datasource );

		for( i = 0; i < maxtbl( rras ); i++ ) {

			strcat( command, " " );
			strcat( command, (char *) gettbl( rras, i ) );
		}
	
		fprintf( Rrdfp, "%s\n", command );

		if( VeryVerbose ) { 

			resp_ptr = getaline( Rrdfp, response, STRSZ );

			if( resp_ptr == (char *) NULL ) {

				elog_notify( 0, "%s\n", "(null)" );

			} else {

				elog_notify( 0, "%s\n", resp_ptr );
			}
		}

		setarr( Rrd_files, key, rrd );
	}

	if( VeryVerbose ) {

		elog_notify( 0, "Recording time '%f' value '%f' from '%s:%s:%s' in '%s'\n",
			time, val, net, sta, dls_var, rrd );
	}

	sprintf( command, "update %s %d:%f", rrd, (int) floor( time ), val );

	fprintf( Rrdfp, "%s\n", command );

	if( VeryVerbose ) { 

		resp_ptr = getaline( Rrdfp, response, STRSZ );

		if( resp_ptr == (char *) NULL ) {

			elog_notify( 0, "%s\n", "(null)" );

		} else {

			elog_notify( 0, "%s\n", resp_ptr );
		}
	}
}

int
main( int argc, char **argv )
{
	int	c;
	int	errflag = 0;
	int	orb;
	int	stop = 0;
	int	nrecs;
	char	*match = ".*/pf/st";
	char	*from = 0;
	char	*statefile = 0;
	char	*pfname = "orb2rrdc";
	char	*orbname;
	char	*dbcache;
	char	*rrdtool;
	char	command[STRSZ];
	char	net[STRSZ];
	char	sta[STRSZ];
	char	rrdvar[STRSZ];
	char	key[STRSZ];
	char	path[FILENAME_MAX];
	Dbptr	db;
	Dbptr	dbt;
	Pf	*pf;
	char	*Default_network;
	Tbl	*dlslines;
	Arr	*Dls_vars_dsparams;
	Arr	*Dls_vars_rras;
	Tbl	*Dls_vars_keys;
	char	*line;
	char	*dls_var;
	char	*dsparams;
	Tbl	*rras;
	int	i;
	int	j;
	OrbreapThr *ort;
	int	pktid;
	char	srcname[ORBSRCNAME_SIZE];
	double	time = 0;
	char	*packet = 0;
	int	nbytes = 0;
	int	bufsize = 0;
	Packet	*pkt = 0;
	int	rc;
	char	*s;
	Pf	*dlspf;
	Tbl	*dlspfkeys;
	char	*element;
	Tbl	*parts;
	double	val;
	Pf	*pfval = 0;

	elog_init( argc, argv );

	while( ( c = getopt( argc, argv, "vVs:p:m:f:" ) ) != -1 ) {

		switch( c ) {

		case 'f':
			from = optarg;
			break;

		case 'm':
			match = optarg;
			break;

		case 'p': 
			pfname = optarg;
			break;

		case 's':
			statefile = optarg;
			break;
			
		case 'v':
			Verbose++;
			break;

		case 'V':
			VeryVerbose++;
			Verbose++;
			break;
		
		default:
			elog_complain( 0, "Unknown option '%c'\n", c );
			errflag++;
			break;
		}
	}

	if( errflag || argc - optind != 2 ) {

		usage();
	}

	if( Verbose ) {

		elog_notify( 0, "Starting at %s (%s $Revision$ $Date$)\n", 
				zepoch2str( str2epoch( "now" ), "%D %T %Z", "" ),
				Program_Name );
	}

	orbname = argv[optind++];
	dbcache = argv[optind++];

	pfread( pfname, &pf );

	rrdtool = pfget_string( pf, "rrdtool" );

	if( rrdtool == NULL || ! strcmp( rrdtool, "" ) ) {

		elog_die( 0, "Error: no rrdtool executable name specified in parameter file\n" );

	} else if( ( rrdtool[0] == '/' && ! is_present( rrdtool ) ) || ( rrdtool[0] != '/' && ! datafile( "PATH", rrdtool ) ) ) {

		elog_die( 0, "Error: can't find rrdtool executable by name of '%s' (check PATH environment " 
			"variable, or absolute path name if given)\n", rrdtool );

	} else if( rrdtool[0] == '/' ) {

		sprintf( command, "%s -", rrdtool );

	} else {

		sprintf( command, "rrdtool -" );
	}

	Rrdfp = popen( command, "r+" );

	if( Rrdfp == (FILE *) NULL ) {

		elog_die( 0, "Failed to open socket to rrdtool command\n" );
	}

	orb = orbopen( orbname, "r&" );

	if( orb < 0 ) {

		elog_die( 0, "Failed to open orb '%s' for reading. Bye.\n", orbname );
	}

	orbselect( orb, match );

	if( from != NULL && statefile == NULL ) {

		pktid = orbposition( orb, from );

		if( Verbose ) {

			elog_notify( 0, "Positioned to packet %d\n", pktid );
		}

	} else if( from != NULL ) {

		elog_complain( 0, "Ignoring -f in favor of existing state file\n" );
	}

	if( statefile != NULL ) {

		stop = 0;

		exhume( statefile, &stop, 15, 0 );

		orbresurrect( orb, &pktid, &time );

		if( Verbose ) {

			elog_notify( 0, "Resurrecting state to pktid %d, time %s\n",
				pktid, s = strtime( time ) );

			free( s );
		}

		orbseek( orb, pktid );
	}

	dbopen( dbcache, "r+", &db );

	if( db.database < 0 ) {
		
		elog_die( 0, "Failed to open cache database '%s'. Bye.\n", dbcache );

	} else {
		
		db = dblookup( db, "", "rrdcache", "", "" );

		if( db.table < 0 ) {

			elog_die( 0, "Failed to lookup 'rrdcache' table in '%s'. Bye.\n" );
		}
	}

	dbt = dbsubset( db, "endtime == NULL", NULL );

	Rrd_files = newarr( 0 );

	dbquery( dbt, dbRECORD_COUNT, &nrecs );

	for( dbt.record = 0; dbt.record < nrecs; dbt.record++ ) {

		dbgetv( dbt, 0, "net", &net, "sta", &sta, "rrdvar", &rrdvar, NULL );

		dbfilename( db, (char *) &path );

		sprintf( key, "%s:%s:%s", net, sta, rrdvar );

		setarr( Rrd_files, key, path );
	}

	Rrdfile_pattern = pfget_string( pf, "rrdfile_pattern" );
	Status_stepsize_sec = pfget_double( pf, "status_stepsize_sec" );
	Default_network = pfget_string( pf, "default_network" );
	dlslines = pfget_tbl( pf, "dls_vars" );

	Dls_vars_dsparams = newarr( 0 );
	Dls_vars_rras = newarr( 0 );

	for( i = 0; i < maxtbl( dlslines ); i++ ) {
		
		line = gettbl( dlslines, i );
		
		strtr( line, "\t", " " );
		rras = split( line, ' ' );

		dls_var = shifttbl( rras );
		dsparams = shifttbl( rras );

		setarr( Dls_vars_dsparams, dls_var, dsparams );
		setarr( Dls_vars_rras, dls_var, rras );
	}

	ort = orbreapthr_new( orb, -1., 0 );

	for( ; stop == 0; ) {

		orbreapthr_get( ort, &pktid, srcname, &time, &packet, &nbytes, &bufsize );

		if( statefile ) {

			rc = bury();

			if( rc < 0 ) {

				elog_complain( 0, "Unexpected failure of bury command! " 
					"(are there two orb2rrdc's running with the same state" 
					"file?)\n" );

				clear_register( 1 );
			}
		}

		rc = unstuffPkt( srcname, time, packet, nbytes, &pkt );

		if( rc == Pkt_pf ) {

			if( VeryVerbose ) {

				/* Parameter files generally too big for elog */

				fprintf( stderr, "Received a parameter-file '%s' at %s\n%s\n\n", 
						srcname, 
						s = strtime( time ), 
						pf2string( pkt->pf ) );

				free( s );

			} else if( Verbose ) {

				elog_notify( 0, "Received a parameter-file '%s' at %s\n", 
						srcname, s = strtime( time ) );

				free( s );
			}

			pfget( pkt->pf, "dls", (void **) &dlspf );

			dlspfkeys = pfkeys( dlspf );
			Dls_vars_keys = keysarr( Dls_vars_dsparams );

			for( i = 0; i < maxtbl( dlspfkeys ); i++ ) {
			   
			   element = gettbl( dlspfkeys, i );

			   if( strcontains( element, "_", 0, 0, 0 ) ) {

				parts = split( (s = strdup( element )), '_' );

				sprintf( net, "%s", (char *) gettbl( parts, 0 ) );
				sprintf( sta, "%s", (char *) gettbl( parts, 1 ) );

				free( s );
				freetbl( parts, 0 );

			   } else {

				sprintf( net, "%s", Default_network );

				sprintf( sta, "%s", element );
			   }

			   for( j = 0; j < maxtbl( Dls_vars_keys ); j++ ) {

			   	dls_var = gettbl( Dls_vars_keys, j );

				sprintf( key, "%s{%s}", element, dls_var );

				if( pfresolve( dlspf, key, 0, &pfval ) < 0 ) {

					elog_complain( 0, "Unable to extract variable '%s' "
						"(not present or wrong type) from element '%s' "
						"in packet from '%s', timestamped '%s'; Skipping\n",
						key, element, srcname, s = strtime( time ) );

					free( s );

					pfval = 0;

					continue;

				} else {

					val = pfget_double( dlspf, key );
				}

				archive_dlsvar( db, net, sta, dls_var, 
						(char *) getarr( Dls_vars_dsparams, dls_var ),
						(Tbl *) getarr( Dls_vars_rras, dls_var ),
						time, val );
			   }

			}

			freetbl( dlspfkeys, 0 );
			freetbl( Dls_vars_keys, 0 );

		} else if( rc == Pkt_stash ) {

			; /* Do nothing */

		} else {

			if( Verbose ) {

				elog_notify( 0, "Received a packet that's not a parameter file " 
					"(type '%d' from unstuffPkt); skipping\n", rc );
			}
		}
	}
}
