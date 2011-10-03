#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <math.h>

#include "coords.h"
#include "db.h"
#include "stock.h"
#include "dbxml.h"

static void
usage ()
{
    fprintf (stderr, "\nUsage: %s [-v] [-p] [-d document_root] "
		     "[-r rowname] dbname.table "
		     "[name expr [name expr ...]]\n", Program_Name);
    exit (1);
}

int
main (int argc, char **argv)
{
	int	c;
	int 	errflg = 0;
	Dbptr	db;
	char	*database;
	int	verbose = 0;
	int	primary = 0;
	Pf	*pf = NULL;
	char	*xmlstring = NULL;
	char	*rootname = NULL;
	char	*rowname = NULL;
	char	*stylesheet = NULL;
	char	xslt_processor[STRSZ];
	char	command[STRSZ];
	char	tempfile[FILENAME_MAX];
	FILE	*tempfs = NULL;
	Tbl	*fields = 0;
	Tbl	*expressions = 0;
	int	rc;
	int	flags = 0;

	elog_init( argc, argv ) ; 

	while ((c = getopt (argc, argv, "pd:r:t:v")) != -1) {
		switch (c) {

		case 'v':
			verbose++;
			break;

		case 't':
			stylesheet = strdup( optarg );
			break;

		case 'd':
			rootname = strdup( optarg );
			break;

		case 'p':
			primary++;
			break;

		case 'r':
			rowname = strdup( optarg );
			break;

		default:
			errflg++;
			break ;
		}
	}

	if( errflg || argc - optind < 1 || (((argc-optind) % 2) != 1)) {
		usage ();
	}

	pfread( "db2xml", &pf );

	database = argv[optind++];
	
	if( dbopen_table( database, "r+", &db ) < 0 ) { 

		elog_die( 0, "Can't open database %s\n", database ); 
	}

	if( argc - optind > 0 ) {

		fields = newtbl( 20 );	
		expressions = newtbl( 20 );

		while( argc - optind > 0 ) {
			pushtbl( fields, argv[optind++] );	
			pushtbl( expressions, argv[optind++] );	
		}
		
		if( primary ) {
			elog_complain( 1, 
			"Useless use of '-p' with specified expressions, ignoring\n" );
		}

	} else if( primary ) {

		flags |= DBXML_PRIMARY;
	}

	rc = db2xml( db, rootname, rowname, fields, expressions, 
		(void **) &xmlstring, flags );

	if( rc < 0 || xmlstring == NULL ) {
		
		elog_clear_register( 1 );

		elog_die( 0, "db2xml failed\n" );

	} else if( stylesheet != NULL ) {

		sprintf( tempfile, "/tmp/db2xml_%d_%d.xml", getuid(), getpid() );

		tempfs = fopen( tempfile, "w" );

		fwrite( xmlstring, sizeof(char), strlen( xmlstring ), tempfs );

		fclose( tempfs );

		sprintf( xslt_processor, "%s", pfget_string( pf, "xslt_processor" ) );

		strsub( xslt_processor, "STYLESHEET", stylesheet, xslt_processor );

		strsub( xslt_processor, "XMLFILE", tempfile, command );

		system( command );

		unlink( tempfile );

	} else {

		fwrite( xmlstring, sizeof(char), strlen( xmlstring ), stdout );
	}

	free( xmlstring );

	return 0;
}
