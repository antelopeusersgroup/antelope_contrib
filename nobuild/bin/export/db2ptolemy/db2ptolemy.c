#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <math.h>

#include "coords.h"
#include "db.h"
#include "stock.h"
#include "dbptolemy.h"

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
	Pf	pf;
	char	*xmlstring = 0;
	char	*rootname = 0;
	char	*rowname = 0;
	Tbl	*fields = 0;
	Tbl	*expressions = 0;

	elog_init( argc, argv ) ; 

	while ((c = getopt (argc, argv, "pd:r:v")) != -1) {
		switch (c) {

		case 'v':
			verbose++;
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
			"Useless use of '-p' with specified expressions\n" );
		}

	} else if( primary ) {

		dbquery( db, dbPRIMARY_KEY, (Dbvalue *) &fields );
	}

	db2ptolemy( db, fields, expressions, (void **) &xmlstring, 0 );

	if( xmlstring != NULL ) {

		fwrite( xmlstring, sizeof(char), strlen( xmlstring ), stdout );
		free( xmlstring );
	}

	return 0;
}
