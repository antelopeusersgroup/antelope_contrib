#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <math.h>

#include "coords.h"
#include "db.h"
#include "stock.h"
#include "elog.h"

static void
usage ()
{
    char *usage = "aah2db [-v] ahfile [ahfile...] dbname";
    char *author = "Kent Lindquist";
    char *location = "Geophysical Institute, U. of Alaska";
    char *email = "kent@giseis.alaska.edu";
    cbanner ( "$Revision$ $Date$",
		usage, 
		author,
		location,
		email );
}

int
main (int argc, char **argv)
{
    int             c,
                    errflg = 0;
    Dbptr           db;
    char           *dbname;
    int             verbose = 0;

    elog_init ( argc, argv ) ; 

    while ((c = getopt (argc, argv, "vV")) != -1) {
	switch (c) {

	case 'v':
	    verbose++;
	    break;

	case 'V':
	    usage();
	    exit (0);

	default:
	    errflg++;
	    break ;
	}
    }

    if (errflg || argc - optind < 2 ) {
	usage ();
	exit( 1 );
    }

    dbname = argv[optind++];

    return 0;
}
