#include <string.h>
#include "stock.h"
#include "elog.h"
#include "dbgenloc.h"

static void
usage ()
{
    fprintf (stderr,
	     "Usage: %s [-p pf] input-db output-db\n", Program_Name);
    /* banner (Program_Name, "Version $Revision$ $Date$\n"); */
    fprintf (stderr, "\n       Courtesy of Gary Pavlis, Indiana University\n");
    exit (1);
}


int
main (int argc, char **argv)
{
    int             c,
                    errflg = 0;
    char           *in,
                   *out;
    Dbptr           dbin,
                    dbout;
    char            aline[STRSZ];
    int             verbose = 0;
    char           *pfname, *error;
    long	   orid ;
    int		   nostdin=0 ;

    pfname=strdup("dbgenloc");

    elog_init (argc, argv);
    elog_set ( ELOG_MAXMSG, -1, 0 )  ;

    while ((c = getopt (argc, argv, "hnp:vV")) != -1) {
	switch (c) {

	case 'h':
	    usage ();
	    break ;

	case 'n':
	    nostdin = 1;
	    break ;

	case 'p':
	    pfname = optarg ; 
	    break ; 

	case 'v':
	    verbose = 1;
	    break;

	case 'V':
	    cbanner("$Revision$ $Date$\n",
			"dbgenloc input-db output-db",
			"Gary Pavlis",
			"Indiana University",
			"pavlis@indiana.edu");
	    exit (0);

	default:
	    errflg++;
	}
    }

    if (errflg || argc - optind != 2)
	usage ();

    in = argv[optind++];
    if (dbopen (in, "r+", &dbin))
	elog_die(1, "Can't open input database %s\n", in);

    out = argv[optind++];
    if (dbopen (out, "r+", &dbout))
	elog_die(1, "Unable to open output database %s\n", out);


    while (nostdin || gets (aline)) {
	switch (run_location (dbin, dbout, pfname, &orid, &error)) {
	case 0:
	    printf ("location_solution: new origin %ld\n", orid);
	    break;

	default:
	    printf ("location_solution: no_solution : %s\n", error);
	    break;
	}

	fflush (stdout);
	elog_flush (1, 0) ; 

	if ( nostdin ) break;
    }

    return 0;
}

/* $Id$ */
