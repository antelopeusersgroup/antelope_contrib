#include "dbgenloc.h"

static void
usage ()
{
    fprintf (stderr,
	     "Usage: %s input-db output-db\n", Program_Name);
    /* banner (Program_Name, "Version $Revision$ $Date$\n"); */
    exit (1);
}


main (argc, argv)
int             argc;
char          **argv;

{
    extern char    *optarg;
    extern int      optind;
    int             c,
                    errflg = 0;
    char           *in,
                   *out;
    Dbptr           dbin,
                    dbout;
    char            aline[STRSZ];
    int             verbose = 0;
    char           *pfname, *error;
    int		   orid ;

    pfname = Program_Name = argv[0];

    elog_init (argc, argv);
    elog_set ( ELOG_MAXMSG, -1, 0 )  ;

    while ((c = getopt (argc, argv, "hvV")) != -1) {
	switch (c) {
	case 'h':
	    usage ();
	    break ;

	case 'v':
	    verbose = 1;
	    break;

	case 'V':
	    banner (Program_Name, "Version $Revision$ $Date$\n");
	    exit (0);

	default:
	    errflg++;
	}
    }

    if (errflg || argc - optind != 2)
	usage ();

    in = argv[optind++];
    if (dbopen (in, "r+", &dbin))
	die (1, "Can't open input database %s\n", in);

    out = argv[optind++];
    if (dbopen (out, "r+", &dbout))
	die (1, "Unable to open output database %s\n", out);


    while (gets (aline)) {
	switch (run_location (dbin, dbout, pfname, &orid, &error)) {
	case 0:
	    printf ("location_solution: new origin %d\n", orid);
	    break;

	default:
	    printf ("location_solution: no_solution : %s\n", error);
	    break;
	}

	fflush (stdout);
	elog_flush (1, 0) ; 
    }

    return 0;
}

/* $Id$ */
