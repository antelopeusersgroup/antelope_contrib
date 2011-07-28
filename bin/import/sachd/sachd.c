#include <stdio.h>
#include <sys/types.h>
#include <fcntl.h>
#include <unistd.h>

#include "stock.h"
#include "db.h"
#include "libsac.h"

#define SAC_NULL_FLOAT -12345.0

static SAC             header;
static float          *Data;
int Swap = 0;

static void
usage ()
{
    fprintf (stderr, "\nUsage: %s sacfile ... \n", Program_Name);
    banner (Program_Name, 0) ;
    exit (1);
}

int
swap_sac (SAC * sachdr)
{
    float           x;
    int             intel;
    x = sachdr->delta;
    if (x == SAC_NULL_FLOAT
	    || (x > 1e-8 && x < 1e3)) {
	/* no swap */

#ifndef WORDS_BIGENDIAN
	intel = 1;
#else
	intel = 0;
#endif
    } else {
	/* swap */
	char           *s;
	s = (char *) sachdr;
	swap4 (s, s, 110);

#ifdef WORDS_BIGENDIAN
	intel = 1;
#else
	intel = 0;
#endif
    }
    return intel;
}


static int
show_sac (filename, verbose)
char           *filename;
int             verbose;
{
    SACDesc         sacdesc;
    int             i,
		    intel, 
                    fd,
                    n;
    char	    buf[64] ;

    if (filename == 0 || *filename == 0) {
	fd = 0;
    } else {
	if ((fd = open (filename, O_RDONLY)) < 0) {
	    elog_complain(1, "Can't open '%s'\n", filename);
	    return -1 ;
	}
    }

    if (read (fd, &header, sizeof (SAC)) < sizeof (SAC)) {
	fprintf (stderr, "Read error\n");
	exit (1);
    }
    intel = swap_sac (&header) ;
    TRIM_SAC (&header);
    TRIM_SAC (&sac_null);
    FILL_SACDESC (&header, &sacdesc);
    FILL_SACDESC (&sac_null, &sacdesc_null);
    printf ("\nSAC Non-NULL header values:\n\n");
    for (i = 0; i < SAC_NFIELDS; i++) {
	switch (sacdesc.field[i].type) {
	case SAC_FLOAT:
	    if (*(float *) (sacdesc.field[i].ptr) ==
		    *(float *) (sacdesc_null.field[i].ptr))
		break;
	    printf ("%15s = ", sacdesc.field[i].name);
	    printf ("%20f", *(float *) (sacdesc.field[i].ptr));
	    printf ("     %-30s\n", sacdesc.field[i].desc);
	    break;
	case SAC_LONG:
	    if (*(int *) (sacdesc.field[i].ptr) ==
		    *(int *) (sacdesc_null.field[i].ptr))
		break;
	    printf ("%15s = ", sacdesc.field[i].name);
	    printf ("%13d", *(int *) (sacdesc.field[i].ptr));
	    printf ("            %-30s\n", sacdesc.field[i].desc);
	    break;
	case SAC_CHAR:
	    if (!strcmp ((char *) (sacdesc.field[i].ptr),
			 (char *) (sacdesc_null.field[i].ptr)))
		break;
	    printf ("%15s = ", sacdesc.field[i].name);
	    printf ("%13s", (char *) (sacdesc.field[i].ptr));
	    printf ("            %-30s\n", sacdesc.field[i].desc);
	    break;
	}
    }
    if (header.npts == sac_null.npts || header.npts < 1) {
	fprintf (stderr, "Illegal number of data points.\n");
	exit (1);
    }
    n = header.npts * sizeof (float);
    Data = (float *) malloc (n);
    if (Data == NULL) {
	fprintf (stderr, "Malloc error.\n");
	exit (1);
    }
    if (read (fd, Data, n) < n) {
	fprintf (stderr, "Read error\n");
	exit (1);
    }
    N2H4(Data, Data, header.npts) ;
    if (header.npts > 12)
	n = 12;
    else
	n = header.npts;
    printf ("\nFirst %d data samples:\n\n", n);
    for (i = 0; i < n; i++) {
	strdbl ( Data[i], buf ) ;
	printf ("%19s ", buf ) ; 
	if (!((i + 1) % 4))
	    printf ("\n");
    }
    return 0 ;
}


int
main (argc, argv)
int             argc;
char          **argv;
{
    int             retcode = 0, 
		    c,
                    verbose = 0,
                    errflg = 0;

    Program_Name = argv[0];

    while ((c = getopt (argc, argv, "svV")) != -1)
	switch (c) {
	case 's':
	    Swap = 1 ; 
	    break ;

	case 'V':
	    usage ();
	    break;

	case 'v':
	    verbose = 1;
	    break;

	case '?':
	    errflg++;
	}
    if (errflg)
	usage ();

    if (argc - optind < 1)
	retcode += show_sac ("", verbose);
    else {
	for (; optind < argc; optind++) {
	    fprintf ( stdout, "\n--> %s\n", argv[optind] ) ; 
	    retcode += show_sac (argv[optind], verbose);
	}
    }

    return retcode ;
}

