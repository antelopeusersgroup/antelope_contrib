/* Copyright (c) 2004 Boulder Real Time Technologies, Inc. */
/* All rights reserved. */
/*                                                                     
/* Written by Dr. Kent Lindquist, Lindquist Consulting, Inc. */
/*
/* This software may be used freely in any way as long as */
/* the copyright statement above is not removed. */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <math.h>

#include "coords.h"
#include "db.h"
#include "stock.h"
#include "cgeom.h"

#define DEFAULT_FORMAT "as"

static void
usage ()
{
    fprintf( stderr, "Usage: cggrid_convert [-v] [-f format] [-o outfile] filename\n" );
    exit(1) ;
}

int
main( int argc, char **argv )
{
	CGGrid	*cgg;
	int	c;
	int	verbose = 0;
	char	*outfile = 0;
	char	infile[FILENAME_MAX];
	char	*format = DEFAULT_FORMAT;
	char	*s;
	FILE	*fp_in;
	FILE	*fp_out;

    	elog_init ( argc, argv ) ; 
	elog_notify ( 0,
		"%s : $Revision$ $Date$ Started %s\n",
		argv[0], s = strtime( now() ) ) ;
	free( s );

    	while ((c = getopt (argc, argv, "vo:f:")) != -1) {
		switch (c) {

		case 'v':
			verbose++ ;
			break;

		case 'o':
			outfile = optarg;
			break;
		
		case 'f':
			format = optarg;
			break;

		case '?':
			usage ();
			break ;
		}
    	}

    	if( argc - optind != 1 ) {

		usage();

	} else {

		strcpy( infile, argv[optind] );
    	} 

	if( ! is_present( infile ) ) {

		elog_die( 1, "Can't find input file '%s', Bye.", infile );

	} else if( ( fp_in = fopen( infile, "r" ) ) == NULL ) {

		elog_die( 1, "Can't open input file '%s', Bye.", infile );
	}

	if( outfile != NULL ) {

		fp_out = fopen( outfile, "w" );

		if( fp_out == (FILE *) NULL ) {
			elog_die( 1, "Can't open output file '%s', Bye.", outfile );
		}

	} else {

		fp_out = stdout;
	}

	cgg = cggrid_read( fp_in );

	if( cgg == (CGGrid *) NULL ) {

		elog_die( 1, "Failed to read input grid '%s', Bye.", infile );
	}

	if( cggrid_write( cgg, format, fp_out ) < 0 ) {

		if( outfile == NULL ) { 
			outfile = "stdout";
		}

		elog_die( 1, "Failed to write output grid to '%s', Bye.", outfile );
	}

    	return 0;
}
