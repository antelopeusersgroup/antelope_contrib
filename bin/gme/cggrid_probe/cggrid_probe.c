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

#define DEFAULT_FORMAT "%.3f"

static void
usage ()
{
    fprintf( stderr, "Usage: cggrid_probe [-n] [-f format] filename x y\n" );
    exit(1) ;
}

int
main( int argc, char **argv )
{
	CGGrid	*cgg;
	int	c;
	char	*outfile = 0;
	char	infile[FILENAME_MAX];
	char	*base_format = DEFAULT_FORMAT;
	char	output_format[STRSZ];
	FILE	*fp_in;
	int	newline = 1;
	double	x;
	double	y;
	double	val;

    	elog_init ( argc, argv ) ; 

    	while ((c = getopt (argc, argv, "nf:")) != -1) {
		switch (c) {
		case 'n':
			newline = 0;
			break;

		case 'f':
			base_format = optarg;
			break;

		case '?':
			usage ();
			break ;
		}
    	}

    	if( argc - optind != 3 ) {

		usage();

	} else {

		strcpy( infile, argv[optind++] );
		x = atof( argv[optind++] );
		y = atof( argv[optind++] );
    	} 

	if( ! is_present( infile ) ) {

		elog_die( 1, "Can't find input file '%s', Bye.", infile );

	} else if( ( fp_in = fopen( infile, "r" ) ) == NULL ) {

		elog_die( 1, "Can't open input file '%s', Bye.", infile );
	}

	cgg = cggrid_read( fp_in );

	if( cgg == (CGGrid *) NULL ) {

		elog_die( 1, "Failed to read input grid '%s', Bye.", infile );
	}

	val = cggrid_probe( cgg, x, y );

	if( newline ) {

		sprintf( output_format, "%s\n", base_format );

	} else {
		sprintf( output_format, "%s", base_format );
	}

	fprintf( stdout, output_format, val );

    	return 0;
}
