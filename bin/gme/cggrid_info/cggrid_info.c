/*
 *   Copyright (c) 2005 Lindquist Consulting, Inc.
 *   All rights reserved. 
 *                                                                     
 *   Written by Dr. Kent Lindquist, Lindquist Consulting, Inc. 
 * 
 *   This software may be used freely in any way as long as 
 *   the copyright statement above is not removed. 
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <math.h>

#include "coords.h"
#include "db.h"
#include "stock.h"
#include "cgeom.h"

#define DEFAULT_FORMAT "%.8f"

static void
usage ()
{
    fprintf( stderr, "Usage: cggrid_info [-s] [-n] [-f format] cggridfile\n" );
    exit(1) ;
}

int
main( int argc, char **argv )
{
	CGGrid	*cgg;
	FILE	*fp_cgg;
	FILE	*fp_out = stdout;
	int	c;
	char	cggfile[FILENAME_MAX];
	char	*base_format = DEFAULT_FORMAT;
	int	newline = 1;
	int	single_line = 0;

    	elog_init ( argc, argv ) ; 

    	while ((c = getopt (argc, argv, "nsf:")) != -1) {
		switch (c) {
		case 'n':
			newline = 0;
			break;

		case 's':
			single_line = 1;
			break;

		case 'f':
			base_format = optarg;
			break;

		case '?':
			usage ();
			break ;
		}
    	}

    	if( argc - optind < 1 ) {

		usage();

	} else {

		strcpy( cggfile, argv[optind++] );
    	} 

	if( ! is_present( cggfile ) ) {

		elog_die( 1, "Can't find input file '%s', Bye.", cggfile );

	} else if( ( fp_cgg = fopen( cggfile, "r" ) ) == NULL ) {

		elog_die( 1, "Can't open cggrid file '%s', Bye.", cggfile );
	}

	cgg = cggrid_read( fp_cgg );

	if( cgg == (CGGrid *) NULL ) {

		elog_die( 1, "Failed to read input grid '%s', Bye.", cggfile );
	}

	fclose( fp_cgg );

	if( single_line ) {

		fprintf( fp_out, base_format, cgg->minx );
		fprintf( fp_out, " " );

		fprintf( fp_out, base_format, cgg->maxx );
		fprintf( fp_out, " " );

		fprintf( fp_out, base_format, cgg->miny );
		fprintf( fp_out, " " );

		fprintf( fp_out, base_format, cgg->maxy );
		fprintf( fp_out, " " );

		fprintf( fp_out, base_format, cgg->dx );
		fprintf( fp_out, " " );

		fprintf( fp_out, base_format, cgg->dy );
		fprintf( fp_out, " " );

		fprintf( fp_out, "%d", cgg->nx );
		fprintf( fp_out, " " );

		fprintf( fp_out, "%d", cgg->ny );
		fprintf( fp_out, " " );

		fprintf( fp_out, "%s", cgg->units );
		fprintf( fp_out, " " );

	} else {

		fprintf( fp_out, "\nGrid %s:\n\n", cggfile );

		fprintf( fp_out, "\t%-18s: ", "Min X" );
		fprintf( fp_out, base_format, cgg->minx );
		fprintf( fp_out, "\n" );

		fprintf( fp_out, "\t%-18s: ", "Max X" );
		fprintf( fp_out, base_format, cgg->maxx );
		fprintf( fp_out, "\n" );

		fprintf( fp_out, "\t%-18s: ", "Min Y" );
		fprintf( fp_out, base_format, cgg->miny );
		fprintf( fp_out, "\n" );

		fprintf( fp_out, "\t%-18s: ", "Max Y" );
		fprintf( fp_out, base_format, cgg->maxy );
		fprintf( fp_out, "\n" );

		fprintf( fp_out, "\t%-18s: ", "X Grid Spacing" );
		fprintf( fp_out, base_format, cgg->dx );
		fprintf( fp_out, "\n" );

		fprintf( fp_out, "\t%-18s: ", "Y Grid Spacing" );
		fprintf( fp_out, base_format, cgg->dy );
		fprintf( fp_out, "\n" );

		fprintf( fp_out, "\t%-18s: ", "Number of X points" );
		fprintf( fp_out, "%d", cgg->nx );
		fprintf( fp_out, "\n" );

		fprintf( fp_out, "\t%-18s: ", "Number of Y points" );
		fprintf( fp_out, "%d", cgg->ny );
		fprintf( fp_out, "\n" );

		fprintf( fp_out, "\t%-18s: ", "Units" );
		fprintf( fp_out, "%s", cgg->units );
		fprintf( fp_out, "\n" );
	}

	if( newline ) {
	
		fprintf( fp_out, "\n" );
	}

    	return 0;
}
