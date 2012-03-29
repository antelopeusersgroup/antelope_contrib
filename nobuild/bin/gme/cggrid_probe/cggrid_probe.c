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
    fprintf( stderr, "Usage: cggrid_probe [-e] [-n] [-f format] cggridfile { x y |xyfile}\n" );
    exit(1) ;
}

int
main( int argc, char **argv )
{
	CGGrid	*cgg;
	int	c;
	char	cggfile[FILENAME_MAX];
	char	*xyfile = 0;
	char	*base_format = DEFAULT_FORMAT;
	char	output_format[STRSZ];
	char	aline[STRSZ];
	FILE	*fp_cgg;
	FILE	*fp_xy;
	Tbl	*xy;
	int	newline = 1;
	int	echo_xy = 0;
	double	x;
	double	y;
	double	val;

    	elog_init ( argc, argv ) ; 

    	while ((c = getopt (argc, argv, "enf:")) != -1) {
		switch (c) {
		case 'n':
			newline = 0;
			break;

		case 'e':
			echo_xy = 1;
			break;

		case 'f':
			base_format = optarg;
			break;

		case '?':
			usage ();
			break ;
		}
    	}

    	if( argc - optind < 2 ) {

		usage();

	} else if( argc - optind == 2 ) {

		strcpy( cggfile, argv[optind++] );
		xyfile = argv[optind++];

	} else {

		strcpy( cggfile, argv[optind++] );
		x = atof( argv[optind++] );
		y = atof( argv[optind++] );
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

	if( xyfile == 0 ) {

		val = cggrid_probe( cgg, x, y );

		if( echo_xy ) {

			sprintf( output_format, "%s %s %s", 
				base_format, base_format, base_format );

			fprintf( stdout, output_format, x, y, val );

		} else {

			sprintf( output_format, "%s", base_format );

			fprintf( stdout, output_format, val );
		}

		if( newline ) {

			fprintf( stdout, "\n" );

		} else {

			fprintf( stdout, " " );
		}

	} else {

		if( ! strcmp( xyfile, "-" ) ) {
			
			fp_xy = stdin;

		} else {
			
			if( ! is_present( xyfile ) ) {

				elog_die( 1, 
					"Can't find xy file '%s', Bye.",
					xyfile );

			} else if( ( fp_xy = fopen( xyfile, "r" ) ) == NULL ) {

				elog_die( 1, 
					"Can't open xy file '%s', Bye.", 
					xyfile );
			}
		}

		while( getaline( fp_xy, aline, STRSZ ) ) {
			
			strtr( aline, "\t\r\n", "   " );
			xy = split( aline, ' ' );

			if( maxtbl( xy ) != 2 ) {

				elog_die( 0, 
					"Failed to interpret line '%s'. Bye\n",
					aline );
			} else {
				
				x = atof( (char *) gettbl( xy, 0 ) );
				y = atof( (char *) gettbl( xy, 1 ) );
			}

			freetbl( xy, 0 );

			val = cggrid_probe( cgg, x, y );

			if( echo_xy ) {

				sprintf( output_format, "%s %s %s", 
					base_format, base_format, base_format );

				fprintf( stdout, output_format, x, y, val );

			} else {

				sprintf( output_format, "%s", base_format );

				fprintf( stdout, output_format, val );
			}

			if( newline ) {

				fprintf( stdout, "\n" );

			} else {

				fprintf( stdout, " " );
			}
		}
	}

    	return 0;
}
