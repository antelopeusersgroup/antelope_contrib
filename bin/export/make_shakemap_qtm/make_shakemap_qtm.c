/*
 * Copyright (c) 2005 Lindquist Consulting, Inc. 
 * All rights reserved. 
 *
 * Written by Dr. Kent Lindquist, Lindquist Consulting, Inc.
 * 
 * This program may be used freely in any way as long as the
 * copyright statement above is not removed. 
 *
 */

#include <stdlib.h>
#include "cgeom.h"

int
main( int argc, char **argv ) {
	CGGrid	*cgg;
	char	cggrid_file[FILENAME_MAX];
	char	out_file_base[FILENAME_MAX];
	char	binout_file[FILENAME_MAX];
	char	txtout_file[FILENAME_MAX];
	float 	lonmin;
	float 	loninc;
	float 	lonmax;
	float 	latmin;
	float 	latinc;
	float 	latmax;
	int 	lonnum;
	int 	latnum;
	double 	lon;
	double	lat;
	short	fakevel;
	FILE	*B;
	FILE	*T;
	FILE	*C;
	int	ilat;
	int	ilon;

	if( argc != 9 ) {
		
		elog_die( 0, "Usage: make_shakemap_qtm cggrid_file outfile lonmin lonmax lonincrement latmin latmax latincrement\n" );

	} else {
	
		strcpy( cggrid_file, argv[1] );
		strcpy( out_file_base, argv[2] );
		lonmin = atof( argv[3] );
		lonmax = atof( argv[4] );
		loninc = atof( argv[5] );
		latmin = atof( argv[6] );
		latmax = atof( argv[7] );
		latinc = atof( argv[8] );
	}

	sprintf( binout_file, "%s.bin", out_file_base );
	sprintf( txtout_file, "%s.txt", out_file_base );

	C = fopen( cggrid_file, "r" );

	if( C == (FILE *) NULL ) {
		
		elog_die( 1, "Failed to open cggrid file %s. Bye.\n", cggrid_file );
	} 

	cgg = cggrid_read( C );

	if( cgg == (CGGrid *) NULL ) {
		
		elog_die( 1, "Failed to read cggrid from file %s. Bye.\n", cggrid_file );
	} 
	fclose( C );

	lonnum = ( lonmax - lonmin ) / loninc;
	latnum = ( latmax - latmin ) / latinc; 

	B = fopen( binout_file, "w" );
	T = fopen( txtout_file, "w" );

	if( B == (FILE *) NULL ) {
		
		elog_die( 1, "Failed to open file %s for output. Bye.\n", cggrid_file );
	} 
	
	fwrite( (void *) &lonmin, sizeof( float ), 1, B );
	fwrite( (void *) &loninc, sizeof( float ), 1, B );
	fwrite( (void *) &lonnum, sizeof( int ), 1, B );
	fwrite( (void *) &latmin, sizeof( float ), 1, B );
	fwrite( (void *) &latinc, sizeof( float ), 1, B );
	fwrite( (void *) &latnum, sizeof( int ), 1, B );

	for( ilat = 0; ilat < latnum; ilat++ ) {

  	  for( ilon = 0; ilon < lonnum; ilon++ ) {

		lon = lonmin + ilon * loninc;
		lat = latmin + ilat * latinc;

		fakevel = (short) floor( cggrid_probe( cgg, lon, lat ) );

		fwrite( (void *) &fakevel, sizeof( short ), 1, B );

		fprintf( T, "%.3f,%.3f,%d\n", lon, lat, fakevel );
  	  }
	}

	fclose( B );
	fclose( T );

}
