#include <stdlib.h>
#include <stdio.h>
#include "stock.h"
#include "pf.h"
#include "pfxml.h"
#include "elog.h"

void
usage()
{
	fprintf( stderr, "Usage: pf2xml [-s] [-n] pfname\n" );
}

int 
main( int argc, char **argv )
{
	Pf	*pf;
	char	*pfname;
	char	*xml = 0;
	int	rc;
	char	c;
	int 	flags = 0;

	elog_init( argc, argv );
	
	while( (c = getopt( argc, argv, "ns" )) != -1 ) {
		switch( c ) {
		case 'n':
			flags |= PFXML_NEWLINES;
			break;
		case 's':
			flags |= PFXML_STRONG;
			break;
		default:
			usage();
			exit( -1 );
		}
	}

	if( argc - optind != 1 ) {
		usage();
		exit( -1 );
	} else {
		pfname = argv[optind++];	
	}

	rc = pfread( pfname, &pf );
	if( rc == -1 ) {
		die( 1, "pf2xml: Couldn't find parameter file %s\n", pfname );
	} else if( rc < 0 ) {
		die( 1, "pf2xml: problem reading parameter file %s\n", pfname );
	}

	xml = pf2xml( pf, pfname, 0, flags );

	if( xml == NULL ) {
		die( 1, "pf2xml: conversion failed\n" );
	} else {
		printf( "%s", xml );
	}

	return 0;
}
