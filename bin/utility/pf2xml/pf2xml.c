#include <stdlib.h>
#include <stdio.h>
#include "stock.h"
#include "pf.h"
#include "pfxml.h"
#include "elog.h"

void
usage()
{
	fprintf( stderr, "Usage: pf2xml [-s] [-n] [-h file] pfname\n" );
}

int 
main( int argc, char **argv )
{
	Pf	*pf;
	char	*pfname;
	char	*xml = 0;
	char	*header_file = 0;
	char	*prolog = 0;
	char	*prolog_ptr;
	int	prolog_bufsize = 0;
	int 	flags = 0;
	int	offset;
	int	rc;
	char	c;
	FILE	*fp;

	elog_init( argc, argv );
	
	while( (c = getopt( argc, argv, "ansh:" )) != -1 ) {
		switch( c ) {
		case 'a':
			flags |= PFXML_PFFILE_TO_PFARR;
			break;
		case 'n':
			flags |= PFXML_NEWLINES;
			break;
		case 's':
			flags |= PFXML_STRONG;
			break;
		case 'h':
			header_file = strdup( optarg );
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

	if( header_file ) {

		if( ! strcmp( header_file, "-" ) ) {

			fp = stdin;

		} else {

			fp = fopen( header_file, "r" );
		}

		if( fp == (FILE *) NULL ) {
			die( 1, "pf2xml: Failed to open header_file %s\n", header_file );
		}

		allot( char *, prolog, STRSZ );
		memset( prolog, '\0', STRSZ );
		prolog_bufsize += STRSZ;
		prolog_ptr = prolog;

		while( ( c = getc( fp ) ) != EOF ) {
			memcpy( prolog_ptr++, &c, 1 );
			if( ( offset = ( prolog_ptr - prolog ) ) > prolog_bufsize - 2 ) {
				reallot( char *, prolog, prolog_bufsize + STRSZ );
				prolog_bufsize += STRSZ;
				memset( prolog_ptr, '\0', prolog_bufsize - offset );
				prolog_ptr = prolog + offset;
			}
		}

		if( fp != stdin ) {

			fclose( fp );
		}
	}

	rc = pfread( pfname, &pf );
	if( rc == -1 ) {
		die( 1, "pf2xml: Couldn't find parameter file %s\n", pfname );
	} else if( rc < 0 ) {
		die( 1, "pf2xml: problem reading parameter file %s\n", pfname );
	}

	xml = pf2xml( pf, pfname, prolog, flags );

	if( xml == NULL ) {
		die( 1, "pf2xml: conversion failed\n" );
	} else {
		printf( "%s", xml );
	}

	free( xml );
	if( header_file ) free( header_file );
	if( prolog ) free( prolog );

	return 0;
}
