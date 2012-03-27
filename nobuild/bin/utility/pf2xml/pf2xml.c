
/*
 *   Copyright (c) 2007 Lindquist Consulting, Inc.
 *   All rights reserved. 
 *                                                                     
 *   Written by Dr. Kent Lindquist, Lindquist Consulting, Inc. 
 *
 *   This software is licensed under the New BSD license: 
 *
 *   Redistribution and use in source and binary forms,
 *   with or without modification, are permitted provided
 *   that the following conditions are met:
 *   
 *   * Redistributions of source code must retain the above
 *   copyright notice, this list of conditions and the
 *   following disclaimer.
 *   
 *   * Redistributions in binary form must reproduce the
 *   above copyright notice, this list of conditions and
 *   the following disclaimer in the documentation and/or
 *   other materials provided with the distribution.
 *   
 *   * Neither the name of Lindquist Consulting, Inc. nor
 *   the names of its contributors may be used to endorse
 *   or promote products derived from this software without
 *   specific prior written permission.

 *   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
 *   CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED
 *   WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 *   WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 *   PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL
 *   THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY
 *   DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 *   CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 *   PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 *   USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 *   HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
 *   IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 *   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
 *   USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 *   POSSIBILITY OF SUCH DAMAGE.
 *
 */

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
	char	dir[FILENAME_MAX];
	char	base[FILENAME_MAX];
	char	suffix[FILENAME_MAX];
	FILE	*fp;

	elog_init( argc, argv );
	
	while( (c = getopt( argc, argv, "fnsh:" )) != -1 ) {
		switch( c ) {
		case 'f':
			flags |= PFXML_PRESERVE_PFFILE;
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
			elog_die( 1, "pf2xml: Failed to open header_file %s\n", header_file );
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
		elog_die( 1, "pf2xml: Couldn't find parameter file %s\n", pfname );
	} else if( rc < 0 ) {
		elog_die( 1, "pf2xml: problem reading parameter file %s\n", pfname );
	}
	
	parsepath( pfname, dir, base, suffix );

	xml = pf2xml( pf, base, prolog, flags );

	if( xml == NULL ) {
		elog_die( 1, "pf2xml: conversion failed\n" );
	} else {
		printf( "%s", xml );
	}

	free( xml );
	if( header_file ) free( header_file );
	if( prolog ) free( prolog );

	return 0;
}
