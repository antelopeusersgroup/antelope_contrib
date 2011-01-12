
/*
 *   Copyright (c) 2009-2010 Lindquist Consulting, Inc.
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
#include "db.h"
#include "stock.h"
#include "crc.h"

char *private_compute_row_sync( Dbptr db );

static int compute_digest( unsigned char *buf, unsigned int len, unsigned char *digest );
static char *digest2hex( unsigned char *digest );

static int compute_digest( unsigned char *buf, unsigned int len, unsigned char *digest )
{
	struct sha_ctx ctx;

	sha_init( &ctx );
	sha_update( &ctx, (unsigned char *) buf, len );
	sha_final( &ctx );
	sha_digest( &ctx, digest );

	return 0;
}

static char *
digest2hex( unsigned char *digest )
{
	char	*hex;
	int	i;

	allot( char *, hex, 41 );
	
	for( i=0; i<20; i++ ) {
		
		sprintf( &hex[2*i], "%02x", digest[i] );
	}

	hex[40] = '\0';

	return hex;
}

char *
private_compute_row_sync( Dbptr db )
{
	unsigned long record_size;
	unsigned char digest[20];
	char	*sync = (char *) NULL;
	char	*row = (char *) NULL;

	db.field = dbALL;

	dbquery( db, dbRECORD_SIZE, &record_size );

	allot( char *, row, record_size + 2 );

	dbget( db, row );

	compute_digest( (unsigned char *) row, record_size, &digest[0] );

	free( row ); 

	sync = digest2hex( digest );

	return sync;
}

