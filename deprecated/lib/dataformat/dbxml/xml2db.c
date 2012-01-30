
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
#include "db.h"
#include "stock.h"
#include "dbxml.h"

#define XML2DB_ERROR -1
#define XML2DB_NOTFOUND 0
#define XML2DB_START 1
#define XML2DB_END 2
#define XML2DB_EMPTY 3

static int 
get_token( char *source, char *next )
{
	static Hook *hook = NULL;
	char	*pattern = "</?[^>]+/?>";
	long	start = 0;
	long	nchars = 0;
	int	rc = 0;

	rc = strcontains( source, pattern, &hook, &start, &nchars);

	if( rc == 0 ) {

		return XML2DB_NOTFOUND;

	} else if( rc < 0 ) {

		return XML2DB_ERROR;
	} 

	if( *(source + start + nchars - 2) == '/' ) {
		
		return XML2DB_EMPTY;

	} else if( *(source + start + 1) == '/' ) {

		return XML2DB_END;

	} else {

		return XML2DB_START;
	}
}

int
xml2db( Dbptr db, char *xml )
{
	char	*string = NULL;
	char	*next = NULL;
	int	rc = 0;

	string = xml;

	rc = get_token( string, next );

	printf( "tag '%s': rc %d\n", string, rc );

	return rc;
}
