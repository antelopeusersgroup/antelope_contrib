
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
#include "stock.h"
#include "pf.h"

#include "pfxml.h"

#define OPTNEWLINE { if( flags & PFXML_NEWLINES ) { pushstr( (void **) &vstack, "\n" ); } }
#define TBL_ELEM "pftbl_entry"

static void 
safe_strsub( char *source, char *pattern, char *replacement, char *dest )
{
	char 	*copy;

	allot( char *, copy, strlen( source ) + STRSZ );

	strsub( source, pattern, replacement, copy );
	
	strcpy( dest, copy );

	free( copy );
}

static void
add_dataelement( void **vstack, char *value )
{
        char    *copy;

	allot( char *, copy, strlen( value ) + STRSZ );
	strcpy( copy, value );
        strtrim( copy );

        safe_strsub( copy, "&", "&amp;", copy );
        safe_strsub( copy, "<", "&lt;", copy );
        safe_strsub( copy, ">", "&gt;", copy );

        pushstr( vstack, copy );

        free( copy );
}

static char *
maketag( char *tagtype, char *name, int flags )
{
	char	*vstack = 0;

	pushstr( (void **) &vstack, "<" );	

	if( flags & PFXML_STRONG ) {

		if( strcmp( name, "" ) ) {

			pushstr( (void **) &vstack, name );	

		} else {

			pushstr( (void **) &vstack, TBL_ELEM );
		}

		if( tagtype != (char *) NULL && *tagtype != (char) NULL ) {
			pushstr( (void **) &vstack, " type=\"" );
			pushstr( (void **) &vstack, tagtype );
			pushstr( (void **) &vstack, "\"" );
		}

	} else {

		pushstr( (void **) &vstack, tagtype );	

		if( name != (char *) NULL && *name != (char) NULL ) {
			pushstr( (void **) &vstack, " name=\"" );
			pushstr( (void **) &vstack, name );
			pushstr( (void **) &vstack, "\"" );
		}
	}

	pushstr( (void **) &vstack, ">" );


	return popstr( (void **) &vstack, 1 );
}

static char *
endtag( char *tagtype, char *name, int flags )
{
	char	*vstack = 0;

	pushstr( (void **) &vstack, "</" );	

	if( flags & PFXML_STRONG ) {

		if( strcmp( name, "" ) ) {

			pushstr( (void **) &vstack, name );	

		} else {

			pushstr( (void **) &vstack, TBL_ELEM );
		}

	} else {

		pushstr( (void **) &vstack, tagtype );	
	}

	pushstr( (void **) &vstack, ">" );

	return popstr( (void **) &vstack, 1 );
}

char *
pf2xml( Pf *pf, char *name, char *prolog, int flags )
{
	int	type;
	char	*value;
	char 	*vstack = 0;
	char	*s = 0;
	char	*xml = 0;
	char	*tag;
	char	*key;
	int	ikey;
	int	irow;
	Tbl	*keys;
	
	if( pf == NULL ) {
		return NULL;
	}

	if( name == NULL ) {

		elog_log( 0, "pf2xml: name may not be null!\n" );
		return NULL;
	}

	if( prolog != NULL ) {

		pushstr( (void **) &vstack, prolog ); 
		OPTNEWLINE;
	}

	switch( pf->type ) {
	case PFFILE:
		
		if( flags & PFXML_PRESERVE_PFFILE ) {

			tag = maketag( "pffile", name, flags );

		} else {

			tag = maketag( "pfarr", name, flags );
		}

		pushstr( (void **) &vstack, tag ); 
		free( tag );
		OPTNEWLINE;

		keys = pfkeys( pf );

		for( ikey = 0; ikey < maxtbl( keys ); ikey++ ) {

			key = gettbl( keys, ikey );

			type = pfget( pf, key, (void **) &value );

			if( type == PFSTRING ) {

				tag = maketag( "pfstring", key, flags );
				pushstr( (void **) &vstack, tag ); 
				free( tag );
				add_dataelement( (void **) &vstack, value );
				tag = endtag( "pfstring", key, flags );
				pushstr( (void **) &vstack, tag ); 
				free( tag );
				OPTNEWLINE;

			} else {

				xml = pf2xml( (Pf *) value, key, 0, flags );
				pushstr( (void **) &vstack, xml );
				free( xml );
				OPTNEWLINE;
			}
		}

		if( flags & PFXML_PRESERVE_PFFILE ) {

			tag = endtag( "pffile", name, flags );

		} else {

			tag = endtag( "pfarr", name, flags );
		}

		pushstr( (void **) &vstack, tag ); 
		free( tag );
		OPTNEWLINE;

		freetbl( keys, 0 );

		break;

	case PFARR:
		
		tag = maketag( "pfarr", name, flags );
		pushstr( (void **) &vstack, tag ); 
		free( tag );
		OPTNEWLINE;

		keys = pfkeys( pf );

		for( ikey = 0; ikey < maxtbl( keys ); ikey++ ) {

			key = gettbl( keys, ikey );

			type = pfget( pf, key, (void **) &value );

			if( type == PFSTRING ) {

				tag = maketag( "pfstring", key, flags );
				pushstr( (void **) &vstack, tag ); 
				free( tag );
				add_dataelement( (void **) &vstack, value );
				tag = endtag( "pfstring", key, flags );
				pushstr( (void **) &vstack, tag ); 
				free( tag );
				OPTNEWLINE;

			} else {

				xml = pf2xml( (Pf *) value, key, 0, flags );
				pushstr( (void **) &vstack, xml );
				free( xml );
				OPTNEWLINE;
			}
		}

		tag = endtag( "pfarr", name, flags );
		pushstr( (void **) &vstack, tag);
		free( tag );

		freetbl( keys, 0 );

		break;

	case PFTBL:
		tag = maketag( "pftbl", name, flags );
		pushstr( (void **) &vstack, tag ); 
		free( tag );
		OPTNEWLINE;

		for( irow = 0; irow < pfmaxtbl( pf ); irow++ ) {

			type = pfget( pf, (char *) irow, (void **) &value );

			if( type == PFSTRING ) {

				tag = maketag( "pfstring", "", flags );
				pushstr( (void **) &vstack, tag ); 
				free( tag );
				add_dataelement( (void **) &vstack, value );
				tag = endtag( "pfstring", "", flags );
				pushstr( (void **) &vstack, tag ); 
				free( tag );
				OPTNEWLINE;

			} else {

				xml = pf2xml( (Pf *) value, "", 0, flags );
				pushstr( (void **) &vstack, xml );
				free( xml );
				OPTNEWLINE;
			}

		}

		tag = endtag( "pftbl", name, flags );
		pushstr( (void **) &vstack, tag ); 
		free( tag );

		break;
	default:
		elog_complain( 0, "pf2xml: unknown pf type %d\n", pf->type );
		return 0;
	}

	xml = popstr( (void **) &vstack, 1 );

	return xml;
}
