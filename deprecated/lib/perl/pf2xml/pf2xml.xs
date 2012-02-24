
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

#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"

#undef die
#include "db.h"
#include "stock.h"
#include "pfxml.h"

static char *
elogmsgs()
{
	char *log;
	log = elog_string( 0 );
	elog_clear();
	return log;
}

MODULE = Datascope::pf2xml	PACKAGE = Datascope::pf2xml
PROTOTYPES: DISABLE

void
pf2xml( ... )
	PPCODE:
	{
	Pf	*pf = 0;
	char	*xml = 0;
	char	*pfname = 0;
	char	*rootname = 0;
	char	*prolog = 0;
	char	*arg = 0;
	int	flags = 0;
	int	next = 0;

	if( items < 2 || items > 6 ) {
		
		croak( "Usage: pf2xml( [-f], [-n], [-s], pfname, " 
		       "rootname, [, header] )" );
	}

	next = 0;

	while( items > next ) {

		arg = (char *) SvPV_nolen( ST( next ) );

		if( ! strcmp( arg, "-f" ) ) {

			flags |= PFXML_PRESERVE_PFFILE;

		} else if( ! strcmp( arg, "-n" ) ) {

			flags |= PFXML_NEWLINES;
			
		} else if( ! strcmp( arg, "-s" ) ) {

			flags |= PFXML_STRONG;

		} else if( pfname == NULL ) {

			pfname = arg;

		} else if( rootname == NULL ) {

			rootname = arg;

		} else if( prolog == NULL ) {

			prolog = arg;
		}

		next++;
	}

	pf = getPf( pfname );

	if( pf == NULL ) {
		
		if( elog_mark() > 0 ) {

			croak( "Error in parameter file '%s':\n\t%s", 
				pfname, elogmsgs() );

		} else {

			croak( "Couldn't find parameter file '%s':\n\t%s",
				pfname, elogmsgs() );
		}
	}

	xml = pf2xml( pf, rootname, prolog, flags );

	if( xml == 0 ) {

		croak( "pf2xml: Failed to create xml\n%s", elogmsgs() );
	}

	XPUSHs( sv_2mortal( newSVpv( xml, strlen(xml) ) ) );

	if( xml ) {

		free( xml );
	}

	}

void
pffree( name )
	char *name;
	PPCODE:
	{

	if( items != 1 ) {
		
		croak( "Usage: pffree( name )" );
	}

	freePf( name );

	}

