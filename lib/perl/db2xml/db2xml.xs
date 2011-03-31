
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
#include "dbxml.h"

static char *
elogmsgs()
{
	char *log;
	log = elog_string( 0 );
	elog_clear();
	return log;
}

MODULE = Datascope::db2xml	PACKAGE = Datascope::db2xml
PROTOTYPES: DISABLE

void
db2xml( idatabase, itable, ifield, irecord, ... )
	int	idatabase
	int	itable
	int	ifield
	int	irecord
	PPCODE:
	{
	Dbptr	db;
	char	*xml = 0;
	char	*rootnode = 0;
	char	*rownode = 0;
	char	*primary = 0;
	Tbl	*fields = 0;
	Tbl	*expressions = 0;
	AV	*av;
	SV	**svarrvalp;
	int	iarrval;
	int	flags = 0;
	int	next;

	db.database = idatabase;
	db.table = itable;
	db.field = ifield;
	db.record = irecord;

	next = 4;

	if( items > next ) {

		primary = (char *) SvPV_nolen( ST( next ) );

		if( ! strcmp( primary, "-p" ) ) {

			flags |= DBXML_PRIMARY;

			next++;
		}
	}

	if( items > next ) {

		rootnode = (char *) SvPV_nolen( ST( next ) );

		if( ! strcmp( rootnode, "" ) ) {

			rootnode = 0;
		}
	}

	next++;

	if( items > next ) {

		rownode = (char *) SvPV_nolen( ST( next ) );

		if( ! strcmp( rownode, "" ) ) {

			rownode = 0;
		}
	}

	next++;

	if( items > next ) {

		if( ! SvROK( ST( next ) ) ||
		    SvTYPE( SvRV( ST( next ) ) ) != SVt_PVAV ) {

		    croak( "db2xml: %dth argument must be an array reference",
			   next - 2 );
		}

		av = (AV *) SvRV( ST( next ) );

		if( av_len( av ) >= 0 ) {

			fields = newtbl( 0 );
		}

		for( iarrval = 0; iarrval <= av_len( av ); iarrval++ ) {

			svarrvalp = av_fetch( av, iarrval, 0 );
			
			pushtbl( fields, 
				 (void *) SvPV_nolen( *svarrvalp ) );
		}
	}

	next++;

	if( items > next ) {

		if( ! SvROK( ST( next ) ) ||
		    SvTYPE( SvRV( ST( next ) ) ) != SVt_PVAV ) {

		    croak( "db2xml: %dth argument must be an array reference",
			   next - 2 );
		}

		av = (AV *) SvRV( ST( next ) );

		if( av_len( av ) >= 0 ) {

			expressions = newtbl( 0 );
		}

		for( iarrval = 0; iarrval <= av_len( av ); iarrval++ ) {

			svarrvalp = av_fetch( av, iarrval, 0 );
			
			pushtbl( expressions, 
				 (void *) SvPV_nolen( *svarrvalp ) );
		}
	}

	db2xml( db, rootnode, rownode, fields, expressions, 
		(void**) &xml, flags );

	if( fields ) {

		freetbl( fields, 0 );
	}

	if( expressions ) {

		freetbl( expressions, 0 );
	}

	if( xml == 0 ) {

		croak( "db2xml: Failed to create xml\n%s", elogmsgs() );
	}

	EXTEND( sp, 1 );

	PUSHs(sv_2mortal(newSVpv( xml, strlen(xml) ) ) );

	if( xml ) {

		free( xml );
	}

	}
