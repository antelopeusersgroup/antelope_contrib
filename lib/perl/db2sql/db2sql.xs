
/*
 *   Copyright (c) 2010 Lindquist Consulting, Inc.
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
#undef warn
#include "db.h"
#include "stock.h"
#include "db2sql.h"

static char *
elogmsgs()
{
	char *log;
	log = elog_string( 0 );
	elog_clear();
	return log;
}

MODULE = Datascope::db2sql	PACKAGE = Datascope::db2sql
PROTOTYPES: DISABLE

void
dbschema2sqlcreate( idatabase, itable, ifield, irecord, ... )
	long	idatabase
	long	itable
	long	ifield
	long	irecord
	PPCODE:
	{
	Dbptr	db;
	Tbl	*sql = 0;
	char	*stmt = 0;
	long	istmt;
	long	nstmts;
	int	flags = 0;
	int	next;

	db.database = idatabase;
	db.table = itable;
	db.field = ifield;
	db.record = irecord;

	next = 4;

	if( items > next ) {

		flags |= SvIV( ST( next ) );

		next++;
	}

	sql = dbschema2sqlcreate( db, flags );

	if( sql == 0 ) {

		croak( "dbschema2sqlcreate: Failed to create sql\n%s", elogmsgs() );
	}

	nstmts = maxtbl( sql );

	for( istmt = 0; istmt < nstmts; istmt++ ) {

		stmt = (char *) gettbl( sql, istmt );

		XPUSHs( sv_2mortal( newSVpv( stmt, strlen(stmt) ) ) );
	}

	if( sql ) {

		freetbl( sql, free );
	}

	}

void
db2sqlinsert( idatabase, itable, ifield, irecord, ... )
	long	idatabase
	long	itable
	long	ifield
	long	irecord
	PPCODE:
	{
	Dbptr	db;
	Tbl	*sql = 0;
	char	*stmt = 0;
	long	istmt;
	long	nstmts;
	int	flags = 0;
	int	next;
	int	rc;

	db.database = idatabase;
	db.table = itable;
	db.field = ifield;
	db.record = irecord;

	next = 4;

	if( items > next ) {

		flags |= SvIV( ST( next ) );

		next++;
	}

	rc = db2sqlinsert( db, &sql, flags );

	if( rc < 0 ) {

		croak( "db2sqlinsert: Failed to create sql\n%s", elogmsgs() );
	}

	nstmts = maxtbl( sql );

	for( istmt = 0; istmt < nstmts; istmt++ ) {

		stmt = (char *) gettbl( sql, istmt );

		XPUSHs( sv_2mortal( newSVpv( stmt, strlen(stmt) ) ) );
	}

	if( sql ) {

		freetbl( sql, free );
	}

	}

void
pfconfig_asknoecho()
	PPCODE:
	{

	if( pfconfig( "ask", (void *) asknoecho ) ) {

		croak( "db2sql_pfconfig_noecho: pfconfig fails\n%s", elogmsgs() );
	}

	}
