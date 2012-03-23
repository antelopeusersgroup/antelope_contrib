
/*
 *   Copyright (c) 2010-2011 Lindquist Consulting, Inc.
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

static char * perl_createsync( Dbptr db );

CV *Createsync;

static char *
elogmsgs()
{
	char *log;
	log = elog_string( 0 );
	elog_clear();
	return log;
}

static char *
perl_createsync( Dbptr db )
{
	int	n;
	char	*sync;
	dSP;

	ENTER;
	SAVETMPS;
	PUSHMARK( sp );

	XPUSHs(sv_2mortal(newSViv(db.database)));
	XPUSHs(sv_2mortal(newSViv(db.table)));
	XPUSHs(sv_2mortal(newSViv(db.field)));
	XPUSHs(sv_2mortal(newSViv(db.record)));

	PUTBACK;

	n = perl_call_sv( (SV *) Createsync, G_SCALAR );

	if( n != 1 ) {

		croak( "db2sqlinsert: Failed to compute sync field via callback function provided\n%s", elogmsgs() );
	}

	SPAGAIN;

	sync = strdup((char *) POPp);

	PUTBACK;
	FREETMPS;
	LEAVE;

	return sync;
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
	long	flags = 0;
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
	long	flags = 0;
	int	next;
	long	rc;

	db.database = idatabase;
	db.table = itable;
	db.field = ifield;
	db.record = irecord;

	next = 4;

	if( items >= 5 ) {

		Createsync = (CV *) ST( next );

		next++;
	}

	if( items >= 6 ) {

		flags |= SvIV( ST( next ) );

		next++;
	}

	rc = db2sqlinsert( db, &sql, perl_createsync, flags );

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
db2sqldelete( idatabase, itable, ifield, irecord, sync, ... )
	long	idatabase
	long	itable
	long	ifield
	long	irecord
	char	*sync
	PPCODE:
	{
	Dbptr	db;
	Tbl	*sqltbl = (Tbl *) NULL;
	char	*sqlcmd = (char *) NULL;
	int	next;
	long	flags = 0;
	int	rc;

	db.database = idatabase;
	db.table = itable;
	db.field = ifield;
	db.record = irecord;

	next = 5;

	if( items > 5 ) {

		flags |= SvIV( ST( next ) );

		next++;
	}

	rc = db2sqldelete( db, sync, &sqltbl, flags );

	if( rc < 1 || sqltbl == (Tbl *) NULL || maxtbl( sqltbl ) < 1 ) {

		croak( "db2sqldelete: Failed to create sql\n%s", elogmsgs() );
	}

	sqlcmd = (char *) poptbl( sqltbl );

	if( sqlcmd == (char *) NULL ) {

		croak( "db2sqldelete: Failed to create sql\n%s", elogmsgs() );
	}

	XPUSHs( sv_2mortal( newSVpv( sqlcmd, strlen(sqlcmd) ) ) );

	free( sqlcmd );

	freetbl( sqltbl, free );

	}

void
db2sql_get_syncfield_name()
	PPCODE:
	{
	char	*name = (char *) NULL;

	name = db2sql_get_syncfield_name();

	XPUSHs( sv_2mortal( newSVpv( name, strlen(name) ) ) );

	free( name );

	}

void 
db2sql_set_syncfield_name( name )
	char	*name
	PPCODE:
	{

	db2sql_set_syncfield_name( name );

	}

void
pfconfig_asknoecho()
	PPCODE:
	{

	if( pfconfig( "ask", (void *) asknoecho ) ) {

		croak( "db2sql_pfconfig_noecho: pfconfig fails\n%s", elogmsgs() );
	}

	}
