
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
#include "dbmon.h"

typedef struct Perl_dbmon_track {
	Hook	*dbmon_hook;
	CV	*newrow;
	CV	*changerow;
	CV	*delrow;
	SV	*ref;
} Perl_dbmon_track;

static Arr *Hooks = NULL;

static char *
elogmsgs()
{
	char *log;
	log = elog_string( 0 );
	elog_clear();
	return log;
}

static Perl_dbmon_track *
new_perl_dbmon_track() 
{
	Perl_dbmon_track *pdmtr;

	allot( Perl_dbmon_track *, pdmtr, 1 );

	return pdmtr;
}

static void
free_perl_dbmon_track( Perl_dbmon_track *pdmtr )
{
	if( pdmtr->dbmon_hook != (Hook *) NULL ) {

		free_hook( &pdmtr->dbmon_hook );
	}

	free( pdmtr );

	return;
}

static void
perl_newrow( Dbptr db, char *table, char *sync, void *private )
{
	Perl_dbmon_track *pdmtr = (Perl_dbmon_track *) private;
	int	n;
	dSP;

	ENTER;
	SAVETMPS;
	PUSHMARK( sp );

	XPUSHs(sv_2mortal(newSViv(db.database)));
	XPUSHs(sv_2mortal(newSViv(db.table)));
	XPUSHs(sv_2mortal(newSViv(db.field)));
	XPUSHs(sv_2mortal(newSViv(db.record)));

	XPUSHs(sv_2mortal(newSVpv(table, 0)));
	XPUSHs(sv_2mortal(newSVpv(sync, 0)));

	XPUSHs(sv_mortalcopy(pdmtr->ref));

	PUTBACK;

	n = perl_call_sv( (SV *) pdmtr->newrow, G_DISCARD );

	SPAGAIN;

	PUTBACK;
	FREETMPS;
	LEAVE;
}

static void
perl_changerow( char *oldsync, Dbptr db, char *table, char *sync, void *private )
{
	Perl_dbmon_track *pdmtr = (Perl_dbmon_track *) private;
	int	n;
	dSP;

	ENTER;
	SAVETMPS;
	PUSHMARK( sp );

	XPUSHs(sv_2mortal(newSVpv(oldsync, 0)));

	XPUSHs(sv_2mortal(newSViv(db.database)));
	XPUSHs(sv_2mortal(newSViv(db.table)));
	XPUSHs(sv_2mortal(newSViv(db.field)));
	XPUSHs(sv_2mortal(newSViv(db.record)));

	XPUSHs(sv_2mortal(newSVpv(table, 0)));
	XPUSHs(sv_2mortal(newSVpv(sync, 0)));

	XPUSHs(sv_mortalcopy(pdmtr->ref));

	PUTBACK;

	n = perl_call_sv( (SV *) pdmtr->changerow, G_DISCARD );

	SPAGAIN;

	PUTBACK;
	FREETMPS;
	LEAVE;
}

static void
perl_delrow( Dbptr db, char *table, char *sync, void *private )
{
	Perl_dbmon_track *pdmtr = (Perl_dbmon_track *) private;
	int	n;
	dSP;

	ENTER;
	SAVETMPS;
	PUSHMARK( sp );

	XPUSHs(sv_2mortal(newSViv(db.database)));
	XPUSHs(sv_2mortal(newSViv(db.table)));
	XPUSHs(sv_2mortal(newSViv(db.field)));
	XPUSHs(sv_2mortal(newSViv(db.record)));

	XPUSHs(sv_2mortal(newSVpv(table, 0)));
	XPUSHs(sv_2mortal(newSVpv(sync, 0)));

	XPUSHs(sv_mortalcopy(pdmtr->ref));

	PUTBACK;

	n = perl_call_sv( (SV *) pdmtr->delrow, G_DISCARD );

	SPAGAIN;

	PUTBACK;
	FREETMPS;
	LEAVE;
}

MODULE = Datascope::dbmon	PACKAGE = Datascope::dbmon
PROTOTYPES: DISABLE

void
dbmon_init( idatabase, itable, ifield, irecord, hookname, newrow, changerow, delrow, ... )
	long	idatabase
	long	itable
	long	ifield
	long	irecord
	char	*hookname
	CV	*newrow
	CV 	*changerow
	CV 	*delrow
	PPCODE:
	{
	Dbptr	db;
	Perl_dbmon_track *pdmtr = NULL;
	Perl_dbmon_track *old = NULL;
	Tbl	*table_subset = NULL;;
	long	i;
	int	flags = 0;

	db.database = idatabase;
	db.table = itable;
	db.field = ifield;
	db.record = irecord;

	if( items > 8 ) {

		table_subset = newtbl( items - 8 );

		for( i = 8; i < items; i++ ) {

			pushtbl( table_subset, SvPV_nolen( ST(i) ) );
		}
	}

	pdmtr = new_perl_dbmon_track();

	pdmtr->newrow = newrow;
	pdmtr->changerow = changerow;
	pdmtr->delrow = delrow;

	pdmtr->dbmon_hook = dbmon_init( db, table_subset, perl_newrow, perl_changerow, perl_delrow, flags );

	if( table_subset != (Tbl *) NULL ) {
		
		freetbl( table_subset, 0 );
	}

	if( pdmtr->dbmon_hook == 0 ) {

		croak( "dbmon_init: Failed to initiate database monitoring\n%s", elogmsgs() );
	}

	if( Hooks == NULL ) {

		Hooks = newarr( NULL );
	}

	old = (Perl_dbmon_track *) setarr( Hooks, hookname, (void *) pdmtr );

	if( old != (Perl_dbmon_track *) NULL ) {

		free_perl_dbmon_track( old );
	}

	}

void 
dbmon_update( hookname, ref ) 
	char	*hookname
	SV	*ref
	PPCODE:
	{
	Perl_dbmon_track *pdmtr;

	if( Hooks == NULL || 
	    ( pdmtr = (Perl_dbmon_track *) getarr( Hooks, hookname ) ) == NULL ) {

		croak( "dbmon_update: Couldn't find hook by name of '%s'\n", hookname );
	}

	pdmtr->ref = ref;

	dbmon_update( pdmtr->dbmon_hook, (void *) pdmtr );

	}

void 
dbmon_close( hookname )
	char	*hookname
	PPCODE:
	{
	Perl_dbmon_track *pdmtr;

	if( Hooks == NULL || 
	    ( pdmtr = (Perl_dbmon_track *) getarr( Hooks, hookname ) ) == NULL ) {

		croak( "dbmon_close: Couldn't find hook by name of '%s'\n", hookname );
	}

	dbmon_close( &pdmtr->dbmon_hook );

	delarr( Hooks, hookname );

	free( pdmtr );

	}
