
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
#include <stdio.h>
#include <string.h>
#include "db.h"
#include "stock.h"
#include "db2sql.h"
#include "crc.h"

static int find_longest( void *s, void *private );
static int dbfield_isnull( Dbptr db );
static char *generate_sqltable_create( Dbptr db, long flags );
static char *generate_sqlrow_insert( Dbptr db, char *(*createsync)(Dbptr db), long flags );
static long generate_sqltable_insert( Dbptr db, Tbl **tbl, char *(*createsync)(Dbptr db), long flags );

static int
find_longest( void *s, void *longest )
{
	long	l;

	l = strlen( (char *) s );

	if( l > *((long *) longest) ) {

		*((long *) longest) = l;
	}

	return 0;
}

static int
dbfield_isnull( Dbptr db )
{
	char	*fnull = NULL;
	long	fsize;
	char	*stringval = NULL;
	int	isnull = 0;

	dbquery( db, dbFIELD_SIZE, &fsize );
	dbquery( db, dbNULL, &fnull );

	allot( char *, stringval, fsize + 2 );

	dbget( db, stringval );

	strtrim( stringval );

	if( ! strcmp( stringval, fnull ) ) {

		isnull = 1;

	} else {

		isnull = 0;
	}

	free( stringval );

	return isnull;
}

static char *
generate_sqlrow_insert( Dbptr db, char *(*createsync)(Dbptr db), long flags )
{
	void	*stk = 0;
	char	*table;
	Tbl	*fields;
	Dbvalue	dbvalue;
	char	*field;
	long	ifield;
	long	ftype;
	char	*fformat;
	char	part[STRSZ];
	char	*sync;
	char	*copy;

	if( db.record < 0 && 
	    db.record != dbSCRATCH && 
	    db.record != dbNULL ) {

		return NULL;
	}

	dbquery( db, dbTABLE_NAME, &table );
	dbquery( db, dbTABLE_FIELDS, &fields );

	pushstr( &stk, "INSERT INTO `" );
	pushstr( &stk, table );

	pushstr( &stk, "` VALUES(" );

	for( ifield = 0; ifield < maxtbl( fields ); ifield++ ) {
		
		field = gettbl( fields, ifield );

		db = dblookup( db, "", "", field, "" );

		dbquery( db, dbFIELD_TYPE, &ftype );
		dbquery( db, dbFIELD_FORMAT, &fformat );

		dbgetv( db, 0, field, &dbvalue, NULL );

		if( ifield > 0 ) {

			pushstr( &stk, ", " );
		}

		if( ! ( flags & DB2SQL_USE_DATASCOPE_NULLS ) && dbfield_isnull( db ) ) {

			pushstr( &stk, "NULL" );

			continue;
		}

		memset( part, '\0', STRSZ );

		switch( ftype ) {

		case dbSTRING:
			pushstr( &stk, "'" );
			if( strchr( dbvalue.s, '\'' ) == (char *) NULL ) {
				pushstr( &stk, dbvalue.s );
			} else {
				allot( char *, copy, 2 * strlen( dbvalue.s ) );
				strsub( dbvalue.s, "'", "\\'", copy );
				pushstr( &stk, copy );
				free( copy );
			}
			pushstr( &stk, "'" );
			break;

		case dbREAL:
		case dbTIME:
			sprintf( part, fformat, dbvalue.d );
			pushstr( &stk, part );
			break;

		case dbINTEGER:
		case dbYEARDAY:
			sprintf( part, fformat, dbvalue.i );
			pushstr( &stk, part );
			break;

		case dbDBPTR:
			sprintf( part, "%ld %ld %ld %ld", 
					dbvalue.db.database, 
					dbvalue.db.table, 
					dbvalue.db.field, 
					dbvalue.db.record );
			pushstr( &stk, part );
			break;
		}
	}

	if( ! ( flags & DB2SQL_OMIT_SYNC ) ) {

		if( createsync != (char *(*)(Dbptr db)) NULL ) {

			sync = (*createsync)( db );

		} else {

			sync = (char *) NULL;
		}

		pushstr( &stk, ", '" );

		if( sync != (char *) NULL ) {

			pushstr( &stk, sync );

		} else {

			pushstr( &stk, DB2SQL_SYNCFIELD_NULL );
		}

		pushstr( &stk, "'" );

		if( sync != (char *) NULL ) {

			free( sync );
		}
	}

	pushstr( &stk, ");\n" );

	return popstr( &stk, 1 );
}

static char *
generate_sqltable_create( Dbptr db, long flags )
{
	char	*table;
	char	part[STRSZ];
	void	*stk = 0;
	Tbl	*primary;
	Tbl	*fields;
	long	ifield;
	char	*field;
	char	field_a[STRSZ];
	char	field_b[STRSZ];
	char	*fnull;
	long	fsize;
	long	ftype;
	char	*fformat;
	int	precision;
	int	scale;
	long	longest = 0;

	if( db.table < 0 ) {

		return NULL;
	}

	dbquery( db, dbTABLE_NAME, &table );

	pushstr( &stk, "CREATE TABLE `" );
	pushstr( &stk, table );
	pushstr( &stk, "`\n  (\n" );

	dbquery( db, dbTABLE_FIELDS, &fields );

	applytbl( fields, find_longest, (void *) &longest );

	if( ! ( flags & DB2SQL_OMIT_SYNC ) && longest < strlen( Db2sql_syncfield_name ) ) {

		longest = strlen( Db2sql_syncfield_name );
	}

	for( ifield = 0; ifield < maxtbl( fields ); ifield++ ) {

		if( ifield > 0 ) {

			pushstr( &stk, ",\n" );
		}

		field = gettbl( fields, ifield );

		db = dblookup( db, "", table, field, "" );

		pushstr( &stk, "  `" );
		pushstr( &stk, field );
		pushstr( &stk, "`" );
		pushstr( &stk, spaces( longest - strlen(field) + 2 ) );

		dbquery( db, dbFIELD_SIZE, &fsize );
		dbquery( db, dbFIELD_TYPE, &ftype );
		dbquery( db, dbFIELD_FORMAT, &fformat );
		dbquery( db, dbNULL, &fnull );

		memset( part, '\0', STRSZ );

		switch( ftype ) {

		case dbSTRING:
			if( fsize < 256 ) {
				sprintf( part, "CHAR(%ld)", fsize );
			} else {
				sprintf( part, "TEXT(%ld)", fsize );
			}
			pushstr( &stk, part );
			break;

		case dbREAL:
		case dbTIME:
			if( fnull != (char *) NULL && strcontains( fnull, "[eE]", 0, 0, 0 ) ) {
				sprintf( part, "DOUBLE" );
			} else {
				sscanf( fformat, "%%%d.%d", &precision, &scale );
				sprintf( part, "DECIMAL(%d,%d)", precision, scale );
			}
			pushstr( &stk, part );
			break;

		case dbINTEGER:
		case dbYEARDAY:
			sprintf( part, "INTEGER(%ld)", fsize );
			pushstr( &stk, part );
			break;

		case dbDBPTR:
			pushstr( &stk, "CHAR(32)" );
			break;
		}

		/* Disallow defaults for SQL TEXT fields */

		if( fnull != (char *) NULL && strncmp( part, "TEXT", 4 ) ) {

			pushstr( &stk, " DEFAULT " );

			if( ftype == dbSTRING ) {
		
				pushstr( &stk, "'" );
				pushstr( &stk, fnull );
				pushstr( &stk, "'" );

			} else {

				pushstr( &stk, fnull );
			}
		}
	}

	if( ! ( flags & DB2SQL_OMIT_SYNC ) ) {

		pushstr( &stk, ",\n" );

		pushstr( &stk, "  `" );
		pushstr( &stk, Db2sql_syncfield_name );
		pushstr( &stk, "`" );
		pushstr( &stk, spaces( longest - strlen(Db2sql_syncfield_name) + 2 ) );

		pushstr( &stk, DB2SQL_SYNCFIELD_SPEC );
	}

	dbquery( db, dbPRIMARY_KEY, &primary );

	if( maxtbl( primary ) > 0 ) {

		pushstr( &stk, ",\n  PRIMARY KEY (" );

		for( ifield = 0; ifield < maxtbl( primary ); ifield++ ) {

			if( ifield > 0 ) {

				pushstr( &stk, ", " );
			}
	
			field = gettbl( primary, ifield );

			if( strcontains( field, "::", 0, 0, 0 ) ) {

				strsub( field, "::", "  ", field );

				sscanf( field, "%s %s", field_a, field_b );

				pushstr( &stk, "`" );
				pushstr( &stk, field_a );
				pushstr( &stk, "`" );

				/* end-of-range keys can be NULL in Datascope,
				   so don't use it as primary in SQL: 

				pushstr( &stk, ", `" );
				pushstr( &stk, field_b );
				pushstr( &stk, "`" );
				*/

			} else {

				pushstr( &stk, "`" );
				pushstr( &stk, field );
				pushstr( &stk, "`" );
			}
		}

		pushstr( &stk, ")" );
	}

	pushstr( &stk, "\n  );\n" );

	return popstr( &stk, 1 );
}

static long
generate_sqltable_insert( Dbptr db, Tbl **tbl, char *(*createsync)(Dbptr db), long flags ) 
{
	char	*cmd;
	long	nrecs;

	if( *tbl == (Tbl *) NULL ) {
		
		*tbl = newtbl( 0 );
	}

	dbquery( db, dbRECORD_COUNT, &nrecs ); 

	for( db.record = 0; db.record < nrecs; db.record++ ) {
			
		cmd = generate_sqlrow_insert( db, createsync, flags );

		pushtbl( *tbl, cmd );
	}

	return nrecs;
}

int
db2sqldelete( Dbptr db, char *sync, Tbl **tbl, long flags )
{
	void	*stk = 0;
	char	*table;
	char	*cmd;

	if( *tbl == (Tbl *) NULL ) {
		
		*tbl = newtbl( 0 );
	}

	dbquery( db, dbTABLE_NAME, &table );

	pushstr( &stk, "DELETE from `" );
	pushstr( &stk, table );

	pushstr( &stk, "` WHERE " );

	pushstr( &stk, "  `" );
	pushstr( &stk, Db2sql_syncfield_name );
	pushstr( &stk, "` = '" );
	pushstr( &stk, sync );
	pushstr( &stk, "'" );
	pushstr( &stk, ";\n" );

	cmd = popstr( &stk, 1 );

	pushtbl( *tbl, cmd );

	return 1;
}

long 
db2sqlinsert( Dbptr db, Tbl **tbl, char *(*createsync)(Dbptr db), long flags )
{
	char	*cmd;
	long	ncmds = 0;
	long	table_is_view = 0;
	Tbl	*tables;
	char	*table;
	long	itable;

	if( *tbl == (Tbl *) NULL ) {
		
		*tbl = newtbl( 0 );
	}

	if( db.database == dbINVALID ||
	    db.table == dbINVALID ) {

		return 0;

	} else if( db.table == dbALL ) {

		dbquery( db, dbSCHEMA_TABLES, &tables );

		for( itable = 0; itable < maxtbl( tables ); itable++ ) {

			table = gettbl( tables, itable );

			db = dblookup( db, "", table, "", "" );

			ncmds += generate_sqltable_insert( db, tbl, createsync, flags );
		}

	} else if( db.table >= 0 ) {

		dbquery( db, dbTABLE_IS_VIEW, &table_is_view );

		if( table_is_view ) {

			elog_complain( 0, "db2sqlinsert does not support non-base-table views\n" );

			return 0;

		} else {

			if( db.record == dbSCRATCH || 
			    db.record == dbNULL || 
			    db.record >= 0 ) {

				cmd = generate_sqlrow_insert( db, createsync, flags );

				pushtbl( *tbl, cmd );

				ncmds++;

			} else {

				ncmds += generate_sqltable_insert( db, tbl, createsync, flags );
			}
		}
	}

	return ncmds;
}

Tbl *
dbschema2sqlcreate( Dbptr db, long flags )
{
	Tbl	*sql;
	char	*cmd;
	Tbl	*tables;
	char	*table;
	long	itable;

	sql = newtbl( 0 );

	if( db.table >= 0 ) {
		
		cmd = generate_sqltable_create( db, flags );

		pushtbl( sql, cmd );

	} else {

		dbquery( db, dbSCHEMA_TABLES, &tables );

		for( itable = 0; itable < maxtbl( tables ); itable++ ) {

			table = gettbl( tables, itable );

			db = dblookup( db, "", table, "", "" );
			
			cmd = generate_sqltable_create( db, flags );

			pushtbl( sql, cmd );
		}
	}

	return sql;
}

char *
db2sql_get_syncfield_name( void )
{
	char	*name = NULL;

	if( Db2sql_syncfield_name == (char *) NULL ) {

		name = strdup( DB2SQL_SYNCFIELD_NAME_DEFAULT );

	} else {

		name = strdup( Db2sql_syncfield_name );
	}

	return name;
}

void 
db2sql_set_syncfield_name( char *name )
{
	if( name != (char *) NULL ) {

		if( Db2sql_syncfield_name != (char *) NULL &&
		    strcmp( Db2sql_syncfield_name, DB2SQL_SYNCFIELD_NAME_DEFAULT ) ) {

			free( Db2sql_syncfield_name );    	
		}

		Db2sql_syncfield_name = strdup( name );
	}
}
