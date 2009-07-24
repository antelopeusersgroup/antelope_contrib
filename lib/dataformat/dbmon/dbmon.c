
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
#include "dbmon.h"

typedef struct Dbtrack {
	char	dbname[FILENAME_MAX];
	Dbptr	db;
	Arr	*tables;
	void 	(*newrow)(Dbptr, char *, char *, void *);
	void 	(*changerow)(char *, Dbptr, char *, char *, void *);
	void 	(*delrow)(Dbptr, char *, char *, void *);
} Dbtrack;

typedef struct Tabletrack {
	char	table_name[STRSZ];
	char	table_filename[FILENAME_MAX];
	Dbptr	db;
	int	table_exists;
	int	table_nrecs;
	unsigned long table_modtime;
	char	*null_sync;
	Tbl	*syncs;
	int	watch_table;
} Tabletrack;

static int compute_digest( unsigned char *buf, unsigned int len, unsigned char *digest );
static char *dbmon_compute_row_sync( Dbptr db );
static char *digest2hex( unsigned char *digest );
static Dbtrack *new_dbtrack( Dbptr db );
static Tabletrack *new_tabletrack( char *table_name );
static void free_tabletrack( void *ttrp );
static void free_dbtrack( void *dbtrp );
static void focus_tableset( Dbtrack *dbtr, Tbl *table_subset );

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

static char *
dbmon_compute_row_sync( Dbptr db )
{
	unsigned int record_size;
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

static Dbtrack *
new_dbtrack( Dbptr db )
{
	Dbtrack *dbtr = 0;
	Dbvalue val;
	Tabletrack *ttr = 0;
	Tbl	*schema_tables = 0;
	char	*table_name = 0;
	int	itable;

	allot( Dbtrack *, dbtr, 1 );

	dbtr->db = db;
	dbtr->db.table = dbALL;
	dbtr->db.field = dbALL;
	dbtr->db.record = dbALL;

	dbquery( db, dbDATABASE_NAME, &val );

	strcpy( dbtr->dbname, val.t );

	dbtr->tables = newarr( 0 );

	dbquery( db, dbSCHEMA_TABLES, &schema_tables );

	for( itable = 0; itable < maxtbl( schema_tables ); itable++ ) {

		table_name = gettbl( schema_tables, itable );
	
		ttr = new_tabletrack( table_name );

		setarr( dbtr->tables, ttr->table_name, ttr );

		ttr->db = dblookup( db, "", table_name, "", "" );

		ttr->db.record = dbNULL;

		ttr->null_sync = dbmon_compute_row_sync( ttr->db );

		ttr->db.record = dbALL;
	}

	return dbtr;
}

static Tabletrack *
new_tabletrack( char *table_name )
{
	Tabletrack *ttr = 0;

	allot( Tabletrack *, ttr, 1 );

	strcpy( ttr->table_name, table_name );

	ttr->table_exists = 0;
	ttr->table_nrecs = 0;
	ttr->watch_table = 0;
	ttr->table_modtime = 0L;

	ttr->null_sync = NULL;
	ttr->syncs = newtbl( 0 );

	return ttr;
}

static void
free_tabletrack( void *ttrp )
{
	Tabletrack *ttr = (Tabletrack *) ttrp;

	if( ttr->null_sync != (char *) NULL ) {

		free( ttr->null_sync );
	}

	if( ttr->syncs != (Tbl *) NULL ) {
		
		freetbl( ttr->syncs, free );
	}

	free( ttr );

	return;
}

static void
free_dbtrack( void *dbtrp )
{
	Dbtrack *dbtr = (Dbtrack *) dbtrp;

	freearr( dbtr->tables, free_tabletrack );

	free( dbtr );

	return;
}

static void
focus_tableset( Dbtrack *dbtr, Tbl *table_subset )
{
	Tbl	*keys;
	int	ikey;
	int	itable;
	char	*table_name;
	Tabletrack *ttr;

	if( table_subset == (Tbl *) NULL ) {

		keys = keysarr( dbtr->tables );

		for( ikey = 0; ikey < maxtbl( keys ); ikey++ ) {
		
			ttr = (Tabletrack *) getarr( dbtr->tables, gettbl( keys, ikey ) );

			ttr->watch_table = 1;
		}

		freetbl( keys, 0 );

	} else {

		for( itable = 0; itable < maxtbl( table_subset ); itable++ ) {

			table_name = gettbl( table_subset, itable );	

			ttr = (Tabletrack *) getarr( dbtr->tables, table_name );

			ttr->watch_table = 1;
		}
	}

	return;
}

Hook *
dbmon_init( Dbptr db, Tbl *table_subset, 
	    void (*newrow)(Dbptr, char *, char *, void *), 
	    void (*changerow)(char *, Dbptr, char *, char *, void *), 
	    void (*delrow)(Dbptr, char *, char *, void *), 
	    int flags )
{
	Hook	*dbmon_hook = 0;
	Dbtrack *dbtr = 0;

	dbmon_hook = new_hook( free_dbtrack );

	dbtr = new_dbtrack( db );

	focus_tableset( dbtr, table_subset );

	dbtr->newrow = newrow;
	dbtr->changerow = changerow;
	dbtr->delrow = delrow;

	dbmon_hook->p = (void *) dbtr;

	return dbmon_hook;
}

int 
dbmon_update( Hook *dbmon_hook, void *private )
{
	Dbtrack *dbtr = (Dbtrack *) dbmon_hook->p;
	Tabletrack *ttr;
	Tbl	*keys;
	Dbptr	db;
	Dbvalue val;
	int	ikey;
	int	isync;
	int	retcode = 0;
	int	new_nrecs = 0;
	char	*sync;
	char	*oldsync;

	keys = keysarr( dbtr->tables );

	for( ikey = 0; ikey < maxtbl( keys ); ikey++ ) {
		
		ttr = (Tabletrack *) getarr( dbtr->tables, gettbl( keys, ikey ) );

		if( ! ttr->watch_table ) {

			continue;
		}

		dbquery( ttr->db, dbRECORD_COUNT, &new_nrecs );	

		if( ttr->table_nrecs == 0 && new_nrecs <= 0 ) { 			/* Table still nonexistent */

			continue;

		} else if( ttr->table_nrecs == 0 && new_nrecs >= 0 ) {			/* Table appeared */

			dbquery( ttr->db, dbTABLE_FILENAME, &val );

			strcpy( ttr->table_filename, val.t );

			ttr->table_exists = 1;

			db = ttr->db;

			for( db.record = 0; db.record < new_nrecs; db.record++ ) {

				sync = dbmon_compute_row_sync( db );

				settbl( ttr->syncs, db.record, sync );

				dbtr->newrow( db, ttr->table_name, sync, private );	
			}

		} else if( ttr->table_nrecs > 0 && new_nrecs <= 0 ) { 			/* Table disappeared */

			for( isync = 0; isync < maxtbl( ttr->syncs ); isync++ ) {

				sync = (char *) gettbl( ttr->syncs, isync );

				dbtr->delrow( ttr->db, ttr->table_name, sync, private );
			}

			trunctbl( ttr->syncs, 0, free );

			ttr->table_exists = 0;

		} else if( new_nrecs < ttr->table_nrecs ) { 				/* Table shortened */

			elog_log( 0, "Table '%s' inappropriately shortened from %d to %d rows; rebuilding\n", 
				     ttr->table_filename, ttr->table_nrecs, new_nrecs );

			db = ttr->db;

			for( isync = 0; isync < maxtbl( ttr->syncs ); isync++ ) {

				sync = (char *) gettbl( ttr->syncs, isync );

				dbtr->delrow( db, ttr->table_name, sync, private );
			}

			trunctbl( ttr->syncs, 0, free );

			for( db.record = 0; db.record < new_nrecs; db.record++ ) {

				sync = dbmon_compute_row_sync( db );

				settbl( ttr->syncs, db.record, sync );

				dbtr->newrow( db, ttr->table_name, sync, private );	
			}

		} else if( new_nrecs > ttr->table_nrecs ||
		           ttr->table_modtime != filetime( ttr->table_filename ) ) { 	/* Table modified */

			db = ttr->db;

			for( db.record = 0; db.record < ttr->table_nrecs; db.record++ ) {

				oldsync = (char *) gettbl( ttr->syncs, db.record );

				sync = dbmon_compute_row_sync( db );

				if( ! strcmp( oldsync, sync ) ) {			/* same row */

					free( sync );

				} else if( ! strcmp( sync, ttr->null_sync ) ) {		/* marked row */

					dbtr->delrow( db, ttr->table_name, oldsync, private );

					settbl( ttr->syncs, db.record, strdup( ttr->null_sync ) );

					free( oldsync );

				} else {						/* changed row */

					dbtr->changerow( oldsync, db, ttr->table_name, sync, private );	

					settbl( ttr->syncs, db.record, sync );

					free( oldsync );
				}
			}

			if( new_nrecs > ttr->table_nrecs ) {

				for( db.record = ttr->table_nrecs; db.record < new_nrecs; db.record++ ) {

					sync = dbmon_compute_row_sync( db );

					settbl( ttr->syncs, db.record, sync );

					dbtr->newrow( db, ttr->table_name, sync, private );	
				}
			}

		} else {								 /* Table unchanged */

			; 	/* Do nothing */
		}
		
		ttr->table_nrecs = new_nrecs;

		ttr->table_modtime = filetime( ttr->table_filename );
	}

	freetbl( keys, 0 );

	return retcode;
}

void 
dbmon_close( Hook **dbmon_hook )
{
	free_hook( dbmon_hook );

	*dbmon_hook = NULL;

	return;
}

void 
dbmon_status( FILE *fp, Hook *dbmon_hook ) 
{
	Dbtrack *dbtr = (Dbtrack *) dbmon_hook->p;
	Tabletrack *ttr;
	Tbl	*keys;
	int	ikey;
	char	*s;

	fprintf( fp, "Monitoring database: '%s'\n", dbtr->dbname );
	fprintf( fp, "Cached database pointer: %ld %ld %ld %ld\n", 
							dbtr->db.database, 
							dbtr->db.table, 
							dbtr->db.field, 
							dbtr->db.record );

	keys = keysarr( dbtr->tables );

	for( ikey = 0; ikey < maxtbl( keys ); ikey++ ) {
		
		ttr = (Tabletrack *) getarr( dbtr->tables, gettbl( keys, ikey ) );

		fprintf( fp, "Schema table '%s':\n", ttr->table_name );

		if( ! ttr->watch_table ) {

			fprintf( fp, "\tWatched: no\n" );

			continue;
		}

		fprintf( fp, "\tFilename: '%s'\n", ttr->table_filename );

		fprintf( fp, "\tExists: %s\n", ttr->table_exists ? "yes" : "no" );

		if( ttr->table_exists ) {
			fprintf( fp, "\tFile modification time: %s\n", s = strtime( (double) ttr->table_modtime ) );
			free( s );
		}

		fprintf( fp, "\tNumber of records: %d\n", ttr->table_nrecs );

		if( ttr->null_sync != (char *) NULL ) {

			fprintf( fp, "\tNull-row sync string: %s\n", ttr->null_sync );
		}

		if( ttr->syncs != (Tbl *) NULL ) {

			debugtbl( fp, "\tSync strings:\n", ttr->syncs ); 
		}

		fprintf( fp, "\tWatched: yes\n" );
	}

	freetbl( keys, 0 );

	return;
}
