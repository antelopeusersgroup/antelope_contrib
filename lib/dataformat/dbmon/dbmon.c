
/*
 *   Copyright (c) 2009-2011 Lindquist Consulting, Inc.
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
	void 	(*newrow)(Dbptr, char *, long, char *, void *);
	void 	(*changerow)(char *, Dbptr, char *, long, char *, void *);
	void 	(*delrow)(Dbptr, char *, char *, void *);
} Dbtrack;

typedef struct Tabletrack {
	char	table_name[STRSZ];
	char	table_filename[FILENAME_MAX];
	Dbptr	db;
	int	table_exists;
	long	table_nrecs;
	unsigned long table_modtime;
	char	*null_sync;
	Tbl	*syncs;
	int	watch_table;
} Tabletrack;

static Dbtrack *new_dbtrack( Dbptr db );
static Tabletrack *new_tabletrack( char *table_name );
static int sort_aslong( char *ap, char *bp, void *pvt );
static int compute_digest( unsigned char *buf, unsigned int len, unsigned char *digest );
static char *digest2hex( unsigned char *digest );
static void free_tabletrack( void *ttrp );
static void free_dbtrack( void *dbtrp );
static void focus_tableset( Dbtrack *dbtr, Tbl *table_subset );
static void dbmon_build_table( Dbtrack *dbtr, Tabletrack *ttr, void *pvt );
static void dbmon_delete_table( Dbtrack *dbtr, Tabletrack *ttr, void *pvt );
static void dbmon_resync_table( Dbtrack *dbtr, Tabletrack *ttr, void *pvt );

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
	char	*hex = NULL;
	int	i = 0;

	allot( char *, hex, 41 );
	
	for( i=0; i<20; i++ ) {
		
		sprintf( &hex[2*i], "%02x", digest[i] );
	}

	hex[40] = '\0';

	return hex;
}

static Dbtrack *
new_dbtrack( Dbptr db )
{
	Dbtrack *dbtr = NULL;
	Dbvalue val;
	Tabletrack *ttr = NULL;
	Tbl	*schema_tables = NULL;
	char	*table_name = NULL;
	long	itable = 0L;

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
	Tabletrack *ttr = NULL;

	allot( Tabletrack *, ttr, 1 );

	strcpy( ttr->table_name, table_name );

	memset( ttr->table_filename, '\0', FILENAME_MAX );

	ttr->table_exists = 0;
	ttr->table_nrecs = 0L;
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
	Tbl	*keys = NULL;
	long	ikey = 0L;
	long	itable = 0L;
	char	*table_name = NULL;
	Tabletrack *ttr = NULL;

	if( table_subset == (Tbl *) NULL ) {

		keys = keysarr( dbtr->tables );

		for( ikey = 0; ikey < maxtbl( keys ); ikey++ ) {
		
			ttr = (Tabletrack *) getarr( dbtr->tables, 
						     gettbl( keys, ikey ) );

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

static void
dbmon_build_table( Dbtrack *dbtr, Tabletrack *ttr, void *pvt )
{
	Dbptr	db;
	Dbptr	dbscratch;
	long	irecord = 0L;
	long	new_nrecs = 0L;
	char	*sync = NULL;

	db = ttr->db;

	dbscratch = dblookup( db, "", "", "", "dbSCRATCH" );

	dbquery( ttr->db, dbRECORD_COUNT, &new_nrecs );	

	for( irecord = 0; irecord < new_nrecs; irecord++ ) {

		db.record = irecord;

		dbget( db, NULL );

		sync = dbmon_compute_row_sync( dbscratch );

		if( settbl( ttr->syncs, irecord, sync ) != irecord ) {

			elog_complain( 0, "Unexpected failure of settbl for "
					  "index %ld\n", irecord );
		}

		dbtr->newrow( dbscratch, ttr->table_name, irecord, sync, pvt );	
	}

	return;
}

static void
dbmon_delete_table( Dbtrack *dbtr, Tabletrack *ttr, void *pvt )
{
	long	isync;
	char	*sync;

	for( isync = 0; isync < maxtbl( ttr->syncs ); isync++ ) {

		sync = (char *) gettbl( ttr->syncs, isync );

		dbtr->delrow( ttr->db, ttr->table_name, sync, pvt );
	}

	trunctbl( ttr->syncs, 0, free );

	return;
}

static int
sort_aslong( char *ap, char *bp, void *pvt )
{
	long	a = (long) ap;
	long	b = (long) ap;
	int	retcode = 0;

	if( a < b ) {

		retcode = -1;

	} else if( a == b ) {
	
		retcode = 0;

	} else {

		retcode = 1;
	}

	return retcode;
}

static void
dbmon_resync_table( Dbtrack *dbtr, Tabletrack *ttr, void *pvt )
{
	long	isync = 0L;
	long	irecord = 0L;
	long	new_nrecs = 0L;
	long	old_nsyncs = 0L;
	long	imatch = 0L;
	long	nmatches = 0L;
	char	*oldsync = NULL;
	char	*newsync = NULL;
	Tbl	*newsyncs = NULL;
	Tbl	*oldrows_preserve = NULL;
	Tbl	*matches = NULL;
	int	oldrows_preserve_first = 0;
	Dbptr	db;
	Dbptr	dbscratch;

	db = ttr->db;

	dbscratch = dblookup( db, "", "", "", "dbSCRATCH" );

	old_nsyncs = maxtbl( ttr->syncs );

	newsyncs = newtbl( 0 );

	oldrows_preserve = newtbl( 0 );

	/* Use dbquery in attempt to prevent bus error from reading past end 
	   of table that may still be shortening: */

	for( irecord = 0; 
		irecord < dbquery( ttr->db, dbRECORD_COUNT, &new_nrecs ); 
		   irecord++ ) {

		db.record = irecord;

		dbget( db, NULL );

		newsync = dbmon_compute_row_sync( dbscratch );

		if( settbl( newsyncs, irecord, newsync ) != irecord ) {

			elog_complain( 0, "Unexpected failure of settbl for "
					  "index %ld\n", irecord );
		}

		if( ! strcmp( newsync, ttr->null_sync ) ) {

			; /* Do nothing with null row */

		} else if( irecord < old_nsyncs &&
			   ( oldsync = gettbl( ttr->syncs, irecord ) ) != (char *) NULL &&
			   ! strcmp( newsync, oldsync ) ) {

			if( irecord == 0L ) {

				oldrows_preserve_first = 1;
			
			} else {

				pushtbl( oldrows_preserve, (void *) irecord );
			}

		} else {

			matches = greptbl( newsync, ttr->syncs );

			nmatches = maxtbl( matches );

			if( nmatches <= 0 ) {

				dbtr->newrow( dbscratch, ttr->table_name, irecord, newsync, pvt );	

			} else {

				if( nmatches > 1 ) {

					elog_complain( 0, 
						"Warning: there were multiple matches "
						"(%ld of them) for sync '%s' in libdbmon's "
						"previous view of table '%s'\n",
						nmatches, newsync, ttr->table_name );

					sorttbl( matches, sort_aslong, NULL );
				}

				for( imatch = 0; imatch < nmatches; imatch++ ) {

					isync = (long) gettbl( matches, imatch );

					pushtbl( oldrows_preserve, (void *) isync );
				}
			}

			freetbl( matches, NULL );
		}
	}

	while( ( isync = (long) poptbl( oldrows_preserve ) ) != 0L ) {

		oldsync = deltbl( ttr->syncs, isync );

		if( oldsync != (char *) NULL ) {

			free( oldsync );
		}
	}

	if( oldrows_preserve_first ) {

		oldsync = (char *) deltbl( ttr->syncs, 0L );

		if( oldsync != (char *) NULL ) {

			free( oldsync );
		}
	}

	for( isync = 0L; isync < maxtbl( ttr->syncs ); isync++ ) {

		oldsync = (char *) gettbl( ttr->syncs, isync );

		if( strcmp( oldsync, ttr->null_sync ) ) {

			dbtr->delrow( db, ttr->table_name, oldsync, pvt );
		}
	}

	freetbl( oldrows_preserve, NULL );

	freetbl( ttr->syncs, free );

	ttr->syncs = newsyncs;

	return;
}

char *
dbmon_compute_row_sync( Dbptr db )
{
	unsigned long record_size = 0L;
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

Hook *
dbmon_init( Dbptr db, Tbl *table_subset, 
	    void (*newrow)(Dbptr, char *, long, char *, void *), 
	    void (*changerow)(char *, Dbptr, char *, long, char *, void *), 
	    void (*delrow)(Dbptr, char *, char *, void *), 
	    int flags )
{
	Hook	*dbmon_hook = NULL;
	Dbtrack *dbtr = NULL;

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
dbmon_update( Hook *dbmon_hook, void *pvt )
{
	Dbtrack *dbtr = (Dbtrack *) dbmon_hook->p;
	Tabletrack *ttr;
	Tbl	*keys;
	Dbvalue val;
	int	retcode = 0;
	long	ikey;
	long	new_nrecs = 0L;
	char	cmd[FILENAME_MAX+STRSZ];

	sprintf( cmd, "orb2db_msg %s wait", dbtr->dbname );

	system( cmd );

	keys = keysarr( dbtr->tables );

	for( ikey = 0; ikey < maxtbl( keys ); ikey++ ) {
		
		ttr = (Tabletrack *) getarr( dbtr->tables, gettbl( keys, ikey ) );

		if( ! ttr->watch_table ) {

			continue;
		}

		dbflush_indexes( ttr->db );

		dbquery( ttr->db, dbRECORD_COUNT, &new_nrecs );	

		if( ttr->table_nrecs == 0 && new_nrecs <= 0 ) { 			/* Table still nonexistent */

			continue;

		} else if( ttr->table_nrecs == 0 && new_nrecs >= 0 ) {			/* Table appeared */

			elog_notify( 0, "DBMON DEBUG: Table %s appeared\n", ttr->table_name );

			dbquery( ttr->db, dbTABLE_FILENAME, &val );

			strcpy( ttr->table_filename, val.t );

			ttr->table_exists = 1;

			dbmon_build_table( dbtr, ttr, pvt );

		} else if( ttr->table_nrecs > 0 && new_nrecs <= 0 ) { 			/* Table disappeared */

			elog_notify( 0, "DBMON DEBUG: Table %s disappeared\n", ttr->table_name );

			dbmon_delete_table( dbtr, ttr, pvt );

			ttr->table_exists = 0;

		} else if( ttr->table_modtime != filetime( ttr->table_filename ) ) { 	/* Table changed */

			elog_notify( 0, "DBMON DEBUG: Table %s changed\n", ttr->table_name );

			dbmon_resync_table( dbtr, ttr, pvt );

		} else {								 /* Table unchanged */

			elog_notify( 0, "DBMON DEBUG: Table %s unchanged\n", ttr->table_name );

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
	Tabletrack *ttr = NULL;
	Tbl	*keys = NULL;
	long	ikey = 0L;
	char	*s = NULL;

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

		fprintf( fp, "\tNumber of records: %ld\n", ttr->table_nrecs );

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
