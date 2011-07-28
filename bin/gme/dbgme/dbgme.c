/* Copyright (c) 2004 Boulder Real Time Technologies, Inc. */
/* All rights reserved. */
/*                                                                     
/* Written by Dr. Kent Lindquist, Lindquist Consulting, Inc. */
/*
/* This software may be used freely in any way as long as */
/* the copyright statement above is not removed. */

#include "dbgme.h"

#define ORID_NULL -1

typedef struct Recipe_ {
	char	*recipe_name;
	char	*select;
	char 	*filter; 
	char	*delegate_name;
	int  	(*delegate_fn)();
	Pf	*pfdelegate;
} Recipe;

struct delegate_ {
	char	name[STRSZ];
	int	(*delegate_fn)();
} *Internal_delegates = 0;

static int Ndelegates = 0;

static char Pfname[FILENAME_MAX] = "";
static Pf *Pfdbgme = 0;
static Arr *Recipes = 0;
static Arr *Tried = 0;
static int Daemon_sleep_time_sec = 10;
static char *Origin_subset;
int Verbose = 0;
int Force = 0;

void
pftransfer( Pf *pfdest, Pf *pfsrc ) {
	void	*pfval;
	Tbl	*mykeys;
	char	*akey;
	int	ikey;
	int	type;
	
	mykeys = pfkeys( pfsrc );

	for( ikey = 0; ikey < maxtbl( mykeys ); ikey++ ) {
		
		akey = gettbl( mykeys, ikey );
		
		type = pfget( pfsrc, akey, &pfval );

		if( type == PFTBL || type == PFARR ) {

			pfput( pfdest, akey, pfval, PFPF );

		} else {

			pfput( pfdest, akey, pfval, type );
		}
	}

	freetbl( mykeys, 0 );

	return;
}

void 
register_delegate( char *name, int (*delegate)() )
{
	Ndelegates++;

	reallot( struct delegate_ *, Internal_delegates, Ndelegates );

	strcpy( Internal_delegates[Ndelegates-1].name, name );

	Internal_delegates[Ndelegates-1].delegate_fn = delegate;

	return;
}

Recipe *
new_recipe()
{
	Recipe	*recipe = 0;

	allot( Recipe *, recipe, 1 );

	memset( recipe, 0, sizeof( Recipe ) );

	return recipe;
}

void
free_recipe( Recipe **recipe ) 
{
	if( (*recipe)->recipe_name ) free( (*recipe)->recipe_name );
	if( (*recipe)->select ) free( (*recipe)->select );
	if( (*recipe)->filter ) free( (*recipe)->filter );
	if( (*recipe)->delegate_name ) free( (*recipe)->delegate_name );

	if( (*recipe)->pfdelegate ) pffree( (*recipe)->pfdelegate );

	free( *recipe );

	*recipe = 0;

	return;
}

static void
usage()
{
	fprintf( stderr, "\nUsage: %s [-v] [-f] [-d] [-p pfname] -r recipe "
		"[-r recipe...] [-o orid] dbname\n",
		 Program_Name );
	exit (1);
}

int
delegate_gme( Dbptr db, Recipe *recipe )
{
	
	return recipe->delegate_fn( db, recipe->pfdelegate );
}

Recipe *
init_recipe( Pf *pf, char *recipe_name ) 
{
	Recipe 	*recipe;
	Pf	*pfrecipes = 0;
	Pf	*pfrecipe = 0;
	Pf	*pfdelegate = 0;
	Pf	*pfdelegate_defaults = 0;
	int	i;
	int  	(*f)();

	if( Recipes == 0 ) {
		Recipes = newarr( 0 );	
	}

	if( pfresolve( pf, "recipes", 0, &pfrecipes ) < 0 ) {

		elog_die( 0, 
		     "Couldn't find recipes array in parameter-file '%s'\n",
		     Pfname );
	}

	if( pfresolve( pfrecipes, recipe_name, 0, &pfrecipe ) < 0 ) {

		elog_complain( 0, "Couldn't find recipe '%s'\n", recipe_name );
		return 0;
	}

	if( pfget( pf, 
		   "delegate_pf_defaults", 
		   (void **) &pfdelegate_defaults ) == PFINVALID ) {
		
		elog_die( 0, 
		     "Couldn't find 'delegate_pf_defaults' in " 
		     "parameter-file '%s'\n", Pfname );
	}

	if( pfget_string( pfdelegate_defaults, "output_file" ) == 0 ) {

		elog_die( 0, 
		     "Couldn't find 'output_file' in delegate_pf_defaults, "
		     "parameter-file '%s'\n", Pfname );
	}

	if( pfget_string( pfdelegate_defaults, "qgridfmt" ) == 0 ) {

		elog_die( 0, 
		     "Couldn't find 'qgridfmt' in delegate_pf_defaults, "
		     "parameter-file '%s'\n", Pfname );
	}

	pfresolve( pfrecipe, "delegate_pf", 0, &pfdelegate );

	recipe = new_recipe();

	recipe->delegate_name = pfget_string( pfrecipe, "delegate" );
	if( ( recipe->delegate_name == 0 ) ||
	    ( ! strcmp( recipe->delegate_name, "" ) ) ) {

		elog_complain( 0, "no delegate specified for recipe '%s'\n", 
			     recipe_name );
		free_recipe( &recipe );
		return 0;
	}

	recipe->recipe_name = strdup( recipe_name );

	if( ( f = (int (*)(char *)) usermethod( recipe->delegate_name ) ) != NULL ) {

			recipe->delegate_fn = f;

	} else {

		for( i = 0; i < Ndelegates; i++ ) {

			if( ! strcmp( recipe->delegate_name, 
				      Internal_delegates[i].name ) ) {

				recipe->delegate_fn =
					Internal_delegates[i].delegate_fn;
				break;
			}
		}
	} 
	
	if( recipe->delegate_fn == NULL ) {

		elog_complain( 0, "Delegate type '%s' not supported "
			     "and not found in libuser.\n",
			     recipe->delegate_name );
		free_recipe( &recipe );
		return 0;
	}

	recipe->select = pfget_string( pfrecipe, "select" );
	recipe->filter = pfget_string( pfrecipe, "filter" );

	recipe->pfdelegate = pfnew( PFFILE );
	pftransfer( recipe->pfdelegate, pfdelegate_defaults );
	pftransfer( recipe->pfdelegate, pfdelegate );

	pfput_string( recipe->pfdelegate, "recipe_name", recipe_name );

	setarr( Recipes, recipe_name, (void *) recipe );

	return recipe;
}

int
database_changed( Dbptr db ) 
{
	static int initialized = 0;
	static off_t origin_size = 0;
	static time_t origin_mtime = 0;
	int	present;
	Dbptr	dborigin;
	Dbvalue value;	
	struct stat origin_statbuf;

	dborigin = dblookup( db, 0, "origin", 0, 0 );

	dbquery( dborigin, dbTABLE_FILENAME, &value );
	stat( value.t, &origin_statbuf );

	if( ! initialized ) {
		
		origin_size = origin_statbuf.st_size;
		origin_mtime = origin_statbuf.st_mtime;

		initialized++;

		return 0;
	}

	if( origin_size != origin_statbuf.st_size ||
	    origin_mtime != origin_statbuf.st_mtime ) {

		origin_size = origin_statbuf.st_size;
		origin_mtime = origin_statbuf.st_mtime;

		return 1;
	}

	return 0;
}

void
free_views( Dbptr db, Tbl *views )
{
	int	i;

	for( i = 0; i < maxtbl( views ); i++ ) {

		db.table = (int) gettbl( views, i );

		if( db.table >= 0 ) {
		
			dbfree( db );
		}
	}

	freetbl( views, 0 );
}

int
process_orid( Dbptr db, Tbl *recipes, int orid )
{
	Recipe	*recipe;
	char	*recipe_name = 0;
	char	*database;
	char	expr[STRSZ];
	Dbptr	dborigin;
	Dbptr	dbassoc;
	Dbptr	dbarrival;
	Dbptr	dbsite;
	Dbptr	dbwfmeas;
	Dbptr	dbwfmgme;
	Tbl	*wfmeas_keys1, *wfmeas_keys2;
	Tbl	*wfmgme_keys1, *wfmgme_keys2;
	Tbl	*views; 
	int	nrecs;
	int	nrecs_wfmeas;
	int	nrecs_wfmgme;
	int	rc;
	int	i;

	if( Verbose ) {

		elog_notify( 0, "Processing orid %d\n", orid );
	}

	views = newtbl( 0 );

	dbquery( db, dbDATABASE_NAME, &database );

	dbwfmeas = dblookup( db, "", "wfmeas", "", "" );
	dbwfmgme = dblookup( db, "", "wfmgme", "", "" );
	dborigin = dblookup( db, "", "origin", "", "" );
	dbarrival = dblookup( db, "", "arrival", "", "" );
	dbassoc = dblookup( db, "", "assoc", "", "" );
	dbsite = dblookup( db, "", "site", "", "" );

	sprintf( expr, "orid == %d", orid );
	dborigin = dbsubset( dborigin, expr, 0 );
	pushtbl( views, (char *) dborigin.table );
	dbquery( dborigin, dbRECORD_COUNT, &nrecs );

	if( nrecs <= 0 ) {

		elog_complain( 0, "Origin %d not found in %s.origin.\n",
			orid, database );

		free_views( db, views );

		return -1;

	} else if( nrecs > 1 ) {
		
		elog_complain( 0, 
		     "Multiple entries in %s.origin for orid %d. \n",
		     database, orid );

		free_views( db, views );

		return -1;
	}

	db = dbjoin( dborigin, dbassoc, 0, 0, 0, 0, 0 );
	pushtbl( views, (char *) db.table );
	dbquery( db, dbRECORD_COUNT, &nrecs );

	if( nrecs <= 0 ) {

		elog_complain( 0, "No assoc rows for orid %d in %s.\n",
			orid, database );

		free_views( db, views );
		return -1;
	}

	db = dbjoin( dbarrival, db, 0, 0, 0, 0, 0 );
	pushtbl( views, (char *) db.table );
	dbquery( db, dbRECORD_COUNT, &nrecs );

	if( nrecs <= 0 ) {

		elog_complain( 0, "No arrival rows for orid %d in %s.\n",
			orid, database );

		free_views( db, views );
		return -1;
	}

	wfmeas_keys1 = strtbl( "arrival.sta", "arrival.time", 0 );
	wfmeas_keys2 = strtbl( "wfmeas.sta", "wfmeas.time::wfmeas.endtime", 0 );
	wfmgme_keys1 = strtbl( "arrival.sta", "arrival.time", 0 );
	wfmgme_keys2 = strtbl( "wfmgme.sta", "wfmgme.time", 0 );

	dbwfmeas = dbjoin( db, dbwfmeas, &wfmeas_keys1, &wfmeas_keys2, 0, 0, 0 );
	pushtbl( views, (char *) dbwfmeas.table );
	dbquery( dbwfmeas, dbRECORD_COUNT, &nrecs_wfmeas );

	dbwfmgme = dbjoin( db, dbwfmgme, &wfmgme_keys1, &wfmgme_keys2, 0, 0, 0 );
	pushtbl( views, (char *) dbwfmgme.table );
	dbquery( dbwfmgme, dbRECORD_COUNT, &nrecs_wfmgme );

	freetbl( wfmgme_keys1, 0 );
	freetbl( wfmgme_keys2, 0 );

	freetbl( wfmeas_keys1, 0 );
	freetbl( wfmeas_keys2, 0 );

	if( nrecs_wfmgme > 0 ) {

		db = dbwfmgme;
		nrecs = nrecs_wfmgme;

	} else if( nrecs_wfmeas > 0 ) {

		db = dbwfmeas;
		nrecs = nrecs_wfmeas;

	} else {

		elog_complain( 0, "No wfmgme or wfmeas rows for orid %d in %s.\n",
			orid, database );

		free_views( db, views );
		return -1;		
	}

	db = dbjoin( db, dbsite, 0, 0, 0, 0, 0 );
	pushtbl( views, (char *) db.table );
	dbquery( db, dbRECORD_COUNT, &nrecs );

	if( nrecs <= 0 ) {

		elog_complain( 0, "No wfmgme/wfmeas rows for orid %d after joining "
			"to site.\n", orid );

		free_views( db, views );
		return -1;
	}

	for( i = 0; i < maxtbl( recipes ); i++ ) {

		recipe_name = gettbl( recipes, i );
		
		if( Verbose ) {

			elog_notify( 0, "Starting recipe %s for orid %d\n", 
				recipe_name, orid );
		}

		recipe = getarr( Recipes, recipe_name );

		if( recipe->filter && ( strcmp( recipe->filter, "" ) ) ) {

			sprintf( expr, "filter =~ /%s/", recipe->filter );

			db = dbsubset( db, expr, 0 );
			pushtbl( views, (char *) db.table );

			dbquery( db, dbRECORD_COUNT, &nrecs );
			if( nrecs <= 0 ) { 

				elog_complain( 0, "No rows for filter '%s'\n",
					recipe->filter );
				continue;
			}
		}

		if( recipe->select && ( strcmp( recipe->select, "" ) ) ) {

			db = dbsubset( db, recipe->select, 0 );
			pushtbl( views, (char *) db.table );

			dbquery( db, dbRECORD_COUNT, &nrecs );
			if( nrecs <= 0 ) { 

				elog_complain( 0, "No rows for expression '%s'\n",
					recipe->select );
				continue;
			}
		}

		rc = delegate_gme( db, recipe );

		if( rc != 0 ) {
		
			elog_complain( 0, "Recipe %s failed for orid %d\n", 
				recipe_name, orid );

		} else if( Verbose ) {

			elog_notify( 0, "Finished recipe %s for orid %d\n", 
				recipe_name, orid );
		}
	
	}

	free_views( db, views );

	if( Verbose ) {

		elog_notify( 0, "Done with orid %d\n", orid );
	}

	return 0;
}

int
process_gridless_orids( Dbptr *dbin, Tbl *requested_recipes )
{
	Dbptr	db;
	Dbptr	dborigin;
	Dbptr	dbqgrid;
	Dbvalue	aval;
	char	*dbname;
	char	orid_string[STRSZ];
	int	nrecs;
	int	orid;
	int	is_view = 0;
	int	rc;

	/* Force refresh of views by closing and re-opening database: */

	dbquery( *dbin, dbDATABASE_NAME, &aval );
	dbname = strdup( aval.t );
	dbclose( *dbin ); 

	if( dbopen_database( dbname, "r+", dbin ) < 0 ) { 

		elog_die( 0, "Failed to re-open database %s\n", dbname ) ; 

	} else {

		free( dbname );
		db = *dbin;
	}

	dborigin = dblookup( db, "", "origin", "", "" );

	if( Origin_subset != NULL && strcmp( Origin_subset, "" ) ) {

		dborigin = dbsubset( dborigin, Origin_subset, 0 );
		is_view++;
	}

	dbquery( dborigin, dbRECORD_COUNT, &nrecs );

	if( nrecs > 0 ) {

		dbqgrid = dblookup( db, "", "qgrid", "", "" );

		dbquery( dbqgrid, dbRECORD_COUNT, &nrecs );

		if( nrecs > 0 ) {

			db = dbnojoin( dborigin, dbqgrid, 0, 0, 0 );
			is_view++;

		} else {

			db = dborigin;
		}

	} else { 

		db = dborigin;
	}

	if( Tried == (Arr *) NULL ) {
		
		Tried = newarr( 0 );
	}

	dbquery( db, dbRECORD_COUNT, &nrecs );

	for( db.record = 0; db.record < nrecs; db.record++ ) {

		dbgetv( db, 0, "origin.orid", &orid, 0 );

		sprintf( orid_string, "%d", orid );

		if( getarr( Tried, orid_string ) != NULL ) {
			
			continue;
		}

		rc = process_orid( db, requested_recipes, orid );

		if( rc < 0 ) {
			elog_complain( 0, "Problems processing orid %d\n", orid );
		}

		setarr( Tried, orid_string, (void *) 0x1 );
	}

	if( is_view ) {

		dbfree( db );
	}

	return 0;
}

int
main (int argc, char **argv)
{
	int	errflag = 0;
	int	daemon = 0;
	int	orid = -1;
	Dbptr	db;
	Dbptr	dbwfmeas;
	Dbptr	dbwfmgme;
	char	*database = 0;
	Tbl	*requested_recipes = 0;
	char	*recipe_name = 0;
	Recipe	*recipe = 0;
	char	*s;
	int	nrecs_wfmeas;
	int	nrecs_wfmgme;
	int	rc;
	int	c;
	int	i;

	elog_init ( argc, argv ) ; 
	elog_notify ( 0,
		"%s : $Revision$ $Date$ Started %s\n",
		argv[0], s = strtime( now() ) ) ;
	free( s );

	strcpy( Pfname, Program_Name );

	requested_recipes = newtbl( 0 );

	register_delegate( "matlab", delegate_to_matlab );
	register_delegate( "perl", delegate_to_perl );
	register_delegate( "sp_bssa87", sp_bssa87 );
	register_delegate( "trinetsm_es99", trinetsm_es99 );
	register_delegate( "trinetsm_es99_mmi", trinetsm_es99_mmi );

	while ((c = getopt (argc, argv, "dvfp:r:o:")) != -1) {
		switch (c) {
		case 'v':
			Verbose++;
	    		break;
		
		case 'f':
			Force++;
	    		break;
		
		case 'd':
			daemon++;
	    		break;
		
		case 'p':
			strcpy( Pfname, optarg );
			break;

		case 'o':
			orid = atoi( optarg );
			if( orid <= 0 ) {
				elog_die( 0, "orid must be greater than 0. Bye.\n" );
			}
			break;
		case 'r':
			pushtbl( requested_recipes, strdup( optarg ) );
			break;

		default:
			errflag++;
			break ;
		}
	}

	if( errflag ) {
		usage();
	}

	if( argc - optind < 1 ) {
		usage();
	} 

	if( maxtbl( requested_recipes ) < 1 ) {
		elog_complain( 1 , "Must specify at least one recipe. Bye.\n" );
		usage();
	}

	database = argv[optind++];

	if( dbopen_database( database, "r+", &db ) < 0 ) { 

		elog_die( 0, "Can't open database %s\n", database ) ; 
	}

	db = dblookup( db, 0, "qgrid", 0, 0 );

	if( db.table < 0 ) {

		elog_die( 0, "Table qgrid not in schema. Please verify that "
			"%s has the gme1.0 expansion schema.\n",
			database );
	}

	dbwfmeas = dblookup( db, "", "wfmeas", "", "" );
	dbwfmgme = dblookup( db, "", "wfmgme", "", "" );

	if( ! daemon && dbwfmeas.table < 0 && dbwfmgme.table < 0 ) {

		elog_die( 0, "Failed to open %s.wfmeas and %s.wfmgme. Bye.\n",
			database, database );
	} 

	dbquery( dbwfmeas, dbRECORD_COUNT, &nrecs_wfmeas );
	dbquery( dbwfmgme, dbRECORD_COUNT, &nrecs_wfmgme );

	if( ! daemon && nrecs_wfmeas <= 0 && nrecs_wfmgme <= 0 )  {

		elog_die( 0, "No records in %s.wfmeas or %s.wfmgme. Bye.\n",
			database, database );
	}

	if( pfread( Pfname, &Pfdbgme ) != 0 ) {

		elog_die( 0, "Can't read parameter file %s\n", Pfname );
	}

	Daemon_sleep_time_sec = pfget_int( Pfdbgme, "daemon_sleep_time_sec" );
	Origin_subset = pfget_string( Pfdbgme, "origin_subset" );

	for( i =  maxtbl( requested_recipes ) - 1; i >= 0; i-- ) {

		recipe_name = gettbl( requested_recipes, i );

		if( ( recipe = init_recipe( Pfdbgme, recipe_name ) ) == 0 ) {

			elog_complain( 0, 
			     "Couldn't initialize recipe '%s' "
			     "from parameter file '%s', skipping\n",
			     recipe_name, Pfname );

			deltbl( requested_recipes, i );
		} 
	}

	if( maxtbl( requested_recipes ) < 1 ) {

		elog_die( 0, "No more recipes to process, Bye.\n" );
	}

	if( orid != ORID_NULL ) {

		if( process_orid( db, requested_recipes, orid ) < 0 ) {

			elog_complain( 0, "Problems processing orid %d\n", orid );
		}

	} else {

		process_gridless_orids( &db, requested_recipes );
	}

	if( daemon ) {

	  for( ;; ) {

		sleep( Daemon_sleep_time_sec );	

		if( database_changed( db ) ) {

			process_gridless_orids( &db, requested_recipes );
		}
	  }

	} else if( Verbose ) {
		
		elog_notify( 0, 
		   "Finished processing all events. "
		   "Not in daemon mode, exiting.\n" );
	}

	elog_clear_register( 1 );

	return 0;
}
