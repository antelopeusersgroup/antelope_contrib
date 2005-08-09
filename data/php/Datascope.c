/*
 * Antelope interface for PHP
 *
 * Copyright (c) 2005 Lindquist Consulting, Inc.
 * All rights reserved. 
 *                                                                     
 * Written by Dr. Kent Lindquist, Lindquist Consulting, Inc. 
 * 
 * This software may be used freely in any way as long as 
 * the copyright statement above is not removed. 
 * 
 */

#include "php.h"
#include "php_ini.h"
#include "ext/standard/info.h"
#include "php_Datascope.h"
#include "db.h"
#include "tr.h"
#include "stock.h"
#include "coords.h"
#include "response.h"
#include "pf.h"
#include "dbxml.h"

static int le_Datascope;
static int le_dbresponse;

function_entry Datascope_functions[] = {
	PHP_FE(dbex_eval, NULL)		
	PHP_FE(dbextfile, NULL)		
	PHP_FE(dbfind, NULL)		
	PHP_FE(dbgetv, NULL)		
	PHP_FE(dbaddv, NULL)		
	PHP_FE(dbputv, NULL)		
	PHP_FE(dbget, NULL)		
	PHP_FE(dbput, NULL)		
	PHP_FE(dbaddnull, NULL)		
	PHP_FE(dbadd, NULL)		
	PHP_FE(dblookup, NULL)		
	PHP_FE(dbnrecs, NULL)		
	PHP_FE(ds_dbopen, NULL)		
	PHP_FE(ds_dbopen_database, NULL)		
	PHP_FE(ds_dbopen_table, NULL)		
	PHP_FE(ds_dbtmp, NULL)		
	PHP_FE(ds_dbcreate, NULL)		
	PHP_FE(dbfree, NULL)		
	PHP_FE(ds_dbclose, NULL)		
	PHP_FE(dbdestroy, NULL)		
	PHP_FE(dbtruncate, NULL)		
	PHP_FE(dbsort, NULL)		
	PHP_FE(dbgroup, NULL)		
	PHP_FE(dbungroup, NULL)		
	PHP_FE(dbjoin, NULL)		
	PHP_FE(dbnojoin, NULL)		
	PHP_FE(dbtheta, NULL)		
	PHP_FE(dbprocess, NULL)		
	PHP_FE(dbsubset, NULL)		
	PHP_FE(dbseparate, NULL)		
	PHP_FE(dbsever, NULL)		
	PHP_FE(dbunjoin, NULL)		
	PHP_FE(db2xml, NULL)		
	PHP_FE(dbquery, NULL)		
	PHP_FE(dbresponse, NULL)		
	PHP_FE(dbwrite_view, NULL)		
	PHP_FE(dbread_view, NULL)		
	PHP_FE(dbsave_view, NULL)		
	PHP_FE(dbmark, NULL)		
	PHP_FE(dbdelete, NULL)		
	PHP_FE(dbcrunch, NULL)		
	PHP_FE(eval_response, NULL)		
	PHP_FE(pfget, NULL)		
	PHP_FE(pfget_boolean, NULL)		
	PHP_FE(strtdelta, NULL)		
	PHP_FE(strtime, NULL)		
	PHP_FE(strydtime, NULL)		
	PHP_FE(strdate, NULL)		
	PHP_FE(strlocaltime, NULL)		
	PHP_FE(strlocalydtime, NULL)		
	PHP_FE(strlocaldate, NULL)		
	PHP_FE(trapply_calib, NULL)		
	PHP_FE(trloadchan, NULL)		
	PHP_FE(trfree, NULL)		
	PHP_FE(trextract_data, NULL)		
	PHP_FE(trsplit, NULL)		
	PHP_FE(trsplice, NULL)		
	{NULL, NULL, NULL}	
};

zend_module_entry Datascope_module_entry = {
	STANDARD_MODULE_HEADER,
	"Datascope",
	Datascope_functions,
	PHP_MINIT(Datascope),
	PHP_MSHUTDOWN(Datascope),
	NULL,
	NULL,
	PHP_MINFO(Datascope),
	"0.1",
	STANDARD_MODULE_PROPERTIES
};

ZEND_GET_MODULE(Datascope)

static void
_php_free_dbresponse( zend_rsrc_list_entry *rsrc TSRMLS_DC ) {
	Response *response = (Response *) rsrc->ptr;
	
	free_response( response );
}

PHP_MINIT_FUNCTION(Datascope)
{

	le_dbresponse = zend_register_list_destructors_ex( 
					_php_free_dbresponse, NULL, 
					"dbresponse", module_number );

	return SUCCESS;
}

PHP_MSHUTDOWN_FUNCTION(Datascope)
{
	return SUCCESS;
}

PHP_MINFO_FUNCTION(Datascope)
{
	php_info_print_table_start();
	php_info_print_table_header(2, "Datascope support", "enabled");
	php_info_print_table_end();
}

int
z_arrval_to_dbptr( zval *array, Dbptr *db )
{
	HashTable *target_hash;
	zval	**entry;

	if( Z_TYPE_P( array ) != IS_ARRAY ) {

		zend_error( E_ERROR, "dbptr argument is not an array\n" );

	} else if( zend_hash_num_elements( Z_ARRVAL_P( array ) ) != 4 ) {

		zend_error( E_ERROR, "dbptr argument does not have four elements\n" );
	}

	target_hash = HASH_OF( array );

	zend_hash_internal_pointer_reset( target_hash );
	zend_hash_get_current_data( target_hash, (void **) &entry );
	db->database = Z_LVAL_PP( entry );

	zend_hash_move_forward( target_hash );
	zend_hash_get_current_data( target_hash, (void **) &entry );
	db->table = Z_LVAL_PP( entry );

	zend_hash_move_forward( target_hash );
	zend_hash_get_current_data( target_hash, (void **) &entry );
	db->field = Z_LVAL_PP( entry );

	zend_hash_move_forward( target_hash );
	zend_hash_get_current_data( target_hash, (void **) &entry );
	db->record = Z_LVAL_PP( entry );

	return 0;
}

int
z_arrval_to_strtbl( zval *array, Tbl **tbl )
{
	HashTable *target_hash;
	zval	**entry;
	int	nelements;

	if( Z_TYPE_P( array ) != IS_ARRAY ) {

		*tbl = 0;
		return -1;

	} 

	nelements = zend_hash_num_elements( Z_ARRVAL_P( array ) );

	if( nelements < 1 ) {

		*tbl = 0;
		return -1;
	}

	*tbl = newtbl( 0 );

	target_hash = HASH_OF( array );

	zend_hash_internal_pointer_reset( target_hash );

	while( nelements-- > 0 ) {

		zend_hash_get_current_data( target_hash, (void **) &entry );

		pushtbl( *tbl, strdup( Z_STRVAL_PP( entry ) ) );

		zend_hash_move_forward( target_hash );
	}

	return 0;
}

int
zval_to_dbvalue( zval **zvalue, int type, Dbvalue *value )
{
	if( value == (Dbvalue *) NULL ) {

		return -1;
	}

	switch( type ) {
	case dbDBPTR:
		if( z_arrval_to_dbptr( *zvalue, &value->db ) < 0 ) {
			return -1;
		}
		break;
	case dbSTRING:
		if( Z_TYPE_PP( zvalue ) == IS_STRING ) {
			strcpy( value->s, Z_STRVAL_PP( zvalue ) );
		} else if( Z_TYPE_PP( zvalue ) == IS_DOUBLE ) {
			sprintf( value->s, "%f", Z_DVAL_PP( zvalue ) );
		} else if( Z_TYPE_PP( zvalue ) == IS_LONG ) {
			sprintf( value->s, "%f", Z_LVAL_PP( zvalue ) );
		} else if( Z_TYPE_PP( zvalue ) == IS_BOOL ) {
			if( Z_BVAL_PP( zvalue ) ) { 
				sprintf( value->s, "true" );
			} else {
				sprintf( value->s, "false" );
			}
		} else {
			return -1;
		}
		break;
	case dbBOOLEAN:
		if( Z_TYPE_PP( zvalue ) == IS_DOUBLE ) {
			value->i = (int) Z_DVAL_PP( zvalue );
		} else if( Z_TYPE_PP( zvalue ) == IS_LONG ) {
			value->i = Z_LVAL_PP( zvalue );
		} else if( Z_TYPE_PP( zvalue ) == IS_BOOL ) {
			value->i = Z_BVAL_PP( zvalue );
		} else if( Z_TYPE_PP( zvalue ) == IS_STRING ) {
			/* SCAFFOLD: Should translate string */
			value->i = atoi( Z_STRVAL_PP( zvalue ) );
		} else {
			return -1;
		}
		break;
	case dbINTEGER:
	case dbYEARDAY:
		if( Z_TYPE_PP( zvalue ) == IS_DOUBLE ) {
			value->i = (int) Z_DVAL_PP( zvalue );
		} else if( Z_TYPE_PP( zvalue ) == IS_LONG ) {
			value->i = Z_LVAL_PP( zvalue );
		} else if( Z_TYPE_PP( zvalue ) == IS_BOOL ) {
			value->i = Z_BVAL_PP( zvalue );
		} else if( Z_TYPE_PP( zvalue ) == IS_STRING ) {
			value->i = atoi( Z_STRVAL_PP( zvalue ) );
		} else {
			return -1;
		}
		break;
	case dbREAL:
		if( Z_TYPE_PP( zvalue ) == IS_DOUBLE ) {
			value->d = Z_DVAL_PP( zvalue );
		} else if( Z_TYPE_PP( zvalue ) == IS_LONG ) {
			value->d = Z_LVAL_PP( zvalue );
		} else if( Z_TYPE_PP( zvalue ) == IS_STRING ) {
			value->d = atof( Z_STRVAL_PP( zvalue ) );
		} else {
			return -1;
		}
		break;
	case dbTIME:
		if( Z_TYPE_PP( zvalue ) == IS_DOUBLE ) {
			value->d = Z_DVAL_PP( zvalue );
		} else if( Z_TYPE_PP( zvalue ) == IS_LONG ) {
			value->d = Z_LVAL_PP( zvalue );
		} else if( Z_TYPE_PP( zvalue ) == IS_STRING ) {
			value->d = str2epoch( Z_STRVAL_PP( zvalue ) );
		} else {
			return -1;
		}
		break;
	default:
		return -1;
		break;
	}

	return 0;
}

int
pf2zval( Pf *pf, zval *result ) {
	Pf	*pfvalue;
	int	ivalue;
	int	retcode = 0;
	zval	*element;
	Tbl	*keys;
	char	*key;

	switch( pf->type ) {
	case PFSTRING:

		ZVAL_STRING( result, pfexpand( pf ), 1 );
		break;

	case PFTBL:

		array_init( result );

		for( ivalue = 0; ivalue < maxtbl( pf->value.tbl ); ivalue++ ) {

			pfvalue = (Pf *) gettbl( pf->value.tbl, ivalue );

			MAKE_STD_ZVAL( element );

			pf2zval( pfvalue, element );

			add_index_zval( result, ivalue, element );
		}

		break;

	case PFFILE:
	case PFARR:

		keys = keysarr( pf->value.arr );

		array_init( result );

		for( ivalue = 0; ivalue < maxtbl( keys ); ivalue++ ) {

			key = gettbl( keys, ivalue );

			pfvalue = (Pf *) getarr( pf->value.arr, key );

			MAKE_STD_ZVAL( element );

			pf2zval( pfvalue, element );

			add_assoc_zval( result, key, element );
		}

		break;

	case PFINVALID:
	default:
		retcode = 1;
		break;
	}

	return retcode;
}

/* {{{ proto array template( array db, ... ) *
PHP_FUNCTION(template)
{
	zval	*db_array;
	Dbptr	db;
	int	argc = ZEND_NUM_ARGS();

	if( argc != X ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "a", 
					&db_array )
	    == FAILURE) {

		return;

	} else if( z_arrval_to_dbptr( db_array, &db ) < 0 ) {

		return;
	}
}
/* }}} */

/* {{{ proto mixed pfget( string pfname, string pfkey ) */
PHP_FUNCTION(pfget)
{
	int	argc = ZEND_NUM_ARGS();
	char	*pfname;
	int	pfname_len;
	char	*key;
	int	key_len;
	Pf	*pf;
	Pf	*pfvalue;
	char	*string_value;
	char	errstring[STRSZ];
	int	rc;

	if( argc != 2 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "ss", 
					&pfname, &pfname_len,
					&key, &key_len )
	    == FAILURE) {

		return;
	}

	if( ( pf = getPf( pfname ) ) == (Pf *) NULL ) {

		zend_error( E_ERROR, "failure opening parameter file\n" );
	}

	rc = pfresolve( pf, key, 0, &pfvalue );

	if( rc < 0  ) {

		sprintf( errstring, 
			"parameter '%s' not found in parameter-file '%s'\n",
			key, pfname );

		zend_error( E_ERROR, errstring );
	} 
	
	if( pf2zval( pfvalue, return_value ) < 0 ) {

		zend_error( E_ERROR, "pfget: failed to convert value\n" );
	} 

	return;
}
/* }}} */

/* {{{ proto mixed pfget_boolean( string pfname, string pfkey ) */
PHP_FUNCTION(pfget_boolean)
{
	int	argc = ZEND_NUM_ARGS();
	char	*pfname;
	int	pfname_len;
	char	*key;
	int	key_len;
	Pf	*pf;
	int	rc;

	if( argc != 2 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "ss", 
					&pfname, &pfname_len,
					&key, &key_len )
	    == FAILURE) {

		return;
	}

	if( ( pf = getPf( pfname ) ) == (Pf *) NULL ) {

		zend_error( E_ERROR, "failure opening parameter file\n" );
	}

	rc = pfget_boolean( pf, key );

	RETURN_LONG( rc );
}
/* }}} */

/* {{{ proto array trextract_data( array tr ) */
PHP_FUNCTION(trextract_data)
{
	zval	*tr_array;
	Dbptr	tr;
	int	argc = ZEND_NUM_ARGS();
	int 	single_row = 0;
	int	nrecs;
	int	nsamp = 0;
	float	*data = NULL;
	int	i;

	if( argc != 1 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "a", &tr_array )
	    == FAILURE) {

		return;

	} else if( z_arrval_to_dbptr( tr_array, &tr ) < 0 ) {

		return;
	}

	dbquery( tr, dbRECORD_COUNT, &nrecs );

	if( tr.record == dbALL ) {
		if( nrecs == 1 ) {
			single_row = 1;
			tr.record = 0;
		} else {
			single_row = 0;
		}
	} else {
		single_row = 1;
	}

	if( ! single_row ) {

		zend_error( E_ERROR, "trextract_data requires that the "
			"trace-object point at or contain only a single row\n");
	}

	dbgetv( tr, 0, "nsamp", &nsamp, "data", &data, 0 );

	if( nsamp == 0 || data == NULL ) {
	
		zend_error( E_ERROR, 
			"trextract_data: no data in trace object\n" );
	}

	array_init( return_value );

	for( i = 0; i < nsamp; i++ ) {
		
		add_index_double( return_value, i, (double) data[i] );
	}

	return;
}
/* }}} */

/* {{{ proto void trapply_calib( array tr ) */
PHP_FUNCTION(trapply_calib)
{
	zval	*tr_array;
	Dbptr	tr;
	int	argc = ZEND_NUM_ARGS();

	if( argc != 1 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "a", 
					&tr_array )
	    == FAILURE) {

		return;

	} else if( z_arrval_to_dbptr( tr_array, &tr ) < 0 ) {

		return;
	}

	trapply_calib( tr );

	return;
}
/* }}} */

PHP_FUNCTION(trsplit)
{
	zval	*tr_array;
	Dbptr	tr;
	int	argc = ZEND_NUM_ARGS();

	if( argc != 1 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "a", 
					&tr_array )
	    == FAILURE) {

		return;

	} else if( z_arrval_to_dbptr( tr_array, &tr ) < 0 ) {

		return;
	}

	trsplit( tr, 0, 0 );

	return;
}
/* }}} */

PHP_FUNCTION(trsplice)
{
	zval	*tr_array;
	Dbptr	tr;
	int	argc = ZEND_NUM_ARGS();

	if( argc != 1 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "a", 
					&tr_array )
	    == FAILURE) {

		return;

	} else if( z_arrval_to_dbptr( tr_array, &tr ) < 0 ) {

		return;
	}

	trsplice( tr, trTOLERANCE, 0, 0 );

	return;
}
/* }}} */

/* {{{ proto void trfree( array tr ) */
PHP_FUNCTION(trfree)
{
	zval	*tr_array;
	Dbptr	tr;
	int	argc = ZEND_NUM_ARGS();

	if( argc != 1 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "a", 
					&tr_array )
	    == FAILURE) {

		return;

	} else if( z_arrval_to_dbptr( tr_array, &tr ) < 0 ) {

		return;
	}

	trfree( tr );

	return;
}
/* }}} */

/* {{{ proto array trloadchan( array db, double t0, double t1, string sta, string chan ) */
PHP_FUNCTION(trloadchan)
{
	zval	*db_array;
	Dbptr	db;
	Dbptr	tr;
	int	argc = ZEND_NUM_ARGS();
	double	t0;
	double	t1;
	char	*sta;
	int	sta_len;
	char	*chan;
	int	chan_len;

	if( argc != 5 ) {

		WRONG_PARAM_COUNT;
	}

	/* SCAFFOLD should overload t0 and t1 to optionally be strings */
	if( zend_parse_parameters( argc TSRMLS_CC, "addss", 
					&db_array, &t0, &t1,
					&sta, &sta_len, &chan, &chan_len )
	    == FAILURE) {

		return;

	} else if( z_arrval_to_dbptr( db_array, &db ) < 0 ) {

		return;
	}

	tr = trloadchan( db, t0, t1, sta, chan );

	RETURN_DBPTR( tr );
}
/* }}} */


/* {{{ proto int dbput( array db [, string row] ) */
PHP_FUNCTION(dbput)
{
	zval	*db_array;
	Dbptr	db;
	char	*row = 0;
	int	row_len = 0;
	int	argc = ZEND_NUM_ARGS();
	int	rc;

	if( argc < 1 || argc > 2 ) {

		WRONG_PARAM_COUNT;

	} else if( argc == 2 ) {

		if( zend_parse_parameters( argc TSRMLS_CC, "as", 
					&db_array, &row, &row_len )
	    	== FAILURE) {

			return;
		}
	
	} else {

		if( zend_parse_parameters( argc TSRMLS_CC, "a", &db_array )
	    	== FAILURE) {

			return;
		}
	}

	if( z_arrval_to_dbptr( db_array, &db ) < 0 ) {

		return;
	}

	rc = dbput( db, 0 );

	RETURN_LONG( rc );
}
/* }}} */

/* {{{ proto int dbget( array db [, int scratch] ) */
PHP_FUNCTION(dbget)
{
	zval	*db_array;
	Dbptr	db;
	char	*item = 0;
	int	item_size = 0;
	int	argc = ZEND_NUM_ARGS();
	long	scratch = 0;
	int	rc;

	if( argc < 1 || argc > 2 ) {

		WRONG_PARAM_COUNT;

	} else if( argc == 2 ) {

		if( zend_parse_parameters( argc TSRMLS_CC, "al", 
					&db_array, &scratch )
	    	== FAILURE) {

			return;
		}

		/* It's the presence, not the value of the argument 
		   that counts here: */

		scratch = 1;

	} else { 	/* argc == 1 */
	
		if( zend_parse_parameters( argc TSRMLS_CC, "a", &db_array )
	    		== FAILURE) {

			return;
		}
	}

	if( z_arrval_to_dbptr( db_array, &db ) < 0 ) {

		return;
	}

	if( scratch ) {

		rc = dbget( db, 0 );

		RETURN_LONG( rc );

	} else {

		if( db.record == dbALL ) {

			dbquery( db, dbTABLE_SIZE, &item_size );

		} else {

			dbquery( db, dbRECORD_SIZE, &item_size );
		}

		item = (char *) emalloc( item_size * sizeof(char) );

		rc = dbget( db, item );

		if( rc < 0 ) {
			
			efree( item );

			RETURN_STRING( "", 1 );

		} else {

			RETURN_STRING( item, 0 );				
		}
	}
}
/* }}} */

/* {{{ proto int dbwrite_view( array db, string filename ) */
PHP_FUNCTION(dbwrite_view)
{
	zval	*db_array;
	Dbptr	db;
	char	*filename;
	int	filename_len;
	char	errmsg[STRSZ];
	int	argc = ZEND_NUM_ARGS();
	int	rc;
	FILE	*fpview;

	if( argc != 2 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "as", 
					&db_array, &filename, &filename_len )
	    == FAILURE) {

		return;

	} else if( z_arrval_to_dbptr( db_array, &db ) < 0 ) {

		return;
	}

	if( ( fpview = fopen( filename, "w" ) ) == NULL ) {
		
		sprintf( errmsg, "Failed to open file '%s'", filename );
		zend_error( E_ERROR, errmsg );
	}

	rc = dbwrite_view( db, fpview );

	RETURN_LONG( rc );
}
/* }}} */

/* {{{ proto int dbsave_view( array db ) */
PHP_FUNCTION(dbsave_view)
{
	zval	*db_array;
	Dbptr	db;
	int	argc = ZEND_NUM_ARGS();
	int	rc;

	if( argc != 1 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "a", &db_array )
	    == FAILURE) {

		return;

	} else if( z_arrval_to_dbptr( db_array, &db ) < 0 ) {

		return;
	}

	rc = dbsave_view( db );

	RETURN_LONG( rc );
}
/* }}} */

/* {{{ proto array dbread_view( string filename [, string viewname] ) */
PHP_FUNCTION(dbread_view)
{
	Dbptr	db;
	char	*filename;
	int	filename_len;
	char	*name = 0;
	int	name_len;
	char	errmsg[STRSZ];
	int	argc = ZEND_NUM_ARGS();
	int	rc;
	FILE	*fpview;

	if( argc < 1 || argc > 2 ) {

		WRONG_PARAM_COUNT;

	} else if( argc == 1 ) {

		if( zend_parse_parameters( argc TSRMLS_CC, "s", 
						&filename, &filename_len )
	    	== FAILURE) {

			return;
		}

	} else if( argc == 2 ) {

		if( zend_parse_parameters( argc TSRMLS_CC, "ss", 
						&filename, &filename_len,
						&name, &name_len )
	    	== FAILURE) {

			return;
		}
	}

	if( ( fpview = fopen( filename, "r" ) ) == NULL ) {
		
		sprintf( errmsg, "Failed to open file '%s'", filename );
		zend_error( E_ERROR, errmsg );
	}

	rc = dbread_view( fpview, &db, name );

	clear_register( 1 );

	RETURN_DBPTR( db );
}
/* }}} */

/* {{{ proto int dbdelete( array db ) */
PHP_FUNCTION(dbdelete)
{
	zval	*db_array;
	Dbptr	db;
	int	argc = ZEND_NUM_ARGS();
	int	rc;

	if( argc != 1 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "a", &db_array )
	    == FAILURE) {

		return;

	} else if( z_arrval_to_dbptr( db_array, &db ) < 0 ) {

		return;
	}

	rc = dbdelete( db );

	RETURN_LONG( rc );
}
/* }}} */

/* {{{ proto int dbmark( array db ) */
PHP_FUNCTION(dbmark)
{
	zval	*db_array;
	Dbptr	db;
	int	argc = ZEND_NUM_ARGS();
	int	rc;

	if( argc != 1 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "a", &db_array )
	    == FAILURE) {

		return;

	} else if( z_arrval_to_dbptr( db_array, &db ) < 0 ) {

		return;
	}

	rc = dbmark( db );

	RETURN_LONG( rc );
}
/* }}} */

/* {{{ proto int dbcrunch( array db ) */
PHP_FUNCTION(dbcrunch)
{
	zval	*db_array;
	Dbptr	db;
	int	argc = ZEND_NUM_ARGS();
	int	rc;

	if( argc != 1 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "a", &db_array )
	    == FAILURE) {

		return;

	} else if( z_arrval_to_dbptr( db_array, &db ) < 0 ) {

		return;
	}

	rc = dbcrunch( db );

	RETURN_LONG( rc );
}
/* }}} */

/* {{{ proto string db2xml( array db [, string flags [, string rootnode [, string rownode [, array fields [, array expressions]]]]]  ) */
PHP_FUNCTION(db2xml)
{
	zval	*db_array;
	zval	***args;
	Dbptr	db;
	Tbl	*fields = 0;
	Tbl	*expressions = 0;
	int	argc = ZEND_NUM_ARGS();
	char	*rootnode = 0;
	char	*rownode = 0;
	char	*flags_string = 0;
	int	flags = 0;
	char	*xml = 0;
	char	*xml_safe_copy = 0;
	int	rc = 0;

	if( argc < 1 || argc > 6 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( 1 TSRMLS_CC, "a", &db_array )
	    == FAILURE) {

		return;

	} else if( z_arrval_to_dbptr( db_array, &db ) < 0 ) {

		return;
	}

	if( argc >= 2 ) { 

		args = (zval ***) emalloc( argc * sizeof(zval **) );

		if( zend_get_parameters_array_ex( argc, args ) == FAILURE ) {

			efree( args );
			return;
		}

		if( Z_TYPE_PP( args[1] ) != IS_STRING ) {

			efree( args );
			zend_error( E_ERROR, "the flags argument must be a string\n" );
		} else {
			
			flags_string = Z_STRVAL_PP( args[1] );

			if( ! strcmp( flags_string, "DBXML_PRIMARY" ) ) {
				
				flags |= DBXML_PRIMARY;
			
			} else if( ! strcmp( flags_string, "" ) ) {

				;

			} else {

				efree( args );
				zend_error( E_ERROR, "flags must be empty or DBXML_PRIMARY\n" );
			}
		}
	}

	if( argc >= 3 ) {

		if( Z_TYPE_PP( args[2] ) != IS_STRING ) {

			efree( args );
			zend_error( E_ERROR, "rootnode argument must be a string\n" );

		} else {
			
			rootnode = Z_STRVAL_PP( args[2] );
		}
	}

	if( argc >= 4 ) {

		if( Z_TYPE_PP( args[3] ) != IS_STRING ) {

			efree( args );
			zend_error( E_ERROR, "rownode argument must be a string\n" );

		} else {
			
			rownode = Z_STRVAL_PP( args[3] );
		}
	}

	if( argc >= 5 ) {
	
		if( z_arrval_to_strtbl( *args[4], &fields ) < 0 ) {

			efree( args );
			zend_error( E_ERROR, "The fields argument must not be a non-empty list\n" );
		}
	}

	if( argc >= 6 ) {
	
		if( z_arrval_to_strtbl( *args[5], &expressions ) < 0 ) {

			efree( args );
			zend_error( E_ERROR, "The expressions argument must not be a non-empty list\n" );
		}
	}

	if( argc >= 2 ) {

		efree( args );
	}

	rc = db2xml( db, rootnode, rownode, fields, expressions,
		     (void **) &xml, flags );

	if( rc < 0 ) {

		zend_error( E_ERROR, "error calling db2xml\n" );

	} else {

		xml_safe_copy = estrdup( xml );		

		free( xml );

		RETURN_STRING( xml_safe_copy, 0 );
	}
}
/* }}} */

/* {{{ proto resource dbresponse( string filename ) */
PHP_FUNCTION(dbresponse)
{
	char	*filename;
	int	filename_len;
	int	argc = ZEND_NUM_ARGS();
	FILE	*fp;
	Response *response;

	if( argc != 1 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( 1 TSRMLS_CC, "s", 
					&filename, &filename_len )
	    == FAILURE) {

		return;
	}

	if( ( fp = fopen( filename, "r" ) ) == NULL ) {

		zend_error( E_ERROR, "Failed to open specified response file\n" );
	}

	if( read_response( fp , &response ) != 0 ) {

		fclose( fp );
		zend_error( E_ERROR, "failed to read specified response file\n" );
	}

	fclose( fp );

	ZEND_REGISTER_RESOURCE( return_value, response, le_dbresponse );
}
/* }}} */

/* {{{ proto array eval_response( resource response, double omega ) */
PHP_FUNCTION(eval_response)
{
	int	argc = ZEND_NUM_ARGS();
	Response *response;
	zval	*res;
	double	omega;
	double	real, imag;

	if( argc != 2 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( 2 TSRMLS_CC, "rd", &res, &omega ) == FAILURE) {

		return;
	}

	ZEND_FETCH_RESOURCE( response, Response *, &res, -1, 
			     "dbresponse", le_dbresponse );

	if( ! response ) {
		RETURN_FALSE;
	}

	eval_response( omega, response, &real, &imag );

	array_init( return_value );
	add_index_double( return_value, 0, real );
	add_index_double( return_value, 1, imag );

	return;
}
/* }}} */

/* {{{ proto array dbfind( array db, string expression, [, int first [, int reverse]] ) */
PHP_FUNCTION(dbfind)
{
	zval	*db_array;
	Dbptr	db;
	zval	***args;
	char	*expr;
	int	expr_len;
	int	argc = ZEND_NUM_ARGS();
	int	reverse = 0;
	int	result = 0;

	if( argc < 2 ) {

		WRONG_PARAM_COUNT;

	} else if( argc > 4 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( 2 TSRMLS_CC, "as", 
					&db_array,
					&expr, &expr_len )
	    == FAILURE) {

		return;

	} else if( z_arrval_to_dbptr( db_array, &db ) < 0 ) {

		return;
	}

	if( argc > 2 ) { 

		args = (zval ***) emalloc( argc * sizeof(zval **) );

		if( zend_get_parameters_array_ex( argc, args ) == FAILURE ) {

			efree( args );
			return;
		}

		if( Z_TYPE_PP( args[2] ) != IS_LONG ) {

			zend_error( E_ERROR, "the argument specifying the starting record number must be an integer\n" );

		} else {

			db.record = Z_LVAL_PP( args[2] );
		}

		if( argc == 4 ) {

			if( Z_TYPE_PP( args[3] ) != IS_LONG ) {

				zend_error( E_ERROR, "the 'reverse' argument must be an integer\n" );

			} else {

				reverse = Z_LVAL_PP( args[3] );
			}
		}

		efree( args );
	}

	result = dbfind( db, expr, reverse, 0 );

	RETURN_LONG( result );
}
/* }}} */

/* {{{ proto string strtdelta( double epoch ) */
PHP_FUNCTION(strtdelta)
{
	int	argc = ZEND_NUM_ARGS();
	double	epoch;
	char	*s;

	if( argc != 1 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "d", &epoch ) == FAILURE) {

		return;
	}

	s = strtdelta( epoch );

	RETVAL_STRING( s, 1 );

	free( s );
}
/* }}} */

/* {{{ proto string strtime( double epoch ) */
PHP_FUNCTION(strtime)
{
	int	argc = ZEND_NUM_ARGS();
	double	epoch;
	char	*s;

	if( argc != 1 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "d", &epoch ) == FAILURE) {

		return;
	}

	s = strtime( epoch );

	RETVAL_STRING( s, 1 );

	free( s );
}
/* }}} */

/* {{{ proto string strydtime( double epoch ) */
PHP_FUNCTION(strydtime)
{
	int	argc = ZEND_NUM_ARGS();
	double	epoch;
	char	*s;

	if( argc != 1 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "d", &epoch ) == FAILURE) {

		return;
	}

	s = strydtime( epoch );

	RETVAL_STRING( s, 1 );

	free( s );
}
/* }}} */

/* {{{ proto string strdate( double epoch ) */
PHP_FUNCTION(strdate)
{
	int	argc = ZEND_NUM_ARGS();
	double	epoch;
	char	*s;

	if( argc != 1 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "d", &epoch ) == FAILURE) {

		return;
	}

	s = strdate( epoch );

	RETVAL_STRING( s, 1 );

	free( s );
}
/* }}} */

/* {{{ proto string strlocaltime( double epoch ) */
PHP_FUNCTION(strlocaltime)
{
	int	argc = ZEND_NUM_ARGS();
	double	epoch;
	char	*s;

	if( argc != 1 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "d", &epoch ) == FAILURE) {

		return;
	}

	s = strlocaltime( epoch );

	RETVAL_STRING( s, 1 );

	free( s );
}
/* }}} */

/* {{{ proto string strlocalydtime( double epoch ) */
PHP_FUNCTION(strlocalydtime)
{
	int	argc = ZEND_NUM_ARGS();
	double	epoch;
	char	*s;

	if( argc != 1 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "d", &epoch ) == FAILURE) {

		return;
	}

	s = strlocalydtime( epoch );

	RETVAL_STRING( s, 1 );

	free( s );
}
/* }}} */

/* {{{ proto string strlocaldate( double epoch ) */
PHP_FUNCTION(strlocaldate)
{
	int	argc = ZEND_NUM_ARGS();
	double	epoch;
	char	*s;

	if( argc != 1 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "d", &epoch ) == FAILURE) {

		return;
	}

	s = strlocaldate( epoch );

	RETVAL_STRING( s, 1 );

	free( s );
}
/* }}} */

/* {{{ proto array dbquery( array db, string code ) */
PHP_FUNCTION(dbquery)
{
	zval	*db_array;
	Dbptr	db;
	int	argc = ZEND_NUM_ARGS();
	char	errmsg[STRSZ];
	char	*dbstring_code;
	int	dbstring_code_len;
	int	dbcode;
	Dbvalue value;
	int	retcode;
	int	i;

	if( argc != 2 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "as", 
					&db_array,
					&dbstring_code, &dbstring_code_len )
	    == FAILURE) {

		return;

	} else if( z_arrval_to_dbptr( db_array, &db ) < 0 ) {

		return;
	}

	dbcode = xlatname( dbstring_code, Dbxlat, NDbxlat );

	switch( dbcode ) {
        case dbSCHEMA_DESCRIPTION:
        case dbTIMEDATE_NAME:
        case dbDATABASE_DESCRIPTION:
        case dbTABLE_DESCRIPTION:
        case dbFIELD_DESCRIPTION:
        case dbSCHEMA_DETAIL:
        case dbDATABASE_DETAIL:
        case dbTABLE_DETAIL:
        case dbFIELD_DETAIL:
        case dbSCHEMA_NAME:
        case dbDATABASE_NAME:
        case dbTABLE_NAME:
        case dbFIELD_NAME:
        case dbTABLE_FILENAME:
        case dbTABLE_DIRNAME:
        case dbFIELD_RANGE:
        case dbFIELD_FORMAT:
        case dbDBPATH:
        case dbFORMAT:
        case dbFIELD_UNITS:
        case dbFIELD_BASE_TABLE:
        case dbUNIQUE_ID_NAME:
		if( ( retcode = dbquery(db, dbcode, &value) ) >= 0 ) {

			RETVAL_STRING( value.t, 1 );

		} else {

			zend_error( E_ERROR, "Error parsing response from dbquery\n" );
		}
		break;

        case dbDATABASE_COUNT:
        case dbTABLE_COUNT:
        case dbFIELD_COUNT:
        case dbRECORD_COUNT:
        case dbTABLE_SIZE:
        case dbFIELD_SIZE:
        case dbFIELD_INDEX:
        case dbVIEW_TABLE_COUNT:
        case dbRECORD_SIZE:
		if( ( retcode = dbquery(db, dbcode, &value) ) >= 0 ) {

			RETVAL_LONG( value.i );

		} else {

			zend_error( E_ERROR, "Error parsing response from dbquery\n" );
		}
                break;  

        case dbTABLE_IS_WRITEABLE:
        case dbTABLE_IS_VIEW:
        case dbTABLE_PRESENT:
		if( ( retcode = dbquery(db, dbcode, &value) ) >= 0 ) {

			RETVAL_BOOL( value.i );

		} else {

			zend_error( E_ERROR, "Error parsing response from dbquery\n" );
		}
                break;  

        case dbFIELD_TYPE:
		if( ( retcode = dbquery(db, dbcode, &value) ) >= 0 ) {

			RETVAL_STRING( xlatnum( value.i, Dbxlat, NDbxlat ), 1 );

		} else {

			zend_error( E_ERROR, "Error parsing response from dbquery\n" );
		}
                break;  
 
        case dbLINK_FIELDS:
        case dbSCHEMA_FIELDS:
	case dbSCHEMA_TABLES:
        case dbFIELD_TABLES:
        case dbVIEW_TABLES:
        case dbTABLE_FIELDS:
        case dbPRIMARY_KEY:
        case dbALTERNATE_KEY:
        case dbFOREIGN_KEYS:
		if( ( retcode = dbquery(db, dbcode, &value) ) >= 0 ) {

			array_init( return_value );

			for( i = 0; i < maxtbl( value.tbl ); i++ ) {

				add_next_index_string( return_value, gettbl( value.tbl, i ), 1 );
			}

		} else {

			zend_error( E_ERROR, "Error parsing response from dbquery\n" );
		}
                break;  
 
        default:
		sprintf( errmsg, "dbquery: bad code '%s'", dbcode );
		zend_error( E_ERROR, errmsg );
		break ;
	}
}
/* }}} */

/* {{{ proto array dbtruncate( array db, string expression ) */
PHP_FUNCTION(dbtruncate)
{
	zval	*db_array;
	Dbptr	db;
	int	argc = ZEND_NUM_ARGS();
	long	nrecords;
	int	rc;

	if( argc != 2 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "al", 
					&db_array, &nrecords )
	    == FAILURE) {

		return;

	} else if( z_arrval_to_dbptr( db_array, &db ) < 0 ) {

		return;
	}

	rc = dbtruncate( db, nrecords );

	RETURN_LONG( rc );
}
/* }}} */

/* {{{ proto array dbsubset( array db, string expression ) */
PHP_FUNCTION(dbsubset)
{
	zval	*db_array;
	Dbptr	db;
	int	argc = ZEND_NUM_ARGS();
	char	*expr;
	int	expr_len;
	int	retcode;
	int	i;

	if( argc != 2 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "as", 
					&db_array,
					&expr, &expr_len )
	    == FAILURE) {

		return;

	} else if( z_arrval_to_dbptr( db_array, &db ) < 0 ) {

		return;
	}

	db = dbsubset( db, expr, 0 );

	RETURN_DBPTR( db );
}
/* }}} */

/* {{{ proto array dbseparate( array db, string table ) */
PHP_FUNCTION(dbseparate)
{
	zval	*db_array;
	Dbptr	db;
	int	argc = ZEND_NUM_ARGS();
	char	*table;
	int	table_len;
	int	retcode;
	int	i;

	if( argc != 2 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "as", 
					&db_array,
					&table, &table_len )
	    == FAILURE) {

		return;

	} else if( z_arrval_to_dbptr( db_array, &db ) < 0 ) {

		return;
	}

	db = dbseparate( db, table );

	RETURN_DBPTR( db );
}
/* }}} */

/* {{{ proto array dbsever( array db, string table ) */
PHP_FUNCTION(dbsever)
{
	zval	*db_array;
	Dbptr	db;
	int	argc = ZEND_NUM_ARGS();
	char	*table;
	int	table_len;
	int	retcode;
	int	i;

	if( argc != 2 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "as", 
					&db_array,
					&table, &table_len )
	    == FAILURE) {

		return;

	} else if( z_arrval_to_dbptr( db_array, &db ) < 0 ) {

		return;
	}

	db = dbsever( db, table, 0 );

	RETURN_DBPTR( db );
}
/* }}} */

/* {{{ proto array dbunjoin( array db, string database ) */
PHP_FUNCTION(dbunjoin)
{
	zval	*db_array;
	Dbptr	db;
	int	argc = ZEND_NUM_ARGS();
	char	*database;
	int	database_len;
	int	retcode;
	int	i;

	if( argc != 2 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "as", 
					&db_array,
					&database, &database_len )
	    == FAILURE) {

		return;

	} else if( z_arrval_to_dbptr( db_array, &db ) < 0 ) {

		return;
	}

	retcode = dbunjoin( db, database, 1 );

	RETURN_LONG( retcode );
}
/* }}} */

/* {{{ proto mixed dbex_eval( array db, string expression ) */
PHP_FUNCTION(dbex_eval)
{
	zval	*db_array;
	Dbptr	db;
	int	type;
	int	argc = ZEND_NUM_ARGS();
	char	*expr;
	int	expr_len;
	Dbvalue	value;
	char	warning[STRSZ];

	if( argc != 2 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "as", 
					&db_array,
					&expr, &expr_len )
	    == FAILURE) {

		return;

	} else if( z_arrval_to_dbptr( db_array, &db ) < 0 ) {

		return;
	}

	type = dbex_evalstr( db, expr, 0, &value );

	switch( type ) {
	case dbDBPTR:
		RETVAL_DBPTR( value.db );
		break;
	case dbSTRING:
		copystrip( value.t, value.t, strlen( value.t ) );
		RETVAL_STRING( value.t, 1 );
		free( value.t );
		break;
	case dbBOOLEAN:
		RETVAL_BOOL( value.i );
		break;
	case dbINTEGER:
	case dbYEARDAY:
		RETVAL_LONG( value.i );
		break;
	case dbREAL:
	case dbTIME:
		RETVAL_DOUBLE( value.d );
		break;
	default:
		sprintf( warning, 
			"Can't interpret field of type %s",
			xlatnum( type, Dbxlat, NDbxlat ) );
		zend_error( E_WARNING, warning );
		break;
	}
}
/* }}} */

/* {{{ proto array dbnojoin( array db1, array db2 [, string key, ...] ) */
PHP_FUNCTION(dbnojoin)
{
	int	argc = ZEND_NUM_ARGS();
	zval	***args;
	Dbptr	db1;
	Dbptr	db2;
	Dbptr	db;
	char	*key;
	Tbl	*pattern1 = 0;
	Tbl	*pattern2 = 0;
	Tbl	*halves;
	int	i;

	if( argc < 2 ) {

		WRONG_PARAM_COUNT;
	}

	args = (zval ***) emalloc( argc * sizeof(zval **) );

	if( zend_get_parameters_array_ex( argc, args ) == FAILURE ) {

		efree( args );
		return;
	}

	if( Z_TYPE_PP( args[0] ) != IS_ARRAY ) {

		efree( args );
		zend_error( E_ERROR, "Error reading first dbpointer\n" );

	} else if( z_arrval_to_dbptr( *args[0], &db1 ) < 0 ) {

		efree( args );
		zend_error( E_ERROR, "Error reading first dbpointer\n" );
		return;
	}	

	if( Z_TYPE_PP( args[1] ) != IS_ARRAY ) {

		efree( args );
		zend_error( E_ERROR, "Error reading second dbpointer\n" );

	} else if( z_arrval_to_dbptr( *args[1], &db2 ) < 0 ) {

		efree( args );
		zend_error( E_ERROR, "Error reading second dbpointer\n" );
		return;
	}	

	for( i = 2; i < argc; i++ ) {

		if( Z_TYPE_PP( args[i] ) != IS_STRING ) {

			efree( args );
			zend_error( E_ERROR, "dbnojoin join-keys must be string values\n" );
			return;
		}

		key = Z_STRVAL_PP( args[i] );

		if( pattern1 == (Tbl *) NULL ) {

			pattern1 = newtbl( 0 );
		}

		if( pattern2 == (Tbl *) NULL ) {

			pattern2 = newtbl( 0 );
		}

		if( strchr( key, '#' ) == 0 ) {

			pushtbl( pattern1, strdup( key ) );
			pushtbl( pattern2, strdup( key ) );

		} else {
			
			key = strdup( key );

			halves = split( key, '#' );

			pushtbl( pattern1, strdup( shifttbl( halves ) ) );
			pushtbl( pattern2, strdup( poptbl( halves ) ) );

			freetbl( halves, 0 );

			free( key );
		}
	}

	db = dbnojoin( db1, db2, &pattern1, &pattern2, 0 );

	if( pattern1 != (Tbl *) NULL ) {
	
		freetbl( pattern1, free );
	}

	if( pattern2 != (Tbl *) NULL ) {
	
		freetbl( pattern2, free );
	}

	efree( args );

	RETURN_DBPTR( db );
}
/* }}} */

/* {{{ proto array dbsort( array db [, string key, ...] ) */
PHP_FUNCTION(dbsort)
{
	int	argc = ZEND_NUM_ARGS();
	zval	***args;
	Dbptr	db;
	char	*key;
	Tbl	*sortfields = 0;
	int	flags = 0;
	int	i;
	
	if( argc < 1 ) {

		WRONG_PARAM_COUNT;
	}

	args = (zval ***) emalloc( argc * sizeof(zval **) );

	if( zend_get_parameters_array_ex( argc, args ) == FAILURE ) {

		efree( args );
		return;
	}

	if( Z_TYPE_PP( args[0] ) != IS_ARRAY ) {

		efree( args );

		zend_error( E_ERROR, "Error reading dbpointer\n" );

	} else if( z_arrval_to_dbptr( *args[0], &db ) < 0 ) {

		efree( args );

		zend_error( E_ERROR, "Error reading bpointer\n" );

		return;
	}	

	for( i = 1; i < argc; i++ ) {

		if( Z_TYPE_PP( args[i] ) != IS_STRING ) {

			efree( args );

			zend_error( E_ERROR, "dbsort keys must be string values\n" );
			return;
		}

		key = Z_STRVAL_PP( args[i] );

		if( strcmp( key, "-u" ) == 0 ) {

			flags |= dbSORT_UNIQUE;
			continue;
		}

		if( strcmp( key, "-r" ) == 0 ) {

			flags |= dbSORT_REVERSE;
			continue;
		}

		if( sortfields == (Tbl *) NULL ) {

			sortfields = newtbl( 0 );
		}

		pushtbl( sortfields, key );
	}

	db = dbsort( db, sortfields, flags, 0 );

	freetbl( sortfields, 0 );

	efree( args );

	RETURN_DBPTR( db );
}

/* {{{ proto array dbgroup( array db, string key [, string key, ...] ) */
PHP_FUNCTION(dbgroup)
{
	int	argc = ZEND_NUM_ARGS();
	zval	***args;
	Dbptr	db;
	char	*key;
	Tbl	*groupfields = 0;
	int	i;
	
	if( argc < 2 ) {

		WRONG_PARAM_COUNT;
	}

	args = (zval ***) emalloc( argc * sizeof(zval **) );

	if( zend_get_parameters_array_ex( argc, args ) == FAILURE ) {

		efree( args );
		return;
	}

	if( Z_TYPE_PP( args[0] ) != IS_ARRAY ) {

		efree( args );
		zend_error( E_ERROR, "Error reading dbpointer\n" );

	} else if( z_arrval_to_dbptr( *args[0], &db ) < 0 ) {

		efree( args );
		zend_error( E_ERROR, "Error reading bpointer\n" );
		return;
	}	

	if( Z_TYPE_PP( args[1] ) != IS_STRING ) {

		efree( args );
		zend_error( E_ERROR, "dbgroup keys must be string values\n" );
		return;
	}

	key = Z_STRVAL_PP( args[1] );

	groupfields = strtbl( key, 0 );

	for( i = 2; i < argc; i++ ) {

		if( Z_TYPE_PP( args[i] ) != IS_STRING ) {

			efree( args );
			zend_error( E_ERROR, "dbgroup keys must be string values\n" );
			return;
		}

		key = Z_STRVAL_PP( args[i] );

		pushtbl( groupfields, key );
	}

	db = dbgroup( db, groupfields, 0, 1 );

	freetbl( groupfields, 0 );

	efree( args );

	RETURN_DBPTR( db );
}
/* }}} */

/* {{{ proto array dbjoin( array db1, array db2 [, string key, ...] ) */
PHP_FUNCTION(dbjoin)
{
	int	argc = ZEND_NUM_ARGS();
	zval	***args;
	Dbptr	db1;
	Dbptr	db2;
	Dbptr	db;
	char	*key;
	Tbl	*pattern1 = 0;
	Tbl	*pattern2 = 0;
	Tbl	*halves;
	int	outer = 0;
	int	i;

	if( argc < 2 ) {

		WRONG_PARAM_COUNT;
	}

	args = (zval ***) emalloc( argc * sizeof(zval **) );

	if( zend_get_parameters_array_ex( argc, args ) == FAILURE ) {

		efree( args );
		return;
	}

	if( Z_TYPE_PP( args[0] ) != IS_ARRAY ) {

		efree( args );
		zend_error( E_ERROR, "Error reading first dbpointer\n" );

	} else if( z_arrval_to_dbptr( *args[0], &db1 ) < 0 ) {

		efree( args );
		zend_error( E_ERROR, "Error reading first dbpointer\n" );
		return;
	}	

	if( Z_TYPE_PP( args[1] ) != IS_ARRAY ) {

		efree( args );
		zend_error( E_ERROR, "Error reading second dbpointer\n" );

	} else if( z_arrval_to_dbptr( *args[1], &db2 ) < 0 ) {

		efree( args );
		zend_error( E_ERROR, "Error reading second dbpointer\n" );
		return;
	}	

	for( i = 2; i < argc; i++ ) {

		if( Z_TYPE_PP( args[i] ) != IS_STRING ) {

			efree( args );
			zend_error( E_ERROR, "dbjoin join-keys must be string values\n" );
			return;
		}

		key = Z_STRVAL_PP( args[i] );

		if( strcmp( key, "-outer" ) == 0 ) {

			outer = 1;
			continue;
		}

		if( pattern1 == (Tbl *) NULL ) {

			pattern1 = newtbl( 0 );
		}

		if( pattern2 == (Tbl *) NULL ) {

			pattern2 = newtbl( 0 );
		}

		if( strchr( key, '#' ) == 0 ) {

			pushtbl( pattern1, strdup( key ) );
			pushtbl( pattern2, strdup( key ) );

		} else {
			
			key = strdup( key );

			halves = split( key, '#' );

			pushtbl( pattern1, strdup( shifttbl( halves ) ) );
			pushtbl( pattern2, strdup( poptbl( halves ) ) );

			freetbl( halves, 0 );

			free( key );
		}
	}

	db = dbjoin( db1, db2, &pattern1, &pattern2, outer, 0, 0 );

	if( pattern1 != (Tbl *) NULL ) {
	
		freetbl( pattern1, free );
	}

	if( pattern2 != (Tbl *) NULL ) {
	
		freetbl( pattern2, free );
	}

	efree( args );

	RETURN_DBPTR( db );
}
/* }}} */

/* {{{ proto array dbtheta( array db1, array db2 [, string expression] ) */
PHP_FUNCTION(dbtheta)
{
	int	argc = ZEND_NUM_ARGS();
	zval	***args;
	Dbptr	db1;
	Dbptr	db2;
	Dbptr	db;
	char	*expression = 0;

	if( argc < 2 || argc > 3 ) {

		WRONG_PARAM_COUNT;
	}

	args = (zval ***) emalloc( argc * sizeof(zval **) );

	if( zend_get_parameters_array_ex( argc, args ) == FAILURE ) {

		efree( args );
		return;
	}

	if( Z_TYPE_PP( args[0] ) != IS_ARRAY ) {

		efree( args );
		zend_error( E_ERROR, "Error reading first dbpointer\n" );

	} else if( z_arrval_to_dbptr( *args[0], &db1 ) < 0 ) {

		efree( args );
		zend_error( E_ERROR, "Error reading first dbpointer\n" );
		return;
	}	

	if( Z_TYPE_PP( args[1] ) != IS_ARRAY ) {

		efree( args );
		zend_error( E_ERROR, "Error reading second dbpointer\n" );

	} else if( z_arrval_to_dbptr( *args[1], &db2 ) < 0 ) {

		efree( args );
		zend_error( E_ERROR, "Error reading second dbpointer\n" );
		return;
	}	

	if( argc == 3 ) {

		if( Z_TYPE_PP( args[2] ) != IS_STRING ) {

			efree( args );
			zend_error( E_ERROR, "dbtheta expression must be a string value\n" );
			return;
		}

		expression = Z_STRVAL_PP( args[2] );
	}

	db = dbtheta( db1, db2, expression, 0, 0 );

	efree( args );

	RETURN_DBPTR( db );
}
/* }}} */

/* {{{ proto array dbprocess( array db, string cmd [, string cmd, ...] ) */
PHP_FUNCTION(dbprocess)
{
	zval	*db_array;
	Dbptr	db;
	int	argc = ZEND_NUM_ARGS();
	zval	***args;
	Tbl	*cmdlist = 0;
	int	i;

	if( argc < 2 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( 1 TSRMLS_CC, "a", &db_array ) == FAILURE) {

		return;

	} else if( z_arrval_to_dbptr( db_array, &db ) < 0 ) {

		return;
	}

	args = (zval ***) emalloc( argc * sizeof(zval **) );

	if( zend_get_parameters_array_ex( argc, args ) == FAILURE ) {

		efree( args );
		return;
	}

	cmdlist = newtbl( argc - 1 );

	for( i = 1; i < argc; i++ ) {

		if( Z_TYPE_PP( args[i] ) != IS_STRING ) {

			efree( args );
			zend_error( E_ERROR, "Error reading command list\n" );

		} else {

			pushtbl( cmdlist, Z_STRVAL_PP( args[i] ) );
		}
	}

	db = dbprocess( db, cmdlist, 0 );
	
	if( cmdlist ) { 

		freetbl( cmdlist, 0 );
	}

	efree( args );

	RETURN_DBPTR( db );
}
/* }}} */

/* {{{ proto array dbfree( array db ) */
PHP_FUNCTION(dbfree)
{
	zval	*db_array;
	Dbptr	db;
	int	argc = ZEND_NUM_ARGS();
	int	rc;

	if( argc != 1 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "a", &db_array ) == FAILURE) {

		return;

	} else if( z_arrval_to_dbptr( db_array, &db ) < 0 ) {

		return;
	}

	rc = dbfree( db );

	RETURN_LONG( rc );
}
/* }}} */

/* {{{ proto array ds_dbclose( array db ) */
PHP_FUNCTION(ds_dbclose)
{
	zval	*db_array;
	Dbptr	db;
	int	argc = ZEND_NUM_ARGS();
	int	rc;

	if( argc != 1 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "a", &db_array ) == FAILURE) {

		return;

	} else if( z_arrval_to_dbptr( db_array, &db ) < 0 ) {

		return;
	}

	rc = dbclose( db );

	RETURN_LONG( rc );
}
/* }}} */

/* {{{ proto array dbdestroy( array db ) */
PHP_FUNCTION(dbdestroy)
{
	zval	*db_array;
	Dbptr	db;
	int	argc = ZEND_NUM_ARGS();
	int	rc;

	if( argc != 1 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "a", &db_array ) == FAILURE) {

		return;

	} else if( z_arrval_to_dbptr( db_array, &db ) < 0 ) {

		return;
	}

	rc = dbdestroy( db );

	RETURN_LONG( rc );
}
/* }}} */

/* {{{ proto array dbaddnull( array db ) */
PHP_FUNCTION(dbaddnull)
{
	zval	*db_array;
	Dbptr	db;
	int	argc = ZEND_NUM_ARGS();
	int	rc;

	if( argc != 1 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "a", &db_array ) == FAILURE) {

		return;

	} else if( z_arrval_to_dbptr( db_array, &db ) < 0 ) {

		return;
	}

	rc = dbaddnull( db );

	RETURN_LONG( rc );
}
/* }}} */

/* {{{ proto array dbungroup( array db ) */
PHP_FUNCTION(dbungroup)
{
	zval	*db_array;
	Dbptr	db;
	int	argc = ZEND_NUM_ARGS();
	int	nrecs;

	if( argc != 1 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "a", &db_array ) == FAILURE) {

		return;

	} else if( z_arrval_to_dbptr( db_array, &db ) < 0 ) {

		return;
	}

	db = dbungroup( db, 0 );

	RETURN_DBPTR( db );
}
/* }}} */

/* {{{ proto array dbnrecs( array db ) */
PHP_FUNCTION(dbnrecs)
{
	zval	*db_array;
	Dbptr	db;
	int	argc = ZEND_NUM_ARGS();
	int	nrecs;

	if( argc != 1 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "a", &db_array ) == FAILURE) {

		return;

	} else if( z_arrval_to_dbptr( db_array, &db ) < 0 ) {

		return;
	}

	dbquery( db, dbRECORD_COUNT, &nrecs );

	RETURN_LONG( nrecs );
}
/* }}} */


/* {{{ proto mixed dbputv( array db, string field_name, mixed value, [, field_name, value, ... ] ) */
PHP_FUNCTION(dbputv)
{
	zval	*db_array_in;
	zval	*db_array;
	Dbptr	db;
	zval	***args;
	int	type;
	Dbvalue	value;
	int	argc = ZEND_NUM_ARGS();
	int	i;
	int	rc;
	int	nfields;
	int	fieldname_index;
	int	fieldval_index;
	char	*field_name;
	int	retcode = 0;

	if( argc < 3 ) {

		WRONG_PARAM_COUNT;

	} else if( ( argc - 1 ) % 2 != 0 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( 1 TSRMLS_CC, "a", &db_array_in ) == FAILURE ) {

		zend_error( E_ERROR, "Error parsing dbptr\n" );

	} else if( z_arrval_to_dbptr( db_array_in, &db ) < 0 ) {

		zend_error( E_ERROR, "Error interpreting dbptr\n" );
	}

	args = (zval ***) emalloc( argc * sizeof(zval **) );

	if( zend_get_parameters_array_ex( argc, args ) == FAILURE ) {

		efree( args );
		zend_error( E_ERROR, "Error getting params array\n" );
	}

	nfields = ( argc - 1 ) / 2;

	for( i = 0; i < nfields; i++ ) {

		fieldname_index = i * 2 + 1;
		fieldval_index = fieldname_index + 1;

		if( Z_TYPE_PP( args[fieldname_index] ) != IS_STRING ) {
			zend_error( E_ERROR, "Error getting fieldname\n" );
		}

		field_name = Z_STRVAL_PP( args[fieldname_index] );

		db = dblookup( db, 0, 0, field_name, 0 );

		rc = dbquery( db, dbFIELD_TYPE, &type );

		if( rc == dbINVALID ) {
			
			zend_error( E_ERROR, "Error getting fieldtype\n" );
		}

		if( zval_to_dbvalue( args[fieldval_index], type, &value ) < 0 ) {

			zend_error( E_ERROR, "Error getting fieldvalue\n" );
		}

		switch( type ) {
		case dbDBPTR:
			retcode |= dbputv( db, 0, field_name, value.db, 0 );
			break;
		case dbSTRING:
			retcode |= dbputv( db, 0, field_name, value.s, 0 );
			break;
		case dbBOOLEAN:
		case dbINTEGER:
		case dbYEARDAY:
			retcode |= dbputv( db, 0, field_name, value.i, 0 );
			break;
		case dbREAL:
		case dbTIME:
			retcode |= dbputv( db, 0, field_name, value.d, 0 );
			break;
		default:
			retcode = dbINVALID;
			break;
		}
	}

	if( retcode != 0 ) {

		zend_error( E_ERROR, "Error in dbputv call\n" );
	}

	efree( args );

	RETURN_LONG( retcode );
}
/* }}} */

/* {{{ proto mixed dbaddv( array db, string field_name, mixed value, [, field_name, value, ... ] ) */
PHP_FUNCTION(dbaddv)
{
	zval	*db_array_in;
	zval	*db_array;
	Dbptr	db;
	zval	***args;
	int	type;
	Dbvalue	value;
	int	argc = ZEND_NUM_ARGS();
	int	i;
	int	rc;
	int	nfields;
	int	fieldname_index;
	int	fieldval_index;
	char	*field_name;
	int	retcode = 0;

	if( argc < 3 ) {

		WRONG_PARAM_COUNT;

	} else if( ( argc - 1 ) % 2 != 0 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( 1 TSRMLS_CC, "a", &db_array_in ) == FAILURE ) {

		zend_error( E_ERROR, "Error parsing dbptr\n" );

	} else if( z_arrval_to_dbptr( db_array_in, &db ) < 0 ) {

		zend_error( E_ERROR, "Error interpreting dbptr\n" );
	}

	db.record = dbNULL;

	rc = dbget( db, 0 );

	if( rc == dbINVALID ) {
		
		zend_error( E_ERROR, "Error getting null record\n" );
	}

	db.record = dbSCRATCH;

	args = (zval ***) emalloc( argc * sizeof(zval **) );

	if( zend_get_parameters_array_ex( argc, args ) == FAILURE ) {

		efree( args );
		zend_error( E_ERROR, "Error getting params array\n" );
	}

	nfields = ( argc - 1 ) / 2;

	for( i = 0; i < nfields; i++ ) {

		fieldname_index = i * 2 + 1;
		fieldval_index = fieldname_index + 1;

		if( Z_TYPE_PP( args[fieldname_index] ) != IS_STRING ) {
			zend_error( E_ERROR, "Error getting fieldname\n" );
		}

		field_name = Z_STRVAL_PP( args[fieldname_index] );

		db = dblookup( db, 0, 0, field_name, 0 );

		rc = dbquery( db, dbFIELD_TYPE, &type );

		if( rc == dbINVALID ) {
			
			zend_error( E_ERROR, "Error getting fieldtype\n" );
		}

		if( zval_to_dbvalue( args[fieldval_index], type, &value ) < 0 ) {

			zend_error( E_ERROR, "Error getting fieldvalue\n" );
		}

		switch( type ) {
		case dbDBPTR:
			retcode |= dbputv( db, 0, field_name, value.db, 0 );
			break;
		case dbSTRING:
			retcode |= dbputv( db, 0, field_name, value.s, 0 );
			break;
		case dbBOOLEAN:
		case dbINTEGER:
		case dbYEARDAY:
			retcode |= dbputv( db, 0, field_name, value.i, 0 );
			break;
		case dbREAL:
		case dbTIME:
			retcode |= dbputv( db, 0, field_name, value.d, 0 );
			break;
		default:
			retcode = -1;
			break;
		}
	}

	if( retcode != 0 ) {
		zend_error( E_ERROR, "Error in dbaddv call\n" );
	}

	retcode = dbaddchk( db, 0 );

	if( retcode < 0 ) {
		clear_register( 1 );
		zend_error( E_ERROR, "Error with addchk in dbaddv\n" );
	}

	efree( args );

	RETURN_LONG( retcode );
}
/* }}} */

/* {{{ proto mixed dbgetv( array db, string field_name [, field_name, ... ] ) */
PHP_FUNCTION(dbgetv)
{
	zval	*db_array_in;
	zval	*db_array;
	Dbptr	db;
	zval	***args;
	int	type;
	Dbvalue	value;
	int	argc = ZEND_NUM_ARGS();
	int	single = 0;
	int	i;
	char	warning[STRSZ];

	if( argc < 2 ) {

		WRONG_PARAM_COUNT;

	} else if( argc == 2 ) {

		single = 1;

	} else {

		single = 0;
	}

	if( zend_parse_parameters( 1 TSRMLS_CC, "a", &db_array_in ) == FAILURE ) {

		return;

	} else if( z_arrval_to_dbptr( db_array_in, &db ) < 0 ) {

		return;
	}

	args = (zval ***) emalloc( argc * sizeof(zval **) );

	if( zend_get_parameters_array_ex( argc, args ) == FAILURE ) {

		efree( args );
		return;
	}

	if( ! single ) {

		array_init( return_value );
	}

	for( i = 1; i < argc; i++ ) {

		if( Z_TYPE_PP( args[i] ) != IS_STRING ) {
			if( ! single ) {
				efree( return_value );
			}
			zend_error( E_ERROR, "fieldnames must be strings\n" );
		}

		db = dblookup( db, 0, 0, Z_STRVAL_PP( args[i] ), 0 );

		dbquery( db, dbFIELD_TYPE, &type );

		if( dbget( db, value.s ) < 0 )
		{
			if( ! single ) {
				efree( return_value );
			}
			zend_error( E_ERROR, "failed to retrieve value\n" );
		}

		switch( type )
		{
		case dbDBPTR:
			if( single ) {
				RETVAL_DBPTR( value.db );
			} else {
				MAKE_DBPTR_ZVAL( db_array, value.db );
				add_next_index_zval( return_value, db_array );
			}
			break;
		case dbSTRING:
			copystrip( value.s, value.s, strlen( value.s ) );
			if( single ) {
				RETVAL_STRING( value.s, 1 );
			} else {
				add_next_index_string( return_value, value.s, 1 );
			}
			break;
		case dbBOOLEAN:
			copystrip( value.s, value.s, strlen( value.s ) );
			if( single ) {
				RETVAL_BOOL( atoi( value.s ) );
			} else {
				add_next_index_bool( return_value, atoi( value.s ) );
			}
			break;
		case dbINTEGER:
		case dbYEARDAY:
			copystrip( value.s, value.s, strlen( value.s ) );
			if( single ) {
				RETVAL_LONG( atoi( value.s ) );
			} else {
				add_next_index_long( return_value, atoi( value.s ) );
			}
			break;
		case dbREAL:
		case dbTIME:
			copystrip( value.s, value.s, strlen( value.s ) );
			if( single ) {
				RETVAL_DOUBLE( (double) atof( value.s ) );
			} else {
				add_next_index_double( return_value, (double) atof( value.s ) );
			}
			break;
		default:
			sprintf( warning, 
				"Can't interpret field of type %s",
				xlatnum( type, Dbxlat, NDbxlat ) );
			zend_error( E_WARNING, warning );
			break;
		}
	}

	efree( args );
}
/* }}} */

/* {{{ proto mixed dbadd( array db [, string record ] ) */
PHP_FUNCTION(dbadd)
{
	zval	*db_array_in;
	Dbptr	db;
	zval	***args = 0;
	char	*record = 0;
	int	argc = ZEND_NUM_ARGS();
	int	rc;

	if( argc < 1 || argc > 2 ) {

		WRONG_PARAM_COUNT;
	} 
	
	if( zend_parse_parameters( 1 TSRMLS_CC, "a", &db_array_in ) == FAILURE ) {

		return;

	} else if( z_arrval_to_dbptr( db_array_in, &db ) < 0 ) {

		return;
	}

	if( argc == 2 ) {

		args = (zval ***) emalloc( argc * sizeof(zval **) );

		if( zend_get_parameters_array_ex( argc, args ) == FAILURE ) {

			efree( args );
			return;

		} else if( Z_TYPE_PP( args[1] ) != IS_STRING ) {

			efree( args );
			zend_error( E_ERROR, "record must be a string\n" );

		} else {

			record = Z_STRVAL_PP( args[1] );
		}
	}

	rc = dbadd( db, record );

	if( args ) { efree( args ); }

	RETURN_LONG( rc );
}
/* }}} */

/* {{{ proto mixed dbextfile( array db [, string tablename ] ) */
PHP_FUNCTION(dbextfile)
{
	zval	*db_array_in;
	Dbptr	db;
	zval	***args = 0;
	char	*tablename = 0;
	char	filename[FILENAME_MAX];
	int	argc = ZEND_NUM_ARGS();

	if( argc < 1 || argc > 2 ) {

		WRONG_PARAM_COUNT;
	} 
	
	if( zend_parse_parameters( 1 TSRMLS_CC, "a", &db_array_in ) == FAILURE ) {

		return;

	} else if( z_arrval_to_dbptr( db_array_in, &db ) < 0 ) {

		return;
	}

	if( argc == 2 ) {

		args = (zval ***) emalloc( argc * sizeof(zval **) );

		if( zend_get_parameters_array_ex( argc, args ) == FAILURE ) {

			efree( args );
			return;

		} else if( Z_TYPE_PP( args[1] ) != IS_STRING ) {

			efree( args );
			zend_error( E_ERROR, "tablename must be a string\n" );

		} else {

			tablename = Z_STRVAL_PP( args[1] );
		}
	}

	dbextfile( db, tablename, filename );

	if( args ) { efree( args ); }

	RETURN_STRING( filename, 1 )
}
/* }}} */

/* {{{ proto array dblookup( array db, string database, string table, string field, string record ) */
PHP_FUNCTION(dblookup)
{
	zval	*db_array;
	Dbptr	db;
	char	*database = NULL;
	char	*table = NULL;
	char	*field = NULL;
	char	*record = NULL;
	int	database_len;
	int	table_len;
	int	field_len;
	int	record_len;
	int	argc = ZEND_NUM_ARGS();

	if( argc != 5 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "assss", 
					&db_array,
					&database, &database_len,
					&table, &table_len,
					&field, &field_len,
					&record, &record_len )
	    == FAILURE) {

		return;

	} else if( z_arrval_to_dbptr( db_array, &db ) < 0 ) {

		return;
	}

	db = dblookup( db, database, table, field, record );

	RETURN_DBPTR( db );
}
/* }}} */

/* {{{ proto array ds_dbopen( string dbname, string permissions ) */
PHP_FUNCTION(ds_dbopen)
{
	Dbptr	db;
	char	*dbname = NULL;
	int	dbname_len;
	char	*perm = NULL;
	int	perm_len;
	int	argc = ZEND_NUM_ARGS();
	
	if( argc != 2 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "ss", 
					&dbname, &dbname_len, 
					&perm, &perm_len )
	    == FAILURE) {

		return;
	}

	dbopen( dbname, perm, &db ); 

	RETURN_DBPTR( db );
}
/* }}} */

/* {{{ proto array ds_dbopen_database( string dbname, string permissions ) */
PHP_FUNCTION(ds_dbopen_database)
{
	Dbptr	db;
	char	*dbname = NULL;
	int	dbname_len;
	char	*perm = NULL;
	int	perm_len;
	int	argc = ZEND_NUM_ARGS();
	
	if( argc != 2 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "ss", 
					&dbname, &dbname_len, 
					&perm, &perm_len )
	    == FAILURE) {

		return;
	}

	dbopen_database( dbname, perm, &db ); 

	RETURN_DBPTR( db );
}
/* }}} */

/* {{{ proto array ds_dbopen_table( string dbname, string permissions ) */
PHP_FUNCTION(ds_dbopen_table)
{
	Dbptr	db;
	char	*dbname = NULL;
	int	dbname_len;
	char	*perm = NULL;
	int	perm_len;
	int	argc = ZEND_NUM_ARGS();
	
	if( argc != 2 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "ss", 
					&dbname, &dbname_len, 
					&perm, &perm_len )
	    == FAILURE) {

		return;
	}

	dbopen_table( dbname, perm, &db ); 

	RETURN_DBPTR( db );
}
/* }}} */

/* {{{ proto array ds_dbtmp( string schema ) */
PHP_FUNCTION(ds_dbtmp)
{
	Dbptr	db;
	char	*schema = NULL;
	int	schema_len;
	int	argc = ZEND_NUM_ARGS();
	
	if( argc != 1 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "s", 
				   &schema, &schema_len )
	    == FAILURE) {

		return;
	}

	db = dbtmp( schema );

	RETURN_DBPTR( db );
}
/* }}} */

/* {{{ proto int ds_dbcreate( string schema ) */
PHP_FUNCTION(ds_dbcreate)
{
	Dbptr	db;
	char	*filename = NULL;
	int	filename_len;
	char	*schema = NULL;
	int	schema_len;
	char	*dbpath = NULL;
	int	dbpath_len;
	char	*description = NULL;
	int	description_len;
	char	*detail = NULL;
	int	detail_len;
	int	argc = ZEND_NUM_ARGS();
	int	rc;
	
	if( argc < 1 || argc > 5 ) {

		WRONG_PARAM_COUNT;
	}

	if( argc == 1 &&
		zend_parse_parameters( argc TSRMLS_CC, "s", 
				   &filename, &filename_len )
	    == FAILURE) {

		return;

	} else if( argc == 2 && 
		zend_parse_parameters( argc TSRMLS_CC, "ss", 
				   &filename, &filename_len,
				   &schema, &schema_len )
	    == FAILURE) {

		return;

	} else if( argc == 3 && 
		zend_parse_parameters( argc TSRMLS_CC, "sss", 
				   &filename, &filename_len,
				   &schema, &schema_len,
				   &dbpath, &dbpath_len )
	    == FAILURE) {

		return;

	} else if( argc == 4 && 
		zend_parse_parameters( argc TSRMLS_CC, "ssss", 
				   &filename, &filename_len,
				   &schema, &schema_len,
				   &dbpath, &dbpath_len,
				   &description, &description_len )
	    == FAILURE) {

		return;

	} else if( argc == 5 && 
		zend_parse_parameters( argc TSRMLS_CC, "sssss", 
				   &filename, &filename_len,
				   &schema, &schema_len,
				   &dbpath, &dbpath_len,
				   &description, &description_len,
				   &detail, &detail_len )
	    == FAILURE) {

		return;
	}

	rc = dbcreate( filename, schema, dbpath, description, detail );

	RETURN_LONG( rc );
}
/* }}} */

/* local variables
 * End:
 * vim600: fdm=marker
 */
