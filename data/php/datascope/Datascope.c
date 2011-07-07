/*
 *   Antelope interface for PHP
 *
 *   Copyright (c) 2005-2006 Lindquist Consulting, Inc.
 *   All rights reserved.
 *
 *   Written by Dr. Kent Lindquist, Lindquist Consulting, Inc.
 *
 *   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY
 *   KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
 *   WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
 *   PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS
 *   OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR
 *   OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
 *   OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 *   SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 *   This software may be used freely in any way as long as
 *   the copyright statement above is not removed.
 */

#include "php.h"
#include "php_ini.h"
#include "ext/standard/info.h"
#include "Datascope_php.h"
#include "db.h"
#include "tr.h"
#include "stock.h"
#include "coords.h"
#include "response.h"
#include "pf.h"
#include "dbxml.h"
#include "pfxml.h"

/* Prevent the deviants.h reassignment to std_now() from corrupting
 * the name of the PHP function */
#undef now

static int le_dbresponse;

static Arr *Hooks = 0;

static char *Elog_replacement = 0;

static function_entry Datascope_functions[] = {
	PHP_FE(dbex_eval, NULL)		
	PHP_FE(dbextfile, NULL)		
	PHP_FE(dbfind, NULL)		
	PHP_FE(dbmatches, NULL)		
	PHP_FE(dbgetv, NULL)		
	PHP_FE(dbaddv, NULL)		
	PHP_FE(dbputv, NULL)		
	PHP_FE(dbadd_remark, NULL)		
	PHP_FE(dbget_remark, NULL)		
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
	PHP_FE(dbinvalid, NULL)		
	PHP_FE(dbstrtype, NULL)		
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
	PHP_FE(dbbase, NULL)		
	PHP_FE(db2xml, NULL)		
	PHP_FE(dbquery, NULL)		
	PHP_FE(dbresponse, NULL)		
	PHP_FE(dbwrite_view, NULL)		
	PHP_FE(dbread_view, NULL)		
	PHP_FE(dbsave_view, NULL)		
	PHP_FE(dbcompile, NULL)		
	PHP_FE(dbnextid, NULL)		
	PHP_FE(dbmark, NULL)		
	PHP_FE(dbdelete, NULL)		
	PHP_FE(dbcrunch, NULL)		
	PHP_FE(dblist2subset, NULL)		
	PHP_FE(dbget_range, NULL)		
	PHP_FE(eval_response, NULL)		
	PHP_FE(pfget, NULL)		
	PHP_FE(pfget_boolean, NULL)		
	PHP_FE(pfupdate, NULL)		
	PHP_FE(pffiles, NULL)		
	PHP_FE(pf2string, NULL)		
	PHP_FE(pfrequire, NULL)		
	PHP_FE(pfcompile, NULL)		
	PHP_FE(pfwrite, NULL)		
	PHP_FE(pfput, NULL)		
	PHP_FE(pfdel, NULL)		
	PHP_FE(pf2xml, NULL)		
	PHP_FE(strtdelta, NULL)		
	PHP_FE(strtime, NULL)		
	PHP_FE(strydtime, NULL)		
	PHP_FE(strdate, NULL)		
	PHP_FE(strlocaltime, NULL)		
	PHP_FE(strlocalydtime, NULL)		
	PHP_FE(strlocaldate, NULL)		
	PHP_FE(now, NULL)
	PHP_FE(is_epoch_string, NULL)
	PHP_FE(epoch2str, NULL)
	PHP_FE(str2epoch, NULL)
	PHP_FE(epoch, NULL)
	PHP_FE(yearday, NULL)			 
	PHP_FE(trapply_calib, NULL)		
	PHP_FE(trloadchan, NULL)		
	PHP_FE(trsample, NULL)		
	PHP_FE(trsamplebins, NULL)		
	PHP_FE(trfilter, NULL)		
	PHP_FE(trfree, NULL)		
	PHP_FE(trdestroy, NULL)		
	PHP_FE(trextract_data, NULL)		
	PHP_FE(trdata, NULL)		
	PHP_FE(trdatabins, NULL)		
	PHP_FE(trsplit, NULL)		
	PHP_FE(trsplice, NULL)		
	PHP_FE(trendtime, NULL)
	PHP_FE(trnsamp, NULL)
	PHP_FE(trsamp2time, NULL)
	PHP_FE(trsamprate, NULL)
	PHP_FE(trtime2samp, NULL)
	PHP_FE(finit_db, NULL)
	PHP_FE(map_autodrm_netsta, NULL)
	PHP_FE(map_autodrm_chanaux, NULL)
	PHP_FE(autodrm_net, NULL)
	PHP_FE(autodrm_aux, NULL)
	PHP_FE(map_seed_netsta, NULL)
	PHP_FE(map_seed_chanloc, NULL)
	PHP_FE(seed_net, NULL)
	PHP_FE(seed_loc, NULL)
	PHP_FE(abspath, NULL)
	PHP_FE(relpath, NULL)
	PHP_FE(cleanpath, NULL)
	PHP_FE(concatpaths, NULL)
	PHP_FE(parsepath, NULL)
	PHP_FE(yesno, NULL)
	PHP_FE(datafile, NULL)
	PHP_FE(datapath, NULL)
	PHP_FE(makedir, NULL)
	PHP_FE(make_pathdirs, NULL)
	PHP_FE(grn, NULL)
	PHP_FE(grname, NULL)
	PHP_FE(srn, NULL)
	PHP_FE(srname, NULL)
	PHP_FE(elog_init, NULL)
	PHP_FE(elog_log, NULL)
	PHP_FE(elog_debug, NULL)
	PHP_FE(elog_notify, NULL)
	PHP_FE(elog_alert, NULL)
	PHP_FE(elog_complain, NULL)
	PHP_FE(elog_die, NULL)
	PHP_FE(elog_string, NULL)
	PHP_FE(elog_clear, NULL)
	PHP_FE(elog_mark, NULL)
	PHP_FE(elog_flush, NULL)
	PHP_FE(elog_callback, NULL)
	{NULL, NULL, NULL}	
};

zend_module_entry Datascope_module_entry = {
	STANDARD_MODULE_HEADER,
	PHP_DATASCOPE_EXTNAME,
	Datascope_functions,
	PHP_MINIT(Datascope),
	PHP_MSHUTDOWN(Datascope),
	NULL,
	NULL,
	PHP_MINFO(Datascope),
	PHP_DATASCOPE_EXTVER,
	STANDARD_MODULE_PROPERTIES
};

ZEND_GET_MODULE(Datascope)

static void
_php_free_dbresponse( zend_rsrc_list_entry *rsrc TSRMLS_DC ) {
	Response *response = (Response *) rsrc->ptr;
	
	free_response( response );
}

void register_Datascope_constants( INIT_FUNC_ARGS )
{
	int	i;

	for( i = 0; i < NDbxlat; i++ ) {

		zend_register_long_constant( Dbxlat[i].name,
					     strlen( Dbxlat[i].name ) + 1, 
					     Dbxlat[i].num,
					     CONST_CS | CONST_PERSISTENT,
					     module_number TSRMLS_CC );
	}
}

PHP_MINIT_FUNCTION(Datascope)
{
	le_dbresponse = zend_register_list_destructors_ex( 
					_php_free_dbresponse, NULL, 
					"dbresponse", 0 );

	register_Datascope_constants( INIT_FUNC_ARGS_PASSTHRU );

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

static int
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

static int
z_arrval_to_strtbl( zval *array, Tbl **tbl )
{
	HashTable *target_hash;
	zval	**entry;
	int	nelements;

	if( Z_TYPE_P( array ) != IS_ARRAY ) {

		*tbl = 0;
		return -1;
	} 

	target_hash = Z_ARRVAL_P( array );

	nelements = zend_hash_num_elements( target_hash );

	if( nelements < 1 ) {

		*tbl = 0;
		return -1;
	}

	*tbl = newtbl( 0 );

	zend_hash_internal_pointer_reset( target_hash );

	while( nelements-- > 0 ) {

		zend_hash_get_current_data( target_hash, (void **) &entry );

		pushtbl( *tbl, strdup( Z_STRVAL_PP( entry ) ) );

		zend_hash_move_forward( target_hash );
	}

	return 0;
}

static int
z_arrval_to_inttbl( zval *array, Tbl **tbl )
{
	HashTable *target_hash;
	zval	**entry;
	int	nelements;

	if( Z_TYPE_P( array ) != IS_ARRAY ) {

		*tbl = 0;
		return -1;
	} 

	target_hash = Z_ARRVAL_P( array );

	nelements = zend_hash_num_elements( target_hash );

	if( nelements < 1 ) {

		*tbl = 0;
		return -1;
	}

	*tbl = newtbl( 0 );

	zend_hash_internal_pointer_reset( target_hash );

	while( nelements-- > 0 ) {

		zend_hash_get_current_data( target_hash, (void **) &entry );

		pushtbl( *tbl, (void *) Z_LVAL_PP( entry ) );

		zend_hash_move_forward( target_hash );
	}

	return 0;
}

static int
z_arrval_hashtype( zval *array )
{
	HashTable *target_hash;
	int	nelements;
	int	type;
	int	retcode = HASH_KEY_IS_LONG;

	if( Z_TYPE_P( array ) != IS_ARRAY ) {

		return HASH_KEY_NON_EXISTANT;
	} 

	target_hash = Z_ARRVAL_P( array );

	nelements = zend_hash_num_elements( target_hash );

	if( nelements < 1 ) {

		return HASH_KEY_NON_EXISTANT;
	}

	zend_hash_internal_pointer_reset( target_hash );

	while( nelements-- > 0 ) {

		type = zend_hash_get_current_key_type( target_hash );

		if( type == HASH_KEY_IS_STRING ) {
			
			retcode = HASH_KEY_IS_STRING;

			break;
		}

		zend_hash_move_forward( target_hash );
	}

	return retcode;
}

static int
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
			sprintf( value->s, "%ld", Z_LVAL_PP( zvalue ) );
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
			value->i = (long) Z_DVAL_PP( zvalue );
		} else if( Z_TYPE_PP( zvalue ) == IS_LONG ) {
			value->i = Z_LVAL_PP( zvalue );
		} else if( Z_TYPE_PP( zvalue ) == IS_BOOL ) {
			value->i = Z_BVAL_PP( zvalue );
		} else if( Z_TYPE_PP( zvalue ) == IS_STRING ) {
			value->i = yesno( Z_STRVAL_PP( zvalue ) );
		} else {
			return -1;
		}
		break;
	case dbINTEGER:
	case dbYEARDAY:
		if( Z_TYPE_PP( zvalue ) == IS_DOUBLE ) {
			value->i = (long) Z_DVAL_PP( zvalue );
		} else if( Z_TYPE_PP( zvalue ) == IS_LONG ) {
			value->i = Z_LVAL_PP( zvalue );
		} else if( Z_TYPE_PP( zvalue ) == IS_BOOL ) {
			value->i = Z_BVAL_PP( zvalue );
		} else if( Z_TYPE_PP( zvalue ) == IS_STRING ) {
			value->i = atol( Z_STRVAL_PP( zvalue ) );
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

static int 
zval2pf( zval *zvalue, Pf **pf )
{
	int	retcode = 0;
	char	value[STRSZ];
	HashTable *target_hash;
	int	nelements;
	zval	**entry;
	Pf	*pfvalue;
	char	*key = 0;
	ulong	num;

	if( Z_TYPE_P( zvalue ) == IS_STRING ) {

		*pf = pfnew( PFSTRING );

		(*pf)->value.s = strdup( Z_STRVAL_P( zvalue ) );

	} else if( Z_TYPE_P( zvalue ) == IS_LONG ) {

		*pf = pfnew( PFSTRING );

		sprintf( value, "%ld", Z_LVAL_P( zvalue ) );

		(*pf)->value.s = strdup( value );

	} else if( Z_TYPE_P( zvalue ) == IS_DOUBLE ) {

		*pf = pfnew( PFSTRING );

		strdbl( Z_DVAL_P( zvalue ), value );

		(*pf)->value.s = strdup( value );

	} else if( Z_TYPE_P( zvalue ) == IS_BOOL ) {

		*pf = pfnew( PFSTRING );

		sprintf( value, "%d", Z_BVAL_P( zvalue ) );

		(*pf)->value.s = strdup( value );

	} else if( Z_TYPE_P( zvalue ) == IS_ARRAY &&
		   z_arrval_hashtype( zvalue ) == HASH_KEY_IS_LONG ) {

		*pf = pfnew( PFTBL );

		target_hash = Z_ARRVAL_P( zvalue );

		nelements = zend_hash_num_elements( target_hash );

		zend_hash_internal_pointer_reset( target_hash );

		while( nelements-- > 0 ) {

			zend_hash_get_current_data( target_hash, 
						    (void **) &entry );

			zval2pf( *entry, &pfvalue );

			pushtbl( (*pf)->value.tbl, (char *) pfvalue );

			zend_hash_move_forward( target_hash );
		}

	} else if( Z_TYPE_P( zvalue ) == IS_ARRAY &&
		   z_arrval_hashtype( zvalue ) == HASH_KEY_IS_STRING ) {

		*pf = pfnew( PFARR );

		target_hash = Z_ARRVAL_P( zvalue );

		nelements = zend_hash_num_elements( target_hash );

		zend_hash_internal_pointer_reset( target_hash );

		while( nelements-- > 0 ) {

			zend_hash_get_current_key( target_hash, &key, &num, 1 );
						   
			zend_hash_get_current_data( target_hash, 
						    (void **) &entry );

			zval2pf( *entry, &pfvalue );

			setarr( (*pf)->value.arr, key, pfvalue );

			efree( key );

			zend_hash_move_forward( target_hash );
		}

	} else {
		
		zend_error( E_ERROR, "Type not supported for pf conversion\n" );

		retcode = -1;
	}

	return retcode;
}

static int
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

/* {{{ proto ?????? template( array db, ... ) *
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
* }}} */

/* {{{ proto int elog_init( int argc, array argv ) */
PHP_FUNCTION(elog_init)
{
	int	thisfunction_argc = ZEND_NUM_ARGS();
	long	argc;
	zval	*zval_argv;
	char	**Cstyle_argv;
	HashTable *argv_hash;
	zval	**entry;
	int	iarg;
	int	rc;

	if( thisfunction_argc != 2 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( thisfunction_argc TSRMLS_CC, "la", 
					&argc, &zval_argv )
	    == FAILURE) {

		return;
	}

	if( zend_hash_num_elements( Z_ARRVAL_P( zval_argv ) ) != argc ) {
		
		zend_error( E_ERROR, "argc and argv do not match!\n" );
	}

	argv_hash = HASH_OF( zval_argv );

	zend_hash_internal_pointer_reset( argv_hash );

	allot( char **, Cstyle_argv, argc );

	for( iarg = 0; iarg < argc; iarg++ ) {

		rc = zend_hash_get_current_data( argv_hash, (void **) &entry );

		if( ( rc == SUCCESS ) && 
		    ( Z_TYPE_PP( entry ) == IS_STRING ) ) {

			Cstyle_argv[iarg] = Z_STRVAL_PP( entry );

		} else {

			free( Cstyle_argv );

			zend_error( E_ERROR, "Second argument must be " 
					     "an array of strings\n" );
		}

		zend_hash_move_forward( argv_hash );
	}

	rc = elog_init( argc, Cstyle_argv );

	free( Cstyle_argv );

	RETURN_LONG( rc );
}
/* }}} */

int
elog_callback( int severity, char *logstring, Tbl *Elog )
{
	int	rc;
	char	msg[STRSZ];
	zval	*function_name;
	zval	*retval;
	zval	*zval_severity;
	zval	*zval_logstring;
	zval	**params[2];

	CLS_FETCH();

	MAKE_STD_ZVAL( function_name );

	ZVAL_STRING( function_name, Elog_replacement, 1 );

	MAKE_STD_ZVAL( zval_severity );
	ZVAL_LONG( zval_severity, severity );

	params[0] = &zval_severity;

	MAKE_STD_ZVAL( zval_logstring );
	ZVAL_STRING( zval_logstring, logstring, 1 );

	params[1] = &zval_logstring;

	rc = call_user_function_ex( CG(function_table), 
				    NULL, 
				    function_name, 
				    &retval, 
				    2,
				    params,
				    0, 
				    NULL TSRMLS_CC );

	zval_dtor( function_name );

	if( rc != SUCCESS ) {
		
		sprintf( msg, "Elog callback to function '%s' failed!\n", 
			 Elog_replacement );

		zend_error( E_WARNING, "%s", msg );

		return 0;
	}

	if( Z_TYPE_P( retval ) != IS_LONG ) {

		sprintf( msg, "Elog callback function '%s' must return an "
			      "integer! Callback failed.\n", 
			      Elog_replacement );

		zend_error( E_WARNING, "%s", msg );

		return 0;
	}

	rc = Z_LVAL_P( retval );

	zval_dtor( retval );

	return rc;
}

/* {{{ proto void elog_callback( string replacement ) */
PHP_FUNCTION(elog_callback)
{
	int	argc = ZEND_NUM_ARGS();
	char	*replacement;
	int	replacement_len;

	if( argc != 1 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "s", 
					&replacement, &replacement_len )
	    == FAILURE) {

		return;
	}

	Elog_replacement = strdup( replacement );

	elog_set( ELOG_CALLBACK, 0, (void *) elog_callback );

	return;
}
/* }}} */

/* {{{ proto void elog_log( string msg ) */
PHP_FUNCTION(elog_log)
{
	int	argc = ZEND_NUM_ARGS();
	char	*msg;
	int	msg_len;

	if( argc != 1 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "s", &msg, &msg_len )
	    == FAILURE) {

		return;
	}

	elog_log( 0, "%s", msg );

	return;
}
/* }}} */

/* {{{ proto void elog_debug( string msg ) */
PHP_FUNCTION(elog_debug)
{
	int	argc = ZEND_NUM_ARGS();
	char	*msg;
	int	msg_len;

	if( argc != 1 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "s", &msg, &msg_len )
	    == FAILURE) {

		return;
	}

	elog_debug( 0, "%s", msg );

	return;
}
/* }}} */

/* {{{ proto void elog_alert( string msg ) */
PHP_FUNCTION(elog_alert)
{
	int	argc = ZEND_NUM_ARGS();
	char	*msg;
	int	msg_len;

	if( argc != 1 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "s", &msg, &msg_len )
	    == FAILURE) {

		return;
	}

	elog_alert( 0, "%s", msg );

	return;
}
/* }}} */

/* {{{ proto void elog_complain( string msg ) */
PHP_FUNCTION(elog_complain)
{
	int	argc = ZEND_NUM_ARGS();
	char	*msg;
	int	msg_len;

	if( argc != 1 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "s", &msg, &msg_len )
	    == FAILURE) {

		return;
	}

	elog_complain( 0, "%s", msg );

	return;
}
/* }}} */

/* {{{ proto void elog_notify( string msg ) */
PHP_FUNCTION(elog_notify)
{
	int	argc = ZEND_NUM_ARGS();
	char	*msg;
	int	msg_len;

	if( argc != 1 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "s", &msg, &msg_len )
	    == FAILURE) {

		return;
	}

	elog_notify( 0, "%s", msg );

	return;
}
/* }}} */

/* {{{ proto void elog_die( string msg ) */
PHP_FUNCTION(elog_die)
{
	int	argc = ZEND_NUM_ARGS();
	char	*msg;
	int	msg_len;

	if( argc != 1 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "s", &msg, &msg_len )
	    == FAILURE) {

		return;
	}

	elog_die( 0, "%s", msg );

	return;
}
/* }}} */

/* {{{ proto string elog_string( int n ) */
PHP_FUNCTION(elog_string)
{
	int	argc = ZEND_NUM_ARGS();
	long	n;
	char	*log;
	char	*log_safe_copy;

	if( argc != 1 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "l", &n )
	    == FAILURE) {

		return;
	}

	log = elog_string( n );

	log_safe_copy = estrdup( log );

	free( log );

	RETURN_STRING( log_safe_copy, 0 );
}
/* }}} */

/* {{{ proto void elog_clear( void ) */
PHP_FUNCTION(elog_clear)
{
	int	argc = ZEND_NUM_ARGS();

	if( argc != 0 ) {

		WRONG_PARAM_COUNT;
	}

	elog_clear();

	return;
}
/* }}} */

/* {{{ proto int elog_mark( void ) */
PHP_FUNCTION(elog_mark)
{
	int	argc = ZEND_NUM_ARGS();
	int	nmessages;

	if( argc != 0 ) {

		WRONG_PARAM_COUNT;
	}

	nmessages = elog_mark();

	RETURN_LONG( nmessages );
}
/* }}} */

/* {{{ proto void elog_flush( int deliver, int first ) */
PHP_FUNCTION(elog_flush)
{
	int	argc = ZEND_NUM_ARGS();
	long	deliver;
	long	first;

	if( argc != 2 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "ll", &deliver, &first )
	    == FAILURE) {

		return;
	}

	elog_flush( (int) deliver, (int) first );

	return;
}
/* }}} */

/* {{{ proto int yesno( string s ) */
PHP_FUNCTION(yesno)
{
	int	argc = ZEND_NUM_ARGS();
	char	*s;
	int	s_len;
	int	rc;

	if( argc != 1 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "s", 
			&s, &s_len )
	    == FAILURE) {

		return;
	}

	rc = yesno( s );

	RETURN_LONG( rc );
}
/* }}} */

/* {{{ proto int makedir( string dir ) */
PHP_FUNCTION(makedir)
{
	int	argc = ZEND_NUM_ARGS();
	char	*dir;
	int	dir_len;
	int	rc;

	if( argc != 1 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "s", 
			&dir, &dir_len )
	    == FAILURE) {

		return;
	}

	rc = makedir( dir );

	RETURN_LONG( rc );
}
/* }}} */

/* {{{ proto int make_pathdirs( string filename ) */
PHP_FUNCTION(make_pathdirs)
{
	int	argc = ZEND_NUM_ARGS();
	char	*filename;
	int	filename_len;
	int	rc;

	if( argc != 1 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "s", 
			&filename, &filename_len )
	    == FAILURE) {

		return;
	}

	rc = make_pathdirs( filename );

	RETURN_LONG( rc );
}
/* }}} */

/* {{{ proto string datafile( string envname, string filename ) */
PHP_FUNCTION(datafile)
{
	int	argc = ZEND_NUM_ARGS();
	char	*envname;
	int	envname_len;
	char	*filename;
	int	filename_len;
	char	*dfile;
	char	*dfile_safe_copy;

	if( argc != 2 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "ss", 
			&envname, &envname_len, &filename, &filename_len )
	    == FAILURE) {

		return;
	}

	dfile = datafile( envname, filename );

	if( dfile == 0 ) {

		return;
	}

	dfile_safe_copy = estrdup( dfile );

	free( dfile );
	
	RETURN_STRING( dfile_safe_copy, 0 );
}
/* }}} */

/* {{{ proto string datapath( string envname, string dirname, string filename, string suffix ) */
PHP_FUNCTION(datapath)
{
	int	argc = ZEND_NUM_ARGS();
	char	*envname;
	int	envname_len;
	char	*dirname;
	int	dirname_len;
	char	*filename;
	int	filename_len;
	char	*suffix;
	int	suffix_len;
	char	*dfile;
	char	*dfile_safe_copy;

	if( argc != 4 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "ssss", 
			&envname, &envname_len, 
			&dirname, &dirname_len, 
			&filename, &filename_len, 
			&suffix, &suffix_len )
	    == FAILURE) {

		return;
	}

	dfile = datapath( envname, dirname, filename, suffix );

	if( dfile == 0 ) {

		return;
	}

	dfile_safe_copy = estrdup( dfile );

	free( dfile );
	
	RETURN_STRING( dfile_safe_copy, 0 );
}
/* }}} */

/* {{{ proto string abspath( string relpath ) */
PHP_FUNCTION(abspath)
{
	int	argc = ZEND_NUM_ARGS();
	char	*rel;
	int	rel_len;
	char	abs[FILENAME_MAX];

	if( argc != 1 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "s", 
			&rel, &rel_len )
	    == FAILURE) {

		return;
	}

	abspath( rel, abs );
	
	RETURN_STRING( abs, 1 );
}
/* }}} */

/* {{{ proto string relpath( string from, string to ) */
PHP_FUNCTION(relpath)
{
	int	argc = ZEND_NUM_ARGS();
	char	*from;
	int	from_len;
	char	*to;
	int	to_len;
	char	rel[FILENAME_MAX];

	if( argc != 2 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "ss", 
			&from, &from_len, &to, &to_len )
	    == FAILURE) {

		return;
	}

	relpath( from, to, rel );
	
	RETURN_STRING( rel, 1 );
}
/* }}} */

/* {{{ proto string cleanpath( string path [, int nolinks] ) */
PHP_FUNCTION(cleanpath)
{
	int	argc = ZEND_NUM_ARGS();
	char	*path;
	int	path_len;
	char	new[FILENAME_MAX];
	int	flags = 0;
	long	nolinks = 0;

	if( argc < 1 || argc > 2 ) {

		WRONG_PARAM_COUNT;

	} else if( argc == 1 ) {

		if( zend_parse_parameters( argc TSRMLS_CC, "s", 
				&path, &path_len )
	    	== FAILURE) {
	
			return;
		}

	} else {		/* argc == 2 */

		if( zend_parse_parameters( argc TSRMLS_CC, "sl", 
				&path, &path_len, &nolinks )
	    	== FAILURE) {
	
			return;
		}
	}

	flags = nolinks;

	cleanpath( path, flags, new );
	
	RETURN_STRING( new, 1 );
}
/* }}} */

/* {{{ proto string concatpaths( string a, string b ) */
PHP_FUNCTION(concatpaths)
{
	int	argc = ZEND_NUM_ARGS();
	char	*a;
	int	a_len;
	char	*b;
	int	b_len;
	char	*new;
	char	*new_safe_copy;

	if( argc != 2 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "ss", 
			&a, &a_len, &b, &b_len )
	    == FAILURE) {

		return;
	}

	new = concatpaths( a, b, NULL );

	new_safe_copy = estrdup( new );

	free( new );
	
	RETURN_STRING( new_safe_copy, 0 );
}
/* }}} */

/* {{{ proto array parsepath( string a ) */
PHP_FUNCTION(parsepath)
{
	int	argc = ZEND_NUM_ARGS();
	char	*a;
	int	a_len;
	char	dir[FILENAME_MAX];
	char	base[FILENAME_MAX];
	char	suffix[FILENAME_MAX];

	if( argc != 1 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "s", 
			&a, &a_len )
	    == FAILURE) {

		return;
	}

	parsepath( a, dir, base, suffix );

	array_init( return_value );

	add_next_index_string( return_value, dir, 1 );
	add_next_index_string( return_value, base, 1 );

	if( suffix != NULL ) {

		add_next_index_string( return_value, suffix, 1 );
	}

	return;
}
/* }}} */

/* {{{ proto string map_autodrm_netsta( string anet, string fsta ) */
PHP_FUNCTION(map_autodrm_netsta)
{
	int	argc = ZEND_NUM_ARGS();
	char	*anet;
	int	anet_len;
	char	*fsta;
	int	fsta_len;
	char	sta[STRSZ];

	if( argc != 2 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "ss", 
			&anet, &anet_len, &fsta, &fsta_len )
	    == FAILURE) {

		return;
	}

	map_autodrm_netsta( anet, fsta, sta );
	
	RETURN_STRING( sta, 1 );
}
/* }}} */

/* {{{ proto string map_autodrm_chanaux( string sta, string fchan, string aux ) */
PHP_FUNCTION(map_autodrm_chanaux)
{
	int	argc = ZEND_NUM_ARGS();
	char	*sta;
	int	sta_len;
	char	*fchan;
	int	fchan_len;
	char	*aux;
	int	aux_len;
	char	chan[STRSZ];

	if( argc != 3 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "sss", 
		&sta, &sta_len, &fchan, &fchan_len, &aux, &aux_len )
	    == FAILURE) {

		return;
	}

	map_autodrm_chanaux( sta, fchan, aux, chan );
	
	RETURN_STRING( chan, 1 );
}
/* }}} */

/* {{{ proto array autodrm_net( string sta ) */
PHP_FUNCTION(autodrm_net)
{
	int	argc = ZEND_NUM_ARGS();
	char	*sta;
	int	sta_len;
	char	anet[STRSZ];
	char	fsta[STRSZ];

	if( argc != 1 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "s", 
			&sta, &sta_len )
	    == FAILURE) {

		return;
	}

	array_init( return_value );

	autodrm_net( sta, anet, fsta );

	add_next_index_string( return_value, anet, 1 );
	add_next_index_string( return_value, fsta, 1 );
	
	return;
}
/* }}} */

/* {{{ proto array autodrm_aux( string sta, string chan ) */
PHP_FUNCTION(autodrm_aux)
{
	int	argc = ZEND_NUM_ARGS();
	char	*sta;
	int	sta_len;
	char	*chan;
	int	chan_len;
	char	fchan[STRSZ];
	char	aux[STRSZ];

	if( argc != 2 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "ss", 
			&sta, &sta_len, &chan, &chan_len )
	    == FAILURE) {

		return;
	}

	array_init( return_value );

	autodrm_aux( sta, chan, fchan, aux );

	add_next_index_string( return_value, fchan, 1 );
	add_next_index_string( return_value, aux, 1 );
	
	return;
}
/* }}} */

/* {{{ proto string map_seed_netsta( string snet, string fsta ) */
PHP_FUNCTION(map_seed_netsta)
{
	int	argc = ZEND_NUM_ARGS();
	char	*snet;
	int	snet_len;
	char	*fsta;
	int	fsta_len;
	char	sta[STRSZ];

	if( argc != 2 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "ss", 
			&snet, &snet_len, &fsta, &fsta_len )
	    == FAILURE) {

		return;
	}

	map_seed_netsta( snet, fsta, sta );
	
	RETURN_STRING( sta, 1 );
}
/* }}} */

/* {{{ proto string map_seed_chanloc( string sta, string fchan, string loc ) */
PHP_FUNCTION(map_seed_chanloc)
{
	int	argc = ZEND_NUM_ARGS();
	char	*sta;
	int	sta_len;
	char	*fchan;
	int	fchan_len;
	char	*loc;
	int	loc_len;
	char	chan[STRSZ];

	if( argc != 3 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "sss", 
		&sta, &sta_len, &fchan, &fchan_len, &loc, &loc_len )
	    == FAILURE) {

		return;
	}

	map_seed_chanloc( sta, fchan, loc, chan );
	
	RETURN_STRING( chan, 1 );
}
/* }}} */

/* {{{ proto array seed_net( string sta ) */
PHP_FUNCTION(seed_net)
{
	int	argc = ZEND_NUM_ARGS();
	char	*sta;
	int	sta_len;
	char	snet[STRSZ];
	char	fsta[STRSZ];

	if( argc != 1 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "s", 
			&sta, &sta_len )
	    == FAILURE) {

		return;
	}

	array_init( return_value );

	seed_net( sta, snet, fsta );

	add_next_index_string( return_value, snet, 1 );
	add_next_index_string( return_value, fsta, 1 );
	
	return;
}
/* }}} */

/* {{{ proto array seed_loc( string sta, string chan ) */
PHP_FUNCTION(seed_loc)
{
	int	argc = ZEND_NUM_ARGS();
	char	*sta;
	int	sta_len;
	char	*chan;
	int	chan_len;
	char	fchan[STRSZ];
	char	loc[STRSZ];

	if( argc != 2 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "ss", 
			&sta, &sta_len, &chan, &chan_len )
	    == FAILURE) {

		return;
	}

	array_init( return_value );

	seed_loc( sta, chan, fchan, loc );

	add_next_index_string( return_value, fchan, 1 );
	add_next_index_string( return_value, loc, 1 );
	
	return;
}
/* }}} */

/* {{{ proto int finit_db( array db ) */
PHP_FUNCTION(finit_db)
{
	zval	*db_array;
	Dbptr	db;
	int	argc = ZEND_NUM_ARGS();
	int	rc;

	if( argc != 1 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "a", 
					&db_array )
	    == FAILURE) {

		return;

	} else if( z_arrval_to_dbptr( db_array, &db ) < 0 ) {

		return;
	}

	rc = finit_db( db );

	RETURN_LONG( rc );
}
/* }}} */

/* {{{ proto double now( void ) */
PHP_FUNCTION(now)
{
	int	argc = ZEND_NUM_ARGS();
	double	epoch;

	if( argc != 0 ) {

		WRONG_PARAM_COUNT;
	}

	epoch = std_now();

	RETURN_DOUBLE( epoch );
}
/* }}} */

/* {{{ proto double is_epoch_string( string timestring ) */
PHP_FUNCTION(is_epoch_string)
{
	int	argc = ZEND_NUM_ARGS();
	double	epoch;
	char	*timestring;
	int	timestring_len;
	int	rc;

	if( argc != 1 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "s", 
				&timestring, &timestring_len )
	    == FAILURE) {

		return;
	}

	rc = is_epoch_string( timestring, &epoch );

	if( rc > 0 ) {

		RETURN_DOUBLE( epoch );

	} else {

		return;
	}
}
/* }}} */

/* {{{ proto double epoch( int yearday ) */
PHP_FUNCTION(epoch)
{
	int	argc = ZEND_NUM_ARGS();
	double	time;
	long	yearday;

	if( argc != 1 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "l", 
				&yearday )
	    == FAILURE) {

		return;
	}

	time = epoch( yearday );

	RETURN_DOUBLE( time );
}
/* }}} */

/* {{{ proto int yearday( double epoch ) */
PHP_FUNCTION(yearday)
{
	int	argc = ZEND_NUM_ARGS();
	double	epoch;
	int	yd;

	if( argc != 1 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "d", 
				&epoch )
	    == FAILURE) {

		return;
	}

	yd = yearday( epoch );

	RETURN_LONG( yd );
}
/* }}} */

/* {{{ proto double str2epoch( string timestring ) */
PHP_FUNCTION(str2epoch)
{
	int	argc = ZEND_NUM_ARGS();
	double	epoch;
	char	*timestring;
	int	timestring_len;

	if( argc != 1 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "s", 
				&timestring, &timestring_len )
	    == FAILURE) {

		return;
	}

	epoch = str2epoch( timestring );

	RETURN_DOUBLE( epoch );
}
/* }}} */

/* {{{ proto string epoch2str( double epoch, string format [, string timezone] ) */
PHP_FUNCTION(epoch2str)
{
	int	argc = ZEND_NUM_ARGS();
	double	epoch;
	char	*format = 0;
	int	format_len;
	char	*timezone = 0;
	int	timezone_len = 0;
	char	*timestring;
	char	*timestring_safe_copy;

	if( argc < 2 || argc > 3 ) {

		WRONG_PARAM_COUNT;

	} else if( argc == 2 ) {

		if( zend_parse_parameters( argc TSRMLS_CC, "ds", 
			&epoch, &format, &format_len )
	    		== FAILURE) {

			return;
		}

	} else {	/* argc == 3 */

		if( zend_parse_parameters( argc TSRMLS_CC, "dss", 
			&epoch, &format, &format_len, &timezone, &timezone_len )
	    		== FAILURE) {

			return;
		}
	}

	timestring = zepoch2str( epoch, format, timezone );

	if( timestring == 0 ) {
		
		zend_error( E_ERROR, "Time conversion failure\n" );
		return;
	} 

	timestring_safe_copy = estrdup( timestring );

	free( timestring );

	RETURN_STRING( timestring_safe_copy, 0 );
}
/* }}} */

/* {{{ proto int grn( double lat, double lon ) */
PHP_FUNCTION(grn)
{
	int	argc = ZEND_NUM_ARGS();
	double	lat;
	double	lon;
	int	grn;

	if( argc != 2 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "dd", 
				&lat, &lon )
	    == FAILURE) {

		return;
	}

	grn = grnumber( lat, lon ); 

	RETURN_LONG( grn );
}
/* }}} */

/* {{{ proto string grname( double lat, double lon ) */
PHP_FUNCTION(grname)
{
	int	argc = ZEND_NUM_ARGS();
	double	lat;
	double	lon;
	int	grn;
	char	name[STRSZ];

	if( argc != 2 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "dd", 
				&lat, &lon )
	    == FAILURE) {

		return;
	}

	grn = grnumber( lat, lon ); 

	grname( grn, name );

	RETURN_STRING( name, 1 );
}
/* }}} */

/* {{{ proto int srn( double lat, double lon ) */
PHP_FUNCTION(srn)
{
	int	argc = ZEND_NUM_ARGS();
	double	lat;
	double	lon;
	int	grn;
	int	srn;

	if( argc != 2 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "dd", 
				&lat, &lon )
	    == FAILURE) {

		return;
	}

	grn = grnumber( lat, lon );

	srn = srnumber( grn ); 

	RETURN_LONG( srn );
}
/* }}} */

/* {{{ proto string srname( double lat, double lon ) */
PHP_FUNCTION(srname)
{
	int	argc = ZEND_NUM_ARGS();
	double	lat;
	double	lon;
	int	grn;
	int	srn;
	char	name[STRSZ];

	if( argc != 2 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "dd", 
				&lat, &lon )
	    == FAILURE) {

		return;
	}

	grn = grnumber( lat, lon );

	srn = srnumber( grn ); 

	srname( srn, name );

	RETURN_STRING( name, 1 );
}
/* }}} */

/* {{{ proto int trtime2samp( double time0, double samprate, double time1 ) */
PHP_FUNCTION(trtime2samp)
{
	int	argc = ZEND_NUM_ARGS();
	double	time0;
	double	samprate;
	double	time1;
	int	isamp;

	if( argc != 3 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "ddd", 
			&time0, &samprate, &time1 )
	    == FAILURE) {

		return;
	}

	isamp = TIME2SAMP( time0, samprate, time1 );

	RETURN_LONG( isamp );
}
/* }}} */

/* {{{ proto double trsamp2time( double time0, double samprate, int isamp ) */
PHP_FUNCTION(trsamp2time)
{
	int	argc = ZEND_NUM_ARGS();
	double	time0;
	double	samprate;
	long	isamp;
	double	time1;

	if( argc != 3 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "ddl", 
			&time0, &samprate, &isamp )
	    == FAILURE) {

		return;
	}

	time1 = SAMP2TIME( time0, samprate, isamp );

	RETURN_DOUBLE( time1 );
}
/* }}} */

/* {{{ proto double trsamprate( double time0, int nsamp, double endtime ) */
PHP_FUNCTION(trsamprate)
{
	int	argc = ZEND_NUM_ARGS();
	double	time0;
	long	nsamp;
	double	endtime;
	double	samprate;

	if( argc != 3 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "dld", 
			&time0, &nsamp, &endtime )
	    == FAILURE) {

		return;
	}

	samprate = SAMPRATE( time0, nsamp, endtime );

	RETURN_DOUBLE( samprate );
}
/* }}} */

/* {{{ proto int trnsamp( double time0, double samprate, double endtime ) */
PHP_FUNCTION(trnsamp)
{
	int	argc = ZEND_NUM_ARGS();
	double	time0;
	double	samprate;
	double	endtime;
	long	nsamp;

	if( argc != 3 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "ddd", 
			&time0, &samprate, &endtime )
	    == FAILURE) {

		return;
	}

	nsamp = NSAMP( time0, samprate, endtime );

	RETURN_LONG( nsamp );
}
/* }}} */

/* {{{ proto double trendtime( double time0, double samprate, int nsamp ) */
PHP_FUNCTION(trendtime)
{
	int	argc = ZEND_NUM_ARGS();
	double	time0;
	double	samprate;
	long	nsamp;
	double	endtime;

	if( argc != 3 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "ddl", 
			&time0, &samprate, &nsamp )
	    == FAILURE) {

		return;
	}

	endtime = ENDTIME( time0, samprate, nsamp );

	RETURN_DOUBLE( endtime );
}
/* }}} */

/* {{{ proto array dbinvalid( array db, ... ) */
PHP_FUNCTION(dbinvalid)
{
	Dbptr	db;
	int	argc = ZEND_NUM_ARGS();

	if( argc != 0 ) {

		WRONG_PARAM_COUNT;
	}

	db = dbinvalid();

	RETURN_DBPTR( db );
}
/* }}} */

/* {{{ proto int pfupdate( string pfname ) */
PHP_FUNCTION(pfupdate)
{
	int	argc = ZEND_NUM_ARGS();
	char	*pfname;
	int	pfname_len;
	int	rc;

	if( argc != 1 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "s", 
					&pfname, &pfname_len )
	    == FAILURE) {

		return;
	}

	rc = updatePf( pfname );

	RETURN_LONG( rc );
}
/* }}} */

/* {{{ proto int pfrequire( string pfname, string atime ) */
PHP_FUNCTION(pfrequire)
{
	int	argc = ZEND_NUM_ARGS();
	char	*pfname;
	int	pfname_len;
	char	*atime;
	int	atime_len;
	int	rc;

	if( argc != 2 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "ss", 
				&pfname, &pfname_len, &atime, &atime_len )
	    == FAILURE) {

		return;
	}

	rc = pfrequire( pfname, atime );

	RETURN_LONG( rc );
}
/* }}} */

/* {{{ proto int pfwrite( string filename, string pfname ) */
PHP_FUNCTION(pfwrite)
{
	int	argc = ZEND_NUM_ARGS();
	char	*filename;
	int	filename_len;
	char	*pfname;
	int	pfname_len;
	Pf	*pf;
	int	rc;

	if( argc != 2 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "ss", 
				&filename, &filename_len, &pfname, &pfname_len )
	    == FAILURE) {

		return;
	}

	if( ( pf = getPf( pfname ) ) == (Pf *) NULL ) {

		zend_error( E_ERROR, "failure opening parameter file\n" );
	}

	rc = pfwrite( filename, pf );

	if( rc != 0 ) {

		zend_error( E_ERROR, "failed to write parameter file\n" );
	}

	RETURN_LONG( rc );
}
/* }}} */

/* {{{ proto void pfcompile( string value, string pfname ) */
PHP_FUNCTION(pfcompile)
{
	int	argc = ZEND_NUM_ARGS();
	char	*value;
	int	value_len;
	char	*pfname;
	int	pfname_len;
	Pf	*pf;
	int	rc;

	if( argc != 2 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "ss", 
				&value, &value_len, &pfname, &pfname_len )
	    == FAILURE) {

		return;
	}

	if( ( pf = getPf( pfname ) ) == (Pf *) NULL ) {

		pf = pfnew( PFFILE );

		putPf( pfname, pf );
	}

	rc = pfcompile( value, &pf );

	if( rc != 0 ) {

		zend_error( E_ERROR, "failed to compile value into "
				     "parameter file\n" );
		return;
	}

	return;
}
/* }}} */

/* {{{ proto string pf2string( string pfname ) */
PHP_FUNCTION(pf2string)
{
	int	argc = ZEND_NUM_ARGS();
	char	*pfname;
	int	pfname_len;
	Pf	*pf;
	char	*value;
	char	*value_safe_copy;

	if( argc != 1 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "s", 
					&pfname, &pfname_len )
	    == FAILURE) {

		return;
	}

	if( ( pf = getPf( pfname ) ) == (Pf *) NULL ) {

		zend_error( E_ERROR, "failure opening parameter file\n" );
	}
	
	value = pf2string( pf );

	value_safe_copy = estrdup( value );

	free( value );

	RETURN_STRING( value_safe_copy, 0 );
}
/* }}} */

/* {{{ proto array pffiles( string pfname [, int all] ) */
PHP_FUNCTION(pffiles)
{
	int	argc = ZEND_NUM_ARGS();
	char	*pfname;
	int	pfname_len;
	Tbl	*files;
	long	all = 0;
	int	i;

	if( argc < 1 || argc > 2 ) {

		WRONG_PARAM_COUNT;

	} else if( argc == 1 ) {

		if( zend_parse_parameters( argc TSRMLS_CC, "s", 
						&pfname, &pfname_len )
	    		== FAILURE) {

			return;
		}

	} else {	/* argc == 2 */

		if( zend_parse_parameters( argc TSRMLS_CC, "sl", 
						&pfname, &pfname_len, &all )
	    		== FAILURE) {

			return;
		}
	}

	files = pffiles( pfname, (int) all );

	if( files == (Tbl *) NULL ) {

		zend_error( E_ERROR, "No filenames returned!\n" );
	}

	array_init( return_value );

	for( i = 0; i < maxtbl( files ); i++ ) {

		add_next_index_string( return_value, gettbl( files, i ), 1 );
	}

	return;
}
/* }}} */

/* {{{ proto mixed pfdel( string pfname, string pfkey ) */
PHP_FUNCTION(pfdel)
{
	int	argc = ZEND_NUM_ARGS();
	char	*pfname;
	int	pfname_len;
	char	*key;
	int	key_len;
	Pf	*pf;
	Pf	*pfvalue;
	char	errstring[STRSZ];

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

	pfvalue = pfdel( pf, key );

	if( pfvalue == (Pf *) NULL  ) {

		sprintf( errstring, 
			"parameter '%s' not found in parameter-file '%s'\n",
			key, pfname );

		zend_error( E_ERROR, "%s", errstring );
	} 
	
	if( pf2zval( pfvalue, return_value ) < 0 ) {

		zend_error( E_ERROR, "pfdel: failed to convert value\n" );
	} 

	pffree( pfvalue );

	return;
}
/* }}} */

/* {{{ proto string pf2xml( string pfname [, int flags [, string prolog [, string name]]] ) */
PHP_FUNCTION(pf2xml)
{
	int	argc = ZEND_NUM_ARGS();
	char	*pfname;
	int	pfname_len;
	char	*name = 0;
	int	name_len = 0;
	char	*prolog = 0;
	int	prolog_len = 0;
	char	*flags_string = 0;
	int	flags_string_len = 0;
	int	flags = 0;
	Pf	*pf;
	char	*xml;
	char	*xml_safe_copy;

	if( argc < 1 || argc > 4 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "s|sss", 
					&pfname, &pfname_len,
					&flags_string, &flags_string_len,
					&prolog, &prolog_len,
					&name, &name_len )
	    == FAILURE) {

		return;
	}

	if( ( pf = getPf( pfname ) ) == (Pf *) NULL ) {

		zend_error( E_ERROR, "failure opening parameter file\n" );
	}

	if( name == (char *) NULL ) {
		
		name = pfname;
	}

	if( flags_string != NULL &&
	    strmatches( flags_string, ".*PFXML_STRONG.*", 0 ) ) {

		flags |= PFXML_STRONG;
	}

	if( flags_string != NULL &&
	    strmatches( flags_string, ".*PFXML_NEWLINES.*", 0 ) ) {

		flags |= PFXML_NEWLINES;
	}

	if( flags_string != NULL &&
	    strmatches( flags_string, ".*PFXML_PRESERVE_PFFILE.*", 0 ) ) {

		flags |= PFXML_PRESERVE_PFFILE;
	}

	xml = pf2xml( pf, name, prolog, flags );

	xml_safe_copy = estrdup( xml );

	free( xml );

	RETURN_STRING( xml_safe_copy, 0 );
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

		zend_error( E_ERROR, "%s", errstring );
	} 
	
	if( pf2zval( pfvalue, return_value ) < 0 ) {

		zend_error( E_ERROR, "pfget: failed to convert value\n" );
	} 

	return;
}
/* }}} */

/* {{{ proto void pfput( string pfkey, mixed pfvalue, string pfname ) */
PHP_FUNCTION(pfput)
{
	int	argc = ZEND_NUM_ARGS();
	zval	*zval_pfvalue;
	char	*pfname;
	int	pfname_len;
	char	*key;
	int	key_len;
	Pf	*pf;
	Pf	*pfvalue;
	int	rc;

	if( argc != 3 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "szs", 
					&key, &key_len, &zval_pfvalue, 
					&pfname, &pfname_len )
	    == FAILURE) {

		return;
	}

	if( ( pf = existsPf( pfname ) ) == (Pf *) NULL ) {

		pf = pfnew( PFFILE );

		putPf( pfname, pf );
	}

	rc = zval2pf( zval_pfvalue, &pfvalue );

	if( rc != 0 ) {
		
		zend_error( E_ERROR, "pfput failed\n" );
	}

	pfput( pf, key, (char *) pfvalue, PFPF );

	return;
}
/* }}} */

/* {{{ proto int pfget_boolean( string pfname, string pfkey ) */
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

/* {{{ proto array trdata( array tr [, int i0 [, int npts]] ) */
PHP_FUNCTION(trdata)
{
	zval	*tr_array;
	Dbptr	tr;
	float	*data = NULL;
	int	argc = ZEND_NUM_ARGS();
	int 	single_row = 0;
	long	nrecs;
	long	nsamp_retrieve = 0;
	long	nsamp_available = 0;
	long 	nsamp_requested = -1;
	long	i0 = 0;
	int	isource;
	int	idest;

	if( argc < 1 || argc > 3 ) {

		WRONG_PARAM_COUNT;
	}

	if( argc == 1 &&
	    zend_parse_parameters( argc TSRMLS_CC, "a", &tr_array )
	    == FAILURE) {

		return;

	} else if( argc == 2 &&
	    zend_parse_parameters( argc TSRMLS_CC, "al", &tr_array,
	    &i0 ) == FAILURE) {

		return;

	} else if( argc == 3 &&
	    zend_parse_parameters( argc TSRMLS_CC, "all", &tr_array,
	    &i0, &nsamp_requested ) == FAILURE) {

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

	dbgetv( tr, 0, "nsamp", &nsamp_available, "data", &data, NULL );

	if( nsamp_available == 0 || data == NULL ) {
	
		zend_error( E_ERROR, 
			"trextract_data: no data in trace object\n" );
	}

	array_init( return_value );

	nsamp_retrieve = nsamp_available - i0;

	if( nsamp_retrieve < nsamp_requested ) {
		
		zend_error( E_WARNING, "trdata: requested more samples than "
			"are available in this row; truncating returned "
			"array\n" );

	} else if( nsamp_requested != -1 ) {

		nsamp_retrieve = nsamp_requested;
	}
		
	for( idest = 0, isource = i0;
		idest < nsamp_retrieve; 
			idest++, isource++ ) {
		
		add_index_double( return_value, idest, (double) data[isource] );
	}

	return;
}
/* }}} */

/* {{{ proto array trdatabins( array tr, int binsize, [, int i0 [, int npts]] ) */
PHP_FUNCTION(trdatabins)
{
	zval	*tr_array;
	Dbptr	tr;
	float	*data = NULL;
	int	argc = ZEND_NUM_ARGS();
	int 	single_row = 0;
	long	nrecs;
	long	nsamp_retrieve = 0;
	long	nsamp_available = 0;
	long 	nsamp_requested = -1;
	long	binsize = 1;
	long	i0 = 0;
	double	min;
	double	max;
	long	isource;
	long	idest;

	if( argc < 2 || argc > 4 ) {

		WRONG_PARAM_COUNT;
	}

	if( argc == 2 &&
	    zend_parse_parameters( argc TSRMLS_CC, "al", &tr_array, &binsize )
	    == FAILURE) {

		return;

	} else if( argc == 3 &&
	    zend_parse_parameters( argc TSRMLS_CC, "all", &tr_array,
	    &binsize, &i0 ) == FAILURE) {

		return;

	} else if( argc == 4 &&
	    zend_parse_parameters( argc TSRMLS_CC, "alll", &tr_array,
	    &binsize, &i0, &nsamp_requested ) == FAILURE) {

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

	dbgetv( tr, 0, "nsamp", &nsamp_available, "data", &data, NULL );

	if( nsamp_available == 0 || data == NULL ) {
	
		zend_error( E_ERROR, 
			"trextract_data: no data in trace object\n" );
	}

	array_init( return_value );

	nsamp_retrieve = nsamp_available - i0;

	if( nsamp_retrieve < nsamp_requested ) {
		
		zend_error( E_WARNING, "trdatabins: requested more samples "
			"than are available in this row; truncating returned "
			"array\n" );

	} else if( nsamp_requested != -1 ) {

		nsamp_retrieve = nsamp_requested;
	}

	idest = 0;

	for( isource = i0; isource < i0 + nsamp_retrieve; isource++ ) {
		
		if( ( isource - i0 ) % binsize == 0 ) {
			
			if( isource != i0 ) {

				add_index_double( return_value, idest++, min );
				add_index_double( return_value, idest++, max );
			}

			min = max = (double) data[isource];
		}

		if( max < (double) data[isource] ) {
			
			max = (double) data[isource];
		}

		if( min > (double) data[isource] ) {
			
			min = (double) data[isource];
		}
	}

	add_index_double( return_value, idest++, min );
	add_index_double( return_value, idest++, max );

	return;
}
/* }}} */

/* {{{ proto array trextract_data( array tr ) */
PHP_FUNCTION(trextract_data)
{
	zval	*tr_array;
	Dbptr	tr;
	int	argc = ZEND_NUM_ARGS();
	int 	single_row = 0;
	long	nrecs;
	long	nsamp = 0;
	float	*data = NULL;
	long	i;

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

	dbgetv( tr, 0, "nsamp", &nsamp, "data", &data, NULL );

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

/* {{{ proto array trsplit( array tr ) */
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

/* {{{ proto array trsplice( array tr ) */
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

/* {{{ proto void trdestroy( array tr ) */
PHP_FUNCTION(trdestroy)
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

	trdestroy( &tr );

	return;
}
/* }}} */

/* {{{ proto array trfilter( array tr, string filter ) */
PHP_FUNCTION(trfilter)
{
	zval	*tr_array;
	Dbptr	tr;
	char	*filter;
	int	filter_len;
	int	argc = ZEND_NUM_ARGS();
	int	rc;

	if( argc != 2 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "as", 
			&tr_array, &filter, &filter_len )
	    == FAILURE) {

		return;

	} else if( z_arrval_to_dbptr( tr_array, &tr ) < 0 ) {

		return;
	}

	rc = trfilter( tr, filter );

	RETURN_LONG( rc );
}
/* }}} */

/* {{{ proto array trloadchan( array db, double t0, double t1, string sta, string chan ) */
PHP_FUNCTION(trloadchan)
{
	zval	***args;
	int	argc = ZEND_NUM_ARGS();
	Dbptr	db;
	Dbptr	tr;
	double	t0;
	double	t1;
	char	*t0_string = 0;
	char	*t1_string = 0;
	char	*sta = 0;
	char	*chan = 0;

	if( argc != 5 ) {

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
		zend_error( E_ERROR, "Error reading dbpointer\n" );
		return;
	}	

	if( Z_TYPE_PP( args[1] ) == IS_DOUBLE ) {

		t0 = Z_DVAL_PP( args[1] );

	} else if( Z_TYPE_PP( args[1] ) == IS_STRING ) {

		t0_string = Z_STRVAL_PP( args[1] );

		if( dbstrtype( db, t0_string ) == strTIME ) {
			
			t0 = str2epoch( t0_string );

		} else {

			efree( args );

			zend_error( E_ERROR, "Error reading value for t0: must "
			   "be a double-precision epoch time or a string "
			   "interpretable by str2epoch(3)\n" );

			return;
		}

	} else {

		efree( args );

		zend_error( E_ERROR, "Error reading value for t0: must be a "
		   "double-precision epoch time or a string interpretable by "
		   "str2epoch(3)\n" );

		return;
	}

	if( Z_TYPE_PP( args[2] ) == IS_DOUBLE ) {

		t1 = Z_DVAL_PP( args[2] );

	} else if( Z_TYPE_PP( args[2] ) == IS_STRING ) {

		t1_string = Z_STRVAL_PP( args[2] );

		if( dbstrtype( db, t1_string ) == strTIME ) {
			
			t1 = str2epoch( t1_string );

		} else {

			efree( args );

			zend_error( E_ERROR, "Error reading value for t1: must "
			   "be a double-precision epoch time or a string "
			   "interpretable by str2epoch(3)\n" );

			return;
		}

	} else {

		efree( args );

		zend_error( E_ERROR, "Error reading value for t1: must be a "
		   "double-precision epoch time or a string interpretable by "
		   "str2epoch(3)\n" );

		return;
	}

	if( Z_TYPE_PP( args[3] ) != IS_STRING ) {

		efree( args );
		zend_error( E_ERROR, "station-name must be string value\n" );
		return;

	} else {

		sta = Z_STRVAL_PP( args[3] );
	}

	if( Z_TYPE_PP( args[4] ) != IS_STRING ) {

		efree( args );
		zend_error( E_ERROR, "channel-name must be string value\n" );
		return;

	} else {

		chan = Z_STRVAL_PP( args[4] );
	}

	tr = trloadchan( db, t0, t1, sta, chan );

	efree( args );

	RETURN_DBPTR( tr );
}
/* }}} */

/* {{{ proto array trsample( array db, double t0, double t1, string sta, string chan [, int apply_calib] ) */
PHP_FUNCTION(trsample)
{
	zval	***args;
	int	argc = ZEND_NUM_ARGS();
	Dbptr	db;
	Dbptr	tr;
	double	t0;
	double	t1;
	char	*t0_string = 0;
	char	*t1_string = 0;
	char	*sta = 0;
	char	*chan = 0;
	int	apply_calib = 0;
	long	nrecs;
	long	nsamp;
	long	itrace;
	int	ireturn = 0;
	double	time;
	double	samprate;
	float	*data;

	if( argc < 5 || argc > 6 ) {

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
		zend_error( E_ERROR, "Error reading dbpointer\n" );
		return;
	}	

	if( Z_TYPE_PP( args[1] ) == IS_DOUBLE ) {

		t0 = Z_DVAL_PP( args[1] );

	} else if( Z_TYPE_PP( args[1] ) == IS_STRING ) {

		t0_string = Z_STRVAL_PP( args[1] );

		if( dbstrtype( db, t0_string ) == strTIME ) {
			
			t0 = str2epoch( t0_string );

		} else {

			efree( args );

			zend_error( E_ERROR, "Error reading value for t0: must "
			   "be a double-precision epoch time or a string "
			   "interpretable by str2epoch(3)\n" );

			return;
		}

	} else {

		efree( args );

		zend_error( E_ERROR, "Error reading value for t0: must be a "
		   "double-precision epoch time or a string interpretable by "
		   "str2epoch(3)\n" );

		return;
	}

	if( Z_TYPE_PP( args[2] ) == IS_DOUBLE ) {

		t1 = Z_DVAL_PP( args[2] );

	} else if( Z_TYPE_PP( args[2] ) == IS_STRING ) {

		t1_string = Z_STRVAL_PP( args[2] );

		if( dbstrtype( db, t1_string ) == strTIME ) {
			
			t1 = str2epoch( t1_string );

		} else {

			efree( args );

			zend_error( E_ERROR, "Error reading value for t1: must "
			   "be a double-precision epoch time or a string "
			   "interpretable by str2epoch(3)\n" );

			return;
		}

	} else {

		efree( args );

		zend_error( E_ERROR, "Error reading value for t1: must be a "
		   "double-precision epoch time or a string interpretable by "
		   "str2epoch(3)\n" );

		return;
	}

	if( Z_TYPE_PP( args[3] ) != IS_STRING ) {

		efree( args );
		zend_error( E_ERROR, "station-name must be string value\n" );
		return;

	} else {

		sta = Z_STRVAL_PP( args[3] );
	}

	if( Z_TYPE_PP( args[4] ) != IS_STRING ) {

		efree( args );
		zend_error( E_ERROR, "channel-name must be string value\n" );
		return;

	} else {

		chan = Z_STRVAL_PP( args[4] );
	}

	if( argc == 6 ) {

		if( Z_TYPE_PP( args[5] ) != IS_LONG ) {

			efree( args );
			zend_error( E_ERROR, 
				"apply_calib must be an integer value\n" );
			return;

		} else {

			apply_calib = Z_LVAL_PP( args[5] );
		}
	}

	tr = trloadchan( db, t0, t1, sta, chan );

	if( apply_calib ) {
		
		trapply_calib( tr );
	}

	array_init( return_value );

	dbquery( tr, dbRECORD_COUNT, &nrecs );

	for( tr.record = 0; tr.record < nrecs; tr.record++ ) {

		dbgetv( tr, 0, "time", &time, 
			       "nsamp", &nsamp,
			       "samprate", &samprate,
			       "data", &data,
			       NULL );

		for( itrace = 0; itrace < nsamp; itrace++ ) {

			add_index_double( return_value, 
					  ireturn++, 
					  SAMP2TIME( time, samprate, itrace ) );

			add_index_double( return_value, 
					  ireturn++, 
					  (double) data[itrace] );
		}
	}

	trdestroy( &tr );

	efree( args );

	return;
}
/* }}} */

/* {{{ proto array trsamplebins( array db, double t0, double t1, string sta, string chan, int binsize [, int apply_calib] ) */
PHP_FUNCTION(trsamplebins)
{
	zval	***args;
	int	argc = ZEND_NUM_ARGS();
	Dbptr	db;
	Dbptr	tr;
	double	t0;
	double	t1;
	long	binsize = 1;
	char	*t0_string = 0;
	char	*t1_string = 0;
	char	*sta = 0;
	char	*chan = 0;
	int	apply_calib = 0;
	long	nrecs;
	long	nsamp;
	long	itrace;
	int	ireturn = 0;
	double	time;
	double	samprate;
	float	*data;
	double	min;
	double	max;

	if( argc < 6 || argc > 7 ) {

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
		zend_error( E_ERROR, "Error reading dbpointer\n" );
		return;
	}	

	if( Z_TYPE_PP( args[1] ) == IS_DOUBLE ) {

		t0 = Z_DVAL_PP( args[1] );

	} else if( Z_TYPE_PP( args[1] ) == IS_STRING ) {

		t0_string = Z_STRVAL_PP( args[1] );

		if( dbstrtype( db, t0_string ) == strTIME ) {
			
			t0 = str2epoch( t0_string );

		} else {

			efree( args );

			zend_error( E_ERROR, "Error reading value for t0: must "
			   "be a double-precision epoch time or a string "
			   "interpretable by str2epoch(3)\n" );

			return;
		}

	} else {

		efree( args );

		zend_error( E_ERROR, "Error reading value for t0: must be a "
		   "double-precision epoch time or a string interpretable by "
		   "str2epoch(3)\n" );

		return;
	}

	if( Z_TYPE_PP( args[2] ) == IS_DOUBLE ) {

		t1 = Z_DVAL_PP( args[2] );

	} else if( Z_TYPE_PP( args[2] ) == IS_STRING ) {

		t1_string = Z_STRVAL_PP( args[2] );

		if( dbstrtype( db, t1_string ) == strTIME ) {
			
			t1 = str2epoch( t1_string );

		} else {

			efree( args );

			zend_error( E_ERROR, "Error reading value for t1: must "
			   "be a double-precision epoch time or a string "
			   "interpretable by str2epoch(3)\n" );

			return;
		}

	} else {

		efree( args );

		zend_error( E_ERROR, "Error reading value for t1: must be a "
		   "double-precision epoch time or a string interpretable by "
		   "str2epoch(3)\n" );

		return;
	}

	if( Z_TYPE_PP( args[3] ) != IS_STRING ) {

		efree( args );
		zend_error( E_ERROR, "station-name must be string value\n" );
		return;

	} else {

		sta = Z_STRVAL_PP( args[3] );
	}

	if( Z_TYPE_PP( args[4] ) != IS_STRING ) {

		efree( args );
		zend_error( E_ERROR, "channel-name must be string value\n" );
		return;

	} else {

		chan = Z_STRVAL_PP( args[4] );
	}

	if( Z_TYPE_PP( args[5] ) != IS_LONG ) {

		efree( args );
		zend_error( E_ERROR, "binsize must be integer value\n" );
		return;

	} else {

		binsize = Z_LVAL_PP( args[5] );
	}

	if( argc == 7 ) {

		if( Z_TYPE_PP( args[6] ) != IS_LONG ) {

			efree( args );
			zend_error( E_ERROR, 
				"apply_calib must be an integer value\n" );
			return;

		} else {

			apply_calib = Z_LVAL_PP( args[6] );
		}
	}

	tr = trloadchan( db, t0, t1, sta, chan );

	if( apply_calib ) {
		
		trapply_calib( tr );
	}

	array_init( return_value );

	dbquery( tr, dbRECORD_COUNT, &nrecs );

	for( tr.record = 0; tr.record < nrecs; tr.record++ ) {

		dbgetv( tr, 0, "time", &time, 
			       "nsamp", &nsamp,
			       "samprate", &samprate,
			       "data", &data,
			       NULL );

		for( itrace = 0; itrace < nsamp; itrace++ ) {

			if( itrace % binsize == 0 ) {

				if( itrace != 0 ) {

					add_index_double( return_value, 
					  		  ireturn++, 
					  		  min );

					add_index_double( return_value, 
					  		  ireturn++, 
					  		  max );
				}

				add_index_double( return_value, 
					  	ireturn++, 
					  	SAMP2TIME( time, 
							   samprate, 
							   itrace ) );

				min = max = (double) data[itrace];
			}

			if( max < (double) data[itrace] ) {

				max = (double) data[itrace];
			}

			if( min > (double) data[itrace] ) {

				min = (double) data[itrace];
			}
		}

		add_index_double( return_value, ireturn++, min );
		add_index_double( return_value, ireturn++, max );
	}

	trdestroy( &tr );

	efree( args );

	return;
}
/* }}} */

/* {{{ proto array dblist2subset( array db [, array list] ) */
PHP_FUNCTION(dblist2subset)
{
	zval	*db_array;
	zval	*list_array;
	Dbptr	db;
	Tbl	*list = 0;
	int	argc = ZEND_NUM_ARGS();

	if( argc < 1 || argc > 2 ) {

		WRONG_PARAM_COUNT;

	} else if( argc == 2 ) {

		if( zend_parse_parameters( argc TSRMLS_CC, "aa", 
					&db_array, &list_array )
	    	== FAILURE) {

			return;
		}

		if( z_arrval_to_dbptr( db_array, &db ) < 0 ) {

			return;
		}

		if( z_arrval_to_inttbl( list_array, &list ) < 0 ) {

			return;
		}

	} else {

		if( zend_parse_parameters( argc TSRMLS_CC, "a", 
						&db_array )
	    	== FAILURE) {

			return;
		}

		if( z_arrval_to_dbptr( db_array, &db ) < 0 ) {

			return;
		}

		list = (Tbl *) NULL;
	}

	db = dblist2subset( db, list );

	if( list ) {
		
		freetbl( list, 0 );
	}

	RETURN_DBPTR( db );
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

/* {{{ proto mixed dbget( array db [, int scratch] ) */
PHP_FUNCTION(dbget)
{
	zval	*db_array;
	Dbptr	db;
	char	*item = 0;
	long	item_size = 0;
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

/* {{{ proto int dbcompile( array db, string element ) */
PHP_FUNCTION(dbcompile)
{
	zval	*db_array;
	Dbptr	db;
	char	*element;
	int	element_len;
	int	argc = ZEND_NUM_ARGS();
	int	rc;

	if( argc != 2 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "as", 
					&db_array, &element, &element_len )
	    == FAILURE) {

		return;

	} else if( z_arrval_to_dbptr( db_array, &db ) < 0 ) {

		return;
	}

	rc = dbcompile( db, element );

	RETURN_LONG( rc );
}
/* }}} */

/* {{{ proto int dbadd_remark( array db, string remark ) */
PHP_FUNCTION(dbadd_remark)
{
	zval	*db_array;
	Dbptr	db;
	char	*remark;
	int	remark_len;
	int	argc = ZEND_NUM_ARGS();
	int	rc;

	if( argc != 2 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "as", 
					&db_array, &remark, &remark_len )
	    == FAILURE) {

		return;

	} else if( z_arrval_to_dbptr( db_array, &db ) < 0 ) {

		return;
	}

	rc = dbadd_remark( db, remark );

	RETURN_LONG( rc );
}
/* }}} */

/* {{{ proto string dbget_remark( array db ) */
PHP_FUNCTION(dbget_remark)
{
	zval	*db_array;
	Dbptr	db;
	char	*remark = 0;
	char	*remark_safe_copy;
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

	rc = dbget_remark( db, &remark );

	if( rc == 0 ) {

		remark_safe_copy = estrdup( remark );		

		free( remark );

		RETURN_STRING( remark_safe_copy, 0 );

	} else {

		return;
	}
}
/* }}} */

/* {{{ proto string dbstrtype( array db, string value ) */
PHP_FUNCTION(dbstrtype)
{
	zval	*db_array;
	Dbptr	db;
	char	*value;
	int	value_len;
	int	argc = ZEND_NUM_ARGS();
	int	type;

	if( argc != 2 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "as", 
					&db_array, &value, &value_len )
	    == FAILURE) {

		return;

	} else if( z_arrval_to_dbptr( db_array, &db ) < 0 ) {

		return;
	}

	type = dbstrtype( db, value );

	switch( type ) {
	case strREAL:
		RETURN_STRING( "strREAL", 1 );
		break;
	case strINTEGER:
		RETURN_STRING( "strINTEGER", 1 );
		break;
	case strNULL:
		RETURN_STRING( "strNULL", 1 );
		break;
	case strSTRING:
		RETURN_STRING( "strSTRING", 1 );
		break;
	case strTIME:
		RETURN_STRING( "strTIME", 1 );
		break;
	case strFIELD:
		RETURN_STRING( "strFIELD", 1 );
		break;
	default:
		RETURN_STRING( "strUNKNOWN", 1 );
		break;
	}
}
/* }}} */

/* {{{ proto int dbnextid( array db, string id_name ) */
PHP_FUNCTION(dbnextid)
{
	zval	*db_array;
	Dbptr	db;
	char	*id_name;
	int	id_name_len;
	int	argc = ZEND_NUM_ARGS();
	int	id;

	if( argc != 2 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "as", 
					&db_array, &id_name, &id_name_len )
	    == FAILURE) {

		return;

	} else if( z_arrval_to_dbptr( db_array, &db ) < 0 ) {

		return;
	}

	id = dbnextid( db, id_name );

	RETURN_LONG( id );
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
		zend_error( E_ERROR, "%s", errmsg );
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
		zend_error( E_ERROR, "%s", errmsg );
	}

	rc = dbread_view( fpview, &db, name );

	elog_clear_register( 1 );

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
			zend_error( E_ERROR, 
			"The fields argument must not be a non-empty list\n" );
		}
	}

	if( argc >= 6 ) {
	
		if( z_arrval_to_strtbl( *args[5], &expressions ) < 0 ) {

			efree( args );

			if( fields ) {
				freetbl( fields, free );
			}
			zend_error( E_ERROR, 
			"The expressions argument must not be a non-empty list\n" );
		}
	}

	if( argc >= 2 ) {

		efree( args );
	}

	rc = db2xml( db, rootnode, rownode, fields, expressions,
		     (void **) &xml, flags );

	if( fields ) {
		
		freetbl( fields, free );
	}

	if( expressions ) {
		
		freetbl( expressions, free );
	}

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

/* {{{ proto int dbfind( array db, string expression, [, int first [, int reverse]] ) */
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

/* {{{ proto mixed dbquery( array db, mixed dbcode ) */
PHP_FUNCTION(dbquery)
{
	zval	*db_array;
	zval	*zval_dbcode;
	Dbptr	db;
	int	argc = ZEND_NUM_ARGS();
	char	errmsg[STRSZ];
	char	*dbstring_code;
	int	dbcode;
	Dbvalue value;
	int	retcode;
	int	i;

	if( argc != 2 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "az", 
					&db_array,
					&zval_dbcode )
	    == FAILURE) {

		return;

	} else if( z_arrval_to_dbptr( db_array, &db ) < 0 ) {

		return;
	}

	if( Z_TYPE_P( zval_dbcode ) == IS_LONG ) {

		dbcode = Z_LVAL_P( zval_dbcode );

	}  else if( Z_TYPE_P( zval_dbcode ) == IS_STRING ) {

		dbstring_code = Z_STRVAL_P( zval_dbcode );

		dbcode = xlatname( dbstring_code, Dbxlat, NDbxlat );

	} else {

		zend_error( E_ERROR, 
			"second argument to dbquery() not understood\n" );
	}


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
	case dbSCHEMA_DEFAULT:
	case dbDATABASE_FILENAME:
	case dbIDSERVER:
	case dbLOCKS:
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
	case dbTABLE_IS_TRANSIENT:
	case dbDATABASE_IS_WRITABLE:
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
		sprintf( errmsg, "dbquery: bad code '%d'", dbcode );
		zend_error( E_ERROR, "%s", errmsg );
		break ;
	}
}
/* }}} */

/* {{{ proto int dbtruncate( array db, string expression ) */
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

/* {{{ proto array dbbase( array db, string tablename ) */
PHP_FUNCTION(dbbase)
{
	zval	*db_array;
	Dbptr	db;
	int	argc = ZEND_NUM_ARGS();
	char	*tablename;
	int	tablename_len;

	if( argc != 2 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "as", 
					&db_array,
					&tablename, &tablename_len )
	    == FAILURE) {

		return;

	} else if( z_arrval_to_dbptr( db_array, &db ) < 0 ) {

		return;
	}

	db = dbbase( db, tablename );

	RETURN_DBPTR( db );
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

	db = dbsubset( db, expr, NULL );

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

/* {{{ proto int dbunjoin( array db, string database ) */
PHP_FUNCTION(dbunjoin)
{
	zval	*db_array;
	Dbptr	db;
	int	argc = ZEND_NUM_ARGS();
	char	*database;
	int	database_len;
	int	retcode;

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
		zend_error( E_WARNING, "%s", warning );
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

	groupfields = strtbl( key, NULL );

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
	char	*key = NULL;
	Tbl	*pattern1 = NULL;
	Tbl	*pattern2 = NULL;
	Tbl	*halves = NULL;
	int	outer = 0;
	int	patterns_allocated = 0;
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

		patterns_allocated = 1;

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

			freetbl( halves, NULL );

			free( key );
		}
	}

	db = dbjoin( db1, db2, &pattern1, &pattern2, outer, NULL, NULL );

	if( pattern1 != (Tbl *) NULL ) {

		if( patterns_allocated ) {

			freetbl( pattern1, free );

		} else {

			freetbl( pattern1, NULL );
		}
	}

	if( pattern2 != (Tbl *) NULL ) {
	
		if( patterns_allocated ) {

			freetbl( pattern2, free );

		} else {

			freetbl( pattern2, NULL );
		}
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

/* {{{ proto array dbmatches( array db1, array db2, string hookname [, string key, ...] ) */
PHP_FUNCTION(dbmatches)
{
	int	argc = ZEND_NUM_ARGS();
	zval	***args;
	Dbptr	dbk;
	Dbptr	dbt;
	char	*hookname = 0;
	char	*key = 0;
	Tbl	*kpattern = 0;
	Tbl	*tpattern = 0;
	Tbl	*halves = 0;
	Tbl	*matches = 0;
	Hook	*hook = 0;
	int	hook_is_new;
	int	rc;
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

	} else if( z_arrval_to_dbptr( *args[0], &dbk ) < 0 ) {

		efree( args );
		zend_error( E_ERROR, "Error reading first dbpointer\n" );
		return;
	}	

	if( Z_TYPE_PP( args[1] ) != IS_ARRAY ) {

		efree( args );
		zend_error( E_ERROR, "Error reading second dbpointer\n" );

	} else if( z_arrval_to_dbptr( *args[1], &dbt ) < 0 ) {

		efree( args );
		zend_error( E_ERROR, "Error reading second dbpointer\n" );
		return;
	}	

	if( Z_TYPE_PP( args[2] ) != IS_STRING ) {

		efree( args );
		zend_error( E_ERROR,
			"dbmatches hookname must be a string value\n" );
		return;
	}

	hookname = Z_STRVAL_PP( args[2] );

	if( Hooks == (Arr *) NULL ) {
		
		Hooks = newarr( 0 );
	}

	if( ( hook = getarr( Hooks, hookname ) ) == 0 ) {

		hook_is_new = 1;

	} else {

		hook_is_new = 0;
	}

	for( i = 3; i < argc; i++ ) {

		if( Z_TYPE_PP( args[i] ) != IS_STRING ) {

			efree( args );
			zend_error( E_ERROR, "dbmatches join-keys must be string values\n" );
			return;
		}

		key = Z_STRVAL_PP( args[i] );

		if( kpattern == (Tbl *) NULL ) {

			kpattern = newtbl( 0 );
		}

		if( tpattern == (Tbl *) NULL ) {

			tpattern = newtbl( 0 );
		}

		if( strchr( key, '#' ) == 0 ) {

			pushtbl( kpattern, strdup( key ) );
			pushtbl( tpattern, strdup( key ) );

		} else {
			
			key = strdup( key );

			halves = split( key, '#' );

			pushtbl( kpattern, strdup( shifttbl( halves ) ) );
			pushtbl( tpattern, strdup( poptbl( halves ) ) );

			freetbl( halves, 0 );

			free( key );
		}
	}

	rc = dbmatches( dbk, dbt, &kpattern, &tpattern, &hook, &matches );

	if( rc < 0 ) {

		efree( args );

		zend_error( E_ERROR, "dbmatches failed!\n" );

		return;
	}

	if( hook_is_new && hook != (Hook *) NULL ) {
		
		setarr( Hooks, hookname, hook );
	}

	array_init( return_value );

	for( i = 0; i < maxtbl( matches ); i++ ) {
		
		add_index_long( return_value, i, (long) gettbl( matches, i ) );
	}

	if( kpattern != (Tbl *) NULL ) {
	
		freetbl( kpattern, free );
	}

	if( tpattern != (Tbl *) NULL ) {
	
		freetbl( tpattern, free );
	}

	efree( args );

	freetbl( matches, 0 );

	return;
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

/* {{{ proto int dbfree( array db ) */
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

/* {{{ proto int ds_dbclose( array db ) */
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

/* {{{ proto int dbdestroy( array db ) */
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

/* {{{ proto int dbaddnull( array db ) */
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

/* {{{ proto array dbget_range( array db ) */
PHP_FUNCTION(dbget_range)
{
	zval	*db_array;
	Dbptr	db;
	long	first;
	long	last;
	int	argc = ZEND_NUM_ARGS();

	if( argc != 1 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "a", &db_array ) == FAILURE) {

		return;

	} else if( z_arrval_to_dbptr( db_array, &db ) < 0 ) {

		return;
	}

	dbget_range( db, &first, &last );

	array_init( return_value );

	add_next_index_long( return_value, first );
	add_next_index_long( return_value, last );

	return;
}
/* }}} */

/* {{{ proto int dbnrecs( array db ) */
PHP_FUNCTION(dbnrecs)
{
	zval	*db_array;
	Dbptr	db;
	int	argc = ZEND_NUM_ARGS();
	long	nrecs;

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


/* {{{ proto int dbputv( array db, string field_name, mixed value, [, field_name, value, ... ] ) */
PHP_FUNCTION(dbputv)
{
	zval	*db_array_in;
	Dbptr	db;
	zval	***args;
	long	type;
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

		db = dblookup( db, NULL, NULL, field_name, NULL );

		rc = dbquery( db, dbFIELD_TYPE, &type );

		if( rc == dbINVALID ) {
			
			zend_error( E_ERROR, "Error getting fieldtype\n" );
		}

		if( zval_to_dbvalue( args[fieldval_index], type, &value ) < 0 ) {

			zend_error( E_ERROR, "Error getting fieldvalue\n" );
		}

		switch( type ) {
		case dbDBPTR:
			retcode |= dbputv( db, 0, field_name, value.db, NULL );
			break;
		case dbSTRING:
			retcode |= dbputv( db, 0, field_name, value.s, NULL );
			break;
		case dbBOOLEAN:
		case dbINTEGER:
		case dbYEARDAY:
			retcode |= dbputv( db, 0, field_name, value.i, NULL );
			break;
		case dbREAL:
		case dbTIME:
			retcode |= dbputv( db, 0, field_name, value.d, NULL );
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

/* {{{ proto int dbaddv( array db, string field_name, mixed value, [, field_name, value, ... ] ) */
PHP_FUNCTION(dbaddv)
{
	zval	*db_array_in;
	Dbptr	db;
	zval	***args;
	long	type;
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

		db = dblookup( db, NULL, NULL, field_name, NULL );

		rc = dbquery( db, dbFIELD_TYPE, &type );

		if( rc == dbINVALID ) {
			
			zend_error( E_ERROR, "Error getting fieldtype\n" );
		}

		if( zval_to_dbvalue( args[fieldval_index], type, &value ) < 0 ) {

			zend_error( E_ERROR, "Error getting fieldvalue\n" );
		}

		switch( type ) {
		case dbDBPTR:
			retcode |= dbputv( db, 0, field_name, value.db, NULL );
			break;
		case dbSTRING:
			retcode |= dbputv( db, 0, field_name, value.s, NULL );
			break;
		case dbBOOLEAN:
		case dbINTEGER:
		case dbYEARDAY:
			retcode |= dbputv( db, 0, field_name, value.i, NULL );
			break;
		case dbREAL:
		case dbTIME:
			retcode |= dbputv( db, 0, field_name, value.d, NULL );
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
		elog_clear_register( 1 );
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
	zval	***args = NULL;
	long	type;
	Dbvalue	value;
	int	argc = ZEND_NUM_ARGS();
	int	single = 0;
	int	i;
	int	array_mode = 0;
	char	warning[STRSZ];
	char	*fieldname = NULL;
	Tbl	*fieldnames = 0;
	int	loopstart;
	int	loopmax;

	if( argc < 2 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( 1 TSRMLS_CC, "a", &db_array_in ) == FAILURE ) {

		return;

	} else if( z_arrval_to_dbptr( db_array_in, &db ) < 0 ) {

		return;
	}

	args = (zval ***) emalloc( argc * sizeof(zval **) );

	if( zend_get_parameters_array_ex( argc, args ) == FAILURE ) {

		if( args ) {

			efree( args );
		}

		return;
	}

	if( Z_TYPE_PP( args[1] ) == IS_ARRAY ) {

		array_mode = 1;

		array_init( return_value );

		single = 0;

		z_arrval_to_strtbl( *args[1], &fieldnames );

		loopstart = 0;

		loopmax = maxtbl( fieldnames );

	} else if( argc > 2  ) {

		array_mode = 0;

		array_init( return_value );

		single = 0;

		loopstart = 1;

		loopmax = argc;

	} else {

		array_mode = 0;

		single = 1;

		loopstart = 1;

		loopmax = argc;
	}

	for( i = loopstart; i < loopmax; i++ ) {

		if( array_mode ) {

			fieldname = gettbl( fieldnames, i );

		} else {

			if( Z_TYPE_PP( args[i] ) != IS_STRING ) {

				if( ! single ) {

					efree( return_value );
				}

				efree( args );

				zend_error( E_ERROR, 
					"fieldnames must be strings\n" );

			} else {

				fieldname = Z_STRVAL_PP( args[i] );
			}
		}

		db = dblookup( db, NULL, NULL, fieldname, NULL );

		dbquery( db, dbFIELD_TYPE, &type );

		if( dbget( db, value.s ) < 0 )
		{
			if( ! single ) {

				efree( return_value );
			}

			efree( args );

			zend_error( E_ERROR, "dbgetv failed to retrieve value\n" );
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
				RETVAL_BOOL( atol( value.s ) );
			} else {
				add_next_index_bool( return_value, atol( value.s ) );
			}
			break;
		case dbINTEGER:
		case dbYEARDAY:
			copystrip( value.s, value.s, strlen( value.s ) );
			if( single ) {
				RETVAL_LONG( atol( value.s ) );
			} else {
				add_next_index_long( return_value, atol( value.s ) );
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
				"dbgetv can't interpret field of type %s",
				xlatnum( type, Dbxlat, NDbxlat ) );
			zend_error( E_WARNING, "%s", warning );
			break;
		}
	}

	if( fieldnames ) {

		freetbl( fieldnames, free );
	}

	efree( args );

	return;
}
/* }}} */

/* {{{ proto int dbadd( array db [, string record ] ) */
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

/* {{{ proto string dbextfile( array db [, string tablename ] ) */
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
