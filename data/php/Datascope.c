#include "config.h"
#include "php.h"
#include "php_ini.h"
#include "ext/standard/info.h"
#include "php_Datascope.h"
#include "db.h"
#include "stock.h"
#include "coords.h"
#include "response.h"

static int le_Datascope;
static int le_dbresponse;

function_entry Datascope_functions[] = {
	PHP_FE(dbex_eval, NULL)		
	PHP_FE(dbextfile, NULL)		
	PHP_FE(dbfind, NULL)		
	PHP_FE(dbgetv, NULL)		
	PHP_FE(dbaddv, NULL)		
	PHP_FE(dbputv, NULL)		
	PHP_FE(dbaddnull, NULL)		
	PHP_FE(dbadd, NULL)		
	PHP_FE(dblookup, NULL)		
	PHP_FE(dbnrecs, NULL)		
	PHP_FE(ds_dbopen, NULL)		
	PHP_FE(dbfree, NULL)		
	PHP_FE(ds_dbclose, NULL)		
	PHP_FE(dbdestroy, NULL)		
	PHP_FE(dbtruncate, NULL)		
	PHP_FE(dbprocess, NULL)		
	PHP_FE(dbsubset, NULL)		
	PHP_FE(dbquery, NULL)		
	PHP_FE(dbresponse, NULL)		
	PHP_FE(strtdelta, NULL)		
	PHP_FE(eval_response, NULL)		
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

		return -1;

	} else if( zend_hash_num_elements( Z_ARRVAL_P( array ) ) != 4 ) {

		return -1;
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

		/* SCAFFOLD needs error message */

		return;
	}

	if( read_response( fp , &response ) != 0 ) {

		/* SCAFFOLD needs error message */

		fclose( fp );

		return;
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

			/* SCAFFOLD; need cleanup and error msg */
			return;

		} else {

			db.record = Z_LVAL_PP( args[2] );
		}

		if( argc == 4 ) {

			if( Z_TYPE_PP( args[3] ) != IS_LONG ) {

				/* SCAFFOLD; need cleanup and error msg */
				return;

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

/* {{{ proto array dbquery( array db, string code ) */
PHP_FUNCTION(dbquery)
{
	zval	*db_array;
	Dbptr	db;
	int	argc = ZEND_NUM_ARGS();
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
		}
                break;  

        case dbTABLE_IS_WRITEABLE:
        case dbTABLE_IS_VIEW:
        case dbTABLE_PRESENT:
		if( ( retcode = dbquery(db, dbcode, &value) ) >= 0 ) {

			RETVAL_BOOL( value.i );
		}
                break;  

        case dbFIELD_TYPE:
		if( ( retcode = dbquery(db, dbcode, &value) ) >= 0 ) {

			RETVAL_STRING( xlatnum( value.i, Dbxlat, NDbxlat ), 1 );
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

			RETVAL_STRING( "SCAFFOLD: problem\n", 1 );
		}
                break;  
 
        default:
		/* SCAFFOLD 
		sprintf( errmsg, "dbquery: bad code '%s'", dbstring_code );
		mxFree( dbstring_code );
		mexErrMsgTxt( errmsg );
		*/
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
		/* SCAFFOLD 
		sprintf( warning, 
			"Can't interpret field of type %s",
			xlatnum( type, Dbxlat, NDbxlat ) );
		mexWarnMsgTxt( warning );
		*/
		break;
	}
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

			/* SCAFFOLD; need cleanup and error msg */
			return;

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

		fprintf( stderr, "SCAFFOLD: Error parsing dbptr\n" );
		return;

	} else if( z_arrval_to_dbptr( db_array_in, &db ) < 0 ) {

		fprintf( stderr, "SCAFFOLD: Error interpreting dbptr\n" );
		return;
	}

	args = (zval ***) emalloc( argc * sizeof(zval **) );

	if( zend_get_parameters_array_ex( argc, args ) == FAILURE ) {

		efree( args );
		fprintf( stderr, "SCAFFOLD: Error getting params array\n" );
		return;
	}

	nfields = ( argc - 1 ) / 2;

	for( i = 0; i < nfields; i++ ) {

		fieldname_index = i * 2 + 1;
		fieldval_index = fieldname_index + 1;

		if( Z_TYPE_PP( args[fieldname_index] ) != IS_STRING ) {
			fprintf( stderr, "SCAFFOLD: Error getting fieldname\n" );
			return;
		}

		field_name = Z_STRVAL_PP( args[fieldname_index] );

		db = dblookup( db, 0, 0, field_name, 0 );

		rc = dbquery( db, dbFIELD_TYPE, &type );

		if( rc == dbINVALID ) {
			
			fprintf( stderr, "SCAFFOLD: Error getting fieldtype\n" );
			return;
		}

		if( zval_to_dbvalue( args[fieldval_index], type, &value ) < 0 ) {

			fprintf( stderr, "SCAFFOLD: Error getting fieldvalue\n" );
			return;
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

		fprintf( stderr, "SCAFFOLD: Error somewhere\n" );
		return;
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

		fprintf( stderr, "SCAFFOLD: Error parsing dbptr\n" );
		return;

	} else if( z_arrval_to_dbptr( db_array_in, &db ) < 0 ) {

		fprintf( stderr, "SCAFFOLD: Error interpreting dbptr\n" );
		return;
	}

	db.record = dbNULL;

	rc = dbget( db, 0 );

	if( rc == dbINVALID ) {
		
		fprintf( stderr, "SCAFFOLD: Error getting null record\n" );
		return;
	}

	db.record = dbSCRATCH;

	args = (zval ***) emalloc( argc * sizeof(zval **) );

	if( zend_get_parameters_array_ex( argc, args ) == FAILURE ) {

		efree( args );
		fprintf( stderr, "SCAFFOLD: Error getting params array\n" );
		return;
	}

	nfields = ( argc - 1 ) / 2;

	for( i = 0; i < nfields; i++ ) {

		fieldname_index = i * 2 + 1;
		fieldval_index = fieldname_index + 1;

		if( Z_TYPE_PP( args[fieldname_index] ) != IS_STRING ) {
			fprintf( stderr, "SCAFFOLD: Error getting fieldname\n" );
			return;
		}

		field_name = Z_STRVAL_PP( args[fieldname_index] );

		db = dblookup( db, 0, 0, field_name, 0 );

		rc = dbquery( db, dbFIELD_TYPE, &type );

		if( rc == dbINVALID ) {
			
			fprintf( stderr, "SCAFFOLD: Error getting fieldtype\n" );
			return;
		}

		if( zval_to_dbvalue( args[fieldval_index], type, &value ) < 0 ) {

			fprintf( stderr, "SCAFFOLD: Error getting fieldvalue\n" );
			return;
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
		fprintf( stderr, "SCAFFOLD: Error somewhere\n" );
		return;
	}

	retcode = dbaddchk( db, 0 );

	if( retcode < 0 ) {
		clear_register( 1 );
		fprintf( stderr, "SCAFFOLD: Error with addchk\n" );
		return;
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
			/* SCAFFOLD; need cleanup and error msg */
			return;
		}

		db = dblookup( db, 0, 0, Z_STRVAL_PP( args[i] ), 0 );

		dbquery( db, dbFIELD_TYPE, &type );

		if( dbget( db, value.s ) < 0 )
		{
			/* SCAFFOLD; need cleanup and error msg */
			return;
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
			/* SCAFFOLD 
			sprintf( warning, 
				"Can't interpret field of type %s",
				xlatnum( type, Dbxlat, NDbxlat ) );
			mexWarnMsgTxt( warning );
			*/
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

			/* SCAFFOLD; need error msg */
			efree( args );
			return;

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

			/* SCAFFOLD; need error msg */
			efree( args );
			return;

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

/* local variables
 * End:
 * vim600: fdm=marker
 */
