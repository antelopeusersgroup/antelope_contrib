/*
 * Antelope Orb interface for PHP
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
#include "Orb_php.h"
#include "stock.h"
#include "orb.h"
#include "Pkt.h"
#include "pf.h"
#include "pforbstat.h"

/* Prevent the deviants.h reassignment to std_now() from corrupting
 * the name of the PHP function */
#undef now

static int le_Orb;

static char *Elog_replacement = 0;

function_entry Orb_functions[] = {
	PHP_FE(orbopen, NULL)		
	PHP_FE(orbping, NULL)		
	PHP_FE(orbtell, NULL)		
	PHP_FE(orbposition, NULL)		
	PHP_FE(orbafter, NULL)		
	PHP_FE(orbseek, NULL)		
	PHP_FE(orbwait, NULL)		
	PHP_FE(orbclose, NULL)		
	PHP_FE(orbselect, NULL)		
	PHP_FE(orbreject, NULL)		
	PHP_FE(pforbstat, NULL)		
	PHP_FE(split_srcname, NULL)		
	{NULL, NULL, NULL}	
};

zend_module_entry Orb_module_entry = {
	STANDARD_MODULE_HEADER,
	"Orb",
	Orb_functions,
	PHP_MINIT(Orb),
	PHP_MSHUTDOWN(Orb),
	NULL,
	NULL,
	PHP_MINFO(Orb),
	"0.1",
	STANDARD_MODULE_PROPERTIES
};

ZEND_GET_MODULE(Orb)

void register_Orb_constants( INIT_FUNC_ARGS )
{
	int	i;

	for( i = 0; i < Orbxlatn; i++ ) {

		zend_register_long_constant( Orbxlat[i].name,
					     strlen( Orbxlat[i].name ) + 1, 
					     Orbxlat[i].num,
					     CONST_CS | CONST_PERSISTENT,
					     module_number TSRMLS_CC );
	}

	for( i = 0; i < Orbconstn; i++ ) {

		zend_register_long_constant( Orbconst[i].name,
					     strlen( Orbconst[i].name ) + 1, 
					     Orbconst[i].num,
					     CONST_CS | CONST_PERSISTENT,
					     module_number TSRMLS_CC );
	}
}

PHP_MINIT_FUNCTION(Orb)
{
	register_Orb_constants( INIT_FUNC_ARGS_PASSTHRU );

	REGISTER_LONG_CONSTANT( "PFORBSTAT_SERVER", 
				 PFORBSTAT_SERVER, 
				 CONST_CS | CONST_PERSISTENT );
				 
	REGISTER_LONG_CONSTANT( "PFORBSTAT_SOURCES", 
				 PFORBSTAT_SOURCES, 
				 CONST_CS | CONST_PERSISTENT );

	REGISTER_LONG_CONSTANT( "PFORBSTAT_CLIENTS", 
				 PFORBSTAT_CLIENTS, 
				 CONST_CS | CONST_PERSISTENT );

	REGISTER_LONG_CONSTANT( "PFORBSTAT_DATABASES", 
				 PFORBSTAT_DATABASES, 
				 CONST_CS | CONST_PERSISTENT );

	REGISTER_LONG_CONSTANT( "PFORBSTAT_CONNECTIONS", 
				 PFORBSTAT_CONNECTIONS, 
				 CONST_CS | CONST_PERSISTENT );

	return SUCCESS;
}

PHP_MSHUTDOWN_FUNCTION(Orb)
{
	return SUCCESS;
}

PHP_MINFO_FUNCTION(Orb)
{
	php_info_print_table_start();
	php_info_print_table_header(2, "Orb support", "enabled");
	php_info_print_table_end();
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

/* {{{ proto int orbopen( string name, string perm ) */
PHP_FUNCTION(orbopen)
{
	char	*orbname;
	int	orbname_len;
	char	*perm;
	int	perm_len;
	int	orbfd;
	int	argc = ZEND_NUM_ARGS();

	if( argc != 2 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "ss", 
					&orbname, &orbname_len,
					&perm, &perm_len )
	    == FAILURE) {

		return;
	}

	orbfd = orbopen( orbname, perm );

	RETURN_LONG( orbfd );
}
/* }}} */

/* {{{ proto int orbclose( int orbfd ) */
PHP_FUNCTION(orbclose)
{
	long	orbfd;
	int	rc;
	int	argc = ZEND_NUM_ARGS();

	if( argc != 1 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "l", &orbfd )
	    == FAILURE) {

		return;
	}
	
	rc = orbclose( (int) orbfd );

	RETURN_LONG( rc );
}
/* }}} */

/* {{{ proto int orbtell( int orbfd ) */
PHP_FUNCTION(orbtell)
{
	long	orbfd;
	int	pktid;
	int	argc = ZEND_NUM_ARGS();

	if( argc != 1 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "l", &orbfd )
	    == FAILURE) {

		return;
	}
	
	pktid = orbtell( (int) orbfd );

	RETURN_LONG( pktid );
}
/* }}} */

/* {{{ proto int orbping( int orbfd ) */
PHP_FUNCTION(orbping)
{
	long	orbfd;
	int	version;
	int	argc = ZEND_NUM_ARGS();

	if( argc != 1 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "l", &orbfd )
	    == FAILURE) {

		return;
	}
	
	orbping( (int) orbfd, &version );

	RETURN_LONG( version );
}
/* }}} */

/* {{{ proto int orbselect( int orbfd, string regex ) */
PHP_FUNCTION(orbselect)
{
	long	orbfd;
	char	*regex;
	int	regex_len;
	int	argc = ZEND_NUM_ARGS();
	int	rc;

	if( argc != 2 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "ls",
				 &orbfd, &regex, &regex_len )
	    == FAILURE) {

		return;
	}
	
	rc = orbselect( (int) orbfd, regex );

	RETURN_LONG( rc );
}
/* }}} */

/* {{{ proto int orbreject( int orbfd, string regex ) */
PHP_FUNCTION(orbreject)
{
	long	orbfd;
	char	*regex;
	int	regex_len;
	int	argc = ZEND_NUM_ARGS();
	int	rc;

	if( argc != 2 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "ls",
				 &orbfd, &regex, &regex_len )
	    == FAILURE) {

		return;
	}
	
	rc = orbreject( (int) orbfd, regex );

	RETURN_LONG( rc );
}
/* }}} */

/* {{{ proto int orbafter( int orbfd, double time ) */
PHP_FUNCTION(orbafter)
{
	long	orbfd;
	double	time;
	int	argc = ZEND_NUM_ARGS();
	int	pktid;

	if( argc != 2 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "ld",
				 &orbfd, &time )
	    == FAILURE) {

		return;
	}
	
	pktid = orbafter( (int) orbfd, time );

	RETURN_LONG( pktid );
}
/* }}} */

/* {{{ proto int orbposition( int orbfd, string from ) */
PHP_FUNCTION(orbposition)
{
	long	orbfd;
	char	*from;
	int	from_len;
	int	argc = ZEND_NUM_ARGS();
	int	pktid;

	if( argc != 2 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "ls",
				 &orbfd, &from, &from_len )
	    == FAILURE) {

		return;
	}
	
	pktid = orbposition( (int) orbfd, from );

	RETURN_LONG( pktid );
}
/* }}} */

/* {{{ proto int orbseek( int orbfd, mixed which ) */
PHP_FUNCTION(orbseek)
{
	long	orbfd;
	zval	*zval_which;
	int	which;
	int	argc = ZEND_NUM_ARGS();
	int	pktid;

	if( argc != 2 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "lz",
				 &orbfd, &zval_which )
	    == FAILURE) {

		return;
	}

	if( Z_TYPE_P( zval_which ) == IS_STRING ) {

		which = xlatname( Z_STRVAL_P( zval_which ), Orbconst, Orbconstn );

		if( which == -1 ) {

			zend_error( E_ERROR, 
		     	   "orbseek 'which' code not recognized" );

			return;
		}
	
	} else if( Z_TYPE_P( zval_which ) == IS_LONG ) {

		which = Z_LVAL_P( zval_which );

	} else {

		zend_error( E_ERROR, 
		     "orbseek argument must be string or integer" );

		return;
	}

	pktid = orbseek( (int) orbfd, which );

	RETURN_LONG( pktid );
}
/* }}} */

/* {{{ proto int orbwait( int orbfd, string re, double mintime, double timeout ) */
PHP_FUNCTION(orbwait)
{
	long	orbfd;
	char	*re;
	int	re_len;
	double	mintime;
	double	timeout;
	int	argc = ZEND_NUM_ARGS();
	int	rc;

	if( argc != 4 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "lsdd",
				 &orbfd, &re, &re_len,
				 &mintime, &timeout)
	    == FAILURE) {

		return;
	}
	
	rc = orbwait( (int) orbfd, re, mintime, timeout );

	RETURN_LONG( rc );
}
/* }}} */

/* {{{ proto int pforbstat( int orbfd, int flags ) */
PHP_FUNCTION(pforbstat)
{
	long	orbfd;
	long 	flags;
	int	argc = ZEND_NUM_ARGS();
	Pf 	*pf;

	if( argc != 2 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "ll", &orbfd, &flags )
	    == FAILURE) {

		return;
	}
	
	pf = pforbstat( (int) orbfd, (int) flags );

	if( pf != NULL ) {

		pf2zval( pf, return_value );
	
		pffree( pf );
	}

	return;
}
/* }}} */

/* {{{ proto array split_srcname( string srcname ) */
PHP_FUNCTION(split_srcname)
{
	char	*srcname;
	int	srcname_len;
	int	argc = ZEND_NUM_ARGS();
	Srcname parts;

	if( argc != 1 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "s", 
					&srcname, &srcname_len )
	    == FAILURE) {

		return;
	}

	split_srcname( srcname, &parts );

	array_init( return_value );

	add_assoc_string_ex( return_value, 
			     "net", strlen( "net" ) + 1, 
			     parts.src_net, 1 );

	add_assoc_string_ex( return_value, 
			     "sta", strlen( "sta" ) + 1, 
			     parts.src_sta, 1 );

	add_assoc_string_ex( return_value, 
			     "chan", strlen( "chan" ) + 1, 
			     parts.src_chan, 1 );

	add_assoc_string_ex( return_value, 
			     "loc", strlen( "loc" ) + 1, 
			     parts.src_loc, 1 );

	add_assoc_string_ex( return_value, 
			     "suffix", strlen( "suffix" ) + 1, 
			     parts.src_suffix, 1 );

	add_assoc_string_ex( return_value, 
			     "subcode", strlen( "subcode" ) + 1, 
			     parts.src_subcode, 1 );

	return;
}
/* }}} */


/* local variables
 * End:
 * vim600: fdm=marker
 */
