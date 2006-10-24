#ifndef PHP_ORB_H
#define PHP_ORB_H

#define PHP_ORB_API
#define PHP_ORB_EXTNAME "Orb"
#define PHP_ORB_EXTVER  "0.2"

#include "php.h"
#include "php_ini.h"
#include "ext/standard/info.h"

#ifdef ZTS
#include "TSRM.h"
#endif

#define ZVAL_DBPTR(ZVAL,DB) { \
		array_init( ZVAL ); \
		add_index_long( (ZVAL), 0, (DB).database ); \
		add_index_long( (ZVAL), 1, (DB).table ); \
		add_index_long( (ZVAL), 2, (DB).field ); \
		add_index_long( (ZVAL), 3, (DB).record ); \
	}

extern zend_module_entry Orb_module_entry;
#define phpext_Orb_ptr &Orb_module_entry

PHP_MINIT_FUNCTION(Orb);
PHP_MSHUTDOWN_FUNCTION(Orb);
PHP_MINFO_FUNCTION(Orb);
PHP_FUNCTION(orbopen);
PHP_FUNCTION(orbping);
PHP_FUNCTION(orbclose);
PHP_FUNCTION(orbtell);
PHP_FUNCTION(orbposition);
PHP_FUNCTION(orbafter);
PHP_FUNCTION(orbseek);
PHP_FUNCTION(orbwait);
PHP_FUNCTION(orbselect);
PHP_FUNCTION(orbreject);
PHP_FUNCTION(orbreap);
PHP_FUNCTION(orbreap_nd);
PHP_FUNCTION(orbreap_timeout);
PHP_FUNCTION(orbget);
PHP_FUNCTION(orbput);
PHP_FUNCTION(orbputx);
PHP_FUNCTION(orbstat);
PHP_FUNCTION(orbsources);
PHP_FUNCTION(orbclients);
PHP_FUNCTION(pforbstat);
PHP_FUNCTION(unstuffpkt);
PHP_FUNCTION(orbpkt_string);
PHP_FUNCTION(split_srcname);
PHP_FUNCTION(join_srcname);
PHP_FUNCTION(suffix2pkttype);

#ifdef ZTS
#define ORB_G(v) TSRMG(Orb_globals_id, zend_Orb_globals *, v)
#else
#define ORB_G(v) (Orb_globals.v)
#endif
 
#endif	/* PHP_ORB_H */
