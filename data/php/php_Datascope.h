#ifndef PHP_DATASCOPE_H
#define PHP_DATASCOPE_H

extern zend_module_entry Datascope_module_entry;
#define phpext_Datascope_ptr &Datascope_module_entry

#define PHP_DATASCOPE_API

#ifdef ZTS
#include "TSRM.h"
#endif

PHP_MINIT_FUNCTION(Datascope);
PHP_MSHUTDOWN_FUNCTION(Datascope);
PHP_MINFO_FUNCTION(Datascope);
PHP_FUNCTION(dbopen);
PHP_FUNCTION(dblookup);
PHP_FUNCTION(dbgetv);
PHP_FUNCTION(dbnrecs);
PHP_FUNCTION(dbprocess);
PHP_FUNCTION(dbex_eval);
PHP_FUNCTION(dbquery);
PHP_FUNCTION(dbsubset);

#ifdef ZTS
#define DATASCOPE_G(v) TSRMG(Datascope_globals_id, zend_Datascope_globals *, v)
#else
#define DATASCOPE_G(v) (Datascope_globals.v)
#endif

#define ZVAL_DBPTR(ZVAL,DB) { \
		array_init( ZVAL ); \
		add_index_long( (ZVAL), 0, (DB).database ); \
		add_index_long( (ZVAL), 1, (DB).table ); \
		add_index_long( (ZVAL), 2, (DB).field ); \
		add_index_long( (ZVAL), 3, (DB).record ); \
	}

#define MAKE_DBPTR_ZVAL(ZVAL,DB) { MAKE_STD_ZVAL(ZVAL); ZVAL_DBPTR(ZVAL,DB) }

#define RETVAL_DBPTR(DB) ZVAL_DBPTR( return_value, (DB) )

#define RETURN_DBPTR(DB) { RETVAL_DBPTR(DB); return; }

#endif	/* PHP_DATASCOPE_H */
