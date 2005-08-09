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
PHP_FUNCTION(ds_dbopen);
PHP_FUNCTION(ds_dbopen_database);
PHP_FUNCTION(ds_dbopen_table);
PHP_FUNCTION(ds_dbclose);
PHP_FUNCTION(ds_dbtmp);
PHP_FUNCTION(ds_dbcreate);
PHP_FUNCTION(dbfree);
PHP_FUNCTION(dbdestroy);
PHP_FUNCTION(dbtruncate);
PHP_FUNCTION(dblookup);
PHP_FUNCTION(dbgetv);
PHP_FUNCTION(dbaddv);
PHP_FUNCTION(dbget);
PHP_FUNCTION(dbput);
PHP_FUNCTION(dbaddnull);
PHP_FUNCTION(dbadd);
PHP_FUNCTION(dbputv);
PHP_FUNCTION(dbnrecs);
PHP_FUNCTION(dbsort);
PHP_FUNCTION(dbgroup);
PHP_FUNCTION(dbungroup);
PHP_FUNCTION(dbjoin);
PHP_FUNCTION(dbnojoin);
PHP_FUNCTION(dbtheta);
PHP_FUNCTION(dbprocess);
PHP_FUNCTION(dbex_eval);
PHP_FUNCTION(dbextfile);
PHP_FUNCTION(dbfind);
PHP_FUNCTION(dbquery);
PHP_FUNCTION(dbresponse);
PHP_FUNCTION(dbseparate);
PHP_FUNCTION(dbsever);
PHP_FUNCTION(dbsubset);
PHP_FUNCTION(dbunjoin);
PHP_FUNCTION(db2xml);
PHP_FUNCTION(dbwrite_view);
PHP_FUNCTION(dbread_view);
PHP_FUNCTION(dbsave_view);
PHP_FUNCTION(dbmark);
PHP_FUNCTION(dbdelete);
PHP_FUNCTION(dbcrunch);
PHP_FUNCTION(pfget);
PHP_FUNCTION(pfget_boolean);
PHP_FUNCTION(trapply_calib);
PHP_FUNCTION(trsplit);
PHP_FUNCTION(trsplice);
PHP_FUNCTION(trloadchan);
PHP_FUNCTION(trfree);
PHP_FUNCTION(trextract_data);
PHP_FUNCTION(eval_response);
PHP_FUNCTION(strtdelta);
PHP_FUNCTION(strtime);
PHP_FUNCTION(strydtime);
PHP_FUNCTION(strdate);
PHP_FUNCTION(strlocaltime);
PHP_FUNCTION(strlocalydtime);
PHP_FUNCTION(strlocaldate);

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
