#ifndef PHP_DATASCOPE_H
#define PHP_DATASCOPE_H

extern zend_module_entry Datascope_module_entry;
#define phpext_Datascope_ptr &Datascope_module_entry

#define PHP_DATASCOPE_API
#define PHP_DATASCOPE_EXTNAME "Datascope"
#define PHP_DATASCOPE_EXTVER  "1.0"
	  
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
PHP_FUNCTION(dbinvalid);
PHP_FUNCTION(dbstrtype);
PHP_FUNCTION(dbtruncate);
PHP_FUNCTION(dblookup);
PHP_FUNCTION(dbgetv);
PHP_FUNCTION(dbaddv);
PHP_FUNCTION(dbadd_remark);
PHP_FUNCTION(dbget_remark);
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
PHP_FUNCTION(dbmatches);
PHP_FUNCTION(dbquery);
PHP_FUNCTION(dbresponse);
PHP_FUNCTION(dbseparate);
PHP_FUNCTION(dbsever);
PHP_FUNCTION(dbsubset);
PHP_FUNCTION(dbunjoin);
PHP_FUNCTION(dbbase);
PHP_FUNCTION(db2xml);
PHP_FUNCTION(dbwrite_view);
PHP_FUNCTION(dbread_view);
PHP_FUNCTION(dbsave_view);
PHP_FUNCTION(dbcompile);
PHP_FUNCTION(dbnextid);
PHP_FUNCTION(dbmark);
PHP_FUNCTION(dbdelete);
PHP_FUNCTION(dbcrunch);
PHP_FUNCTION(dblist2subset);
PHP_FUNCTION(dbget_range);
PHP_FUNCTION(pfget);
PHP_FUNCTION(pfget_boolean);
PHP_FUNCTION(pfupdate);
PHP_FUNCTION(pffiles);
PHP_FUNCTION(pf2string);
PHP_FUNCTION(pfrequire);
PHP_FUNCTION(pfcompile);
PHP_FUNCTION(pfwrite);
PHP_FUNCTION(pfput);
PHP_FUNCTION(pfdel);
PHP_FUNCTION(pf2xml);
PHP_FUNCTION(trapply_calib);
PHP_FUNCTION(trsplit);
PHP_FUNCTION(trsplice);
PHP_FUNCTION(trloadchan);
PHP_FUNCTION(trsample);
PHP_FUNCTION(trsamplebins);
PHP_FUNCTION(trfilter);
PHP_FUNCTION(trfree);
PHP_FUNCTION(trdestroy);
PHP_FUNCTION(trextract_data);
PHP_FUNCTION(trdata);
PHP_FUNCTION(trdatabins);
PHP_FUNCTION(trendtime);
PHP_FUNCTION(trnsamp);
PHP_FUNCTION(trsamp2time);
PHP_FUNCTION(trsamprate);
PHP_FUNCTION(trtime2samp);
PHP_FUNCTION(eval_response);
PHP_FUNCTION(strtdelta);
PHP_FUNCTION(strtime);
PHP_FUNCTION(strydtime);
PHP_FUNCTION(strdate);
PHP_FUNCTION(strlocaltime);
PHP_FUNCTION(strlocalydtime);
PHP_FUNCTION(strlocaldate);
PHP_FUNCTION(now);
PHP_FUNCTION(is_epoch_string);
PHP_FUNCTION(epoch2str);
PHP_FUNCTION(str2epoch);
PHP_FUNCTION(epoch);
PHP_FUNCTION(yearday);
PHP_FUNCTION(finit_db);
PHP_FUNCTION(map_autodrm_netsta);
PHP_FUNCTION(map_autodrm_chanaux);
PHP_FUNCTION(autodrm_net);
PHP_FUNCTION(autodrm_aux);
PHP_FUNCTION(map_seed_netsta);
PHP_FUNCTION(map_seed_chanloc);
PHP_FUNCTION(seed_net);
PHP_FUNCTION(seed_loc);
PHP_FUNCTION(abspath);
PHP_FUNCTION(relpath);
PHP_FUNCTION(cleanpath);
PHP_FUNCTION(concatpaths);
PHP_FUNCTION(parsepath);
PHP_FUNCTION(yesno);
PHP_FUNCTION(makedir);
PHP_FUNCTION(make_pathdirs);
PHP_FUNCTION(datafile);
PHP_FUNCTION(datapath);
PHP_FUNCTION(grn);
PHP_FUNCTION(grname);
PHP_FUNCTION(srn);
PHP_FUNCTION(srname);
PHP_FUNCTION(elog_init);
PHP_FUNCTION(elog_log);
PHP_FUNCTION(elog_debug);
PHP_FUNCTION(elog_notify);
PHP_FUNCTION(elog_alert);
PHP_FUNCTION(elog_complain);
PHP_FUNCTION(elog_die);
PHP_FUNCTION(elog_string);
PHP_FUNCTION(elog_clear);
PHP_FUNCTION(elog_mark);
PHP_FUNCTION(elog_flush);
PHP_FUNCTION(elog_callback);

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
