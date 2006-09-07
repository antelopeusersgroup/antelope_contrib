#ifndef PHP_ORB_H
#define PHP_ORB_H

extern zend_module_entry Orb_module_entry;
#define phpext_Orb_ptr &Orb_module_entry

#define PHP_ORB_API

#ifdef ZTS
#include "TSRM.h"
#endif

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
PHP_FUNCTION(pforbstat);
PHP_FUNCTION(split_srcname);

#ifdef ZTS
#define ORB_G(v) TSRMG(Orb_globals_id, zend_Orb_globals *, v)
#else
#define ORB_G(v) (Orb_globals.v)
#endif
 
#endif	/* PHP_ORB_H */
