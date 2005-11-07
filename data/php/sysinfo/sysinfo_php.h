#ifndef PHP_SYSINFO_H
#define PHP_SYSINFO_H

extern zend_module_entry sysinfo_module_entry;
#define phpext_sysinfo_ptr &sysinfo_module_entry

#define PHP_SYSINFO_API

#ifdef ZTS
#include "TSRM.h"
#endif

PHP_MINIT_FUNCTION(sysinfo);
PHP_MSHUTDOWN_FUNCTION(sysinfo);
PHP_MINFO_FUNCTION(sysinfo);
PHP_FUNCTION(my_ip);
PHP_FUNCTION(my_hardware);
PHP_FUNCTION(my_hostname);
PHP_FUNCTION(my_os);
PHP_FUNCTION(my_username);
PHP_FUNCTION(ip2name);
PHP_FUNCTION(name2ip);
PHP_FUNCTION(ip);
PHP_FUNCTION(pidexecname);
PHP_FUNCTION(pidcmdline);
PHP_FUNCTION(pidpwd);
PHP_FUNCTION(pid_exists);
PHP_FUNCTION(pidinfo);
PHP_FUNCTION(nomem);
PHP_FUNCTION(sysloadavg);
PHP_FUNCTION(sysmem);
PHP_FUNCTION(sysmounted);
PHP_FUNCTION(syscpu);
PHP_FUNCTION(statvfs);

#ifdef ZTS
#define SYSINFO_G(v) TSRMG(sysinfo_globals_id, zend_sysinfo_globals *, v)
#else
#define SYSINFO_G(v) (sysinfo_globals.v)
#endif
 
#endif	/* PHP_SYSINFO_H */
