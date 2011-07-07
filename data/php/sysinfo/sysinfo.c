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
 * 
 */

#include "php.h"
#include "php_ini.h"
#include "ext/standard/info.h"
#include "sysinfo_php.h"
#include "stock.h"
#include "sysdata.h"

/* Prevent the deviants.h reassignment to std_now() from corrupting
 * the name of the PHP function */
#undef now

function_entry sysinfo_functions[] = {
	PHP_FE(my_ip, NULL)		
	PHP_FE(my_hardware, NULL)		
	PHP_FE(my_hostname, NULL)		
	PHP_FE(my_os, NULL)		
	PHP_FE(my_username, NULL)		
	PHP_FE(ip2name, NULL)		
	PHP_FE(name2ip, NULL)		
	PHP_FE(ip, NULL)		 
	PHP_FE(pidexecname, NULL)		 
	PHP_FE(pidcmdline, NULL)		 
	PHP_FE(pidpwd, NULL)		 
	PHP_FE(pid_exists, NULL)		 
	PHP_FE(pidinfo, NULL)		 
	PHP_FE(nomem, NULL)		 
	PHP_FE(sysloadavg, NULL)		 
	PHP_FE(sysmem, NULL)		 
	PHP_FE(sysmounted, NULL)		 
	PHP_FE(syscpu, NULL)		 
	PHP_FE(statvfs, NULL)		 
	{NULL, NULL, NULL}	
};

zend_module_entry sysinfo_module_entry = {
	STANDARD_MODULE_HEADER,
	"sysinfo",
	sysinfo_functions,
	PHP_MINIT(sysinfo),
	PHP_MSHUTDOWN(sysinfo),
	NULL,
	NULL,
	PHP_MINFO(sysinfo),
	"0.1",
	STANDARD_MODULE_PROPERTIES
};

ZEND_GET_MODULE(sysinfo)

PHP_MINIT_FUNCTION(sysinfo)
{
	return SUCCESS;
}

PHP_MSHUTDOWN_FUNCTION(sysinfo)
{
	return SUCCESS;
}

PHP_MINFO_FUNCTION(sysinfo)
{
	php_info_print_table_start();
	php_info_print_table_header(2, "sysinfo support", "enabled");
	php_info_print_table_end();
}

/* {{{ proto array my_ip( void ) */
PHP_FUNCTION(my_ip)
{
	int	argc = ZEND_NUM_ARGS();
	char	hostname[STRSZ];
	char	ipc[STRSZ];
	int	ip;

	if( argc != 0 ) {

		WRONG_PARAM_COUNT;
	}

	my_ip( hostname, ipc, &ip );

	array_init( return_value );

	add_next_index_string( return_value, hostname, 1 );
	add_next_index_string( return_value, ipc, 1 );

	return;
}
/* }}} */

/* {{{ proto string my_hardware( void ) */
PHP_FUNCTION(my_hardware)
{
	int	argc = ZEND_NUM_ARGS();
	char	hardware[STRSZ];

	if( argc != 0 ) {

		WRONG_PARAM_COUNT;
	}

	my_hardware( hardware );

	RETURN_STRING( hardware, 1 );
}
/* }}} */

/* {{{ proto string my_hostname( void ) */
PHP_FUNCTION(my_hostname)
{
	int	argc = ZEND_NUM_ARGS();
	char	hostname[STRSZ];

	if( argc != 0 ) {

		WRONG_PARAM_COUNT;
	}

	my_hostname( hostname );

	RETURN_STRING( hostname, 1 );
}
/* }}} */

/* {{{ proto string my_os( void ) */
PHP_FUNCTION(my_os)
{
	int	argc = ZEND_NUM_ARGS();
	char	os[STRSZ];

	if( argc != 0 ) {

		WRONG_PARAM_COUNT;
	}

	my_os( os );

	RETURN_STRING( os, 1 );
}
/* }}} */

/* {{{ proto string my_username( void ) */
PHP_FUNCTION(my_username)
{
	int	argc = ZEND_NUM_ARGS();
	char	username[STRSZ];

	if( argc != 0 ) {

		WRONG_PARAM_COUNT;
	}

	my_username( username );

	RETURN_STRING( username, 1 );
}
/* }}} */

/* {{{ proto string ip2name( string ip ) */
PHP_FUNCTION(ip2name)
{
	int	argc = ZEND_NUM_ARGS();
	char	*ip;
	int	ip_len;
	char	name[STRSZ];

	if( argc != 1 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "s", &ip, &ip_len ) 
		== FAILURE ) {

		return;
	}

	ip2name( inet_addr( ip ), name );

	RETURN_STRING( name, 1 );
}
/* }}} */

/* {{{ proto string name2ip( string name ) */
PHP_FUNCTION(name2ip)
{
	int	argc = ZEND_NUM_ARGS();
	char	*name;
	int	name_len;
	char	ipc[STRSZ];
	struct in_addr addr;

	if( argc != 1 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "s", &name, &name_len ) 
		== FAILURE ) {

		return;
	}

	name2ip( name, &addr, ipc );

	RETURN_STRING( ipc, 1 );
}
/* }}} */

/* {{{ proto int ip( string ip ) */
PHP_FUNCTION(ip)
{
	int	argc = ZEND_NUM_ARGS();
	char	*ip;
	int	ip_len;

	if( argc != 1 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "s", &ip, &ip_len ) 
		== FAILURE ) {

		return;
	}

	RETURN_LONG( inet_addr( ip ) );
}
/* }}} */

/* {{{ proto string pidexecname( int pid ) */
PHP_FUNCTION(pidexecname)
{
	int	argc = ZEND_NUM_ARGS();
	long	pid;
	char	*execname;
	char	*execname_safe_copy;

	if( argc != 1 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "l", &pid ) 
		== FAILURE ) {

		return;
	}

	execname = pidexecname( pid );

	execname_safe_copy = estrdup( execname );

	free( execname );

	RETURN_STRING( execname_safe_copy, 0 );
}
/* }}} */

/* {{{ proto string pidcmdline( int pid ) */
PHP_FUNCTION(pidcmdline)
{
	int	argc = ZEND_NUM_ARGS();
	long	pid;
	char	*cmdline;
	char	*cmdline_safe_copy;

	if( argc != 1 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "l", &pid ) 
		== FAILURE ) {

		return;
	}

	cmdline = pidcmdline( pid );

	cmdline_safe_copy = estrdup( cmdline );

	free( cmdline );

	RETURN_STRING( cmdline_safe_copy, 0 );
}
/* }}} */

/* {{{ proto string pidpwd( int pid ) */
PHP_FUNCTION(pidpwd)
{
	int	argc = ZEND_NUM_ARGS();
	long	pid;
	char	*pwd;
	char	*pwd_safe_copy;

	if( argc != 1 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "l", &pid ) 
		== FAILURE ) {

		return;
	}

	pwd = pidpwd( pid );

	pwd_safe_copy = estrdup( pwd );

	free( pwd );

	RETURN_STRING( pwd_safe_copy, 0 );
}
/* }}} */

/* {{{ proto bool pid_exists( int pid ) */
PHP_FUNCTION(pid_exists)
{
	int	argc = ZEND_NUM_ARGS();
	long	pid;
	int	exists;

	if( argc != 1 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "l", &pid ) 
		== FAILURE ) {

		return;
	}

	exists = pid_exists( pid );

	RETURN_BOOL( exists );
}
/* }}} */

/* {{{ proto array pidinfo( int pid ) */
PHP_FUNCTION(pidinfo)
{
	int	argc = ZEND_NUM_ARGS();
	long	pid;
	Pidstat	process;
	int	rc;

	if( argc != 1 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "l", &pid ) 
		== FAILURE ) {

		return;
	}

	rc = pidinfo( pid, &process );

	if( rc < 0 ) {
		
		zend_error( E_ERROR, "pidinfo failed!\n" );
	}

	array_init( return_value );

	add_assoc_long( return_value, "pid", process.pid );
	add_assoc_long( return_value, "uid", process.uid );
	add_assoc_double( return_value, "pcpu", process.pcpu );
	add_assoc_string( return_value, "state", process.state, 1 );
	add_assoc_long( return_value, "size", process.size );
	add_assoc_double( return_value, "cputime", process.cputime );
	add_assoc_double( return_value, "ccputime", process.ccputime );
	add_assoc_long( return_value, "ppid", process.ppid );
	add_assoc_long( return_value, "rss", process.rss );
	add_assoc_long( return_value, "started", process.started );

	return;
}
/* }}} */

/* {{{ proto int nomem( int address, int nbytes ) */
PHP_FUNCTION(nomem)
{
	int	argc = ZEND_NUM_ARGS();
	long	address;
	long	nbytes;
	char	*addr;
	char	*result;

	if( argc != 2 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "ll", &address, &nbytes ) 
		== FAILURE ) {

		return;
	}

	addr = (char *) address;

	result = (char *) nomem( addr, nbytes );

	RETURN_LONG( (long) result );
}
/* }}} */

/* {{{ proto array sysloadavg( void ) */
PHP_FUNCTION(sysloadavg)
{
	int	argc = ZEND_NUM_ARGS();
	double	avg[3];
	int	nproc;

	if( argc != 0 ) {

		WRONG_PARAM_COUNT;
	}

	sysloadavg( avg, &nproc );

	array_init( return_value );

	add_assoc_long( return_value, "nproc", nproc );
	add_assoc_double( return_value, "avg1m", avg[0] );
	add_assoc_double( return_value, "avg5m", avg[1] );
	add_assoc_double( return_value, "avg15m", avg[2] );

	return;
}
/* }}} */

/* {{{ proto array sysmem( void ) */
PHP_FUNCTION(sysmem)
{
	int	argc = ZEND_NUM_ARGS();
	double	total;
	double	physical;
	double	used;

	if( argc != 0 ) {

		WRONG_PARAM_COUNT;
	}

	sysmem( &total, &physical, &used );

	array_init( return_value );

	add_assoc_double( return_value, "total", total );
	add_assoc_double( return_value, "used", used );
	add_assoc_double( return_value, "physmem", physical );

	return;
}
/* }}} */

/* {{{ proto array syscpu( void ) */
PHP_FUNCTION(syscpu)
{
	int	argc = ZEND_NUM_ARGS();
	int	ncpu;
	CpuUsage *cpu;
	int	i;

	if( argc != 0 ) {

		WRONG_PARAM_COUNT;
	}

	syscpu( &ncpu, &cpu );

	array_init( return_value );

	add_next_index_long( return_value, ncpu );

	for( i = 0; i < ncpu; i++ ) {

		add_next_index_double( return_value, cpu[i].idle );
		add_next_index_double( return_value, cpu[i].user );
		add_next_index_double( return_value, cpu[i].kernel );
		add_next_index_double( return_value, cpu[i].iowait );
		add_next_index_double( return_value, cpu[i].swap );
	}

	return;
}
/* }}} */

/* {{{ proto array statvfs( string path ) */
PHP_FUNCTION(statvfs)
{
	int	argc = ZEND_NUM_ARGS();
	char	*path;
	int	path_len;
	Std_statvfs sbuf;
	double	bs; /* block size in MB */

	if( argc != 1 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "s", &path, &path_len ) 
		== FAILURE ) {

		return;
	}

	if( std_statvfs( path, &sbuf ) != 0 ) {
	
		zend_error( E_ERROR, "Failed to get stat information for path!\n" );
	}

	array_init( return_value );

	bs = sbuf.f_frsize / 1024. / 1024.;

	add_assoc_double( return_value, "Mb_total", bs * sbuf.f_blocks );
	add_assoc_double( return_value, "Mb_avail", bs * sbuf.f_bavail );
	add_assoc_double( return_value, "used", 
					1. - (1.*sbuf.f_bavail)/sbuf.f_blocks );

	add_assoc_long( return_value, "inodes", sbuf.f_files );
	add_assoc_long( return_value, "inodes_avail", sbuf.f_favail );
	add_assoc_double( return_value, "inodes_used", 
					1. - (1.*sbuf.f_favail)/sbuf.f_files );

	add_assoc_long( return_value, "id", sbuf.f_fsid );

	return;
}
/* }}} */

/* {{{ proto array sysmounted( void ) */
PHP_FUNCTION(sysmounted)
{
	int	argc = ZEND_NUM_ARGS();
	Tbl	*sysmnt;
	SysMounted *amount;
	char	*mountrow;
	int	i;

	if( argc != 0 ) {

		WRONG_PARAM_COUNT;
	}

	sysmounted( &sysmnt );

	array_init( return_value );


	for( i = 0; i < maxtbl( sysmnt ); i++ ) {
		
		amount = (SysMounted *) gettbl( sysmnt, i );

		allot( char *, mountrow, sizeof( amount->mountpt ) +
					 sizeof( amount->fstype ) +
					 sizeof( amount->host ) + 
					 sizeof( amount->source ) + 20 );

		sprintf( mountrow, "%s\t%s\t%s\t%s",
				   amount->mountpt,
				   amount->fstype,
				   amount->host,
				   amount->source );

		add_next_index_string( return_value, mountrow, 1 );
		
		free( mountrow );
	}

	return;
}
/* }}} */

/* local variables
 * End:
 * vim600: fdm=marker
 */
