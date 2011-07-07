/*
 *   Antelope Orb interface for PHP
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

#include <netinet/in.h>
#include <arpa/inet.h>
#include "Orb_php.h"
#undef WORDS_BIGENDIAN
#include "stock.h"
#include "orb.h"
#include "forb.h"
#undef orbpkt_string
#include "Pkt.h"
#include "pf.h"
#include "pforbstat.h"

/* Prevent the deviants.h reassignment to std_now() from corrupting
 * the name of the PHP function */
#undef now

static int le_Orb_Packet;

#define PHP_ORB_PACKET_RES_NAME "Orb_Packet"

static function_entry Orb_functions[] = {
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
	PHP_FE(orbreap, NULL)		
	PHP_FE(orbreap_timeout, NULL)		
	PHP_FE(orbget, NULL)		
	PHP_FE(orbput, NULL)		
	PHP_FE(orbputx, NULL)		
	PHP_FE(orbstat, NULL)		
	PHP_FE(orbsources, NULL)		
	PHP_FE(orbclients, NULL)		
	PHP_FE(pforbstat, NULL)		
	PHP_FE(split_srcname, NULL)		
	PHP_FE(join_srcname, NULL)		
	PHP_FE(suffix2pkttype, NULL)		
	PHP_FE(unstuffpkt, NULL)		
	PHP_FE(orbpkt_string, NULL)
	{NULL, NULL, NULL}	
};

zend_module_entry Orb_module_entry = {
	STANDARD_MODULE_HEADER,
	PHP_ORB_EXTNAME,
	Orb_functions,
	PHP_MINIT(Orb),
	PHP_MSHUTDOWN(Orb),
	NULL,
	NULL,
	PHP_MINFO(Orb),
	PHP_ORB_EXTVER,
	STANDARD_MODULE_PROPERTIES
};

/* Missing from pf.h in Antelope 4.8: */
extern void putPf_nofree( char *name, Pf *pf );

zend_object_value orb_pkt_obj_new( zend_class_entry *class_type TSRMLS_DC );
zend_object_value orb_chan_obj_new( zend_class_entry *class_type TSRMLS_DC );
zend_object_value orb_stat_obj_new( zend_class_entry *class_type TSRMLS_DC );
zend_object_value orb_source_obj_new( zend_class_entry *class_type TSRMLS_DC );
zend_object_value orb_client_obj_new( zend_class_entry *class_type TSRMLS_DC );

static zend_object_handlers orb_pkt_obj_handlers;
static zend_object_handlers orb_chan_obj_handlers;
static zend_object_handlers orb_stat_obj_handlers;
static zend_object_handlers orb_source_obj_handlers;
static zend_object_handlers orb_client_obj_handlers;

typedef struct _php_orb_pkt_obj {
	zend_object	std;
	int		type;
	int		pkt_res;
} php_orb_pkt_obj;

typedef struct _php_orb_chan_obj {
	zend_object	std;
	PktChannel	*pktchan;
	int		pkt_res;
} php_orb_chan_obj;

typedef struct _php_orb_stat_obj {
	zend_object	std;
	Orbstat		*os;
} php_orb_stat_obj;

typedef struct _php_orb_source_obj {
	zend_object	std;
	Orbsrc		*os;
} php_orb_source_obj;

typedef struct _php_orb_client_obj {
	zend_object	std;
	Orbclient	*oc;
} php_orb_client_obj;

PHP_METHOD(orb_pkt, packettype);
PHP_METHOD(orb_pkt, time);
PHP_METHOD(orb_pkt, nchannels);
PHP_METHOD(orb_pkt, channels);
PHP_METHOD(orb_pkt, version);
PHP_METHOD(orb_pkt, dfile);
PHP_METHOD(orb_pkt, pf);
PHP_METHOD(orb_pkt, db);
PHP_METHOD(orb_pkt, string);
PHP_METHOD(orb_pkt, parts);

zend_class_entry *php_orb_pkt_entry;
#define PHP_ORB_PKT_NAME "orb_pkt"
static function_entry php_orb_pkt_functions[] = {
	PHP_ME(orb_pkt, packettype, NULL, ZEND_ACC_PUBLIC)
	PHP_ME(orb_pkt, time, NULL, ZEND_ACC_PUBLIC)
	PHP_ME(orb_pkt, nchannels, NULL, ZEND_ACC_PUBLIC)
	PHP_ME(orb_pkt, channels, NULL, ZEND_ACC_PUBLIC)
	PHP_ME(orb_pkt, version, NULL, ZEND_ACC_PUBLIC)
	PHP_ME(orb_pkt, dfile, NULL, ZEND_ACC_PUBLIC)
	PHP_ME(orb_pkt, pf, NULL, ZEND_ACC_PUBLIC)
	PHP_ME(orb_pkt, db, NULL, ZEND_ACC_PUBLIC)
	PHP_ME(orb_pkt, string, NULL, ZEND_ACC_PUBLIC)
	PHP_ME(orb_pkt, parts, NULL, ZEND_ACC_PUBLIC)
	{ NULL, NULL, NULL }
};

PHP_METHOD(orb_channel, nsamp);
PHP_METHOD(orb_channel, iuser1);
PHP_METHOD(orb_channel, iuser2);
PHP_METHOD(orb_channel, iuser3);
PHP_METHOD(orb_channel, net);
PHP_METHOD(orb_channel, sta);
PHP_METHOD(orb_channel, chan);
PHP_METHOD(orb_channel, loc);
PHP_METHOD(orb_channel, segtype);
PHP_METHOD(orb_channel, cuser1);
PHP_METHOD(orb_channel, cuser2);
PHP_METHOD(orb_channel, time);
PHP_METHOD(orb_channel, samprate);
PHP_METHOD(orb_channel, calib);
PHP_METHOD(orb_channel, calper);
PHP_METHOD(orb_channel, duser1);
PHP_METHOD(orb_channel, duser2);
PHP_METHOD(orb_channel, data);

zend_class_entry *php_orb_chan_entry;
#define PHP_ORB_CHAN_NAME "orb_channel"
static function_entry php_orb_chan_functions[] = {
	PHP_ME(orb_channel, nsamp, NULL, ZEND_ACC_PUBLIC)
	PHP_ME(orb_channel, iuser1, NULL, ZEND_ACC_PUBLIC)
	PHP_ME(orb_channel, iuser2, NULL, ZEND_ACC_PUBLIC)
	PHP_ME(orb_channel, iuser3, NULL, ZEND_ACC_PUBLIC)
	PHP_ME(orb_channel, net, NULL, ZEND_ACC_PUBLIC)
	PHP_ME(orb_channel, sta, NULL, ZEND_ACC_PUBLIC)
	PHP_ME(orb_channel, chan, NULL, ZEND_ACC_PUBLIC)
	PHP_ME(orb_channel, loc, NULL, ZEND_ACC_PUBLIC)
	PHP_ME(orb_channel, segtype, NULL, ZEND_ACC_PUBLIC)
	PHP_ME(orb_channel, cuser1, NULL, ZEND_ACC_PUBLIC)
	PHP_ME(orb_channel, cuser2, NULL, ZEND_ACC_PUBLIC)
	PHP_ME(orb_channel, time, NULL, ZEND_ACC_PUBLIC)
	PHP_ME(orb_channel, samprate, NULL, ZEND_ACC_PUBLIC)
	PHP_ME(orb_channel, calib, NULL, ZEND_ACC_PUBLIC)
	PHP_ME(orb_channel, calper, NULL, ZEND_ACC_PUBLIC)
	PHP_ME(orb_channel, duser1, NULL, ZEND_ACC_PUBLIC)
	PHP_ME(orb_channel, duser2, NULL, ZEND_ACC_PUBLIC)
	PHP_ME(orb_channel, data, NULL, ZEND_ACC_PUBLIC)
	{ NULL, NULL, NULL }
};

PHP_METHOD(orb_stat, when);
PHP_METHOD(orb_stat, started);
PHP_METHOD(orb_stat, orb_start);
PHP_METHOD(orb_stat, address);
PHP_METHOD(orb_stat, version);
PHP_METHOD(orb_stat, who);
PHP_METHOD(orb_stat, host);
PHP_METHOD(orb_stat, connections);
PHP_METHOD(orb_stat, messages);
PHP_METHOD(orb_stat, maxdata);
PHP_METHOD(orb_stat, errors);
PHP_METHOD(orb_stat, rejected);
PHP_METHOD(orb_stat, closes);
PHP_METHOD(orb_stat, opens);
PHP_METHOD(orb_stat, port);
PHP_METHOD(orb_stat, pid);
PHP_METHOD(orb_stat, nsources);
PHP_METHOD(orb_stat, nclients);
PHP_METHOD(orb_stat, maxsrc);
PHP_METHOD(orb_stat, maxpkts);

zend_class_entry *php_orb_stat_entry;
#define PHP_ORB_STAT_NAME "orb_stat"
static function_entry php_orb_stat_functions[] = {
	PHP_ME(orb_stat, when, NULL, ZEND_ACC_PUBLIC)
	PHP_ME(orb_stat, started, NULL, ZEND_ACC_PUBLIC)
	PHP_ME(orb_stat, orb_start, NULL, ZEND_ACC_PUBLIC)
	PHP_ME(orb_stat, address, NULL, ZEND_ACC_PUBLIC)
	PHP_ME(orb_stat, version, NULL, ZEND_ACC_PUBLIC)
	PHP_ME(orb_stat, who, NULL, ZEND_ACC_PUBLIC)
	PHP_ME(orb_stat, host, NULL, ZEND_ACC_PUBLIC)
	PHP_ME(orb_stat, connections, NULL, ZEND_ACC_PUBLIC)
	PHP_ME(orb_stat, messages, NULL, ZEND_ACC_PUBLIC)
	PHP_ME(orb_stat, maxdata, NULL, ZEND_ACC_PUBLIC)
	PHP_ME(orb_stat, errors, NULL, ZEND_ACC_PUBLIC)
	PHP_ME(orb_stat, rejected, NULL, ZEND_ACC_PUBLIC)
	PHP_ME(orb_stat, closes, NULL, ZEND_ACC_PUBLIC)
	PHP_ME(orb_stat, opens, NULL, ZEND_ACC_PUBLIC)
	PHP_ME(orb_stat, port, NULL, ZEND_ACC_PUBLIC)
	PHP_ME(orb_stat, pid, NULL, ZEND_ACC_PUBLIC)
	PHP_ME(orb_stat, nsources, NULL, ZEND_ACC_PUBLIC)
	PHP_ME(orb_stat, nclients, NULL, ZEND_ACC_PUBLIC)
	PHP_ME(orb_stat, maxsrc, NULL, ZEND_ACC_PUBLIC)
	PHP_ME(orb_stat, maxpkts, NULL, ZEND_ACC_PUBLIC)
	{ NULL, NULL, NULL }
};

PHP_METHOD(orb_source, srcname);
PHP_METHOD(orb_source, slatest_time);
PHP_METHOD(orb_source, soldest_time);
PHP_METHOD(orb_source, active);
PHP_METHOD(orb_source, npkts);
PHP_METHOD(orb_source, nbytes);
PHP_METHOD(orb_source, slatest);
PHP_METHOD(orb_source, soldest);

zend_class_entry *php_orb_source_entry;
#define PHP_ORB_SOURCE_NAME "orb_source"
static function_entry php_orb_source_functions[] = {
	PHP_ME(orb_source, srcname, NULL, ZEND_ACC_PUBLIC)
	PHP_ME(orb_source, slatest_time, NULL, ZEND_ACC_PUBLIC)
	PHP_ME(orb_source, soldest_time, NULL, ZEND_ACC_PUBLIC)
	PHP_ME(orb_source, active, NULL, ZEND_ACC_PUBLIC)
	PHP_ME(orb_source, npkts, NULL, ZEND_ACC_PUBLIC)
	PHP_ME(orb_source, nbytes, NULL, ZEND_ACC_PUBLIC)
	PHP_ME(orb_source, slatest, NULL, ZEND_ACC_PUBLIC)
	PHP_ME(orb_source, soldest, NULL, ZEND_ACC_PUBLIC)
	{ NULL, NULL, NULL }
};

PHP_METHOD(orb_client, lastpkt);
PHP_METHOD(orb_client, started);
PHP_METHOD(orb_client, address);
PHP_METHOD(orb_client, perm);
PHP_METHOD(orb_client, what);
PHP_METHOD(orb_client, host);
PHP_METHOD(orb_client, who);
PHP_METHOD(orb_client, select);
PHP_METHOD(orb_client, reject);
PHP_METHOD(orb_client, read);
PHP_METHOD(orb_client, pid);
PHP_METHOD(orb_client, bytes);
PHP_METHOD(orb_client, packets);
PHP_METHOD(orb_client, pktid);
PHP_METHOD(orb_client, port);
PHP_METHOD(orb_client, thread);
PHP_METHOD(orb_client, fd);
PHP_METHOD(orb_client, nreject);
PHP_METHOD(orb_client, nselect);
PHP_METHOD(orb_client, errors);
PHP_METHOD(orb_client, priority);
PHP_METHOD(orb_client, lastrequest);
PHP_METHOD(orb_client, nrequests);
PHP_METHOD(orb_client, nwrites);
PHP_METHOD(orb_client, nreads);
PHP_METHOD(orb_client, written);

zend_class_entry *php_orb_client_entry;
#define PHP_ORB_CLIENT_NAME "orb_client"
static function_entry php_orb_client_functions[] = {
	PHP_ME(orb_client, lastpkt, NULL, ZEND_ACC_PUBLIC)
	PHP_ME(orb_client, started, NULL, ZEND_ACC_PUBLIC)
	PHP_ME(orb_client, address, NULL, ZEND_ACC_PUBLIC)
	PHP_ME(orb_client, perm, NULL, ZEND_ACC_PUBLIC)
	PHP_ME(orb_client, what, NULL, ZEND_ACC_PUBLIC)
	PHP_ME(orb_client, host, NULL, ZEND_ACC_PUBLIC)
	PHP_ME(orb_client, who, NULL, ZEND_ACC_PUBLIC)
	PHP_ME(orb_client, select, NULL, ZEND_ACC_PUBLIC)
	PHP_ME(orb_client, reject, NULL, ZEND_ACC_PUBLIC)
	PHP_ME(orb_client, read, NULL, ZEND_ACC_PUBLIC)
	PHP_ME(orb_client, pid, NULL, ZEND_ACC_PUBLIC)
	PHP_ME(orb_client, bytes, NULL, ZEND_ACC_PUBLIC)
	PHP_ME(orb_client, packets, NULL, ZEND_ACC_PUBLIC)
	PHP_ME(orb_client, pktid, NULL, ZEND_ACC_PUBLIC)
	PHP_ME(orb_client, port, NULL, ZEND_ACC_PUBLIC)
	PHP_ME(orb_client, thread, NULL, ZEND_ACC_PUBLIC)
	PHP_ME(orb_client, fd, NULL, ZEND_ACC_PUBLIC)
	PHP_ME(orb_client, nreject, NULL, ZEND_ACC_PUBLIC)
	PHP_ME(orb_client, nselect, NULL, ZEND_ACC_PUBLIC)
	PHP_ME(orb_client, errors, NULL, ZEND_ACC_PUBLIC)
	PHP_ME(orb_client, priority, NULL, ZEND_ACC_PUBLIC)
	PHP_ME(orb_client, lastrequest, NULL, ZEND_ACC_PUBLIC)
	PHP_ME(orb_client, nrequests, NULL, ZEND_ACC_PUBLIC)
	PHP_ME(orb_client, nwrites, NULL, ZEND_ACC_PUBLIC)
	PHP_ME(orb_client, nreads, NULL, ZEND_ACC_PUBLIC)
	PHP_ME(orb_client, written, NULL, ZEND_ACC_PUBLIC)
	{ NULL, NULL, NULL }
};

ZEND_GET_MODULE(Orb)

static void
_php_free_Orb_Packet( zend_rsrc_list_entry *rsrc TSRMLS_DC ) {
	Packet *packet = (Packet *) rsrc->ptr;

	if( packet != (Packet *) NULL ) {
		
		freePkt( packet );
	}

	return;
}

void 
register_Orb_constants( INIT_FUNC_ARGS )
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

	for( i = 0; i < Pktxlatn; i++ ) {

		zend_register_long_constant( Pktxlat[i].name,
					     strlen( Pktxlat[i].name ) + 1, 
					     Pktxlat[i].num,
					     CONST_CS | CONST_PERSISTENT,
					     module_number TSRMLS_CC );
	}
}

void 
register_pforbstat_constants( INIT_FUNC_ARGS )
{
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
	return;

}

static void
register_Orb_classes( TSRMLS_D )
{
	zend_class_entry ce_pkt;
	zend_class_entry ce_chan;
	zend_class_entry ce_stat;
	zend_class_entry ce_source;
	zend_class_entry ce_client;

	memcpy( &orb_pkt_obj_handlers, 
		zend_get_std_object_handlers(),
		sizeof( zend_object_handlers ) );

	orb_pkt_obj_handlers.clone_obj = NULL;
	INIT_CLASS_ENTRY( ce_pkt, PHP_ORB_PKT_NAME, php_orb_pkt_functions );	
	ce_pkt.create_object = orb_pkt_obj_new;
	php_orb_pkt_entry = zend_register_internal_class( &ce_pkt TSRMLS_CC );

	memcpy( &orb_chan_obj_handlers, 
		zend_get_std_object_handlers(),
		sizeof( zend_object_handlers ) );

	orb_chan_obj_handlers.clone_obj = NULL;
	INIT_CLASS_ENTRY( ce_chan, PHP_ORB_CHAN_NAME, php_orb_chan_functions );	
	ce_chan.create_object = orb_chan_obj_new;
	php_orb_chan_entry = zend_register_internal_class( &ce_chan TSRMLS_CC );

	memcpy( &orb_stat_obj_handlers, 
		zend_get_std_object_handlers(),
		sizeof( zend_object_handlers ) );

	orb_stat_obj_handlers.clone_obj = NULL;
	INIT_CLASS_ENTRY( ce_stat, PHP_ORB_STAT_NAME, php_orb_stat_functions );	
	ce_stat.create_object = orb_stat_obj_new;
	php_orb_stat_entry = zend_register_internal_class( &ce_stat TSRMLS_CC );

	memcpy( &orb_source_obj_handlers, 
		zend_get_std_object_handlers(),
		sizeof( zend_object_handlers ) );

	orb_source_obj_handlers.clone_obj = NULL;
	INIT_CLASS_ENTRY( ce_source, PHP_ORB_SOURCE_NAME, php_orb_source_functions );	
	ce_source.create_object = orb_source_obj_new;
	php_orb_source_entry = zend_register_internal_class( &ce_source TSRMLS_CC );

	memcpy( &orb_client_obj_handlers, 
		zend_get_std_object_handlers(),
		sizeof( zend_object_handlers ) );

	orb_client_obj_handlers.clone_obj = NULL;
	INIT_CLASS_ENTRY( ce_client, PHP_ORB_CLIENT_NAME, php_orb_client_functions );	
	ce_client.create_object = orb_client_obj_new;
	php_orb_client_entry = zend_register_internal_class( &ce_client TSRMLS_CC );

	return;
}

PHP_MINIT_FUNCTION(Orb)
{
	le_Orb_Packet = zend_register_list_destructors_ex( 
				_php_free_Orb_Packet, NULL, 
				PHP_ORB_PACKET_RES_NAME, 0 );
	
	register_Orb_constants( INIT_FUNC_ARGS_PASSTHRU );

	register_pforbstat_constants( INIT_FUNC_ARGS_PASSTHRU );

	register_Orb_classes( TSRMLS_C );

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

static zval *
pkt2zval( int type, char *srcname, Packet *pkt )
{
	zval	*zval_pkt;
	zend_class_entry *ce;
	php_orb_pkt_obj *intern;

	ce = zend_fetch_class( PHP_ORB_PKT_NAME, 
			       strlen( PHP_ORB_PKT_NAME ), 
			       ZEND_FETCH_CLASS_AUTO );

	MAKE_STD_ZVAL( zval_pkt );

	object_init_ex( zval_pkt, ce );

	intern = (php_orb_pkt_obj *) 
			zend_objects_get_address( zval_pkt TSRMLS_CC );

	intern->type = type;
	intern->pkt_res = zend_list_insert( pkt, le_Orb_Packet );

	return zval_pkt;
}

static void
pktchan2zval( PktChannel *pktchan, int pkt_res, zval *zval_pktchan )
{
	zend_class_entry *ce;
	php_orb_chan_obj *intern;

	ce = zend_fetch_class( PHP_ORB_CHAN_NAME, 
			       strlen( PHP_ORB_CHAN_NAME ), 
			       ZEND_FETCH_CLASS_AUTO );

	object_init_ex( zval_pktchan, ce );

	intern = (php_orb_chan_obj *) 
			zend_objects_get_address( zval_pktchan TSRMLS_CC );

	intern->pktchan = pktchan;
	intern->pkt_res = pkt_res;

	zend_list_addref( intern->pkt_res );

	return;
}

static void 
orbstat2zval( Orbstat *os, zval *zval_orbstat )
{
	zend_class_entry *ce;
	php_orb_stat_obj *intern;

	ce = zend_fetch_class( PHP_ORB_STAT_NAME, 
			       strlen( PHP_ORB_STAT_NAME ), 
			       ZEND_FETCH_CLASS_AUTO );

	object_init_ex( zval_orbstat, ce );

	intern = (php_orb_stat_obj *) 
			zend_objects_get_address( zval_orbstat TSRMLS_CC );

	intern->os = os;

	return;
}

static void 
orbsource2zval( Orbsrc *os, zval *zval_orbsource )
{
	zend_class_entry *ce;
	php_orb_source_obj *intern;

	ce = zend_fetch_class( PHP_ORB_SOURCE_NAME, 
			       strlen( PHP_ORB_SOURCE_NAME ), 
			       ZEND_FETCH_CLASS_AUTO );

	object_init_ex( zval_orbsource, ce );

	intern = (php_orb_source_obj *) 
			zend_objects_get_address( zval_orbsource TSRMLS_CC );

	intern->os = os;

	return;
}

static void 
orbclient2zval( Orbclient *oc, zval *zval_orbclient )
{
	zend_class_entry *ce;
	php_orb_client_obj *intern;

	ce = zend_fetch_class( PHP_ORB_CLIENT_NAME, 
			       strlen( PHP_ORB_CLIENT_NAME ), 
			       ZEND_FETCH_CLASS_AUTO );

	object_init_ex( zval_orbclient, ce );

	intern = (php_orb_client_obj *) 
			zend_objects_get_address( zval_orbclient TSRMLS_CC );

	intern->oc = oc;

	return;
}

void 
orb_pkt_obj_free_resources( php_orb_pkt_obj *intern )
{
	if( intern ) {

		zend_list_delete( intern->pkt_res );
	}

	return;
}

void 
orb_chan_obj_free_resources( php_orb_chan_obj *intern )
{
	if( intern ) {

		zend_list_delete( intern->pkt_res );
	}

	return;
}

void 
orb_stat_obj_free_resources( php_orb_stat_obj *intern )
{
	; 

	return;
}

void 
orb_source_obj_free_resources( php_orb_source_obj *intern )
{
	; 

	return;
}

void 
orb_client_obj_free_resources( php_orb_client_obj *intern )
{
	; 

	return;
}

void
orb_pkt_obj_free_storage( void *object TSRMLS_DC )
{
	php_orb_pkt_obj *intern = (php_orb_pkt_obj *) object;

	zend_hash_destroy( intern->std.properties );
	FREE_HASHTABLE( intern->std.properties );

	orb_pkt_obj_free_resources( intern );

	efree( object );
}

void
orb_chan_obj_free_storage( void *object TSRMLS_DC )
{
	php_orb_chan_obj *intern = (php_orb_chan_obj *) object;

	zend_hash_destroy( intern->std.properties );
	FREE_HASHTABLE( intern->std.properties );

	orb_chan_obj_free_resources( intern );

	efree( object );
}

void
orb_stat_obj_free_storage( void *object TSRMLS_DC )
{
	php_orb_stat_obj *intern = (php_orb_stat_obj *) object;

	zend_hash_destroy( intern->std.properties );
	FREE_HASHTABLE( intern->std.properties );

	orb_stat_obj_free_resources( intern );

	efree( object );
}

void
orb_source_obj_free_storage( void *object TSRMLS_DC )
{
	php_orb_source_obj *intern = (php_orb_source_obj *) object;

	zend_hash_destroy( intern->std.properties );
	FREE_HASHTABLE( intern->std.properties );

	orb_source_obj_free_resources( intern );

	efree( object );
}

void
orb_client_obj_free_storage( void *object TSRMLS_DC )
{
	php_orb_client_obj *intern = (php_orb_client_obj *) object;

	zend_hash_destroy( intern->std.properties );
	FREE_HASHTABLE( intern->std.properties );

	orb_client_obj_free_resources( intern );

	efree( object );
}

zend_object_value
orb_pkt_obj_new( zend_class_entry *class_type TSRMLS_DC )
{
	zend_object_value retval;
	php_orb_pkt_obj	*intern;
	zval	*tmp;

	intern = emalloc( sizeof( php_orb_pkt_obj ) );
	intern->std.ce = class_type;
	intern->std.guards = NULL;

	ALLOC_HASHTABLE( intern->std.properties );
	zend_hash_init( intern->std.properties, 0, NULL, ZVAL_PTR_DTOR, 0 );
	zend_hash_copy( intern->std.properties, 
			&class_type->default_properties,
			(copy_ctor_func_t) zval_add_ref,
			(void *) &tmp,
			sizeof(zval *));
	retval.handle = zend_objects_store_put( intern, 
		(zend_objects_store_dtor_t) zend_objects_destroy_object, 
		(zend_objects_free_object_storage_t) orb_pkt_obj_free_storage,
		NULL TSRMLS_CC);
	retval.handlers = &orb_pkt_obj_handlers;

	return retval;
}

zend_object_value
orb_chan_obj_new( zend_class_entry *class_type TSRMLS_DC )
{
	zend_object_value retval;
	php_orb_chan_obj *intern;
	zval	*tmp;

	intern = emalloc( sizeof( php_orb_chan_obj ) );
	intern->std.ce = class_type;
	intern->std.guards = NULL;

	ALLOC_HASHTABLE( intern->std.properties );
	zend_hash_init( intern->std.properties, 0, NULL, ZVAL_PTR_DTOR, 0 );
	zend_hash_copy( intern->std.properties, 
			&class_type->default_properties,
			(copy_ctor_func_t) zval_add_ref,
			(void *) &tmp,
			sizeof(zval *));
	retval.handle = zend_objects_store_put( intern, 
		(zend_objects_store_dtor_t) zend_objects_destroy_object, 
		(zend_objects_free_object_storage_t) orb_chan_obj_free_storage,
		NULL TSRMLS_CC);
	retval.handlers = &orb_chan_obj_handlers;

	return retval;
}

zend_object_value
orb_stat_obj_new( zend_class_entry *class_type TSRMLS_DC )
{
	zend_object_value retval;
	php_orb_stat_obj *intern;
	zval	*tmp;

	intern = emalloc( sizeof( php_orb_stat_obj ) );
	intern->std.ce = class_type;
	intern->std.guards = NULL;

	ALLOC_HASHTABLE( intern->std.properties );
	zend_hash_init( intern->std.properties, 0, NULL, ZVAL_PTR_DTOR, 0 );
	zend_hash_copy( intern->std.properties, 
			&class_type->default_properties,
			(copy_ctor_func_t) zval_add_ref,
			(void *) &tmp,
			sizeof(zval *));
	retval.handle = zend_objects_store_put( intern, 
		(zend_objects_store_dtor_t) zend_objects_destroy_object, 
		(zend_objects_free_object_storage_t) orb_stat_obj_free_storage,
		NULL TSRMLS_CC);
	retval.handlers = &orb_chan_obj_handlers;

	return retval;
}

zend_object_value
orb_source_obj_new( zend_class_entry *class_type TSRMLS_DC )
{
	zend_object_value retval;
	php_orb_source_obj *intern;
	zval	*tmp;

	intern = emalloc( sizeof( php_orb_source_obj ) );
	intern->std.ce = class_type;
	intern->std.guards = NULL;

	ALLOC_HASHTABLE( intern->std.properties );
	zend_hash_init( intern->std.properties, 0, NULL, ZVAL_PTR_DTOR, 0 );
	zend_hash_copy( intern->std.properties, 
			&class_type->default_properties,
			(copy_ctor_func_t) zval_add_ref,
			(void *) &tmp,
			sizeof(zval *));
	retval.handle = zend_objects_store_put( intern, 
		(zend_objects_store_dtor_t) zend_objects_destroy_object, 
		(zend_objects_free_object_storage_t) orb_source_obj_free_storage,
		NULL TSRMLS_CC);
	retval.handlers = &orb_chan_obj_handlers;

	return retval;
}

zend_object_value
orb_client_obj_new( zend_class_entry *class_type TSRMLS_DC )
{
	zend_object_value retval;
	php_orb_client_obj *intern;
	zval	*tmp;

	intern = emalloc( sizeof( php_orb_client_obj ) );
	intern->std.ce = class_type;
	intern->std.guards = NULL;

	ALLOC_HASHTABLE( intern->std.properties );
	zend_hash_init( intern->std.properties, 0, NULL, ZVAL_PTR_DTOR, 0 );
	zend_hash_copy( intern->std.properties, 
			&class_type->default_properties,
			(copy_ctor_func_t) zval_add_ref,
			(void *) &tmp,
			sizeof(zval *));
	retval.handle = zend_objects_store_put( intern, 
		(zend_objects_store_dtor_t) zend_objects_destroy_object, 
		(zend_objects_free_object_storage_t) orb_client_obj_free_storage,
		NULL TSRMLS_CC);
	retval.handlers = &orb_chan_obj_handlers;

	return retval;
}

int
get_this_orb_pkt_res( zval *object )
{
	php_orb_pkt_obj *intern;

	intern = (php_orb_pkt_obj *) 
		    zend_objects_get_address( object TSRMLS_CC );

	return intern->pkt_res;
}

Packet *
get_this_orb_pkt( zval *object )
{
	Packet	*pkt;
	int	res;
	int	res_type;

	res = get_this_orb_pkt_res( object );

	pkt = (Packet *) zend_list_find( res, &res_type );

	if( res_type != le_Orb_Packet ) {

		return (Packet *) NULL;
	}

	return pkt;
}

int
get_this_orb_pkt_type( zval *object )
{
	php_orb_pkt_obj *intern;

	intern = (php_orb_pkt_obj *) 
		    zend_objects_get_address( object TSRMLS_CC );

	return intern->type;
}

PktChannel *
get_this_orb_pktchan( zval *object )
{
	php_orb_chan_obj *intern;

	intern = (php_orb_chan_obj *) 
		    zend_objects_get_address( object TSRMLS_CC );

	return intern->pktchan;
}

Orbstat *
get_this_orb_stat( zval *object )
{
	php_orb_stat_obj *intern;

	intern = (php_orb_stat_obj *) 
		    zend_objects_get_address( object TSRMLS_CC );

	return intern->os;
}

Orbsrc *
get_this_orb_source( zval *object )
{
	php_orb_source_obj *intern;

	intern = (php_orb_source_obj *) 
		    zend_objects_get_address( object TSRMLS_CC );

	return intern->os;
}

Orbclient *
get_this_orb_client( zval *object )
{
	php_orb_client_obj *intern;

	intern = (php_orb_client_obj *) 
		    zend_objects_get_address( object TSRMLS_CC );

	return intern->oc;
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
* }}} */

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

/* {{{ proto array orbreap( int orbfd ) */
PHP_FUNCTION(orbreap)
{
	long	orbfd;
	int	pktid;
	char	srcname[STRSZ];
	double	pkttime;
	char *pkt = 0;
	int	bufsize = 0;
	int	nbytes = 0;
	int	rc;
	int	argc = ZEND_NUM_ARGS();

	if( argc != 1 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "l", &orbfd )
	    == FAILURE) {

		return;
	}
	
	rc = orbreap( (int) orbfd, &pktid, srcname, &pkttime, 
		      &pkt, &nbytes, &bufsize );

	if( rc < 0 ) {
		
		zend_error( E_ERROR, "orbreap failed" );

		return;
	}

	array_init( return_value );

	add_next_index_long( return_value, pktid );
	add_next_index_string( return_value, srcname, 1 );
	add_next_index_double( return_value, pkttime );
	add_next_index_stringl( return_value, pkt, (uint) nbytes, 1 );
	add_next_index_long( return_value, nbytes );

	if( pkt != 0 ) {

		free( pkt );
	}

	return;
}
/* }}} */

/* {{{ proto array orbreap_timeout( int orbfd, int maxseconds ) */
PHP_FUNCTION(orbreap_timeout)
{
	long	orbfd;
	long	maxseconds;
	int	pktid;
	char	srcname[STRSZ];
	double	pkttime;
	char *pkt = 0;
	int	bufsize = 0;
	int	nbytes = 0;
	int	rc;
	int	argc = ZEND_NUM_ARGS();

	if( argc != 2 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "ll", &orbfd, &maxseconds )
	    == FAILURE) {

		return;
	}
	
	rc = orbreap_timeout( (int) orbfd, (int) maxseconds, &pktid, 
			srcname, &pkttime, &pkt, &nbytes, &bufsize );

	if( rc < 0 ) {
		
		return;
	}

	array_init( return_value );

	add_next_index_long( return_value, pktid );
	add_next_index_string( return_value, srcname, 1 );
	add_next_index_double( return_value, pkttime );
	add_next_index_stringl( return_value, pkt, (uint) nbytes, 1 );
	add_next_index_long( return_value, nbytes );

	if( pkt != 0 ) {

		free( pkt );
	}

	return;
}
/* }}} */

/* {{{ proto array orbget( int orbfd, mixed which ) */
PHP_FUNCTION(orbget)
{
	long	orbfd;
	int	pktid;
	char	srcname[STRSZ];
	double	pkttime;
	char *pkt = 0;
	int	bufsize = 0;
	int	nbytes = 0;
	int	rc;
	zval	*zval_which;
	int	which;
	int	argc = ZEND_NUM_ARGS();

	if( argc != 2 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "lz", &orbfd, &zval_which )
	    == FAILURE) {

		return;
	}
	
	if( Z_TYPE_P( zval_which ) == IS_STRING ) {

		which = xlatname( Z_STRVAL_P( zval_which ), Orbconst, Orbconstn );

		if( which == -1 ) {

			zend_error( E_ERROR, 
		     	   "orbget 'which' code not recognized" );

			return;
		}
	
	} else if( Z_TYPE_P( zval_which ) == IS_LONG ) {

		which = Z_LVAL_P( zval_which );

	} else {

		zend_error( E_ERROR, 
		     "orget 'which' argument must be string or integer" );

		return;
	}

	rc = orbget( (int) orbfd, which, &pktid, srcname, &pkttime, 
		      &pkt, &nbytes, &bufsize );

	if( rc < 0 ) {
		
		zend_error( E_ERROR, "orbget failed" );

		return;
	}

	array_init( return_value );

	add_next_index_long( return_value, pktid );
	add_next_index_string( return_value, srcname, 1 );
	add_next_index_double( return_value, pkttime );
	add_next_index_stringl( return_value, pkt, (uint) nbytes, 1 );
	add_next_index_long( return_value, nbytes );

	if( pkt != 0 ) {

		free( pkt );
	}

	return;
}
/* }}} */

/* {{{ proto int orbput( int orbfd, string srcname, double time, string packet, int nbytes ) */
PHP_FUNCTION(orbput)
{
	long	orbfd;
	char	*srcname;
	int	*srcname_len;
	double	time;
	char	*packet;
	int	*packet_len;
	long	nbytes;
	int	rc;
	int	argc = ZEND_NUM_ARGS();

	if( argc != 5 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "lsdsl", 
		&orbfd, &srcname, &srcname_len, &time,
		&packet, &packet_len, &nbytes )
	    == FAILURE) {

		return;
	}
	
	rc = orbput( (int) orbfd, srcname, time, packet, (int) nbytes );

	RETURN_LONG( rc );
}
/* }}} */

/* {{{ proto int orbputx( int orbfd, string srcname, double time, string packet, int nbytes ) */
PHP_FUNCTION(orbputx)
{
	long	orbfd;
	char	*srcname;
	int	*srcname_len;
	double	time;
	char	*packet;
	int	*packet_len;
	long	nbytes;
	int	pktid;
	int	argc = ZEND_NUM_ARGS();

	if( argc != 5 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "lsdsl", 
		&orbfd, &srcname, &srcname_len, &time,
		&packet, &packet_len, &nbytes )
	    == FAILURE) {

		return;
	}
	
	pktid = orbputx( (int) orbfd, srcname, time, packet, (int) nbytes );

	RETURN_LONG( pktid );
}
/* }}} */

/* {{{ proto object orbstat( int orbfd ) */
PHP_FUNCTION(orbstat)
{
	long	orbfd;
	Orbstat *os = 0;
	int	rc;
	int	argc = ZEND_NUM_ARGS();

	if( argc != 1 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "l", &orbfd )
	    == FAILURE) {

		return;
	}
	
	rc = orbstat( (int) orbfd, &os );

	orbstat2zval( os, return_value );

	return;
}
/* }}} */

/* {{{ proto array orbsources( int orbfd ) */
PHP_FUNCTION(orbsources)
{
	long	orbfd;
	Orbsrc	*sources = 0;
	int	nsources;
	int	isource;
	double	when;
	zval	*list;
	zval	*asrc;
	int	rc;
	int	argc = ZEND_NUM_ARGS();

	if( argc != 1 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "l", &orbfd )
	    == FAILURE) {

		return;
	}
	
	rc = orbsources( (int) orbfd, &when, &sources, &nsources );

	MAKE_STD_ZVAL( list );

	array_init( list );

	for( isource = 0; isource < nsources; isource++ ) {

		MAKE_STD_ZVAL( asrc );

		orbsource2zval( sources + isource, asrc );

		add_next_index_zval( list, asrc );
	}

	array_init( return_value );

	add_next_index_double( return_value, when );
	add_next_index_zval( return_value, list );

	return;
}
/* }}} */

/* {{{ proto array orbclients( int orbfd ) */
PHP_FUNCTION(orbclients)
{
	long	orbfd;
	Orbclient *clients = 0;
	int	nclients;
	int	iclient;
	double	when;
	zval	*list;
	zval	*aclient;
	int	rc;
	int	argc = ZEND_NUM_ARGS();

	if( argc != 1 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "l", &orbfd )
	    == FAILURE) {

		return;
	}
	
	rc = orbclients( (int) orbfd, &when, &clients, &nclients );

	MAKE_STD_ZVAL( list );

	array_init( list );

	for( iclient = 0; iclient < nclients; iclient++ ) {

		MAKE_STD_ZVAL( aclient );

		orbclient2zval( clients + iclient, aclient );

		add_next_index_zval( list, aclient );
	}

	array_init( return_value );

	add_next_index_double( return_value, when );
	add_next_index_zval( return_value, list );

	return;
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
		     "orbseek 'which' argument must be string or integer" );

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

/* {{{ proto array pforbstat( int orbfd, int flags ) */
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

/* N.B. the following function needs to be lowercase; despite the case-
 * insensitivity of PHP functions to the interpreter, the Zend engine
 * only handles the implementation well if the C-side function name
 * is all lowercase. Thus 'unstuffpkt' instead of 'unstuffPkt': 
 */

/* {{{ proto array unstuffPkt( string srcname, double time, string packet, int nbytes ) */
PHP_FUNCTION(unstuffpkt)
{
	Packet	*pkt = 0;
	char	*srcname;
	int	*srcname_len;
	double	time;
	char	*packet;
	int	*packet_len;
	long	nbytes;
	int	rc;
	zval	*zval_pkt;
	int	argc = ZEND_NUM_ARGS();

	if( argc != 4 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "sdsl", 
		&srcname, &srcname_len, &time,
		&packet, &packet_len, &nbytes )
	    == FAILURE) {

		return;
	}
	
	rc = unstuffPkt( srcname, time, packet, (int) nbytes, &pkt );

	if( rc < 0 ) {
		
		zend_error( E_ERROR, "unstuffPkt failed" );

		return;
	}

	zval_pkt = pkt2zval( rc, srcname, pkt );

	array_init( return_value );

	add_next_index_long( return_value, rc );
	add_next_index_zval( return_value, zval_pkt );

	return;
}
/* }}} */

/* {{{ proto array orbpkt_string( string srcname, double time, string packet, int nbytes ) */
PHP_FUNCTION(orbpkt_string)
{
	char	*srcname;
	int	*srcname_len;
	double	time;
	char	*packet;
	int	*packet_len;
	long	nbytes;
	char	*forbpkt;
	char	*forbpkt_safe;
	int	argc = ZEND_NUM_ARGS();

	if( argc != 4 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "sdsl", 
		&srcname, &srcname_len, &time,
		&packet, &packet_len, &nbytes )
	    == FAILURE) {

		return;
	}
	
	forbpkt = forbpacket_string( srcname, time, packet, (int) nbytes );

	if( forbpkt == 0 ) {
		
		zend_error( E_ERROR, "orbpkt_string failed" );

		return;
	}

	forbpkt_safe = estrdup( forbpkt );

	free( forbpkt );

	RETURN_STRING( forbpkt_safe, 0 );
}
/* }}} */

PHP_METHOD(orb_pkt, packettype)
{
	Packet	*pkt;
	int	type;

	pkt = get_this_orb_pkt( getThis() );
	type = get_this_orb_pkt_type( getThis() );

	array_init( return_value );

	add_next_index_long( return_value, type );
	add_next_index_string( return_value, pkt->pkttype->desc, 1 );

	return;
}

PHP_METHOD(orb_pkt, time)
{
	Packet	*pkt;

	pkt = get_this_orb_pkt( getThis() );

	RETURN_DOUBLE( pkt->time );
}

PHP_METHOD(orb_pkt, nchannels)
{
	Packet	*pkt;

	pkt = get_this_orb_pkt( getThis() );

	RETURN_LONG( pkt->nchannels );
}

PHP_METHOD(orb_pkt, channels)
{
	Packet	*pkt;
	int	pkt_res;
	long	ichannel;
	char	msg[STRSZ];
	PktChannel *pktchan;
	int	argc = ZEND_NUM_ARGS();

	if( argc != 1 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "l", &ichannel )
	    == FAILURE) {

		return;
	}

	pkt = get_this_orb_pkt( getThis() );
	pkt_res = get_this_orb_pkt_res( getThis() );

	if( ichannel < 0 ) {

		zend_error( E_WARNING, "Channel number is less than 0\n" );

		ZVAL_NULL( return_value );

	} else if( pkt->channels == (Tbl *) NULL ) {

		zend_error( E_WARNING, "Empty channels structure in packet\n" );

		ZVAL_NULL( return_value );

	} else if( ichannel >= maxtbl( pkt->channels ) ) {

		sprintf( msg, "Channel index %ld too high; packet contains "
			      "only %ld channels\n", 
			      ichannel, maxtbl( pkt->channels ) );
		
		ZVAL_NULL( return_value );

	} else {

		pktchan = gettbl( pkt->channels, ichannel );

		if( pktchan == (PktChannel *) NULL ) {
			
			zend_error( E_WARNING, "Null PktChannel structure!\n" );

			ZVAL_NULL( return_value );

		} else {

			pktchan2zval( pktchan, pkt_res, return_value );
		}
	}

	return;
}

PHP_METHOD(orb_pkt, version)
{
	Packet	*pkt;

	pkt = get_this_orb_pkt( getThis() );

	RETURN_LONG( pkt->version );
}

PHP_METHOD(orb_pkt, string)
{
	Packet	*pkt;

	pkt = get_this_orb_pkt( getThis() );

	if( pkt->string != NULL ) {

		ZVAL_STRING( return_value, pkt->string, 1 );

	} else {

		ZVAL_NULL( return_value );
	}

	return;
}

PHP_METHOD(orb_pkt, dfile)
{
	Packet	*pkt;

	pkt = get_this_orb_pkt( getThis() );

	if( pkt->dfile != NULL && pkt->dfile_size > 0 ) {
		
		array_init( return_value );

		add_next_index_string( return_value, pkt->dfile, 1 );
		add_next_index_long( return_value, pkt->dfile_size );

	} else {
		
		ZVAL_NULL( return_value );
	}

	return;
}

PHP_METHOD(orb_pkt, pf)
{
	Packet	*pkt;
	char	name[STRSZ];

	pkt = get_this_orb_pkt( getThis() );

	sprintf( name, "%s::pf", PHP_ORB_PKT_NAME );

	if( pkt->pf != (Pf *) NULL ) {

		putPf_nofree( name, pkt->pf );

		pf2zval( pkt->pf, return_value );

	} else {

		ZVAL_NULL( return_value );
	}

	return;
}

PHP_METHOD(orb_pkt, db)
{
	Packet	*pkt;

	pkt = get_this_orb_pkt( getThis() );

	ZVAL_DBPTR( return_value, pkt->db );

	return;
}

PHP_METHOD(orb_pkt, parts)
{
	Packet	*pkt;

	pkt = get_this_orb_pkt( getThis() );

	array_init( return_value );

	add_next_index_string( return_value, pkt->parts.src_net, 1 );
	add_next_index_string( return_value, pkt->parts.src_sta, 1 );
	add_next_index_string( return_value, pkt->parts.src_chan, 1 );
	add_next_index_string( return_value, pkt->parts.src_loc, 1 );
	add_next_index_string( return_value, pkt->parts.src_suffix, 1 );
	add_next_index_string( return_value, pkt->parts.src_subcode, 1 );

	return;
}

PHP_METHOD(orb_channel, nsamp)
{
	PktChannel *pktchan;

	pktchan = get_this_orb_pktchan( getThis() );

	RETURN_LONG( pktchan->nsamp );
}

PHP_METHOD(orb_channel, iuser1)
{
	PktChannel *pktchan;

	pktchan = get_this_orb_pktchan( getThis() );

	RETURN_LONG( pktchan->iuser1 );
}

PHP_METHOD(orb_channel, iuser2)
{
	PktChannel *pktchan;

	pktchan = get_this_orb_pktchan( getThis() );

	RETURN_LONG( pktchan->iuser2 );
}

PHP_METHOD(orb_channel, iuser3)
{
	PktChannel *pktchan;

	pktchan = get_this_orb_pktchan( getThis() );

	RETURN_LONG( pktchan->iuser3 );
}

PHP_METHOD(orb_channel, data)
{
	PktChannel *pktchan;
	int	isamp;

	pktchan = get_this_orb_pktchan( getThis() );

	array_init( return_value );

	for( isamp = 0; isamp < pktchan->nsamp; isamp++ ) {

		add_next_index_long( return_value, pktchan->data[isamp] );
	}

	return;
}

PHP_METHOD(orb_channel, time)
{
	PktChannel *pktchan;

	pktchan = get_this_orb_pktchan( getThis() );

	RETURN_DOUBLE( pktchan->time );
}

PHP_METHOD(orb_channel, samprate)
{
	PktChannel *pktchan;

	pktchan = get_this_orb_pktchan( getThis() );

	RETURN_DOUBLE( pktchan->samprate );
}

PHP_METHOD(orb_channel, calib)
{
	PktChannel *pktchan;

	pktchan = get_this_orb_pktchan( getThis() );

	RETURN_DOUBLE( pktchan->calib );
}

PHP_METHOD(orb_channel, calper)
{
	PktChannel *pktchan;

	pktchan = get_this_orb_pktchan( getThis() );

	RETURN_DOUBLE( pktchan->calper );
}

PHP_METHOD(orb_channel, duser1)
{
	PktChannel *pktchan;

	pktchan = get_this_orb_pktchan( getThis() );

	RETURN_DOUBLE( pktchan->duser1 );
}

PHP_METHOD(orb_channel, duser2)
{
	PktChannel *pktchan;

	pktchan = get_this_orb_pktchan( getThis() );

	RETURN_DOUBLE( pktchan->duser2 );
}

PHP_METHOD(orb_channel, net)
{
	PktChannel *pktchan;

	pktchan = get_this_orb_pktchan( getThis() );

	RETURN_STRING( pktchan->net, 1 );
}

PHP_METHOD(orb_channel, sta)
{
	PktChannel *pktchan;

	pktchan = get_this_orb_pktchan( getThis() );

	RETURN_STRING( pktchan->sta, 1 );
}

PHP_METHOD(orb_channel, chan)
{
	PktChannel *pktchan;

	pktchan = get_this_orb_pktchan( getThis() );

	RETURN_STRING( pktchan->chan, 1 );
}

PHP_METHOD(orb_channel, loc)
{
	PktChannel *pktchan;

	pktchan = get_this_orb_pktchan( getThis() );

	RETURN_STRING( pktchan->loc, 1 );
}

PHP_METHOD(orb_channel, segtype)
{
	PktChannel *pktchan;

	pktchan = get_this_orb_pktchan( getThis() );

	RETURN_STRING( pktchan->segtype, 1 );
}

PHP_METHOD(orb_channel, cuser1)
{
	PktChannel *pktchan;

	pktchan = get_this_orb_pktchan( getThis() );

	RETURN_STRING( pktchan->cuser1, 1 );
}

PHP_METHOD(orb_channel, cuser2)
{
	PktChannel *pktchan;

	pktchan = get_this_orb_pktchan( getThis() );

	RETURN_STRING( pktchan->cuser2, 1 );
}

PHP_METHOD(orb_stat, when)
{
	Orbstat *os;

	os = get_this_orb_stat( getThis() );

	RETURN_DOUBLE( os->when );
}

PHP_METHOD(orb_stat, started)
{
	Orbstat *os;

	os = get_this_orb_stat( getThis() );

	RETURN_DOUBLE( os->started );
}

PHP_METHOD(orb_stat, orb_start)
{
	Orbstat *os;

	os = get_this_orb_stat( getThis() );

	RETURN_DOUBLE( os->orb_start );
}

PHP_METHOD(orb_stat, address)
{
	Orbstat *os;
	char	*address;
	struct in_addr addr;

	os = get_this_orb_stat( getThis() );

	memcpy( &addr, &os->address, sizeof( struct in_addr ) );

	address = inet_ntoa( addr );

	RETURN_STRING( address, 1 );
}

PHP_METHOD(orb_stat, version)
{
	Orbstat *os;

	os = get_this_orb_stat( getThis() );

	RETURN_STRING( os->version, 1 );
}

PHP_METHOD(orb_stat, who)
{
	Orbstat *os;

	os = get_this_orb_stat( getThis() );

	RETURN_STRING( os->who, 1 );
}

PHP_METHOD(orb_stat, host)
{
	Orbstat *os;

	os = get_this_orb_stat( getThis() );

	RETURN_STRING( os->host, 1 );
}

PHP_METHOD(orb_stat, connections)
{
	Orbstat *os;

	os = get_this_orb_stat( getThis() );

	RETURN_LONG( os->connections );
}

PHP_METHOD(orb_stat, messages)
{
	Orbstat *os;

	os = get_this_orb_stat( getThis() );

	RETURN_LONG( os->messages );
}

PHP_METHOD(orb_stat, maxdata)
{
	Orbstat *os;

	os = get_this_orb_stat( getThis() );

	RETURN_LONG( os->maxdata );
}

PHP_METHOD(orb_stat, errors)
{
	Orbstat *os;

	os = get_this_orb_stat( getThis() );

	RETURN_LONG( os->errors );
}

PHP_METHOD(orb_stat, rejected)
{
	Orbstat *os;

	os = get_this_orb_stat( getThis() );

	RETURN_LONG( os->rejected );
}

PHP_METHOD(orb_stat, closes)
{
	Orbstat *os;

	os = get_this_orb_stat( getThis() );

	RETURN_LONG( os->closes );
}

PHP_METHOD(orb_stat, opens)
{
	Orbstat *os;

	os = get_this_orb_stat( getThis() );

	RETURN_LONG( os->opens );
}

PHP_METHOD(orb_stat, port)
{
	Orbstat *os;

	os = get_this_orb_stat( getThis() );

	RETURN_LONG( os->port );
}

PHP_METHOD(orb_stat, pid)
{
	Orbstat *os;

	os = get_this_orb_stat( getThis() );

	RETURN_LONG( os->pid );
}

PHP_METHOD(orb_stat, nsources)
{
	Orbstat *os;

	os = get_this_orb_stat( getThis() );

	RETURN_LONG( os->nsources );
}

PHP_METHOD(orb_stat, nclients)
{
	Orbstat *os;

	os = get_this_orb_stat( getThis() );

	RETURN_LONG( os->nclients );
}

PHP_METHOD(orb_stat, maxsrc)
{
	Orbstat *os;

	os = get_this_orb_stat( getThis() );

	RETURN_LONG( os->maxsrc );
}

PHP_METHOD(orb_stat, maxpkts)
{
	Orbstat *os;

	os = get_this_orb_stat( getThis() );

	RETURN_LONG( os->maxpktid );
}

PHP_METHOD(orb_source, srcname)
{
	Orbsrc *os;

	os = get_this_orb_source( getThis() );

	RETURN_STRING( os->srcname, 1 );
}

PHP_METHOD(orb_source, slatest_time)
{
	Orbsrc *os;

	os = get_this_orb_source( getThis() );

	RETURN_DOUBLE( os->slatest_time );
}

PHP_METHOD(orb_source, soldest_time)
{
	Orbsrc *os;

	os = get_this_orb_source( getThis() );

	RETURN_DOUBLE( os->soldest_time );
}

PHP_METHOD(orb_source, active)
{
	Orbsrc *os;

	os = get_this_orb_source( getThis() );

	RETURN_LONG( os->active );
}

PHP_METHOD(orb_source, npkts)
{
	Orbsrc *os;

	os = get_this_orb_source( getThis() );

	RETURN_LONG( os->npkts );
}

PHP_METHOD(orb_source, nbytes)
{
	Orbsrc *os;

	os = get_this_orb_source( getThis() );

	RETURN_LONG( os->nbytes );
}

PHP_METHOD(orb_source, slatest)
{
	Orbsrc *os;

	os = get_this_orb_source( getThis() );

	RETURN_LONG( os->slatest );
}

PHP_METHOD(orb_source, soldest)
{
	Orbsrc *os;

	os = get_this_orb_source( getThis() );

	RETURN_LONG( os->soldest );
}

PHP_METHOD(orb_client, lastpkt)
{
	Orbclient *oc;

	oc = get_this_orb_client( getThis() );

	RETURN_DOUBLE( oc->lastpkt );
}

PHP_METHOD(orb_client, started)
{
	Orbclient *oc;

	oc = get_this_orb_client( getThis() );

	RETURN_DOUBLE( oc->started );
}

PHP_METHOD(orb_client, address)
{
	Orbclient *oc;
	char	*address;
	struct in_addr addr;

	oc = get_this_orb_client( getThis() );

	memcpy( &addr, &oc->address, sizeof( struct in_addr ) );

	address = inet_ntoa( addr );

	RETURN_STRING( address, 1 );
}

PHP_METHOD(orb_client, perm)
{
	Orbclient *oc;
	char	perm_string[2];

	oc = get_this_orb_client( getThis() );

	sprintf( perm_string, "%c", oc->perm );

	RETURN_STRING( perm_string, 1 );
}

PHP_METHOD(orb_client, what)
{
	Orbclient *oc;

	oc = get_this_orb_client( getThis() );

	RETURN_STRING( oc->what, 1 );
}

PHP_METHOD(orb_client, host)
{
	Orbclient *oc;

	oc = get_this_orb_client( getThis() );

	RETURN_STRING( oc->host, 1 );
}

PHP_METHOD(orb_client, who)
{
	Orbclient *oc;

	oc = get_this_orb_client( getThis() );

	RETURN_STRING( oc->who, 1 );
}

PHP_METHOD(orb_client, select)
{
	Orbclient *oc;

	oc = get_this_orb_client( getThis() );

	RETURN_STRING( oc->select, 1 );
}

PHP_METHOD(orb_client, reject)
{
	Orbclient *oc;

	oc = get_this_orb_client( getThis() );

	RETURN_STRING( oc->reject, 1 );
}

PHP_METHOD(orb_client, read)
{
	Orbclient *oc;

	oc = get_this_orb_client( getThis() );

	RETURN_LONG( oc->read );
}

PHP_METHOD(orb_client, pid)
{
	Orbclient *oc;

	oc = get_this_orb_client( getThis() );

	RETURN_LONG( oc->pid );
}

PHP_METHOD(orb_client, bytes)
{
	Orbclient *oc;

	oc = get_this_orb_client( getThis() );

	RETURN_LONG( oc->bytes );
}

PHP_METHOD(orb_client, packets)
{
	Orbclient *oc;

	oc = get_this_orb_client( getThis() );

	RETURN_LONG( oc->packets );
}

PHP_METHOD(orb_client, pktid)
{
	Orbclient *oc;

	oc = get_this_orb_client( getThis() );

	RETURN_LONG( oc->pktid );
}

PHP_METHOD(orb_client, port)
{
	Orbclient *oc;

	oc = get_this_orb_client( getThis() );

	RETURN_LONG( oc->port );
}

PHP_METHOD(orb_client, thread)
{
	Orbclient *oc;

	oc = get_this_orb_client( getThis() );

	RETURN_LONG( oc->thread );
}

PHP_METHOD(orb_client, fd)
{
	Orbclient *oc;

	oc = get_this_orb_client( getThis() );

	RETURN_LONG( oc->fd );
}

PHP_METHOD(orb_client, nreject)
{
	Orbclient *oc;

	oc = get_this_orb_client( getThis() );

	RETURN_LONG( oc->nreject );
}

PHP_METHOD(orb_client, nselect)
{
	Orbclient *oc;

	oc = get_this_orb_client( getThis() );

	RETURN_LONG( oc->nselect );
}

PHP_METHOD(orb_client, errors)
{
	Orbclient *oc;

	oc = get_this_orb_client( getThis() );

	RETURN_LONG( oc->errors );
}

PHP_METHOD(orb_client, priority)
{
	Orbclient *oc;

	oc = get_this_orb_client( getThis() );

	RETURN_LONG( oc->priority );
}

PHP_METHOD(orb_client, lastrequest)
{
	Orbclient *oc;

	oc = get_this_orb_client( getThis() );

	RETURN_LONG( oc->lastrequest );
}

PHP_METHOD(orb_client, nrequests)
{
	Orbclient *oc;

	oc = get_this_orb_client( getThis() );

	RETURN_LONG( oc->nrequests );
}

PHP_METHOD(orb_client, nwrites)
{
	Orbclient *oc;

	oc = get_this_orb_client( getThis() );

	RETURN_LONG( oc->nwrites );
}

PHP_METHOD(orb_client, nreads)
{
	Orbclient *oc;

	oc = get_this_orb_client( getThis() );

	RETURN_LONG( oc->nreads );
}

PHP_METHOD(orb_client, written)
{
	Orbclient *oc;

	oc = get_this_orb_client( getThis() );

	RETURN_LONG( oc->written );
}

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

/* {{{ proto string join_srcname( string net,
				  string sta,
				  string chan,
				  string loc,
				  string suffix,
				  string subcode ) */
PHP_FUNCTION(join_srcname)
{
	char	*net;
	char	*sta;
	char	*chan;
	char	*loc;
	char	*suffix;
	char	*subcode;
	int	net_len;
	int	sta_len;
	int	chan_len;
	int	loc_len;
	int	suffix_len;
	int	subcode_len;
	Srcname parts;
	char	srcname[ORBSRCNAME_SIZE];
	int	argc = ZEND_NUM_ARGS();

	if( argc != 6 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "ssssss", 
					&net, &net_len,
					&sta, &sta_len,
					&chan, &chan_len,
					&loc, &loc_len,
					&suffix, &suffix_len,
					&subcode, &subcode_len )
	    == FAILURE) {

		return;
	}

	strcpy( parts.src_net, net );
	strcpy( parts.src_sta, sta );
	strcpy( parts.src_chan, chan );
	strcpy( parts.src_loc, loc );
	strcpy( parts.src_suffix, suffix );
	strcpy( parts.src_subcode, subcode );

	join_srcname( &parts, srcname );

	RETURN_STRING( srcname, 1 );
}
/* }}} */

/* {{{ proto array suffix2pkttype( string suffix ) */
PHP_FUNCTION(suffix2pkttype)
{
	char	*suffix;
	int	suffix_len;
	PacketType *pktt;
	int	argc = ZEND_NUM_ARGS();

	if( argc != 1 ) {

		WRONG_PARAM_COUNT;
	}

	if( zend_parse_parameters( argc TSRMLS_CC, "s", 
					&suffix, &suffix_len )
	    == FAILURE) {

		return;
	}

	pktt = suffix2pkttype( suffix );

	array_init( return_value );

	add_assoc_string_ex( return_value, 
			     "name", strlen( "name" ) + 1, 
			     pktt->name,  1 );

	add_assoc_long( return_value, "content", pktt->content );

	add_assoc_string_ex( return_value, 
			     "desc", strlen( "desc" ) + 1, 
			     pktt->desc, 1 );

	return;
}
/* }}} */


/* local variables
 * End:
 * vim600: fdm=marker
 */
