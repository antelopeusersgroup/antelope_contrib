/*
 * orb2ew.c
 *
 * Kent Lindquist
 * Lindquist Consulting
 * 2003
 *
 */

#include <stdlib.h>

#include "orbew.h"

#define DATATYPE "s4" 
#define DATASIZE 4
#define SOCKET_LISTEN_BACKLOG 10
#define SERVER_RESET_ALLOWANCE_SEC 1
#define VERYVERBOSE_DEBUGBNS 0

#define THR_PRIORITY_EXPORT_SERVER 1
#define THR_PRIORITY_PFWATCH 2

typedef struct ExportServerThread {

	/* Shared variables: */

	mutex_t	es_mutex;
	thread_t thread_id;
	Pf	*pf;
	int	update;
	int	new;
	int	stop;

	/* Thread-only variables: */

	char	name[STRSZ];			
	char	server_ipaddress[STRSZ];
	in_port_t server_port;	
	char	expect_heartbeat_string[STRSZ];	
	Hook	*expect_heartbeat_hook;
	char	send_heartbeat_string[STRSZ];	
	int	send_heartbeat_sec;	
	double	last_heartbeat_sent;
	double	last_heartbeat_received;
	char	select[STRSZ];
	char	reject[STRSZ];
	char	my_inst_str[STRSZ];
	char	my_mod_str[STRSZ];
	int	my_inst;	
	int	my_mod;	
	int	my_type_heartbeat;		
	int	my_type_tracebuf;		
	int	so;
	struct sockaddr_in sin;
	char	*buf;	
	int 	bufsize;
	int	nbytes;
	enum Loglevel loglevel;

	Arr	*Export_Threads;

} ExportServerThread;

typedef struct ExportThread {

	ExportServerThread *es;
	thread_t thread_id;
	char	name[STRSZ];
	int	so;
	int	orbfd;
	Bns	*bnsin;			
	Bns	*bnsout;			

} ExportThread;

char	*Pfname = "orb2ew";
char	*Orbname = 0;
Arr	*Export_Server_Threads;
rwlock_t Export_Server_Threads_rwlock;
Arr	*Pins;
rwlock_t Pins_rwlock;

static void
usage() 
{
	cbanner( "$Date$", 
		 "[-p pfname] orb",
		 "Kent Lindquist", 
		 "Lindquist Consulting", 
		 "kent@lindquistconsulting.com" );
	
	return;
}

static int 
get_pinno( PktChannel *pktchan )
{
	char	srcname[STRSZ];
	int	pinno;

	sprintf( srcname, "%s_%s_%s", 
			  pktchan->net, 
			  pktchan->sta, 
			  pktchan->chan );

	rw_rdlock( &Pins_rwlock );

	pinno = (int) getarr( Pins, srcname );

	rw_unlock( &Pins_rwlock );

	return pinno;
}

int
pktchan_to_tracebuf( PktChannel *pktchan,
		     TracePacket *tp,
		     double starttime, 
		     int *nbytes )
{
	char	*datap;
	int	dsize_bytes;
	double	endtime;
	int	pinno;
	char	*ptr;

	strcpy( tp->trh.datatype, DATATYPE );

	pinno = get_pinno( pktchan );
	ptr = (char *) &tp->trh.pinno;
	hi2mi( &pinno, &ptr, 1 );

	strcpy( tp->trh.sta, pktchan->sta );
	strcpy( tp->trh.chan, pktchan->chan );
	strcpy( tp->trh.net, pktchan->net );

	ptr = (char *) &tp->trh.samprate;
	hd2md( &pktchan->samprate, &ptr, 1 );

	ptr = (char *) &tp->trh.nsamp;
	hi2mi( &pktchan->nsamp, &ptr, 1 );

	ptr = (char *) &tp->trh.starttime;
	hd2md( &starttime, &ptr, 1 );

	ptr = (char *) &tp->trh.endtime;
	endtime = ENDTIME( starttime, pktchan->samprate, pktchan->nsamp );
	hd2md( &endtime, &ptr, 1 );

	strcpy( tp->trh.quality, "" );
	strcpy( tp->trh.pad, "" );

	datap = &tp->msg[0] + sizeof( TRACE_HEADER );
	dsize_bytes = DATASIZE * tp->trh.nsamp;

	*nbytes = dsize_bytes + sizeof( TRACE_HEADER );

	hi2mi( pktchan->data, &datap, tp->trh.nsamp );

	return 0;
}

static ExportThread *
new_ExportThread( ExportServerThread *es, int so )
{
	ExportThread *et;

	allot( ExportThread *, et, 1 );

	strcpy( et->name, "" );

	et->es = es; 
	et->so = so;
	et->orbfd = -1;
	et->bnsin = NULL;
	et->bnsout = NULL;

	return et;
}

static void
free_ExportThread( void *arg ) 
{
	ExportThread *et = (ExportThread *) arg;

	free( et );

	return;
}

static ExportServerThread *
new_ExportServerThread( char *name )
{
	ExportServerThread *es;

	allot( ExportServerThread *, es, 1 );

	strcpy( es->name, name );
	strcpy( es->select, "" );
	strcpy( es->reject, "" );

	es->update = 1;
	es->new = 1;
	es->stop = 0;
	es->pf = pfnew( PFARR );
	es->buf = 0;
	es->bufsize = 0;
	es->nbytes = 0;
	es->loglevel = QUIET;
	es->thread_id = -1;
	es->last_heartbeat_sent = 0;
	es->last_heartbeat_received = 0;
	es->expect_heartbeat_hook = NULL;

	es->Export_Threads = newarr( 0 );

	mutex_init( &es->es_mutex, USYNC_THREAD, NULL );

	return es;
}

static void
free_ExportServerThread( ExportServerThread **es )
{

	mutex_destroy( &(*es)->es_mutex );

	if( (*es)->pf ) {

		pffree( (*es)->pf );
	}

	if( (*es)->buf ) {

		free( (*es)->buf );
	}

	if( (*es)->Export_Threads ) {
		
		freearr( (*es)->Export_Threads, free_ExportThread );
	}

	free( *es );

	*es = 0;

	return;
}

static void
add_export_server_thread( char *name, ExportServerThread *es ) 
{
	rw_wrlock( &Export_Server_Threads_rwlock );

	setarr( Export_Server_Threads, name, (void *) es );

	rw_unlock( &Export_Server_Threads_rwlock );

	return;
}

ExportServerThread *
find_export_server_thread_byid( thread_t tid )
{
	ExportServerThread *es;
	ExportServerThread *found = (ExportServerThread *) NULL;
	Tbl	*keys;
	char	*key;
	int 	i;

	rw_rdlock( &Export_Server_Threads_rwlock );

	keys = keysarr( Export_Server_Threads );

	for( i = 0; i < maxtbl( keys ); i++ ) {

		key = gettbl( keys, i );

		es = (ExportServerThread *) getarr( Export_Server_Threads, key );

		if( es->thread_id == tid ) {

			found = es;

			break;
		}
	}

	freetbl( keys, 0 );

	rw_unlock( &Export_Server_Threads_rwlock );

	return found;
}

ExportServerThread *
find_export_server_thread_byname( char *name )
{
	ExportServerThread *es;

	rw_rdlock( &Export_Server_Threads_rwlock );

	es = getarr( Export_Server_Threads, name ); 

	rw_unlock( &Export_Server_Threads_rwlock );

	return es;
}

static void
stop_export_server_thread( char *name )
{
	ExportServerThread *es;

	if( ( es = find_export_server_thread_byname( name ) ) == 0 ) { 

		elog_complain( 1, "stop_export_server_thread: "
			  "Couldn't find export_server thread '%s' in registry\n",
			  name );
		return;
	}

	if( Flags.VeryVerbose ) {

		elog_notify( 0, "'%s': Request sent to stop export_server thread, "
			     "thread-id %d\n", name, es->thread_id );
	}

	mutex_lock( &es->es_mutex );

	es->stop = 1;

	mutex_unlock( &es->es_mutex );

	return;
}

static Tbl *
export_server_thread_names() 
{
	Tbl	*keys;
	Tbl	*dup;

	rw_rdlock( &Export_Server_Threads_rwlock );

	keys = keysarr( Export_Server_Threads );

	dup = duptbl( keys, (void *(*)()) strdup );

	freetbl( keys, 0 );

	rw_unlock( &Export_Server_Threads_rwlock );

	return dup;
}

static int
num_export_server_threads()
{
	int	nthreads;

	rw_rdlock( &Export_Server_Threads_rwlock );

	nthreads = cntarr( Export_Server_Threads );

	rw_unlock( &Export_Server_Threads_rwlock );

	return nthreads;
}

static void 
stop_all_export_server_threads()
{
	int	i;
	Tbl	*keys;

	keys = export_server_thread_names();

	for( i = 0; i < maxtbl( keys ); i++ ) {

		stop_export_server_thread( gettbl( keys, i ) );
	}

	freetbl( keys, free );

	return;
}

static void
delete_export_server_thread( ExportServerThread *es ) 
{
	if( es != (ExportServerThread *) NULL ) {

		mutex_trylock( &es->es_mutex );
		mutex_unlock( &es->es_mutex );

		rw_wrlock( &Export_Server_Threads_rwlock );

		delarr( Export_Server_Threads, es->name );

		free_ExportServerThread( &es );

		rw_unlock( &Export_Server_Threads_rwlock );
	}

	return;
}

void 
close_export_connection( ExportThread *et )
{
	char	eof = 004;

	bnsput( et->bnsout, &eof, BYTES, 1 );
	bnsflush( et->bnsout );

	bnsfree( et->bnsout );

	/* Keep import_generic from interpreting flushed data
	   as a malformed heartbeat: */

	bnsclr( et->bnsin );

	bnsclose( et->bnsin );

	et->bnsin = 0;

	delarr( et->es->Export_Threads, et->name );

	return;
}

void
close_export_server_connection( ExportServerThread *es )
{
	ExportThread *et;
	Tbl	*keys;
	char	*akey;
	int	i;

	keys = keysarr( es->Export_Threads );

	for( i = 0; i < maxtbl( keys ); i++ ) {

		akey = gettbl( keys, i );

		et = getarr( es->Export_Threads, akey );

		close_export_connection( et );

		thr_kill( et->thread_id, SIGKILL );
	}

	freetbl( keys, 0 );

	if( es->stop == 0 ) {
		
		sleep( SERVER_RESET_ALLOWANCE_SEC );
	}

	return;
}

static void
orb2ew_export_server_shutdown()
{
	int	status = 0;
	ExportServerThread *es;
	char	name[STRSZ] = "Unknown";

	if( ( es = find_export_server_thread_byid( thr_self() ) ) == NULL ) {

		elog_complain( 0, "Couldn't find thread %d in registry!\n",
			  	  thr_self() );

	} else {
	
		close_export_server_connection( es ); 

		if( es->name != NULL ) {

			strcpy( name, es->name );
		}
	}

	if( Flags.verbose ) {

		elog_notify( 0, 
		"'%s': Thread (thread-id %d) stopping at user request\n",
		  name, thr_self() );
	}

	delete_export_server_thread( es );

	thr_exit( (void *) &status );

	return;
}

static void
reconfig_export_server_thread( ExportServerThread *es )
{
	char	*loglevel;

	mutex_lock( &es->es_mutex );

	if( es->stop == 1 ) {

		orb2ew_export_server_shutdown();
	}

	if( es->update == 1 ) {

		if( es->loglevel >= VERBOSE ) {

			if( es->new ) {
					
				elog_notify( 0, 
			  	"'%s': Configuring thread with: ", 
			  	es->name );

			} else {

				elog_notify( 0, 
			  	"'%s': Reconfiguring thread with: ", 
			  	es->name );

			}

			pfout( stderr, es->pf );
		}
			
		close_export_server_connection( es ); 

		strcpy( es->server_ipaddress,
			pfget_string( es->pf, "server_ipaddress" ) );

		es->server_port = 
			(in_port_t) pfget_int( es->pf, "server_port" );

		strcpy( es->expect_heartbeat_string,
			pfget_string( es->pf, "expect_heartbeat_string" ) );

		if( es->expect_heartbeat_hook ) {

			free_hook( &es->expect_heartbeat_hook );
		}

		strcpy( es->send_heartbeat_string,
			pfget_string( es->pf, "send_heartbeat_string" ) );

		es->send_heartbeat_sec = 
			pfget_int( es->pf, "send_heartbeat_sec" );

		strcpy( es->select, pfget_string( es->pf, "select" ) );

		strcpy( es->reject, pfget_string( es->pf, "reject" ) );

		strcpy( es->my_inst_str,
			pfget_string( es->pf, "my_inst" ) );

		strcpy( es->my_mod_str,
			pfget_string( es->pf, "my_mod" ) );

		ewlogo_tologo( es->my_inst_str, 
			       es->my_mod_str, 
			       Default_TYPE_HEARTBEAT, 
			       &es->my_inst,
			       &es->my_mod,
			       &es->my_type_heartbeat );

		ewlogo_tologo( es->my_inst_str, 
			       es->my_mod_str, 
			       Default_TYPE_TRACEBUF, 
			       &es->my_inst,
			       &es->my_mod,
			       &es->my_type_tracebuf );

		loglevel = pfget_string( es->pf, "loglevel" );

		es->loglevel = translate_loglevel( loglevel );
		
		es->update = 0;
		es->new = 0;
	}

	mutex_unlock( &es->es_mutex );

	return;
}

int
buf_send( ExportThread *et, TracePacket *tp, int nbytes_tp )
{
	char	msg[STRSZ];
	char	stx = STX;	
	char	etx = ETX;	
	char	esc = ESC;	
	char	*cp;
	int	rc;
	int	retcode = 0;
	int	i;

	sprintf( msg, "%c%3d%3d%3d", 
			stx,
			et->es->my_inst,
			et->es->my_mod,
			et->es->my_type_tracebuf );
	
	cp = (char *) tp;

	rc = bnsput( et->bnsout, msg, BYTES, 10 );
	retcode += rc;

	if( rc != 0 ) {

		elog_complain( 1, "'%s': bnsput error %d\n",
				et->name, bnserrno( et->bnsout ) );
	}

	for( i = 0; i < nbytes_tp; i++ ) {

		if( *cp == STX || *cp == ETX || *cp == ESC ) {

			rc = bnsput( et->bnsout, &esc, BYTES, 1 );
			retcode += rc;

			if( rc != 0 ) {

				elog_complain( 1, "'%s': bnsput error %d\n", 
						et->name, bnserrno( et->bnsout ) );
			}

			rc = bnsput( et->bnsout, cp++, BYTES, 1 );
			retcode += rc;

			if( rc != 0 ) {

				elog_complain( 1, "'%s': bnsput error %d\n", 
						et->name, bnserrno( et->bnsout ) );
			}

		} else {

			rc = bnsput( et->bnsout, cp++, BYTES, 1 );
			retcode += rc;

			if( rc != 0 ) {

				elog_complain( 1, "'%s': bnsput error %d\n", 	
						et->name, bnserrno( et->bnsout ) );
			}
		}
	}
	
	rc = bnsput( et->bnsout, &etx, BYTES, 1 );
	retcode += rc;

	if( rc != 0 ) {

		elog_complain( 1, "'%s': bnsput error %d\n", 
				et->name, bnserrno( et->bnsout ) );
	}

	rc = bnsflush( et->bnsout );
	retcode += rc;

	if( rc != 0 ) {

		elog_complain( 1, "'%s': bnsflush error %d\n",
				et->name, bnserrno( et->bnsout ) );
	}

	return retcode;
}

static void *
orb2ew_export( void *arg )
{
	ExportThread *et = (ExportThread *) arg;
	int	rc;
	int     pktid;
	char    srcname[STRSZ];
	double  mytime;
	char    *rawpkt = NULL;
	struct Packet *Pkt = NULL;
	struct PktChannel *pktchan = NULL;
	int	ichan;
	int     bufsize = 0;
	int	nbytes_orb = 0;
	int	nbytes_tp = 0;
	TracePacket tp;
	int	status = 0;
	int	pinno = 0;
	char	*ptr;

	if( ( et->orbfd = orbopen( Orbname, "r&" ) ) < 0 ) {
		
		elog_complain( 0,
			"'%s': Failed to open orb '%s' for reading\n",
			et->name, Orbname );

		status = -1;

		thr_exit( (void *) &status );
	}

	if( strcmp( et->es->select, "" ) ) {

		rc = orbselect( et->orbfd, et->es->select );
		
		if( et->es->loglevel == VERBOSE || Flags.verbose ) {

			elog_notify( 0, 
			  "'%s': %d sources selected after orbselect for '%s'\n", 
			  et->name, rc, et->es->select );
		}
	}

	if( strcmp( et->es->reject, "" ) ) {

		rc = orbreject( et->orbfd, et->es->reject );
		
		if( et->es->loglevel == VERBOSE || Flags.verbose ) {

			elog_notify( 0, 
			  "'%s': %d sources selected after orbreject on '%s'\n", 
			  et->name, rc, et->es->reject );
		}
	}

	et->bnsin = bnsnew( et->so, BNS_BUFFER_SIZE ); 
	et->bnsout = bnsnew( et->so, BNS_BUFFER_SIZE ); 

	bnsuse_sockio( et->bnsin );
	bnsuse_sockio( et->bnsout );

	if( et->es->loglevel == VERYVERBOSE && VERYVERBOSE_DEBUGBNS ) { 

		et->bnsin->debug = 1;
		et->bnsout->debug = 1;
	}

	if( et->es->send_heartbeat_sec*1000 < DEFAULT_BNS_TIMEOUT ) {

		bnstimeout( et->bnsin, 
			et->es->send_heartbeat_sec * 1000 );

	} else {

		bnstimeout( et->bnsin, DEFAULT_BNS_TIMEOUT );
	}

	for( ;; ) {

		rc = orbreap( et->orbfd, &pktid, srcname, &mytime,
			&rawpkt, (int *)&nbytes_orb, &bufsize );

		if( rc < 0 )
		{
			clear_register( 1 );
			continue;
		}

		rc = unstuffPkt( srcname, mytime, rawpkt, nbytes_orb, &Pkt );
		if( rc == 0 )
		{
			elog_complain( 0, 
				"'%s': Unstuff failure in orb2ew for %s\n",
				et->name, srcname );
			continue;
		}

		for( ichan = 0; ichan < Pkt->nchannels; ichan++ )
		{
			pktchan = gettbl( Pkt->channels, ichan );

			if( pktchan_to_tracebuf( pktchan, &tp,
						 mytime, &nbytes_tp  ) )
			{
				continue;
			}

			if( ( et->es->loglevel >= VERYVERBOSE ) || 
				Flags.VeryVerbose ) {

				ptr = (char *) &tp.trh.pinno;
				mi2hi( &ptr, &pinno, 1 );

				elog_notify( 0, 
					"'%s': Sending packet %s timed %s as pin %d\n", 
					et->name, srcname, strtime( mytime ), pinno );
			}

			rc = buf_send( et, &tp, nbytes_tp );

			if( rc != 0 ) {

				goto close_export;
			}
		}
	}

	close_export:

	if( et->es->loglevel >= VERBOSE || Flags.verbose ) {

		elog_notify( 0, "'%s': Closing export connection\n",
				et->name );
	}

	close_export_connection( et );

	status = 0;

	thr_exit( (void *) &status );
}

static void
refresh_export_server_thread( ExportServerThread *es )
{
	int	rc;
	int	aso;
	struct sockaddr_in client;
	socklen_t clientlen = sizeof( client );
	int	on = 1;
	ExportThread *et;

	reconfig_export_server_thread( es );

	memset( &es->sin, 0, sizeof( struct sockaddr ) );
	memset( &client, 0, sizeof( struct sockaddr ) );

	es->so = socket( PF_INET, SOCK_STREAM, 0 );

	es->sin.sin_family = AF_INET;

	es->sin.sin_port = htons( es->server_port ); 

	if( strcmp( es->server_ipaddress, "" ) ) {

		es->sin.sin_addr.s_addr = inet_addr( es->server_ipaddress );

	} else {

		es->sin.sin_addr.s_addr = htonl( INADDR_ANY ); 
	}

	rc = setsockopt( es->so, SOL_SOCKET, SO_REUSEADDR, 
			 &on, sizeof( char *) );

	rc = bind( es->so, (struct sockaddr *) &es->sin, sizeof( es->sin ) );

	rc = listen( es->so, SOCKET_LISTEN_BACKLOG ); 

	for( ;; ) {

		aso = accept( es->so, (struct sockaddr *) &client, &clientlen ); 
		if( aso < 0 ) {
			
			elog_complain( 0, 
				"'%s': Failed socket fd %d on accept\n", 
				es->name, aso );
			continue;	
		}

		if( ( es->loglevel >= VERBOSE ) || Flags.verbose ) {

			 elog_notify( 0, "'%s': Accepted socket fd %d\n", 
					es->name, aso );
		}
		
		et = new_ExportThread( es, aso );

		rc = thr_create( NULL, 0, orb2ew_export, 
				  (void *) et, 
				  THR_DETACHED,
				  &et->thread_id );

		if( rc != 0 ) {

			elog_complain( 1,
			    "'%s': Failed to create export thread: "
			    "thr_create error %d\n", es->name, rc );
			
			free_ExportThread( &et );

			continue;

		} else {

			sprintf( et->name, "%s_tid_%d", 
				 es->name, et->thread_id );

			mutex_lock( &es->es_mutex );

			setarr( es->Export_Threads, et->name, et );
		
			mutex_unlock( &es->es_mutex );

			thr_continue( et->thread_id );
		}
	}
}

static void *
orb2ew_export_server( void *arg )
{
	char	*name = (char *) arg;
	ExportServerThread *es;
	int	status = 0;
	int	rc;

	thr_setprio( thr_self(), THR_PRIORITY_EXPORT_SERVER );

	if( Flags.verbose ) {

		elog_notify( 0,
			  "'%s':...orb2ew_export_server thread started\n",
			  name );
	}

	if( ( es = find_export_server_thread_byid( thr_self() ) ) == NULL ) {

		elog_complain( 1, 
			"Couldn't find thread id %d in registry!\n",
			 thr_self() );

		status = -1;

		thr_exit( (void *) &status );
	} 

	for( ;; ) {

		refresh_export_server_thread( es );
	}
}

static void
update_export_server_thread( char *name, Pf *pf )
{
	ExportServerThread *es;
	Pf	*oldpf;
	char	key[STRSZ];
	int	ret;

	if( ( es = find_export_server_thread_byname( name ) ) == 0 ) {

		es = new_ExportServerThread( name );

		pfput_int( es->pf, "server_port", DEFAULT_SERVER_PORT );

		pfput_string( es->pf, "server_ipaddress", "" );

		pfput_int( es->pf, 
			   "send_heartbeat_sec", 
			   DEFAULT_SEND_HEARTBEAT_SEC );

		pfput_int( es->pf, 
			   "expect_heartbeat_sec", 
			   DEFAULT_EXPECT_HEARTBEAT_SEC );

		pfput_string( es->pf, 
			      "send_heartbeat_string", 
			      DEFAULT_SEND_HEARTBEAT_STRING );

		pfput_string( es->pf, 
			      "expect_heartbeat_string", 
			      DEFAULT_EXPECT_HEARTBEAT_STRING );

		pfput_string( es->pf, 
			      "select", 
			      DEFAULT_SELECT );

		pfput_string( es->pf, 
			      "reject", 
			      DEFAULT_REJECT );

		pfput_string( es->pf, 
			      "loglevel", 
			      Program_loglevel );

		pfput_string( es->pf, 
			      "my_inst", 
			      DEFAULT_INST );

		pfput_string( es->pf, 
			      "my_mod", 
			      DEFAULT_MOD );
	} 

	mutex_lock( &es->es_mutex );

	oldpf = pfdup( es->pf );

	pfput_string( es->pf, "loglevel", Program_loglevel );

	pfreplace( pf, es->pf, "defaults{server_port}",
			       "server_port", "int" );

	pfreplace( pf, es->pf, "defaults{send_heartbeat_sec}",
			       "send_heartbeat_sec", "int" );

	pfreplace( pf, es->pf, "defaults{send_heartbeat_string}",
			       "send_heartbeat_string", "string" );

	pfreplace( pf, es->pf, "defaults{expect_heartbeat_sec}",
			       "expect_heartbeat_sec", "int" );

	pfreplace( pf, es->pf, "defaults{expect_heartbeat_string}",
			       "expect_heartbeat_string", "string" );

	pfreplace( pf, es->pf, "defaults{select}",
			       "select", "string" );

	pfreplace( pf, es->pf, "defaults{reject}",
			       "reject", "string" );

	pfreplace( pf, es->pf, "defaults{loglevel}",
			       "loglevel", "string" );

	pfreplace( pf, es->pf, "defaults{my_inst}", "my_inst", "string" );

	pfreplace( pf, es->pf, "defaults{my_mod}", "my_mod", "string" );

	sprintf( key, "export_servers{%s}{server_port}", name );
	pfreplace( pf, es->pf, key, "server_port", "int" );

	sprintf( key, "export_servers{%s}{server_ipaddress}", name );
	pfreplace( pf, es->pf, key, "server_ipaddress", "string" );

	sprintf( key, "export_servers{%s}{send_heartbeat_sec}", name );
	pfreplace( pf, es->pf, key, "send_heartbeat_sec", "int" );

	sprintf( key, "export_servers{%s}{send_heartbeat_string}", name );
	pfreplace( pf, es->pf, key, "send_heartbeat_string", "string" );

	sprintf( key, "export_servers{%s}{expect_heartbeat_sec}", name );
	pfreplace( pf, es->pf, key, "expect_heartbeat_sec", "int" );

	sprintf( key, "export_servers{%s}{expect_heartbeat_string}", name );
	pfreplace( pf, es->pf, key, "expect_heartbeat_string", "string" );

	sprintf( key, "export_servers{%s}{select}", name );
	pfreplace( pf, es->pf, key, "select", "string" );

	sprintf( key, "export_servers{%s}{reject}", name );
	pfreplace( pf, es->pf, key, "reject", "string" );

	sprintf( key, "export_servers{%s}{loglevel}", name );
	pfreplace( pf, es->pf, key, "loglevel", "string" );

	sprintf( key, "export_servers{%s}{my_inst}", name );
	pfreplace( pf, es->pf, key, "my_inst", "string" );

	sprintf( key, "export_servers{%s}{my_mod}", name );
	pfreplace( pf, es->pf, key, "my_mod", "string" );

	if( es->new ) {

		add_export_server_thread( name, es );

		ret = thr_create( NULL, 0, orb2ew_export_server, 
				  (void *) name, 
				  THR_DETACHED,
				  &es->thread_id );

		if( ret != 0 ) {

			elog_complain( 1,
			    "'%s': Failed to create export_server thread: "
			    "thr_create error %d\n", name, ret );
			
			mutex_unlock( &es->es_mutex );

			delete_export_server_thread( es );

			return;
		}

	} else if( pfcmp( oldpf, es->pf ) ) {

		es->update = 1;

	} else {

		es->update = 0;
	}

	if( oldpf ) {

		pffree( oldpf );
	}

	if( Flags.VeryVerbose ) {

		if( es->new ) {

			elog_notify( 0,
				"'%s': Started export_server thread as thread-id %d\n", 
				es->name, es->thread_id );

		} else if( es->update ) {

			elog_notify( 0,
				"'%s': Posted updates for export_server thread "
				"(thread-id %d)\n", 
				es->name, es->thread_id );

		} else {

			elog_notify( 0,
				"'%s': ExportServerThread thread (thread-id %d) unchanged\n",
				es->name, es->thread_id );
		}
	}

	mutex_unlock( &es->es_mutex );

	return;
}

static void 
refresh_pins_list( Pf *pf )
{
	Pf	*pfpins;
	Tbl	*new_keys;
	char	*akey;
	int	pinno;
	int	i;

	rw_wrlock( &Pins_rwlock );

	freearr( Pins, 0 );

	Pins = newarr( 0 );

	if( pfget( pf, "pins", (void **) &pfpins ) != PFARR ) {

		if( Flags.verbose ) {

			elog_notify( 0, "No 'pins' translations array in "
					"parameter file\n" );
		}

		rw_unlock( &Pins_rwlock );

		return;
	}

	new_keys = pfkeys( pfpins );

	for( i = 0; i < maxtbl( new_keys ); i++ ) {

		akey = gettbl( new_keys, i );

		pinno = pfget_int( pfpins, akey );

		if( Flags.VeryVerbose ) {
			
			elog_notify( 0, "\tAssigning %s to pin %d\n",
					akey, pinno );
		}

		setarr( Pins, akey, (void *) pinno );
	}

	freetbl( new_keys, 0 );

	rw_unlock( &Pins_rwlock );

	return;
}

static void
reconfigure_export_server_threads( Pf *pf )
{
	Pf	*pfexport_servers;
	Tbl	*new_keys;
	Tbl	*existing_keys;
	Arr	*anarr;
	char	*akey;
	int	i;

	if( pfget( pf, "export_servers", (void **) &pfexport_servers ) != PFARR ) {

		elog_complain( 1, 
		   "parameter 'export_servers' not present or not an array\n" );

		stop_all_export_server_threads();
		
		if( Flags.verbose && num_export_server_threads() <= 0 ) {

			elog_complain( 0, 
			"Warning: no export_server threads defined; nothing to do\n" );
		}

		return;
	} 

	new_keys = pfkeys( pfexport_servers );

	if( maxtbl( new_keys ) <= 0 ) {

		stop_all_export_server_threads();
		
		if( Flags.verbose && num_export_server_threads() <= 0 ) {

			elog_complain( 0, 
			"Warning: no export_server threads defined; nothing to do\n" );
		}

		freetbl( new_keys, 0 );

		return;
	} 

	existing_keys = export_server_thread_names();

	for( i = 0; i < maxtbl( existing_keys ); i++ ) {
		
		akey = gettbl( existing_keys, i );

		if( ( anarr = pfget_arr( pfexport_servers, akey ) ) == NULL ) {

			stop_export_server_thread( akey ); 

		} else {

			update_export_server_thread( akey, pf );

			freearr( anarr, 0 );
		}
	}

	freetbl( existing_keys, free );
		
	for( i = 0; i < maxtbl( new_keys ); i++ ) {

		akey = gettbl( new_keys, i );

		if( find_export_server_thread_byname( akey ) != 0 ) {

			continue;
		}

		update_export_server_thread( akey, pf );
	}

	if( Flags.verbose && num_export_server_threads() <= 0 ) {

		elog_complain( 0, 
		"Warning: no export_server threads defined; nothing to do\n" );
	}

	freetbl( new_keys, 0 );

	return;
}

static void *
orb2ew_pfwatch( void *arg )
{
	Pf	*pf;
	int	rc;

	thr_setprio( thr_self(), THR_PRIORITY_PFWATCH );

	rwlock_init( &Export_Server_Threads_rwlock, USYNC_THREAD, NULL );

	Export_Server_Threads = newarr( 0 );

	rwlock_init( &Pins_rwlock, USYNC_THREAD, NULL );

	Pins = newarr( 0 );

	memset( &Ewinfo, '\0', sizeof( Earthworm_Info ) );

	mutex_init( &Ewinfo.ew_mutex, USYNC_THREAD, NULL );

	strcpy( Ewinfo.pfname, DEFAULT_EARTHWORM_PFNAME );

	for( ;; ) {

		refresh_earthworm_info();

		rc = pfupdate( Pfname, &pf );

		if( rc < 0 ) {
			
			elog_complain( 1, "pfupdate pf parameter file '%s' failed\n",
				  Pfname );

		} else if( rc == 0 ) {
			
			; /* No reconfiguration necessary */

		} else if( rc == 1 ) {

			set_program_loglevel( pf );

			if( Flags.verbose ) {
				
				elog_notify( 0, 
				"Reconfiguring orb2ew from parameter file\n" );
			}
			
			refresh_pins_list( pf );

			reconfigure_export_server_threads( pf );
		}

		sleep( PFWATCH_SLEEPTIME_SEC );
	}
}

int
main( int argc, char **argv )
{
	char	c;
	int	rc;
	thread_t pfwatch_tid;

	elog_init( argc, argv );

	while( ( c = getopt( argc, argv, "p:" ) ) != -1 ) {

		switch( c ) {
		case 'p':
			Pfname = optarg;
			break;

		case '?':
		default:
			usage();
			die( 0, "option not understood\n" );
		}
	}

	if( argc - optind != 1 ) {

		usage();
		die( 0, "Must specify an output orb name\n" );

	} else {
		
		Orbname = argv[optind++];
	}

	rc = thr_create( NULL, 0, orb2ew_pfwatch, 0, 0, &pfwatch_tid );

	if( rc != 0 ) {

		die( 1, "Failed to create parameter-file watch thread, "
			"thr_create error %d\n", rc );
	}

	thr_join( pfwatch_tid, (thread_t *) NULL, (void **) NULL );

	if( Flags.verbose ) {

		elog_notify( 0, "Program terminated\n" );
	}

	return( 0 );
}
