/*
 * orb2ew.c
 *
 * Copyright (c) 2003-2005 Lindquist Consulting, Inc.
 * All rights reserved. 
 *                                                                     
 * Written by Dr. Kent Lindquist, Lindquist Consulting, Inc. 
 * 
 * This software may be used freely in any way as long as 
 * the copyright statement above is not removed. 
 *
 */

#include <stdlib.h>
#include <setjmp.h>

#include "orbew.h"

#define DATATYPE "s4" 
#define DATASIZE 4
#define SOCKET_LISTEN_BACKLOG 10
#define SERVER_RESET_ALLOWANCE_SEC 1
#define BIND_FAILURE_SLEEP_SEC 10
#define VERYVERBOSE_DEBUGBNS 0

#define THR_PRIORITY_EXPORT_SERVER 1
#define THR_PRIORITY_PFWATCH 2

typedef struct ExportServerThread {

	/* Shared variables: */

	mutex_t	es_mutex;
	thread_t thread_id;
	int	ready;
	cond_t  ready_cond;
	mutex_t ready_mutex;
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
	double	starttime;
	char	select[STRSZ];
	char	reject[STRSZ];
	char	my_inst_str[STRSZ];
	char	my_mod_str[STRSZ];
	char	my_type_str[STRSZ];
	int	my_inst;	
	int	my_mod;	
	int	my_type;
	int	my_type_heartbeat;		
	int	timesort_queue_maxpkts;
	int	max_tracebuf_size;
	char	large_tracebuf_handling[STRSZ];
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
	PktChannelPipe *pcp;

} ExportThread;

char	*Pfname = "orb2ew";
char	*Orbname = 0;
Arr	*Export_Server_Threads;
rwlock_t Export_Server_Threads_rwlock;
Arr	*Pins;
rwlock_t Pins_rwlock;

thread_key_t shutdown_handler_key;
thread_key_t sigusr1_buf_key;

thread_key_t reconfig_handler_key;
thread_key_t sigusr2_buf_key;

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
	char	netstachanloc[STRSZ];
	int	pinno;

	sprintf( netstachanloc, "%s_%s_%s_%s", 
			  pktchan->net, 
			  pktchan->sta, 
			  pktchan->chan,
			  pktchan->loc );

	rw_rdlock( &Pins_rwlock );

	pinno = (int) getarr( Pins, netstachanloc );

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
	int	header_size;

	ptr = &tp->msg[0];

	pinno = get_pinno( pktchan );
	hi2mi( &pinno, &ptr, 1 );

	hi2mi( &pktchan->nsamp, &ptr, 1 );

	hd2md( &starttime, &ptr, 1 );

	endtime = ENDTIME( starttime, pktchan->samprate, pktchan->nsamp );
	hd2md( &endtime, &ptr, 1 );

	hd2md( &pktchan->samprate, &ptr, 1 );

	strncpy( ptr, pktchan->sta, TRACE_STA_LEN );
	ptr += TRACE_STA_LEN;

	strncpy( ptr, pktchan->net, TRACE_NET_LEN );
	ptr += TRACE_NET_LEN;

	strncpy( ptr, pktchan->chan, TRACE_CHAN_LEN );
	ptr += TRACE_CHAN_LEN;

	strncpy( ptr, DATATYPE, 3 );
	ptr += 3;

	strncpy( ptr, "", 2 );	/* quality */
	ptr += 2;

	strncpy( ptr, "", 2 );	/* pad */
	ptr += 2;

	header_size = ptr - &tp->msg[0];

	datap = ptr;
	dsize_bytes = DATASIZE * pktchan->nsamp;

	*nbytes = dsize_bytes + header_size;

	hi2mi( pktchan->data, &datap, pktchan->nsamp );

	return 0;
}

int
pktchan_to_tracebuf2( PktChannel *pktchan,
		     TracePacket *tp,
		     double starttime, 
		     int *nbytes )
{
	char	*datap;
	int	dsize_bytes;
	double	endtime;
	int	pinno;
	char	*ptr;
	char	version[2];
	char	loc[TRACE2_LOC_LEN];
	int	header_size;

	ptr = &tp->msg[0];

	pinno = get_pinno( pktchan );
	hi2mi( &pinno, &ptr, 1 );

	hi2mi( &pktchan->nsamp, &ptr, 1 );

	hd2md( &starttime, &ptr, 1 );

	endtime = ENDTIME( starttime, pktchan->samprate, pktchan->nsamp );
	hd2md( &endtime, &ptr, 1 );

	hd2md( &pktchan->samprate, &ptr, 1 );

	strncpy( ptr, pktchan->sta, TRACE2_STA_LEN );
	ptr += TRACE2_STA_LEN;

	strncpy( ptr, pktchan->net, TRACE2_NET_LEN );
	ptr += TRACE2_NET_LEN;

	strncpy( ptr, pktchan->chan, TRACE2_CHAN_LEN );
	ptr += TRACE2_CHAN_LEN;

	if( STREQ( pktchan->loc, "" ) ) {

		strcpy( loc, LOC_NULL_STRING );

	} else {

		strcpy( loc, pktchan->loc );
	}

	strncpy( ptr, loc, TRACE2_LOC_LEN );
	ptr += TRACE2_LOC_LEN;

	version[0] = TRACE2_VERSION0;
	version[1] = TRACE2_VERSION1;

	strncpy( ptr, version, 2 );	
	ptr += 2;

	strncpy( ptr, DATATYPE, 3 );
	ptr += 3;

	strncpy( ptr, "", 2 );	/* quality */
	ptr += 2;

	strncpy( ptr, "", 2 );	/* pad */
	ptr += 2;

	header_size = ptr - &tp->msg[0];

	datap = ptr;
	dsize_bytes = DATASIZE * pktchan->nsamp;

	*nbytes = dsize_bytes + header_size;

	hi2mi( pktchan->data, &datap, pktchan->nsamp );

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
	et->pcp = NULL;

	return et;
}

static void
free_ExportThread( void *arg ) 
{
	ExportThread *et = (ExportThread *) arg;

	if( et->pcp != NULL ) {
		
		pktchannelpipe_free( et->pcp );
	}

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
	es->thread_id = (pthread_t) -1;
	es->so = -1;
	es->last_heartbeat_sent = 0;
	es->last_heartbeat_received = 0;
	es->expect_heartbeat_hook = NULL;
	es->starttime = NULL_STARTTIME;
	es->timesort_queue_maxpkts = 0;

	es->Export_Threads = newarr( 0 );

	es->ready = 0;
	mutex_init( &es->es_mutex, USYNC_THREAD, NULL );
	mutex_init( &es->ready_mutex, USYNC_THREAD, NULL );
	cond_init( &es->ready_cond, USYNC_THREAD, NULL );

	return es;
}

static void
free_ExportServerThread( ExportServerThread **es )
{
	mutex_destroy( &(*es)->es_mutex );
	mutex_destroy( &(*es)->ready_mutex );
	cond_destroy( &(*es)->ready_cond );

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

void 
close_export_connection( ExportThread *et )
{
	char	eof = 004;

	bnsput( et->bnsout, &eof, BYTES, 1 );
	bnsflush( et->bnsout );

	bnsfree( et->bnsout );

	et->bnsout = 0;

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
	int	*statusp;

	close( es->so ); 

	keys = keysarr( es->Export_Threads );

	for( i = 0; i < maxtbl( keys ); i++ ) {

		akey = gettbl( keys, i );

		et = getarr( es->Export_Threads, akey );

		thr_kill( et->thread_id, SIGUSR1 );

		thr_join( et->thread_id, 0, (void **) &statusp );

		if( et->es->loglevel >= VERBOSE || Flags.verbose ) {

			elog_notify( 0, "'%s': Export connection closed with status %d\n",
				et->name, *statusp );
		}
	}

	freetbl( keys, 0 );

	if( es->stop == 0 ) {
		
		sleep( SERVER_RESET_ALLOWANCE_SEC );
	}

	return;
}

static void
stop_export_server_thread( char *name )
{
	ExportServerThread *es;
	int	*statusp = 0;

	if( ( es = find_export_server_thread_byname( name ) ) == 0 ) { 

		elog_complain( 1, "stop_export_server_thread: "
			  "Couldn't find export_server thread '%s' in registry\n",
			  name );
		return;
	}

	if( Flags.VeryVerbose ) {

		elog_notify( 0, "'%s': Request sent to stop export_server thread, "
			     "thread-id %ld\n", name, (long) es->thread_id );
	}

	es->stop = 1;

	thr_kill( es->thread_id, SIGUSR1 );

	if( Flags.verbose ) {

		elog_notify( 0, "'%s': Export_server thread (thread-id %ld) "
			     "stopped with status %d\n",
			     name, (long) es->thread_id, *statusp );
	} 

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

	freetbl( keys, (void (*)(void *)) free );

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

static void
orb2ew_export_server_shutdown()
{
	ExportServerThread *es;
	char	name[STRSZ] = "Unknown";

	if( ( es = find_export_server_thread_byid( thr_self() ) ) == NULL ) {

		elog_complain( 0, "Couldn't find thread %ld in registry!\n",
			  	  (long) thr_self() );

	} else {
	
		close_export_server_connection( es ); 

		if( es->name != NULL ) {

			strcpy( name, es->name );
		}
	}

	if( Flags.verbose ) {

		elog_notify( 0, 
		"'%s': Export Server Thread (thread-id %ld) stopping at user request\n",
		  name, (long) thr_self() );
	}

	delete_export_server_thread( es );
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
			et->es->my_type );
	
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

static void
process_sigusr1( int sig )
{
	void	(*handler)(int);

	thr_getspecific( shutdown_handler_key, (void **) &handler );

	if( sig != SIGUSR1 || handler == NULL ) {

		abort();
	}

	(*handler)( sig );
}

static void
process_sigusr2( int sig )
{
	void	(*handler)(int);

	thr_getspecific( reconfig_handler_key, (void **) &handler );

	if( sig != SIGUSR2 || handler == NULL ) {

		abort();
	}

	(*handler)( sig );
}

static int
set_sigusr1_handler( void (*handler)(int) )
{
	if( handler == SIG_DFL || handler == SIG_IGN ) {
		
		return EINVAL;
	}

	thr_setspecific( shutdown_handler_key, (void *) handler );

	return 0;
}

static int
set_sigusr2_handler( void (*handler)(int) )
{
	if( handler == SIG_DFL || handler == SIG_IGN ) {
		
		return EINVAL;
	}

	thr_setspecific( reconfig_handler_key, (void *) handler );

	return 0;
}

void
sigusr1_handler( int sig )
{
	sigjmp_buf *sigusr1_buf;

	thr_getspecific( sigusr1_buf_key, (void **) &sigusr1_buf );

	siglongjmp( *sigusr1_buf, 1 );
}

void
sigusr2_handler( int sig )
{
	sigjmp_buf *sigusr2_buf;

	thr_getspecific( sigusr2_buf_key, (void **) &sigusr2_buf );

	siglongjmp( *sigusr2_buf, 1 );
}

int
tracepacket_send( ExportThread *et, TracePacket *tp, int nbytes_tp, 
		  char *netstachanloc, double time )
{
	int	rc;
	int	pinno = 0;
	char	*ptr;
	char	*s;

	if( ( et->es->loglevel >= VERYVERBOSE ) || 
		Flags.VeryVerbose ) {

		ptr = (char *) &tp->i;
		mi2hi( &ptr, &pinno, 1 );

		elog_notify( 0, 
			"'%s': Sending packet-channel %s "
			"timed %s as pin %d, from %s %s, format %s\n", 
			et->name, netstachanloc, 
			s = strtime( time ), 
			pinno, 
			et->es->my_inst_str,
			et->es->my_mod_str,
			et->es->my_type_str );

		free( s );
	}

	rc = buf_send( et, tp, nbytes_tp );

	return rc;
}

int
pktchan_send( void *etp, PktChannel *pktchan,
	       int queue_code, double gaptime )
{
	ExportThread *et = (ExportThread *) etp;
	char    netstachanloc[STRSZ];
	int	nbytes_tp = 0;
	TracePacket tp;
	int	rc;

	sprintf( netstachanloc, "%s_%s_%s_%s", 
			pktchan->net,
			pktchan->sta,
			pktchan->chan,
			pktchan->loc );

	if( STREQ( et->es->my_type_str, "TYPE_TRACEBUF" ) ) {

		if( pktchan_to_tracebuf( pktchan, &tp,
				 	pktchan->time, &nbytes_tp  ) )
		{
			freePktChannel( pktchan );

			return 0;
		}

	} else if( STREQ( et->es->my_type_str, "TYPE_TRACEBUF2" ) ) {

		if( pktchan_to_tracebuf2( pktchan, &tp,
				 	pktchan->time, &nbytes_tp  ) )
		{
			freePktChannel( pktchan );

			return 0;
		}

	} else {

		freePktChannel( pktchan );

		elog_complain( 0, 
			"'%s': Don't know how to convert and send "
			"format %s!\n",
			et->name, et->es->my_type_str );

		return 0;
	}

	rc = tracepacket_send( et, &tp, nbytes_tp, 
			       netstachanloc, pktchan->time );

	if( rc != 0 ) {
		
		freePktChannel( pktchan );

		thr_kill( et->thread_id, SIGUSR1 );
	}

	return 0;
}

int
pktchan_queueproc( void *etp, PktChannel *pktchan,
	           int queue_code, double gaptime )
{
	ExportThread *et = (ExportThread *) etp;
	int	nbytes_tp_predicted = 0;
	char    netstachanloc[STRSZ];
	char	*s = NULL;
	char	*t = NULL;
	long	is = 0;
	long	ns = 0;
	long	nsamp_split = 0;
	double	pktchan_time_orig = 0.0;
	int	*pktchan_datap_orig = 0;
	int	pktchan_datasz_orig = 0;
	int	pktchan_nsamp_orig = 0;
	int	pktchan_nsamp_remaining = 0;

	sprintf( netstachanloc, "%s_%s_%s_%s", 
			pktchan->net,
			pktchan->sta,
			pktchan->chan,
			pktchan->loc );

	if( et->es->starttime != NULL_STARTTIME &&
	    pktchan->time < et->es->starttime ) {

		if( ( et->es->loglevel >= VERYVERBOSE ) ||
			Flags.VeryVerbose ) {

			elog_notify( 0, 
			"'%s': Skipping packet-channel %s: "
			"timestamp %s is before requested "
			"start %s\n", 
			et->name, netstachanloc, 
			s = strtime( pktchan->time ),
			t = strtime( et->es->starttime ) );
			free( s );
			free( t );
		}

		freePktChannel( pktchan );

		return 0;
	}

	/* Rely on size of TRACE_HEADER and TRACE2_HEADER being 
	   identical: 
	*/

	nbytes_tp_predicted = sizeof( TRACE_HEADER ) + 
			      DATASIZE * pktchan->nsamp;

	if( nbytes_tp_predicted <= et->es->max_tracebuf_size ) {

		pktchan_send( etp, pktchan, queue_code, gaptime );

	} else if( STREQ( et->es->large_tracebuf_handling, "send" ) ) {

		pktchan_send( etp, pktchan, queue_code, gaptime );

	} else if( ! strncmp( et->es->large_tracebuf_handling, "split", 5 ) ) {

		if( strcontains( et->es->large_tracebuf_handling, 
				   "[[:digit:]]+", 0, &is, &ns ) ) {
			
			nsamp_split = 
				atoi( &et->es->large_tracebuf_handling[is] );

			if( et->es->loglevel >= VERYVERBOSE ) {

				elog_notify( 0, 
					"'%s': Splitting packets "
					"down to %ld samples each\n", 
					et->name, nsamp_split );
			}

			pktchan_datap_orig = pktchan->data;
			pktchan_time_orig = pktchan->time;
			pktchan_nsamp_orig = pktchan->nsamp;

			pktchan_nsamp_remaining = pktchan_nsamp_orig;

			while( pktchan_nsamp_remaining > 0 ) {
			
				pktchan->nsamp = 
					pktchan_nsamp_remaining > nsamp_split ?
					nsamp_split : pktchan_nsamp_remaining;

				nbytes_tp_predicted = sizeof( TRACE_HEADER ) +
						DATASIZE * pktchan->nsamp;

				if( nbytes_tp_predicted > 
					et->es->max_tracebuf_size ) {

					elog_complain( 0, 
					"'%s': Rejecting packet, size %d of "
					"sub-packet after splitting to %ld "
					"samples is still too large "
					"(configured maximum is %d)\n", 
					et->name, nbytes_tp_predicted, 
					nsamp_split,
					et->es->max_tracebuf_size );

					pktchan->nsamp = pktchan_nsamp_orig;
					pktchan->data = pktchan_datap_orig;
					pktchan->time = pktchan_time_orig;

					freePktChannel( pktchan );

					return 0;

				}

				pktchan->datasz = pktchan->nsamp;

				if( et->es->loglevel >= VERYVERBOSE ) {

					elog_notify( 0, 
						"'%s': Sub-packet with nsamp "
						"%d starts at %s\n",
						et->name, pktchan->nsamp,
						s = strtime( pktchan->time ) );

					free( s );
				}

				pktchan_send( etp, pktchan, queue_code, gaptime );

				pktchan_nsamp_remaining -= pktchan->nsamp;

				pktchan->time += pktchan->nsamp / pktchan->samprate;

				pktchan->data += pktchan->nsamp;
			}

			pktchan->data = pktchan_datap_orig;
			pktchan->datasz = pktchan_datasz_orig;
			pktchan->time = pktchan_time_orig;
			pktchan->nsamp = pktchan_nsamp_orig;

		} else {

			elog_complain( 0, 
				"'%s': Rejecting packet, size %d is larger "
				"than configured maximum %d, also "
				"large_tracebuf_handling mode '%s' "
				"wasn't understood (expecting 'split ###')\n", 
				et->name, nbytes_tp_predicted, 
				et->es->max_tracebuf_size,
				et->es->large_tracebuf_handling );

			freePktChannel( pktchan );

			return 0;
		}

	} else if( STREQ( et->es->large_tracebuf_handling, "reject" ) ) {

		elog_complain( 0, 
			"'%s': Rejecting packet, size %d is larger "
			"than configured maximum %d\n", 
			et->name, nbytes_tp_predicted, 
			et->es->max_tracebuf_size );

	} else if( STREQ( et->es->large_tracebuf_handling, "reject_silent" ) ) {

		; /* Fallthrough */

	} else {

		elog_complain( 0, 
			"'%s': Unknown large_tracebuf_handling mode "
			" '%s' (options are 'send', 'split', 'reject', "
			"'reject_silent'); Sending packet\n",
			et->name, et->es->large_tracebuf_handling );

		pktchan_send( etp, pktchan, queue_code, gaptime );
	}

	freePktChannel( pktchan );

	return 0;
}

static void *
orb2ew_export( void *arg )
{
	ExportThread *et = (ExportThread *) arg;
	sigjmp_buf sigusr1_buf;
	int	rc;
	int     pktid;
	char    srcname[STRSZ];
	double  mytime;
	char    *rawpkt = NULL;
	int     bufsize = 0;
	int	nbytes_orb = 0;
	int	status = 0;

	if( ( et->orbfd = orbopen( Orbname, "r&" ) ) < 0 ) {
		
		elog_complain( 0,
			"'%s': Failed to open orb '%s' for reading\n",
			et->name, Orbname );

		status = -1;

		thr_exit( (void *) &status );
	}

	if( et->es->starttime != NULL_STARTTIME ) {

		orbafter( et->orbfd, et->es->starttime );
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

#if 0  /* no longer implemented */
		et->bnsin->debug = 1;
		et->bnsout->debug = 1;
#endif
	}

	set_sigusr1_handler( sigusr1_handler );

	thr_setspecific( sigusr1_buf_key, (void *) &sigusr1_buf );

	if( sigsetjmp( sigusr1_buf, 1 ) != 0 ) {

		goto close_export;	
	}

	if( et->es->send_heartbeat_sec*1000 < DEFAULT_BNS_TIMEOUT ) {

		bnstimeout( et->bnsin, et->es->send_heartbeat_sec * 1000 );

	} else {

		bnstimeout( et->bnsin, DEFAULT_BNS_TIMEOUT );
	}

	et->pcp = pktchannelpipe_new( 0, 0, et->es->timesort_queue_maxpkts,
				      pktchan_queueproc, (void *) et );

	for( ;; ) {

		rc = orbreap( et->orbfd, &pktid, srcname, &mytime,
			&rawpkt, (int *)&nbytes_orb, &bufsize );

		if( rc < 0 )
		{
			elog_clear_register( 1 );
			continue;
		}

		pktchannelpipe_push( et->pcp, srcname, mytime, 
				     rawpkt, nbytes_orb );
	}

	close_export:

	if( et->pcp ) {
		
		pktchannelpipe_flush( et->pcp );
	}

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
	struct linger Linger;
	int	on = 1;
	ExportThread *et;
	char	*loglevel;
	char	*starttime_string;
	int	status = 0;
	char	*clienthost;
	int	clientport;

	mutex_lock( &es->es_mutex );

	if( es->stop == 1 ) {

		orb2ew_export_server_shutdown();

		thr_exit( (void *) &status );
	}

	if( es->update == 1 ) {

		close_export_server_connection( es ); 

		loglevel = pfget_string( es->pf, "loglevel" );

		es->loglevel = translate_loglevel( loglevel );
		
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
			
		strcpy( es->server_ipaddress,
			pfget_string( es->pf, "server_ipaddress" ) );

		es->server_port = 
			(in_port_t) pfget_int( es->pf, "server_port" );

		es->timesort_queue_maxpkts = 
			pfget_int( es->pf, "timesort_queue_maxpkts" );

		es->max_tracebuf_size = 
			pfget_int( es->pf, "max_tracebuf_size" );

		strcpy( es->large_tracebuf_handling,
			pfget_string( es->pf, "large_tracebuf_handling" ) );

		strcpy( es->expect_heartbeat_string,
			pfget_string( es->pf, "expect_heartbeat_string" ) );

		if( es->expect_heartbeat_hook ) {

			free_hook( &es->expect_heartbeat_hook );
		}

		es->send_heartbeat_sec = 
			pfget_int( es->pf, "send_heartbeat_sec" );

		strcpy( es->send_heartbeat_string,
			pfget_string( es->pf, "send_heartbeat_string" ) );

		strcpy( es->select, pfget_string( es->pf, "select" ) );

		strcpy( es->reject, pfget_string( es->pf, "reject" ) );

		strcpy( es->my_inst_str,
			pfget_string( es->pf, "my_inst" ) );

		strcpy( es->my_mod_str,
			pfget_string( es->pf, "my_mod" ) );

		strcpy( es->my_type_str,
			pfget_string( es->pf, "my_type" ) );

		ewlogo_tologo( es->my_inst_str, 
			       es->my_mod_str, 
			       Default_TYPE_HEARTBEAT, 
			       &es->my_inst,
			       &es->my_mod,
			       &es->my_type_heartbeat );

		ewlogo_tologo( es->my_inst_str, 
			       es->my_mod_str, 
			       es->my_type_str, 
			       &es->my_inst,
			       &es->my_mod,
			       &es->my_type );

		starttime_string = pfget_string( es->pf, "starttime" );

		if( starttime_string == NULL || 
		    ( ! strcmp( starttime_string, "" ) ) ) {

			es->starttime = NULL_STARTTIME;

		} else {

			es->starttime = str2epoch( starttime_string );
			elog_clear_register( 1 );
		}

		es->update = 0;
		es->new = 0;
	}

	mutex_unlock( &es->es_mutex );

	memset( &es->sin, 0, sizeof( struct sockaddr ) );
	memset( &client, 0, sizeof( struct sockaddr ) );

	es->so = socket( PF_INET, SOCK_STREAM, 0 );

	if( es->so < 0 ) {

		elog_complain( 1, "'%s': Failed to create socket\n", es->name );
	}

	es->sin.sin_family = AF_INET;

	es->sin.sin_port = htons( es->server_port ); 

	if( strcmp( es->server_ipaddress, "" ) ) {

		es->sin.sin_addr.s_addr = inet_addr( es->server_ipaddress );

	} else {

		es->sin.sin_addr.s_addr = htonl( INADDR_ANY ); 
	}

	rc = setsockopt( es->so, SOL_SOCKET, SO_REUSEADDR, 
			 &on, sizeof( char *) );
	
	if( rc < 0 ) {
		
		elog_complain( 1, "'%s': Failed to set address reuse on socket\n",
				es->name );
	}

	Linger.l_onoff = 1;
	Linger.l_linger = 0;

	rc = setsockopt( es->so, SOL_SOCKET, SO_LINGER, 
			(char *) &Linger, sizeof( struct linger ) );

	if( rc < 0 ) {
		
		elog_complain( 1, "'%s': Failed to set linger to hard-close for socket\n",
				es->name );
	}

	while( ( rc = bind( es->so, (struct sockaddr *) &es->sin, 
			    sizeof( es->sin ) ) ) < 0 ) {

		elog_complain( 1, "'%s': Failed to bind socket; sleeping %d sec\n", 
				es->name, BIND_FAILURE_SLEEP_SEC );

		sleep( BIND_FAILURE_SLEEP_SEC );
	}

	if( Flags.verbose || es->loglevel >= VERBOSE ) {

		elog_notify( 1, "'%s': Bound passive socket %d\n", es->name, es->so );
	}

	rc = listen( es->so, SOCKET_LISTEN_BACKLOG ); 

	if( rc != 0 ) {
			elog_complain( 1, 
				"'%s': Failed to listen() on passive socket %d\n",
				es->name, es->so );
	}

	for( ;; ) {

		aso = accept( es->so, (struct sockaddr *) &client, &clientlen ); 
		if( aso < 0 ) {

			if( errno == EINTR ) {
				
				continue;
			}
			
			elog_complain( 1, 
				"'%s': Failed accept on passive socket %d\n", 
				es->name, es->so );

			sleep( 10 );

			continue;	
		}

		clienthost = inet_ntoa( client.sin_addr  );
		clientport = ntohs( client.sin_port );

		if( ( es->loglevel >= VERBOSE ) || Flags.verbose ) {

			 elog_notify( 0, "'%s': Accepted socket fd %d from %s:%d\n", 
					es->name, aso, clienthost, clientport );
		}
		
		et = new_ExportThread( es, aso );

		rc = thr_create( NULL, 0, orb2ew_export, 
				  (void *) et, 
				  0,
				  &et->thread_id );

		if( rc != 0 ) {

			elog_complain( 1,
			    "'%s': Failed to create export thread: "
			    "thr_create error %d\n", es->name, rc );
			
			free_ExportThread( &et );

			continue;

		} else {

			sprintf( et->name, "%s#tid_%ld", 
				 es->name, (long) et->thread_id );

			if( ( es->loglevel >= VERBOSE ) || Flags.verbose ) {

			 	elog_notify( 0, "'%s': Serving %s:%d with export thread '%s'\n", 
					es->name, clienthost, clientport, et->name );
			}

			mutex_lock( &es->es_mutex );

			setarr( es->Export_Threads, et->name, et );
		
			mutex_unlock( &es->es_mutex );
		}
	}
}

static void *
orb2ew_export_server( void *arg )
{
	char	*name = (char *) arg;
	ExportServerThread *es;
	sigjmp_buf sigusr1_buf;
	sigjmp_buf sigusr2_buf;
	int	status = 0;

	set_sigusr1_handler( sigusr1_handler );
	set_sigusr2_handler( sigusr2_handler );

	thr_setspecific( sigusr1_buf_key, (void *) &sigusr1_buf );
	thr_setspecific( sigusr2_buf_key, (void *) &sigusr2_buf );

	thr_setprio( thr_self(), THR_PRIORITY_EXPORT_SERVER );

	if( Flags.verbose ) {

		elog_notify( 0,
			  "'%s':...orb2ew_export_server thread started\n",
			  name );
	}

	if( ( es = find_export_server_thread_byid( thr_self() ) ) == NULL ) {

		elog_complain( 1, 
			"Couldn't find thread id %ld in registry!\n",
			 (long) thr_self() );

		status = -1;

		thr_exit( (void *) &status );
	} 

	if( sigsetjmp( sigusr1_buf, 1 ) != 0 ) {

		orb2ew_export_server_shutdown();

		thr_exit( (void *) &status );
	}

	if( sigsetjmp( sigusr2_buf, 1 ) != 0 ) {

		mutex_unlock( &es->es_mutex );
	}

	mutex_lock( &es->ready_mutex );
	es->ready = 1;
	cond_signal( &es->ready_cond );
	mutex_unlock( &es->ready_mutex );

	refresh_export_server_thread( es );
}

static void
update_export_server_thread( char *name, Pf *pf )
{
	ExportServerThread *es;
	Pf	*oldpf = 0;
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

		pfput_int( es->pf, 
			   "timesort_queue_maxpkts", 
			   DEFAULT_TIMESORT_QUEUE_MAXPKTS );

		pfput_int( es->pf, 
			   "max_tracebuf_size", 
			   MAX_TRACEBUF_SIZ );

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
			      "starttime", 
			      DEFAULT_STARTTIME );

		pfput_string( es->pf, 
			      "large_tracebuf_handling", 
			      DEFAULT_LARGE_TRACEBUF_HANDLING );

		pfput_string( es->pf, 
			      "loglevel", 
			      Program_loglevel );

		pfput_string( es->pf, 
			      "my_inst", 
			      DEFAULT_INST );

		pfput_string( es->pf, 
			      "my_mod", 
			      DEFAULT_MOD );

		pfput_string( es->pf, 
			      "my_type", 
			      DEFAULT_EWEXPORT_TYPE );
	} 

	mutex_lock( &es->es_mutex );

	oldpf = pfdup( es->pf );

	pfput_string( es->pf, "loglevel", Program_loglevel );

	pfreplace( pf, es->pf, "defaults{server_port}",
			       "server_port", "int" );

	pfreplace( pf, es->pf, "defaults{timesort_queue_maxpkts}",
			       "timesort_queue_maxpkts", "int" );

	pfreplace( pf, es->pf, "defaults{max_tracebuf_size}",
			       "max_tracebuf_size", "int" );

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

	pfreplace( pf, es->pf, "defaults{starttime}",
			       "starttime", "string" );

	pfreplace( pf, es->pf, "defaults{large_tracebuf_handling}",
			       "large_tracebuf_handling", "string" );

	pfreplace( pf, es->pf, "defaults{loglevel}",
			       "loglevel", "string" );

	pfreplace( pf, es->pf, "defaults{my_inst}", "my_inst", "string" );

	pfreplace( pf, es->pf, "defaults{my_mod}", "my_mod", "string" );

	pfreplace( pf, es->pf, "defaults{my_type}", "my_type", "string" );

	sprintf( key, "export_servers{%s}{server_port}", name );
	pfreplace( pf, es->pf, key, "server_port", "int" );

	sprintf( key, "export_servers{%s}{timesort_queue_maxpkts}", name );
	pfreplace( pf, es->pf, key, "timesort_queue_maxpkts", "int" );

	sprintf( key, "export_servers{%s}{max_tracebuf_size}", name );
	pfreplace( pf, es->pf, key, "max_tracebuf_size", "int" );

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

	sprintf( key, "export_servers{%s}{starttime}", name );
	pfreplace( pf, es->pf, key, "starttime", "string" );

	sprintf( key, "export_servers{%s}{loglevel}", name );
	pfreplace( pf, es->pf, key, "loglevel", "string" );

	sprintf( key, "export_servers{%s}{large_tracebuf_handling}", name );
	pfreplace( pf, es->pf, key, "large_tracebuf_handling", "string" );

	sprintf( key, "export_servers{%s}{my_inst}", name );
	pfreplace( pf, es->pf, key, "my_inst", "string" );

	sprintf( key, "export_servers{%s}{my_mod}", name );
	pfreplace( pf, es->pf, key, "my_mod", "string" );

	sprintf( key, "export_servers{%s}{my_type}", name );
	pfreplace( pf, es->pf, key, "my_type", "string" );

	mutex_unlock( &es->es_mutex );

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
			
			delete_export_server_thread( es );

			return;
		}

		mutex_lock( &es->ready_mutex );
		while( es->ready == 0 ) {
			cond_wait( &es->ready_cond, &es->ready_mutex );
		}
		mutex_unlock( &es->ready_mutex );

	} else if( pfcmp( oldpf, es->pf ) ) {

		es->update = 1;

		mutex_lock( &es->es_mutex ); 

		thr_kill( es->thread_id, SIGUSR2 );

	} else {

		es->update = 0;
	}

	if( Flags.VeryVerbose ) {

		if( es->new ) {

			elog_notify( 0,
				"'%s': Started export_server thread '%s' as thread-id %ld\n", 
				es->name, es->name, (long) es->thread_id );

		} else if( es->update ) {

			elog_notify( 0,
				"'%s': Posted updates for export_server thread "
				"(thread-id %ld)\n", 
				es->name, (long) es->thread_id );

		} else {

			elog_notify( 0,
				"'%s': ExportServerThread thread (thread-id %ld) unchanged\n",
				es->name, (long) es->thread_id );
		}
	}

	if( oldpf ) {

		pffree( oldpf );
	}

	return;
}

static void 
refresh_pins_list( Pf *pf )
{
	Pf	*pfpins = 0;
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
	Pf	*pfexport_servers = 0;
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

	freetbl( existing_keys, (void (*)(void *)) free );
		
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
	Pf	*pf = 0;
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
	struct sigaction sa1;
	struct sigaction sa2;

	elog_init( argc, argv );

	while( ( c = getopt( argc, argv, "p:" ) ) != -1 ) {

		switch( c ) {
		case 'p':
			Pfname = optarg;
			break;

		case '?':
		default:
			usage();
			elog_die( 0, "option not understood\n" );
		}
	}

	if( argc - optind != 1 ) {

		usage();
		elog_die( 0, "Must specify an output orb name\n" );

	} else {
		
		Orbname = argv[optind++];
	}

	thr_keycreate( &sigusr1_buf_key, NULL );
	thr_keycreate( &shutdown_handler_key, NULL );
	sa1.sa_flags = 0;
	sigemptyset( &sa1.sa_mask );
	sa1.sa_handler = process_sigusr1;
	sigaction( SIGUSR1, &sa1, NULL );

	thr_keycreate( &sigusr2_buf_key, NULL );
	thr_keycreate( &reconfig_handler_key, NULL );
	sa2.sa_flags = 0;
	sigemptyset( &sa2.sa_mask );
	sa2.sa_handler = process_sigusr2;
	sigaction( SIGUSR2, &sa2, NULL );

	rc = thr_create( NULL, 0, orb2ew_pfwatch, 0, 0, &pfwatch_tid );

	if( rc != 0 ) {

		elog_die( 1, "Failed to create parameter-file watch thread, "
			"thr_create error %d\n", rc );
	}

	thr_join( pfwatch_tid, (thread_t *) NULL, (void **) NULL );

	if( Flags.verbose ) {

		elog_notify( 0, "Program terminated\n" );
	}

	return( 0 );
}
