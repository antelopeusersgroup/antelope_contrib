/*
 * ew2orb.c
 *
 * Kent Lindquist
 * Lindquist Consulting
 * 2003
 *
 */

#include <stdlib.h>

#include "orbew.h"

#define SERVER_RESET_ALLOWANCE_SEC 1
#define PACKET_QUEUE_SIZE 50000

#define DEFAULT_BANNER_PATH "/usr/bin/banner"

#define THR_PRIORITY_IMPORT 0
#define THR_PRIORITY_CONVERT 1
#define THR_PRIORITY_PFWATCH 2

#define APPEND_BUFFER(BUFFER,BUFSZ,NBYTES,C) \
	NBYTES++; \
	RESIZE_BUFFER( char *, BUFFER, BUFSZ, NBYTES ); \
	BUFFER[NBYTES-1] = C;
	
#define BAD_BYTE(WHERE,C) \
	complain( 0, "Bad byte in parser at location '%s': ", WHERE ); \
	{ int bad = (C); hexdump( stderr, &bad, 1 ); } \
	fprintf( stderr, "\n" );

typedef struct ImportThread {

	/* Shared variables: */

	mutex_t	it_mutex;
	thread_t thread_id;
	Pf	*pf;
	int	update;
	int	new;
	int	stop;
	Morphtbl *srcname_morphmap;

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
	char	my_inst_str[STRSZ];
	char	my_mod_str[STRSZ];
	char	select[STRSZ];
	char	reject[STRSZ];
	Hook	*select_hook;
	Hook	*reject_hook;
	int	my_inst;	
	int	my_mod;	
	int	my_type_heartbeat;		
	int	so;
	struct sockaddr_in sin;
	Bns	*bnsin;			
	Bns	*bnsout;			
	char	*buf;	
	int 	bufsize;
	int	nbytes;
	enum	{ STREAM_SEARCH, STREAM_ACCUMULATE, STREAM_ESCAPE } sync_state;
	enum Loglevel loglevel;
	int	connectfail;
	int	bindfail;

} ImportThread;

typedef struct Ew2orbPacket {
	char	*buf;
	int	bufsize;
	int	nbytes;
	char	itname[STRSZ];
	ImportThread *it;
	int	inst;
	int	mod;
	int	type;
	char	inststr[STRSZ];
	char	modstr[STRSZ];
	char	typestr[STRSZ];
	TRACE_HEADER *th;
	Packet	*pkt;
	char	*orbpkt;
	int	orbpktsz;
	int	orbpktnbytes;
	double	time;
	char	srcname[STRSZ];
	enum Loglevel loglevel;
} Ew2orbPacket;

Arr	*Import_Threads;
rwlock_t Import_Threads_rwlock;
Mtfifo	*E2oPackets_mtf;
int	Orbfd = -1;
char	*Pfname = "ew2orb";

Ew2orbPacket *
new_Ew2orbPacket()
{
	Ew2orbPacket *e2opkt;
	
	allot( Ew2orbPacket *, e2opkt, 1 );

	e2opkt->buf = 0;
	e2opkt->bufsize = 0;
	e2opkt->nbytes = 0;
	e2opkt->inst = 0;
	e2opkt->mod = 0;
	e2opkt->type = 0;
	e2opkt->loglevel = QUIET;
	strcpy( e2opkt->inststr, "INST_???" );
	strcpy( e2opkt->modstr, "MOD_???" );
	strcpy( e2opkt->typestr, "TYPE_???" );

	e2opkt->th = NULL;
	e2opkt->pkt = NULL;
	e2opkt->orbpkt = NULL;
	e2opkt->orbpktsz = 0;
	e2opkt->orbpktnbytes = 0;

	return e2opkt;
}

static void
free_Ew2orbPacket( Ew2orbPacket *e2opkt )
{
	if( e2opkt == NULL ) {

		return;
	}

	if( e2opkt->buf != NULL ) {
		
		free( e2opkt->buf );
	}

	if( e2opkt->th != NULL ) {
		
		free( e2opkt->th );
	}

	if( e2opkt->pkt != NULL ) {
		
		freePkt( e2opkt->pkt );
	}

	if( e2opkt->orbpkt != NULL ) {
		
		free( e2opkt->orbpkt );
	}

	free( e2opkt );

	return;
}

static void
describe_packet( Ew2orbPacket *e2opkt )
{
	elog_notify( 0, "'%s': Received <%s> <%s> <%s>\n",
		  e2opkt->itname,
		  e2opkt->inststr, 
		  e2opkt->modstr, 
		  e2opkt->typestr );

	if( ! strcmp( e2opkt->typestr, Default_TYPE_HEARTBEAT ) ) { 

		elog_notify( 0, "'%s':\tunpacked Heartbeat: \"%s\"\n",
	  		e2opkt->itname, 
	  		e2opkt->buf );

	} else if( ! strcmp( e2opkt->typestr, Default_TYPE_TRACEBUF ) ) { 

		if( e2opkt->th ) {

			elog_notify( 0, 
				  "'%s': unpacked Trace-data:\n\t"
				  "%s %s %s pinno %d "
				  "datatype %s quality '%s'\n\t"
				  "nsamp %d samprate %f\n\t"
				  "starttime %f endtime %f\n",
				  e2opkt->itname,
				  e2opkt->th->net, 
				  e2opkt->th->sta, 
				  e2opkt->th->chan,
				  e2opkt->th->pinno, 
				  e2opkt->th->datatype, 
				  e2opkt->th->quality, 
				  e2opkt->th->nsamp, 
				  e2opkt->th->samprate, 
				  e2opkt->th->starttime, 
				  e2opkt->th->endtime );
		} else {

			elog_notify( 0, "'%s':\tunpacked Trace data\n", 
			  	e2opkt->itname  );
		}

	} else if( ! strcmp( e2opkt->typestr, Default_TYPE_TRACE_COMP_UA ) ) {

		if( e2opkt->th ) {

			elog_notify( 0, 
				  "'%s': unpacked Compressed trace-data:\n\t"
				  "%s %s %s pinno %d "
				  "datatype %s quality '%s'\n\t"
				  "nsamp %d samprate %f\n\t"
				  "starttime %f endtime %f\n" ,
				  e2opkt->itname,
				  e2opkt->th->net, 
				  e2opkt->th->sta, 
				  e2opkt->th->chan,
				  e2opkt->th->pinno, 
				  e2opkt->th->datatype, 
				  e2opkt->th->quality, 
				  e2opkt->th->nsamp, 
				  e2opkt->th->samprate, 
				  e2opkt->th->starttime, 
				  e2opkt->th->endtime );
		} else {

			elog_notify( 0, "'%s':\tunpacked Compressed trace data\n", 
			  	e2opkt->itname  );
		}

	} else {

		complain( 0, "'%s':\tunpacked Unknown data\n", e2opkt->itname  );
	}

	return;
}

int
buf_intake( ImportThread *it )
{
	Ew2orbPacket *e2opkt;
	char	*cp;
	TRACE_HEADER th;
	char	astring[STRSZ];
	int	inst;
	int	mod;
	int	type;
	char	inststr[STRSZ];
	char	modstr[STRSZ];
	char	typestr[STRSZ];
	char	*s;

	e2opkt = new_Ew2orbPacket();

	e2opkt->it = it;

	strcpy( e2opkt->itname, it->name );

	cp = it->buf;

	memcpy( astring, cp, 3 );
	cp += 3;
	astring[3] = 0;
	e2opkt->inst = atoi( astring );

	memcpy( astring, cp, 3 );
	cp += 3;
	astring[3] = 0;
	e2opkt->mod = atoi( astring );

	memcpy( astring, cp, 3 );
	cp += 3;
	astring[3] = 0;
	e2opkt->type = atoi( astring );

	e2opkt->loglevel = it->loglevel;

	RESIZE_BUFFER( char *, e2opkt->buf, 
		       e2opkt->bufsize, it->nbytes - EWLOGO_SIZE );

	memcpy( e2opkt->buf, cp, it->nbytes - EWLOGO_SIZE );
	e2opkt->nbytes = it->nbytes - EWLOGO_SIZE;

	ewlogo_tostrings( e2opkt->inst, e2opkt->mod, e2opkt->type, 
			  e2opkt->inststr, e2opkt->modstr, e2opkt->typestr );

	if( ! strcmp( e2opkt->typestr, Default_TYPE_HEARTBEAT ) ) {

		if( it->loglevel == VERYVERBOSE ) {

			describe_packet( e2opkt );
		}

		if( ! strmatches( cp, 
				  it->expect_heartbeat_string, 
				  &it->expect_heartbeat_hook ) ) {

			complain( 0, 
				"'%s': Warning: Received heartbeat '%s' doesn't "
				"match expected pattern '%s'\n",
				it->name, cp, it->expect_heartbeat_string );
		} else {
			
			if( it->loglevel == VERYVERBOSE ) {
			  if( it->last_heartbeat_received == 0 ) {

				elog_notify( 0, "'%s': First heartbeat received\n",
					     it->name );
			  } else {

				s = strtdelta( now() - 
					       it->last_heartbeat_received );

				elog_notify( 0, 
					  "'%s': Received-heartbeat latency %s\n",
					  it->name, s );

				free( s );
			  }
			}

			it->last_heartbeat_received = now();
		}

		free_Ew2orbPacket( e2opkt );
			
	} else {

		mtfifo_push( E2oPackets_mtf, (void *) e2opkt ); 
	}

	return 0;
}

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

static void
free_ImportThread( ImportThread **it )
{

	mutex_destroy( &(*it)->it_mutex );

	if( (*it)->pf ) {

		pffree( (*it)->pf );
	}

	if( (*it)->buf ) {

		free( (*it)->buf );
	}

	if( (*it)->srcname_morphmap ) {

		freemorphtbl( (*it)->srcname_morphmap );
	}

	if( (*it)->reject_hook ) {

		free_hook( &((*it)->reject_hook) );
	}

	if( (*it)->select_hook ) {

		free_hook( &((*it)->select_hook) );
	}

	free( *it );

	*it = 0;

	return;
}

ImportThread *
find_import_thread_byname( char *name )
{
	ImportThread *it;

	rw_rdlock( &Import_Threads_rwlock );

	it = getarr( Import_Threads, name ); 

	rw_unlock( &Import_Threads_rwlock );

	return it;
}

static void
stop_import_thread( char *name )
{
	ImportThread *it;

	if( ( it = find_import_thread_byname( name ) ) == 0 ) { 

		complain( 1, "stop_import_thread: "
			  "Couldn't find import thread '%s' in registry\n",
			  name );
		return;
	}

	if( Flags.VeryVerbose ) {

		elog_notify( 0, "'%s': Request sent to stop import thread, "
			     "thread-id %d\n", name, it->thread_id );
	}

	mutex_lock( &it->it_mutex );

	it->stop = 1;

	mutex_unlock( &it->it_mutex );

	return;
}

static Tbl *
import_thread_names() 
{
	Tbl	*keys;
	Tbl	*dup;

	rw_rdlock( &Import_Threads_rwlock );

	keys = keysarr( Import_Threads );

	dup = duptbl( keys, (void *(*)()) strdup );

	freetbl( keys, 0 );

	rw_unlock( &Import_Threads_rwlock );

	return dup;
}

static int
num_import_threads()
{
	int	nthreads;

	rw_rdlock( &Import_Threads_rwlock );

	nthreads = cntarr( Import_Threads );

	rw_unlock( &Import_Threads_rwlock );

	return nthreads;
}

static void 
stop_all_import_threads()
{
	int	i;
	Tbl	*keys;

	keys = import_thread_names();

	for( i = 0; i < maxtbl( keys ); i++ ) {

		stop_import_thread( gettbl( keys, i ) );
	}

	freetbl( keys, free );

	return;
}

ImportThread *
find_import_thread_byid( thread_t tid )
{
	ImportThread *it;
	ImportThread *found = (ImportThread *) NULL;
	Tbl	*keys;
	char	*key;
	int 	i;

	rw_rdlock( &Import_Threads_rwlock );

	keys = keysarr( Import_Threads );

	for( i = 0; i < maxtbl( keys ); i++ ) {

		key = gettbl( keys, i );

		it = (ImportThread *) getarr( Import_Threads, key );

		if( it->thread_id == tid ) {

			found = it;

			break;
		}
	}

	freetbl( keys, 0 );

	rw_unlock( &Import_Threads_rwlock );

	return found;
}


static void
add_import_thread( char *name, ImportThread *it ) 
{
	rw_wrlock( &Import_Threads_rwlock );

	setarr( Import_Threads, name, (void *) it );

	rw_unlock( &Import_Threads_rwlock );

	return;
}

static void
delete_import_thread( ImportThread *it ) 
{
	if( it != (ImportThread *) NULL ) {

		mutex_trylock( &it->it_mutex );
		mutex_unlock( &it->it_mutex );

		rw_wrlock( &Import_Threads_rwlock );

		delarr( Import_Threads, it->name );

		free_ImportThread( &it );

		rw_unlock( &Import_Threads_rwlock );
	}

	return;
}

static void
close_import_connection( ImportThread *it )
{
	char	eof = 004;

	bnsput( it->bnsout, &eof, BYTES, 1 );
	bnsflush( it->bnsout );

	bnsfree( it->bnsout );

	/* Keep export_generic from interpreting flushed data
	   as a malformed heartbeat: */

	bnsclr( it->bnsin );

	bnsclose( it->bnsin );

	it->bnsin = 0;

	if( it->stop == 0 ) {
		
		sleep( SERVER_RESET_ALLOWANCE_SEC );
	}

	return;
}

static void
ew2orb_import_shutdown()
{
	int	status = 0;
	ImportThread *it;
	char	name[STRSZ] = "Unknown";

	if( ( it = find_import_thread_byid( thr_self() ) ) == NULL ) {

		complain( 0, "Couldn't find thread %d in registry!\n",
			  	  thr_self() );

	} else {
	
		if( it->bnsin != NULL ) {

			close_import_connection( it );
		}

		if( it->name != NULL ) {

			strcpy( name, it->name );
		}
	}

	if( Flags.verbose ) {

		elog_notify( 0, 
		"'%s': Thread (thread-id %d) stopping at user request\n",
		  name, thr_self() );
	}

	delete_import_thread( it );

	thr_exit( (void *) &status );

	return;
}

static void
reconfig_import_thread( ImportThread *it )
{
	char	*loglevel;
	Tbl	*morphlist;

	mutex_lock( &it->it_mutex );

	if( it->stop == 1 ) {

		ew2orb_import_shutdown();
	}

	if( it->update == 1 ) {

		if( it->loglevel >= VERBOSE ) {

			if( it->new ) {
					
				elog_notify( 0, 
			  	"'%s': Configuring thread with: ", 
			  	it->name );

			} else {

				elog_notify( 0, 
			  	"'%s': Reconfiguring thread with: ", 
			  	it->name );

			}

			pfout( stderr, it->pf );
		}
			
		if( it->bnsin != NULL ) {
			
			close_import_connection( it );

			it->last_heartbeat_sent = 0;
		}

		strcpy( it->server_ipaddress,
			pfget_string( it->pf, "server_ipaddress" ) );

		it->server_port = 
			(in_port_t) pfget_int( it->pf, "server_port" );

		strcpy( it->expect_heartbeat_string,
			pfget_string( it->pf, "expect_heartbeat_string" ) );

		if( it->expect_heartbeat_hook ) {

			free_hook( &it->expect_heartbeat_hook );
		}

		strcpy( it->send_heartbeat_string,
			pfget_string( it->pf, "send_heartbeat_string" ) );

		it->send_heartbeat_sec = 
			pfget_int( it->pf, "send_heartbeat_sec" );

		strcpy( it->my_inst_str,
			pfget_string( it->pf, "my_inst" ) );

		strcpy( it->my_mod_str,
			pfget_string( it->pf, "my_mod" ) );

		ewlogo_tologo( it->my_inst_str, 
			       it->my_mod_str, 
			       Default_TYPE_HEARTBEAT, 
			       &it->my_inst,
			       &it->my_mod,
			       &it->my_type_heartbeat );

		strcpy( it->select, pfget_string( it->pf, "select" ) );
		strcpy( it->reject, pfget_string( it->pf, "reject" ) );

		if( it->select_hook ) {

			free_hook( &it->select_hook );
		} 

		if( it->reject_hook ) {

			free_hook( &it->reject_hook );
		} 

		loglevel = pfget_string( it->pf, "loglevel" );

		it->loglevel = translate_loglevel( loglevel );
		
		if( it->srcname_morphmap ) {
			
			freemorphtbl( it->srcname_morphmap );
		}

		morphlist = pfget_tbl( it->pf, "srcname_morph" );
		morphlist = healthy_morphlist( morphlist );

		newmorphtbl( morphlist, &it->srcname_morphmap );

		if( morphlist != (Tbl *) NULL ) {
			
			freetbl( morphlist, free );
		}

		it->update = 0;
		it->new = 0;
		it->connectfail = 0;
		it->bindfail = 0;
	}

	mutex_unlock( &it->it_mutex );

	return;
}

static void
refresh_import_thread( ImportThread *it )
{
	reconfig_import_thread( it );

	while( it->bnsin == NULL ) {

		reconfig_import_thread( it );

		it->so = socket( PF_INET, SOCK_STREAM, 0 );
		if( it->so < 0 ) {

			complain( 1,
		 	  "'%s': Failed to open socket to %s:%d\n", 
			  it->name, it->server_ipaddress, it->server_port );

			sleep( CONNECT_FAILURE_SLEEPTIME_SEC );

			continue;
		}
	
		it->sin.sin_family = AF_INET;
		it->sin.sin_port = htons( 0 ); /* Any port */
		it->sin.sin_addr.s_addr = htonl( INADDR_ANY );

		if( bind( it->so, (struct sockaddr *) &it->sin, 
		                                   sizeof( it->sin ) ) ) {
			if( it->bindfail < NCOMPLAIN_MAX ) {

				complain( 1, "'%s': Couldn't bind socket\n",
					  it->name );
			}

			if( it->bindfail++ == NCOMPLAIN_MAX ) {
					
				complain( 0, 
					"'%s': Last message repeated %d "
					"times; will keep retrying "
					"every %d sec\n",
					it->name,
					NCOMPLAIN_MAX,
					CONNECT_FAILURE_SLEEPTIME_SEC );
			}

			close( it->so );
				
			sleep( CONNECT_FAILURE_SLEEPTIME_SEC );

			continue;

		} else {

			it->bindfail = 0;
		}
	
		it->sin.sin_port = htons( it->server_port );
		it->sin.sin_addr.s_addr = inet_addr( it->server_ipaddress );

		if( connect( it->so, (struct sockaddr *) &it->sin, sizeof( it->sin ) ) ) {
			if( it->connectfail < NCOMPLAIN_MAX ) {

				complain( 1, 
					"'%s': Failed to connect socket for %s:%d\n",
					it->name,
					it->server_ipaddress, it->server_port );
			}

			if( it->connectfail++ == NCOMPLAIN_MAX ) {
					
				complain( 0, 
					"'%s': Last message repeated %d "
					"times; will keep retrying "
					"every %d sec\n",
					it->name,
					NCOMPLAIN_MAX,
					CONNECT_FAILURE_SLEEPTIME_SEC );
			}

			close( it->so );
				
			sleep( CONNECT_FAILURE_SLEEPTIME_SEC );

			continue;

		} else {

			it->connectfail = 0;

			if( ( it->loglevel >= VERBOSE ) || Flags.verbose ) {

				elog_notify( 0, 
					  "'%s': import thread Connected "
					  "to remote export module\n",
					  it->name );
			}
		}

		it->bnsin = bnsnew( it->so, BNS_BUFFER_SIZE ); 
		it->bnsout = bnsnew( it->so, BNS_BUFFER_SIZE ); 

		bnsuse_sockio( it->bnsin );
		bnsuse_sockio( it->bnsout );

		if( it->send_heartbeat_sec*1000 < DEFAULT_BNS_TIMEOUT ) {

			bnstimeout( it->bnsin, 
				it->send_heartbeat_sec * 1000 );

		} else {

			bnstimeout( it->bnsin, DEFAULT_BNS_TIMEOUT );
		}
	}

	return;
}

static void
send_heartbeat( ImportThread *it )
{
	char 	msg[STRSZ]; 
	char	stx = STX;
	char	etx = ETX;
	char	nul = NULL;
	int	msglen;

	if( now() - it->last_heartbeat_sent < it->send_heartbeat_sec ) {
		
		return;
	}

	sprintf( msg, "%c%3d%3d%3d%s%c%c",
		      stx,
		      it->my_inst,
		      it->my_mod, 
		      it->my_type_heartbeat,
		      it->send_heartbeat_string,
		      nul,
		      etx );

	msglen = 12 + strlen( it->send_heartbeat_string );

	if( it->loglevel == VERYVERBOSE ) {

		elog_notify( 0, "'%s': Sending heartbeat: \"%s\"\n",
			  it->name, it->send_heartbeat_string );
	}

	bnsput( it->bnsout, msg, BYTES, msglen );

	bnsflush( it->bnsout );

	it->last_heartbeat_sent = now();

	return;
}

static void
display_banner( char *msg )
{
	char	bannercmd[STRSZ];

	if( Flags.have_banner == 0 ) {

		return;
	}

	sprintf( bannercmd, "%s %s", DEFAULT_BANNER_PATH, msg );

	system( bannercmd );

	return;
}

static int
more_packet_data( ImportThread *it )
{
	int	rc;
	char	c;
	char	previous = 0;
	int	bns_errno;

	while( ( rc = bnsget( it->bnsin, &c, BYTES, 1 ) ) == 0 ) {
			
		switch( it->sync_state ) {

		case STREAM_SEARCH: 

			if( c == STX && previous != ESC ) {

				it->sync_state = STREAM_ACCUMULATE;
				it->nbytes = 0;

			} else {

				BAD_BYTE( "search", c );
			}

			break;

		case STREAM_ACCUMULATE:

			if( c == ESC ) { 

				it->sync_state = STREAM_ESCAPE;

			} else if( c == ETX ) {

				/* Guarantee null-termination of 
				   string messages */

				APPEND_BUFFER( it->buf, it->bufsize, 
					       it->nbytes, '\0' );
				it->nbytes--;

				it->sync_state = STREAM_SEARCH;

				return 1;

			} else {

				APPEND_BUFFER( it->buf, it->bufsize, 
					       it->nbytes, c );
			}

			break;

		case STREAM_ESCAPE:
			
			if( c == ETX || c == STX || c == ESC ) {

				APPEND_BUFFER( it->buf, it->bufsize, 
					       it->nbytes, c );

				it->sync_state = STREAM_ACCUMULATE;

			} else {
	
				APPEND_BUFFER( it->buf, it->bufsize, 
					       it->nbytes, previous );
				APPEND_BUFFER( it->buf, it->bufsize, 
					       it->nbytes, c );

				it->sync_state = STREAM_ACCUMULATE;
			}

			break;

		default: 

			BAD_BYTE( "default", c );

			it->sync_state = STREAM_SEARCH;

			break;
		}

		previous = c;
	}

	if( bnserr( it->bnsin ) != 0 ) {
			
		bns_errno = bnserrno( it->bnsin );

		if( ( it->loglevel >= VERBOSE ) || Flags.verbose ) {

		  if( bns_errno == ECONNRESET ) {

			complain( 1, 
				  "'%s': Connection reset by peer\n",
				  it->name );

			if( it->loglevel == VERYVERBOSE ) {

				display_banner( "ECONNRESET" ); 
			}

		  } else {

			complain( 1, 
				  "'%s': bns error %d\n", 
				  it->name, bns_errno );
		  }
		}

		return -1;

	} else if( bnseof( it->bnsin ) ) {

		if( ( it->loglevel >= VERBOSE ) || Flags.verbose ) {

			complain( 1, 
				  "'%s': Connection closed by remote "
				  "export module\n",
				  it->name );

			if( it->loglevel == VERYVERBOSE ) {

				display_banner( "CLOSED" ); 
			}
		}

		return -1;

	} else {

		return 0;
	}
}

static void *
ew2orb_import( void *arg )
{
	char	*name = (char *) arg;
	ImportThread *it;
	int	status = 0;
	int	rc;

	thr_setprio( thr_self(), THR_PRIORITY_IMPORT );

	if( Flags.verbose ) {

		elog_notify( 0,
			  "'%s':...ew2orb_import thread started\n",
			  name );
	}

	if( ( it = find_import_thread_byid( thr_self() ) ) == NULL ) {

		complain( 1, 
			"Couldn't find thread id %d in registry!\n",
			 thr_self() );

		status = -1;

		thr_exit( (void *) &status );
	} 

	for( ;; ) {

		refresh_import_thread( it );

		send_heartbeat( it );

		if( ( rc = more_packet_data( it ) ) > 0 ) {

			buf_intake( it );

		} else if( rc < 0 ) {

			close_import_connection( it );
		}
	}

	return NULL;
}

static ImportThread *
new_ImportThread( char *name )
{
	ImportThread *it;

	allot( ImportThread *, it, 1 );

	strcpy( it->name, name );
	it->update = 1;
	it->new = 1;
	it->stop = 0;
	it->pf = pfnew( PFARR );
	it->bnsin = NULL;
	it->bnsout = NULL;
	it->buf = 0;
	it->bufsize = 0;
	it->nbytes = 0;
	it->sync_state = STREAM_SEARCH;
	it->loglevel = QUIET;
	it->thread_id = -1;
	it->last_heartbeat_sent = 0;
	it->last_heartbeat_received = 0;
	it->expect_heartbeat_hook = NULL;
	it->connectfail = 0;
	it->bindfail = 0;
	it->srcname_morphmap = NULL;
	it->select_hook = NULL;
	it->reject_hook = NULL;

	mutex_init( &it->it_mutex, USYNC_THREAD, NULL );

	return it;
}

static void
update_import_thread( char *name, Pf *pf )
{
	ImportThread *it;
	Pf	*oldpf;
	char	key[STRSZ];
	int	ret;

	if( ( it = find_import_thread_byname( name ) ) == 0 ) {

		it = new_ImportThread( name );

		pfput_int( it->pf, "server_port", DEFAULT_SERVER_PORT );

		pfput_string( it->pf, "server_ipaddress", "" );

		pfput_int( it->pf, 
			   "send_heartbeat_sec", 
			   DEFAULT_SEND_HEARTBEAT_SEC );

		pfput_int( it->pf, 
			   "expect_heartbeat_sec", 
			   DEFAULT_EXPECT_HEARTBEAT_SEC );

		pfput_string( it->pf, 
			      "send_heartbeat_string", 
			      DEFAULT_SEND_HEARTBEAT_STRING );

		pfput_string( it->pf, 
			      "expect_heartbeat_string", 
			      DEFAULT_EXPECT_HEARTBEAT_STRING );

		pfput_string( it->pf, 
			      "select", 
			      DEFAULT_SELECT );

		pfput_string( it->pf, 
			      "reject", 
			      DEFAULT_REJECT );

		pfput_string( it->pf, 
			      "loglevel", 
			      Program_loglevel );

		pfput_string( it->pf, 
			      "my_inst", 
			      DEFAULT_INST );

		pfput_string( it->pf, 
			      "my_mod", 
			      DEFAULT_MOD );
	} 

	mutex_lock( &it->it_mutex );

	oldpf = pfdup( it->pf );

	pfput_string( it->pf, "loglevel", Program_loglevel );

	pfreplace( pf, it->pf, "defaults{server_port}",
			       "server_port", "int" );

	pfreplace( pf, it->pf, "defaults{send_heartbeat_sec}",
			       "send_heartbeat_sec", "int" );

	pfreplace( pf, it->pf, "defaults{send_heartbeat_string}",
			       "send_heartbeat_string", "string" );

	pfreplace( pf, it->pf, "defaults{expect_heartbeat_sec}",
			       "expect_heartbeat_sec", "int" );

	pfreplace( pf, it->pf, "defaults{expect_heartbeat_string}",
			       "expect_heartbeat_string", "string" );

	pfreplace( pf, it->pf, "defaults{select}",
			       "select", "string" );

	pfreplace( pf, it->pf, "defaults{reject}",
			       "reject", "string" );

	pfreplace( pf, it->pf, "defaults{loglevel}",
			       "loglevel", "string" );

	pfreplace( pf, it->pf, "defaults{my_inst}", "my_inst", "string" );

	pfreplace( pf, it->pf, "defaults{my_mod}", "my_mod", "string" );

	pfreplace( pf, it->pf, "defaults{srcname_morph}", 
			       "srcname_morph", "tbl" );

	sprintf( key, "import_from{%s}{server_port}", name );
	pfreplace( pf, it->pf, key, "server_port", "int" );

	sprintf( key, "import_from{%s}{server_ipaddress}", name );
	pfreplace( pf, it->pf, key, "server_ipaddress", "string" );

	sprintf( key, "import_from{%s}{send_heartbeat_sec}", name );
	pfreplace( pf, it->pf, key, "send_heartbeat_sec", "int" );

	sprintf( key, "import_from{%s}{send_heartbeat_string}", name );
	pfreplace( pf, it->pf, key, "send_heartbeat_string", "string" );

	sprintf( key, "import_from{%s}{expect_heartbeat_sec}", name );
	pfreplace( pf, it->pf, key, "expect_heartbeat_sec", "int" );

	sprintf( key, "import_from{%s}{expect_heartbeat_string}", name );
	pfreplace( pf, it->pf, key, "expect_heartbeat_string", "string" );

	sprintf( key, "import_from{%s}{select}", name );
	pfreplace( pf, it->pf, key, "select", "string" );

	sprintf( key, "import_from{%s}{reject}", name );
	pfreplace( pf, it->pf, key, "reject", "string" );

	sprintf( key, "import_from{%s}{loglevel}", name );
	pfreplace( pf, it->pf, key, "loglevel", "string" );

	sprintf( key, "import_from{%s}{my_inst}", name );
	pfreplace( pf, it->pf, key, "my_inst", "string" );

	sprintf( key, "import_from{%s}{my_mod}", name );
	pfreplace( pf, it->pf, key, "my_mod", "string" );

	sprintf( key, "import_from{%s}{srcname_morph}", name );
	pfreplace( pf, it->pf, key, "srcname_morph", "tbl" );

	if( it->new ) {

		ret = thr_create( NULL, 0, ew2orb_import, 
				  (void *) name, 
				  THR_DETACHED|THR_SUSPENDED,
				  &it->thread_id );

		if( ret != 0 ) {

			complain( 1,
			    "'%s': Failed to create import thread: "
			    "thr_create error %d\n", name, ret );
			
			mutex_unlock( &it->it_mutex );

			free_ImportThread( &it );

			return;

		} else {

			add_import_thread( name, it );

			thr_continue( it->thread_id );
		}

	} else if( pfcmp( oldpf, it->pf ) ) {

		it->update = 1;

	} else {

		it->update = 0;
	}

	if( oldpf ) {

		pffree( oldpf );
	}

	if( Flags.VeryVerbose ) {

		if( it->new ) {

			elog_notify( 0,
				"'%s': Started import thread as thread-id %d\n", 
				it->name, it->thread_id );

		} else if( it->update ) {

			elog_notify( 0,
				"'%s': Posted updates for import thread "
				"(thread-id %d)\n", 
				it->name, it->thread_id );

		} else {

			elog_notify( 0,
				"'%s': Import thread (thread-id %d) unchanged\n",
				it->name, it->thread_id );
		}
	}

	mutex_unlock( &it->it_mutex );

	return;
}

static void
reconfigure_import_threads( Pf *pf )
{
	Pf	*pfimport_from;
	Tbl	*new_keys;
	Tbl	*existing_keys;
	Arr	*anarr;
	char	*akey;
	int	i;

	if( pfget( pf, "import_from", (void **) &pfimport_from ) != PFARR ) {

		complain( 1, 
		   "parameter 'import_from' not present or not an array\n" );

		stop_all_import_threads();
		
		if( Flags.verbose && num_import_threads() <= 0 ) {

			complain( 0, 
			"Warning: no import threads defined; nothing to do\n" );
		}

		return;
	} 

	new_keys = pfkeys( pfimport_from );

	if( maxtbl( new_keys ) <= 0 ) {

		stop_all_import_threads();
		
		if( Flags.verbose && num_import_threads() <= 0 ) {

			complain( 0, 
			"Warning: no import threads defined; nothing to do\n" );
		}

		freetbl( new_keys, 0 );

		return;
	} 

	existing_keys = import_thread_names();

	for( i = 0; i < maxtbl( existing_keys ); i++ ) {
		
		akey = gettbl( existing_keys, i );

		if( ( anarr = pfget_arr( pfimport_from, akey ) ) == NULL ) {

			stop_import_thread( akey ); 

		} else {

			update_import_thread( akey, pf );

			freearr( anarr, 0 );
		}
	}

	freetbl( existing_keys, free );
		
	for( i = 0; i < maxtbl( new_keys ); i++ ) {

		akey = gettbl( new_keys, i );

		if( find_import_thread_byname( akey ) != 0 ) {

			continue;
		}

		update_import_thread( akey, pf );
	}

	if( Flags.verbose && num_import_threads() <= 0 ) {

		complain( 0, 
		"Warning: no import threads defined; nothing to do\n" );
	}

	freetbl( new_keys, 0 );

	return;
}

static int
crack_packet( Ew2orbPacket *e2opkt )
{
	int	retcode = 0;
	int	*out = 0;
	int	outsize = 0;
	int	nout = 0;
	PktChannel *pktchan;
	char	*dp;
	char	old_srcname[ORBSRCNAME_SIZE];
	char	new_srcname[ORBSRCNAME_SIZE];
	int	n;

	if( ! strcmp( e2opkt->typestr, Default_TYPE_TRACEBUF ) ) {

		/* Acquiesce to Earthworm packing strategy of 
		   memory-copying the entire trace-header structure. 
		   This was an unwise protocol design since the 
		   field order in a structure, officially, is at
		   the whim of the compiler implementation. */

		allot( TRACE_HEADER *, e2opkt->th, 1 );

		memcpy( e2opkt->th, e2opkt->buf, sizeof( TRACE_HEADER ) );

		dp = e2opkt->buf + sizeof( TRACE_HEADER );

		e2opkt->pkt = newPkt();
		e2opkt->pkt->pkttype = suffix2pkttype("MGENC");

		e2opkt->pkt->nchannels = 1;

		pktchan = newPktChannel();

		pushtbl( e2opkt->pkt->channels, pktchan );

		strcpy( e2opkt->pkt->parts.src_net, e2opkt->th->net );
		strcpy( e2opkt->pkt->parts.src_sta, e2opkt->th->sta );
		strcpy( e2opkt->pkt->parts.src_chan, e2opkt->th->chan );
		strcpy( e2opkt->pkt->parts.src_loc, "" );

		join_srcname( &e2opkt->pkt->parts, old_srcname );

		mutex_lock( &e2opkt->it->it_mutex );
		
		if( strcmp( e2opkt->it->reject, "" ) ) {

		    	n = strmatches( old_srcname, 
					e2opkt->it->reject,
					&e2opkt->it->reject_hook );

			if( n < 0 ) { 

				complain( 0, 
				"Couldn't compile reject expression '%s'\n",
				e2opkt->it->reject );

			} else if( n > 0 ) {

				if( e2opkt->it->loglevel == VERYVERBOSE ) {

					elog_notify( 0, "'%s': Rejecting %s\n",
						     e2opkt->itname,
						     old_srcname );
				}

				mutex_unlock( &e2opkt->it->it_mutex );

				return -1;
			}
		}

		if( strcmp( e2opkt->it->select, "" ) ) {

		    	n = strmatches( old_srcname, 
					e2opkt->it->select,
					&e2opkt->it->select_hook );

			if( n < 0 ) { 

				complain( 0, 
				"Couldn't compile select expression '%s'\n",
				e2opkt->it->select );

			} else if( n == 0 ) {

				if( e2opkt->it->loglevel == VERYVERBOSE ) {

					elog_notify( 0, 
					"'%s': %s doesn't match select expression\n",
						     e2opkt->itname,
						     old_srcname );
				}

				mutex_unlock( &e2opkt->it->it_mutex );

				return -1;
			}
		}

		n = morphtbl( old_srcname, e2opkt->it->srcname_morphmap, 
			      MORPH_ALL|MORPH_PARTIAL, new_srcname );

		mutex_unlock( &e2opkt->it->it_mutex );

		split_srcname( new_srcname, &e2opkt->pkt->parts );

		if( e2opkt->it->loglevel == VERYVERBOSE && 
		    ( n != 0 || strcmp( old_srcname, new_srcname ) ) ) {

			elog_notify( 0, "'%s': mapped %s to %s (%d "
					"transformations )\n", 
					e2opkt->itname, old_srcname, 
					new_srcname, n );
		}

		strcpy( pktchan->net, e2opkt->pkt->parts.src_net );
		strcpy( pktchan->sta, e2opkt->pkt->parts.src_sta );
		strcpy( pktchan->chan, e2opkt->pkt->parts.src_chan );
		strcpy( pktchan->loc, e2opkt->pkt->parts.src_loc );

		pktchan->nsamp = e2opkt->th->nsamp;
		pktchan->samprate = e2opkt->th->samprate;
		pktchan->time = e2opkt->th->starttime;
		pktchan->calib = 0; /* SCAFFOLD */
		pktchan->calper = -1; /* SCAFFOLD */

		allot( int *, pktchan->data, pktchan->nsamp * sizeof( int ) );

		if( ! strcmp( e2opkt->th->datatype, "s4" ) ) {

			mi2hi( &dp, pktchan->data, pktchan->nsamp );

		} else if( ! strcmp( e2opkt->th->datatype, "s2" ) ) {

			ms2hi( &dp, pktchan->data, pktchan->nsamp );

		} else if( ! strcmp( e2opkt->th->datatype, "i4" ) ) {

			vi2hi( &dp, pktchan->data, pktchan->nsamp );

		} else if( ! strcmp( e2opkt->th->datatype, "i2" ) ) {

			vs2hi( &dp, pktchan->data, pktchan->nsamp );
		}
		
		pktchan->datasz = e2opkt->th->nsamp;

		stuffPkt( e2opkt->pkt, 
			  e2opkt->srcname, 
			  &e2opkt->time,
			  &e2opkt->orbpkt, 
			  &e2opkt->orbpktnbytes, 
			  &e2opkt->orbpktsz );

	} else if( ! strcmp( e2opkt->typestr, Default_TYPE_TRACE_COMP_UA ) ) {

		allot( TRACE_HEADER *, e2opkt->th, 1 );

		memcpy( e2opkt->th, e2opkt->buf, sizeof( TRACE_HEADER ) );

		/* SCAFFOLD genuncompress( &out, &nout, &outsize, 
			       e2opkt->buf, e2opkt->nbytes );
		  free( out );
		 */


		retcode = -1;

	} else {
		complain( 0, 
			"convert: Don't know how to crack type <%s>\n",
			e2opkt->typestr );

		retcode = -1;
	}

	return retcode;
}

static void *
ew2orb_convert( void *arg )
{
	Ew2orbPacket *e2opkt;

	thr_setprio( thr_self(), THR_PRIORITY_CONVERT );

	while( mtfifo_pop( E2oPackets_mtf, (void **) &e2opkt ) != 0 ) {

		crack_packet( e2opkt );

		if( e2opkt->loglevel == VERYVERBOSE ) {

			describe_packet( e2opkt );
		}

		if( e2opkt->orbpkt != NULL && e2opkt->orbpktnbytes != 0 ) {

			orbput( Orbfd, e2opkt->srcname, 
			       	       e2opkt->time,
				       e2opkt->orbpkt, 
				       e2opkt->orbpktnbytes );
		}
				

		free_Ew2orbPacket( e2opkt );
	}

	return NULL;
}

static void *
ew2orb_pfwatch( void *arg )
{
	Pf	*pf;
	int	rc;

	thr_setprio( thr_self(), THR_PRIORITY_PFWATCH );

	rwlock_init( &Import_Threads_rwlock, USYNC_THREAD, NULL );

	Import_Threads = newarr( 0 );

	memset( &Ewinfo, '\0', sizeof( Earthworm_Info ) );

	mutex_init( &Ewinfo.ew_mutex, USYNC_THREAD, NULL );

	strcpy( Ewinfo.pfname, DEFAULT_EARTHWORM_PFNAME );

	for( ;; ) {

		refresh_earthworm_info();

		rc = pfupdate( Pfname, &pf );

		if( rc < 0 ) {
			
			complain( 1, "pfupdate pf parameter file '%s' failed\n",
				  Pfname );

		} else if( rc == 0 ) {
			
			; /* No reconfiguration necessary */

		} else if( rc == 1 ) {

			set_program_loglevel( pf );

			if( Flags.verbose ) {
				
				elog_notify( 0, 
				"Reconfiguring ew2orb from parameter file\n" );
			}

			reconfigure_import_threads( pf );
		}

		sleep( PFWATCH_SLEEPTIME_SEC );
	}

	return NULL;
}

static int
have_banner()
{
	struct stat buf;

	if( ! is_file( DEFAULT_BANNER_PATH ) ) {

		return 0;
	}

	if( stat( DEFAULT_BANNER_PATH, &buf ) == 0 && (S_IXOTH & buf.st_mode) ) {

		return 1;

	} else {

		return 0;
	}
}

int
main( int argc, char **argv )
{
	char	c;
	char	*orbname = 0;
	int	rc;

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
		
		orbname = argv[optind++];
	}

	if( ( Orbfd = orbopen( orbname, "w&" ) ) < 0 ) {
		
		die( 0, "Failed to open orb '%s' for writing\n", orbname );
	}

	Flags.have_banner = have_banner();

	sigignore( SIGPIPE );

	E2oPackets_mtf = mtfifo_create( PACKET_QUEUE_SIZE, 1, 0 );

	rc = thr_create( NULL, 0, ew2orb_pfwatch, 0, 0, 0 );

	if( rc != 0 ) {

		die( 1, "Failed to create parameter-file watch thread, "
			"thr_create error %d\n", rc );
	}

	rc = thr_create( NULL, 0, ew2orb_convert, 0, 0, 0 );

	if( rc != 0 ) {

		die( 1, "Failed to create packet-conversion thread, "
			"thr_create error %d\n", rc );
	}

	while( thr_join( (thread_t) NULL, 
			 (thread_t *) NULL, 
			 (void **) NULL ) == 0 );

	if( Flags.verbose ) {

		elog_notify( 0, "Program terminated\n" );
	}

	return( 0 );
}
