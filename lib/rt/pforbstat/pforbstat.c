#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <inttypes.h>
#include <regex.h>
#include "stock.h"
#include "coords.h"
#include "orb.h"
#include "pf.h"

#include "pforbstat.h"

#define BITS_PER_BYTE 8
#define BYTES_PER_K 1024

static double
compute_kbaud( Orbsrc *src )
{
	double	kbaud;
	double	timespan;

	if( ( timespan = src->slatest_time - src->soldest_time ) <= 0 ) {

		kbaud = -1;

	} else {

		kbaud = src->nbytes * BITS_PER_BYTE / 
			( timespan * BYTES_PER_K );
	}

	return kbaud;
}

static double
compute_latency( double when, Orbsrc *src )
{
	return when - src->slatest_time;
}

Pf *
orbstat2pf( Orbstat *orbstat, int orbversion ) 
{
	Pf	*pf;
	char	*s;
	struct in_addr in;

	if( orbstat == (Orbstat *) NULL ) {

		return (Pf *) NULL;
	} 

	pf = pfnew( PFARR );

	if( orbversion > 0 ) {
		pfput_int( pf, "orbversion", orbversion );
	}
	
	pfput_time( pf, "when", orbstat->when );
	pfput_time( pf, "started", orbstat->started );
	pfput_time( pf, "orb_start", orbstat->orb_start );

	pfput_boolean( pf, "connections", orbstat->connections );
	pfput_boolean( pf, "messages", orbstat->messages );

	pfput_int( pf, "maxdata", orbstat->maxdata );
	pfput_int( pf, "errors", orbstat->errors );
	pfput_int( pf, "rejected", orbstat->rejected );
	pfput_int( pf, "closes", orbstat->closes );
	pfput_int( pf, "opens", orbstat->opens );
	pfput_int( pf, "port", orbstat->port );

	memcpy( &in, &orbstat->address, sizeof( struct in_addr ) );
	pfput_string( pf, "address", inet_ntoa( in ) );

	pfput_int( pf, "pid", orbstat->pid );
	pfput_int( pf, "nsources", orbstat->nsources );
	pfput_int( pf, "nclients", orbstat->nclients );
	pfput_int( pf, "maxsrc", orbstat->maxsrc );
	pfput_int( pf, "maxpkts", orbstat->maxpkts );

	s = strdup( orbstat->version );
	strtrim( s );
	pfput_string( pf, "version", s );
	free( s );

	pfput_string( pf, "who", orbstat->who );
	pfput_string( pf, "host", orbstat->host );

	return pf;
}

Pf *
orbsources2pf( double atime, Orbsrc *sources, int nsources )
{
	Orbsrc	*asource;
	Pf 	*pf;
	Pf	*pfsources;
	Pf	*sourcepf;
	int	isource;
	double 	kbaud;
	double	latency_sec;

	pf = pfnew( PFFILE );

	pfput_time( pf, "source_when", atime );

	pfsources = pfnew( PFARR );

	for( isource = 0; isource < nsources; isource++ ) {

		sourcepf = pfnew( PFARR );

		asource = &sources[isource];

		pfput_time( sourcepf, "slatest_time", asource->slatest_time );
		pfput_time( sourcepf, "soldest_time", asource->soldest_time );

		pfput_int( sourcepf, "nbytes", asource->nbytes );
		pfput_int( sourcepf, "npkts", asource->npkts );
		pfput_int( sourcepf, "slatest", asource->slatest );
		pfput_int( sourcepf, "soldest", asource->soldest );

		pfput_boolean( sourcepf, "active", asource->active );
		
		if( ( kbaud = compute_kbaud( asource ) ) >= 0 ) {

			pfput_double( sourcepf, "kbaud", kbaud );
		}

		latency_sec = compute_latency( atime, asource );

		pfput_double( sourcepf, "latency_sec", latency_sec );

		pfput( pfsources, asource->srcname, sourcepf, PFPF );
	}

	pfput( pf, "sources", pfsources, PFPF );

	return pf;
}

Pf *
orbclients2pf( double atime, Orbclient *clients, int nclients )
{
	Orbclient *aclient;
	Pf 	*pf;
	Pf	*pfclients;
	Pf 	*clientpf;
	int	iclient;
	char	thread[STRSZ];
	char	perm_string[2];
	struct in_addr in;
	double	latency_sec;

	pf = pfnew( PFFILE );

	pfput_time( pf, "client_when", atime );

	pfclients = pfnew( PFARR );

	for( iclient = 0; iclient < nclients; iclient++ ) {

		clientpf = pfnew( PFARR );
		
		aclient = &clients[iclient];

		pfput_time( clientpf, "lastpkt", aclient->lastpkt );
		pfput_time( clientpf, "started", aclient->started );

		pfput_int( clientpf, "read", aclient->read );
		pfput_int( clientpf, "pid", aclient->pid );
		pfput_int( clientpf, "bytes", aclient->bytes );
		pfput_int( clientpf, "packets", aclient->packets );
		pfput_int( clientpf, "pktid", aclient->pktid );
		pfput_int( clientpf, "port", aclient->port );

		memcpy( &in, &aclient->address, sizeof( struct in_addr ) );
		pfput_string( clientpf, "address", inet_ntoa( in ) );

		pfput_int( clientpf, "thread", aclient->thread );
		pfput_int( clientpf, "fd", aclient->fd );
		pfput_int( clientpf, "nreject", aclient->nreject );
		pfput_int( clientpf, "nselect", aclient->nselect );
		pfput_int( clientpf, "errors", aclient->errors );
		pfput_int( clientpf, "priority", aclient->priority );
		pfput_int( clientpf, "lastrequest", aclient->lastrequest );
		pfput_int( clientpf, "mymessages", aclient->mymessages );
		pfput_int( clientpf, "nrequests", aclient->nrequests );
		pfput_int( clientpf, "nwrites", aclient->nwrites );
		pfput_int( clientpf, "nreads", aclient->nreads );
		pfput_int( clientpf, "written", aclient->written );

		perm_string[0] = aclient->perm;
		perm_string[1] = '\0';
		pfput_string( clientpf, "perm", perm_string );

		pfput_string( clientpf, "what", aclient->what );
		pfput_string( clientpf, "host", aclient->host );
		pfput_string( clientpf, "who", aclient->who );
		pfput_string( clientpf, "select", aclient->select );
		pfput_string( clientpf, "reject", aclient->reject );

		if( aclient->lastpkt > 0 ) {

			latency_sec = atime - aclient->lastpkt;

			pfput_double( clientpf, "latency_sec", latency_sec );
		}

		sprintf( thread, "%d", aclient->thread );

		pfput( pfclients, thread, clientpf, PFPF );
	}

	pfput( pf, "clients", pfclients, PFPF );

	return pf;
}

static Pf *
orbconnections2pf( Pf *pfanalyze )
{
	Pf	*pf;
	Pf	*pfserver;
	Pf	*pfconnections;
	Pf	*pfclients;
	Pf	*pfclient;
	Pf	*pfconnection;
	Tbl	*client_keys;
	int	ikey;
	char	*client_key;
	char	*what;
	char	*perm;
	char	*clientaddress;
	char	*serveraddress;
	int	serverport;
	double	atime;
	regex_t	preg;
	char	formal_name[STRSZ];
	int	formal_count = 0;

	regcomp( &preg, "^orb2orb ", 0 );

	pf = pfnew( PFFILE );

	atime = pfget_time( pfanalyze, "client_when" );

	pfput_time( pf, "connections_when", atime );

	pfget( pfanalyze, "server", (void **) &pfserver );
	serveraddress = pfget_string( pfserver, "address" );
	serverport = pfget_int( pfserver, "port" );

	pfget( pfanalyze, "clients", (void **) &pfclients );

	client_keys = pfkeys( pfclients );

	pfconnections = pfnew( PFTBL );

	for( ikey = 0; ikey < maxtbl( client_keys ); ikey++ ) {

		client_key = gettbl( client_keys, ikey );
		pfget( pfclients, client_key, (void **) &pfclient );

		what = pfget_string( pfclient, "what" );
		perm = pfget_string( pfclient, "perm" );
		clientaddress = pfget_string( pfclient, "address" );

		if( ! regexec( &preg, what, 0, 0, 0 ) ) {

			pfconnection = pfnew( PFARR );
			pfput_string( pfconnection, "what", what );

			if( ! strcmp( perm, "r" ) ) {

				pfput_string( pfconnection, "toaddress", clientaddress );
				pfput_string( pfconnection, "fromaddress", serveraddress );
				pfput_int( pfconnection, "fromport", serverport );

			} else {

				pfput_string( pfconnection, "fromaddress", clientaddress );
				pfput_string( pfconnection, "toaddress", serveraddress );
				pfput_int( pfconnection, "toport", serverport );
			}

			sprintf( formal_name, "client%d", ++formal_count );
			pfput( pfconnections, formal_name, pfconnection, PFPF );
		}
	}

	pfput( pf, "connections", pfconnections, PFPF );

	regfree( &preg );

	return pf;
}

Pf *
pforbstat( int orbfd, int flags )
{
	Pf	*pf = 0;
	Pf	*pfans;
	Orbstat *orbstatus = 0;
	Orbsrc	*sources = 0;
	Orbclient *clients = 0;
	double	atime;
	int	orbversion;
	int	nsources = 0;
	int	nclients = 0;
	char	*s;

	if( orbfd <= 0 ) {

		return (Pf *) NULL;
	}

	if( flags & PFORBSTAT_CONNECTIONS ) {

		flags |= PFORBSTAT_CLIENTS;
	}

	orbping( orbfd, &orbversion );

	pf = pfnew( PFFILE );

	if( flags & PFORBSTAT_SERVER ) {
		
		orbstat( orbfd, &orbstatus );
		pfans = orbstat2pf( orbstatus, orbversion );

		pfput( pf, "server", pfans, PFPF );
	}

	if( flags & PFORBSTAT_SOURCES ) { 

		orbsources( orbfd, &atime, &sources, &nsources ); 
		pfans = orbsources2pf( atime, sources, nsources );
		
		pfcompile( s = pf2string( pfans ), &pf );
		free( s );
		
		pffree( pfans );
	}

	if( flags & PFORBSTAT_CLIENTS ) {

		orbclients( orbfd, &atime, &clients, &nclients );
		pfans = orbclients2pf( atime, clients, nclients ); 

		pfcompile( s = pf2string( pfans ), &pf );
		free( s );
	
		pffree( pfans );
	}

	if( flags & PFORBSTAT_CONNECTIONS ) {

		pfans = orbconnections2pf( pf );

		pfcompile( s = pf2string( pfans ), &pf );
		free( s );
	
		pffree( pfans );
	}

	return pf;
}
