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
	char	name[STRSZ];
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

		memcpy( &in.s_addr, &aclient->address, sizeof( struct in_addr ) );
		pfput_string( clientpf, "address", inet_ntoa( in ) );

		ip2name( in.s_addr, name );
		pfput_string( clientpf, "name", name );

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

static void
extract_orb2orb_orbargs( char *what, char *orb_from_arg, char *orb_to_arg )
{
	char	*split_what;
	Tbl	*orb2orb_args;

	split_what = strdup( what );
	orb2orb_args = split( split_what, ' ' );

	shifttbl( orb2orb_args );
	strcpy( orb_from_arg, shifttbl( orb2orb_args ) );

	if( orb_from_arg[0] == '-' ) {

		/* Old style orb2orb command-line; 
		 * assume no start-time, period, 
		 * or end-time are specified:
		 */
		strcpy( orb_to_arg, poptbl( orb2orb_args ) );
		strcpy( orb_from_arg, poptbl( orb2orb_args ) );
		
	} else {

		strcpy( orb_to_arg, shifttbl( orb2orb_args ) );
	}

	free( split_what );
	freetbl( orb2orb_args, 0 );

	return;
}

static void
parse_orbname( char *orbname, char *orb_address, int *orb_port )
{
	char	*split_orbname;
	Tbl	*orbname_parts;
	char	orbname_port[STRSZ];
	Hook	*hook = 0;
	static Pf *pfnames = 0;
		
	if( ! strcmp( orbname, ":" ) ) {
		
		strcpy( orb_address, "" );
		strcpy( orbname_port, "" );

	} else {
		
		split_orbname = strdup( orbname );
		orbname_parts = split( split_orbname, ':' );

		if( maxtbl( orbname_parts ) == 1 && orbname[0] == ':' ) {

			strcpy( orb_address, "" );
			strcpy( orbname_port, poptbl( orbname_parts ) );

		} else if( maxtbl( orbname_parts ) == 1 ) {

			strcpy( orb_address, shifttbl( orbname_parts ) );
			strcpy( orbname_port, "" );

		} else if( maxtbl( orbname_parts ) == 2 ) {

			strcpy( orb_address, shifttbl( orbname_parts ) );
			strcpy( orbname_port, poptbl( orbname_parts ) );

		} else {

			complain( 0, "pforbstat: unexpected error translating orb2orb argument <%s>\n",
				  orbname );
			strcpy( orb_address, "" );
			strcpy( orbname_port, "" );
		}

		free( split_orbname );
		freetbl( orbname_parts, 0 );
	}

	if( ! strcmp( orbname_port, "" ) ) {
		
		*orb_port = ORB_TCP_PORT;

	} else if( strmatches( orbname_port, "^[0-9]+$", &hook ) ) {
		
		*orb_port = atoi( orbname_port );

	} else {

		if( pfnames == 0 ) {

			pfread( "orbserver_names", &pfnames );
		}

		if( pfget_string( pfnames, orbname_port ) == 0 ) {

			complain( 0, "pforbstat: couldn't translate orb port \":%s\"\n", orbname_port );

			*orb_port = 0;

		} else {
		
			*orb_port = pfget_int( pfnames, orbname_port );
		}
	}

	return;
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
	char	anaddress[STRSZ];
	int	serverport;
	int	aport;
	double	atime;
	regex_t	preg_findclient;
	char	formal_name[STRSZ];
	int	formal_count = 0;
	char	myhostname[STRSZ];
	char	myipc[STRSZ];
	long	myip;
	char	orb_from_arg[STRSZ];
	char	orb_to_arg[STRSZ];
	char	orbarg_from_ip[STRSZ];
	char	orbarg_to_ip[STRSZ];
	int	orbarg_from_port;
	int	orbarg_to_port;
	double	latency_sec;
	struct in_addr addr;

	regcomp( &preg_findclient, "^orb2orb ", 0 );

	pf = pfnew( PFFILE );

	atime = pfget_time( pfanalyze, "client_when" );
	pfput_time( pf, "connections_when", atime );

	my_ip( myhostname, myipc, &myip );
	pfput_string( pf, "orbstat_machine", myipc );

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

		if( ! regexec( &preg_findclient, what, 0, 0, 0 ) ) {

			pfconnection = pfnew( PFARR );
			pfput_string( pfconnection, "what", what );

			extract_orb2orb_orbargs( what, orb_from_arg, orb_to_arg );
			parse_orbname( orb_from_arg, orbarg_from_ip, &orbarg_from_port );
			parse_orbname( orb_to_arg, orbarg_to_ip, &orbarg_to_port );

			pfput_string( pfconnection, "orb_from_arg", orb_from_arg );
			pfput_string( pfconnection, "orb_to_arg", orb_to_arg );
			pfput_string( pfconnection, "orbarg_from_ip", orbarg_from_ip );
			pfput_string( pfconnection, "orbarg_to_ip", orbarg_to_ip );
			pfput_int( pfconnection, "orbarg_from_port", orbarg_from_port );
			pfput_int( pfconnection, "orbarg_to_port", orbarg_to_port );

			if( pfget_string( pfclient, "latency_sec" ) != NULL ) {

				latency_sec = pfget_double( pfclient, "latency_sec" );
				pfput_double( pfconnection, "latency_sec", latency_sec );

			} else {

				pfput_double( pfconnection, "latency_sec", -9999999.99999 );
			}

			perm = pfget_string( pfclient, "perm" );

			clientaddress = pfget_string( pfclient, "address" );

			if( ! strcmp( perm, "r" ) ) {

				pfput_string( pfconnection, "fromaddress", serveraddress );
				pfput_int( pfconnection, "fromport", serverport );

				if( ! strcmp( clientaddress, "127.0.0.1" ) ) {
					
					name2ip( orbarg_from_ip, &addr, anaddress );
					printf( "DEBUG: <%s> is <%s>\n", orbarg_from_ip, anaddress );

				} else {

					strcpy( anaddress, clientaddress );
				}

				pfput_string( pfconnection, "toaddress", anaddress );
				pfput_int( pfconnection, "toport", aport );

			} else { /* perm is "w" */

				if( ! strcmp( clientaddress, "127.0.0.1" ) ) {
					
					name2ip( orbarg_from_ip, &addr, anaddress );
					printf( "DEBUG: <%s> is <%s>\n", orbarg_from_ip, anaddress );

				} else {

					strcpy( anaddress, clientaddress );
				}

				pfput_string( pfconnection, "fromaddress", anaddress );
				pfput_int( pfconnection, "fromport", orbarg_from_port );

				pfput_string( pfconnection, "toaddress", serveraddress );
				pfput_int( pfconnection, "toport", serverport );
			}

			sprintf( formal_name, "client%d", ++formal_count );
			pfput( pfconnections, formal_name, pfconnection, PFPF );
		}
	}

	pfput( pf, "connections", pfconnections, PFPF );

	regfree( &preg_findclient );

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
