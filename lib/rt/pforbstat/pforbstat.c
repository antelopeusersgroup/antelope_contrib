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
#define STREQ(a, b) (strcmp((a), (b)) == 0)
#define STREQN(a, b, n) (strncmp((a), (b), (n)) == 0)

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
extract_orb2orb_orbargs( char *what, char *cmdline_fromorb, char *cmdline_toorb )
{
	char	*split_what;
	Tbl	*orb2orb_args;

	split_what = strdup( what );
	orb2orb_args = split( split_what, ' ' );

	shifttbl( orb2orb_args );
	strcpy( cmdline_fromorb, shifttbl( orb2orb_args ) );

	if( cmdline_fromorb[0] == '-' ) {

		/* Old style orb2orb command-line; 
		 * assume no start-time, period, 
		 * or end-time are specified:
		 */
		strcpy( cmdline_toorb, poptbl( orb2orb_args ) );
		strcpy( cmdline_fromorb, poptbl( orb2orb_args ) );
		
	} else {

		strcpy( cmdline_toorb, shifttbl( orb2orb_args ) );
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
		
	if( STREQ( orbname, ":" ) ) {
		
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

	if( STREQ( orbname_port, "" ) ) {
		
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

static void
translate_nonroutable_address( char *serveraddress, char *result )
{
	FILE	*fp;
	char	aline[STRSZ];
	char	anaddress[STRSZ];
	char	ipc[STRSZ];
	struct in_addr addr;
	Tbl	*parts;
	int	rc;
	static Hook *hook = 0;

	if( STREQN( serveraddress, "192.168", 7 ) ) {

		ip2name( inet_addr( serveraddress ), anaddress );

		if( strmatches( anaddress, "[.]", &hook ) ) {
			
			strcpy( result, serveraddress );

		} else if( ( fp = fopen( "/etc/resolv.conf", "r" ) ) == NULL ) {

			complain( 1, "Couldn't open resolv.conf\n" );

		}  else {

			while( getaline( fp, aline, STRSZ ) ) {

				if( ! STREQN( aline, "domain", 6 ) ) {

					continue;
				}
				parts = split( aline, ' ' );

				sprintf( anaddress, "%s.%s", 
					 anaddress, poptbl( parts ) );

				freetbl( parts, 0 );
			}

			fclose( fp );
		}

		rc = name2ip( anaddress, &addr, ipc );

		if( rc == 0 ) {
			strcpy( result, ipc );
		} else {
			complain( 1, "name2ip failed for %s\n", anaddress );
			strcpy( result, serveraddress );
		}

	} else {

		strcpy( result, serveraddress );
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
	char	o2omachine[STRSZ];
	char	orbstat_machine_hostname[STRSZ];
	char	orbstat_machine_ipc[STRSZ];
	long	orbstat_machine_ip;
	char	cmdline_fromorb[STRSZ];
	char	cmdline_toorb[STRSZ];
	char	cmdline_fromip[STRSZ];
	char	cmdline_toip[STRSZ];
	char	*reject;
	char	*select;
	int	cmdline_fromport;
	int	cmdline_toport;
	double	latency_sec;
	struct in_addr addr;

	regcomp( &preg_findclient, "^orb2orb ", 0 );

	pf = pfnew( PFFILE );

	atime = pfget_time( pfanalyze, "client_when" );
	pfput_time( pf, "connections_when", atime );

	my_ip( orbstat_machine_hostname, 
	       orbstat_machine_ipc, 
	       &orbstat_machine_ip );

	pfget( pfanalyze, "server", (void **) &pfserver );
	serveraddress = pfget_string( pfserver, "address" );
	serverport = pfget_int( pfserver, "port" );

	translate_nonroutable_address( orbstat_machine_ipc, anaddress );
	pfput_string( pf, "orbstat_machine", anaddress );

	translate_nonroutable_address( serveraddress, anaddress );
	pfput_string( pfserver, "address", anaddress );
	serveraddress = pfget_string( pfserver, "address" );

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

			strcpy( o2omachine, "?" );

			extract_orb2orb_orbargs( what, 
						 cmdline_fromorb, 
						 cmdline_toorb );

			parse_orbname( cmdline_fromorb, 
				       cmdline_fromip, 
				       &cmdline_fromport );

			parse_orbname( cmdline_toorb, 
				       cmdline_toip, 
				       &cmdline_toport );

			pfput_string( pfconnection, 
				      "cmdline_fromorb", cmdline_fromorb );

			pfput_string( pfconnection, 
				      "cmdline_toorb", cmdline_toorb );

			pfput_string( pfconnection, 
				      "cmdline_fromip", cmdline_fromip );

			pfput_string( pfconnection, 
				      "cmdline_toip", cmdline_toip );

			pfput_int( pfconnection, 
				   "cmdline_fromport", cmdline_fromport );

			pfput_int( pfconnection, 
				   "cmdline_toport", cmdline_toport );

			if( ( reject = pfget_string( pfclient, "reject") ) != NULL ) {

				pfput_string( pfconnection, "reject", reject );
			}
			
			if( ( select = pfget_string( pfclient, "select") ) != NULL ) {

				pfput_string( pfconnection, "select", select );
			}
			
			if( pfget_string( pfclient, "latency_sec" ) != NULL ) {

				latency_sec =
					pfget_double( pfclient, "latency_sec" );

				pfput_double( pfconnection, 
					      "latency_sec", latency_sec );

			} else {

				pfput_double( pfconnection, 
					      "latency_sec", -9999999.99999 );
			}

			perm = pfget_string( pfclient, "perm" );

			clientaddress = pfget_string( pfclient, "address" );

			if( STREQ( perm, "r" ) ) {

				pfput_string( pfconnection, 
					      "fromaddress", serveraddress );

				pfput_int( pfconnection, 
					   "fromport", serverport );

				if( STREQ( clientaddress, "127.0.0.1" ) &&
				    ! STREQ( cmdline_fromip, "" ) ) {
					
					name2ip( cmdline_fromip, 
						 &addr, anaddress );

				} else if( STREQ( clientaddress, "127.0.0.1" ) 
					   && STREQ( cmdline_fromip, "" ) ) {

					/* SCAFFOLD not convinced this
					   is correct: */
					strcpy( anaddress, serveraddress );

				} else if( STREQN( clientaddress, "192.168", 7 ) 
					   && ! STREQ( cmdline_fromip, "" ) ) {

					translate_nonroutable_address( 
						clientaddress, anaddress );

				} else if( STREQN( clientaddress, "192.168", 7 ) 
					   && STREQ( cmdline_fromip, "" ) ) {

					translate_nonroutable_address( 
						clientaddress, anaddress );

				} else {

					strcpy( anaddress, clientaddress );
				}

				pfput_string( pfconnection, 
					      "toaddress", anaddress );

				pfput_int( pfconnection, 
					      "toport", cmdline_toport );

			} else { /* perm is "w" */

				pfput_string( pfconnection, 
					      "toaddress", serveraddress );

				pfput_int( pfconnection, "toport", serverport );

				if( STREQ( clientaddress, "127.0.0.1" ) &&
				    ! STREQ( cmdline_fromip, "" ) ) {
					
					name2ip( cmdline_fromip, 
						 &addr, anaddress );

				} else if( STREQ( clientaddress, "127.0.0.1" ) 
					   && STREQ( cmdline_fromip, "" ) ) {

					/* SCAFFOLD not convinced this
					   is correct: */
					strcpy( anaddress, serveraddress );

				} else if( STREQN( clientaddress, "192.168", 7 ) 
					   && ! STREQ( cmdline_fromip, "" ) ) {

					translate_nonroutable_address( 
						clientaddress, anaddress );

				} else if( STREQN( clientaddress, "192.168", 7 ) 
					   && STREQ( cmdline_fromip, "" ) ) {

					translate_nonroutable_address( 
						clientaddress, anaddress );

				} else {

					strcpy( anaddress, clientaddress );
				}

				pfput_string( pfconnection, 
					      "fromaddress", anaddress );

				pfput_int( pfconnection, 
					   "fromport", cmdline_fromport );
			}

			if( STREQ( clientaddress, "127.0.0.1" ) ) {

				strcpy( o2omachine, clientaddress );

			}  else if( STREQ( clientaddress, serveraddress ) ) {

				strcpy( o2omachine, serveraddress );
			}

			pfput_string( pfconnection, "o2omachine", o2omachine );

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
