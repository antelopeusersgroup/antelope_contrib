#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <inttypes.h>
#include <regex.h>
#include <string.h>
#include "stock.h"
#include "coords.h"
#include "orb.h"
#include "pf.h"
#include "md5.h"

#include "pforbstat.h"

#define BITS_PER_BYTE 8
#define BYTES_PER_K 1024
#define STREQ(a, b) (strcmp((a), (b)) == 0)
#define STREQN(a, b, n) (strncmp((a), (b), (n)) == 0)

static Arr *Nonroutable = 0;

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
	pfput_int( pf, "maxpkts", orbstat->maxpktid );

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
		pfput_int( clientpf, "lastrequest", aclient->lastrequest );
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
id_clients( Pf *pf )
{
	Pf	*pfclients;
	Pf	*pfclient;
	char	client_summary[STRSZ];
	char	*serveraddress;
	char	*serverport;
	Tbl	*client_keys;
	int	ikey;
	char	*client_key;
	char	clientid_key[STRSZ];
	char	*clientaddress;
	char	*what;
	char	clientid[33];

	pfeval( pf, "server{address}", &serveraddress );
	pfeval( pf, "server{port}", &serverport );

	pfget( pf, "clients", (void **) &pfclients );

	client_keys = pfkeys( pfclients );

	for( ikey = 0; ikey < maxtbl( client_keys ); ikey++ ) {

		client_key = gettbl( client_keys, ikey );

		pfget( pfclients, client_key, (void **) &pfclient );

		clientaddress = pfget_string( pfclient, "address" );
		what = pfget_string( pfclient, "what" );

		sprintf( client_summary, "%s %s %s %s", 
				serveraddress, serverport,
				clientaddress, what );

		sprintf( clientid_key, "clients{%s}{clientid}", client_key );

		mdhex( clientid, client_summary, strlen( client_summary ) );

		pfset( pf, clientid_key, clientid );
	}

	freetbl( client_keys, 0 );

	return;
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
	int	len = 0;
		
	if( STREQ( orbname, ":" ) ) {
		
		strcpy( orb_address, "127.0.0.1" );
		strcpy( orbname_port, "" );

	} else {
		
		split_orbname = strdup( orbname );
		orbname_parts = split( split_orbname, ':' );

		if( maxtbl( orbname_parts ) == 1 && orbname[0] == ':' ) {

			strcpy( orb_address, "127.0.0.1" );
			strcpy( orbname_port, poptbl( orbname_parts ) );

		} else if( maxtbl( orbname_parts ) == 1 ) {

			strcpy( orb_address, shifttbl( orbname_parts ) );
			strcpy( orbname_port, "" );

		} else if( maxtbl( orbname_parts ) == 2 ) {

			strcpy( orb_address, shifttbl( orbname_parts ) );
			strcpy( orbname_port, poptbl( orbname_parts ) );

		} else {

			elog_complain( 0, "pforbstat: unexpected error translating orb2orb argument <%s>\n",
				  orbname );
			strcpy( orb_address, "" );
			strcpy( orbname_port, "" );
		}

		free( split_orbname );
		freetbl( orbname_parts, 0 );
	}

	if( ( len = strlen( orbname_port ) ) > 0 ) {

		if( orbname_port[len-1] == '@' ) {
			
			orbname_port[len-1] = '\0';
		}
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

			elog_complain( 0, "pforbstat: couldn't translate orb port \":%s\"\n", orbname_port );

			*orb_port = 0;

		} else {
		
			*orb_port = pfget_int( pfnames, orbname_port );
		}
	}

	if( hook != (Hook *) NULL ) {

		free_hook( &hook );
	}

	return;
}

static int
is_localhost( char *address ) {
	
	if( STREQ( address, "localhost" ) || 
	    STREQ( address, "127.0.0.1" ) ) {

		return 1;

	} else {

		return 0;
	}
}

static int 
is_nonroutable( char *address )
{
	if( STREQN( address, "192.168", 7 ) ||
	    STREQN( address, "10.", 3 ) ||
	    STREQN( address, "172.", 4 ) ||
	    ( STREQN( address, "127.", 4 ) && ! STREQ( address, "127.0.0.1" ) ) ) {

		return 1;

	} else {
		
		return 0;
	}
}

static int
report_nonroutable( char *address )
{
	if( Nonroutable == (Arr *) NULL ) {
		
		Nonroutable = newarr( 0 );
	}

	if( getarr( Nonroutable, address ) == NULL ) {
		
		setarr( Nonroutable, address, (void *) 0x1 );

		return 1;

	} else {

		return 0;
	}
}

static int
is_dbprogram( char *what, char *dbprogram, char *dbpath ) 
{
	static Pf *pflib = 0;
	static Morphtbl *morphmap = 0;
	Tbl	*morphlist;
	int	rc;
	char	result[STRSZ];
	Tbl	*parts;

	rc = pfupdate( "libpforbstat", &pflib );

	if( rc < 0 || pflib == NULL ) {

		elog_log( 0, "pforbstat: failed to load libpforbstat.pf "
				   "during database analysis\n" );
		return 0;

	}  else if( rc == 0 && morphmap == NULL ) {

		elog_log( 0, "pforbstat: no morphmap present for database analysis\n" );
		return 0;
	}

	if( rc > 0 ) {

		if( morphmap != (Morphtbl *) NULL ) {

			freemorphtbl( morphmap );
			
			morphmap = 0;
		}

		morphlist = pfget_tbl( pflib, "dbprograms_morph" );

		if( morphlist == NULL ) {
			
			elog_log( 0, "pforbstat: failed to get dbprograms_morph "
					"from libpforbstat.pf during database analysis\n" );
			return 0;
		}
		
		rc = newmorphtbl( morphlist, &morphmap );

		freetbl( morphlist, 0 );

		if( rc != 0 ) {

			elog_log( 0, "pforbstat: %d errors translating dbprograms_morph "
					"from libpforbstat.pf during database analysis\n", rc );
			return 0;
		}
	} 
	
	rc = morphtbl( what, morphmap, 0, result );

	if( rc <= 0 ) {

		strcpy( dbprogram, "" );
		strcpy( dbpath, "" );

		return 0;

	} else {

		parts = split( result, ' ' );

		strcpy( dbprogram, gettbl( parts, 0 ) );
		strcpy( dbpath, gettbl( parts, 1 ) );
		
		freetbl( parts, 0 );

		return 1;
	} 
}

static Pf *
orbdatabases2pf( Pf *pfanalyze ) 
{
	Pf	*pf;
	Pf	*pfdatabases;
	Pf	*pfdatabase;
	Pf	*pfclients;
	Pf	*pfclient;
	double	atime;
	Tbl	*client_keys;
	int	ikey;
	char	*client_key;
	char	*what;
	char	*serverhost;
	char	*clientid;
	char	*clientaddress;
	char	*serveraddress;
	char	*serverport;
	char	dbprogram[STRSZ];
	char	dbpath[STRSZ];
	char	dir[STRSZ];
	char	dfile[STRSZ];
	char	formal_name[STRSZ];
	int	formal_count = 0;
	char	*delim = ":";
	char	*hostdir;
	char	*serverhostcopy;
	char	*abspath;

	pf = pfnew( PFFILE );

	pfdatabases = pfnew( PFTBL );

	atime = pfget_time( pfanalyze, "client_when" );
	pfput_time( pf, "databases_when", atime );

	pfeval( pfanalyze, "server{address}", &serveraddress );
	pfeval( pfanalyze, "server{port}", &serverport );
	pfeval( pfanalyze, "server{host}", &serverhost );

	serverhostcopy = strdup( serverhost );
	strtok_r( serverhostcopy, delim, &hostdir );

	pfget( pfanalyze, "clients", (void **) &pfclients );

	client_keys = pfkeys( pfclients );

	for( ikey = 0; ikey < maxtbl( client_keys ); ikey++ ) {

		client_key = gettbl( client_keys, ikey );
		pfget( pfclients, client_key, (void **) &pfclient );

		what = pfget_string( pfclient, "what" );
		clientaddress = pfget_string( pfclient, "address" );
		clientid = pfget_string( pfclient, "clientid" );

		if( is_dbprogram( what, dbprogram, dbpath ) ) {

			pfdatabase = pfnew( PFARR );

			pfput_string( pfdatabase, "clientid", clientid );
			pfput_string( pfdatabase, "serveraddress", serveraddress );
			pfput_string( pfdatabase, "serverport", serverport );
			pfput_string( pfdatabase, "dbprogram", dbprogram );

			if( is_localhost( clientaddress ) ) {

				pfput_string( pfdatabase, "dbmachine", serveraddress );

				abspath = concatpaths( hostdir, dbpath, 0 );
				parsepath( abspath, dir, dfile, 0 );
				free( abspath );

			} else {

				pfput_string( pfdatabase, "dbmachine", clientaddress );

				abspath = concatpaths( "", dbpath, 0 );
				parsepath( abspath, dir, dfile, 0 );
				free( abspath );
			}

			pfput_string( pfdatabase, "dir", dir );
			pfput_string( pfdatabase, "dfile", dfile );

			sprintf( formal_name, "client%03d", ++formal_count );
			pfput( pfdatabases, formal_name, pfdatabase, PFPF );
		}
	}

	freetbl( client_keys, 0 );

	free( serverhostcopy );

	pfput( pf, "databases", pfdatabases, PFPF );

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
	char	*clientid;
	char	*what;
	char	*perm;
	char	*clientaddress;
	char	*serveraddress;
	char	clientaddress_ipc[STRSZ];
	char	serveraddress_ipc[STRSZ];
	int	serverport;
	double	atime;
	regex_t	preg_findclient;
	char	closeorb[STRSZ];
	char	o2omachine[STRSZ];
	char	farorb[STRSZ];
	int	closeport;
	int	farport;
	char	orbstat_machine_hostname[STRSZ];
	char	orbstat_machine_ipc[STRSZ];
	int	orbstat_machine_ip;
	char	cmdline_fromorb[STRSZ];
	char	cmdline_toorb[STRSZ];
	char	cmdline_fromip[STRSZ];
	char	cmdline_fromipc[STRSZ];
	char	cmdline_toip[STRSZ];
	char	cmdline_toipc[STRSZ];
	char	formal_name[STRSZ];
	int	formal_count = 0;
	int	cmdline_fromport;
	int	cmdline_toport;
	struct in_addr addr;
	
	regcomp( &preg_findclient, "^orb2orb ", 0 );

	pf = pfnew( PFFILE );

	atime = pfget_time( pfanalyze, "client_when" );
	pfput_time( pf, "connections_when", atime );

	my_ip( orbstat_machine_hostname, 
	       orbstat_machine_ipc, 
	       &orbstat_machine_ip );

	if( is_localhost( orbstat_machine_ipc ) ) {
		
		elog_complain( 0, "libpforbstat: orbstat machine is localhost; giving up on connection analysis\n" );
		regfree( &preg_findclient );
		return pf;

	} else {

		pfput_string( pf, "orbstat_machine", orbstat_machine_ipc );
	}

	pfget( pfanalyze, "server", (void **) &pfserver );
	serveraddress = pfget_string( pfserver, "address" );
	serverport = pfget_int( pfserver, "port" );

	if( is_nonroutable( serveraddress ) && 
	    report_nonroutable( serveraddress ) ) {

		elog_complain( 0, "libpforbstat: warning: monitored server %s is nonroutable\n", serveraddress );
	}
	
	if( name2ip( serveraddress, &addr, serveraddress_ipc ) < 0 ) {
		
		elog_complain( 0, "libpforbstat: warning: name translation failed for %s\n", serveraddress );
		strcpy( serveraddress_ipc, serveraddress );
	}

	if( is_localhost( serveraddress ) ) {

		strcpy( closeorb, orbstat_machine_ipc );

	} else {

		strcpy( closeorb, serveraddress_ipc );
	}

	closeport = serverport;

	/* SCAFFOLD: this causes memory problems. Leave untranslated for now:
	pfput_string( pfserver, "address", closeorb ); Record the translated server address */

	pfget( pfanalyze, "clients", (void **) &pfclients );

	client_keys = pfkeys( pfclients );

	pfconnections = pfnew( PFTBL );

	for( ikey = 0; ikey < maxtbl( client_keys ); ikey++ ) {

		client_key = gettbl( client_keys, ikey );
		pfget( pfclients, client_key, (void **) &pfclient );

		what = pfget_string( pfclient, "what" );

		if( ! regexec( &preg_findclient, what, 0, 0, 0 ) ) {

			pfconnection = pfnew( PFARR );

			/* Easy things: */

			pfput_string( pfconnection, "what", what );

			if( ( clientid = pfget_string( pfclient, "clientid") ) != NULL ) {

				pfput_string( pfconnection, "clientid", clientid );
			}
			
			/* Preparatory raw-information acquisition: */

			extract_orb2orb_orbargs( what, 
						 cmdline_fromorb, 
						 cmdline_toorb );

			parse_orbname( cmdline_fromorb, 
				       cmdline_fromip, 
				       &cmdline_fromport );

			if( name2ip( cmdline_fromip, &addr, cmdline_fromipc ) < 0 ) {
		
				elog_complain( 0, 
					"libpforbstat: warning: name translation failed for %s\n", cmdline_fromipc );
				strcpy( cmdline_fromipc, cmdline_fromip );
			}

			parse_orbname( cmdline_toorb, 
				       cmdline_toip, 
				       &cmdline_toport );

			if( name2ip( cmdline_toip, &addr, cmdline_toipc ) < 0 ) {
		
				elog_complain( 0, 
					"libpforbstat: warning: name translation failed for %s\n", cmdline_toipc );
				strcpy( cmdline_toipc, cmdline_toip );
			}

			perm = pfget_string( pfclient, "perm" );

			clientaddress = pfget_string( pfclient, "address" );

			if( name2ip( clientaddress, &addr, clientaddress_ipc ) < 0 ) {
		
				elog_complain( 0, 
					"libpforbstat: warning: name translation failed for %s\n", clientaddress );
				strcpy( clientaddress_ipc, clientaddress );
			}

			/* Analysis */

			if( is_nonroutable( clientaddress ) &&
			    report_nonroutable( clientaddress ) ) { 
				
				elog_complain( 0, "libpforbstat: warning: clientaddress %s is nonroutable\n", 
						clientaddress );
			} 
			
			if( is_localhost( clientaddress ) ) {
			
				strcpy( o2omachine, serveraddress_ipc );

			} else {
				
				strcpy( o2omachine, clientaddress_ipc );
			}

			pfput_string( pfconnection, "o2omachine", o2omachine );

			if( STREQ( perm, "w" ) ) {

				strcpy( farorb, cmdline_fromipc );
				farport = cmdline_fromport;

			} else if( STREQ( perm, "r" ) ) {		

				strcpy( farorb, cmdline_toipc );
				farport = cmdline_toport;

			} else {
				elog_complain( 0, 
						"libpforbstat: unexpected perm '%s' in client info; giving up on client\n", 
						perm );
				pffree( pfconnection );
				continue;
			}

			if(is_localhost(farorb)) {

				strcpy( farorb, o2omachine );
			}

			if( STREQ( perm, "w" ) ) {
			
				pfput_string( pfconnection, "fromaddress", farorb );
				pfput_int( pfconnection, "fromport", farport );

				pfput_string( pfconnection, "toaddress", closeorb );
				pfput_int( pfconnection, "toport", closeport );

				pfput_string( pfconnection, "closeorb", "toaddress" );

			} else {	/* perm == "r" */

				pfput_string( pfconnection, "fromaddress", closeorb );
				pfput_int( pfconnection, "fromport", closeport );

				pfput_string( pfconnection, "toaddress", farorb );
				pfput_int( pfconnection, "toport", farport );

				pfput_string( pfconnection, "closeorb", "fromaddress" );
			} 	

			sprintf( formal_name, "client%03d", ++formal_count );
			pfput( pfconnections, formal_name, pfconnection, PFPF );
		}
	}

	freetbl( client_keys, 0 );

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

	if( flags & PFORBSTAT_DATABASES ) {

		flags |= PFORBSTAT_CLIENTS;
	}

	orbping( orbfd, &orbversion );

	pf = pfnew( PFFILE );
	
	pfput_double( pf, "pforbstat_version", PFORBSTAT_VERSION );

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

		id_clients( pf );
	
		pffree( pfans );
	}

	if( flags & PFORBSTAT_CONNECTIONS ) {

		pfans = orbconnections2pf( pf );

		pfcompile( s = pf2string( pfans ), &pf );
		free( s );
	
		pffree( pfans );
	}

	if( flags & PFORBSTAT_DATABASES ) {

		pfans = orbdatabases2pf( pf );

		pfcompile( s = pf2string( pfans ), &pf );
		free( s );
	
		pffree( pfans );
	}

	return pf;
}
