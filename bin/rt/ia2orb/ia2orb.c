/*
 *   Copyright (c) 2007-2009 Lindquist Consulting, Inc.
 *   All rights reserved. 
 *                                                                     
 *   Written by Dr. Kent Lindquist, Lindquist Consulting, Inc. 
 *
 *   This software is licensed under the New BSD license: 
 *
 *   Redistribution and use in source and binary forms,
 *   with or without modification, are permitted provided
 *   that the following conditions are met:
 *   
 *   * Redistributions of source code must retain the above
 *   copyright notice, this list of conditions and the
 *   following disclaimer.
 *   
 *   * Redistributions in binary form must reproduce the
 *   above copyright notice, this list of conditions and
 *   the following disclaimer in the documentation and/or
 *   other materials provided with the distribution.
 *   
 *   * Neither the name of Lindquist Consulting, Inc. nor
 *   the names of its contributors may be used to endorse
 *   or promote products derived from this software without
 *   specific prior written permission.
 *
 *   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
 *   CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED
 *   WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 *   WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 *   PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL
 *   THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY
 *   DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 *   CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 *   PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 *   USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 *   HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
 *   IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 *   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
 *   USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 *   POSSIBILITY OF SUCH DAMAGE.
 *
 */

#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <math.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <fcntl.h>
#include <pthread.h>
#include <signal.h>
#include <string.h>

#include "stock.h"
#include "coords.h"
#include "db.h"
#include "orb.h"
#include "tr.h"
#include "Pkt.h"
#include "brttpkt.h"
#include "brttutil.h"
#include "swapbytes.h"

#define IA2ORB_EXPECT_ZERO 0
#define IA2ORB_INTERNAL_TIMEOUT 5
#define IA2ORB_RETRIEVE_SUCCEEDED 0
#define IA2ORB_RETRIEVE_FAILED_PERMANENT -1
#define IA2ORB_RETRIEVE_FAILED_TRANSIENT -2
#define PFWATCH_SLEEPTIME_SEC 1
#define IA_BLOCKSIZE_SEC 300
#define IA_REASONABLE_WRITETIME_SEC 30
#define EPSILON_SEC 2
#define MAX_SLEEPTIME_SEC ( (int) ( 1.5 * (double) IA_BLOCKSIZE_SEC ) )
#define EXHUME_SLEEPTIME_SEC 5

typedef struct ia2orb_sta {
	char	sta[STRSZ];
	char	net[STRSZ];
	char	grid[STRSZ];
	char	relay_port[STRSZ];
	char	relay_server_ip[STRSZ];
	char	hashkey[STRSZ];
	char	netsta[STRSZ];
	char	remoteloc[STRSZ];
	int	byteswap_msd_timecorr;
	enum	{OFF, ON_DEMAND, CONTINUOUS} acq_mode;
	pthread_t acq_tid;
	Pmtfifo *mtf;
	Stbl	*acquired;
	double	*latest_acquired;
	double	latest_on_server;
	char	msdfile_target[FILENAME_MAX];
} Ia2orb_sta;

typedef struct Acqfile {
	char	fname[FILENAME_MAX];
} Acqfile;

static Arr *Iarelics;

typedef struct ia2orb_req {
	double	reqtime;
} Ia2orb_req;

typedef struct ia2orb_run {
	int	Stop;
	int	dataorbwritefd;
	int	cmdorbreadfd;
	int	cmdorbwritefd;
	int	cmd_pktid;
	double	cmd_pkttime;
	OrbreapThr *cmd_ort;
	OrbputThr *cmd_opt;
	pthread_mutex_t threadname_mutex;
	pthread_mutex_t runcmd_mutex;
	Arr 	*thread_names;
	Dbptr	db;
	Arr 	*IAs;
} Ia2orb_run;

static Ia2orb_run Iarun;

typedef struct ia2orb_config {
	Pf	*pf;
	char	*pfname;
	char	*statefile;
	char	*orbname;
	char	*cmdorbname;
	char	*target;
	char	ia_user[STRSZ];
	char	download_dir[FILENAME_MAX];
	char	ssh[STRSZ];
	char	scp[STRSZ];
	int	max_rangerequest_sec;
	int	reject_future_data_sec;
	int	reject_old_request_sec;
	int	keep_miniseed;
	int	verbose;
	int	veryverbose;
	Arr	*instrument_network;
} Ia2orb_config;

static Ia2orb_config Cf;

int usage( void );
int byteswap_msd( char *msdfilename );
int runcmd_tbl( Tbl *args, char **result, int free_commands, int expect );
void config_ia2orb( void );
void config_ia2orb_init( void );
void ia2orb_resurrect( Ia2orb_sta *ia );
void clean_acquired_stbl( Ia2orb_sta *ia );
double normalize_time( double time );
int delete_if_old( void *filenamep, void *acqp );
void configure_instrument_network( void );
char *epoch2filename( double epoch );
double filename2epoch( char *filename );
double server_latest( Ia2orb_sta *ia );
int retrieve_block( Ia2orb_sta *ia, double reqtime );
int retrieve_next( Ia2orb_sta *ia );
void launch_acq_thread( Ia2orb_sta *ia );
Ia2orb_req *new_ia2orb_req( double reqtime );
Ia2orb_sta *new_ia2orb_sta( char *sta, char *net, char *grid, char *relay_server_ip, char *relay_port, 
			    char *acq_mode, int byteswap );
void free_ia2orb_sta( Ia2orb_sta **ia );
int update_ia2orb_sta( Ia2orb_sta *ia, char *sta, char *net, char *grid, char *acq_mode, int byteswap );
static int cmp_acqfile( void *ap, void *bp );
int already_acquired( Ia2orb_sta *ia, char *filename );
void register_acquired( Ia2orb_sta *ia, char *filename );
void deactivate_ia( Ia2orb_sta *ia );
void deactivate_ia_fromarray( char *key, void *iap, void *pvt );
void issue_dlcmd_response( Pf *pf, char *response );
void *ia2orb_command( void *arg );
void *ia2orb_pfwatch( void *arg );
void *ia2orb_acquire_continuous( void *iap );
void *ia2orb_acquire_on_demand( void *iap );
char *thread_name( void );
void thread_register( char *name );

int
usage() 
{
	cbanner( "$Date: 2007/03/08 08:00:00 $", 
		 "[-kvV] [-p pfname] [-t target] [-c cmdorbname] orbname",
		 "Dr. Kent Lindquist",
		 "Lindquist Consulting, Inc.",
		 "kent@lindquistconsulting.com" );

	exit( 1 );
} 

char *
thread_name( void )
{
	char	key[STRSZ];
	char	*name;

	sprintf( key, "%lu", (unsigned long) pthread_self() );

	pthread_mutex_lock( &Iarun.threadname_mutex ); 

	if( ( name = (char *) getarr( Iarun.thread_names, key ) ) == (char *) NULL ) {

		name = strdup( "unknown" );

	} else {

		name = strdup( name );
	}

	pthread_mutex_unlock( &Iarun.threadname_mutex ); 

	return name;
}

void
thread_register( char *name ) 
{
	char	key[STRSZ];

	sprintf( key, "%lu", (unsigned long) pthread_self() );

	pthread_mutex_lock( &Iarun.threadname_mutex ); 

	setarr( Iarun.thread_names, key, strdup( name ) );

	pthread_mutex_unlock( &Iarun.threadname_mutex ); 

	return;
}

void
ia2orb_resurrect( Ia2orb_sta *ia )
{
	Relic	*relic;
	int	rc;
	char	*ns;
	char	*s;

	if( Iarelics == (Arr *) NULL ) {
		
		Iarelics = newarr( 0 );
	}

	if( ( relic = (Relic *) getarr( Iarelics, ia->netsta ) ) == (Relic *) NULL ) {

		allot( Relic *, relic, 1 );

		setarr( Iarelics, ia->netsta, (void *) relic );

		allot( double *, relic->dp, 1 );

		ia->latest_acquired = relic->dp;

		*ia->latest_acquired = 0;

		rc = resurrect( ia->netsta, *relic, DOUBLE_RELIC );

		if( ia->acq_mode == CONTINUOUS ) {

			if( rc == 0 && Cf.verbose ) {

				elog_notify( 0, 	
					"[thread '%s']: Resurrected '%s' to acquire next file after '%s UTC'\n",
					ns = thread_name(), ia->netsta, s = strtime( *ia->latest_acquired ) );

				free( ns );
				free( s );
			
			} else if( rc == 1 && Cf.veryverbose ) {

				elog_complain( 0, 	
					"[thread '%s']: Attempted but failed to resurrect acquisition state of '%s' "
					"(continuing, setting to most recent data)\n",
					ns = thread_name(), ia->netsta );

				free( ns );
			}

		} else if( ia->acq_mode == ON_DEMAND ) {

			if( rc == 0 && Cf.verbose ) {

				elog_notify( 0, 	
					"[thread '%s']: Resurrected '%s' to track file acquisitions in case of switch "
					"to continuous mode\n",
					ns = thread_name(), ia->netsta );

				free( ns );

			} else if( rc == 1 && Cf.veryverbose ) {

				elog_notify( 0, 	
					"[thread '%s']: Attempted but failed to resurrect acquisition state of '%s' "
					"(harmless--continuing)\n",
					ns = thread_name(), ia->netsta );

				free( ns );
			}
		}

	} else {

		ia->latest_acquired = relic->dp;

		if( Cf.veryverbose ) {

			elog_notify( 0, 	
				"[thread '%s']: Continuing '%s' in response to resurrection request with previously "
				"exhumed/buried value of '%s UTC'\n",
				ns = thread_name(), ia->netsta, s = strtime( *ia->latest_acquired ) );

			free( ns );
			free( s );
		}	
	}

	return;
}

int
byteswap_msd( char *msdfilename )
{
	char	*buffer;
	char	*record;
	Msd	*msd;
	int	size;
	int	tc;
	unsigned char *from;
	
	msd = msdnew();

	size = wmapfile( msdfilename, 0, (char **) &buffer );

	if( size < 0 ) {

		return -1;
	}

	msdput( msd, MSD_FILENAME, msdfilename,
		     MSD_MSEED, buffer, 
	     	     MSD_MSEED_NBYTES, size, 
		     0 );	     

	while( msdnxt( msd ) ) {
		
		msdget( msd, MSD_TIME_CORRECTION, &tc, 0 );

		from = (unsigned char *) &tc;

		vi2hi( &from, &tc, 1 );

		msdget( msd, MSD_RECORD, &record, 0 );

		memcpy( record + 40, &tc, 4 );
	}

	msdfree( msd );

	unmapfile( msdfilename, 0 );

	return 0;
}

int
runcmd_tbl( Tbl *args, char **result, int free_commands, int expect )
{
	int	rc;
	char	**runargv;
	int	iarg;
	char	*s;
	char	*ns;
	char	*null_string_message = "\t[runcmd() returned null string]";
	char	*empty_string_message = "\t[command returned empty string]";
	char	*string_results;
	char	*result_copy;
	char	*newline_replacement;

	if( args == (Tbl *) NULL || maxtbl( args ) <= 0 ) {

		elog_complain( 0, 
			"[thread '%s']: runcmd_tbl: ignoring empty command\n",
			ns = thread_name() );

		free( ns );

		if( free_commands ) {
			
			freetbl( args, 0 );
		}

		return 1;
	}

	allot( char **, runargv, maxtbl( args ) + 1 );

	for( iarg = 0; iarg < maxtbl( args ); iarg++ ) {
		
		runargv[iarg] = gettbl( args, iarg );
	}

	runargv[iarg] = 0;

	if( Cf.verbose ) {

		elog_notify( 0, "[thread '%s']: Executing '%s'\n",
				ns = thread_name(), 
				s = jointbl( args, " " ) );

		free( s );
		free( ns );
	}

	rc = runcmd( runargv, result ); 
 
	if( Cf.veryverbose ) {

		ns = thread_name();

		allot( char *, newline_replacement, STRSZ );

		sprintf( newline_replacement, "\nia2orb: [thread '%s']: ", ns );

		if( *result == (char *) NULL ) {

			string_results = strdup( null_string_message );

		} else if( ! strcmp( *result, "" ) ) {

			string_results = strdup( empty_string_message );

		} else {

			allot( char *, string_results, 10 * STRSZ );

			result_copy = strdup( *result );

			chomp( result_copy );

			strsub( result_copy, "\n", newline_replacement, string_results );

			free( result_copy );
		}

		elog_notify( 1, 
			"[thread '%s']: Command '%s' results: [exit code %d]:%s%s\n\n",
			ns, runargv[0], rc, newline_replacement, string_results );

		free( ns );

		free( newline_replacement );

		free( string_results );

	} else if( rc != expect ) {

		ns = thread_name();

		allot( char *, newline_replacement, STRSZ );

		sprintf( newline_replacement, "\nia2orb: [thread '%s']: ", ns );

		if( *result == (char *) NULL ) {

			string_results = strdup( null_string_message );

		} else if( ! strcmp( *result, "" ) ) {

			string_results = strdup( empty_string_message );

		} else {

			allot( char *, string_results, 10 * STRSZ );

			result_copy = strdup( *result );

			chomp( result_copy );

			strsub( result_copy, "\n", newline_replacement, string_results );

			free( result_copy );
		}

		elog_complain( 1, 
			"[thread '%s']: Command '%s' results (exit code %d):%s%s\n\n",
			ns, runargv[0], rc, newline_replacement, string_results );

		free( ns );

		free( newline_replacement );

		free( string_results );
	}

	free( runargv );

	if( free_commands ) {

		freetbl( args, 0 );
	}
	
	return rc;
}

void
config_ia2orb()
{
	char	*ia_user;
	char	*download_dir;
	char	*ssh;
	char	*scp;
	char	*ns;

	Cf.reject_future_data_sec = pfget_int( Cf.pf, "reject_future_data_sec" );

	Cf.reject_old_request_sec = pfget_int( Cf.pf, "reject_old_request_sec" );

	Cf.max_rangerequest_sec = pfget_int( Cf.pf, "max_rangerequest_sec" );

	ia_user 	= pfget_string( Cf.pf, "ia_user" );
	download_dir 	= pfget_string( Cf.pf, "download_dir" );
	ssh 		= pfget_string( Cf.pf, "ssh" );
	scp 		= pfget_string( Cf.pf, "scp" );

	if( ia_user == NULL ) {

		elog_die( 0, 
			"[thread '%s']: Couldn't find ia_user in parameter file; Bye.\n", 
			ns = thread_name() );

		free( ns );
	}

	if( download_dir == NULL ) {

		elog_die( 0, 
			"[thread '%s']: Couldn't find download_dir in parameter file; Bye.\n", 
			ns = thread_name() );

		free( ns );

	} else if( ! is_dir( download_dir ) ) {

		elog_die( 0, 
			"[thread '%s']: The 'download_dir' specified in parameter-file '$s' does not appear "
			"to be a directory. Bye.\n", 
			ns = thread_name() , Cf.pfname );

		free( ns );
	}

	if( ssh == NULL ) {

		elog_die( 0, 
			"[thread '%s']: Couldn't find ssh in parameter file; Bye.\n", 
			ns = thread_name() );

		free( ns );
	}

	if( scp == NULL ) {

		elog_die( 0, 
			"[thread '%s']: Couldn't find scp in parameter file; Bye.\n", 
			ns = thread_name() );

		free( ns );
	}

	strcpy( Cf.ia_user, ia_user );
	strcpy( Cf.download_dir, download_dir );
	strcpy( Cf.ssh, ssh );
	strcpy( Cf.scp, scp );

	return;
}

void 
launch_acq_thread( Ia2orb_sta *ia )
{
	char	byteswap_msg[STRSZ];
	char	*ns;
	int	rc;

	if( ia->byteswap_msd_timecorr ) {

		sprintf( byteswap_msg, " (byteswapping time correction)" );

	} else {

		strcpy( byteswap_msg, "" );
	}

	switch( ia->acq_mode ) {

	case CONTINUOUS:

		rc = pthread_create( &ia->acq_tid, NULL, ia2orb_acquire_continuous, (void *) ia );

		if( Cf.veryverbose ) {
				
			elog_notify( 0, 
				"[thread '%s']: Launching thread %lu to acquire station '%s' continuously%s\n",
				ns = thread_name(), ia->acq_tid, ia->sta, byteswap_msg );

			free( ns );
		}

		break;

	case ON_DEMAND:

		rc = pthread_create( &ia->acq_tid, NULL, ia2orb_acquire_on_demand, (void *) ia );

		if( Cf.veryverbose ) {
				
			elog_notify( 0, 
				"[thread '%s']: Launching thread %lu to acquire station '%s' on-demand%s\n",
				ns = thread_name(), ia->acq_tid, ia->sta, byteswap_msg );

			free( ns );
		}

		break;

	default:

		if( Cf.veryverbose ) {
				
			elog_notify( 0, 
				"[thread '%s']: Not acquiring %s:%s\n",
				ns = thread_name(), ia->net, ia->sta );

			free( ns );
		}
		
		break;
	}

	return;
}

void
configure_instrument_network( void )
{
	Ia2orb_sta *ia;
	Tbl	*newkeys;
	Tbl	*existingkeys;
	Arr	*seen;
	int	ikey;
	int	rc;
	char	acq_mode[STRSZ];
	char	net[STRSZ];
	char	grid[STRSZ];
	char	relay_server_ip[STRSZ];
	char	relay_port[STRSZ];
	char	tf[STRSZ];
	char	hashkey[STRSZ];
	char	*key;
	char	*line;
	char	*sta;
	char	*ns;

	newkeys = keysarr( Cf.instrument_network );

	if( maxtbl( newkeys ) <= 0 && cntarr( Iarun.IAs ) <= 0 ) {
		
		elog_die( 0, 
			"[thread '%s']: No stations configured in parameter-file '%s' "
			"(parameter 'ia_network'). Bye.\n",
			ns = thread_name(), Cf.pfname );
		
		free( ns );
	}

	if( Cf.veryverbose ) {
		
		elog_notify( 0, 	
			"[thread '%s']: There are %d stations in the ia_network\n",
			ns = thread_name(), maxtbl( newkeys ) ); 

		free( ns );
	}

	seen = newarr( 0 );

	for( ikey = 0; ikey < maxtbl( newkeys ); ikey++ ) {

		sta = gettbl( newkeys, ikey );

		line = (char *) getarr( Cf.instrument_network, sta );

		strcpy( acq_mode, "" );

		rc = sscanf( line, "%s %s %s %s %s %s", 
					&net[0], 
					&grid[0], 
					&relay_server_ip[0], 
					&relay_port[0], 
					&tf[0],
					&acq_mode[0] );

		if( rc < 6 ) {

			elog_complain( 0, "[thread '%s']: Failure parsing ia_network array row for '%s'. "
				          "Expecting 6 arguments after station name; got only %d arguments "
				          "(which were '%s'). Skipping.\n",
				          ns = thread_name(), sta, rc, line );

			free( ns );
			
			continue;

		} else if( strcmp( acq_mode, "continuous" ) &&
			   strcmp( acq_mode, "on-demand" ) &&
			   strcmp( acq_mode, "off" ) ) {

			elog_complain( 0, "[thread '%s']: Acquisition mode '%s' for station '%s' not understood; "
			          	"must be 'off', 'on-demand' or 'continuous'. Skipping.\n",
					ns = thread_name(), acq_mode, sta );

			free( ns );

			continue;

		}

		sprintf( hashkey, "%s:%s", relay_server_ip, relay_port );

		if( getarr( seen, hashkey ) != (void *) NULL ) {

			ia = getarr( Iarun.IAs, hashkey );

			elog_complain( 0, "[thread '%s']: Ignoring ia_network array row for '%s' because "
					  "relay_server_ip:port combination '%s' is repeated from station '%s' "
					  "already configured from the same array\n",
					  ns = thread_name(), sta, hashkey, ia->sta );

			free( ns );

			continue;

		} else {

			setarr( seen, hashkey, (void *) 0x1 );
		}

		if( (ia = (Ia2orb_sta *) getarr( Iarun.IAs, hashkey ) ) != (Ia2orb_sta *) NULL ) {

			rc = update_ia2orb_sta( ia, sta, net, grid, acq_mode, yesno( tf ) );

			if( rc != 0 ) {

				launch_acq_thread( ia );
			}

		} else {

			ia = new_ia2orb_sta( sta, net, grid, relay_server_ip, relay_port, acq_mode, yesno( tf ) );

			launch_acq_thread( ia );

			setarr( Iarun.IAs, ia->hashkey, ia );
		}
	}

	freetbl( newkeys, 0 );
		
	existingkeys = keysarr( Iarun.IAs );

	for( ikey = 0; ikey < maxtbl( existingkeys ); ikey++ ) {

		key = gettbl( existingkeys, ikey );

		if( getarr( seen, key ) == NULL ) {

			ia = delarr( Iarun.IAs, key );

			if( Cf.veryverbose ) {

				elog_notify( 0, "[thread '%s']: Station '%s' via relay connection '%s' "
					  "disappeared from ia_network array -- removing acquisition threads "
					  "and tracking\n",
					  ns = thread_name(), ia->sta, hashkey );

				free( ns );
			}

			deactivate_ia( ia );

			free_ia2orb_sta( &ia );
		}
	}

	freetbl( existingkeys, 0 );

	freearr( seen, 0 );

	return;
}

void
config_ia2orb_init()
{
	Cf.pfname = "ia2orb";
	Cf.target = "ia2orb";
	Cf.statefile = NULL;
	Cf.reject_future_data_sec = 0;
	Cf.reject_old_request_sec = 0;
	Cf.max_rangerequest_sec = 3600;
	Cf.keep_miniseed = 0;
	Cf.verbose = 0;
	Cf.veryverbose = 0;
	Cf.cmdorbname = NULL;
	Cf.instrument_network = NULL;
	strcpy( Cf.ia_user, "" );
	strcpy( Cf.download_dir, "" );
	strcpy( Cf.ssh, "" );
	strcpy( Cf.scp, "" );

	return;
}

int
delete_if_old( void *afp, void *acqp )
{
	Stbl	*acquired = (Stbl *) acqp;
	Acqfile	*af = (Acqfile *) afp;
	void	*element;

	if( filename2epoch( af->fname ) < std_now() - Cf.reject_old_request_sec ) {

		element = delstbl( acquired, (void *) af );

		free( element );

		return 1;

	} else {

		return 0;
	}
}

void
clean_acquired_stbl( Ia2orb_sta *ia )
{
	int	deleted = 0;
	int	max_entries;
	char	*ns;

	max_entries = (int) ( 2 * ( (double) Cf.reject_old_request_sec / (double) IA_BLOCKSIZE_SEC ) );

	if( maxstbl( ia->acquired ) < max_entries ) { 

		return;
	}

	deleted += applystbl( ia->acquired, delete_if_old, (void *) ia->acquired );

	if( Cf.veryverbose ) {

		elog_notify( 0, 	
			"[thread '%s']: Deleted %d entries from table of files acquired from '%s'\n",
			ns = thread_name(), deleted, ia->netsta );

		free( ns );
	}

	return;
}

int
retrieve_block( Ia2orb_sta *ia, double reqtime )
{
	char 	future_rejection_onset[STRSZ];
	char	msdfile_source[FILENAME_MAX];
	char	*result = 0;
	char	*source;
	char	*ds;
	char	*ns;
	char	*s;
	Tbl	*cmdargs;
	int	rc;

	if( Cf.reject_old_request_sec > 0 && 
	    reqtime < std_now() - Cf.reject_old_request_sec ) {

		elog_notify( 0, 	
			"[thread '%s']: Ignoring data request because start-time '%s UTC' preceeds "
			"current system time by more than '%s'\n",
			ns = thread_name(), s = strtime( reqtime ), 
			ds = strtdelta( Cf.reject_old_request_sec ) );

		free( s );
		free( ds );
		free( ns );

		return IA2ORB_RETRIEVE_FAILED_PERMANENT;
	}

	s = epoch2filename( reqtime );

	strcpy( msdfile_source, s );

	free( s );

	if( already_acquired( ia, msdfile_source ) ) {

		if( Cf.veryverbose ) {

			elog_notify( 0, 	
				"[thread '%s']: Ignoring request to retrieve already-acquired file '%s'\n",
				ns = thread_name(), msdfile_source );

			free( ns );
		}

		return IA2ORB_RETRIEVE_FAILED_PERMANENT;
	}

	sprintf( ia->msdfile_target, "%s/%s_%s",
					Cf.download_dir, 
					ns = thread_name(),
				        msdfile_source );
	
	free( ns );

	if( Cf.veryverbose ) {

		elog_notify( 0, "[thread '%s']: Acquiring file %s...\n", 
			ns = thread_name(),
			msdfile_source );

		free( ns );
	}

	source = strconcat( ia->remoteloc, ":", msdfile_source, 0 );

	cmdargs = strtbl( Cf.scp,
			  "-P", 
			  ia->relay_port, 
			  source,
			  ia->msdfile_target,
			  0 );

	rc = runcmd_tbl( cmdargs, &result, 0, IA2ORB_EXPECT_ZERO ); 
	
	freetbl( cmdargs, 0 );

	free( source );

	free( result );

	if( rc != 0 ) {

		elog_complain( 0, "[thread '%s']: Failed to acquire %s\n", 
				ns = thread_name(),
				msdfile_source );

		free( ns );

		return IA2ORB_RETRIEVE_FAILED_TRANSIENT;
	}

	if( ia->byteswap_msd_timecorr ) {

		rc = byteswap_msd( ia->msdfile_target );

		if( rc != 0 ) {

			elog_complain( 1, 
				"[thread '%s']: Failed to open miniseed file '%s' "
				"to byte swap the Tc value\n", 
				ns = thread_name(),
				ia->msdfile_target );

			free( ns );

			return IA2ORB_RETRIEVE_FAILED_PERMANENT;
		}
	}

	cmdargs = strtbl( "miniseed2orb", 
			  "-u", 
			  0 );

	if( Cf.verbose ) {
		
		pushtbl( cmdargs, "-v" );
	}

	if( Cf.reject_future_data_sec >= 0 ) {

		sprintf( future_rejection_onset, "%f", 
			 std_now() + Cf.reject_future_data_sec );

		pushtbl( cmdargs, "-e" );
		pushtbl( cmdargs, future_rejection_onset );
	}

	pushtbl( cmdargs, ia->msdfile_target );
	pushtbl( cmdargs, Cf.orbname );

	rc = runcmd_tbl( cmdargs, &result, 0, IA2ORB_EXPECT_ZERO );

	freetbl( cmdargs, 0 );

	free( result );

	if( ! Cf.keep_miniseed ) {

		unlink( ia->msdfile_target );
	}

	if( rc != 0 ) {

		elog_complain( 0, "[thread '%s']: Failed to upload %s to orbserver\n",
			ns = thread_name(),
			ia->msdfile_target );

		free( ns );

		return IA2ORB_RETRIEVE_FAILED_PERMANENT;
	}

	*ia->latest_acquired = filename2epoch( msdfile_source );

	register_acquired( ia, msdfile_source );

	if( Cf.statefile != (char *) NULL ) {

		bury();
	}

	return IA2ORB_RETRIEVE_SUCCEEDED;
}

int
already_acquired( Ia2orb_sta *ia, char *filename )
{
	int	acquired = 0;

	if( tststbl( ia->acquired, (void *) filename ) != (void *) NULL ) {

		acquired = 1;

	} else {

		acquired = 0;
	}

	return acquired;
}

void
register_acquired( Ia2orb_sta *ia, char *filename )
{
	Acqfile	*af;
	Acqfile	*afp;

	allot( Acqfile *, af, 1 );

	strcpy( af->fname, filename );

	if( ( afp = (Acqfile *) addstbl( ia->acquired, (void *) af ) ) != af ) {

		/* Duplicate entry */

		free( af );
	}

	clean_acquired_stbl( ia );

	return;
}

double
server_latest( Ia2orb_sta *ia )
{
	char	latest_on_server[FILENAME_MAX];
	double	latest_epoch;
	Tbl	*cmdargs;
	Tbl	*files = 0;
	char	*result = 0;
	int	rc = 0;
	char	*ns;

	cmdargs = strtbl( Cf.ssh,
			  "-p",
			  ia->relay_port,
			  ia->remoteloc,
			  "ls *MSD | tail -2",
			  0 );

	rc = runcmd_tbl( cmdargs, &result, 0, IA2ORB_EXPECT_ZERO ); 

	freetbl( cmdargs, 0 );

	if( rc != 0 ) {
		
		elog_complain( 0, 	
			"[thread '%s']: Failed to list contents of %s:%s via ssh\n",
			ns = thread_name(), ia->remoteloc, ia->relay_port );

		free( ns );

		if( result != NULL ) {
			
			free( result );
		}

		return -1;
	}

	files = split( result, '\n' );

	strcpy( latest_on_server, gettbl( files, maxtbl( files ) - 1 ) );

	free( result );

	freetbl( files, 0 ); 

	latest_epoch = filename2epoch( latest_on_server ); 

	return latest_epoch;
}

int
retrieve_next( Ia2orb_sta *ia )
{
	double	candidate;
	double	old_candidate;
	int	estimated_sleep_sec;
	char	*ns;
	char	*ds;
	char	*s;
	char	*t;

	if( ia->latest_on_server == 0 ||
	    ia->latest_on_server <= *ia->latest_acquired ) {

		ia->latest_on_server = server_latest( ia );

		if( ia->latest_on_server < 0 ) {

			return -1;
		}
	}

	if( *ia->latest_acquired <= 0 ) {

		candidate = ia->latest_on_server;

	} else {

		candidate = *ia->latest_acquired + IA_BLOCKSIZE_SEC;
	}

	if( candidate > ia->latest_on_server ) {
	
		if( Cf.veryverbose ) {

			elog_notify( 0, 
				"[thread '%s']: No new files on %s:%s\n",
				ns = thread_name(),
				ia->remoteloc, 
				ia->relay_port );

			free( ns );
		}

		estimated_sleep_sec = (int) ( candidate + IA_BLOCKSIZE_SEC - std_now() + EPSILON_SEC );

		if( estimated_sleep_sec < 0 ) {

			estimated_sleep_sec = 0;
		}

	} else {

		while( retrieve_block( ia, candidate ) < 0 &&
		       candidate + IA_BLOCKSIZE_SEC < std_now() ) {
			
			candidate += IA_BLOCKSIZE_SEC;

			if( Cf.reject_old_request_sec > 0 && 
			    candidate < std_now() - Cf.reject_old_request_sec ) {

				old_candidate = candidate;

				candidate = normalize_time( std_now() - Cf.reject_old_request_sec + IA_BLOCKSIZE_SEC ); 

				if( Cf.verbose ) {

					elog_notify( 0, 	
						"[thread '%s']: Advancing acquisition point to '%s UTC' because previous "
						"candidate start-time '%s UTC' preceeds current system time by more than '%s'\n",
						ns = thread_name(), 
						s = strtime( candidate ),
						t = strtime( old_candidate ), 
						ds = strtdelta( Cf.reject_old_request_sec ) );

					free( s );
					free( t );
					free( ds );
					free( ns );
				}
			}
		}

		estimated_sleep_sec = (int) ( candidate + 2 * IA_BLOCKSIZE_SEC - std_now() + EPSILON_SEC );

		if( estimated_sleep_sec < 0 ) {

			estimated_sleep_sec = 0;
		}
	}

	if( estimated_sleep_sec > MAX_SLEEPTIME_SEC ) {

		estimated_sleep_sec = MAX_SLEEPTIME_SEC;
	}

	return estimated_sleep_sec;
}

double
filename2epoch( char *filename )
{
	char	new[FILENAME_MAX];
	regex_t *pattern_buffer = 0;

	allot( regex_t *, pattern_buffer, 1 );

	regcomp( pattern_buffer, 
		 "([0-9][0-9][0-9][0-9])([0-9][0-9])([0-9][0-9]).([0-9][0-9])([0-9][0-9])([0-9][0-9]).MSD", 
		 REG_EXTENDED );

	patsub( filename, pattern_buffer, "$1-$2-$3 $4:$5:$6", &new[0] );

	regfree( pattern_buffer );

	free( pattern_buffer );

	return str2epoch( new );
}

double
normalize_time( double time )
{
	double	start;

	/* Normalize time to presumed start of a datalogger data file,
	   on the assumption of regular block sizes on the minute mark 
	   (works for the 5 min blocks at the time of writing): */

	start = floor( time ) - ( ( (int) floor( time ) ) % IA_BLOCKSIZE_SEC );

	return start;
}

char *
epoch2filename( double reqtime ) 
{
	double	start;
	char	*s;

	start = normalize_time( reqtime );

	s = epoch2str( start, "%Y%m%d.%H%M%S.MSD" );

	return s;
}

void
deactivate_ia_fromarray( char *key, void *iap, void *pvt )
{
	Ia2orb_sta *ia = (Ia2orb_sta *) iap;

	deactivate_ia( ia );

	return;
}

void
deactivate_ia( Ia2orb_sta *ia ) 
{
	char	*threadrc;
	char	*ns;

	if( ia->acq_tid != (pthread_t) NULL ) {

		if( Cf.veryverbose ) {

			elog_notify( 0, 
				"[thread '%s']: Cancelling acquisition thread for station '%s'...\n",
				ns = thread_name(), ia->sta );

			free( ns );
		}

		pthread_cancel( ia->acq_tid );

		pthread_join( ia->acq_tid, (void **) &threadrc );

		if( Cf.veryverbose ) {

			elog_notify( 0, 
				"[thread '%s']: ...acquisition thread for station '%s' successfully cancelled\n",
				ns = thread_name(), ia->sta );
			
			free( ns );
		}
	}

	if( ia->mtf != (Pmtfifo *) NULL ) {

		pmtfifo_destroy( ia->mtf, free );
	}

	return;
}

int 
update_ia2orb_sta( Ia2orb_sta *ia, char *sta, char *net, char *grid, char *acq_mode, int byteswap )
{
	int	new_acq_mode;

	if( ! strcmp( acq_mode, "continuous" ) ) {

		new_acq_mode = CONTINUOUS;

	} else if( ! strcmp( acq_mode, "on-demand" ) ) {

		new_acq_mode = ON_DEMAND;

	} else {

		new_acq_mode = OFF;
	}

	if( ! strcmp( ia->sta, sta ) && 
	    ! strcmp( ia->net, net ) &&
	    ! strcmp( ia->grid, grid ) &&
	    ia->acq_mode == new_acq_mode && 
	    ia->byteswap_msd_timecorr == byteswap ) {

		return 0;
	}

	ia->acq_mode = new_acq_mode;

	deactivate_ia( ia );

	ia->acq_tid = (pthread_t) NULL;
	ia->mtf = (Pmtfifo *) NULL;

	strcpy( ia->sta, sta );
	strcpy( ia->net, net );
	strcpy( ia->grid, grid );

	sprintf( ia->netsta, "%s_%s", ia->net, ia->sta );

	ia->byteswap_msd_timecorr = byteswap;

	strcpy( ia->msdfile_target, "" );

	return 1;
}

void
free_ia2orb_sta( Ia2orb_sta **ia )
{
	if( (*ia)->acquired != (Stbl *) NULL ) {

		freestbl( (*ia)->acquired, free );
	}

	if( Cf.statefile == (char *) NULL && (*ia)->latest_acquired != (double *) NULL ) {

		free( (*ia)->latest_acquired );
	}

	free( *ia );

	*ia = (Ia2orb_sta *) NULL;
}

static int
cmp_acqfile( void *ap, void *bp ) 
{
	Acqfile *a = (Acqfile *) ap;
	Acqfile *b = (Acqfile *) bp;

	return strcmp( a->fname, b->fname );
}

Ia2orb_sta *
new_ia2orb_sta( char *sta, char *net, char *grid, char *relay_server_ip, char *relay_port, 
	        char *acq_mode, int byteswap )
{
	Ia2orb_sta *ia;

	allot( Ia2orb_sta *, ia, 1 );

	strcpy( ia->sta, sta );
	strcpy( ia->net, net );
	strcpy( ia->grid, grid );
	strcpy( ia->relay_server_ip, relay_server_ip );
	strcpy( ia->relay_port, relay_port );

	ia->byteswap_msd_timecorr = byteswap;

	sprintf( ia->remoteloc, "%s@%s", Cf.ia_user, ia->relay_server_ip );
	
	sprintf( ia->hashkey, "%s:%s", ia->relay_server_ip, ia->relay_port );

	sprintf( ia->netsta, "%s_%s", ia->net, ia->sta );

	if( ! strcmp( acq_mode, "continuous" ) ) {

		ia->acq_mode = CONTINUOUS;

	} else if( ! strcmp( acq_mode, "on-demand" ) ) {

		ia->acq_mode = ON_DEMAND;

	} else {

		ia->acq_mode = OFF;
	}

	ia->acq_tid = (pthread_t) NULL;
	ia->mtf = (Pmtfifo *) NULL;
	ia->latest_acquired = (double *) NULL;

	ia->latest_on_server = 0;

	ia->acquired = newstbl( cmp_acqfile );

	strcpy( ia->msdfile_target, "" );

	return ia;
}

Ia2orb_req *
new_ia2orb_req( double reqtime )
{
	Ia2orb_req *iar;

	allot( Ia2orb_req *, iar, 1 );

	iar->reqtime = reqtime;

	return iar;
}

void
issue_dlcmd_response( Pf *pf, char *response )
{
	Pf	*pfresp = 0;
	char	srcname[ORBSRCNAME_SIZE];
	char	*s;
	double	time;
	int	nbytes = 0;
	int 	packetsz = 0;
	char	*packet = (char *) NULL;
	Packet	*pkt = (Packet *) NULL;


	pfcompile( s = pf2string( pf ), &pfresp );

	free( s );

	pfput_string( pfresp, "response", response );

	pkt = newPkt();

	pkt->pkttype = suffix2pkttype( "pf" );

	strcpy( pkt->parts.src_suffix, "pf" );
	strcpy( pkt->parts.src_subcode, "dlcmr" );

	pkt->pf = pfresp;

	pkt->time = std_now();

	stuffPkt( pkt, srcname, &time, &packet, &nbytes, &packetsz );

	freePkt( pkt );

	orbputthr_put( Iarun.cmd_opt, srcname, time, packet, nbytes );

	free( packet );

	return;
}

void *
ia2orb_pfwatch( void *arg )
{
	int	true = 1;
	int	first = 1;
	int	rc;
	char	*ns;

	thread_register( "pfwatch" );

	while( true ) {

		rc = pfupdate( Cf.pfname, &Cf.pf );

		if( first ) {

			first = 0;

			config_ia2orb();

			if( Cf.veryverbose ) {

				elog_notify( 0, 	
					"[thread '%s']: Configuring acquisition threads\n",
					ns = thread_name() );

				free( ns );
			} 

			Cf.instrument_network = pfget_arr( Cf.pf, "ia_network" );

			configure_instrument_network();

		} else if( rc < 0 ) {

			elog_complain( 0, 	
				"[thread '%s']: pfupdate of parameter file '%s' failed\n",
				ns = thread_name(), Cf.pfname );

			free( ns );

		} else if( rc == 0 ) {

			; /* No reconfiguration necessary */

		} else if( rc == 1 ) {

			elog_notify( 0, 	
				"[thread '%s']: Re-configuring acquisition threads based on change in "
				"parameter file\n",
				ns = thread_name() );

			free( ns );

			freearr( Cf.instrument_network, 0 );

			Cf.instrument_network = pfget_arr( Cf.pf, "ia_network" );

			configure_instrument_network();

		} else {

			elog_complain( 0, 	
				"[thread '%s']: unexpected result %d from pfupdate\n",
				ns = thread_name(), rc );

			free( ns );
		}

		sleep( PFWATCH_SLEEPTIME_SEC );
	}

	return (void *) NULL;
}

void *
ia2orb_command( void *arg ) 
{
	char	srcname[ORBSRCNAME_SIZE];
	char	response[STRSZ];
	char	*packet = 0;
	int	nbytes = 0;
	int	bufsize = 0;
	int	rc;
	Packet	*pkt = 0;
	char	*command;
	double	reqtime;
	double	reqstarttime;
	double	reqendtime;
	double	reqtime_norm;
	double	reqendtime_norm;
	double	pkttime;
	int	pktid;
	char	*s;
	char	*t;
	char	*d;
	char	*dp;
	char	*ns;
	Tbl	*keys;
	Ia2orb_sta *ia;
	Ia2orb_req *iar = NULL;
	int	ikey;

	thread_register( "command" );

	while( ! Iarun.Stop ) {

		rc = orbreapthr_get( Iarun.cmd_ort, &pktid, srcname,
		   		     &pkttime, &packet, &nbytes, &bufsize );

		if( rc != ORBREAPTHR_OK ) {

			continue;
		}

		rc = unstuffPkt( srcname, pkttime, packet, nbytes, &pkt );

		if( rc != Pkt_pf ) {

			freePkt( pkt );

			pkt = (Packet *) NULL;

			free( packet );

			packet = (char *) NULL;

			nbytes = bufsize = 0;

			continue;
		}

		Iarun.cmd_pkttime = pkttime;
		Iarun.cmd_pktid = pktid;

		if( Cf.veryverbose ) {
			
			elog_notify( 0, 	
				"[thread '%s']: Received command packet\n%s\n",
				ns = thread_name(), s = pf2string( pkt->pf ) );

			free( s );
			free( ns );
		}

		command = pfget_string( pkt->pf, "command" );

		if( command == 0 || ! strcmp( command, "" ) ) {
			
			sprintf( response, "[ia2orb thread '%s']: Empty or non-existent"
				"command in parameter-file packet %s; "
				"skipping",
				ns = thread_name(), srcname );

			free( ns );

			elog_complain( 0, "%s\n", response );

		} else if( ! strcmp( command, "acqnet" ) ) {

			reqtime = pfget_time( pkt->pf, "time" );

			keys = keysarr( Iarun.IAs );

			for( ikey = 0; ikey < maxtbl( keys ); ikey++ ) {

				ia = getarr( Iarun.IAs, gettbl( keys, ikey ) );

				if( ia->acq_mode == ON_DEMAND ) {

					iar = new_ia2orb_req( reqtime );

					pmtfifo_push( ia->mtf, (void *) iar );
				}
			}

			freetbl( keys, 0 );

			sprintf( response, 
				"[ia2orb thread '%s']: Issued acquisition-request to all on-demand stations for "
				"window overlapping '%s UTC'\n",
				ns = thread_name(),
				s = strtime( reqtime ) );

			free( ns );
			free( s );

			if( Cf.verbose ) {

				elog_notify( 0, response );
			}

		} else if( ! strcmp( command, "acqrange" ) ) {

			reqstarttime = pfget_time( pkt->pf, "start" );
			reqendtime = pfget_time( pkt->pf, "end" );

			if( reqendtime - reqstarttime > Cf.max_rangerequest_sec ) {

				d = strtdelta( Cf.max_rangerequest_sec );
				dp = d;
				strtrim( dp );

				sprintf( response, 
					"[ia2orb thread '%s']: Ignoring acquisition-request to all "
					"on-demand stations for window overlapping range "
					"'%s UTC' to '%s UTC': Time-span exceeds maximum of %s configured "
					"in '%s' parameter-file ('max_rangerequest_sec' parameter)\n",
					ns = thread_name(),
					s = strtime( reqstarttime ),
					t = strtime( reqendtime ),
					dp,
					Cf.pfname );

				free( ns );
				free( s );
				free( t );
				free( d );

				elog_complain( 0, response );

			} else {

				reqtime = reqstarttime;

				keys = keysarr( Iarun.IAs );

				reqtime_norm = normalize_time( reqtime );
				reqendtime_norm = normalize_time( reqendtime );

				while( reqtime_norm < reqendtime_norm + IA_BLOCKSIZE_SEC ) {

					for( ikey = 0; ikey < maxtbl( keys ); ikey++ ) {

						ia = getarr( Iarun.IAs, gettbl( keys, ikey ) );

						if( ia->acq_mode == ON_DEMAND ) {

							iar = new_ia2orb_req( reqtime_norm );

							pmtfifo_push( ia->mtf, (void *) iar );
						}
					}

					reqtime_norm += IA_BLOCKSIZE_SEC;
				}

				freetbl( keys, 0 );

				sprintf( response, 
					"[ia2orb thread '%s']: Issued acquisition-request to all "
					"on-demand stations for window overlapping range "
					"'%s UTC' to '%s UTC'\n",
					ns = thread_name(),
					s = strtime( reqstarttime ),
					t = strtime( reqendtime ) );

				free( ns );
				free( s );
				free( t );

				if( Cf.verbose ) {

					elog_notify( 0, response );
				}
			}

		} else {

			sprintf( response, "[ia2orb thread '%s']: command '%s' "
				"not understood, skipping",
				ns = thread_name(), command );

			free( ns );

			elog_complain( 0, "%s\n", response );
		}

		issue_dlcmd_response( pkt->pf, response );

		freePkt( pkt );

		pkt = (Packet *) NULL;

		free( packet );

		packet = (char *) NULL;

		nbytes = bufsize = 0;

		if( Cf.statefile != (char *) NULL ) {

			bury();
		}
	}

	applyarr( Iarun.IAs, deactivate_ia_fromarray, 0 );

	return (void *) NULL;
}

void *
ia2orb_acquire_continuous( void *iap ) 
{
	Ia2orb_sta *ia = (Ia2orb_sta *) iap;
	int	estimated_sleeptime_sec;
	int	old;
	char	threadname[STRSZ];
	char	*ns;

	sprintf( threadname, "acq_cont_%s", ia->netsta );

	thread_register( threadname );

	if( Cf.statefile != (char *) NULL ) {
		
		ia2orb_resurrect( ia );

	} else {

		allot( double *, ia->latest_acquired, 1 );

		*ia->latest_acquired = 0;
	}

	while( ! Iarun.Stop && ia->acq_mode == CONTINUOUS ) {
		
		pthread_testcancel();

		pthread_setcancelstate( PTHREAD_CANCEL_DISABLE, &old );

		estimated_sleeptime_sec = retrieve_next( ia );

		pthread_setcancelstate( old, &old );

		if( estimated_sleeptime_sec > 0 ) {

			if( Cf.veryverbose ) {
			
				elog_notify( 0, 	
					"[thread '%s']: Sleeping %d seconds\n\n",
					ns = thread_name(), estimated_sleeptime_sec );

				free( ns );
			}

			wait_for_time( std_now() + estimated_sleeptime_sec, 0 );

		} else if( estimated_sleeptime_sec < 0 ) {

			elog_notify( 0, 	
				"[thread '%s']: Shutting down continuous-acquisition thread "
				"for '%s' due to unrecoverable problems\n\n",
				ns = thread_name(), ia->netsta );
				
			free( ns );

			ia->acq_mode = OFF;
		}
	}

	return (void *) NULL;
}

void *
ia2orb_acquire_on_demand( void *iap ) 
{
	Ia2orb_sta *ia = (Ia2orb_sta *) iap;
	Ia2orb_req *iar = NULL;
	char	threadname[STRSZ];
	char	*ns;
	char	*s;
	int	estimated_sleeptime_sec;
	int	old;
	int	rc;

	ia->mtf = pmtfifo_create( 0, 1, 0 );

	pmtfifo_set_timeout( ia->mtf, IA2ORB_INTERNAL_TIMEOUT );

	sprintf( threadname, "acq_ondmd_%s", ia->netsta );

	thread_register( threadname );

	if( Cf.statefile != (char *) NULL ) {
		
		ia2orb_resurrect( ia );

	} else {

		allot( double *, ia->latest_acquired, 1 );

		*ia->latest_acquired = 0;
	}

	while( ! Iarun.Stop ) {

		pthread_testcancel();
	
		rc = pmtfifo_pop( ia->mtf, (void **) &iar );

		if( rc != PMTFIFO_OK ) {

			continue;
		}

		pthread_setcancelstate( PTHREAD_CANCEL_DISABLE, &old );

		rc = retrieve_block( ia, iar->reqtime );

		if( rc == IA2ORB_RETRIEVE_FAILED_TRANSIENT &&
		    iar->reqtime > std_now() - IA_BLOCKSIZE_SEC - IA_REASONABLE_WRITETIME_SEC ) {

			estimated_sleeptime_sec = 
			    (int) rint( iar->reqtime + IA_BLOCKSIZE_SEC + IA_REASONABLE_WRITETIME_SEC - std_now() );

			if( Cf.veryverbose ) {

				elog_notify( 0, 	
					"[thread '%s']: Sleeping %d seconds and re-posting on-demand data request "
					"because start-time '%s UTC' is close to or less than one IA block-size "
					"(%d sec) before system-clock time\n",
					ns = thread_name(), estimated_sleeptime_sec,
					s = strtime( iar->reqtime ), IA_BLOCKSIZE_SEC );

				free( s );
				free( ns );
			}

			sleep( estimated_sleeptime_sec );

			pmtfifo_push( ia->mtf, (void *) iar );

		} else if( iar != (Ia2orb_req *) NULL ) {

			free( iar );
		}

		pthread_setcancelstate( old, &old );
	}

	return (void *) NULL;
}

int
main( int argc, char **argv )
{
	int	c;
	int	errflag = 0;
	char	*s;
	char	srcname[ORBSRCNAME_SIZE];
	int	rc;
	pthread_t tid_command;
	pthread_t tid_pfwatch;
	char	*threadrc;
	char	*ns;

	elog_init( argc, argv );

	config_ia2orb_init();

	while( ( c = getopt( argc, argv, "c:p:s:t:kvV" ) ) != -1 ) {

		switch( c ) {
		case 'c':
			Cf.cmdorbname = optarg;
			break;
		case 'k':
			Cf.keep_miniseed++;
			break;
		case 'p':
			Cf.pfname = optarg;
			break;
		case 's':
			Cf.statefile = optarg;
			break;
		case 't':
			Cf.target = optarg;
			break;
		case 'v':
			Cf.verbose++;
			break;
		case 'V':
			Cf.veryverbose++;
			Cf.verbose++;
			break;
		case '?':
			errflag++;
			break;
		}
	}

	if( errflag || ( argc - optind != 1 ) ) {

		usage();

	} else {

		Cf.orbname = argv[optind];
	}

	if( Cf.cmdorbname == NULL ) {

		Cf.cmdorbname = Cf.orbname;
	}

	Iarun.thread_names = newarr( 0 );
	Iarun.IAs = newarr( 0 );
	Iarun.Stop = 0;

	thread_register( "main" );

	if( Cf.verbose ) {

		elog_notify( 0, "[thread '%s']: Program started at %s UTC\n", 
				ns = thread_name(), s = strtime( std_now() ) );

		free( s );
		free( ns );
	}

	pfread( Cf.pfname, &Cf.pf );

	config_ia2orb();

	if( Cf.statefile != (char *) NULL ) {

		rc = exhume( Cf.statefile, &Iarun.Stop, EXHUME_SLEEPTIME_SEC, 0 );

		if( rc < 0 ) {

			elog_complain( 0, "[thread '%s']: Failed to exhume state information from "
					  "statefile '%s'; disabling state-tracking\n",
					  ns = thread_name(), Cf.statefile );

			free( ns );

			Cf.statefile = NULL;

		} else if( rc == 0 && Cf.verbose ) {

			elog_notify( 0, "[thread '%s']: Initiated state tracking in statefile '%s'\n",
					  ns = thread_name(), Cf.statefile );

			free( ns );

		} else if( rc > 0 && Cf.verbose ) {

			elog_notify( 0, "[thread '%s']: Retrieving state information from statefile '%s'\n",
					  ns = thread_name(), Cf.statefile );

			free( ns );
		}
	}

	if( ( Iarun.dataorbwritefd = orbopen( Cf.orbname, "w&" ) ) < 0 ) {

		elog_die( 0, 
			"[thread '%s']: Failed to open output-data orbserver '%s' for writing. Bye.\n", 
			ns = thread_name(), Cf.orbname ); 

		free( ns );

	} else if( Cf.verbose ) {

		elog_notify( 0, "[thread '%s']: Opened data output orb '%s' for writing.\n", 
				ns = thread_name(), Cf.orbname );

		free( ns );
	}

	if( ( Iarun.cmdorbwritefd = orbopen( Cf.cmdorbname, "w&" ) ) < 0 ) {

		elog_die( 0, "[thread '%s']: Failed to open command orbserver '%s' for writing. Bye.\n",
				ns = thread_name(), Cf.orbname ); 

		free( ns );

	} else if( Cf.verbose ) {

		elog_notify( 0, "[thread '%s']: Opened cmd output orb '%s' for writing.\n", 
				ns = thread_name(), Cf.orbname );

		free( ns );
	}

	if( ( Iarun.cmd_opt = orbputthr_new( Iarun.cmdorbwritefd, 0 ) ) == NULL ) {

		elog_die( 1, 
			"[thread '%s']: Unexpected failure to open cmd-orb orbput thread!. Bye.\n",
			ns = thread_name());

		free( ns );
	}

	if( ( Iarun.cmdorbreadfd = orbopen( Cf.cmdorbname, "r&" ) ) < 0 ) {

		elog_die( 0, 
			"[thread '%s']: Failed to open command orbserver '%s' for reading. Bye.\n",
			ns = thread_name(), Cf.orbname ); 

		free( ns );

	} else if( Cf.verbose ) {

		elog_notify( 0, "[thread '%s']: Opened command input orb '%s' for reading.\n", 
				ns = thread_name(), Cf.cmdorbname );

		free( ns );
	}

	sprintf( srcname, "%s/pf/dlcm", Cf.target );

	if( Cf.verbose ) {

		elog_notify( 0, "[thread '%s']: Watching orb '%s' for '%s' command packets\n", 
				ns = thread_name(), Cf.cmdorbname, srcname );

		free( ns );
	}

	orbselect( Iarun.cmdorbreadfd, srcname );

	if( Cf.statefile ) {

		rc = orbresurrect( Iarun.cmdorbreadfd, &Iarun.cmd_pktid, &Iarun.cmd_pkttime );

		if( Cf.verbose ) {
			
			if( rc == 0 ) {
			
				elog_notify( 0, "[thread '%s']: Repositioned command-orb '%s' to pktid %d, "
						"time '%s UTC' to read command packets\n", 
						ns = thread_name(), Cf.cmdorbname, Iarun.cmd_pktid, 
						s = strtime( Iarun.cmd_pkttime ) );

				free( ns );
				free( s );

			} else {

				elog_complain( 0, "[thread '%s']: Failed to reposition command-orb '%s' "
						"based on state information in '%s'; continuing\n",
						ns = thread_name(), Cf.cmdorbname, Cf.statefile );

				free( ns );
			}
		}
	}

	if( ( Iarun.cmd_ort = orbreapthr_new( Iarun.cmdorbreadfd, IA2ORB_INTERNAL_TIMEOUT, 0 ) ) == NULL ) {

		elog_die( 1, 
			"[thread '%s']: Unexpected failure to open command-orb orbreap thread!. Bye.\n",
			ns = thread_name() );

		free( ns );
	}

	pthread_mutex_init( &Iarun.runcmd_mutex, NULL );
	pthread_mutex_init( &Iarun.threadname_mutex, NULL );

	rc = pthread_create( &tid_pfwatch, NULL, ia2orb_pfwatch, 0 );

	rc = pthread_create( &tid_command, NULL, ia2orb_command, 0 );

	while( pthread_join( tid_command, (void **) &threadrc ) == 0 );

	if( Cf.verbose ) {

		elog_notify( 0, "[thread '%s']: Program terminated\n", ns = thread_name() );

		free( ns );
	}

	return 0;
}
