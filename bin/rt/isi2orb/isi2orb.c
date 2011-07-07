
/*
 *   Copyright (c) 2007 Lindquist Consulting, Inc.
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
#include "stock.h"
#include "db.h"
#include "orb.h"
#include "Pkt.h"
#include "libisi_isi.h"
#include "libisi_iacp.h"
#include "libisi_util.h"

#define ISI2ORB_NULL_CALIB 0.0
#define ISI2ORB_NULL_CALPER -1.0
#define ISI2ORB_SHUTDOWN_SEC 1.0
#define ISI2ORB_EPSILON 1.0
#define ISI2ORB_NULL_TIME -9999999999.999
#define STATEFILE_BURY_INTERVAL_NPKTS 50

Arr *Cnf_entries = 0;
int Verbose = 0;
int VeryVerbose = 0;

static void 
usage() 
{
	char	*version = "$Date$";
	char 	*usage = "[-vV] [-p pfname] [-S statefile] server orbname\n";
	char	*author = "Kent Lindquist";
	char	*location = "Lindquist Consulting, Inc.";
	char	*email = "kent@lindquistconsulting.com";

	cbanner( version, usage, author, location, email );

	return;
}

static void
mortician() 
{
	if( VeryVerbose ) {

		elog_notify( 0, "Saving state\n" );
	}
}

static void
isi2elog( char *msg )
{
	if( Verbose ) {

		elog_notify( 0, msg );
	}

	return;
}

ISI_STREAM_CNF *
get_cnf( char *key )
{
	ISI_STREAM_CNF *entry;

	if( Cnf_entries != (Arr *) NULL ) {

		entry = (ISI_STREAM_CNF *) getarr( Cnf_entries, key );

	} else {

		entry = (ISI_STREAM_CNF *) NULL;
	}


	return entry;
}

int
main( int argc, char **argv )
{
	ISI 	*isi = 0;
	ISI_PARAM par;
	ISI_DATA_REQUEST  *dreq = 0;
	ISI_GENERIC_TS    *ts = 0;
	ISI_CNF_REPORT    *report = 0;
	ISI_STREAM_CNF    *entry = 0;
	Relic	relic;
	Packet	*pkt = 0;
	PktChannel *pktchan = 0;
	Tbl	*streams = 0;
	Tbl	*statekeys = 0;
	Tbl	*matching = 0;
	Arr	*added = 0;
	Arr	*latest = 0;
	Pf	*pf = 0;
	Pf	*pfstatepeek = 0;
	FILE	*fpstatepeek = 0;
	char	*orbname = 0;
	char	*statefile = 0;
	char	*net = 0;
	char	*server = 0;
	char	*streamspec = 0;
	char	*segtype = 0;
	char	*isi_logging = 0;
	char	*packet = 0;
	char	*keyp = 0;
	char	*too_old_string = 0;
	char	*too_new_string = 0;
	char	*s = 0;
	char	*u = 0;
	char	c = NULL;
	char	msg[STRSZ];
	char	key[STRSZ];
	char	regkey[STRSZ];
	char	srcname[ORBSRCNAME_SIZE];
	unsigned char buf[STRSZ];
	double	*nextrequest = 0;
	double	oldest = 0;
	double	t = 0;
	double	statefile_rewind_max_sec = 0;
	int	orbfd = -1;
	int	packetsz = 0;
	int	nbytes = 0;
	int	isamp = 0;
	int	ikey = 0;
	int	ireq = 0;
	int	icnf = 0;
	int	errflag = 0;
	int	status = 0;
	int	port = 0;
	int	debug = 0;
	int	rc = 0;

	char	*pfname = "isi2orb";
	double	begtime = ISI_NEWEST;
	double	endtime = ISI_KEEPUP;
	double	too_old = ISI2ORB_NULL_TIME;
	double	too_new = ISI2ORB_NULL_TIME;
	int	format = ISI_FORMAT_GENERIC;
	int	compress = ISI_COMP_NONE;
	int	timeout_msec = IACP_DEF_AT_TIMEO;
	int	bury_counter = STATEFILE_BURY_INTERVAL_NPKTS;
	int	uppercase = 1;
	int	stop = 0;

	elog_init( argc, argv );

	while( ( c = getopt( argc, argv, "vVp:S:" ) ) != -1 ) {

	switch( c ) {

	case 'v':
		Verbose++;
		break;

	case 'V':
		Verbose++;
		VeryVerbose++;
		break;

	case 'p':
		pfname = optarg;
		break;

	case 'S':
		statefile = optarg;
		break;

	case '?':
		errflag++;
		break;
	}

	}

	if( errflag ) {
		
		usage();

		exit( -1 );
	}

	if( argc - optind != 2 ) {

		usage();

		exit( -1 );

	} else {
		
		server = argv[optind++];
		orbname = argv[optind];
	}

	if( Verbose ) {

		elog_notify( 0, "Program starting at %s UTC\n", 
				s = strtime( now() ) );

		free( s );
	}

	added = newarr( 0 );
	latest = newarr( 0 );

	if( statefile ) {

		if( is_present( statefile ) ) {

			fpstatepeek = fopen( statefile, "r" );
			pfin( fpstatepeek, &pfstatepeek );
			statekeys = pfkeys( pfstatepeek );
			fclose( fpstatepeek );

		} else {

			statekeys = newtbl( 0 );
		}

		rc = exhume( statefile, &stop, ISI2ORB_SHUTDOWN_SEC, mortician );

		if( rc == -1 ) {
			
			elog_complain( 1, "State file failure; disabling\n" );

			statefile = 0;
		}

		if( Verbose ) {

			if( rc == 0 ) {

				sprintf( msg, "file not found; "
					      "initiating new state file" );

			} else if( rc == 1 ) {

				sprintf( msg, "previous state file "
					      "recovered without error" );

			} else if( rc == 2 ) {

				sprintf( msg, "previous state file "
					      "was corrupt; ignoring state " 
					      "information" );
				
			} else {

				/* Shouldn't happen: */

				sprintf( msg, "Unknown recovery status "
					      "after exhuming file" );
			}

			elog_notify( 0, "Recording state information " 
				"in file '%s' (%s)\n", statefile, msg );
		}

		for( ikey = 0; ikey < maxtbl( statekeys ); ikey++ ) {

			keyp = gettbl( statekeys, ikey );

			allot( double *, nextrequest, 1 );

			setarr( latest, keyp, (void *) nextrequest );

			relic.dp = nextrequest;

			rc = resurrect( keyp, relic, TIME_RELIC );

			if( rc == 0 && Verbose ) {
					
				elog_notify( 0,
				  "resurrected state for '%s' as %s "
				  "UTC\n",
				  keyp, 
				  s = strtime( *nextrequest ) );

				free( s );
			}
		}

		if( statekeys ) {

			freetbl( statekeys, 0 );
		}
	}

	pfread( pfname, &pf );

	net = pfget_string( pf, "net" );
	streams = pfget_tbl( pf, "streams" );
	segtype = pfget_string( pf, "segtype" );
	isi_logging = pfget_string( pf, "isi_logging" );
	uppercase = pfget_boolean( pf, "uppercase" );
	port = pfget_int( pf, "port" );
	debug = pfget_int( pf, "isi_debug" );
	timeout_msec = pfget_int( pf, "iacp_timeout_msec" );
	statefile_rewind_max_sec = pfget_double( pf, "statefile_rewind_max_sec" );
	too_old_string = pfget_string( pf, "too_old" );
	too_new_string = pfget_string( pf, "too_new" );

	if( too_old_string && strcmp( too_old_string, "" ) ) {

		too_old = str2epoch( too_old_string );

		if( Verbose ) {

			elog_notify( 0, "Rejecting packets more than %f seconds older "
					"than system time\n", too_old );
		}
	}

	if( too_new_string && strcmp( too_new_string, "" ) ) {

		too_new = str2epoch( too_new_string );

		if( Verbose ) {

			elog_notify( 0, "Rejecting packets more than %f seconds newer "
					"than system time\n", too_new );
		}
	}

	if( maxtbl( streams ) <= 0 ) {
		
		elog_die( 0, "No streams specified in 'streams' table "
			     "of %s.pf. Bye\n", pfname );

	} else {

		streamspec = jointbl( streams, "+" );
	}

	freetbl( streams, 0 );

	if( Verbose ) {
		
		elog_notify( 0, "Using stream specification '%s'\n", 
			streamspec );
	}

	if( ( orbfd = orbopen( orbname, "r&" ) ) < 0 ) {
		
		elog_die( 0, "Failed to open %s for writing. Bye.\n", orbname );
	}

	pkt = newPkt();

	pkt->pkttype = suffix2pkttype( "GENC" );

	pktchan = newPktChannel();

	pushtbl( pkt->channels, pktchan );

	pkt->nchannels = 1;

	utilNetworkInit();

	isiInitDefaultPar( &par );

	if( debug > 0 ) {
		
		isiSetDebugFlag( &par, debug );
	}

	if( port != 0 ) {

		isiSetServerPort( &par, port );

		if( VeryVerbose ) {

			elog_notify( 0, "Server port reset to %d\n", port );
		}
	}

	if( timeout_msec >= IACP_MINTIMEO ) {

		par.attr.at_timeo = timeout_msec;

		if( VeryVerbose ) {

			elog_notify( 0, "IDA Protocol timeout (iacp_timeout_msec parameter) set to %d milliseconds\n", timeout_msec );
		}

	} else {

		elog_complain( 0, "Ignoring attempt to set iacp_timeout_msec to %d, which is below "
				  "IDA Protocol timeout minimum of %d milliseconds\n", 
				  timeout_msec, IACP_MINTIMEO );
	} 


	if( ! strcmp( isi_logging, "elog" ) ) {

		isiStartLogging( &par, 0, isi2elog, "isi2orb" ); 

	} else {

		isiStartLogging( &par, isi_logging, 0, "isi2orb" ); 
	}

	Cnf_entries = newarr( 0 );

	if( ( report = isiCnf( server, &par ) ) == NULL ) {

		elog_die( 0, "isiCnf failure. Bye.\n" );
	}

	if( VeryVerbose ) {

		fprintf( stdout, "Configuration report:\n\n" );
		fprintf( stdout,
			"  Sta Chn Loc    Sint    Lat       Long        Elev"
			"     Depth     Calib      Calper    Hang    Vang   "
			"Inst\n" );
	}

	for( icnf = 0; icnf < report->nentry; icnf++ ) {

		if( VeryVerbose ) {

			isiPrintStreamCnf( stdout, &report->entry[icnf] );
		}

		sprintf( key, "%s.%s.%s", 
			      report->entry[icnf].name.sta,
			      report->entry[icnf].name.chn,
			      report->entry[icnf].name.loc );

		setarr( Cnf_entries, key, &(report->entry[icnf]) );
	}

	free( report );

	dreq = isiAllocSimpleDataRequest( begtime, endtime, streamspec );

	if( dreq == NULL ) {
		
		elog_die( 1, "isiAllocSimpleDataRequest: failed\n" );
	}

	isiSetDatreqFormat( dreq, format );
	isiSetDatreqCompress( dreq, compress );

	for( ireq = 0; ireq < dreq->nreq; ireq++ ) {
		
		sprintf( key, "%s.%s.%s", 
			dreq->req.twind[ireq].name.sta,
			dreq->req.twind[ireq].name.chn,
			dreq->req.twind[ireq].name.loc );

		sprintf( regkey, "%s\\.%s\\.%s",
		  strcmp( dreq->req.twind[ireq].name.sta, ISI_NAME_WILDCARD ) ? 
			dreq->req.twind[ireq].name.sta : "[^.]*",
		  strcmp( dreq->req.twind[ireq].name.chn, ISI_NAME_WILDCARD ) ? 
			dreq->req.twind[ireq].name.chn : "[^.]*",
		  strcmp( dreq->req.twind[ireq].name.loc, ISI_NAME_WILDCARD ) ? 
			dreq->req.twind[ireq].name.loc : "[^.]*" );

		if( ( nextrequest = getarr( latest, key ) ) != NULL ) {

			if( statefile_rewind_max_sec != 0 && 
			    *nextrequest < now() - statefile_rewind_max_sec ) {
			

				if( Verbose ) {
					
					elog_notify( 0, 
					  "Data request for '%s' is more than "
					  "'%s' behind present; ignoring "
					  "state file, setting to newest\n",
					  key,
					  s = strtdelta( statefile_rewind_max_sec ) );

					free( s );
				}

				*nextrequest = 0;

				dreq->req.twind[ireq].beg = ISI_NEWEST;

			} else if( *nextrequest > now() ) {

				elog_complain( 0, 
					  "Data request for '%s' is ahead of "
					  "system clock; ignoring "
					  "state file, setting to newest\n",
					  key );

				*nextrequest = 0;

				dreq->req.twind[ireq].beg = ISI_NEWEST;

			} else {

				dreq->req.twind[ireq].beg = *nextrequest;
			}

		} else {

			matching = greparr( regkey, latest );

			oldest = 0;

			for( ikey = 0; ikey < maxtbl( matching ); ikey++ ) {

				keyp = (char *) gettbl( matching, ikey );

				if( keyp == NULL ) {

					continue;
				}

				nextrequest = (double *) getarr( latest, keyp );

				if( nextrequest == NULL ) {

					continue;
				}


				if( oldest == 0 || oldest > *nextrequest ) {

					oldest = *nextrequest;
				}
			}

			freetbl( matching, 0 );

			if( oldest != 0 ) {

				if( statefile_rewind_max_sec != 0 && 
			    	    oldest < now() - statefile_rewind_max_sec ) {
			
					if( Verbose ) {

					elog_notify( 0, 
					  "Data request for '%s' is more than "
					  "'%s' behind present; ignoring "
					  "state file, setting to newest\n",
					  key,
					  s = strtdelta( statefile_rewind_max_sec ) );

					free( s );
					}

					oldest = now();

					dreq->req.twind[ireq].beg = ISI_NEWEST;

				} else {

					dreq->req.twind[ireq].beg = oldest;
				}
			}

		}
	}

	if( VeryVerbose ) {

		fprintf( stdout, "ISI Data request:\n\n" );

		isiPrintDatreq( stdout, dreq );
	}

	isi = isiInitiateDataRequest( server, &par, dreq );

	isiFreeDataRequest( dreq );

	while( ! stop &&
	       ( ts = isiReadGenericTS( isi, &status ) ) != NULL ) {
	
		if( VeryVerbose ) {

			elog_notify( 0, "%s\n", 
				isiGenericTsHdrString( &ts->hdr, 
						       (char *) buf ) );
		}

		sprintf( key, "%s.%s.%s", 
				ts->hdr.name.sta,
				ts->hdr.name.chn,
				ts->hdr.name.loc );

		if( Verbose && getarr( added, key ) == NULL ) {

			elog_notify( 0, "Adding new channel %s\n", key );

			setarr( added, key, (void *) 0x1 );
		}

		nextrequest = (double *) getarr( latest, key );

		if( nextrequest == NULL ) {

			allot( double *, nextrequest, 1 );

			*nextrequest = ts->hdr.tols.value + ISI2ORB_EPSILON;

			setarr( latest, key, (void *) nextrequest );

			if( statefile ) {
				
				relic.dp = nextrequest;

				rc = resurrect( key, relic, TIME_RELIC );
			}

		} else if( ts->hdr.tols.value < *nextrequest ) {

			if( VeryVerbose ) {
				
				elog_notify( 0, "Skipping '%s' data packet " 
				  "because endtime '%s' is less than "
				  "statefile computation '%s'\n",
				  key,
				  s = strtime( ts->hdr.tols.value ),
				  u = strtime( *nextrequest ) );

				free( s );
				free( u );
			}

			continue;

		} else {

			*nextrequest = ts->hdr.tols.value + ISI2ORB_EPSILON;
		}

		strcpy( pkt->parts.src_net, net );
		strcpy( pkt->parts.src_sta, ts->hdr.name.sta );
		strcpy( pkt->parts.src_chan, ts->hdr.name.chn );
		if( strcmp( ts->hdr.name.loc, "  " ) ) {

			strcpy( pkt->parts.src_loc, ts->hdr.name.loc );

		} else {

			strcpy( pkt->parts.src_loc, "" );
		}

		strcpy( pktchan->net, net );
		strcpy( pktchan->sta, ts->hdr.name.sta );
		strcpy( pktchan->chan, ts->hdr.name.chn );
		if( strcmp( ts->hdr.name.loc, "  " ) ) {

			strcpy( pktchan->loc, ts->hdr.name.loc );

		} else {

			strcpy( pktchan->loc, "" );
		}
		strcpy( pktchan->segtype, segtype );

		if( uppercase ) {

			str2upper(  pkt->parts.src_net );
			str2upper(  pkt->parts.src_sta );
			str2upper(  pkt->parts.src_chan );
			str2upper(  pkt->parts.src_loc );

			str2upper(  pktchan->net );
			str2upper(  pktchan->sta );
			str2upper(  pktchan->chan );
			str2upper(  pktchan->loc );
		}

		if( ( entry = get_cnf( key ) ) != (ISI_STREAM_CNF *) NULL ) {

			pktchan->calib = entry->inst.calib;
			pktchan->calper = entry->inst.calper;

		} else {

			pktchan->calib = ISI2ORB_NULL_CALIB; 
			pktchan->calper = ISI2ORB_NULL_CALPER;
		}

		pktchan->nsamp = ts->hdr.nsamp;
		pktchan->datasz = ts->hdr.nsamp;
		pktchan->samprate = 1. / isiSrateToSint( &ts->hdr.srate );
		pktchan->time = ts->hdr.tofs.value;

		if( too_old != ISI2ORB_NULL_TIME &&
		    pktchan->time < now() - too_old ) {

		    if( VeryVerbose ) {

			elog_complain( 0, "Rejecting '%s' packet because time '%s' "
					  "is more than %s older than system clock\n",
					  key,
					  s = strtime( pktchan->time ),
					  u = strtdelta( too_old ) );

			free( s );
			free( u );
		    }

		    continue;
		}

		if( too_new != ISI2ORB_NULL_TIME &&
		    pktchan->time > now() + too_new ) {

		    if( VeryVerbose ) {

			elog_complain( 0, "Rejecting '%s' packet because time '%s' "
					  "is more than %s newer than system clock\n",
					  key,
					  s = strtime( pktchan->time ),
					  u = strtdelta( too_new ) );

			free( s );
			free( u );
		    }

		    continue;
		}

		reallot( int *, pktchan->data, pktchan->nsamp * sizeof( int ) );

		if( ts->hdr.desc.type == ISI_TYPE_INT8 ) {

		   for( isamp = 0; isamp < pktchan->nsamp; isamp++ ) {

			pktchan->data[isamp] = 
				(int) *( (char *) ts->data + isamp );
		   }

		} else if( ts->hdr.desc.type == ISI_TYPE_INT16 ) {

		   for( isamp = 0; isamp < pktchan->nsamp; isamp++ ) {

			pktchan->data[isamp] = 
				(int) *( (short *) ts->data + isamp );
		   }

		} else if( ts->hdr.desc.type == ISI_TYPE_INT32 ) {

		   for( isamp = 0; isamp < pktchan->nsamp; isamp++ ) {

			pktchan->data[isamp] = *( (int *) ts->data + isamp );
		   }

		} else if( ts->hdr.desc.type == ISI_TYPE_INT64 ) {

		   for( isamp = 0; isamp < pktchan->nsamp; isamp++ ) {

			pktchan->data[isamp] = 
				(int) *( (long int *) ts->data + isamp );
		   }

		} else if( ts->hdr.desc.type == ISI_TYPE_REAL32 ) {

		   elog_complain( 0, "Warning: cast from real32 to int32; " 
				     "data may be truncated\n" );

		   for( isamp = 0; isamp < pktchan->nsamp; isamp++ ) {

			pktchan->data[isamp] = 
				(int) *( (float *) ts->data + isamp );
		   }

		} else if( ts->hdr.desc.type == ISI_TYPE_REAL64 ) {

		   elog_complain( 0, "Warning: cast from real64 to int32; " 
				     "data may be truncated\n" );

		   for( isamp = 0; isamp < pktchan->nsamp; isamp++ ) {

			pktchan->data[isamp] = 
				(int) *( (double *) ts->data + isamp );
		   }

		} else {

			elog_complain( 0, "datatype not supported\n" );

			continue;
		}

		rc = stuffPkt( pkt, srcname, &t, &packet, &nbytes, &packetsz );

		if( rc < 0 ) {
			
			elog_complain( 0, "stuffPkt routine failed\n" );

			continue;

		} else if( orbput( orbfd, srcname, t, packet, nbytes ) ) {

			elog_complain( 0, "orbput fails\n" );
		}

		if( statefile && bury_counter-- <= 0 ) {

			bury();

			bury_counter = STATEFILE_BURY_INTERVAL_NPKTS;
		}
	}


	if( statefile ) {

		bury();
	}

	elog_notify( 0, "Program stopping at %s UTC\n", 
			s = strtime( now() ) );

	return 0;
}
