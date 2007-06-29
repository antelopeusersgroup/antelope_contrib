
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
#include "libisi_util.h"

#define NULL_CALIB 0.0
#define NULL_CALPER -1.0

Arr *Cnf_entries = 0;

static void 
usage() 
{
	char	*version = "$Date$";
	char 	*usage = "[-vV] [-p pfname] server orbname\n";
	char	*author = "Kent Lindquist";
	char	*location = "Lindquist Consulting, Inc.";
	char	*email = "kent@lindquistconsulting.com";

	cbanner( version, usage, author, location, email );

	return;
}

static void
isi2elog( char *msg )
{
	elog_notify( 0, msg );

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

	return;
}

int
main( int argc, char **argv )
{
	ISI 	*isi;
	ISI_PARAM par;
	int	orbfd;
	int	verbose = 0;
	int	veryverbose = 0;
	int	errflag = 0;
	char	*pfname = "isi2orb";
	char	*orbname;
	char	*net;
	char	*server;
	char	*streamspec;
	char	*segtype;
	char	*isi_logging;
	char	key[STRSZ];
	char	c;
	char	*s;
	Tbl	*streams;
	Arr	*added;
	Pf	*pf;
	ISI_DATA_REQUEST *dreq;
	ISI_GENERIC_TS *ts;
	ISI_CNF_REPORT *report;
	ISI_STREAM_CNF *entry;
	int	format = ISI_FORMAT_GENERIC;
	int	compress = ISI_COMP_NONE;
	double	begtime = ISI_NEWEST;
	double	endtime = ISI_KEEPUP;
	int	status;
	unsigned char buf[STRSZ];
	char	srcname[ORBSRCNAME_SIZE];
	int	packetsz = 0;
	int	nbytes;
	double	t;
	char	*packet;
	Packet	*pkt;
	PktChannel *pktchan;
	int	isamp;
	int	icnf;
	int	uppercase = 1;
	int	rc;

	elog_init( argc, argv );

	while( ( c = getopt( argc, argv, "vVp:" ) ) != -1 ) {

	switch( c ) {

	case 'v':
		verbose++;
		break;

	case 'V':
		verbose++;
		veryverbose++;
		break;

	case 'p':
		pfname = optarg;
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

	if( verbose ) {

		elog_notify( 0, "Program starting at %s UTC\n", 
				s = strtime( now() ) );

		free( s );
	}

	pfread( pfname, &pf );

	net = pfget_string( pf, "net" );
	server = pfget_string( pf, "server" );
	streams = pfget_tbl( pf, "streams" );
	segtype = pfget_string( pf, "segtype" );
	isi_logging = pfget_string( pf, "isi_logging" );
	uppercase = pfget_boolean( pf, "uppercase" );

	if( maxtbl( streams ) <= 0 ) {
		
		elog_die( 0, "No streams specified in 'streams' table "
			     "of %s.pf. Bye\n", pfname );
	} else {

		streamspec = jointbl( streams, "+" );
	}

	if( verbose ) {
		
		elog_notify( 0, "Using stream specification '%s'\n", 
			streamspec );
	}

	if( ( orbfd = orbopen( orbname, "r&" ) ) < 0 ) {
		
		die( 0, "Failed to open %s for writing. Bye.\n", orbname );
	}

	added = newarr( 0 );

	pkt = newPkt();

	pkt->pkttype = suffix2pkttype( "GENC" );

	pktchan = newPktChannel();

	pushtbl( pkt->channels, pktchan );

	pkt->nchannels = 1;

	utilNetworkInit();

	isiInitDefaultPar( &par );

	if( ! strcmp( isi_logging, "elog" ) ) {

		isiStartLogging( &par, 0, isi2elog, "isi2orb" ); 

	} else {

		isiStartLogging( &par, isi_logging, 0, "isi2orb" ); 
	}

	Cnf_entries = newarr( 0 );

	if( ( report = isiCnf( server, &par ) ) == NULL ) {

		elog_die( 0, "isiCnf failure. Bye.\n" );
	}

	if( veryverbose ) {

		fprintf( stdout, "Configuration report:\n\n" );
		fprintf( stdout,
			"  Sta Chn Loc    Sint    Lat       Long        Elev"
			"     Depth     Calib      Calper    Hang    Vang   "
			"Inst\n" );
	}

	for( icnf = 0; icnf < report->nentry; icnf++ ) {

		if( veryverbose ) {

			isiPrintStreamCnf( stdout, &report->entry[icnf] );
		}

		sprintf( key, "%s.%s.%s", 
			      report->entry[icnf].name.sta,
			      report->entry[icnf].name.chn,
			      report->entry[icnf].name.loc );

		setarr( Cnf_entries, key, report->entry + icnf );
	}

	free( report );

	dreq = isiAllocSimpleDataRequest( begtime, endtime, streamspec );

	if( dreq == NULL ) {
		
		elog_die( 1, "isiAllocSimpleDataRequest: failed\n" );
	}

	isiSetDatreqFormat( dreq, format );
	isiSetDatreqCompress( dreq, compress );

	if( veryverbose ) {

		fprintf( stdout, "ISI Data request:\n\n" );

		isiPrintDatreq( stdout, dreq );
	}

	isi = isiInitiateDataRequest( server, &par, dreq );

	isiFreeDataRequest( dreq );

	while( ( ts = isiReadGenericTS( isi, &status ) ) != NULL ) {
	
		if( veryverbose ) {

			elog_notify( 0, "%s\n", 
				isiGenericTsHdrString( &ts->hdr, 
						       (char *) buf ) );
		}

		sprintf( key, "%s.%s.%s", 
				ts->hdr.name.sta,
				ts->hdr.name.chn,
				ts->hdr.name.loc );

		if( verbose && getarr( added, key ) == NULL ) {

			elog_notify( 0, "Adding new channel %s\n", key );

			setarr( added, key, (void *) 0x1 );
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

			pktchan->calib = NULL_CALIB; 
			pktchan->calper = NULL_CALPER;
		}

		pktchan->nsamp = ts->hdr.nsamp;
		pktchan->datasz = ts->hdr.nsamp;
		pktchan->samprate = 1. / isiSrateToSint( &ts->hdr.srate );
		pktchan->time = ts->hdr.tofs.value;

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
			
			complain( 0, "stuffPkt routine failed\n" );

			continue;

		} else if( orbput( orbfd, srcname, t, packet, nbytes ) ) {

			complain( 0, "orbput fails\n" );
		}
	}

	elog_notify( 0, "Program stopping at %s UTC\n", 
			s = strtime( now() ) );

	return 0;
}
