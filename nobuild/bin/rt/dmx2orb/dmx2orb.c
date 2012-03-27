#include <stdlib.h>
#include <stdio.h>
#include "db.h"
#include "orb.h"
#include "Pkt.h"
#include "tr.h"
#include "pf.h"
#include "stock.h"
#include "coords.h"

#define STATIONCOMP 5
#define DESCRIPTRACE 7
#define INSTRUMENT 31

static Pf *pf;

typedef struct {
	char	sync;		/* Should be 'S' otherwise there's an error */
	char	machine;	/* "6" => Intel x86 architecture (i.e. little-endian) */
	short	struct_type;
	long	struct_length_bytes;
	long	data_length_bytes;
} Suds_tag;

typedef struct {
	char	network[4];
	char	st_name[5];
	char	component;
	short	inst_type;
} Suds_statident;

typedef struct {
	Suds_statident sc_name;
	short	azim;
	short	incid;
	double	st_lat;
	double	st_long;
	float	elev;
	char	enclosure;
	char	annotation;
	char	recorder;
	char	rockclass;
	short	rocktype;
	char	sitecondition;
	char	sensor_type;
	char	data_type;
	char	data_units;
	char	polarity;
	char	st_status;
	float	max_gain;
	float	clip_value;
	float	con_mvolts;
	short	channel;
	short	atod_gain;
	long	effective;
	float	clock_correct;
	float	station_delay;
} Suds_stationcomp;

typedef struct {
	Suds_statident dt_name;
	double	begintime;
	short	localtime;
	char	datatype;
	char	descriptor;
	short	digi_by;
	short 	processed;
	long	length;
	float	rate;
	float	mindata;
	float	maxdata;
	float 	avenoise;
	long	numclip;
	double	time_correct;
	float	rate_correct;
} Suds_descriptrace;

typedef struct {
	Suds_statident in_name;
	short	in_serial;
	short 	comps;
	short 	channel;
	char	sens_type;
	char	datatype;
	long	void_samp;
	float	dig_con;
	float	aa_corner;
	float	aa_poles;
	float	nat_freq;
	float	damping;
	float	mot_con;
	float	gain;
	float	local_x;
	float	local_y;
	float	local_z;
	long	effective;	
	float	pre_event;
	short	trig_num;
	char	study[6];
	short	sn_serial;
} Suds_instrument;

int
read_header( FILE *fp, Suds_tag *header ) {

	int	swap;

	fread( &header->sync, sizeof( char ), 1, fp );
	fread( &header->machine, sizeof( char ), 1, fp );

	fread( &header->struct_type, sizeof( short ), 1, fp );
	fread( &header->struct_length_bytes, sizeof( long ), 1, fp );
	fread( &header->data_length_bytes, sizeof( long ), 1, fp );

#ifdef _BIG_ENDIAN 

	if( header->machine == '6' ) {
		swap = 1;
	} else {
		swap = 0;
	}

#else /* Assume little-endian */

	if( header->machine == '6' ) { 	
		swap = 0;
	} else {
		swap = 1;		/* Assume big-endian order */
	}

#endif

	if( swap ) {
		swap2( &header->struct_type, &header->struct_type, 1 );
		swap4( &header->struct_length_bytes, &header->struct_length_bytes, 1 );
		swap4( &header->data_length_bytes, &header->data_length_bytes, 1 );
	}

	return swap;
}

void 
read_stationcomp( FILE *fp, Suds_tag header, Suds_stationcomp *s, int swap ) {
	
	fread( &s->sc_name.network, sizeof( char ), 4, fp );
	fread( &s->sc_name.st_name, sizeof( char ), 5, fp );
	fread( &s->sc_name.component, sizeof( char ), 1, fp );
	fread( &s->sc_name.inst_type, sizeof( short ), 1, fp );
	fread( &s->azim, sizeof( short ), 1, fp );
	fread( &s->incid, sizeof( short ), 1, fp );
	fread( &s->st_lat, sizeof( double ), 1, fp );
	fread( &s->st_long, sizeof( double ), 1, fp );
	fread( &s->elev, sizeof( float ), 1, fp );
	fread( &s->enclosure, sizeof( char ), 1, fp );
	fread( &s->annotation, sizeof( char ), 1, fp );
	fread( &s->recorder, sizeof( char ), 1, fp );
	fread( &s->rockclass, sizeof( char ), 1, fp );
	fread( &s->rocktype, sizeof( short ), 1, fp );
	fread( &s->sitecondition, sizeof( char ), 1, fp );
	fread( &s->sensor_type, sizeof( char ), 1, fp );
	fread( &s->data_type, sizeof( char ), 1, fp );
	fread( &s->data_units, sizeof( char ), 1, fp );
	fread( &s->polarity, sizeof( char ), 1, fp );
	fread( &s->st_status, sizeof( char ), 1, fp );
	fread( &s->max_gain, sizeof( float ), 1, fp );
	fread( &s->clip_value, sizeof( float ), 1, fp );
	fread( &s->con_mvolts, sizeof( float ), 1, fp );
	fread( &s->channel, sizeof( short ), 1, fp );
	fread( &s->atod_gain, sizeof( short ), 1, fp );
	fread( &s->effective, sizeof( long ), 1, fp );
	fread( &s->clock_correct, sizeof( float ), 1, fp );
	fread( &s->station_delay, sizeof( float ), 1, fp );

	if( swap ) {
		swap2( &s->sc_name.inst_type, &s->sc_name.inst_type, 1 );
		swap2( &s->azim, &s->azim, 1 );
		swap2( &s->incid, &s->incid, 1 );
		swap8( &s->st_lat, &s->st_lat, 1 );
		swap8( &s->st_long, &s->st_long, 1 );
		swap4( &s->elev, &s->elev, 1 );
		swap2( &s->rocktype, &s->rocktype, 1 );
		swap4( &s->max_gain, &s->max_gain, 1 );
		swap4( &s->clip_value, &s->clip_value, 1 );
		swap4( &s->con_mvolts, &s->con_mvolts, 1 );
		swap2( &s->channel, &s->channel, 1 );
		swap2( &s->atod_gain, &s->atod_gain, 1 );
		swap4( &s->effective, &s->effective, 1 );
		swap4( &s->clock_correct, &s->clock_correct, 1 );
		swap4( &s->station_delay, &s->station_delay, 1 );
	}

	fseek( fp, header.data_length_bytes, SEEK_CUR );
}

void
read_descriptrace( FILE *fp, Suds_tag header, Suds_descriptrace *d, int swap, long **data, int *bufsiz ) {

	fread( &d->dt_name.network, sizeof( char ), 4, fp );
	fread( &d->dt_name.st_name, sizeof( char ), 5, fp );
	fread( &d->dt_name.component, sizeof( char ), 1, fp );
	fread( &d->dt_name.inst_type, sizeof( short ), 1, fp );
	fread( &d->begintime, sizeof( double ), 1, fp );
	fread( &d->localtime, sizeof( short ), 1, fp );
	fread( &d->datatype, sizeof( char ), 1, fp );
	fread( &d->descriptor, sizeof( char ), 1, fp );
	fread( &d->digi_by, sizeof( short ), 1, fp );
	fread( &d->processed, sizeof( short ), 1, fp );
	fread( &d->length, sizeof( long ), 1, fp );
	fread( &d->rate, sizeof( float ), 1, fp );
	fread( &d->mindata, sizeof( float ), 1, fp );
	fread( &d->maxdata, sizeof( float ), 1, fp );
	fread( &d->avenoise, sizeof( float ), 1, fp );
	fread( &d->numclip, sizeof( long ), 1, fp );
	fread( &d->time_correct, sizeof( double ), 1, fp );
	fread( &d->rate_correct, sizeof( float ), 1, fp );

	if( swap ) {
		swap2( &d->dt_name.inst_type, &d->dt_name.inst_type, 1 );
		swap8( &d->begintime, &d->begintime, 1 );
		swap2( &d->localtime, &d->localtime, 1 );
		swap2( &d->digi_by, &d->digi_by, 1 );
		swap2( &d->processed, &d->processed, 1 );
		swap4( &d->length, &d->length, 1 );
		swap4( &d->rate, &d->rate, 1 );
		swap4( &d->mindata, &d->mindata, 1 );
		swap4( &d->maxdata, &d->maxdata, 1 );
		swap4( &d->avenoise, &d->avenoise, 1 );
		swap4( &d->numclip, &d->numclip, 1 );
		swap8( &d->time_correct, &d->time_correct, 1 );
		swap4( &d->rate_correct, &d->rate_correct, 1 );
	}

	if( d->datatype != '2' ) {

		elog_complain( 1, "Datatype %s not understood--skipping\n", d->datatype );
		fseek( fp, header.data_length_bytes, SEEK_CUR );

	} else {
		if( *bufsiz < header.data_length_bytes ) {
			reallot( long *, *data, d->length ); 
			*bufsiz = d->length * sizeof( long );
		}

		fread( *data, sizeof( long ), d->length, fp );

		if( swap ) {
			swap4( *data, *data, d->length );
		}
	}
}

void
read_instrument( FILE *fp, Suds_tag header, Suds_instrument *i, int swap ) {

	fread( &i->in_name.network, sizeof( char ), 4, fp );
	fread( &i->in_name.st_name, sizeof( char ), 5, fp );
	fread( &i->in_name.component, sizeof( char ), 1, fp );
	fread( &i->in_name.inst_type, sizeof( short ), 1, fp );
	fread( &i->in_serial, sizeof( short ), 1, fp );
	fread( &i->comps, sizeof( short ), 1, fp );
	fread( &i->channel, sizeof( short ), 1, fp );
	fread( &i->sens_type, sizeof( char ), 1, fp );
	fread( &i->datatype, sizeof( char ), 1, fp );
	fread( &i->void_samp, sizeof( long ), 1, fp );
	fread( &i->dig_con, sizeof( float ), 1, fp );
	fread( &i->aa_corner, sizeof( float ), 1, fp );
	fread( &i->aa_poles, sizeof( float ), 1, fp );
	fread( &i->nat_freq, sizeof( float ), 1, fp );
	fread( &i->damping, sizeof( float ), 1, fp );
	fread( &i->mot_con, sizeof( float ), 1, fp );
	fread( &i->gain, sizeof( float ), 1, fp );
	fread( &i->local_x, sizeof( float ), 1, fp );
	fread( &i->local_y, sizeof( float ), 1, fp );
	fread( &i->local_z, sizeof( float ), 1, fp );
	fread( &i->effective, sizeof( long ), 1, fp );
	fread( &i->pre_event, sizeof( float ), 1, fp );
	fread( &i->trig_num, sizeof( short ), 1, fp );
	fread( &i->study, sizeof( char ), 6, fp );
	fread( &i->sn_serial, sizeof( short ), 1, fp );

	if( swap ) { 
		swap2( &i->in_name.inst_type, &i->in_name.inst_type, 1 );
		swap2( &i->in_serial, &i->in_serial, 1 );
		swap2( &i->comps, &i->comps, 1 );
		swap2( &i->channel, &i->channel, 1 );
		swap4( &i->void_samp, &i->void_samp, 1 );
		swap4( &i->dig_con, &i->dig_con, 1 );
		swap4( &i->aa_corner, &i->aa_corner, 1 );
		swap4( &i->aa_poles, &i->aa_poles, 1 );
		swap4( &i->nat_freq, &i->nat_freq, 1 );
		swap4( &i->damping, &i->damping, 1 );
		swap4( &i->mot_con, &i->mot_con, 1 );
		swap4( &i->gain, &i->gain, 1 );
		swap4( &i->local_x, &i->local_x, 1 );
		swap4( &i->local_y, &i->local_y, 1 );
		swap4( &i->local_z, &i->local_z, 1 );
		swap4( &i->effective, &i->effective, 1 );
		swap4( &i->pre_event, &i->pre_event, 1 );
		swap2( &i->trig_num, &i->trig_num, 1 );
		swap2( &i->sn_serial, &i->sn_serial, 1 );
	}
	
	fseek( fp, header.data_length_bytes, SEEK_CUR );
}

void
send_to_orb( Suds_tag header,
	     Suds_stationcomp s,
	     Suds_instrument i,
	     Suds_descriptrace d,
	     long *data,
	     int orbfd ) {

	static Packet *pkt = NULL;
	static PktChannel *pktchan;
	char	*srcname;
	static char *packet = NULL;
	static int nbytes;
	int	segment_nsamps;
	static int packetsz = 0;
	char	*t;
	int	offset = 0;
	Pf	*chanpf;
	char	channel[STRSZ];

	sprintf( channel, "channels{%d}", s.channel );
	if( pfresolve( pf, channel, 0, &chanpf ) < 0 ) {
		elog_complain( 1, "Didn't find channel %d in parameter file\n",
				s.channel );
		return;
	}

	srcname = pfget_string( chanpf, "srcname" ); 

	if( srcname == NULL || ! strcmp( srcname, "Ignore" ) ) {
		return;
	}

	if( ! pkt ) {
		pkt = newPkt();
		pktchan = newPktChannel();
		pushtbl( pkt->channels, pktchan );
		pkt->nchannels = 1;
		pkt->pkttype = suffix2pkttype( "replay" );
	} else {
		clrPkt( pkt );
	}

	split_srcname( srcname, &pkt->parts );
	pktchan->samprate = d.rate;
	pktchan->calib = pfget_double( chanpf, "calib" );
	pktchan->calper = pfget_double( chanpf, "calper" );
	strcpy( pktchan->net, pkt->parts.src_net );
	strcpy( pktchan->sta, pkt->parts.src_sta );
	strcpy( pktchan->chan, pkt->parts.src_chan );
	strcpy( pktchan->loc, "" );
	strcpy( pktchan->segtype, pfget_string( chanpf, "segtype" ) ); 

	while( offset < d.length ) {

		if( d.length - offset < 100 ) {
			segment_nsamps = d.length - offset;
		} else {
			segment_nsamps = 100;
		}

		pkt->time = SAMP2TIME( d.begintime, d.rate, offset );
		pktchan->data = (int *) ( data + offset );
		pktchan->time = pkt->time;
		pktchan->nsamp = segment_nsamps;
		pktchan->datasz = segment_nsamps;

		if( stuffPkt( pkt, srcname, &pkt->time, &packet, &nbytes, &packetsz ) < 0 ) {
			elog_complain( 1, "Failed to stuff %s packet starting %s\n",
			     srcname, t = strtime( d.begintime ) );
			free( t );
		} else if( orbput( orbfd, srcname, pkt->time, packet, nbytes ) < 0 ) {
			elog_complain( 1, "Failed to put %s packet starting %s on orb\n",
			     srcname, t = strtime( d.begintime ) );
			free( t );
		}

		offset += segment_nsamps;
	}
}

int
at_eof( FILE *fp ) {
	char	try;
	int	at_eof; 

	fread( &try, sizeof( char ), 1, fp );
	at_eof = feof( fp );
	fseek( fp, -1, SEEK_CUR );

	return at_eof;
}

main( int argc, char **argv ) {
	char	*orbname;
	char 	*filename;	
	FILE	*fp;
	Suds_tag header;
	Suds_stationcomp stationcomp;
	Suds_descriptrace descriptrace;
	Suds_instrument instrument;
	int	swap;
	int	orbfd;
	long	*data = NULL;
	int	bufsiz = 0;

	if( argc != 3 ) {
		elog_die( 1, "Usage: %s filename orbname\n", argv[0] );
	} else {
		filename = argv[1];
		orbname = argv[2];
	}

	pfread( "dmx2orb", &pf );

	if( ( orbfd = orbopen( orbname, "r&" ) ) < 0 ) {
		elog_die( 1, "Failed to connect to orb %s\n", orbname );
	}

	fp = fopen( filename, "r" );

	while( ! at_eof( fp ) ) {

		swap = read_header( fp, &header );

		switch( header.struct_type ) {
		case STATIONCOMP:
			read_stationcomp( fp, header, &stationcomp, swap );
			break;
		case INSTRUMENT:
			read_instrument( fp, header, &instrument, swap );
			break;
		case DESCRIPTRACE:
			read_descriptrace( fp, header, &descriptrace, 
					   swap, &data, &bufsiz );
			send_to_orb( header, stationcomp, instrument,
					descriptrace, data, orbfd );
			break;
		default:
			fseek( fp, header.struct_length_bytes, SEEK_CUR );
			fseek( fp, header.data_length_bytes, SEEK_CUR );
			elog_complain( 1, "Unknown Suds-structure type %d, skipping\n",
				header.struct_type );
			break;
		}
	}

	fclose( fp );
}
