/*
 * wave_client.c
 * 
 * Library routines for connecting to wave_server
 * Extensive Modification/Redesign of routines originally 
 * written by Will Kohler 
 *
 * Kent Lindquist, in consultation with Roger Hansen
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * May, 1996
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <limits.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <math.h>
#include "earthworm.h"
#include "util_ext.h"
#include "trace_buf.h"
#include "wave_client.h"

#define STREQ(a, b) (strcmp((a), (b)) == 0)

#define SLIP_TOLERANCE 0.5	/* Number of nominal sample intervals by which
				  the start time of a packet is allowed to
				  be different from that predicted from the 
				  endtime and sample rate of the previous
				  packet. Can be fractional. Violations
				  cause a gap to be declared. */

#define MAX_CMD_LEN  100  /* Max #chars in a command to the server */
#define ROUNDOFF_FACTOR 1000000 /* Round off starttime and endtimes to 
				      nearest 1/ROUNDOFF_FACTOR to clean up
				      sample rates */
		

static int Config_Init = 0;	/* 1=configuration completed */ 

char    WaveServer[24] = "";  /* IPAddress:Port for connecting to wave-server */
static char WaveServerIP[16];   /* WaveServer IP Address */
static char WaveServerPort[6];  /* WaveServer well-known port number */

/*
 * Internal functions
 */
static void wave_client_config( void );
static int wave_client_get_request( char *, double, double, char *, 
					DataPtr *, int *, TraceSegment ** );
static int wave_client_degap( int, char *, DataPtr *, int, TraceSegment **,
				DataPtr *, double *, int *, double * );

static int wave_client_buf_to_file( DataPtr *, char *, int, char *, int, int * );
int SocketConnect( int * );

/**********************************************************************
 * wave_client_config() set up wf_client library.                     *
 *    This routine expects the string WaveServer to be set by the     *
 *    calling routine to contain the IP address (#.#.#.#) of the      *
 *    machine running the wave-server and the well-known port number  *
 *    for connecting to the wave-server. The IP address and the port  *
 *    number must be separated by a colon.			      *
 **********************************************************************/
static void
wave_client_config( void )
{
	char	*ws_dup;
	char	*p;

	if( Config_Init ) return;

	if( STREQ( WaveServer, "" ) )
	{
		(void) fprintf( stderr,
			"wave_client_config: empty WaveServer Address/Port;");
		(void) fprintf( stderr, " exiting\n" );
		exit( -1 );
	}
	else 
	{
		ws_dup = (char *) strdup( WaveServer );
		p = (char *) strpbrk( ws_dup, ":" );
		if( p == (char *) NULL )
		{
			(void) fprintf( stderr,
			"wave_client_config: Error decoding Address/Port;");
			(void) fprintf( stderr, " exiting\n" );
			exit( -1 );
		}
		*p = '\0';
		(void) strcpy( WaveServerIP, ws_dup );
		(void) strcpy( WaveServerPort, ++p );
		free( ws_dup );
	}

	Config_Init = 1;
}

/**********************************************************************
 * wave_client_pin_to_file() Saves the requested data for a pin       *
 *    to a file.                                                      *
 **********************************************************************/
int
wave_client_pin_to_file(
	int	pin,		/* requested pin number */
	double  reqs,           /* requested start-time of waveforms */
	double  reqe,           /* requested end-time of waveforms */
	int	gapflag,	/* How to handle gaps in the data */
	char	*pathfile,	/* Where to write the file of waveform data */
	int	append,		/* flag to append to file if file exists */
	int	*foff,		/* returned byte-offset of data in file */
	double	*ts,		/* start time of first sample saved */
	int	*nsamp, 	/* number of samples saved */
	double	*samprate,	/* sample rate of saved data */
	char	*datatype)	/* datatype of saved data */
{
	DataPtr data;
	int	rc;
	int	ngap;

	rc = wave_client_pin_to_buf( pin, reqs, reqe, gapflag,
				ts, nsamp, samprate, datatype, &data );

	if( rc < 0 )
	{
		return( rc );
	}
	else
	{
		ngap = rc;
	}

	rc = wave_client_buf_to_file( &data, datatype, *nsamp, pathfile, 
					append, foff );

	if( rc < 0 )
	{
		free( data.c );
		return( rc );
	}
	else
	{
		free( data.c );
		return( ngap );
	}
}

/**********************************************************************
 * wave_client_stachan_to_file() Saves the requested station-channel  *
 *    to a file.                                                      *
 **********************************************************************/
int
wave_client_stachan_to_file(
        char    *sta,           /* requested station */
	char    *chan,          /* requested channel */
	char    *net,           /* requested network */
	double  reqs,           /* requested start-time of waveforms */
	double  reqe,           /* requested end-time of waveforms */
	int	gapflag,	/* How to handle gaps in the data */
	char	*pathfile,	/* Where to write the file of waveform data */
	int	append,		/* flag to append to file if file exists */
	int	*foff,		/* returned byte-offset of data in file */
	double	*ts,		/* start time of first sample saved */
	int	*nsamp, 	/* number of samples saved */
	double	*samprate,	/* sample rate of saved data */
	char	*datatype)	/* datatype of saved data */
{
	DataPtr data;
	int	rc;
	int	ngap;

	rc = wave_client_stachan_to_buf( sta, chan, net, reqs, reqe, gapflag,
				ts, nsamp, samprate, datatype, &data );

	if( rc < 0 )
	{
		return( rc );
	}
	else
	{
		ngap = rc;
	}

	rc = wave_client_buf_to_file( &data, datatype, *nsamp, pathfile, 
					append, foff );

	if( rc < 0 )
	{
		free( data.c );
		return( rc );
	}
	else
	{
		free( data.c );
		return( ngap );
	}
}

/**********************************************************************
 * wave_client_buf_to_file() Save a data buffer to a file. Append     *
 *   to an existing file if the append flag is set. Otherwise save    *
 *   to new file or overwrite old one as necessary.                   *
 **********************************************************************/
static int
wave_client_buf_to_file(
	DataPtr *data,		/* Data to save to file */
	char	*datatype,	/* type of data to save */
	int	nsamp,		/* number of data samples to save */
	char	*pathfile,	/* Where to write the file of waveform data */
	int	append,		/* flag to append to file if file exists */
	int	*foff)		/* returned byte-offset of data in file */
{
	FILE	*fp;		
	struct stat *filestats;
	int	dsampsize;
	long	databuflen;

	/* Open the file that will contain the trace data
	   **********************************************/
	if( append == 0 )
	{
		fp = fopen( pathfile, "wb" );
		if ( fp == NULL )
		{
			return( ERR_FILEIO );
		}
		*foff = 0;
	}
	else 
	{
		filestats = (struct stat *) malloc( sizeof( struct stat ) );

		fp = fopen( pathfile, "ab" );
		if ( fp == NULL )
		{
			return( ERR_FILEIO );
		}

		(void) fstat( fileno( fp ), filestats );

		*foff = filestats->st_size;

		free( filestats );
	}
	
	dsampsize = datasize( datatype );

	databuflen = nsamp * dsampsize;

	if( fwrite( data->c, sizeof( char ), databuflen, fp ) != databuflen )
	{
		(void) fclose( fp );
		return( ERR_FILEIO );
	}

	(void) fclose( fp );
	return( 0 );
}

/**********************************************************************
 * wave_client_pin_to_buf() Returns a buffer of data with gaps        *
 *   handled as requested. It is the caller's responsibility to free  *
 *   the memory for the returned data buffer. Returns the number of   *
 *   gaps that were filled.                                           *
 **********************************************************************/
int
wave_client_pin_to_buf(
	int	pin,		/* Requested pin number */
	double  reqs,           /* requested start-time of waveforms */
	double  reqe,           /* requested end-time of waveforms */
	int	gapflag,	/* how to handle gaps in the data */
	double	*ts,		/* start time of first sample returned */
	int	*nsamp,		/* number of samples returned */
	double	*samprate,	/* sample rate of returned data */
	char    *datatype,      /* datatype of the returned data */
	DataPtr *data)          /* pointer to the returned data */
{
	DataPtr	data_wkg;	/* Working area for data */
	TraceSegment *tseg;	/* TraceSegment accounting structures */
	int	nseg;		/* Number of data segments */
	int	rc;

	rc = wave_client_get_pin_segments( pin, reqs, reqe,
			datatype, &data_wkg, &nseg, &tseg );

	if( rc < 0 ) return( rc );

	rc = wave_client_degap( gapflag, datatype, &data_wkg, nseg,
				&tseg, data, ts, nsamp, samprate );
	
	return( rc );
}


/**********************************************************************
 * wave_client_stachan_to_buf() Returns a buffer of data with gaps    *
 *   handled as requested. It is the caller's responsibility to free  *
 *   the memory for the returned data buffer. Returns the number of   *
 *   gaps that were filled.                                           *
 **********************************************************************/
int
wave_client_stachan_to_buf(
        char    *sta,           /* requested station */
	char    *chan,          /* requested channel */
	char    *net,           /* requested network */
	double  reqs,           /* requested start-time of waveforms */
	double  reqe,           /* requested end-time of waveforms */
	int	gapflag,	/* how to handle gaps in the data */
	double	*ts,		/* start time of first sample returned */
	int	*nsamp,		/* number of samples returned */
	double	*samprate,	/* sample rate of returned data */
	char    *datatype,      /* datatype of the returned data */
	DataPtr *data)          /* pointer to the returned data */
{
	DataPtr	data_wkg;	/* Working area for data */
	TraceSegment *tseg;	/* TraceSegment accounting structures */
	int	nseg;		/* Number of data segments */
	int	rc;

	rc = wave_client_get_stachan_segments( sta, chan, net, reqs, reqe,
			datatype, &data_wkg, &nseg, &tseg );

	if( rc < 0 ) return( rc );

	rc = wave_client_degap( gapflag, datatype, &data_wkg, nseg,
				&tseg, data, ts, nsamp, samprate );
	
	return( rc );
}

/**********************************************************************
 * wave_client_get_stachan_segments() Get the data for a given        *
 *   station-channel from the wave-server.                            *
 *   It is the caller's responsibility to free the memory for the     *
 *   DataPtr and the TraceSegment structures.                         *
 **********************************************************************/
int
wave_client_get_stachan_segments( 
	char	*sta,		/* requested station */
	char	*chan,		/* requested channel */
	char	*net,		/* requested network */
	double	reqs,		/* requested start-time of waveforms */
	double	reqe,		/* requested end-time of waveforms */
	char	*datatype,	/* datatype of the returned data */
	DataPtr *data,		/* pointer to the returned data */
	int	*nseg,		/* Number of data-segments returned */
	TraceSegment **tseg)	/* Structures for the returned data segments */
{
	char	request[MAX_CMD_LEN];	/* Request message sent to wave_server */
	int	rc;
	char	request_net[9];

	if( net == NULL ) 
	{
		strcpy( request_net, NETWORK_NULL_STRING );
	}
	else if( strcmp( net, "" ) == 0 )
	{
		strcpy( request_net, NETWORK_NULL_STRING );
	}
	else
	{
		strcpy( request_net, net );
	}

	(void) sprintf( request, "SCNREQ: %s %s %s %.6f %.6f\n",
			sta, chan, request_net, reqs, reqe );

	rc = wave_client_get_request( request, reqs, reqe, 
				      datatype, data, nseg, tseg );

	return( rc );
}

/**********************************************************************
 * wave_client_get_pin_segments() Get the data for a given pin        *
 *   from the wave-server.		                              *
 *   It is the caller's responsibility to free the memory for the     *
 *   DataPtr and the TraceSegment structures.                         *
 **********************************************************************/
int
wave_client_get_pin_segments( 
	int	pin,		/* requested pin number */
	double	reqs,		/* requested start-time of waveforms */
	double	reqe,		/* requested end-time of waveforms */
	char	*datatype,	/* datatype of the returned data */
	DataPtr *data,		/* pointer to the returned data */
	int	*nseg,		/* Number of data-segments returned */
	TraceSegment **tseg)	/* Structures for the returned data segments */
{
	char	request[MAX_CMD_LEN];	/* Request message sent to wave_server */
	int	rc;

	(void) sprintf( request, "PINREQ: %d %.6f %.6f\n", pin, reqs, reqe );

	rc = wave_client_get_request( request, reqs, reqe,
				      datatype, data, nseg, tseg );

	return( rc );
}

/**********************************************************************
 * wave_client_degap() Handle gaps in data as instructed by gap-flag. *
 *    Frees the input DataPtr and TraceSegment structures. Malloc's   *
 *    memory for the output DataPtr, which must be freed by the       *
 *    caller. Nothing needs to be freed if an error condition is      *
 *    returned. Returns the number of gaps filled.		      *
 **********************************************************************/
static int
wave_client_degap(
	int	gapflag,	/* Flag for how to handle gaps */
	char	*datatype,	/* datatype of the input data */
	DataPtr *data_in,	/* pointer to the input data */
	int	nseg,		/* Number of data-segments input */
	TraceSegment **tseg,	/* Structures for the input data segments */
	DataPtr *data_out,	/* Returned de-gapped data */
	double	*starttime,	/* Starttime of returned data */
	int	*nsamp_out,	/* Number of returned data samples */
	double	*samprate)	/* Sample rate of returned data */
{
	char	*dataptr;
	int	dsampsize;
	int	seg_index;
	int	nsamp_in = 0;
	int	nmissing = 0;
	int	newsize;
	int	ncopy;
	char	*fill_value;
	short	fill_short;
	int	fill_int;
	float	fill_float;
	int	i;
	long long starttime_nsegm1_ll;
	long long starttime_0_ll;

	data_out->c = (char *) NULL;
	*nsamp_out = 0;

	if( nseg == 1 )
	{
		data_out->c = data_in->c;
		*starttime = (*tseg)[0].starttime;
		*nsamp_out = (*tseg)[0].nsamp;
		*samprate = (*tseg)[0].samprate;
		free( *tseg );
		return( 0 );
	}

	if( gapflag == GAP_FAIL )
	{
		free( data_in->c );
		free( *tseg );
		return( ERR_GAP );	
	}

	for( seg_index = 1; seg_index < nseg; seg_index++)
	{
		if( (*tseg)[seg_index-1].endtime >= (*tseg)[seg_index].starttime )
		{
			(void) fprintf( stderr,
			    "Overlapping or out-of-order data segments in wave_client_degap\n");
			free( data_in->c );
			free( *tseg );
			return( ERR_GAP );	
		}
	}

	switch( gapflag )
	{
		case GAP_FILL_ZEROS:
			fill_short = 0;
			fill_int = 0;
			fill_float = 0.;
			break;
		case GAP_FILL_MAX:
			fill_short = SHRT_MAX;
			fill_int = INT_MAX;
			fill_float = FLT_MAX;
			break;
		default:			
			/* Unknown gap flag */
			(void) fprintf(stderr,
				"Unknown gap flag in wave_client_degap\n");
			free( data_in->c );
			free( *tseg );
			return( ERR_GAP );
	}
	
	if( STREQ( datatype, "s2" ) )
	{
		fill_value = (char *) &fill_short;
	} 
	else if( STREQ( datatype, "s4" ) )
	{
		fill_value = (char *) &fill_int;
	} 
	else if( STREQ( datatype, "t4" ) )
	{
		fill_value = (char *) &fill_float;
	}
	else
	{
		free( data_in->c );
		free( *tseg );
		return( ERR_DATATYPE );
	}

	dsampsize = datasize( datatype );

	for( seg_index = 0; seg_index < nseg; seg_index++)
	{
		nsamp_in += (*tseg)[seg_index].nsamp;
	}

	for( seg_index = 1; seg_index < nseg; seg_index++)
	{
		nmissing += ( (*tseg)[seg_index].starttime -
			      (*tseg)[seg_index-1].endtime ) * 
			         (*tseg)[seg_index-1].samprate + 0.5 - 1;
	}
	
	newsize = (nsamp_in + nmissing) * dsampsize;

	*nsamp_out = (nsamp_in + nmissing);

	data_out->c = (char *) malloc( newsize * sizeof( char ) );
	if( data_out->c == (char *) NULL )
	{
		free( data_in->c );
		free( *tseg );
		return( ERR_ALLOC );
	}
	
	dataptr = data_out->c;
	for( seg_index = 0; seg_index < nseg - 1; seg_index++)
	{
		ncopy = (*tseg)[seg_index].nsamp * dsampsize;
		(void) memcpy( dataptr, (*tseg)[seg_index].dp.c, ncopy );
		dataptr += ncopy;
		nmissing = ( (*tseg)[seg_index+1].starttime -
			      (*tseg)[seg_index].endtime ) * 
			         (*tseg)[seg_index].samprate + 0.5 - 1;

		for(i = 0; i < nmissing; i++ )
		{
			(void) memcpy( dataptr, fill_value, dsampsize );
			dataptr += dsampsize;
		}
	}

	ncopy = (*tseg)[nseg - 1].nsamp * dsampsize;
	(void) memcpy( dataptr, (*tseg)[nseg - 1].dp.c, ncopy );

	*starttime = (*tseg)[0].starttime;
	starttime_nsegm1_ll = ROUNDOFF_FACTOR * (*tseg)[nseg - 1].starttime;
	starttime_0_ll = ROUNDOFF_FACTOR * (*tseg)[0].starttime;
	*samprate = ( *nsamp_out - (*tseg)[nseg - 1].nsamp ) / 
			( (double) ( starttime_nsegm1_ll - starttime_0_ll ) );
	*samprate *= ROUNDOFF_FACTOR;

	free( data_in->c );
	free( *tseg );

	return( nseg - 1 );
}

/**********************************************************************
 * wave_client_get_request() Get the data for a given timespan and    *
 *   pin or stachan from the wave-server. Data are returned with an   *
 *   array of structures dividing the data into segments based on the *
 *   locations of any gaps in the data returned by the wave-server.   *
 *   It is the caller's responsibility to free the memory associated  *
 *   with the returned data-pointer and the TraceSegment structures.  *
 *   Note that the data-pointers in the TraceSegment structures merely*
 *   point into the main data, and do not themselves have memory      *
 *   that needs to be freed. Nothing needs to be freed by the caller  *
 *   if an error condition is returned. Returns the number of gaps    *
 *   in the data.						      *
 *   This routine is the basic block out of which other wave_client   *
 *   functions are built.			                      *
 **********************************************************************/
static int
wave_client_get_request( 
	char	*request,	/* request message to send to wave-server */
	double	reqs,		/* starttime of request */
	double	reqe,		/* endtime of request */
	char	*datatype,	/* datatype of the returned data */
	DataPtr *data,		/* pointer to the returned data */
	int	*nseg,		/* Number of data-segments returned */
	TraceSegment **tseg)	/* Structures for the returned data segments */
{
	int	length;		/* length of request message */

	int	ndatabuf;	/* Number of trace packets being sent */
	int	nbufrec;	/* Number of trace packets received */
	int	ntossed = 0;	/* Number of trace packets thrown out */
	char	*dataptr;	/* Pointer for walking around in returned-data area*/

	TRACE_HEADER trh;	/* Header info on a given packet */
	char	*tp_buf;	/* Buffer for data from a given Trace packet */
	char	*tp_bufptr;	/* pointer for walking around in *tp_buf */
	long	databuflen;	/* Total length (bytes) of trace-packet coming in */
	int	nbufsamp;	/* Number of data samples in a given packet */
	size_t	dsampsize;	/* Size of a single data sample */

	double	next_expected;	/* expected beginning time of next trace packet */
	int	sd;		/* Socket descriptor */
	int	rc;		/* Socket call return code */
	int	ntoget;		/* Number of bytes to read from socket */
	int	nr;		/* Number of bytes received from socket */

	long long starttime_ll;	/* starttime for roundoff-error avoidance */
	long long endtime_ll;	/* endtime for roundoff-error avoidance */

	wave_client_config();

	*nseg = 0;
	data->c = (char *) NULL;
	*tseg = (TraceSegment *) NULL;

	rc = SocketConnect( &sd );
	if( rc < 0 ) return( rc );

	tp_buf = (char *) malloc( (size_t) MAX_TRACEBUF_SIZ );
	if( tp_buf == (char *) NULL )
	{
		return( ERR_ALLOC );
	}

	/* Build and send request to the wave-server 
	   *****************************************/
	length = (int) strlen( request );
	if ( send( sd, request, length, 0 ) == -1 )
	{
		SocketPerror( "Error sending request" );
		SocketClose( sd );
		free( tp_buf );
		return( ERR_SOCKET );
	}

	/* Start reading the socket. The first five characters contain
	   the total number of data buffers to be shipped. 
	   ***********************************************/
	nbufrec = 0;

	ntoget = 5;
	tp_bufptr = tp_buf;
	while( ntoget > 0 )
	{
		nr = recv( sd, tp_bufptr, ntoget, 0 );
		if( nr == -1 )
		{
			SocketPerror( "Error reading socket" );
			SocketClose( sd );
			free( tp_buf );
			return( ERR_SOCKET );
		}
		tp_bufptr += nr;
		ntoget -= nr;
	}
	tp_buf[4] = '\0';
	ndatabuf = atoi( tp_buf );
	if( ndatabuf == 0 )
	{
		SocketClose( sd );
		free( tp_buf );
		return( ERR_NODATA );
	}

	do
	{
		/* Get a data buffer header from the server
	   	   ****************************************/
		ntoget = sizeof( TRACE_HEADER );
		tp_bufptr = tp_buf;

		while( ntoget > 0 )
		{
			nr = recv( sd, tp_bufptr, ntoget, 0 );
			if( nr == -1 )
			{
				SocketPerror( "Error reading socket" );
				SocketClose( sd );
				if( data->c != (char *) NULL )
				{
					free( data->c );
					free( *tseg );
					*nseg = 0;
				}
				free( tp_buf );
				return( ERR_SOCKET );
			}
			tp_bufptr += nr;
			ntoget -= nr;
		}

		(void) memcpy( &trh, tp_buf, sizeof( TRACE_HEADER ) );

		/* Catch extraneous packets and throw them away */
		if( trh.endtime < reqs || trh.starttime > reqe )
		{
			ntossed++;

			if( ndatabuf - ntossed == 0 )
			{
				SocketClose( sd );
				free( tp_buf );
				return( ERR_NODATA );
			}

			/* Grab the data and throw them away */
			ntoget = trh.nsamp * datasize( trh.datatype );
			tp_bufptr = tp_buf + sizeof( TRACE_HEADER );
			while( ntoget > 0 )
			{
				nr = recv( sd, tp_bufptr, ntoget, 0 );
				if ( nr == -1 )
				{
					SocketPerror( "Error reading socket" );
					SocketClose( sd );
					free( *tseg );
					free( data->c );
					free( tp_buf );
					return( ERR_SOCKET );
				}
				tp_bufptr += nr;
				ntoget -= nr;
			}

			continue;
		}

		/* Update the accounting info for division of the incoming
		   data into unbroken segments 
		   *******************************************************/
		if( *nseg == 0 )
		{
			(void) strcpy( datatype, trh.datatype );
			dsampsize = datasize( datatype );
			nbufsamp = trh.nsamp;

			databuflen = (long) nbufsamp * dsampsize +
						sizeof( TRACE_HEADER );
			if( databuflen > MAX_TRACEBUF_SIZ )
			{
				SocketClose( sd );
				free( tp_buf );
				return( ERR_OVRFLW );
			}

			data->c = (char *)malloc( ndatabuf * nbufsamp * dsampsize );
			if( data->c == (char *) NULL )
			{
				SocketClose( sd );
				free( tp_buf );
				return( ERR_ALLOC );
			}
			else
			{
				dataptr = data->c;
			}

			*tseg = (TraceSegment *) malloc( sizeof( TraceSegment ) );
			if( *tseg == NULL )
			{
				SocketClose( sd );
				free( data->c );
				free( tp_buf );
				return( ERR_ALLOC );
			}

			(*tseg)[0].starttime = trh.starttime;
			(*tseg)[0].nsamp = trh.nsamp;
			(*tseg)[0].dp.c = data->c;

			next_expected = trh.endtime + 1./trh.samprate;

			(*nseg)++;
		}
		else
		{
			if( fabs( trh.starttime - next_expected ) <=
					SLIP_TOLERANCE / trh.samprate )
			{
				(*tseg)[*nseg-1].nsamp += trh.nsamp;
				next_expected = trh.endtime + 1./trh.samprate;
			}
			else 
			{
				/* Close out this segment and start a new one */

				/* Use current trh.samprate, leaning on our
				   assumption that it is unchanging */
				(*tseg)[*nseg-1].endtime = next_expected -
								1./trh.samprate;

				/* The unrounded samprate calculation, left
				   for reference: */
				/* (*tseg)[*nseg-1].samprate =		   */
				/*   ( (*tseg)[*nseg-1].nsamp - 1 ) /	   */
				/*         ( (*tseg)[*nseg-1].endtime -	   */
				/*           (*tseg)[*nseg-1].starttime ); */

				/* Round off starttime and endtime for
				   samprate calculation: cleans up sample rates
				   for data streams with clean sample rates
				   and packet starttimes */

				starttime_ll = ROUNDOFF_FACTOR *
						(*tseg)[*nseg-1].starttime;
				endtime_ll = ROUNDOFF_FACTOR *
						(*tseg)[*nseg-1].endtime;
				(*tseg)[*nseg-1].samprate =
				   ( (*tseg)[*nseg-1].nsamp - 1 ) /
					(double) (endtime_ll - starttime_ll);
				(*tseg)[*nseg-1].samprate *= ROUNDOFF_FACTOR;

				(*nseg)++;

				*tseg = (TraceSegment *) realloc( *tseg, 
				   *nseg * sizeof( TraceSegment ) );
				
				(*tseg)[*nseg-1].starttime = trh.starttime;
				(*tseg)[*nseg-1].nsamp = trh.nsamp;
				(*tseg)[*nseg-1].dp.c = dataptr;

				next_expected = trh.endtime + 1./trh.samprate;
			}
		}

		/* Read samples for one buffer from the server
		   *******************************************/
		ntoget = databuflen - sizeof( TRACE_HEADER );
		tp_bufptr = tp_buf + sizeof( TRACE_HEADER );
		while( ntoget > 0 )
		{
			nr = recv( sd, tp_bufptr, ntoget, 0 );
			if ( nr == -1 )
			{
				SocketPerror( "Error reading socket" );
				SocketClose( sd );
				free( *tseg );
				free( data->c );
				free( tp_buf );
				return( ERR_SOCKET );
			}
			tp_bufptr += nr;
			ntoget -= nr;
		}

		tp_bufptr = tp_buf + sizeof( TRACE_HEADER );
		(void) memcpy( dataptr, tp_bufptr, nbufsamp * dsampsize );
		dataptr += nbufsamp * dsampsize;
		
	} while( ++nbufrec < ndatabuf );

	/* Close out accounting info for the current TraceSegment
	   ******************************************************/
	(*tseg)[*nseg-1].endtime = trh.starttime + ( trh.nsamp - 1 ) / trh.samprate;

	/* Round off starttime and endtime for
	   samprate calculation: cleans up sample rates
	   for data streams with clean sample rates */

	starttime_ll = ROUNDOFF_FACTOR * (*tseg)[*nseg-1].starttime;
	endtime_ll = ROUNDOFF_FACTOR * (*tseg)[*nseg-1].endtime;
	(*tseg)[*nseg-1].samprate = ( (*tseg)[*nseg-1].nsamp - 1 ) /
					(double) (endtime_ll - starttime_ll);
	(*tseg)[*nseg-1].samprate *= ROUNDOFF_FACTOR; 

	SocketClose( sd );
	free( tp_buf );

	return( *nseg - 1 );
}

/**********************************************************************
 * wave_client_get_menu() Inquires about the data the wave-server has *
 *   available. The menu structures returned must be freed by the     *
 *   caller unless an error is returned.			      *
 **********************************************************************/
int
wave_client_get_menu( int *nstachans, WSEntry **menu )
{
	char	request[MAX_CMD_LEN];	/* Request to wave-server */
	int	length;
	char	buf[MAX_CMD_LEN];	/* Buffer for returned info */
	int	nrec;			/* Number of bytes received */
	int	i;
	char	c;
	int	rc;
	int	sd;		/* Socket descriptor */
	int	row_index;

	wave_client_config();

	rc = SocketConnect( &sd );
	if( rc < 0 ) return( rc );

	(void) sprintf( request, "INQ:\n\0" );
	length = (int) strlen( request );
	if( send( sd, request, length, 0 ) == -1 )
	{
		SocketPerror( "Error sending inquiry" );
		SocketClose( sd );
		return( ERR_SOCKET );
	}

	/* Get number of rows coming
	   *************************/
	nrec = recv( sd, buf, 5, 0 );
	if( nrec != 5 )
	{
		SocketClose( sd );
		return( ERR_SOCKET );
	}
	buf[4] = '\0';
	*nstachans = atoi( buf );

	*menu = (WSEntry *) malloc( *nstachans * sizeof( WSEntry ) );
	if( *menu == (WSEntry *) NULL )
	{
		SocketClose( sd );
		return( ERR_ALLOC );
	}

	for( row_index = 0; row_index < *nstachans; row_index++)
	{
		nrec = 0;
		do
		{
			if( nrec == MAX_CMD_LEN )
			{
				SocketClose( sd );
				free( *menu );
				return( ERR_OVRFLW );
			}
			i = recv( sd, &c, 1, 0 );
			if( i == 1 )
			{
				buf[nrec++] = c;
			}
			else if( i == -1 )
			{
				SocketPerror( "Socket receive error" );
				SocketClose( sd );
				free( *menu );
				return( ERR_SOCKET );
			}
		} while( c != '\n' );

		if( sscanf( buf, "%d %s %s %s %lf %lf",
			&(*menu)[row_index].pin,
			(*menu)[row_index].sta,
			(*menu)[row_index].chan,
			(*menu)[row_index].net,
			&(*menu)[row_index].tstart,
			&(*menu)[row_index].tend) < 6 )
		{
			(*menu)[row_index].pin = -1;
			(void) strcpy( (*menu)[row_index].sta, "-" );
			(void) strcpy( (*menu)[row_index].chan, "-" );
			(void) strcpy( (*menu)[row_index].net, "-" );
			(*menu)[row_index].tstart = 9999999999.999;
			(*menu)[row_index].tend = 9999999999.999;
		}
	}

	SocketClose( sd );
	return( 0 );
}

/**********************************************************************
 * wave_client_buf_to_float() Convert a buffer of waveform data to    *
 *  necessary.                                                        *
 **********************************************************************/
int
wave_client_buf_to_float( char *datatype, int nsamp, DataPtr *data )
{
	DataPtr	wkg_data;
	int	index;

	if( STREQ( datatype, "t4" ) ) return( 0 );

	wkg_data.c = malloc( nsamp * sizeof( float ) );
	if( wkg_data.c == NULL )
	{
		return( ERR_ALLOC );
	}

	if( STREQ( datatype, "s2" ) )
	{
		for( index = 0; index < nsamp; index++ )
		{
			wkg_data.t4[index] = (float) data->s2[index];
		}
	}
	else if( STREQ( datatype, "s4" ) )
	{
		for( index = 0; index < nsamp; index++ )
		{
			wkg_data.t4[index] = (float) data->s4[index];
		}
	}
	else
	{
		free( wkg_data.c );
		return( ERR_DATATYPE );
	}
	free( data->c );
	data->c = wkg_data.c;
	return( 0 );
}

/**********************************************************************
 * SocketConnect() Connect to the wave-server via a socket. Return    *
 *   zero on success. 						      *
 **********************************************************************/
int
SocketConnect( int *sd )
{
	u_long  addr;		/* Wave-server address */
	struct hostent *hp;	/* Host address info */
	char    hp_buffer[1024]; /* buffer for info pointed to by *hp members */
	int     hp_buflen = 1024;
	int     h_errnop;	/* Errno for gethostbyname_r call */
	struct sockaddr_in name; /* Socket address info */

	/* Set up socket
	****************/
	if( (int) ( addr = inet_addr( WaveServerIP ) ) == -1 )
	{
		return( ERR_NOHOST );
	}
	
	hp = (struct hostent *) malloc( sizeof( struct hostent ) );

	gethostbyaddr_r( (char *) &addr, sizeof( addr ), AF_INET,
			hp, hp_buffer, hp_buflen, &h_errnop );

	if( hp == (struct hostent *) NULL )
	{
		return( ERR_NOHOST );
	}

	(void) memset( (char *) &name, '\0', sizeof(name) );
	name.sin_family = AF_INET;
	name.sin_port = htons( (unsigned short) atoi( WaveServerPort ) );
	(void) memcpy( (char *) &name.sin_addr, hp->h_addr_list[0],
					sizeof( name.sin_addr ) );

	if ( ( *sd = socket( AF_INET, SOCK_STREAM, 0 ) ) == -1 )
	{
		SocketPerror( "Socket call failed" );
		free( hp );
		return( ERR_SOCKET );
	}

	if ( connect( *sd, (struct sockaddr *)&name, sizeof( name ) ) == -1 )
	{
		SocketPerror( "Connect to host failed" );
		SocketClose( *sd );
		free( hp );
		return( ERR_SOCKET );
	}

	free( hp );

	return( 0 );
}
