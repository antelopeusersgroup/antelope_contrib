/*
 * wave_client.h
 * 
 * Library routines to request data from the wave_server
 *
 * Extensive modification of Will Kohler's original
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * May, 1996
 *
 */

#ifndef WAVE_CLIENT_H
#define WAVE_CLIENT_H

extern char WaveServer[];	/* IPAddress:Port for connecting to wave-server */

/* Typedefs for wf_client routines
**********************************/
typedef union {
	char	*c;
	short   *s2;
	int     *s4;
	float   *t4;
} DataPtr;

typedef struct {
	double	starttime;	/* time of first sample in waveform segment */
	double	endtime;	/* time of last sample in waveform segment */
	double	samprate;	/* Averaged sample rate in waveform segment */
	int	nsamp;		/* Number of samples in waveform segment */
	DataPtr dp;		/* Pointer to data in this waveform segment */
} TraceSegment;			/* Section of contiguous waveform data */

typedef struct {
	int	pin;		/* Earthworm Pin number */
	char	sta[7];		/* Station name */
	char	chan[9];	/* Channel name */
	char	net[9]; 	/* Channel name */
	double	tstart;		/* Start time of tank */
	double	tend;		/* Start time of latest buffer in tank */
} WSEntry;	/* Wave-server entry: info on data the wave-server has stored */

/* Prototypes for functions in wf_client.c
******************************************/
int wave_client_get_stachan_segments( char *, char *, char *, double, double,
					char *, DataPtr *, int *,
					TraceSegment ** );
int wave_client_get_pin_segments( int, double, double, char *, DataPtr *,
					int *, TraceSegment ** );
int wave_client_stachan_to_buf( char *, char *, char *, double, double,
				int, double *, int *, double *, char *, 
				DataPtr * );
int wave_client_pin_to_buf( int, double, double, int, double *, int *,
				double *, char *, DataPtr * );
int wave_client_buf_to_float( char *, int, DataPtr * );
int wave_client_stachan_to_file( char *, char *, char *, double, double, int, 
				char *, int, int *, double *, int *, 
				double *, char * );
int wave_client_pin_to_file( int, double, double, int, 
				char *, int, int *, double *, int *, 
				double *, char * );
int wave_client_get_menu( int *, WSEntry ** );

/* Error codes returned by functions in wave_client.c library: 
   ***********************************************************/
#define ERR_ALLOC     -1       /* error allocating memory buffer        */
#define ERR_NOHOST    -2       /* host name unknown                     */
#define ERR_SOCKET    -3       /* error with socket connect, send, recv */
#define ERR_FILEIO    -4       /* error opening or writing to file      */
#define ERR_NODATA    -5       /* data requested is not in tank         */
#define ERR_OVRFLW    -6       /* data buffer length > BufferSize       */
#define ERR_GAP       -7       /* gap in data 				*/ 
#define ERR_DATATYPE  -8       /* Unknown data type                     */

/* Flags for instructions on response to gaps in data
   **************************************************/
#define GAP_FAIL       1
#define GAP_FILL_ZEROS 2
#define GAP_FILL_MAX   3

#endif
