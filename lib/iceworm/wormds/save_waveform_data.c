#include <stdlib.h>
#include <stdio.h>
#include "wave_client.h"
#include "iceworm_extensions.h"
 
#define STREQ(a, b) (strcmp((a), (b)) == 0)

/************************************************************************
 * save_waveform_data() save waveform data to the specified file pointer.*
 ***********************************************************************/
int
save_waveform_data( char *format, FILE *fp, int nsamp, char *datatype, DataPtr data )
{
	int	dsize;
	DataPtr dcopy;
	int	rc;

	if( STREQ( format, "Raw" ) )
	{
		dsize = datasize( datatype );
		return fwrite( data.c, sizeof( char ), nsamp * dsize, fp );
	}
	else if( STREQ( format, "AlaskanAH" ) )
	{
		dsize = datasize( datatype );
		return fwrite( data.c, sizeof( char ), nsamp * dsize, fp );
	}
	else if( STREQ( format, "SAC" ) )
	{
		if( STREQ( datatype, "t4" ) )
		{
			dsize = datasize( datatype );
			return fwrite( data.c, sizeof( char ), nsamp * dsize, fp );
		}
		else
		{
			dsize = datasize( datatype );
			dcopy.c = malloc( nsamp * dsize );
			memcpy( dcopy.c, data.c, nsamp * dsize );
			wave_client_buf_to_float( datatype, nsamp, &dcopy );
			dsize = datasize( "t4" );
			rc = fwrite( data.c, sizeof( char ), nsamp * dsize, fp );
			free( dcopy.c );
			return rc;
		}
	}
	else
	{
		fprintf( stderr, "Data format %s not supported\n", format );
		return( -1 );
	}
}
