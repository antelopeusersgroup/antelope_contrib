/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1997
 */

#define USAGE "Error using ==> trgetwf\n\n\
Usage: [DATA, NSAMP, T0, T1] = TRGETWF ( DBPTR [, REQS, REQE] )\n"

#include "antelope_mex.h"

static char *messages[] = { 
	"failed to read a database field",
	"unknown data type",
	"(pre-allocated) data buffer is too small to hold entire sample",
	"couldn't open the waveform file",
	"couldn't map the waveform file into memory",
	"the data file is too short",
	"some failure reading and interpreting the data",
	"couldn't allocate the required memory",
	"No intersection between requested time interval and data"
	};
static int Nmessages = 9;

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	Dbptr	db;
	float	*data = NULL;
	char	errmsg[STRSZ];
	long	nmax = 0;
	double	reqs, reqe;
	double	t0, t1;
	long	nrecs;
	long	nsamp;
	int	rc;
	double	*doublep;
	char	*string;
	int	i;

	if( nrhs != 1 && nrhs != 3  )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
        else if( ! get_dbptr( prhs[0], &db ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }

	if( nrhs == 1 )
	{
		reqs = 0;
		reqe = 0;
	}
	else
	{
		if( ! mtlb_get_string( prhs[1], &string ) )
		{
			if( ! get_scalar( prhs[1], &reqs ) )
			{
				antelope_mexUsageMsgTxt ( USAGE );
				return;
			}
		}
		else
		{
			reqs = str2epoch( string );
			mxFree( string );
		}
	
		if( ! mtlb_get_string( prhs[2], &string ) )
		{
			if( ! get_scalar( prhs[2], &reqe ) )
			{
				antelope_mexUsageMsgTxt ( USAGE );
				return;
			}
		}
		else
		{
			reqe = str2epoch( string );
			mxFree( string );
		}
	}

	if( db.record < 0 )
	{
		dbquery( db, dbRECORD_COUNT, &nrecs );
		if( nrecs == 1 )
		{
			db.record = 0;
		}
		else
		{
			mexErrMsgTxt( "trgetwf: Needs single database row" );
		}
	}
		
	rc = trgetwf( db, 0, &data, &nmax, reqs, reqe, &t0, &t1, &nsamp, 0, 0 );
	antelope_mex_clear_register( 1 );

	rc *= -1;
	if( rc == 0 )
	{
		/* Success */
	}
	else if( rc > Nmessages )
	{
		mexErrMsgTxt( "trgetwf: Unknown failure" );
	}
	else
	{
		sprintf( errmsg, "trgetwf: %s", messages[rc-1] );
		mexErrMsgTxt( errmsg );
	}

	plhs[0] = mxCreateDoubleMatrix( (mwSize) nsamp, 1, mxREAL );
	if( plhs[0] == NULL )
	{
		free( data );
		mexErrMsgTxt( "trgetwf: failed to allocate memory\n" );
	}

	doublep = (double *) mxGetPr( plhs[0] );

	for( i=0; i<nsamp; i++ )
	{
		doublep[i] = (double) data[i];
	}
	free( data );

	plhs[1] = CreateDouble( (double) nsamp );
	if( plhs[1] == NULL )
	{
		mxDestroyArray( plhs[0] );
		mexErrMsgTxt( "trgetwf: failed to create return value" );
	}

	plhs[2] = CreateDouble( t0 );
	if( plhs[2] == NULL )
	{
		mxDestroyArray( plhs[0] );
		mxDestroyArray( plhs[1] );
		mexErrMsgTxt( "trgetwf: failed to create return value" );
	}

	plhs[3] = CreateDouble( t1 );
	if( plhs[3] == NULL )
	{
		mxDestroyArray( plhs[0] );
		mxDestroyArray( plhs[1] );
		mxDestroyArray( plhs[2] );
		mexErrMsgTxt( "trgetwf: failed to create return value" );
	}
}
