/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1999
 */

#define USAGE "Error using arrtimes\n\n\
Usage: [TIMES, PHASENAMES] = ARRTIMES ( DELTA, DEPTH )\n"

#include "antelope_mex.h"
#include "tttaup.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	double	delta;
	double	depth;
	char 	**phases = 0;
	double	*times = 0;
	double	*pr;
	int	narrivals;
	Tbl	*phasetbl;
	int	i;

	if( nrhs != 2  )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
        else if( ! get_scalar( prhs[0], &delta ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }
        else if( ! get_scalar( prhs[1], &depth ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }

	narrivals = arrtimes( delta, depth, &phases, &times );

	if( narrivals <= 0 ) 
	{
		return;
	}

	phasetbl = newtbl( narrivals );

	for( i = 0; i < narrivals; i++ ) 
	{
		pushtbl( phasetbl, phases[i] );
	}

	plhs[0] = mxCreateDoubleMatrix( narrivals, 1, mxREAL );
	pr = mxGetPr( plhs[0] );

	for( i = 0; i < narrivals; i++ ) 
	{
		pr[i] = times[i];
	}
	
	plhs[1] = stringtbl2cellstr( phasetbl );

	freetbl( phasetbl, 0 );
}
