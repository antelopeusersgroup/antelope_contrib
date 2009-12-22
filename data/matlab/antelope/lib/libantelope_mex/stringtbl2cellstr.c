/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1999
 */

#include "antelope_mex.h"

mxArray *
stringtbl2cellstr( Tbl *tbl )
{
	mxArray *result;
	mxArray *mystring;
	char	*s;
	long	M;
	long	i;

	M = maxtbl( tbl );

	if( M <= 0 ) 
	{
		return 0;
	}
	else 
	{
		result = mxCreateCellMatrix( M, 1 );
	}

	for( i = 0; i < M; i++ )
	{
		s = gettbl( tbl, i );

		if( s == 0 ) 
		{
			mxDestroyArray( result );
			return 0;
		}

		mystring = mxCreateString( s );

		if( mystring == 0 )
		{
			mxDestroyArray( result );
			return 0;
		}

		mxSetCell( result, i, mystring );
	}

	return result;
}
