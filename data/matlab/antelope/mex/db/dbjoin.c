/* 
 * Matlab interface to Datascope package
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1997
 */

#define USAGE "Error using ==> dbjoin\n\n\
Usage: DBPTR = DBJOIN ( DBPTR1, DBPTR2 [, PATTERN1 [, PATTERN2]] [, 'outer'] )\n"

#include "antelope_mex.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	Dbptr	db, db1, db2;
	Tbl	*pattern1 = 0;
	Tbl	*pattern2 = 0;
	Tbl	**pattern1_p = 0;
	Tbl	**pattern2_p = 0;
	char	errmsg[STRSZ];
	char	*outer;
	int	outer_join = 0;
	int	rhs_index;

	if( nrhs < 2 || nrhs > 5  )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
        else if( ! get_dbptr( prhs[0], &db1 ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }
        else if( ! get_dbptr( prhs[1], &db2 ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }

	if( nrhs >= 3 && mxIsChar( prhs[nrhs-1] ) )
	{
		if( ! get_string( prhs[nrhs-1], &outer ) )
		{
			antelope_mexUsageMsgTxt ( USAGE );
			return;
		}
		else if( ! STREQ( outer, "outer" ) )
		{
			mxFree( outer );
			antelope_mexUsageMsgTxt ( USAGE );
			return;
		}
		else
		{
			mxFree( outer );
			outer_join = 1;
		}
	}

	if( ( nrhs == 3 && ! outer_join ) ||
	    ( nrhs == 4 &&   outer_join ) )
	{
		rhs_index = nrhs - outer_join - 1;

		if( ! ( pattern1 = cellstr2stringtbl( prhs[rhs_index] ) ) )
		{
			antelope_mexUsageMsgTxt ( USAGE );
			return;
		}
		else
		{
			pattern2 = pattern1;
			pattern1_p = &pattern1;
			pattern2_p = &pattern2;
		}
	}

	if( ( nrhs == 4 && ! outer_join ) ||
	    ( nrhs == 5 &&   outer_join ) )
	{
		rhs_index = nrhs - outer_join - 2;

		if( ! ( pattern1 = cellstr2stringtbl( prhs[rhs_index] ) ) )
		{
			antelope_mexUsageMsgTxt ( USAGE );
			return;
		}
		else
		{
			pattern1_p = &pattern1;
		}

		rhs_index = nrhs - outer_join - 1;

		if( ! ( pattern2 = cellstr2stringtbl( prhs[rhs_index] ) ) )
		{
			if( pattern1 ) freetbl( pattern1, 0 );
			antelope_mexUsageMsgTxt ( USAGE );
			return;
		}
		else
		{
			pattern2_p = &pattern2;
		}
	}
	db = dbjoin( db1, db2, pattern1_p, pattern2_p, outer_join, 0, 0 );
	antelope_mex_clear_register( 1 );

	if( pattern2 && pattern2 != pattern1 )
	{
		freetbl( pattern2, 0 );
	}

	if( pattern1 )
	{
		freetbl( pattern1, 0 );
	}

	if( db.table == dbINVALID )
	{
		mexErrMsgTxt( "dbjoin: join failed" );
	}

	plhs[0] = CreateDbptrStructFromDbptr( db );

	if( plhs[0] == NULL )
	{
		sprintf( errmsg, "dbjoin: failed to create database-pointer " );
		strcat( errmsg, "structure for result" );
		mexErrMsgTxt( errmsg );
	}
}
