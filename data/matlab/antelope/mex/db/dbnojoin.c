/* 
 * Matlab interface to Datascope package
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1997
 */

#define USAGE "Error using ==> dbnojoin\n\n\
Usage: DBPTR = DBNOJOIN ( DBPTR1, DBPTR2 [, PATTERN1 [, PATTERN2]] )\n"

#include "antelope_mex.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	Dbptr	db, db1, db2;
	Tbl	*pattern1 = 0;
	Tbl	*pattern2 = 0;
	Tbl	**pattern1_p = 0;
	Tbl	**pattern2_p = 0;
	char	errmsg[STRSZ];
	int	rhs_index;

	if( nrhs < 2 || nrhs > 4  )
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

	if( nrhs == 3 )
	{
		if( ! ( pattern1 = cellstr2stringtbl( prhs[2] ) ) )
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

	if( nrhs == 4 )
	{
		if( ! ( pattern1 = cellstr2stringtbl( prhs[2] ) ) )
		{
			antelope_mexUsageMsgTxt ( USAGE );
			return;
		}
		else
		{
			pattern1_p = &pattern1;
		}

		if( ! ( pattern2 = cellstr2stringtbl( prhs[3] ) ) )
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
	db = dbnojoin( db1, db2, pattern1_p, pattern2_p, 0 );
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
		mexErrMsgTxt( "dbnojoin: join failed" );
	}

	plhs[0] = CreateDbptrStructFromDbptr( db );

	if( plhs[0] == NULL )
	{
		sprintf( errmsg, "dbnojoin: failed to create database-" );
		strcat( errmsg, "pointer structure for result" );
		mexErrMsgTxt( errmsg );
	}
}
