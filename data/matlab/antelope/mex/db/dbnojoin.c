/* 
 * Antelope Toolbox for Matlab
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
	char	*firststring = 0;
	char	*laststring = 0;
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
		if( mtlb_get_string( prhs[2], &firststring ) )
		{
			pattern1 = strtbl( firststring, NULL );
			pattern1_p = &pattern1;
		} 
		else if( get_stringtbl( prhs[2], &pattern1 ) )
		{
			pattern1_p = &pattern1;
		}
		else
		{
			antelope_mexUsageMsgTxt ( USAGE );
			return;
		}

		pattern2 = pattern1;
		pattern2_p = &pattern2;
	}

	if( nrhs == 4 )
	{
		if( mtlb_get_string( prhs[2], &firststring ) )
		{
			pattern1 = strtbl( firststring, NULL );
			pattern1_p = &pattern1;
		} 
		else if( get_stringtbl( prhs[2], &pattern1 ) )
		{
			pattern1_p = &pattern1;
		}
		else
		{
			antelope_mexUsageMsgTxt ( USAGE );
			return;
		}

		if( mtlb_get_string( prhs[3], &laststring ) )
		{
			pattern2 = strtbl( laststring, NULL );
			pattern2_p = &pattern2;
		} 
		else if( get_stringtbl( prhs[3], &pattern2 ) )
		{
			pattern2_p = &pattern2;
		}
		else
		{
			antelope_mexUsageMsgTxt ( USAGE );
			return;
		}
	}

	db = dbnojoin( db1, db2, pattern1_p, pattern2_p, 0 );
	antelope_mex_clear_register( 1 );
	
	if( firststring ) mxFree( firststring );
	if( laststring ) mxFree( laststring );

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
