/* 
 * Antelope Toolbox for Matlab
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
	char	*firststring = 0;
	char	*middlestring = 0;
	char	*laststring = 0;
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

	if( nrhs < 3 ) 
	{
		/* Fallthrough: no extra arguments */
	}
	else if( nrhs == 3 ) 
	{
		if( mtlb_get_string( prhs[2], &laststring ) )
		{
			if( STREQ( laststring, "outer" ) )
			{
				outer_join = 1;
			}
			else 
			{
				pattern1 = strtbl( laststring, NULL );

				pattern2 = pattern1;
				pattern1_p = &pattern1;
				pattern2_p = &pattern2;
			}
		}
		else if( get_stringtbl( prhs[2], &pattern1 ) )
		{
			pattern2 = pattern1;
			pattern1_p = &pattern1;
			pattern2_p = &pattern2;
		}
		else
		{
			antelope_mexUsageMsgTxt ( USAGE );
			return;
		}
	} 
	else if( nrhs == 4 ) 
	{ 
		if( mtlb_get_string( prhs[3], &laststring ) )
		{
			if( STREQ( laststring, "outer" ) )
			{
				outer_join = 1;
			}
			else 
			{
				pattern2 = strtbl( laststring, NULL );

				pattern2_p = &pattern2;
			}

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

			if( outer_join )
			{
				pattern2 = pattern1;
				pattern2_p = &pattern2;
			}
		}
		else if( get_stringtbl( prhs[3], &pattern2 ) )
		{
			pattern2_p = &pattern2;

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
		}
		else
		{
			antelope_mexUsageMsgTxt ( USAGE );
			return;
		}
	}
	else if( nrhs == 5 ) 
	{
		if( ! mtlb_get_string( prhs[4], &laststring ) )
		{
			antelope_mexUsageMsgTxt ( USAGE );
			return;
		}
		else if( ! STREQ( laststring, "outer" ) )
		{
			mxFree( laststring );
			antelope_mexUsageMsgTxt ( USAGE );
			return;
		}
		else
		{
			outer_join = 1;
		}

		if( mtlb_get_string( prhs[3], &middlestring ) )
		{
			pattern2 = strtbl( middlestring, NULL );
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
	}
	else
	{
		/* Admittedly redundant */
                antelope_mexUsageMsgTxt ( USAGE );
		return;
	}

	db = dbjoin( db1, db2, pattern1_p, pattern2_p, outer_join, 0, 0 );
	antelope_mex_clear_register( 1 );

	if( firststring ) mxFree( firststring );
	if( middlestring ) mxFree( middlestring );
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
