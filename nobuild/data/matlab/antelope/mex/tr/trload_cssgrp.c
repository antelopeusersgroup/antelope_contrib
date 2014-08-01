/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1997
 */

#define USAGE "Error using ==> trload_cssgrp\n\n\
Usage: TRPTR = TRLOAD_CSSGRP ( DBPTR, TIME, ENDTIME [, TRPTR] [, TABLE] )\n"

#include "antelope_mex.h"
#include "tr.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	Dbptr	db;
	Dbptr	tr;
	int	type;
	char	*table = NULL;
	char	*time_str = NULL, *endtime_str = NULL;
	double	scalar;
	double	*doublep;
	float	*floatp;
	int	*intp;
	char	*s;
	char	errmsg[STRSZ];
	int	rc;

	if( nrhs < 3 || nrhs > 5 )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
        else if( ! get_dbptr( prhs[0], &db ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }
	else if( nrhs == 4 )
	{
		if( ! mtlb_get_string( prhs[3], &table ) )
		{
			if( ! get_dbptr( prhs[3], &tr ) )
			{
				antelope_mexUsageMsgTxt ( USAGE );
				return;
			}
		}
	}
	else if( nrhs == 5 )
	{
		if( ! get_dbptr( prhs[3], &tr ) )
		{
			antelope_mexUsageMsgTxt ( USAGE );
			return;
		}

		if( ! mtlb_get_string( prhs[4], &table ) )
		{
			antelope_mexUsageMsgTxt ( USAGE );
			return;
		}
	}

	if( ! mtlb_get_string( prhs[1], &time_str ) )
	{
		if( ! get_scalar( prhs[1], &scalar ) )
		{
			if( table ) mxFree( table );
                	antelope_mexUsageMsgTxt ( USAGE );
			return;
		}
		else
		{
			s = epoch2str( scalar, "%E\0" );
			antelope_mex_clear_register( 1 );
			
			time_str = mxCalloc( STRSZ, sizeof( char ) );
			strcpy( time_str, s );
			free( s );
		}
	}

	if( ! mtlb_get_string( prhs[2], &endtime_str ) )
	{
		if( ! get_scalar( prhs[2], &scalar ) )
		{
			if( table ) mxFree( table );
                	antelope_mexUsageMsgTxt ( USAGE );
			return;
		}
		else
		{
			s = epoch2str( scalar, "%E\0" );
			antelope_mex_clear_register( 1 );
			
			endtime_str = mxCalloc( STRSZ, sizeof( char ) );
			strcpy( endtime_str, s );
			free( s );
		}
	}

	if( nrhs <= 3 || ( nrhs == 4 && table != NULL ) ) 
	{
		tr.database = dbINVALID;
	}

	rc = trload_cssgrp( db, time_str, endtime_str, &tr, table, 0 );
	antelope_mex_clear_register( 1 );

	dbfree( db );

	if( table ) mxFree( table );
	mxFree( time_str );
	mxFree( endtime_str );

	if( rc < 0 )
	{
		mexPrintf( "trload_cssgrp: No matching data were found\n" );
	}
	else if( rc > 0 )
	{
		mexErrMsgTxt( "trload_cssgrp failed\n" );
	}
	else  /* Success */
	{
		plhs[0] = CreateDbptrStructFromDbptr( tr );

		if( plhs[0] == NULL )
		{
			sprintf( errmsg, "trload_cssgrp: failed to create " );
			strcat( errmsg, "database-pointer structure " );
			strcat( errmsg, "for result" );
			mexErrMsgTxt( errmsg );
		}
	}
}
