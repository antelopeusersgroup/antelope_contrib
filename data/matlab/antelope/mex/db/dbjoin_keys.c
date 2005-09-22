/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1997
 */

#define USAGE "Error using ==> dbjoin_keys\n\n\
Usage: DBPTR = DBJOIN_KEYS ( DBPTR1, DBPTR2 **or** DBPTR, TABLE1, TABLE2 )\n"

#include "antelope_mex.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	Dbptr	db, db1, db2;
	Tbl	*pattern1 = 0;
	Tbl	*pattern2 = 0;
	Tbl	*join_keys = 0;
	char	join_key[STRSZ];
	char	*key1, *key2;
	char	*table1;
	char 	*table2;
	int	N;
	int	i;

	if( nrhs != 2 && nrhs != 3 )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
        else if( ! get_dbptr( prhs[0], &db1 ) )
        {
               	antelope_mexUsageMsgTxt ( USAGE );
		return;
        }

	if( nrhs == 2 )
	{
        	if( ! get_dbptr( prhs[1], &db2 ) )
        	{
                	antelope_mexUsageMsgTxt ( USAGE );
			return;
        	}
	}
	else 
	{
		if( ! mtlb_get_string( prhs[1], &table1 ) )
		{
			antelope_mexUsageMsgTxt ( USAGE );
			return;
		}
		else if( ! mtlb_get_string( prhs[2], &table2 ) )
		{
			mxFree( table1 );
			antelope_mexUsageMsgTxt ( USAGE );
			return;
		}
		
		db1 = dblookup( db1, 0, table1, 0, 0 );
		antelope_mex_clear_register( 1 );
		if( db1.table == dbINVALID )
		{
			mxFree( table1 );
			mxFree( table2 );
			mexErrMsgTxt ( "dbjoin_keys: table lookup failed" );
			return;
		}
		
		db2 = dblookup( db1, 0, table2, 0, 0 );
		antelope_mex_clear_register( 1 );
		if( db2.table == dbINVALID )
		{
			mxFree( table1 );
			mxFree( table2 );
			mexErrMsgTxt ( "dbjoin_keys: table lookup failed" );
			return;
		}

		mxFree( table1 );
		mxFree( table2 );
	}

	db1.record = dbSCRATCH;
	db2.record = dbSCRATCH;

	db = dbjoin( db1, db2, &pattern1, &pattern2, 0, 0, 0 );
	antelope_mex_clear_register( 1 );

	if( db.table == dbINVALID )
	{
		mexErrMsgTxt( "dbjoin_keys: join failed" );
	}

	if( ! pattern1 || ! pattern2 )
	{
		mexErrMsgTxt( "dbjoin_keys: no join keys found" );
	}

	if( maxtbl( pattern1 ) != maxtbl( pattern2 ) )
	{
		mexErrMsgTxt( "dbjoin_keys: mismatched tables of keys" );
	}

	N = maxtbl( pattern1 );

	join_keys = newtbl( N );

	for( i = 0; i < N; i++ )
	{
		key1 = gettbl( pattern1, i );
		key2 = gettbl( pattern2, i );

		if( STREQ( key1, key2 ) )
		{
			strcpy( join_key, key1 );
		}
		else
		{
			sprintf( join_key, "%s == %s", key1, key2 );
		}
		pushtbl( join_keys, strdup( join_key ) );
	}

	if( pattern1 ) freetbl( pattern1, 0 );
	if( pattern2 ) freetbl( pattern2, 0 );

	plhs[0] = stringtbl2cellstr( join_keys );

	freetbl( join_keys, 0 );

	if( plhs[0] == NULL )
	{
		mexErrMsgTxt( "dbjoin_keys: failed to create list of keys" );
	}
}
