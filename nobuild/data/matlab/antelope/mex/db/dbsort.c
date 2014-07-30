/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1997
 */

#define USAGE "Error using ==> dbsort\n\n\
Usage: DBPTR = DBSORT ( DBPTR [,FIELD, FIELD,...] )\n"

#include "antelope_mex.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	Dbptr	db;
	Tbl	*tbl;
	char	**sortfields;
	char	errmsg[STRSZ];
	int	i, arg_index, nvarargs;
	int	flags = 0;
	int	rc;

	if( nrhs < 1 )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
        else if( ! get_dbptr( prhs[0], &db ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }

	nvarargs = nrhs - 1;
	for( i = 0; i<nvarargs; i++) 
	{
		arg_index = i + 1;
		if( mxGetClassID( prhs[arg_index] ) != mxCHAR_CLASS )
		{
			antelope_mexUsageMsgTxt ( USAGE );
			return;
		}
	}

	if( nvarargs > 0 )
	{
		tbl = newtbl( nvarargs );
		sortfields = (char **) mxCalloc( nvarargs, sizeof( char * ) );
		for( i = 0; i < nvarargs; i++ )
		{
			arg_index = i + 1;
			get_malloced_string( prhs[arg_index], &sortfields[i] );

			if( STREQ( sortfields[i], "dbSORT_UNIQUE" ) ) {

				flags |= dbSORT_UNIQUE;

			} else if( STREQ( sortfields[i], "dbSORT_REVERSE" ) ) {

				flags |= dbSORT_REVERSE;

			} else {

				pushtbl( tbl, sortfields[i] );
			}
		}
	}
	else
	{
		rc = dbquery ( db, dbPRIMARY_KEY, &tbl );
		antelope_mex_clear_register( 1 );
		if( rc == dbINVALID ) 
		{
			mexErrMsgTxt( "dbsort: query for primary keys failed" );
		}
	}

	db = dbsort ( db, tbl, flags, 0) ;
	antelope_mex_clear_register( 1 );

	if( nvarargs > 0 )
	{
		for( i = 0; i < nvarargs; i++ )
		{
			mxFree( sortfields[i] );
		}
		mxFree( sortfields );
		freetbl( tbl, 0 );
	}

	if( db.table == dbINVALID )
	{
		mexErrMsgTxt( "dbsort failed" );
	}

	plhs[0] = CreateDbptrStructFromDbptr( db );

	if( plhs[0] == NULL )
	{
		sprintf( errmsg, "dbsort: failed to create database-pointer " );
		strcat( errmsg, "structure for result" );
		mexErrMsgTxt( errmsg );
	}
}
