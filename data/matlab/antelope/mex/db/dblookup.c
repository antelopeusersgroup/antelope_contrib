/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1997
 */

#define USAGE "Error using ==> dblookup\n\n\
Usage: DBPTR = DBLOOKUP ( DBPTR, DATABASE, TABLE, FIELD, RECORD )\n"

#include "antelope_mex.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	Dbptr	db;
	char	*database;
	char	*table;
	char	*field;
	char	*record;
	char	errmsg[STRSZ];

	if( nrhs != 5  )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
	else if( ! get_dbptr( prhs[0], &db ) )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
	else if( ! mtlb_get_string( prhs[1], &database ) )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
	else if( ! mtlb_get_string( prhs[2], &table ) )
	{
		mxFree( database );
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
	else if( ! mtlb_get_string( prhs[3], &field ) )
	{
		mxFree( database );
		mxFree( table );
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
	else if( ! mtlb_get_string( prhs[4], &record ) )
	{
		mxFree( database );
		mxFree( table );
		mxFree( field );
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}

	if( ! STREQ( record, "" ) &&
	    ! STREQ( record, "dbSCRATCH" ) &&
	    ! STREQ( record, "dbNULL" ) && 
	    ! STREQ( record, "dbALL" ) )
	{
		/* We're trying to match a record... */

		if( STREQ( field, "dbALL" ) ||
		    ( STREQ( field, "" ) && db.field < 0 ) )
		{
			/* But we don't know which field to look at */

			mxFree( database );
			mxFree( table );
			mxFree( field );
			mxFree( record );
			mexErrMsgTxt(
			  "dblookup: Specify field before matching records" );
		}

	}

	db = dblookup( db, database, table, field, record );
	antelope_mex_clear_register( 1 );

	mxFree( database );
	mxFree( table );
	mxFree( field );
	mxFree( record );

	plhs[0] = CreateDbptrStructFromDbptr( db );

	if( plhs[0] == NULL )
	{
		sprintf( errmsg, "dblookup: failed to create " );
		strcat( errmsg, "database-pointer structure for result" );
		mexErrMsgTxt( errmsg );
	}
}
