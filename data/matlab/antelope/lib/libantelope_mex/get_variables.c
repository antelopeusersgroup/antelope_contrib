/*
 * Matlab interface to Datascope package
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1999
 */

#include "antelope_mex.h"

int	
get_dbptr( mxArray *in, Dbptr *db )
{
        if( ! AssertIsDbptrStruct( in ) )
        {
		return 0;
        }

	*db = CastDbptrStructToDbptr( in );

	if( (*db).database == dbINVALID )
	{
		mexWarnMsgTxt( "failed to interpret database pointer\n" );
		return 0;
	}

	return 1;
}

int 
get_string( mxArray *in, char **string )
{

        if( mxGetClassID( in ) != mxCHAR_CLASS )
        {
		return 0;
        }

	get_malloced_string( in, string );
	
	return 1;
}

int
get_pf( mxArray *in, Pf **pf )
{
        if( mxGetClassID( in ) != mxOBJECT_CLASS )
        {
		return 0;
        }
	else if( ! mxIsClass( in, "dbpf" ) ) 
	{
		return 0;
	}

	*pf = mxArray2Pf( in );

	if( *pf == NULL )
	{
		mexWarnMsgTxt ( "Null parameter-file\n" );
		return 0;
	}

	return 1;
}

int
get_response( mxArray *in, Response **response )
{
        if( mxGetClassID( in ) != mxOBJECT_CLASS )
        {
		return 0;
        }
	else if( ! mxIsClass( in, "dbresponse" ) ) 
	{
		return 0;
	}

	*response = mxArray2Response( in );

	if( *response == NULL )
	{
		mexWarnMsgTxt ( "Null response-object\n" );
		return 0;
	}

	return 1;
}

int
get_scalar( mxArray *in, double *scalar )
{
	if( ! mxIsNumeric( in ) ||
	    ! mxIsDouble( in ) ||
	    mxIsEmpty( in ) ||
	    mxIsComplex( in ) ||
	    ( mxGetN( in ) * mxGetM( in ) != 1 ) )
	{
		return 0;
	}

	*scalar = mxGetScalar( in );

	return 1;
}
