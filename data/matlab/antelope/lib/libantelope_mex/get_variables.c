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
	mxArray *cell;

        if( mxGetClassID( in ) == mxCHAR_CLASS )
	{
		get_malloced_string( in, string );
	
		return 1;
	} 
	else if( mxGetClassID( in ) == mxCELL_CLASS &&
		mxGetNumberOfElements( in ) == 1 )
	{
		cell = mxGetCell( in, 0 );

		if( mxGetClassID( cell ) != mxCHAR_CLASS )
		{
			return 0;
		}
		else
		{
			get_malloced_string( cell, string );

			return 1;
		}
	}
	else
        {
		return 0;
        }
}

int
get_stringtbl( mxArray *in, Tbl **tbl )
{
	mxArray *input_array[1];
	mxArray *output_array[1];

	input_array[0] = in;
	mexCallMATLAB( 1, output_array, 1, input_array, "iscellstr" );

	if( ! mxGetScalar( output_array[0] ) )
	{
		*tbl = 0;
		return 0;
	}

	*tbl = cellstr2stringtbl( in );

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
