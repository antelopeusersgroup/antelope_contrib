/*
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Lindquist Consulting
 * 1997-2003
 */

#include "antelope_mex.h"

int	
get_dbptr( const mxArray *in, Dbptr *db )
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
get_trimmed_string( const mxArray *in, char **string )
{
	int	rc;

	rc = mtlb_get_string( in, string );

	if( rc != 0 ) 
	{
		strtrim( *string );
	}

	return rc;
}

int 
mtlb_get_string( const mxArray *in, char **string )
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
get_stringtbl( const mxArray *in, Tbl **tbl )
{
	mxArray *input_array[1];
	mxArray *output_array[1];

	input_array[0] = (mxArray *) in;
	mexCallMATLAB( 1, output_array, 1, input_array, "iscellstr" );

	if( ! mxGetScalar( output_array[0] ) )
	{
		mexPrintf( "Input must be a cell-array of strings, e.g. {'string1'; 'string2'}\n" );
		*tbl = 0;
		return 0;
	}

	*tbl = cellstr2stringtbl( (mxArray *) in );

	return 1;
}

int
get_inttbl( const mxArray *in, Tbl **tbl )
{
	double	*data;
	long	myint;
	long	npts;
	long	i;

	if( ! mxIsNumeric( in ) ||
	    ! mxIsDouble( in ) ||
	    mxIsEmpty( in ) ||
	    mxIsComplex( in ) )
	{
		*tbl = (Tbl *) NULL;
		return 0;
	}

	npts = mxGetM( in ) * mxGetN( in );

	data = mxGetPr( in );

	*tbl = newtbl( 0 );

	for( i = 0; i < npts; i++ ) 
	{
		myint = (long) data[i];

		pushtbl( *tbl, (char *) myint );
	}

	return 1;
}

int
get_pf( const mxArray *in, Pf **pf )
{
	if( ! mxIsClass( in, "dbpf" ) ) 
	{
		return 0;
	}

	*pf = mxArray2Pf( (mxArray *) in );

	if( *pf == NULL )
	{
		mexWarnMsgTxt ( "Null parameter-file\n" );
		return 0;
	}

	return 1;
}

int
get_scalar( const mxArray *in, double *scalar )
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

int
mtlb_get_response( const mxArray *in, Response **response )
{
	*response = mxArray2Response( (mxArray *) in );

	if( *response == NULL )
	{
		mexWarnMsgTxt ( "Null dbresponse-object\n" );
		return 0;
	}

	return 1;
}

#ifdef HAVE_CGEOM
int
get_cggrid( const mxArray *in, CGGrid **cgg )
{
	*cgg = mxArray2CGGrid( (mxArray *) in );

	if( *cgg == (CGGrid *) NULL ) 
	{
		mexWarnMsgTxt ( "Null cggrid-object\n" );
		return 0;
	}

	return 1;
}
#endif
