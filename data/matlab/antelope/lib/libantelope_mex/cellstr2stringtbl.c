/* 
 * Matlab interface to Datascope package
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1999
 */

#include "antelope_mex.h"

Tbl *
cellstr2stringtbl( mxArray *cellstr )
{
	Tbl	*result;
	mxArray	*input_array[1];
	mxArray	*output_array[1];
	mxArray	*cell;
	char	*s;
	int	Ncells;
	int	i;

	input_array[0] = cellstr;
	mexCallMATLAB( 1, output_array, 1, input_array, "iscellstr" );

	if( ! mxGetScalar( output_array[0] ) )
	{
		mexWarnMsgTxt( "cellstr2stringtbl: failed to convert cellstr" );
		return (Tbl *) NULL;
	}

	Ncells = mxGetM( cellstr ) * mxGetN( cellstr );

	if( Ncells <= 0 ) 
	{
		mexWarnMsgTxt( "cellstr2stringtbl: empty cellstr" );
		return (Tbl *) NULL;
	}
	else 
	{
		result = newtbl( 0 );
	}

	for( i = 0; i < Ncells; i++ )
	{

		cell = mxGetCell( cellstr, i );

		if( ! get_string( cell, &s ) )
		{
			freetbl( result, 0 );
			mexWarnMsgTxt( 
				"cellstr2stringtbl: failed to convert string" );
			return (Tbl *) NULL;
			
		}

		pushtbl( result, s );
	}

	return result;
}
