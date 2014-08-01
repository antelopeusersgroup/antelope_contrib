/* 
 * Antelope Toolbox for Matlab
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
	mxArray	*cell;
	char	*s;
	int	Ncells;
	int	i;

	Ncells = mxGetM( cellstr ) * mxGetN( cellstr );

	if( Ncells <= 0 ) 
	{
		mexWarnMsgTxt( "cellstr2stringtbl: empty cellstr\n" );
		return (Tbl *) NULL;
	}
	else 
	{
		result = newtbl( 0 );
	}

	for( i = 0; i < Ncells; i++ )
	{

		cell = mxGetCell( cellstr, i );

		if( ! mtlb_get_string( cell, &s ) )
		{
			freetbl( result, 0 );
			mexWarnMsgTxt( 
				"cellstr2stringtbl: failed to convert string\n" );
			return (Tbl *) NULL;
			
		}

		pushtbl( result, s );
	}

	return result;
}
