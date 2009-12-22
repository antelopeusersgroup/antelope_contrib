/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1999
 */

#include "antelope_mex.h"

mxArray *
pftbl2cellarr( Pf *pftbl, int recursive )
{
	mxArray *result;
	mxArray *mystring;
	mxArray	*mytbl;
	mxArray *myarr;
	mxArray *answer;
	void	*value;
	int	type;
	int	M;
	long	i;

	if( pftbl->type != PFTBL )
	{
		return 0;
	}

	M = pfmaxtbl( pftbl );

	if( M <= 0 ) 
	{
		return 0;
	}
	else 
	{
		result = mxCreateCellMatrix( M, 1 );
	}

	for( i = 0; i < M; i++ )
	{
		type = pfpeek( pftbl, (char *) i, (Pf **) &value );

		switch( type )
		{
		case PFTBL:
			/* Must be recursive because a table doesn't 
			   have any subsidiary names that can be looked up */ 
			mytbl = pftbl2cellarr( (Pf *) value, recursive );
			if( mytbl == 0 )
			{
				mxDestroyArray( result );
				return 0;
			}
			else
			{
				mxSetCell( result, i, mytbl );
			}
			break;
		case PFARR:
			if( recursive )
			{
				myarr = pfarr2struct( (Pf *) value, recursive );
			} 
			else
			{
				myarr = Pf2mxArray( (Pf *) value, "" );
			}
			if( myarr == 0 )
			{
				mxDestroyArray( result );
				return 0;
			}
			else
			{
				mxSetCell( result, i, myarr );
			}
			break;
		case PFSTRING:
			pfget( pftbl, (char *) i, &value );
			antelope_mex_clear_register( 1 );
			mystring = pfstring2mxArray( (char *) value );
			if( mystring == 0 )
			{
				mxDestroyArray( result );
				return 0;
			}
			else 
			{
				mxSetCell( result, i, mystring );
			}
			break;
#ifdef PFPROMPT
		case PFPROMPT:
			answer = mxPfprompt( ((Pf *) value)->value.s );
			if( answer == 0 )
			{
				mxDestroyArray( result );
				return 0;
			}
			else
			{
				mxSetCell( result, i, answer );
			}
			break;
#endif
		case PFFILE:
		case PFINVALID:
		default:
			mxDestroyArray( result );
			return 0;
		}
	}

	return result;
}
