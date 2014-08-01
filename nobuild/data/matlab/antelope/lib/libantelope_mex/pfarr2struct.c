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
pfarr2struct( Pf *pf, int recursive )
{
	mxArray	*mystruct;
	mxArray *mytbl;
	mxArray *myarr;
	mxArray *mystring;
	mxArray *answer;
	Tbl	*keys;
	int	nfields;
	char	**fieldnames;
	char	errmsg[STRSZ];
	char	*name;
	void	*value;
	int	type;
	int	i;


	if( pf->type != PFARR && pf->type != PFFILE )
	{
		return (mxArray *) NULL;
	}

	keys = pfkeys( pf );
	antelope_mex_clear_register( 1 );

	if( keys == NULL )
	{
		return (mxArray *) NULL;
	}

	nfields = maxtbl( keys );

	if( nfields == 0 )
	{
		return (mxArray *) NULL;
	}

	fieldnames = (char **) mxCalloc( nfields, sizeof( char * ) );
	for( i = 0; i < nfields; i++ )
	{
		fieldnames[i] = (char *) mxCalloc( mxMAXNAM, 
						   sizeof( char ) );
		strcpy( fieldnames[i], gettbl( keys, i ) );
	}

	mystruct = mxCreateStructMatrix( 1, 1, nfields,
				(const char **) fieldnames );

	for( i = 0; i < nfields; i++ )
	{
		mxFree( fieldnames[i] );
	}
	mxFree( fieldnames );

	if( mystruct == 0 )
	{
		sprintf( errmsg, "pfarr2struct: failed to create struct. " );
		strcat( errmsg, "Possible illegal field names.\n" );
		mexPrintf( errmsg );
		return (mxArray *) NULL;
	}

	for( i = 0; i < nfields; i++ )
	{

		name = gettbl( keys, i );

		if( name == NULL )
		{
			mxDestroyArray( mystruct );
			return (mxArray *) NULL;
		}

		type = pfpeek( pf, name, (Pf **) &value );

		switch( type )
		{
		case PFTBL:
			mytbl = pftbl2cellarr( (Pf *) value, recursive );
			if( mytbl == 0 )
			{
				mxDestroyArray( mystruct );
				return 0;
			}
			else
			{
				mxSetField( mystruct, 0, name, mytbl );
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
				mxDestroyArray( mystruct );
				return 0;
			}
			else
			{
				mxSetField( mystruct, 0, name, myarr );
			}
			break;
		case PFSTRING:
			pfget( pf, name, &value );
			antelope_mex_clear_register( 1 );
			mystring = pfstring2mxArray( (char *) value );
			if( mystring == 0 )
			{
				mxDestroyArray( mystruct );
				return 0;
			}
			else 
			{
				mxSetField( mystruct, 0, name, mystring );
			}
			break;
#ifdef PFPROMPT
		case PFPROMPT:
			answer = mxPfprompt( ((Pf *) value)->value.s );
			if( answer == 0 )
			{
				mxDestroyArray( mystruct );
				return 0;
			}
			else
			{
				mxSetField( mystruct, 0, name, answer );
			}
			break;
#endif
		case PFFILE:
		case PFINVALID:
		default:
			mxDestroyArray( mystruct );
			return 0;
		}
	}

	return mystruct;
}
