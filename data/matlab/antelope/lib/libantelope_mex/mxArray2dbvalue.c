/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1999
 */

#include <stdlib.h>
#include "antelope_mex.h"

Dbvalue *
mxArray2dbvalue( const mxArray *in, long type )
{
	Dbvalue	*value;
	char	warning[STRSZ];
	char	*s;

	value = (Dbvalue *) mxCalloc( 1, sizeof( Dbvalue ) );

	switch( type )
	{
	case dbDBPTR:
		if( ! get_dbptr( in, &value->db ) )
		{
			mxFree( value );
			value = (Dbvalue *) NULL;
		}
		break;
	case dbSTRING:
		if( ! mtlb_get_string( in, &s ) )
		{
			mxFree( value );
			value = (Dbvalue *) NULL;
		}
		else
		{
			strcpy( value->s, s );
			mxFree( s );
		}
		break;
	case dbBOOLEAN:
	case dbINTEGER:
	case dbYEARDAY:
		if( ! get_scalar( in, &value->d ) )
		{
			mxFree( value );
			value = (Dbvalue *) NULL;
		}
		else
		{
			value->i = (long) value->d;
		}
		break;
	case dbREAL:
	case dbTIME:
		if( ! get_scalar( in, &value->d ) )
		{
			mxFree( value );
			value = (Dbvalue *) NULL;
		}
		break;
	default:
		sprintf( warning, 
			"Field type %s not recognized in dbaddv",
			xlatnum( type, Dbxlat, NDbxlat ) );
		mexWarnMsgTxt( warning );
		mxFree( value );
		value = (Dbvalue *) NULL;
		break;
	}

	return value;
}
