/* 
 * Matlab interface to Datascope package
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1999
 */

#include <stdlib.h>
#include "antelope_mex.h"

mxArray *
dbfield2mxArray( Dbptr db )
{
	mxArray	*result;
	int	type;
	Dbvalue	value;
	char	warning[STRSZ];

	dbquery( db, dbFIELD_TYPE, &type );
	antelope_mex_clear_register( 1 );

	if( dbget( db, value.s ) < 0 )
	{
		antelope_mex_clear_register( 1 );
		return (mxArray *) NULL;
	}

	switch( type )
	{
	case dbDBPTR:
		result = CreateDbptrStructFromDbptr( value.db );
		break;
	case dbSTRING:
		copystrip( value.s, value.s, strlen( value.s ) );
		result = mxCreateString( value.s );
		break;
	case dbBOOLEAN:
	case dbINTEGER:
	case dbYEARDAY:
		copystrip( value.s, value.s, strlen( value.s ) );
		result = CreateDouble( (double) atoi( value.s ) );
		break;
	case dbREAL:
	case dbTIME:
		copystrip( value.s, value.s, strlen( value.s ) );
		result = CreateDouble( (double) atof( value.s ) );
		break;
	default:
		sprintf( warning, 
			"Can't interpret field of type %s",
			xlatnum( type, Dbxlat, NDbxlat ) );
		mexWarnMsgTxt( warning );
		result = (mxArray *) NULL;
		break;
	}

	return result;
}
