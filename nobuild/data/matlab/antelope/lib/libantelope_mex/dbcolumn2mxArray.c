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

mxArray *
dbcolumn2mxArray( Dbptr db )
{
	mxArray	*result;
	mxArray	*cell;
	long	nrows;
	long	type;
	double	*pr;
	Dbvalue	value;
	char	warning[STRSZ];

	dbquery( db, dbRECORD_COUNT, &nrows );
	antelope_mex_clear_register( 1 );

	dbquery ( db, dbFIELD_TYPE, &type );
	antelope_mex_clear_register( 1 );

	switch( type )
	{
	case dbDBPTR:
	case dbSTRING:
		result = mxCreateCellMatrix( nrows, 1 );
		for( db.record = 0; db.record < nrows; db.record++ )
		{
			if( ( cell = dbfield2mxArray( db ) ) == 0 )
			{
				mxDestroyArray( result );
				return (mxArray *) NULL;
			}
			mxSetCell( result, db.record, cell );
		}
		break;
	case dbBOOLEAN:
	case dbINTEGER:
	case dbYEARDAY:
		result = mxCreateDoubleMatrix( nrows, 1, mxREAL );
		pr = mxGetPr( result );
		for( db.record = 0; db.record < nrows; db.record++ )
		{
			if( dbget( db, value.s ) < 0 )
			{
				antelope_mex_clear_register( 1 );
				mxDestroyArray( result );
				return (mxArray *) NULL;
			}
			copystrip( value.s, value.s, strlen( value.s ) );
			pr[db.record] = (double) atol( value.s );
		}
		break;
	case dbREAL:
	case dbTIME:
		result = mxCreateDoubleMatrix( nrows, 1, mxREAL );
		pr = mxGetPr( result );
		for( db.record = 0; db.record < nrows; db.record++ )
		{
			if( dbget( db, value.s ) < 0 )
			{
				antelope_mex_clear_register( 1 );
				mxDestroyArray( result );
				return (mxArray *) NULL;
			}
			copystrip( value.s, value.s, strlen( value.s ) );
			pr[db.record] = (double) atof( value.s );
		}
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
