/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1997
 */

#define USAGE "Error using ==> dbquery\n\n\
Usage: QUERY_RESULT = DBQUERY ( DBPTR, CODE )\n"

#include "antelope_mex.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	Dbptr	db;
	mxArray	*array_ptr;
	char	errmsg[STRSZ];
	char	*dbstring_code;
	int	dbcode;
	int	retcode;
	char	*string;
	Tbl	*tbl;
	long	n;
	int	i;

	if( nrhs != 2  )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
        else if( ! get_dbptr( prhs[0], &db ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }
        else if( ! mtlb_get_string( prhs[1], &dbstring_code ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }

	dbcode = xlatname( dbstring_code, Dbxlat, NDbxlat );

	switch( dbcode )
	{
	case dbSCHEMA_DEFAULT:
	case dbDATABASE_FILENAME:
	case dbIDSERVER:
	case dbLOCKS:
        case dbSCHEMA_DESCRIPTION:
        case dbTIMEDATE_NAME:
        case dbDATABASE_DESCRIPTION:
        case dbTABLE_DESCRIPTION:
        case dbFIELD_DESCRIPTION:
        case dbSCHEMA_DETAIL:
        case dbDATABASE_DETAIL:
        case dbTABLE_DETAIL:
        case dbFIELD_DETAIL:
        case dbSCHEMA_NAME:
        case dbDATABASE_NAME:
        case dbTABLE_NAME:
        case dbFIELD_NAME:
        case dbTABLE_FILENAME:
        case dbTABLE_DIRNAME:
        case dbFIELD_RANGE:
        case dbFIELD_FORMAT:
        case dbDBPATH:
        case dbFORMAT:
        case dbFIELD_UNITS:
        case dbFIELD_BASE_TABLE:
        case dbUNIQUE_ID_NAME:
		if( ( retcode = dbquery(db, dbcode, &string) ) >= 0 )
		{
			plhs[0] = mxCreateString( string );
		}
		antelope_mex_clear_register( 1 );
		break;

        case dbDATABASE_COUNT:
        case dbTABLE_COUNT:
        case dbFIELD_COUNT:
        case dbRECORD_COUNT:
        case dbTABLE_SIZE:
        case dbFIELD_SIZE:
        case dbFIELD_INDEX:
        case dbVIEW_TABLE_COUNT:
        case dbRECORD_SIZE:
        case dbTABLE_IS_WRITEABLE:
        case dbTABLE_IS_VIEW:
	case dbDATABASE_IS_WRITABLE:
	case dbTABLE_PRESENT:
	case dbTABLE_IS_TRANSIENT:
		if( ( retcode = dbquery(db, dbcode, &n) ) >= 0 )
		{
			antelope_mex_clear_register( 1 );
			plhs[0] = CreateDouble( (double) n );
			if( plhs[0] == NULL )
			{
				mxFree( dbstring_code );
				mexErrMsgTxt( 
				   "dbquery: failed to create return value" );
			}
		}
		else
		{
			antelope_mex_clear_register( 1 );
		}
                break;  

        case dbFIELD_TYPE:
		if( ( retcode = dbquery(db, dbcode, &n) ) >= 0 )
		{
			antelope_mex_clear_register( 1 );
			plhs[0] = mxCreateString( xlatnum( n, Dbxlat, NDbxlat ) );
		}
		else
		{
			antelope_mex_clear_register( 1 );
		}
                break;  
 
        case dbLINK_FIELDS:
        case dbSCHEMA_FIELDS:
	case dbSCHEMA_TABLES:
        case dbFIELD_TABLES:
        case dbVIEW_TABLES:
        case dbTABLE_FIELDS:
        case dbPRIMARY_KEY:
        case dbALTERNATE_KEY:
        case dbFOREIGN_KEYS:
		if( ( retcode = dbquery(db, dbcode, &tbl) ) >= 0 )
		{
			antelope_mex_clear_register( 1 );
			plhs[0] = stringtbl2cellstr( tbl );
		}
		else
		{
			antelope_mex_clear_register( 1 );
		}
                break;  
 
        default:
		sprintf( errmsg, "dbquery: bad code '%s'", dbstring_code );
		mxFree( dbstring_code );
		mexErrMsgTxt( errmsg );
		break ;
	}

	mxFree( dbstring_code );
}
