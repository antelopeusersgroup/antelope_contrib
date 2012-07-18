/* 
 * Antelope Toolbox for Matlab Supplement
 *
 * Nikolaus Horn,
 * strongly depending upon Kent Lindquists work...
 * 2003
 */

#define USAGE "Error using ==> inwhichpolygons\n\n\
Usage: DBPTR = INWHICHPOLYGONS ( DBPTR, LAT, LON)\n"

#include "antelope_mex.h"
#include "polygon.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	Dbptr	db;
	Dbptr	ret;
	Point 	P;
	double	*lat;
	double	*lon;
	int		*dimensions;
	char	errmsg[STRSZ];

	if ( nrhs != 3 ) {
		antelope_mexUsageMsgTxt ( USAGE ); return;
	} else if ( ! get_dbptr( prhs[0], &db ) ) {
		antelope_mexUsageMsgTxt ( USAGE ); return;
	} else if ( !mxIsDouble( prhs[1] ) ) {
		antelope_mexUsageMsgTxt ( USAGE ); return;
	} else if ( !mxIsDouble( prhs[2] ) ) {
		antelope_mexUsageMsgTxt ( USAGE ); return;
	} 
	dimensions= (int *)mxGetDimensions( prhs[1] );
	if (dimensions[1] != 1 || dimensions[0] != 1) {
		mexErrMsgTxt("writepolygondata: LAT must have dimensions 1x1\n");
	}
	dimensions= (int *)mxGetDimensions( prhs[2] );
	if (dimensions[1] != 1 || dimensions[0] != 1) {
		mexErrMsgTxt("writepolygondata: LON must have dimensions 1x1\n");
	}

	lat= mxGetPr(prhs[1]);
	lon= mxGetPr(prhs[2]);
	P.lat= *lat;
	P.lon= *lon;
	ret=inWhichPolygons(db,P);
	
	plhs[0] = CreateDbptrStructFromDbptr( ret );

	if( plhs[0] == NULL )
	{
		sprintf( errmsg, "inwhichpolygons: failed to create " );
		strcat( errmsg, "database-pointer structure for result" );
		mexErrMsgTxt( errmsg );
	}
	antelope_mex_clear_register(1);
}
