/* 
 * Antelope Toolbox for Matlab Supplement
 *
 * Nikolaus Horn,
 * entirely depending upon Kent Lindquists work...
 * 2003
 */

#define USAGE "Error using ==> writepolygondata\n\n\
Usage: WRITEPOLYGONDATA ( DBPTR, LAT, LON, PNAME, PTYPE, AUTH, DIR, DFILE, PCODE )\n"

#include "antelope_mex.h"
#include "polygon.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	Dbptr	db;
	Point	*polygon;
	char 	*pname;
	char	*ptype;
	char	*auth;
	char	*dir;
	char	*dfile;
	char	*s_pcode;
	int		pcode;
	double	*lat;
	double	*lon;
	long		nsamp;
	mwSize 	nlat,nlon;
	const mwSize		*dimensions;
	int	i;
	int retcode;
	char *dbfile_name;
	char *dbschema_name;
	char errmsg[STRSZ];


	if ( nrhs != 9 ) {
		antelope_mexUsageMsgTxt ( USAGE ); return;
	} else if ( ! get_dbptr( prhs[0], &db ) ) {
		antelope_mexUsageMsgTxt ( USAGE ); return;
	} else if ( !mxIsDouble( prhs[1] ) ) {
		antelope_mexUsageMsgTxt ( USAGE ); return;
	} else if ( !mxIsDouble( prhs[2] ) ) {
		antelope_mexUsageMsgTxt ( USAGE ); return;
	} else if ( !mxIsChar( prhs[3] ) ) {
		antelope_mexUsageMsgTxt ( USAGE ); return;
	} else if ( !mxIsChar( prhs[4] ) ) {
		antelope_mexUsageMsgTxt ( USAGE ); return;
	} else if ( !mxIsChar( prhs[5] ) ) {
		antelope_mexUsageMsgTxt ( USAGE ); return;
	} else if ( !mxIsChar( prhs[6] ) ) {
		antelope_mexUsageMsgTxt ( USAGE ); return;
	} else if ( !mxIsChar( prhs[7] ) ) {
		antelope_mexUsageMsgTxt ( USAGE ); return;
	} else if ( !mxIsChar( prhs[8] ) ) {
		antelope_mexUsageMsgTxt ( USAGE ); return;
	}
	dimensions= (mwSize*) mxGetDimensions( prhs[1] );
	if (dimensions[1] != 1 && dimensions[0] != 1) {
		mexErrMsgTxt("writepolygondata: LAT must have dimensions Nx1 or 1xN\n");
	}
	if (dimensions[1] == 1) {
		nlat= dimensions[0];
	} else {
		nlat= dimensions[1];
	}
	dimensions= (mwSize *)mxGetDimensions( prhs[2] );
	if (dimensions[1] != 1 && dimensions[0] != 1) {
		mexErrMsgTxt("writepolygondata: LON must have dimensions Nx1 or 1xN\n");
	}
	if (dimensions[1] == 1) {
		nlon= dimensions[0];
	} else {
		nlon= dimensions[1];
	}
	if (nlat != nlon) {
		mexErrMsgTxt("writepolygondata: LAT and LON must be of same size\n");
	}
	if( ( retcode = dbquery(db, dbDATABASE_FILENAME, &dbfile_name) ) < 0 ) {
			mexErrMsgTxt("writepolygondata: problem getting descriptor filename\n");
	}
	if ( is_file(dbfile_name)==1 ) {
		if( ( retcode = dbquery(db, dbSCHEMA_NAME, &dbschema_name) ) < 0 ) {
				mexErrMsgTxt("writepolygondata: problem getting schema name\n");
		} else {
				if (strmatches(dbschema_name,"polygon1.2.*", 0) != 1) {
						sprintf(errmsg, "writepolygondata: wrong schema '%s' for output database '%s'\n",dbschema_name,dbfile_name);
						mexErrMsgTxt(errmsg);
				}
		}
	} else {
		sprintf(errmsg, "writepolygondata: please create database schema '%s' with schema polygon1.2\n",dbfile_name);
		mexErrMsgTxt(errmsg);
			//there might be a fix, but for the moment this simply does NOT work
		if (dbcreate(dbfile_name,"polygon1.2",0, 0, 0) != 0 ) {
				mexErrMsgTxt("writepolygondata: problem with dbcreate\n");
		}
	}


	
	nsamp= (long) nlat;

	lat= mxGetPr(prhs[1]);
	lon= mxGetPr(prhs[2]);
	polygon= mxCalloc(nsamp+1,sizeof(Point));
	
	for (i= 0; i < nsamp; i++) {
		polygon[i].lat=lat[i];
		polygon[i].lon=lon[i];
	}

	get_malloced_string(prhs[3],&pname);
	get_malloced_string(prhs[4],&ptype);
	get_malloced_string(prhs[5],&auth);
	get_malloced_string(prhs[6],&dir);
	get_malloced_string(prhs[7],&dfile);
	get_malloced_string(prhs[8],&s_pcode);

	pcode= polycode(s_pcode);

	/*maybe nsamp -1, check later...*/
	writePolygonData(db,polygon,nsamp,pname,1,1,ptype,auth,dir,dfile,pcode);

	antelope_mex_clear_register(1);

	mxFree(polygon);
	mxFree(s_pcode);
	mxFree(dfile);
	mxFree(dir);
	mxFree(auth);
	mxFree(ptype);
	mxFree(pname);
}
