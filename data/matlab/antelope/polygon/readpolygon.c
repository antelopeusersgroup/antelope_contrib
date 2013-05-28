/* 
 * Antelope Toolbox for Matlab Supplement
 *
 * Nikolaus Horn,
 * strongly depending upon Kent Lindquists work...
 * 2003
 */

#define USAGE "Error using ==> readpolygon\n\n\
Usage: [LAT, LON] = READPOLYGON ( DBPTR )\n"

#include "antelope_mex.h"
#include "polygon.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	Dbptr	db;
	Point	*polygon;
	int 	nrec;
	int 	single_record;
	double 	NaN;
	double	*lat;
	double	*lon;
	int		nvertices;
	int 	nsaved,closed;
	int		i,j;
	double  *lat_pr, *lon_pr;
	Expression	*expr;
	int		maxneeded;
	int		sumpoints;
	int		*nanindices;
	char 	closedstr[2];

	
	NaN=mxGetNaN();

	if ( nrhs != 1 ) {
		antelope_mexUsageMsgTxt ( USAGE ); return;
	} else if ( ! get_dbptr( prhs[0], &db ) ) {
		antelope_mexUsageMsgTxt ( USAGE ); return;
	} else if ( nlhs != 2 ) {
		antelope_mexUsageMsgTxt ( USAGE ); return;
	}

	if (dbquery( db, dbRECORD_COUNT, &nrec) < 0) {
		antelope_mex_clear_register( 1 );
		mexErrMsgTxt("readpolygon: dbquery(recordcount) failed\n");
	}
	if (nrec <= 0 ) {
		mexErrMsgTxt("readpolygon: no rows in database view\n");
	}
	if (db.record == dbALL) {
		if (nrec == 1) {
			single_record= 1;
			db.record=0;
			dbgetv(db,0,"npoints",&sumpoints,0);
			maxneeded= (nrec*2) + sumpoints;
		} else {
			single_record=0;
			db.record=0;
			dbex_compile(db,"sum(npoints)",&expr,dbINTEGER);
			dbex_eval(db,expr,0,&sumpoints);
			dbex_free(expr);
			maxneeded= (nrec*2) + sumpoints;
			nanindices=mxCalloc(nrec,sizeof(int));
		}
	} else {
		if (db.record >= 0) {
			single_record= 1;
			dbgetv(db,0,"npoints",&sumpoints,"closed",&closedstr,0);
			maxneeded= (nrec*2) + sumpoints;
		} else {
			mexErrMsgTxt("readpolygon: unspecified subset?!?\n");
		}
	}
	
	nsaved= 0;
	nvertices= readPolygon(db,&polygon);
	lat= mxCalloc(maxneeded+1, sizeof(double));
	lon= mxCalloc(maxneeded+1, sizeof(double));
	for (j= 0; j < nvertices; j++) {
		lat[j]= polygon[j].lat;
		lon[j]= polygon[j].lon;
	}
	nsaved += nvertices;
	free(polygon);
	/* eventually close polygon*/
	if (strncmp(closedstr,"y",1)==0) {
		if ( (lat[nvertices-1] != lat[0]) || (lon[nvertices-1] != lon[0]) ) {
			nsaved++;
			lat[nsaved-1]= lat[0];
			lon[nsaved-1]= lon[0];
		}
	}
	if ( (single_record == 0) && (nrec > 1) ) {
		for (i= 1; i < nrec; i++) {
			db.record=i;
			dbgetv(db,0,"closed",&closedstr,0);
			nvertices= readPolygon(db,&polygon);
			/* separate (sub)polygons by NaN */
			lat[nsaved + 1]=NaN;
			lon[nsaved + 1]=NaN;
			nanindices[i-1]= nsaved+1;
			nsaved++;
			for (j= 0; j < nvertices; j++) {
				lat[j+nsaved]= polygon[j].lat;
				lon[j+nsaved]= polygon[j].lon;
			}
			nsaved += nvertices;
			/* eventually close polygon*/
			if (strncmp(closedstr,"y",1)==0) {
				if ( (lat[nvertices-1] != lat[0]) || (lon[nvertices-1] != lon[0]) ) {
					lat[nsaved]= lat[nsaved-nvertices];
					lon[nsaved]= lon[nsaved-nvertices];
					nsaved++;
				}
			}	
			free(polygon);
		}
	}
	plhs[0] = mxCreateDoubleMatrix(nsaved,1,mxREAL);
	lat_pr=(double *)mxGetPr(plhs[0]);
	memcpy(lat_pr, lat, nsaved*sizeof(double));	
	
	plhs[1] = mxCreateDoubleMatrix(nsaved,1,mxREAL);
	lon_pr=(double *)mxGetPr(plhs[1]);
	memcpy(lon_pr, lon, nsaved*sizeof(double));	
	/* NaN does NOT pass the memcpy */
	if (single_record == 0) {
		for (i=0; i < nrec-1; i++) {
			lat_pr[nanindices[i]-1]=NaN;
			lon_pr[nanindices[i]-1]=NaN;
		}
		mxFree(nanindices);
	}

	mxFree(lat);
	mxFree(lon);
}
