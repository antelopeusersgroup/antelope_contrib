#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"
#include "stock.h"
#include "tr.h"


MODULE = wfutil		PACKAGE = wfutil		

void
trloadwf(idatabase, itable, ifield, irecord,sta, chan, t0,t1,filter,calibrate)
	int		idatabase
	int 	itable
	int 	ifield
	int 	irecord
	char	*sta;
	char	*chan;
	char  	*t0;
	char	*t1;
	char	*filter;
	int		calibrate;
	PPCODE:
	{
	Dbptr db,tr,dbsitechan,dbsensor,dbsite,dbinstrument,dbaffiliation,dbnetwork;
	int	nsamp=0,npoints;
	double et0,et1,rt0,rt1;
	float	*data=NULL;
	int i;
	char 	expr[1024];
	double time,samprate;
	char 	datatype[1024];
	int foff;
	int fil=1;
	Dbptr dbout;
	db.database= 	idatabase;
	db.table= 		itable;
	db.field= 		ifield;
	db.record= 		irecord;
	if (strlen(filter) == 0 || strncasecmp(filter,"none",4)==0) {
	   fil=0;
	} else {
 		fil=1;
	}		
	et0= str2epoch(t0);
	et1= str2epoch(t1);
	db=dblookup(db,0,"wfdisc",0,0);
	sprintf(expr,"sta=~/%s/ && chan =~/%s/",sta,chan);
	db=dbsubset(db,expr,0);
	dbsitechan=dblookup(db,0,"sitechan",0,0);
	dbjoin(db,dbsitechan,0,0,0,0,0);
	dbsensor=dblookup(db,0,"sensor",0,0);
	dbjoin(db,dbsensor,0,0,0,0,0);
	dbsite=dblookup(db,0,"site",0,0);
	dbjoin(db,dbsite,0,0,0,0,0);
	dbinstrument=dblookup(db,0,"instrument",0,0);
	dbjoin(db,dbinstrument,0,0,1,0,0);
	dbaffiliation=dblookup(db,0,"affiliation",0,0);
	dbjoin(db,dbaffiliation,0,0,1,0,0);
	dbnetwork=dblookup(db,0,"network",0,0);
	dbjoin(db,dbnetwork,0,0,1,0,0);
	tr=dbinvalid();
	if ( trload_css(db, t0, t1, &tr,0,0) ) {
		croak("trload_css failed\n");
	}
	if ( trsplit(tr, 0, 0) ) {
		croak("trsplit failed");
	}
	if ( trsplice(tr, 0, 0, 0) ) {
		croak("trsplice failed");
	}
	 if (calibrate) {
   	# as trapply_calib always returns 0, no checking is done!
		trapply_calib(tr);
   	}
	tr=dblookup(tr,0,"trace",0,0);
	dbquery(tr,dbRECORD_COUNT,&i);
	if (i > 1) {
		croak("trace seems to be split, giving up...");
	}
	tr.record=0;
	if (fil) {
		if (trfilter(tr,filter)) {
			croak("problems filtering data...");
		};
	}
	dbgetv(tr,0,"time",&rt0,
                "endtime", &rt1,
                "nsamp", &nsamp,
                "samprate", &samprate,
                0);
	if (nsamp < 1) {
		croak("no data available");
	}
	dbgetv(tr,0,"data",&data,0);
	EXTEND(sp,2 + nsamp);
	PUSHs(sv_2mortal(newSVnv(rt0)));
	PUSHs(sv_2mortal(newSVnv(rt1)));
	for (i=0; i < nsamp; i++) {
		PUSHs(sv_2mortal(newSVnv(data[i])));
	}
	dbfree(dbsitechan);
	dbfree(dbsite);
	dbfree(dbsensor);
	dbfree(dbinstrument);
	dbfree(dbaffiliation);
	dbfree(dbnetwork);
	dbfree(db);
	trdestroy(&tr);
	}	
	


