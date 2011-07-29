#include "stock.h"
#include "coords.h"
#include "db.h"
#include "pf.h"
#include "elog.h"
#include "perf.h"
#include "glputil.h"

void usage()
{
	elog_die(0,"Usage:  pmelavg db [-pf pffile]\n");
}
void get_error(int err)
{
	elog_die(0,"Error reading record %d of working view\n",err);
}

int
main(int argc, char **argv)
{
	Dbptr db, dbv, dbgrp, dbev,dbevs, dbo,dbos;
	char *pfname=NULL;
	Pf *pf;
	int i;
	double rmin;
	long nevents;
	long evid, orid, prefor;
	double dr,az,d,r,wt,sumwt;
	double h[4],havg[4],hc[4];
	Tbl *gtbl,*epat,*opat;
	Tbl *matchlist=NULL;
	Hook *hooke,*hooko;  
	int use_r_weight;  
	int use_ssr_weight;
	int ierr;
	hooke=NULL;  hooko=NULL;

	if(argc<2) usage();

	for(i=2;i<argc;++i)
	{
		if(!strcmp(argv[i],"-pf"))
		{
			++i;
			pfname = argv[i];
		}
		else
			usage();
	}
	if(pfname==NULL) pfname=strdup("pmelavg");

	if(pfread(pfname, &pf)) elog_die(0,"pfread failed\n");
	use_r_weight=pfget_boolean(pf,"use_inv_r_weighting");
	use_ssr_weight=pfget_boolean(pf,"use_ssr_weighting");
	if(!(use_r_weight || use_ssr_weight))
		elog_die(0,"Must specify 1/r and/or ssr weighting.\nFix pf file\n");

	if(dbopen(argv[1],"r+",&db)) 
		elog_die(0,"dbopen of %s failed\n",argv[1]);
	dbv = dbform_working_view(db,pf,"pmelavg_dbview");
	if(dbv.record==dbINVALID)
		elog_die(0,"dbprocess failed:  check pf parameter pmelavg_dbview\n");
	dbev = dblookup(db,0,"event",0,0);
	dbo=dblookup(db,0,"origin",0,0);
	dbevs = dblookup(db,0,"event",0,0);
	dbos=dblookup(db,0,"origin",0,0);
	dbevs.record = dbSCRATCH;
	dbos.record=dbSCRATCH;

	/* used in loop below for dbmatches */
	epat = strtbl("evid",NULL );
	opat = strtbl("orid",NULL );

	/* This assumes the pf that defines the working view here
	includes a sort by evid */
	gtbl=strtbl("evid",NULL );
	dbgrp=dbgroup(dbv,gtbl,0,1);
	rmin = pfget_double(pf,"full_weight_distance");
	dbquery(dbgrp,dbRECORD_COUNT,&nevents);

	for(dbgrp.record=0;dbgrp.record<nevents;++dbgrp.record)
	{
		long is,ie;
		Dbptr db_bundle;
		dbgetv(dbgrp,0,"evid",&evid,"bundle",&db_bundle,NULL );
		dbget_range(db_bundle,&is,&ie);
		for(i=0;i<4;++i) havg[i]=0.0;

		for(dbv.record=is,sumwt=0.0;dbv.record<ie;++dbv.record)
		{
			double sdobs;
			wt=1.0;
			if(use_r_weight)
			{
			    if(dbgetv(dbv,0,"hclat",hc,
				"hclon",hc+1,
				"hcdepth",hc+2,
				"origin.lat",h,
				"origin.lon",h+1,
				"origin.depth",h+2,
				"origin.time",h+3,
				"prefor",&prefor,NULL ) == dbINVALID) 
					get_error(dbv.record);
				/* this accumulates a weighted sum with weights
				set as 1/r where r is distance between hypo and
				they hypocentroid it is associated with.  r has
				a floor to prevent 1/0 problems. */
	
				dist(rad(hc[0]),rad(hc[1]),rad(h[0]),rad(h[1]),
					&dr,&az);
				d=deg2km(deg(dr));
				r=hypot(d,h[2]-hc[2]);
				if(r<rmin)
					wt = 1.0/rmin;
				else
					wt = 1.0/r;
			}
			if(use_ssr_weight)
			{
			    if(dbgetv(dbv,0,
                                "origin.lat",h,
                                "origin.lon",h+1,
                                "origin.depth",h+2,
				"origin.time",h+3,
				"sdobs",&sdobs,
                                "prefor",&prefor,NULL ) == dbINVALID)
					get_error(dbv.record);

			    if(sdobs<=0.0) 
				wt *= 1.0;
			    else
				wt *= 1.0/sdobs;
			}
			sumwt += wt;
			daxpy(4,wt,h,1,havg,1);
		}
		dscal(4,1.0/sumwt,havg,1);	
		orid = dbnextid(db,"orid");
		if(orid<0) elog_die(0,"dbnextid failed to get orid\nThe lastid tables is probably locked or read only\n");
		/* get record number of origin table of current prefor 
		so we can clone it */
		dbputv(dbos,0,"orid",prefor,NULL );
		ierr = dbmatches(dbos,dbo,&opat,&opat,&hooko,&matchlist);
		if(ierr==dbINVALID) elog_die(0,"dbmatches threw an error exception\nRun dbverify on database\n");
		if(ierr>0)
		{
			dbo.record = (long)gettbl(matchlist,0);
			freetbl(matchlist,0);
			dbget(dbo,0);
			dbputv(dbos,0,"orid",orid,
				"lat",havg[0],
				"lon",havg[1],
				"depth",havg[2],
				"time",havg[3],
				"algorithm","pmelavg",NULL );
			dbadd(dbo,0);
			/* Now we set the prefor to new orid */
			dbputv(dbevs,0,"evid",evid,NULL );
			if(dbmatches(dbevs,dbev,&epat,&epat,&hooke,&matchlist))
			{
				dbev.record=(long)gettbl(matchlist,0);
				dbputv(dbev,0,"prefor",orid,NULL );
				freetbl(matchlist,0);
			}
			else
				elog_complain(0,"Match failed for evid %ld in event table\nThis should not happen and indicates the database is probably corrupted\n",
					evid);

		}
		else
		{
			elog_complain(0,"No matching origin found for existing preferred origin = %ld\nDatabase is probably corrupted for this to happen\n",
				prefor);
		}
	}
	exit(0);
}
