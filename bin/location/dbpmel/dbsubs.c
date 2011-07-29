#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "elog.h" 
#include "stock.h"
#include "arrays.h"
#include "pf.h"
#include "db.h"
#include "coords.h"
#include "location.h"
#include "pmel.h"
#include "dbpmel.h"

#ifdef MPI_SET
	#include <sys/time.h>
	#include <mpi.h>
#endif

#define KMPERDEG 111.19

Hypocenter db_load_initial(Dbptr dbv,int row)
{
	Hypocenter h;
	dbv.record = row;
	if(dbgetv(dbv, 0, 
		"origin.lat", &(h.lat),
		"origin.lon", &(h.lon),
		"origin.depth",&(h.z),
		"origin.time", &(h.time),
		NULL ) == dbINVALID)
			elog_die(1,"relocate:  dbgetv error fetching previous location data\nFailure at line %d of database view\n",row);
	/* This initializes parts of the hypocenter stucture that define
        this as an initial location. */
        h.dz = 0.0;
        h.dx = 0.0;
        h.dy = 0.0;
        h.dt = 0.0;
        h.rms_raw = -1.0;
        h.rms_weighted = -1.0;
        h.interquartile = -1.0;
        h.number_data = 0;
        h.degrees_of_freedom = 0;
	h.lat0 = h.lat;
	h.lon0=h.lon;
	h.z0 = h.z;
	h.t0 = h.time;
	return(h);
}
/* This function saves all results for one event group in pmel 
to the database.  This should be faster than manipulating multiple
tables for each event individually.  The algorithm is a loop through
evids with this sequence of events

match row of event with evid, extract prefor
match prefor/evid to get row of origin for prefor
extract row of prefor as pattern to scratch record
set hypocenter and orid fields of scratch record
write new origin to origin table
foreach arrival
	compute travel time residual
	write row to assoc

Note the code is all based on dbmatches.  event table matches
uses the primary keys, but the others are more complex using
mainly alternate keys.

Arguments:
	db - input/output database pointer (can be anything valid,
		dblookup is called up front)
	nevents - number of events in this grouping
	evid - vector of event ids for this group of hevent(s)
	h - vector of Hypocenter objects (length nevents)
	ta - vector of Tbls (length nevents) of Tbls with pointers
		to arrival structures used in genloc.
	sswrodf - group average sswr/ndgf (used to compute error
		ellipses scaled by group misfit)
	o - location options structure passed to ggnloc.  (necessary
		to compute origerr consistent with actual locations)
	pf - pointer to parameter file object

Author:  Gary L Pavlis
Written:  August 2001
This was created when I found my original code stolen from relocate
was too kludgy and too oriented toward saving one event at a time.
This should be a much faster algorithm as it reduces the number of
database calls significantly from a single event algorithm.  
The use of dbmatches is probably safer as well since the input and
output databases are forced to be equal.
Revised:  Feb 2002
Major changes when I recognized that the origin table could not be
linked back to the grid point from which each solution originated.  
Consequently I added an extension table that holds just hypo estimates
with a link back to the parent grid.  
*/

#ifdef BYPASS_DBADD
/*
	fp -- the temperary file pointer where test.assoc data
	      is temperary put, this is only meaningful for 
	      the MPI parallel version. 
*/
int dbpmel_save_results(Dbptr db,
	int nevents,
	long *evid,
	Hypocenter *h,
	Tbl **ta,
	Location_options o,
        Pf *pf, FILE* fp)
#else
int dbpmel_save_results(Dbptr db,
	int nevents,
	long *evid,
	Hypocenter *h,
	Tbl **ta,
	Location_options o,
	Pf *pf)
#endif
{
	Dbptr dbe,dbo,dba,dboe; 
	Dbptr dbes,dbos,dbas;
	Dbptr dbs;  /* subset view created by dblist2subset */
	Tbl *opat,*aspat,*aspat2;
	Tbl *matches=NULL,*match2;  /* output list of matching records */
	/* Dan Q advises it is wise to have a separate hook for each
	table passed through dbmatches */
	Hook *hooke=NULL,*hooko=NULL,*hooka=NULL,*hooka2=NULL;
	long prefor;
	int i,j;
	char *auth;
	Arrival *a;
	long orid;
	int nmatch;
	double **C;
	float emodel[4];
	char *alg=(char *)strdup("dbpmel");
	/* A collection of variables from assoc that have to be 
	copied.  Earlier algorithm using dbget raw on a subset 
	view failed so I have to copy by this mechanism */
	double belief;
	char timedef[2],slodef[2],azdef[2];
	double azres,slores,emares;
	Tbl *utbl;  


	/* This is needed as a stub for the predicted_error function.
	Because we don't support slowness vector data in pmel this 
	has to be an empty (NOT NULL) tbl list */
	utbl=newtbl(0);
	/* All of these are used repeatedly so we do one lookup at the
	top */
	dbe = dblookup(db,0,"event",0,0);
	dbo = dblookup(db,0,"origin",0,0);
	dba = dblookup(db,0,"assoc",0,0);
	dboe = dblookup(db,0,"origerr",0,0);
	dbes = dblookup(db,0,"event",0,0);
	dbos = dblookup(db,0,"origin",0,0);
	dbas = dblookup(db,0,"assoc",0,0);
	dbes.record = dbSCRATCH;
	dbos.record = dbSCRATCH;
	dbas.record = dbSCRATCH;

	opat = strtbl("orid",NULL );
	aspat = strtbl("orid",NULL );
	aspat2 = strtbl("arid","orid","sta",NULL );

	auth = pfget_string(pf,"author");

	C = dmatrix(0,3,0,3);
	/* outer loop over nevents */
	for(i=0;i<nevents;++i)
	{		
    		double conf;
		char *modtype;
		int model;
		double smajax,sminax,strike,sdepth,stime;
		int rc;

		/* Save nothing for events marked no use */
		if(h[i].used==0) continue;  
		/* Start with event table to get prefor */
		dbputv(dbes,0,"evid",evid[i],NULL );
		dbe.record = dbALL;
		if(dbmatches(dbes,dbe,0,0,&hooke,&matches)!=1)
			elog_complain(0,"WARNING:  multiple records in event table have evid=%ld.\nThis is a serious database problem that should be corrected.  Using first one found in table\n",
				evid[i]);
		dbe.record = (long)gettbl(matches,0);
		/* this is excessively paranoid, but better safe than sorry*/
		if(dbe.record<0)
		{
			elog_complain(0,"dbmatches invalid record %ld\nSkip saving evid %ld\n",
				dbe.record,evid[i]);
			continue;
		}
		dbgetv(dbe,0,"prefor",&prefor,NULL );
		freetbl(matches,0);
		matches=NULL;

		dbputv(dbos,0,"orid",prefor,NULL );
		dbo.record = dbALL;
		nmatch=dbmatches(dbos,dbo,&opat,&opat,&hooko,&matches);
		if(nmatch>1)
			elog_complain(0,"WARNING:  multiple records in origin table match orid=%ld.\nThis is a serious database problem that should be corrected.  Using first one found in table\n",
				prefor);
		else if(nmatch<=0)
		{
			elog_complain(0,"Cannot find matching origin\
record for orid %ld prefor of event %ld\n",
				prefor,evid[i]);
		}
		dbo.record = (long)gettbl(matches,0);
		if(dbget(dbo,0)==dbINVALID)
		{
			elog_complain(0,"dbget error on origin table for orid %ld\nData for evid %ld will not be saved\n",
				prefor,evid[i]);
			continue;
		}
		freetbl(matches,0);
		matches=NULL;

		/* origerr code cloned from dbgenloc */
		conf = pfget_double(pf,"confidence");
		modtype = pfget_string(pf,"ellipse_type");
     		if(modtype == NULL)
     		{
        		elog_complain(0,"parameter ellipse_type not defined--default to chi_square");
        		model = CHI_SQUARE;
     		}
     		else if( strcmp( modtype, "chi_square" ) == 0 )
     		{
        		model = CHI_SQUARE;
     		}
     		else if( strcmp( modtype, "F_dist" ) == 0 )
     		{
        		model = F_DIST;
     		}
     		else
     		{
        		elog_complain(0, "parameter ellipse_type %s incorrect (must be F_dist or chi_square)--default to chi_square", modtype );
        		model = CHI_SQUARE;
     		}
		predicted_errors(h[i],ta[i],utbl,o,C,emodel);
    		rc = project_covariance( C, model, &conf,
                             h[i].rms_weighted, h[i].degrees_of_freedom,
                             &smajax, &sminax, &strike, &sdepth, &stime );

    		if( rc != 0 )
    		{
        		elog_complain(0, "project_covariance failed." );
        		smajax = -1;
        		sminax = -1;
        		strike = -1;
        		sdepth = -1;
        		stime = -1;
        		conf = 0.;
    		}
		orid = dbnextid(dbo,"orid");

		if(dbaddv(dboe,0,
                	"orid", orid,
                	"sxx",C[0][0],
                	"syy",C[1][1],
                	"szz",C[2][2],
                	"stt",C[3][3],
                	"sxy",C[0][1],
                	"sxz",C[0][2],
                	"syz",C[1][2],
                	"stx",C[0][3],
                	"sty",C[1][3],
                	"stz",C[2][3],
                	"sdobs", h[i].rms_raw,
                	"smajax", smajax,
                	"sminax", sminax,
                	"strike", strike,
                	"sdepth", sdepth,
                	"stime", stime,
                	"conf", conf,
                		NULL ) < 0 )
		{
			elog_complain(0,"Problem adding origerr record for evid %ld\n",
				evid[i]);
		} 


		/* This edits the scratch record and adds it to the end 
		of the origin table */
		dbputv(dbos,0,"orid",orid,
				"lat",h[i].lat,
				"lon",h[i].lon,
				"depth",h[i].z,
				"time",h[i].time,
				"algorithm",alg,
				"auth",auth,NULL );

/*
		printf("Added to origin table: %8d %9.4lf %9.4lf %9.4lf"
			" %17.5lf %-15s %-15s\n", orid, h[i].lat, 
			h[i].lon, h[i].z, h[i].time, alg, auth);
*/

		if(dbadd(dbo,0)==dbINVALID)
			elog_complain(0,"dbadd error for origin table\
for new orid %ld of evid %ld\n",
				orid,evid[i]);
		/* The assoc tables is much more complex.  Rather than
		do a row for row match against each arrival in ta, I
		use a staged reduction.  Than is I first match the full
		assoc table against orid=prefor only to get a full list
		of records for that event.  We then run matches against
		the smaller table for sta/phase in the arrival list.
		Untested but from previous experience with dbmatches on
		large tables I'm fairly sure this is a necessary 
		complication to keep this from be very slow*/
		dbputv(dbas,0,"orid",prefor,NULL );
		dba.record = dbALL;
		nmatch=dbmatches(dbas,dba,&aspat,&aspat,&hooka,&matches);
		if(nmatch<=0)
		{
			elog_complain(0,"No assoc records for orid %ld\
found\nFail to create new assoc records for orid %ld\n",
				prefor,orid);
			freetbl(matches,0);
			continue;
		}
		dbs = dblist2subset(dba,matches);
	
		for(j=0;j<maxtbl(ta[i]);++j)
		{
			double delta,seaz,esaz,wgt;

			a = (Arrival *)gettbl(ta[i],j);


			/* This test is more rigorous than the actual
			key for assoc (arid:orid), but it is safer */
			dbputv(dbas,0,
				"arid",a->arid,
				"orid",prefor,
				"sta",a->sta->name,NULL );
			dbs.record = dbALL;
			nmatch =dbmatches(dbas,dbs,&aspat2,&aspat2,&hooka2,&match2);
			if(nmatch<=0)

			{
				elog_complain(0,"Cannot find station %s arrival for phase %s in assoc table for evid %ld.\nArrival will not be associated\n",
					a->sta->name,a->phase->name,evid[i]);
				continue;
			}
			else if(nmatch>1)
			{
				elog_complain(0,"Warning(save_results):  multiple rows in assoc match station %s and phase %s for evid %ld\nCloning first found\n",
					a->sta->name,a->phase->name,evid[i]);
			}


			dbs.record = (long)gettbl(match2,0);
			/* These variables have to be copied so we
			fetch them here */
			if(dbgetv(dbs,0,"belief",&belief,
				"timedef",timedef,
				"slodef",slodef,
				"azdef",azdef,
				"azres",&azres,
				"slores",&slores,
				"emares",&emares,NULL )
						==dbINVALID)
			{
				elog_complain(0,"dbgetv error on assoc table for orid %ld\nData for evid %ld will not be saved\n",
				    prefor,evid[i]);
				continue;
			}

			freetbl(match2,0);
			match2=NULL;
			/* It may be unnecessary to recompute these, but
			the cost is not high */
        		dist(rad(h[i].lat),rad(h[i].lon),
				rad(a->sta->lat),rad(a->sta->lon), 
				&delta,&esaz);
        		dist(rad(a->sta->lat),rad(a->sta->lon),
				rad(h[i].lat),rad(h[i].lon), 
				&delta,&seaz);

        		wgt = (double)((a->res.weighted_residual)
				/(a->res.raw_residual));

#ifdef BYPASS_DBADD
		/*
		    Using fprintf to directly write to the temperary
		    file instead of using dbaddv to test.assoc
		    since this is much faster (NOTE, we assume in 
		    this case that the rows inserted all have 
		    unique primary keys.)
		*/
                fprintf(fp, "%8d %8d %-6s %-8s %4.2lf %8.3lf %7.2lf "
                        "%7.2lf %8.3lf %-1s %7.1lf %-1s %7.2lf %-1s "
                        "%7.1lf %6.3lf %-15s %8d %17.5lf\n",
                        a->arid, orid, a->sta->name, a->phase->name,
                        belief, deg(delta), deg(seaz), deg(esaz),
                        (double) a->res.raw_residual, timedef,
                        azres, azdef, slores, slodef, emares, wgt, "-",
                        -1, str2epoch("now"));		
#else
			if(dbaddv(dbas,0,
				"arid",a->arid,
				"orid",orid,
				"sta",a->sta->name,
				"phase",a->phase->name,
				"belief",belief,
                                "timedef",timedef,
                                "slodef",slodef,
                                "azdef",azdef,
                                "azres",azres,
                                "slores",slores,
                                "emares",emares,
				"delta",deg(delta),
				"seaz",deg(seaz),
				"esaz",deg(esaz),
				"timeres",(double) a->res.raw_residual,
				"wgt",wgt,NULL )==dbINVALID)
					elog_complain(0,
					"Error adding to assoc table\
 for arid %ld, orid %ld, evid %ld\n",
					a->arid,orid,evid[i]);
			dbadd(dba,0);

#endif

		}


		dbfree(dbs);
		free_hook(&hooka2);

	}


	free_hook(&hooke);
	free_hook(&hooko);
	free_hook(&hooka);
	free_matrix((char **)C,0,3,0);
	freetbl(utbl,0);

	return(0);
}
/* Saves results for one group of events tagged to gridid 
to an extension table containing station correction estimates for
that gridid.  db is the database pointer to save results to,
s is the huge internal structure used by dbpmel, and pf is
the parameter space object.  
*/
void dbpmel_save_sc(int gridid, Dbptr db,SCMatrix *s,Pf *pf)
{
	char *pmelrun,*gridname;
	int *iptr;
	int phacol;
	Tbl *phskeys,*stakeys;
	int i,j,icol;
	char *phase,*sta;
	
	/* we assume these have been verified to be in pf through 
	check_required_pf*/
	pmelrun=pfget_string(pf,"pmel_run_name");
	gridname = pfget_string(pf,"gridname");
	
        phskeys = keysarr(s->phase_index);
        stakeys = keysarr(s->sta_index);
        db = dblookup(db,0,"gridscor",0,0);

        for(j=0;j<maxtbl(phskeys);++j)
        {
        	phase = (char *)gettbl(phskeys,j);
		iptr = (int *)getarr(s->phase_index,phase);
		phacol = *iptr;
		for(i=0;i<maxtbl(stakeys);++i)
		{
			sta = (char *)gettbl(stakeys,i);
			iptr = (int *)getarr(s->sta_index,sta);
			icol = phacol + (*iptr);
			/* This trap is probably superfluous, but better to
			be paranoid */
			if((icol>=0) && (icol<s->ncol))
				dbaddv(db,0,"sta",sta,
					"phase",phase,
					"gridname",gridname,
					"gridid",gridid,
					"pmelrun",pmelrun,
					"tsc",s->sc[icol],
					"tscref",s->scref[icol],
					"tscbias",s->scbias[icol],NULL );
			else
				elog_die(0,"dbpmel_save_sc:  computed index %d for accessing station correction vector is outside range of 1 to %d\nProbably access violation making continuation ill advised\n",
					icol+1,s->ncol);
		}
	}
	freetbl(phskeys,0);
	freetbl(stakeys,0);
}
			
	
