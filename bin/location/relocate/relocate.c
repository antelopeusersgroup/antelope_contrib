#include <stdio.h>
#include <stdlib.h>
#include <string.h>
 
#include "stock.h"
#include "arrays.h"
#include "pf.h"
#include "db.h"
#include "coords.h"
#include "location.h"

#define KMPERDEG 111.19

void usage()
{
	fprintf(stderr,"Usage:  relocate dbin dbout [-pf pf -sift expression -useold]\n");
	exit(1);
}
/* Special function to build an initial hypocenter estimate from
current prefor origin. It is assumed that dbv points to a 
database view containing a join of assoc->arrival->site->origin->event.
row  can be set to any valid row defining the present event because
in this view every row for a given event has the same origin table
entries.  

Author:  Gary L. Pavlis
Written:  January 1997
*/ 
#define UNIQUE 2  /* magic number for dbsort sort unique */

Hypocenter db_load_initial(Dbptr dbv,int row)
{
	Hypocenter h;
	dbv.record = row;
	if(dbgetv(dbv, 0, 
		"origin.lat", &(h.lat),
		"origin.lon", &(h.lon),
		"origin.depth",&(h.z),
		"origin.time", &(h.time),
		0) == dbINVALID)
			die(1,"relocate:  dbgetv error fetching previous location data\nFailure at line %d of database view\n",row);
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

/* This is a group of procedures to update a set of css3.0 
database tables that are changed in a relocation.  Some information
is copied, and some is updated.  It is kind of ugly, but a necessary
evil with this particular schema. */

/* save_origin adds a single record for each event to the origin
table and returns the orid of this event returned by dbnextid
for the output table.  

Arguments:

dbi - input database pointer of complex view used internally in
	this program 
is, ie - range of rows of dbi containing data for this event
depth_fixed - set true when fixed depth solution is used 
hypos - hypocenter object for relocated solution
dbo - output db pointer.  

Function returns the orid assigned to this event from dbnextid

Author:  Gary L. Pavlis
Written:  February 1997
*/

int save_origin(Dbptr dbi, int is, int ie, int depth_fixed,
	Hypocenter h, Dbptr dbo)
{
	int orid;

	/* These are parameters copied from input db -- names = css names */
	int evid;
	int jdate;
	int grn;
	int srn;
	char etype[8];
	double mb;
	int mbid;
	double ms;
	int msid;
	double ml;
	int mlid;
	/* These obtained from this solution */

	int nass;
	int ndef;
	char dtype[2];
	char algorithm[16]="genloc-nlls";
	char *auth;
	double lddate;
	

	/* these are intentionally left null: ndp,depdp,commid


	/* set but obtained directly from hypo structure 
	lat, lon, depth, time */

	dbo = dblookup(dbo,0,"origin",0,0);
	
	/* Grab what we need from the input db for copying.  Note
	that because the joins used here, each of the input db
	records have the same origin entries.  Thus, we just fetch
	stuff from row is.  */
	dbi.record = is;
	if( dbgetv(dbi,0,
		"origin.evid", &evid,
		"origin.jdate", &jdate,
		"origin.grn", &grn,
		"origin.srn", &srn,
		"origin.etype", etype,
		"origin.mb", &mb,
		"origin.mbid", &mbid,
		"origin.ms", &ms,
		"origin.msid", &msid,
		"origin.ml", &ml,
		"origin.mlid", &mlid,
				0) == dbINVALID)
	{
		die(1,"save_origin: dbgetv error reading origin fields of input view at record %d\n",
				is);
	}
	nass = ie - is + 1;
	/* ndef is potentially wrong by this calculation.  We set ndef 
	from the number of degrees of freedom in the solution + npar
	where npar is 3 when depth is fixed and 4 otherwise.  This does
	not match the real definition of ndef, especially when array 
	data are used because each arrival then then has 3 data points
	used to constrain the solution.  These is either an error in 
	my (glp) understanding of the css3.0 schema or a flaw in its 
	design.*/
	if(depth_fixed)
	{
		ndef = h.degrees_of_freedom + 3;
		strcpy(dtype,"r");
	}
	else
	{
		ndef = h.degrees_of_freedom + 4;
		strcpy(dtype,"f");
	}
	auth = cuserid(NULL);
	lddate = now();
	orid = dbnextid(dbo,"orid");
	if(dbaddv(dbo,0,
                "lat",h.lat,
                "lon",h.lon,
                "depth",h.z,
                "time",h.time,
                "orid",orid,
                "evid",evid,
                "jdate",jdate,
                "nass",nass,
                "ndef",ndef,
                "grn",grn,
                "srn",srn,
                "etype",etype,
                "dtype",dtype,
                "mb",mb,
                "mbid",mbid,
                "ms",ms,
                "msid",msid,
                "ml",ml,
                "mlid",mlid,
                "algorithm",algorithm,
                "auth",auth,
                "lddate",lddate,
			0) == dbINVALID)
	{
		die(1,"save_origin: dbaddv error writing orid %d\n",
				orid);
	}
	return(orid);
}
/* This functions adds one new record to output db event table.  

Arguments:
	dbi - input db view -- event table is same for each row of
		this view.  looks only at the first
	is, ie - range of rows of dbi containing data for this event
	orid - orid for this event set to prefor in event table
		It has to be previously define when origin table entry
		is made, so we pass it in this way.
	dbo - output db 

Author:  Gary L. Pavlis
Written:  February 1997
*/

void save_event(Dbptr dbi, int is, int ie, int orid, Dbptr dbo)

{
	/* these are variables in dbi copied to dbo */
	int evid;
	char evname[16];
	
	/*altered in output by this program */
	int prefor;
	char *auth;
	double lddate;

	/* intentionally ignored:  commid */

	dbo = dblookup(dbo,0,"event",0,0);
	dbi.record = is;
	if( dbgetv(dbi,0,
		"event.evid",&evid,
		"event.evname",evname,	
				0) == dbINVALID)
	{
		die(1,"save_event: dbgetv error reading event fields of input view at record %d\n",
				is);
	}
	prefor = orid;
	auth = cuserid(NULL);	
	lddate = now();
	if(dbaddv(dbo,0,
                "evid",evid,
		"evname",evname,
		"prefor",prefor,
                "auth",auth,
                "lddate",lddate,
			0) == dbINVALID)
	{
		die(1,"save_event: dbaddv error writing event record for orid %d\n",
				orid);
	}
}
/*This function adds a single row to the origerr table for this event.

arguements:
	orid - orid assign to this solution
	h - Hypocenter object for this solution
	dbo - output db

This version doesn't set much of this large table.  Eventually, it 
needs to include all the error terms, but that function is not 
written yet. 

Author:  Gary L. Pavlis
Written:  February 1997
*/
void save_origerr(int orid, Hypocenter h, Dbptr dbo)
{
	double sdobs; 
	double lddate;
	/* Intentionally ignored:  sxx, syy, szz, stt, sxy, sxz, syz, stx,sty,
					stz, smajax,sminax,strike,sdepth,stime,
					conf,commid */

	dbo = dblookup(dbo,0,"origerr",0,0);

	/* Bad news here is that we can't save the more useful 
	statistics that this program calculates.  css3.0 only allows
	sdobs = sswr/ndgf */

	sdobs = h.rms_raw;
	lddate = now();
	if(dbaddv(dbo,0,
		"orid",orid,
		"sdobs",sdobs,
                "lddate",lddate,
			0) == dbINVALID)
	{
		die(1,"save_origerr: dbaddv error writing origerr record for orid %d\n",
				orid);
	}
}
/*This function forms a new assoc table for this solution using 
input view, dbv, from is to ie (rows) as a pattern.  This is 
more complex than it's siblings above because of the need to deal
with residuals.  It gets especially ugly because of the fact that
I insist that slowness vectors use components rather than the
polar form.  sigh.
 
arguments:
	dbi - input db view -- event table is same for each row of
		this view.  looks only at the first
	is, ie - range of rows of dbi containing data for this event
	orid - output db orid for this solution
	residual - Tbl from ggnloc of residuals
	h - Hypocenter object for this solution
	dbo - output db

This version doesn't set much of this large table.  Eventually, it 
needs to include all the error terms, but that function is not 
written yet. 

Author:  Gary L. Pavlis
Written:  February 1997
*/
#define TIMENULL -999.000
void save_assoc(Dbptr dbi, int is, int ie, int orid, char *vmodel,
	Tbl *residual,Hypocenter h, Dbptr dbo)
{
	/* These fields are copied from input assoc table */
	int arid;
	char sta[8];
	char phase[10];
	double belief;
	/* These fields are set here */
	double delta;
	double seaz;
	double esaz;
	double timeres;
	double azres;
	double slores;
	double lddate;
	double wgt;
	char timedef[2],slodef[2], azdef[2];
	
	/* intentionally ignored:  emares, commid */


	/* passed through arg list;  orid*/

	/* We use this to produce a keyed arr list of the residual 
	list passed into here as a Tbl */

	Arr *residual_array;
	int i;
	char key[40]; 

	double r, w, reswt,uxresid, uyresid;
	double stalat, stalon; 
	double ux, uy, azimuth;
	double u,phi;  /* polar form of measured slowness vector */
	double duphi;

	dbo = dblookup(dbo,0,"assoc",0,0);
	lddate = now();

	/* We build an associate array for the residual tbl keying
	each entry with a sta/phase/type key  where type is 
	set in ggnloc as time, ux, or uy.  This complication is
	needed to sort out array residuals. */
	residual_array = newarr(0);
	for(i=0;i<maxtbl(residual);++i)
	{
		char *s;
		char keysta[10], keyphase[10], keytype[5];
		s = (char *)gettbl(residual,i);
		sscanf(s,"%s %s %s",keysta,keyphase,keytype);
		sprintf(key,"%s %s %s",keysta,keyphase,keytype);
		setarr(residual_array,key,s);
	}
		

	for(dbi.record=is;dbi.record < ie;++dbi.record)
	{
		char *time_residual_record;
		char *ux_residual_record,*uy_residual_record;
		if( dbgetv(dbi,0,
      	    		"assoc.arid",&arid,
          		"assoc.sta",sta,
          		"assoc.phase",phase,
          		"assoc.belief",&belief,
				0) == dbINVALID)
		{
		  die(1,"save_assoc: dbgetv error reading assoc fields of input view at record %d\n",
				dbi.record);
		}
		if( dbgetv(dbi,0,
			"site.lat",&stalat,
			"site.lon",&stalon,
				0) == dbINVALID)
		{
		  die(1,"save_assoc: dbgetv error reading site fields of input view at record %d\n",
				dbi.record);
		}
		/* Find the time residual record for this arrival */
		sprintf(key,"%s %s time",sta,phase);
		time_residual_record = (char *)getarr(residual_array,key);
		if(time_residual_record == NULL)
		{
			complain(1,"save_assoc:  getarr mismatch for key %s\nCannot set residual\n",key);
			timeres = TIMENULL;
			wgt = 0.0;
			strcpy(timedef,"n");
		}
		else
		{
			sscanf(time_residual_record,"%*s%*s%*s%*lg%lg%lg%lg",
				&r,&w,&reswt);
			timeres = r;
			wgt = w*reswt;
			strcpy(timedef,"d");
		}

		sprintf(key,"%s %s ux",sta,phase);		
		ux_residual_record = (char *)getarr(residual_array,key);
		sprintf(key,"%s %s uy",sta,phase);
		uy_residual_record = (char *)getarr(residual_array,key);
		if( (ux_residual_record == NULL) 
			|| (ux_residual_record == NULL))
		{
			/* This trick is not documented.  By setting 
			the record filed to dbNULL, and then calling dbgetv
			each of the fields will be set to their NULL value */
			dbo.record = dbNULL;
			dbgetv(dbo,0,"azres",&azres,"slores",&slores,0);
			strcpy(azdef,"n");
			strcpy(slodef,"n");
		}
		else
		{
		/* This gets nasty because we have to convert to polar 
		coordinates from ux, uy components */
			sscanf(ux_residual_record,"%*s%*s%*s%*g%lg",&uxresid);

			sscanf(uy_residual_record,"%*s%*s%*s%*g%lg",&uyresid);
		/* We fetch the measured slowness vector to convert */
			if( dbgetv(dbi,0,
				"arrival.slow",&u,
				"arrival.azimuth",&phi,
					0) == dbINVALID)
			{
		  	  die(1,"save_assoc: dbgetv error reading arrival fields of input view at record %d\n",
				dbi.record);
			}
			/* css stores slowness in s/deg, but we use
			s/km internally here so we have to convert */

			slores = sqrt(uxresid*uxresid+uyresid*uyresid);
			slores *= KMPERDEG;

			/* this is the azimuth term */
			u /= KMPERDEG;
			duphi = ux*cos(rad(azimuth)) - uy*sin(rad(azimuth));
			duphi /= u;
			azres = deg(duphi);
			strcpy(azdef,"d");
			strcpy(slodef,"d");

		}
		dist(rad(h.lat),rad(h.lon),rad(stalat),rad(stalon),
				&delta,&esaz);
		dist(rad(stalat),rad(stalon),rad(h.lat),rad(h.lon),
				&delta,&seaz);
		delta = deg(delta);
		seaz = deg(seaz);
		esaz = deg(esaz);
			
		if(dbaddv(dbo,0,
                        "arid",arid,
                        "orid",orid,
                        "sta",sta,
                        "phase",phase,
                        "belief",belief,
                        "delta",delta,
                        "seaz",seaz,
                        "esaz",esaz,
                        "timeres",timeres,
                        "timedef",timedef,
                        "azres",azres,
                        "azdef",azdef,
                        "slores",slores,
                        "slodef",slodef,
                        "wgt",wgt,
                        "vmodel",vmodel,
                        "lddate",lddate,
			0) == dbINVALID)
		{
			die(1,"save_assoc: dbaddv error writing assoc record for arid %d\n",
				arid);
		}
	}
	freearr(residual_array,free);
}

	
int main(int argc, char **argv)
{
	char *version="1.0 (1/23/96)";
	char *dbin;  /* Input db name */
	char *dbout;  /* output db name */
	Dbptr db;  /* input db pointer */
	Dbptr dbo;  /* base output db pointer */
	Dbptr dbv;  /* set to view formed by join */
	char *pfin=NULL;  /* input parameter file */
	char *sift_exp;  /* sift expression for subset */
	int sift = 0;  /* default is no sift.  */
	Tbl *sortkeys;
	/*Pointers to views returned by dbgroup (keyed to origin and event
	respectively */
	Dbptr dborigin_group;
	Tbl *origin_group;  /* relation keys used in grouping*/
	int nevents;
	/* db row variables */
	int rows_in_view;
	int evid,orid,prefor;
	int nrows, nrows_raw;

	int useold=0;
	Pf *pf;
	Tbl *ta,*tu;
	Tbl *reason_converged, *residual;
	Location_options o;
	Arr *arr_phase;
	int i;
	Tbl *converge_history;
	char *line;

	Hypocenter h0;
	Hypocenter *hypos;
	int niterations;

	char *vmodel;

	int ret_code;  /* ggnloc return code */

	if(argc < 3) usage();
	dbin = argv[1];
	dbout = argv[2];
	for(i=3;i<argc;++i)
	{
		if(!strcmp(argv[i],"-pf"))
		{
			++i;
			if(i>=argc) usage();
			pfin = argv[i];
		}
		else if(!strcmp(argv[i],"-sift"))
		{
			++i;
			if(i>=argc) usage();
			sift_exp = argv[i];
			sift = 1;
		}
		else if(!strcmp(argv[i],"-useold"))
			useold = 1;
		else
			usage();
	}
	/* set default this way*/
	if(pfin == NULL) pfin = strdup("relocate");


	/* Initialize the error log and write a version notice */
	elog_init (argc, argv) ;
	elog_notify (0, "%s version %s\n", argv[0], version) ;

	/* Alway join assoc, arrival, and site.  We join site 
	to make sure station table is properly dynamic to account for
	time changes.  With this setup, the stations can even move
	around and this should still work.*/


	if(dbopen(dbin,"r",&db) == dbINVALID) 
		die(1,"Unable to open input database %s\n",dbin);
	if(dbopen(dbout,"r+",&dbo) == dbINVALID) 
		die(1,"Unable to open output database %s\n",dbout);

	dbv = dbjoin ( dblookup(db,0,"event",0,0),
		dblookup(db,0,"origin",0,0),
		0,0,0,0,0);
	if(dbv.table == dbINVALID)
		die(1,"event->origin join failed\n");
	dbv = dbjoin ( dbv, dblookup(db,0,"assoc",0,0),
			0,0,0,0,0);
	if(dbv.table == dbINVALID)
		die(1,"event->origin->assoc join failed\n");
	dbv = dbjoin ( dbv, dblookup(db,0,"arrival",0,0),
			0,0,0,0,0);
	if(dbv.table == dbINVALID)
		die(1,"event->origin->assoc->arrival join failed\n");
	dbv = dbjoin ( dbv, dblookup(db,0,"site",0,0),
			0,0,0,0,0);
	if(dbv.table == dbINVALID)
		die(1,"event->origin->assoc->arrival->site join failed\n");

	/* Subset using sift_key if requested */
	if(sift)
	{
		dbv = dbsubset(dbv,sift_exp,0);
		if(dbv.record == dbINVALID)
			die(1,"dbsubset of %s with expression %s failed\n",
				dbin, sift_exp);
	}
	/* This keeps only the prefered origin records intact */
	dbv = dbsubset(dbv,"orid == prefor", 0);
	if(dbv.record == dbINVALID)
			die(1,"Subset to preferred origin records failed\n");

	/* First we have to run a unique key sort in the following order
	to remove redundant picks made on multiple channels.  We will
	issue a warning if the record count changes. */
	dbquery(dbv, dbRECORD_COUNT, &nrows_raw);
	sortkeys = newtbl(0);
	pushtbl(sortkeys,"evid");
	pushtbl(sortkeys,"sta");
	pushtbl(sortkeys,"phase");
	dbv = dbsort(dbv,sortkeys,UNIQUE,0);
	dbquery(dbv, dbRECORD_COUNT, &nrows);
	if(nrows != nrows_raw)
		complain(0,"Input database has duplicate picks of one or more phases on multiple channels\n\
Which picks will be used here is unpredictable\n\
%d total picks, %d unique\nContinuing\n", nrows_raw, nrows);

	/* This sort is the required one for the grouping that follows*/

	sortkeys = newtbl(3);
	pushtbl(sortkeys,"evid");
	pushtbl(sortkeys,"orid");
	pushtbl(sortkeys,"time");
	dbv = dbsort(dbv,sortkeys,0,0);
	if(dbv.record == dbINVALID)
		die(1,"dbsort on evid,orig,time failed\n");

	/* Set up grouping by events */
	origin_group = newtbl(0);
	pushtbl(origin_group, "evid");
	dborigin_group = dbgroup(dbv, origin_group, "origin_group",1);
	if(dborigin_group.record == dbINVALID)
		die(1,"dbgroup by origin failed\n");

	dbquery(dborigin_group,dbRECORD_COUNT,&nevents);
	elog_notify(0,"Attempting to relocate %d events in subsetted database\n",
		nevents);
	

	/* DB is now set up correctly, now we turn to the parameter files */
	i = pfread(pfin,&pf);
	if(i != 0) die(1,"Pfread error\n");

	o = parse_options_pf (pf);
 	arr_phase = parse_phase_parameter_file(pf);
	vmodel = pfget_string(pf,"velocity_model_name");
	fprintf(stdout,"lat lon depth time rms wrms interquartile ndata ndgf iterations\n");

	/* Main loop.  We utilize the group views and loop through by 
	events */
	for(dborigin_group.record=0;
		dborigin_group.record< nevents;++dborigin_group.record)
	{
		Dbptr db_bundle;  /* db pointer returned from bundle field 
				of dborigin_group for current event */
		Arr *station_table;
		Arr *array_table;
		int is, ie; 
		int orid;  /* orid assigned relocated event in output db */

		if(dbgetv(dborigin_group,0,"evid", &evid,
			"bundle", &db_bundle,0) == dbINVALID)
			complain(1,"dbgetv error for row %d of event group\n",
				dborigin_group.record);
		dbget_range(db_bundle,&is,&ie);

		station_table = dbload_station_table(dbv,
						is,ie,pf);
		array_table = dbload_array_table(dbv,
						is,ie,pf);
		ta = dbload_arrival_table(dbv,
				is,ie,station_table, arr_phase);
		tu = dbload_slowness_table(dbv,
				is,ie,array_table, arr_phase);
		if(useold)
			h0 = db_load_initial(dbv,is);
		else
			h0 = initial_locate(ta, tu, o, pf);

		ret_code = ggnloc(h0,ta,tu,o,
			&converge_history,&reason_converged,&residual);
			
		if(ret_code < 0)
		{
			complain(1,"ggnloc failed to produce a solution\n");
		}
		else 
		{
			if(ret_code > 0)
			    complain(1,"%d travel time calculator failures in ggnloc\nSolution ok\n",
				ret_code);
			
			niterations = maxtbl(converge_history);
	
			hypos = (Hypocenter *)gettbl(converge_history,
								niterations-1);
	
			fprintf(stdout,"%lf %lf %lf %lf %g %g %g %d %d\n",
					hypos->lat,hypos->lon,hypos->z,hypos->time,
					hypos->rms_raw, hypos->rms_weighted,
					hypos->interquartile,
					hypos->degrees_of_freedom,
					niterations);
	
			orid = save_origin(dbv,is,ie,o.fix[3],*hypos,dbo);
			save_event(dbv,is,ie,orid,dbo);
			save_origerr(orid,*hypos,dbo);
			save_assoc(dbv,is,ie,orid,vmodel,residual,*hypos,dbo);
		}
	
		if(maxtbl(converge_history)>0)freetbl(converge_history,free);
		if(maxtbl(reason_converged)>0)freetbl(reason_converged,free);
		if(maxtbl(residual)>0)freetbl(residual,free);
		destroy_data_tables(tu, ta);
		destroy_network_geometry_tables(station_table,array_table);
	}
}

/* $Id$ */
