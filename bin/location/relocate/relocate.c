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

Hypocenter db_load_initial(Dbptr dbv,long row)
{
	Hypocenter h;
	dbv.record = row;
	if(dbgetv(dbv, 0, 
		"origin.lat", &(h.lat),
		"origin.lon", &(h.lon),
		"origin.depth",&(h.z),
		"origin.time", &(h.time),
		NULL) == dbINVALID)
			elog_die(1,"relocate:  dbgetv error fetching previous location data\nFailure at line %ld of database view\n",row);
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

long save_origin(Dbptr dbi, long is, long ie, int depth_fixed,
	Hypocenter h, Dbptr dbo)
{
	long orid;

	/* These are parameters copied from input db -- names = css names */
	long evid;
	long jdate;
	long grn;
	long srn;
	char etype[8];
	double mb;
	long mbid;
	double ms;
	long msid;
	double ml;
	long mlid;
	/* These obtained from this solution */

	long nass;
	long ndef;
	char dtype[2];
	char algorithm[16]="genloc-nlls";
	char auth[20];
	double lddate;
	

	/* these are intentionally left null: ndp,depdp,commid*/


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
				NULL) == dbINVALID)
	{
		elog_die(1,"save_origin: dbgetv error reading origin fields of input view at record %ld\n",
				is);
	}
	nass = ie - is;
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
	my_username(auth);
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
			NULL) == dbINVALID)
	{
		elog_die(1,"save_origin: dbaddv error writing orid %ld\n",
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

/* Changed to int from void by JN in order to return evid. */
long save_event(Dbptr dbi, long is, long ie, long orid, Dbptr dbo)

{
	/* these are variables in dbi copied to dbo */
	long evid;
	char evname[16];
	
	/*altered in output by this program */
	long prefor;
	char auth[20];
	double lddate;

	/* intentionally ignored:  commid */

	dbo = dblookup(dbo,0,"event",0,0);
	dbi.record = is;
	if( dbgetv(dbi,0,
		"event.evid",&evid,
		"event.evname",evname,	
				NULL) == dbINVALID)
	{
		elog_die(1,"save_event: dbgetv error reading event fields of input view at record %ld\n",
				is);
	}
	prefor = orid;
	my_username(auth);
	lddate = now();
	if(dbaddv(dbo,0,
                "evid",evid,
		"evname",evname,
		"prefor",prefor,
                "auth",auth,
                "lddate",lddate,
			NULL ) == dbINVALID)
	{
		elog_die(1,"save_event: dbaddv error writing event record for orid %ld\n",
				orid);
	}
        /* Added by JN */
	return(evid);
}
/*This function adds a single row to the origerr table for this event.

arguments:
	orid - orid assign to this solution
	h - Hypocenter object for this solution
	dbo - output db

This version doesn't set much of this large table.  Eventually, it 
needs to include all the error terms, but that function is not 
written yet. 

Author:  Gary L. Pavlis
Written:  February 1997
*/
void save_origerr(long orid, Hypocenter h, double **C, Dbptr dbo)
{
	double sdobs; 
	double lddate;
	/* Intentionally ignored: smajax,sminax,strike,sdepth,stime,
					conf,commid */

	dbo = dblookup(dbo,0,"origerr",0,0);

	/* Bad news here is that we can't save the more useful 
	statistics that this program calculates.  css3.0 only allows
	sdobs = sswr/ndgf */

	sdobs = h.rms_raw;
	lddate = now();
	if(dbaddv(dbo,0,
		"orid",orid,
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
		"sdobs",sdobs,
                "lddate",lddate,
			NULL) == dbINVALID)
	{
		elog_die(1,"save_origerr: dbaddv error writing origerr record for orid %ld\n",
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
void save_assoc(Dbptr dbi, long is, long ie, long orid, char *vmodel,
	Tbl *residual,Hypocenter h, Dbptr dbo)
{
	/* These fields are copied from input assoc table */
	long arid;
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
	long i;
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
		/* handle S-P case by having the same residual mapped
		to each half of - phase pair */
		if(strchr(keyphase,'-'))
		{
			char *phase1,*phase2;
			/* algorithm to split phase names cloned from dbgenloc */
			phase1 = strdup(keyphase);
			phase2= strchr(phase1,'-');
			*phase2 = '\0';
			++phase2;
			sprintf(key,"%s %s %s",keysta,phase1,keytype);
			setarr(residual_array,key,s);
                        sprintf(key,"%s %s %s",keysta,phase2,keytype);
                        setarr(residual_array,key,s);
			free(phase1);
		}
		else
		{
			/* normal phases are one to one */
			sprintf(key,"%s %s %s",keysta,keyphase,keytype);
			setarr(residual_array,key,s);
		}
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
				NULL) == dbINVALID)
		{
		  elog_die(1,"save_assoc: dbgetv error reading assoc fields of input view at record %ld\n",
				dbi.record);
		}
		if( dbgetv(dbi,0,
			"site.lat",&stalat,
			"site.lon",&stalon,
				NULL) == dbINVALID)
		{
		  elog_die(1,"save_assoc: dbgetv error reading site fields of input view at record %ld\n",
				dbi.record);
		}
		/* Find the time residual record for this arrival */
		sprintf(key,"%s %s time",sta,phase);
		time_residual_record = (char *)getarr(residual_array,key);
		if(time_residual_record == NULL)
		{
			elog_complain(1,"save_assoc:  getarr mismatch for key %s\nCannot set residual\n",key);
			timeres = TIMENULL;
			wgt = 0.0;
			strcpy(timedef,"n");
		}
		else
		{
                        /* Changed by JN to avoid gcc warning */
			/* sscanf(time_residual_record,"%*s%*s%*s%*lg%lg%lg%lg", */
			sscanf(time_residual_record,"%*s%*s%*s%*g%lg%lg%lg",
				&r,&w,&reswt);
			timeres = r;
			wgt = w*reswt;
			strcpy(timedef,"d");
		}

		sprintf(key,"%s %s ux",sta,phase);		
		ux_residual_record = (char *)getarr(residual_array,key);
		sprintf(key,"%s %s uy",sta,phase);
		uy_residual_record = (char *)getarr(residual_array,key);
                /* Corrected by JN
		if( (ux_residual_record == NULL) 
			|| (ux_residual_record == NULL))
                */
		if( (ux_residual_record == NULL) 
			|| (uy_residual_record == NULL))
		{
			/* This trick is not documented.  By setting 
			the record filed to dbNULL, and then calling dbgetv
			each of the fields will be set to their NULL value */
			dbo.record = dbNULL;
			dbgetv(dbo,0,"azres",&azres,"slores",&slores,NULL );
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
					NULL) == dbINVALID)
			{
		  	  elog_die(1,"save_assoc: dbgetv error reading arrival fields of input view at record %ld\n",
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
			NULL ) == dbINVALID)
		{
			elog_die(1,"save_assoc: dbaddv error writing assoc record for arid %ld\n",
				arid);
		}
	}
	freearr(residual_array,0);
}
	
int main(int argc, char **argv)
{
	char *dbin;  /* Input db name */
	char *dbout;  /* output db name */
	Dbptr db;  /* input db pointer */
	Dbptr dbo;  /* base output db pointer */
	Dbptr dbv;  /* set to view formed by join */
	char *pfin=NULL;  /* input parameter file */
	char *sift_exp;  /* sift expression for subset */
	int sift = 0;  /* default is no sift.  */
	Tbl *sortkeys;
	Tbl *joinkey1, *joinkey2;
	/*Pointers to views returned by dbgroup (keyed to origin and event
	respectively */
	Dbptr dborigin_group;
	Tbl *origin_group;  /* relation keys used in grouping*/
	long nevents;
	/* db row variables */
	long evid;
	long nrows, nrows_raw;

	int useold=0;
	Pf *pf;
	Tbl *ta,*tu;
	Tbl *reason_converged, *residual;
	Location_options o;
	Arr *arr_phase;
	int i;
	Tbl *converge_history;

	Hypocenter h0;
	Hypocenter *hypos;
	long niterations;

	char *vmodel;

	int ret_code;  /* ggnloc return code */
	double **C;   /* covariance matrix*/
	float emodel[4];  

	/* entries for S-P feature */
	long nbcs;
	Arr *badclocks;
	/* need global setting of this to handle fixed depth solutions*/
	int global_fix_depth;

	C=dmatrix(0,3,0,3);

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
	cbanner("Version $Revision$ $Date$\n",
			"relocate inputdb outputdb [-pf pf -sift expression -useold]\n",
			"Gary Pavlis",
                        "Indiana University",
                        "pavlis@indiana.edu");

	/* Alway join assoc, arrival, and site.  We join site 
	to make sure station table is properly dynamic to account for
	time changes.  With this setup, the stations can even move
	around and this should still work.*/


	if(dbopen(dbin,"r",&db) == dbINVALID) 
		elog_die(1,"Unable to open input database %s\n",dbin);
	if(dbopen(dbout,"r+",&dbo) == dbINVALID) 
		elog_die(1,"Unable to open output database %s\n",dbout);

	dbv = dbjoin ( dblookup(db,0,"event",0,0),
		dblookup(db,0,"origin",0,0),
		0,0,0,0,0);
	if(dbv.table == dbINVALID)
		elog_die(1,"event->origin join failed\n");
	dbv = dbjoin ( dbv, dblookup(db,0,"assoc",0,0),
			0,0,0,0,0);
	if(dbv.table == dbINVALID)
		elog_die(1,"event->origin->assoc join failed\n");
	dbv = dbjoin ( dbv, dblookup(db,0,"arrival",0,0),
			0,0,0,0,0);
	if(dbv.table == dbINVALID)
		elog_die(1,"event->origin->assoc->arrival join failed\n");
	/* We will explicitly set the keys for this join because it
	was found to fail sometimes */
	joinkey1 = newtbl(0);
	joinkey2 = newtbl(0);
	pushtbl(joinkey1,"arrival.sta");
	pushtbl(joinkey1,"arrival.time");
	pushtbl(joinkey2,"sta");
	pushtbl(joinkey2,"ondate::offdate");
	dbv = dbjoin ( dbv, dblookup(db,0,"site",0,0),
			&joinkey1,&joinkey2,0,0,0);
	if(dbv.table == dbINVALID)
		elog_die(1,"event->origin->assoc->arrival->site join failed\n");

	/* Subset using sift_key if requested */
	if(sift)
	{
		dbv = dbsubset(dbv,sift_exp,0);
		if(dbv.record == dbINVALID)
			elog_die(1,"dbsubset of %s with expression %s failed\n",
				dbin, sift_exp);
	}
	/* This keeps only the prefered origin records intact */
	dbv = dbsubset(dbv,"orid == prefor", 0);
	if(dbv.record == dbINVALID)
			elog_die(1,"Subset to preferred origin records failed\n");

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
		elog_complain(0,"Input database has duplicate picks of one or more phases on multiple channels\n\
Which picks will be used here is unpredictable\n\
%ld total picks, %ld unique\nContinuing\n", nrows_raw, nrows);

	/* This sort is the required one for the grouping that follows*/

	sortkeys = newtbl(3);
	pushtbl(sortkeys,"evid");
	pushtbl(sortkeys,"orid");
	pushtbl(sortkeys,"arrival.time");
	dbv = dbsort(dbv,sortkeys,0,0);
	if(dbv.record == dbINVALID)
		elog_die(1,"dbsort on evid,orid,arrival.time failed\n");

	/* Set up grouping by events */
	origin_group = newtbl(0);
	pushtbl(origin_group, "evid");
	dborigin_group = dbgroup(dbv, origin_group, "origin_group",1);
	if(dborigin_group.record == dbINVALID)
		elog_die(1,"dbgroup by origin failed\n");

	dbquery(dborigin_group,dbRECORD_COUNT,&nevents);
	elog_notify(0,"Attempting to relocate %ld events in subsetted database\n",
		nevents);
	

	/* DB is now set up correctly, now we turn to the parameter files */
	i = pfread(pfin,&pf);
	if(i != 0) elog_die(1,"Pfread error\n");

	o = parse_options_pf (pf);
	global_fix_depth=o.fix[2];
 	arr_phase = parse_phase_parameter_file(pf);
	vmodel = pfget_string(pf,"velocity_model_name");

	/* set up minus phase for bad clock problems */
	badclocks = newarr(0);
	if(db_badclock_definition(db,pf,badclocks))
		elog_complain(0,"Warning:  problems in database definitions of bad clock time periods\n");
	pfget_badclocks(pf,badclocks);
	nbcs = cntarr(badclocks);
	if(nbcs>0) fprintf(stdout,"relocate:  bad clock feature enabled\n\n");
        /* Change by JN to output evid and orid. */
        /* fprintf(stdout,"lat lon depth time rms wrms interquartile ndata ndgf iterations\n"); */
	fprintf(stdout,"evid orid lat lon depth time rms wrms interquartile ndata ndgf iterations\n");

	/* Main loop.  We utilize the group views and loop through by 
	events */
	for(dborigin_group.record=0;
		dborigin_group.record< nevents;++dborigin_group.record)
	{
		Dbptr db_bundle;  /* db pointer returned from bundle field 
				of dborigin_group for current event */
		Arr *station_table;
		Arr *array_table;
		long is, ie; 
		long orid;  /* orid assigned relocated event in output db */

		if(dbgetv(dborigin_group,0,"evid", &evid,
			"bundle", &db_bundle,NULL ) == dbINVALID)
			elog_complain(1,"dbgetv error for row %ld of event group\n",
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
		/* this actually sets up the minus phase feature for bad clocks*/
		if(nbcs)
		{
			if(minus_phases_arrival_edit(ta,arr_phase,badclocks))
				elog_complain(0,"Warning(relocate):  problems in minus_phase_arrival_edit function\n");
		}
		if(useold)
		{
			char dtype[2];
			h0 = db_load_initial(dbv,is);
			/* keep fixed depth if done before.  
			setting dbv.record here is a bit of
			a potential maintenance problem */
			dbv.record=is;
			dbgetv(dbv,0,"dtype",dtype,NULL );
			if( (!strcmp(dtype,"g")) || (!strcmp(dtype,"r")) )
				o.fix[2]=1;
			
		}
		else
			h0 = initial_locate(ta, tu, o, pf);

		ret_code = ggnloc(h0,ta,tu,o,
			&converge_history,&reason_converged,&residual);
			
		if(ret_code < 0)
		{
			elog_complain(1,"ggnloc failed to produce a solution\n");
		}
		else 
		{
			if(ret_code > 0)
			    elog_complain(1,"%d travel time calculator failures in ggnloc\nSolution ok\n",
				ret_code);
			
			niterations = maxtbl(converge_history);
			hypos = (Hypocenter *)gettbl(converge_history,
								niterations-1);
			predicted_errors(*hypos,ta,tu,o,C,emodel);

                        /* Next 3 calls changed by JN to output evid, orid and number_data */
			orid = save_origin(dbv,is,ie,o.fix[3],*hypos,dbo);
			evid = save_event(dbv,is,ie,orid,dbo);

			fprintf(stdout,"%ld %ld %lf %lf %lf %lf %g %g %g %d %d %ld\n",
					evid,
					orid,
					hypos->lat,hypos->lon,hypos->z,hypos->time,
					hypos->rms_raw, hypos->rms_weighted,
					hypos->interquartile,
					hypos->number_data,
					hypos->degrees_of_freedom,
					niterations);
	
			save_origerr(orid,*hypos,C,dbo);
			save_assoc(dbv,is,ie,orid,vmodel,residual,*hypos,dbo);
			/* These save genloc add on tables */
			save_emodel(orid,emodel,dbo);
			save_predarr(dbo,ta,tu,*hypos,orid,vmodel);
		}
		o.fix[2]=global_fix_depth;
		if(maxtbl(converge_history)>0)freetbl(converge_history,free);
		if(maxtbl(reason_converged)>0)freetbl(reason_converged,free);
		if(maxtbl(residual)>0)freetbl(residual,free);
		destroy_data_tables(tu, ta);
		destroy_network_geometry_tables(station_table,array_table);
	}
	return(0);
}

/* $Id$ */
