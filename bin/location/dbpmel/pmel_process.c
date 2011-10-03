#include <math.h>
#include <stdio.h>
#include <unistd.h>
#include "stock.h"
#include "arrays.h"
#include "coords.h"
#include "db.h"
#include "pf.h"
#include "elog.h"
#include "location.h"
#include "pmel.h"
#include "dbpmel.h"
#include "perf.h"


/* This small routine loads the hypocentroid from the view pointer.
It will fail if the hypocentroid extension table has not been joined
to the working view.

Arguments:
	dbv - working view pointer
	rec - record to read hypocentroid coordinates from
	h - Hypocenter structure in which hypocentroid is returned.
		(Only space coordinate components are set)

Returns:
0 - aok
-1 - dbgetv error
*/	
int load_hypocentroid(Dbptr dbv,int rec, Hypocenter *h)
{
	double lat,lon,depth;
	dbv.record = rec;
	if(dbgetv(dbv,0,"hypocentroid.dlat",&lat,
		"hypocentroid.dlon",&lon,
		"hypocentroid.depth",&depth,NULL ) == dbINVALID)
	{
		elog_complain(0,"dbgetv error reading hypocentroid coordinates from row %d of working view\n",
			rec);
		h->lat = 0.0;
		h->lon = 0.0;
		h->z = 0.0;
		return(-1);
	}
	h->lat = lat;
	h->lon = lon;
	h->z = depth;
	return(0);
}
enum FREEZE_METHOD get_freeze_method(Pf *pf)
{
	enum FREEZE_METHOD result;
	char *s;
	s=pfget_string(pf,"freeze_method");
	if(s==NULL)
	{
		elog_notify(0,"freeze_method parameter not defined in parameter file\nDefault to maximum arrival count space coordinates");
		result=ALLSPACE_MAXARRIVALS;
	}
	else if(strcmp(s,"depth_maxarrival")==0)
		result=DEPTH_MAXARRIVALS;
	else if(strcmp(s,"allspace_maxarrivals")==0)
		result=ALLSPACE_MAXARRIVALS;
	else if(strcmp(s,"all_maxarrivals")==0)
		result=ALL_MAXARRIVALS;
	else if(strcmp(s,"depth_minrms")==0)
		result=DEPTH_MINRMS;
	else if(strcmp(s,"allspace_minrms")==0)
		result=ALLSPACE_MINRMS;
	else if(strcmp(s,"all_minrms")==0)
		result=ALL_MINRMS;
	else
	{
		result=ALLSPACE_MAXARRIVALS;
		elog_notify(0,"Invalid keyword %s used for parameter freeze_method\nDefault allspace_maxarrivals",s);
	}
	return(result);
}


#define EVIDGRP "evidgroup"
/* this is the main processing routine for dbpmel.  It takes an input
list of grid point, which are defined by integers gridid that are
passed through gridid_list, and processes data associated with each
of these grid points in sequence.   It uses datascope db manipulations
to form a view of arrivals associated with the current grid point 
and runs pmel against arrivals for that group of events.  Detailed
behaviour comes through a long list of options defined in the 
parameter file.  

Arguments:
	db - input db view of formed by a complex sequence (see below).
	gridlist - input Tbl list of gridids stored as pure
		integers in the Tbl.   i.e. they are extracted
		as gridid = (int)gettbl(...)
	pf - parameter space of options.  It is important that
		this pf object be checked with a routine that 
		verifies if key parameters are set using the function
		check_required_pf.  This is necessary because pf
		is accessed deep in this code to set check some
		of the many complex options.  This stops the program
		from aborting at inconvenient points within a 
		processing loop.

The view defined by db is very complicated.  This comment could go
out of data, but as of the date of initial writing the view was
formed by running dbprocess with this sequence:
        dbopen cluster
        dbjoin event
        dbjoin origin
        dbsubset orid==prefor
        dbjoin assoc
        dbjoin arrival
        dbjoin site
	dbjoin hypocentroid
        dbsort gridid evid

This is followed by a join with site using ondate:offdate keys to 
make the join work properly in varying station geometry.  A subset
by a sift key is also applied, but this should only reduce data not
alter the view.  The view is next sorted by the key
gridid:evid:sta:phase with a dbUNIQUE to prevent redundant picks on
multiple channels. 

Author:  Gary Pavlis
Written:  Fall 2000
*/

int dbpmel_process(Dbptr db, Tbl *gridlist,Pf *pf)
{
	Pf *vpf;
	Arr *arr_phase;
	char *vmodel;
	/* Holds indexed list of stations with bad clocks problems 
	that should be used only with S-P type timing */
	Arr *badclocks;
	Location_options o;
	/* Hold station table.   We make the array table empty always. */
	Arr *stations;
	int nbcs;
	int i,j,k;
	Tbl *grptbl;
	Tbl *grdidtbl;
	Dbptr dbevid_grp;  /* dbgroup pointers */
	/* Used by dbmatches */
	static Hook *hook=NULL;
	Tbl *reclist;
	Dbptr dbgs;
	Dbptr dbbundle;
	Dbptr dbcs;
	Tbl **ta;  /* We use an array of Tbls for arrival times to 
			allow a simple loop through an event group.*/
	Hypocenter *h0;  
	long *evid;  /* parallel array to h0 of event ids for each h0[i]*/
	Hypocenter hypocentroid;
	/* The associative array here is loaded from the pf and contains
	information on which events are to be treated as calibration events.
	The fixlist array of character strings is passed to pmel as a 
	parallel of vectors defining events with fixed coordinates */
	Arr *events_to_fix;
	/* This is the structure that encapsulates all the station
	correction elements.  WARNING: it is built up in pieces below,
	and later modifications most handle this carefully.*/
	SCMatrix *smatrix;
	Tbl *converge=NULL,*pmelhistory=NULL;
	Arr *arr_phase_3D;
	/* needed for output db tables */
	char *runname, *gridname;
	int pmelfail;
	Tbl *phaselist;


	initialize_hypocenter(&hypocentroid);
	runname = pfget_string(pf,"pmel_run_name");
	gridname = pfget_string(pf,"gridname");

	/* This genloc routine defines location parameters.  These
	are set globally here for the entire run.*/
	o = parse_options_pf(pf);
	/* Pmel is know to fail if this option is turned on */
	if(((o.generalized_inverse)==PSEUDO_RECENTERED)
		|| ((o.generalized_inverse)==DAMPED_RECENTERED) )
	{
		elog_notify(0,"parameter file specifies recenter\
option which is know to cause problems\nrecenter set off\n");
		if((o.generalized_inverse)==PSEUDO_RECENTERED)
			o.generalized_inverse = PSEUDOINVERSE;
		if((o.generalized_inverse)==DAMPED_RECENTERED)
			o.generalized_inverse = DAMPED_INVERSE;
	}


	/* This uses the same method of defining phase handles as dbgenloc*/
	vmodel=pfget_string(pf,"travel_time_model");
	if(pfload("GENLOC_MODELS", "tables/genloc", vmodel, &vpf) != 0)
		elog_die(0,"pfload failed reading working velocity model %s\n",
			vmodel);
 	arr_phase = parse_phase_parameter_file(vpf);
	pffree(vpf);

	/* We want to explicitly clear the station corrections in the 
	phase handle list if any were set by the above function.  If not,
	we have a real mess with corrections in corrections.  */
	clear_station_corrections(arr_phase);

	/* Now load the model used to compute bias correction with 
	a (presumed) 3D model.  Not really required 3D, but we call it that
	anyway.*/
	arr_phase_3D = parse_3D_phase(pf);

	/* We have edit the array of phase handles to a list we select. 
	This way the user doesn't have to be concerned that the arr_phase and
	arr_phase_3d lists are one to one. */
	phaselist = pfget_tbl(pf,"phases_to_use");
	edit_phase_handle(arr_phase,phaselist);
	edit_phase_handle(arr_phase_3D,phaselist);

	/* load the station table and use it to create the station
	indexes for the smatrix structure and setup the result
	vectors stored there.*/
	stations = pmel_dbload_stations(db,pf);
	smatrix = create_SCMatrix(stations,arr_phase);

	/* These routines set up definitions of stations to use 
	S-P type phases with due to "bad clocks".   We use this
	here because we want to either throw away data from stations
	with bad clocks or use the S-P feature.  This sets up
	that feature, but lets the decision of how to handle
	this problem be set in pf.*/
	badclocks = newarr(0);
	if(db_badclock_definition(db,pf,badclocks))
		elog_notify(0,"Problems in database definitions of bad clocks time periods\n");
	pfget_badclocks(pf,badclocks);
	/* nbcs is used as a boolean below, but the cntarr sets it 
	to a nonzero value if there are any entries in the badclock 
	array.  This is useful for efficiency to bypass the messy
	S-P code unless it is necessary. */
	nbcs = cntarr(badclocks);


	events_to_fix = load_calibration_events(pf);
	/* New code added Aug 2008 to reduce absolute location skews
	when the reference model is very wrong */
	int freeze=pfget_boolean(pf,"enable_cluster_freeze");
	enum FREEZE_METHOD fm;
	if(freeze) fm=get_freeze_method(pf);
	/* We next create a grouping of the working view by
	the gridid:evid key.  We use dbmatches below to match
	gridids and the record list from dbmatches is the set
	of group pointers for the collection of events matching
	a given gridid.*/
	grptbl = strtbl("gridid","evid",NULL );
	grdidtbl = strtbl("gridid",NULL );  /* used below */
	dbevid_grp = dbgroup(db,grptbl,EVIDGRP,1);
	if(dbevid_grp.record == dbINVALID)
		elog_die(0,"dbgroup failed on gridid:evid bundling\n");
	dbgs = dblookup(db,0,EVIDGRP,0,0);
	dbgs.record = dbSCRATCH;

	dbcs = dblookup(db,0,"gridstat",0,0);
	
	for(i=0;i<maxtbl(gridlist);++i)
	{
		long gridid;
		int nevents;
		long is,ie;
		int ndata;
		int ierr;

		gridid = (long)gettbl(gridlist,i);


		dbputv(dbgs,0,"gridid",gridid,NULL );
		dbevid_grp.record=dbALL;
		dbmatches(dbgs,dbevid_grp,&grdidtbl,&grdidtbl,&hook,
					&reclist);
		nevents = maxtbl(reclist);
		if(nevents<=0)
		{
			fprintf(stdout,"No data for gridid = %ld\n",gridid);
			freetbl(reclist,0);
			continue;
		}
		allot(Tbl **,ta,nevents);
		allot(Hypocenter *,h0,nevents);
		allot(long *,evid,nevents);
		
		/* reclist now contains a collection of record numbers
		for gridid:evid grouped parts of the working view. */
		for(j=0,ndata=0;j<nevents;++j)
		{
			dbevid_grp.record = (long)gettbl(reclist,j);
			dbgetv(dbevid_grp,0,"evid",evid+j,
				"bundle",&dbbundle,NULL );
			dbget_range(dbbundle,&is,&ie);

			ta[j] = dbload_arrival_table(dbbundle,
                                is,ie,stations, arr_phase);
			if(nbcs)
			{
				if(minus_phases_arrival_edit(ta[j],
					arr_phase,badclocks))
				{
					elog_complain(0,"Warning (dbpmel_process):  problems in editing arrival table for minus phases in minus_phases_arrival function\n");
				}
			}
			ndata += maxtbl(ta[j]);
			h0[j] = db_load_initial(dbbundle,is);
			
		}
		/* We assume the hypocentroid has been joined to this
		view and we can just grab the first row of the view pointer
		to get the hypocentroid location for this group. */
		if(load_hypocentroid(dbbundle,is,&hypocentroid))
		{
			elog_complain(0,"Error loading hypocentroid from working view for gridid=%ld;  Skipping to next gridid in processing list\n",
				gridid);
			for(k=0;k<nevents;++k) freetbl(ta[i],free);
			continue;
		}

		/* This function alters the phase handles by setting the
		station corrections to those computed from the difference
		in travel time between the model defined in the arr_phase_3D
		definition and that in arr_phase.  */
		ierr = initialize_station_corrections(arr_phase,arr_phase_3D,
			stations,&hypocentroid);
		if(ierr>0)
		{
			elog_complain(0,"%d problems setting path anomaly corrections for gridid=%ld\n",
				ierr,gridid);
		}
		else if(ierr<0)
		{
			elog_complain(0,"Cannot compute any path anomaly corrections for gridid=%ld\nSkipping to next grid point\n",
				gridid);
			for(k=0;k<nevents;++k) freetbl(ta[k],free);
			free(evid);
			free(ta);
			continue;
		}
		/* We make the S work space of size ndata times smatrix->ncol
		to avoid having to handle fixed coordinates.  We could work in
		a space as small as ndata-4*nevents if all events had free 
		coordinates,but this would be too small if any coordinates are
		fixed.  Hence, we may waste some memory, but this should not
		be a large factor.  Note the use of reallot which causes this
		workspace to be dynamic from group to group */
		reallot(double *,smatrix->S,ndata*(smatrix->ncol));
		smatrix->nrow = ndata;
		for(k=0;k<(ndata*(smatrix->ncol));++k) smatrix->S[k]=0.0;

		/* This computes the set of reference station corrections
		for this group of events */
		ierr = compute_scref(smatrix, &hypocentroid,stations,
			arr_phase,arr_phase_3D);
		if(ierr)
		{
			elog_complain(0,"%d errors in compute_scref\n",ierr);
		}
		/* It is necessary to initialize the sc vector in smatrix
		to the contents of the reference station corrections to make
		the results internally consistent.  pmel internally uses the
		station corrections in the phase handles in computing travel 
		times and sc is used to as the work vector to store the
		current estimates.  The reference corrections are always the
		starting solution and this copy is required to keep the
		solution consistent with this fact */
		dcopy(smatrix->ncol,smatrix->scref,1,smatrix->sc,1);
		Arr *fixarrtmp;
		if(freeze)
		{
			if(in_fixdepthlist(events_to_fix,evid,nevents))
				fixarrtmp=duparr(events_to_fix,(void *)strdup);
			else
				fixarrtmp=get_freezearr(fm,h0,evid,ta,nevents);
		}
		else
			fixarrtmp=duparr(events_to_fix,(void *)strdup);
		

		/* This is the main processing routine.  It was 
		intentionally built without any db hooks to make it
		more portable */
		if(pmel(nevents,evid,ta,h0,
			fixarrtmp,&hypocentroid,smatrix,
			arr_phase,&o,pf,&converge,&pmelhistory))
		{
			elog_notify(0,
			  "No solution from pmel for cluster id = %ld\n",
				gridid);
			freearr(fixarrtmp,free);
			continue;
		}
		freearr(fixarrtmp,free);
		fprintf(stdout,"Cluster id=%ld pmel convergence reason\n",
			gridid);
		for(k=0,pmelfail=0;k<maxtbl(converge);++k)
		{
			char *swork;
			swork = (char *)gettbl(converge,k);
			fprintf(stdout,"%s\n",swork);

			/* The string ABORT in the convergence list
			is used to flag a failure. */
			if(strstr(swork,"ABORT")!=NULL) pmelfail=1;
		}
		if(!pmelfail)
		{
			dbpmel_save_sc(gridid,db,smatrix,pf);


			if(dbpmel_save_results(db,nevents,evid,h0,
				ta,o,pf))

			{
				elog_complain(0,"Problems saving results\
for cluster id %ld\n",
					gridid);
			}


			/* Missing function here should update
			hypocentroid row */

			if(dbaddv(dbcs,0,"gridid",gridid,
				"gridname", gridname,
				"pmelrun",runname,
				"sswrodgf",smatrix->sswrodgf,
				"ndgf",smatrix->ndgf,
				"sdobs",smatrix->rmsraw,NULL ) == dbINVALID)
			{
				elog_complain(0,"dbaddv error for gridid %ld adding to gridstat table\n",
				gridid);
			}
		}
		for(k=0;k<nevents;++k) freetbl(ta[k],free);
		free(ta);
		free(evid);
		free(h0);
		freetbl(converge,0);
		freetbl(pmelhistory,free);

		converge=NULL;
		pmelhistory=NULL;
		freetbl(reclist,0);

	}
	freearr(events_to_fix,0);
	destroy_SCMatrix(smatrix);
	return(0);
}
