/* This is the main processing routine for mwap.  There
are a few internal functions defined first.  The main routine
here is mwap_process */

#include <stdio.h>
#include "tr.h"
#include "location.h"
#include "multiwavelet.h"
#include "mwap.h"
/* This small function returns an associative array of pointers to 
doubles keyed by station names that are the arrival times read from
the database.  Because we are dealing with this db bundle pointer
this is simpler than manipulating the db directly 

Author:  G Pavlis
*/
Arr *get_arrivals(Dbptr dbbundle)
{
	int is, ie;
	Arr *a;
	double time,*t;
	char sta[20];

	dbget_range(dbbundle,&is,&ie);
	a = newarr(0);

	for(dbbundle.record=is;dbbundle.record<ie;++dbbundle.record)
	{
		if(dbgetv(dbbundle,0,
			"sta",sta,
			"arrival.time",	&time,0) == dbINVALID)
		{
			elog_complain(0,"dbgetv error reading arrival information from row %d\n",
				dbbundle.record);
			continue;
		}
		/* the idea here is to skip this step if an arrival is
		already set.  This often because only every third trace
		or a three-component set yields a unique station name */
		if(getarr(a,sta) == NULL)
		{
			t=malloc(sizeof(double));
			if(t==NULL)elog_die(0,"get_arrivals malloc failure for simple double\n");
			*t = time;
			setarr(a,sta,(void *)t);
		}
	}
	return(a);
}
/* Does what is says and performs a consistency check on the 
gathers passed via the gathers vector (length nwavelets).
Causes the program to die if the number of station in a gather
is not the same for all wavelets.  This should never really
happen and chaos will result if it does so we kill the program
under this condition.  This may be only needed for debugging, but
the effort in the check is so small it can probably be left
in forever.  
*/
void check_gather_consistency(MWgather **gathers,int nwavelets)
{
	int i;
	for(i=1;i<nwavelets;++i)
	{
		if(gathers[i]->nsta != gathers[i-1]->nsta)
			elog_die(0,"Mismatch in station count for wavelets in a single frequency band.  Wavelet %d gather has %d stations while wavelet %d has %d\nCannot continue -- exiting\n",
				i,gathers[i]->nsta,i-1,gathers[i-1]->nsta);
	}
}	
/* This function is used to exactly duplicate the arrival array of doubles from
i to o.  If *o is not a null pointer it is assumed the arr already
exists and freearr is called to clear it's contents before 
calling setarr to copy contents.*/
void dup_arrival_array(Arr *in,Arr **o)
{
	Tbl *t;
	double *value,*copy;
	char *key;
	int i;

	if((*o) == NULL)
		*o = newarr(0);
	else
	{
		freearr(*o,free);
		*o=newarr(0);
	}

	t = keysarr(in);
	for(i=0;i<maxtbl(t);++i)
	{
		key = gettbl(t,i);
		value = (double *)getarr(in,key);
		allot(double *,copy,1);
		*copy = *value;
		setarr(*o,key,copy);
	}
	freetbl(t,0);
}
/* companion to the above, but this version will copy entries in
*in to *o.  Errors are issued only if *o does not contain an entry
for the same key as *in.  If *o is null the dup function above
is called and the function returns immediately after returning
from it.  
*/
void copy_arrival_array(Arr *in,Arr **o)
{
	Tbl *t;
	double *value,*copy;
	char *key;
	int i;

	if((*o) == NULL)
	{
		*o = newarr(0);
		dup_arrival_array(in,o);
	}
	else
	{
		t = keysarr(in);
		for(i=0;i<maxtbl(t);++i)
		{
			key = gettbl(t,i);
			value = (double *)getarr(in,key);
			copy = (double *)getarr(*o,key);
			if(copy == NULL)
			{
				setarr(*o,key,value);
			}
			else
			{
				*copy = *value;
			}
		}
		freetbl(t,0);
	}
}

void copy_MWslowness_vector(MWSlowness_vector *u0,MWSlowness_vector *u)
{
	u->ux = u0->ux;
	u->uy = u0->uy;
	u->refsta = u0->refsta;
}
/* This short routine parses a parameter space for the alignment option 
to use for the stack.  That is, how a 3c array should be rotated.
Function returns an int that relates to the group of defines immediately
below */
#define PMTHEORY 1 /* Use theoretical value from a model */
#define PMESTIMATE 2  /* Use the particle motion major axis computed here */
#define PMZ 3   /* Use the veritical component and do not rotate.*/

int get_stack_align_mode(Pf *pf)
{
	char *s;
	int align_opt;

	s=pfget_string(pf,"stack_alignment");
	if(s==NULL)
	{
		elog_notify(0,"stack_alignment parameter not defined\nDefaulting to model based estimate\n");
		align_opt=PMTHEORY;
	}
	else if(!strcmp(s,"theoretical"))
		align_opt=PMTHEORY;
	else if(!strcmp(s,"pmestimate"))
		align_opt=PMESTIMATE;
	else if(!strcmp(s,"vertical"))
		align_opt=PMZ;
	else
	{
		elog_notify(0,"Illegal stack_alignment parameter = %s\nUsing default=model-based estimate\n",
			s);
		align_opt=PMTHEORY;
	} 

	return(align_opt);
}
/* small companion function to avoid repetitious error messages
from saving various special tables at the end of processing */
void dbsave_error(char *table,int evid, int band)
{
	elog_complain(0,"Error saving %s table for evid %d and band %d\n",
			table, evid, band);
}
/* This function implements a scan for timing problems using 
procedures borrowed from libgenloc.  The algorith basically utilizes
an array of "Bad_Clock" objects that define time intervals marked
as bad for a given station.  When timing is always good for a station
it is simply marked ok.  If a station has any times in it's history 
flagged with time problems the algorithm search and tests the arrival
time it is given against that list.  If time is bad during that period
the MWstation object flag for timing problems is raised. 

Arguments:

a - Associative array of arrival times keyed by station names.
	This list drives the algorithm as each station in this
	list and only stations in this list are scanned.  
arrsta - Associative array of MWstation objects keyed by station
	name.  The clock_is_bad field of this object for any 
	station with a timing problem is set nonzero.
bcarr - associative array of Bad_Clock structures (objects) that
	are passed to method functions that check for timing
	problems.  

Author:  GAry L. Pavlis
Written:  April 2002
*/

void MWcheck_timing(Arr *a,Arr *arrsta,Arr *bcarr)
{
	Tbl *stakeys;
	char *name;
	int i;

	stakeys = keysarr(a);

	for(i=0;i<maxtbl(stakeys);++i)
	{
		Bad_Clock *bc;
		MWstation *sta;
		double *atime;

		name=gettbl(stakeys,i);
		bc=(Bad_Clock *)getarr(bcarr,name);
		sta=(MWstation *)getarr(arrsta,name);
		if(sta==NULL)
			elog_complain(0,"MWcheck_timing: no match in station table for arrival at station %s\n",name);
		else
		{
			if(bc==NULL)
				sta->clock_is_bad=0;
			else
			{
				atime = (double *)getarr(a,name);
				if(clock_is_bad(bc->badtimes,*atime))
					sta->clock_is_bad=1;
				else
					sta->clock_is_bad=0;
			}
		}
	}
}
				
/* This is the main processing function for this program.  

Arguments:
	dbv - db pointer to a complex view of the database to be 
		processed.  That is, it has these properties:
		1.  It is a join of:
			event->origin->assoc->arrival
		2.  subset to single arrival name AND orid==prefor
		3.  sorted by evid/sta

	pf - input parameter space

The main processing loop here keys on the grouping defined in the view
passed as dbgrp.  That is, seismograms for each event group are processed
as a complete gather.  After that, are nested loops to do the multiwavelet
processing as described in Bear and Pavlis (1999a,b).  

Author:  Gary Pavlis
Date:  March 1999+
*/
#define LAG_ERROR -100000 /* Computed lags smaller than this returned
			by compute_optimal_lag are treated as an error
			condition.  Should probably be in an include file*/
void mwap_process(Dbptr dbv,char *phase,  Pf *pf) 
{
	int nevents;  /* number of events=number of groups in dbgrp */
	MWbasis *mw;  /* Multiwavelet basis functions */
	Tbl **decimators;  /* List of loaded decimators used to construct
				multiwavelet transforms in lower bands */
        Tbl **dec_objects;  /*Actual decimation filter objects */
	/* Note:  mw and dec_objects define the multiwavelet transform */
	int nwavelets,nbands;

	/* sets coherence mode used to determine optimal lag */
	int coherence_type;
	
	Arr *stations;  /* This associative array holds MWstation objects
			that contain header like data that is station 
			dependent */
	Arr *badclocks;  /* associative array keyed by sta name holding
			list of time intervals with bad timing */
	char *refsta;  /* Name of reference station */
	double refelev;  /* reference elevation from parameter file */
	int nsta;  /* number of stations */
	int ntest;
	Dbptr db;  /* generic db lookup parameter */
	Dbptr dbgrp;  /* evid group db pointer */
	Dbptr tr;  /* trace database */
	Dbptr dbmps;  /* mwpredslow table */
	Tbl *sortkeys,*sortkeys2;  /* used because different tr routines require
				different sort orders */

	int *pad;  /* vector of length nbands holding computed time padding
			lengths for each band in samples */
	int tpad;  /*time pad actually used  (max of *pad) */
	Time_Window *swin, *nwin;  /* arrays defining time windows
		for signal and noise respectively (relative to arrival)*/
	Time_Window swinall, nwinall;  /*define read time intervals (extracted
			from swin and nwin arrays */
	int *decfac;  /* array of decimation factors needed at times */
	Arr *mwarr;  /* Holds indexed multiwavelet transformed trace objects*/
	/* We keep three copies of arrival time information.  
		arrival0 = original times read from db (never altered)
		arrivals = current working copy
		arrival_new = new estimate obtained from "arrivals" 
	*/
	Arr *arrival0,*arrivals,*arrival_new;
	Arr *static_result;  /* Holds error statistics for static estimates */
	MWSlowness_vector u0,u;
	int i,j;
	double avgamp, amperr;
	int ampndgf;
	int iterations;
	double ucovariance[9];
	char *array_name;
	int accumulate;

	/* These are channel code names used in trace library rotation
	functions rotate_to_standard and trrotate.  */
	char *stdchans[3]={ EW, NS , VERTICAL };
	char *pcchans[3]={"R","T","ZL"};

	Arr *mwsig_arr,*mwnoise_arr;  /* these index pointers to mw transformed
			signal and noise series */
	Arr **sn_ratios;  /* vector of Arr pointers of length nbands indexing
		signal to noise ratio estimates (stored in a structure) for
		every station */
	Spherical_Coordinate polarization0,polarization;
	Spherical_Coordinate polarz={1.0,0.0,0.0};
	Arr *model_times=NULL;
	MWSlowness_vector model_slow;
	double rctm[9];  /*ray coordinate transformation matrix*/
	double timeref;  /* time reference at reference station */
	double time;
	double t0,twin;
	double si;
	double fc,fwin;
	int evid;
	int lag;  /* optimal lab computed by coherence measure */
	double peakcm;  /*Peak value of coherence measure */
	/* For a given gather we set moveout computed moveout time in
	seconds relative to the reference station.  This time includes
	the combined current static estimates.  This is a vector workspace
	that is recycled for every gather.  It is alloced soon as we
	know the number of stations in the site table.  */
	double *moveout;
	MWgather **gathers;
	Particle_Motion_Ellipse *avgpm;
	Particle_Motion_Error *avgerr;
	char *pmtype_to_use;  /* type of particle motion estimate to use
				for polarization */
	Arr *pm_arr,*pmerr_arr;
	Arr *pmarray,*errarray;
	/* This vector defines the "up" direction.  For P waves this
	initialization is correct.  For S it may not be appropriate, but
	this is something to repair later */
	double up[3]={0.0,0.0,1.0};
	int bankid;  /* mutliwavelet group id */
	int band_exit = 0;
	/* name of parameter file produced by GUI to control this program */
	char *guipf;
	int stack_alignment;
	Pf *pfcontrol;
	int loopback;
	int numberpasses=0;
	/* These define the relative time window used for stack and
	particle motion.  s denotes stack, ts0 etc are pm */
	double sts0,ste0;  /* we don't need the equivalent of ts1 and te1 */
	double ts0,ts1,te1,te0;

	/* This is essential or copy_arrival_array can produce garbage */
	arrival0=NULL;
	arrivals = NULL;
	arrival_new=NULL;
	pm_arr = NULL;
	pmerr_arr = NULL;
	pmarray = NULL;
	errarray = NULL;
	si = pfget_double(pf,"sample_interval");
	/* First we need to load the multiwavelet functions and the 
	associated decimators for the transform.  Each of these
	routines will die if serious problems occur and have no
	error returns.  Wavelet functions can be loaded from a parameter
	file or a db.  */
	if(pfget_boolean(pf,"get_wavelets_from_database"))
	{	
		mw = load_multiwavelets_db(dbv,pf,&nwavelets,&bankid);
	}
	else
	{
        	mw = load_multiwavelets_pf(pf,&nwavelets);
		bankid = pfget_int(pf,"bankid");
	}
        decimators = define_decimation(pf,&nbands);
	allot(int *,decfac,nbands);
        dec_objects = build_decimation_objects(decimators,nbands,decfac);

	print_band_info(mw,decfac,pf);

	/* This creates the station objects.  The time extracted here
	is needed to sort out the ontime:endtime key in the site table.
	This is done is a less than bombproof fashion by randomly 
	grabbing the time in the first record view.
	Because of the way the site table works this will always work
	in some fashion.  It will only matter if a station ever moves
	and then we have a bad problem anyway.  */
	dbv.record = 0;
	dbgetv(dbv,0,"time",&time,0);
	stations = build_station_objects(dbv,pf,time);
	refsta = get_refsta(stations);
	array_name = pfget_string(pf,"array_name");
	if(array_name == NULL)
	{
		elog_complain(0,"WARNING:  array_name not defined in parameter file.  Set to default of ARRAY\n");
		array_name = strdup("ARRAY");

	}
	refelev = pfget_double(pf,"reference_elevation");
	/* This loads a definition of bad clocks from an extension
	table called timing.  This comes from libgenloc where it
	is used to handle automatic switching to S-P times. */
	badclocks=newarr(0);
	if(db_badclock_definition(dbv,pf,badclocks))
	{
		elog_notify(0,"Problems in setting up table of stations with timing problems\n");
	}
	/* This function can define stations as always having bad timing
	based on a parameter Tbl list of station names keyed by bad_clock.*/
	pfget_badclocks(pf,badclocks);

	pmtype_to_use = pfget_string(pf,"array_particle_motion_to_use");
	if(pmtype_to_use==NULL) pmtype_to_use=strdup(PMOTION_BEAM);
	/* this used to be a variable, but we no longer have a choice.*/
	coherence_type=USE_COHERENCE;
	
	/* This variable sets if we should reset the arrival estimates
	to starting values for each band.  When true the results accumulate
	from band to band.  That is we keep adding corrections from previous
	band to progressively higher frequency.*/
	accumulate = pfget_boolean(pf,"accumulate_statics");
	/* compute time pad lengths for each band of the mw transforms */
	pad = compute_tpad(dec_objects, mw, stations,pf);

	/* These routine parses the parameter file for noise and
	analysis time window information respectively returning
	arrays of Time_Window structures of length nbands*/
	decfac = get_decimation_factors(dec_objects, pf);
	swin = get_signal_windows(decfac,pad,pf);
	nwin = get_noise_windows(decfac,pad,pf);
	print_window_data(decfac,nbands,swin,nwin,pf);	

	/* This gets time windows for signal and noise needed for
	reading data (i.e. largest time ranges needed) */
	swinall = compute_time_window(swin,decfac,nbands);
	nwinall = compute_time_window(nwin,decfac,nbands);

	guipf = pfget_string(pf,"mwapcontrol");
	/* better safe than sorry */
	if(guipf==NULL)
	{
		elog_die(0,"Missing required parameter mwapcontrol");
	}

	/* We can create these works spaces now for efficiency so 
	we don't have to constantly recreate them dynamically below */
	allot(double *,moveout,cntarr(stations));
	allot(MWgather **,gathers,nwavelets);

	/* This associative array holds indexed pointers to multiwavelet
	transformed traces.  We create it here, but it is repeatedly
	freed and cleared below */
	mwarr = newarr(0);
	/* This one has to be initialized*/
	static_result=newarr(0);

	/* We need this table repeatedly below so we avoid constant 
	lookups */
	dbmps = dblookup(dbv,0,"mwpredslow",0,0);
	if(dbmps.record == dbINVALID)
		elog_die(0,"db lookup failed for mwpredslow table\nMWavelet schema extensions are required\n");

	/* Now we loop through the outer loop event by event.  
	This is structured here by using a dbgroup defined db pointer
	that is passed through the argument list.  The db pointer 
	is incremented and then the bundle is taken apart to crack
	apart each group of traces (the gather).  Note we use
	a defined name to look up the evid grouped table. */
	dbgrp = dblookup(dbv,0,EVIDBDLNAME,0,0);
	if (dbgrp.record == dbINVALID)
		elog_die(0,"Error in dblookup for named evid group table = %s\n",
			EVIDBDLNAME);
        dbquery(dbgrp,dbRECORD_COUNT,&nevents);
        fprintf(stdout,"Processing begins for %d events\n",nevents);

	sortkeys = newtbl(0);
	pushtbl(sortkeys,"sta");
	pushtbl(sortkeys,"chan");
	pushtbl(sortkeys,"time");
	sortkeys2 = newtbl(0);
	pushtbl(sortkeys2,"time");
	pushtbl(sortkeys2,"sta");
	pushtbl(sortkeys2,"chan");

	for(dbgrp.record=0;dbgrp.record<nevents;++dbgrp.record)
	{
		Dbptr db_bundle;
		int evid; 
		int is, ie; 
		int ierr;
		double modaz;

		if(dbgetv(dbgrp,0,"evid", &evid,
                        "bundle", &db_bundle,0) == dbINVALID)
		{
                        elog_complain(1,"dbgetv error for row %d of event group\nAttempting to continue by skipping to next event\n",
                                dbgrp.record);
			continue;
		}

                dbget_range(db_bundle,&is,&ie);

		if(ie-is<3)
		{
			elog_complain(0,"Insufficient data to process for evid %d\nNeed at least three station -- found only %d\n",
				evid,ie-is);
			continue;
		}
		/* We utilize what we call plane wave statics here
		to approximately correct for wavefront curvature.
		We set the record number to is so we can access 
		the correct origin information from the db.  Because
		we used a join allrows of this group should have the
		same origin data.  */
		ierr = set_pwstatics(stations,refsta,phase,db_bundle,pf);
		if(ierr)elog_complain(0,"%d errors computing %d plane wave statics for evid %d\n",
			ierr,ie-is,evid);

		/* This routine loads an Arr of arrival times from 
		the input db to be used to compute initial slowness
		vector and initial statics.  */
		arrival0 = get_arrivals(db_bundle);

		/* We edit the MWstation array to flag stations
		with bad timing in this function */
		MWcheck_timing(arrival0,stations,badclocks);

		/* Save these times */
		copy_arrival_array(arrival0,&arrivals);


		/* Initialize slowness vector to 0 and then estimate
		it from data using current arrival times */
		u0.ux = 0.0;  u0.uy = 0.0;  u0.refsta = refsta;
		timeref = compute_time_reference(stations,arrivals,refsta,u0);
		/* for the first pass we use weights defined for the 
		lowest frequency band.  This is done because it asssumed
		that if frequency dependent weighting is actually used
		the lowest band would have the widest effective aperture. */
		ierr = estimate_slowness_vector(u0,arrivals,stations,
			refsta, refelev, timeref, phase, nbands-1,&u);
		/* It is necessary to reset the time reference to handle 
		the case correctly when the reference station does not
		actually record this event.  This function uses a moveout
		correction that depends upon the slowness vector, so it can
		float about a bit in that situation */
		if(ierr>0)
			elog_notify(0,"%d nonfatal errors in estimate_slowness_vetor for evid %d\n",ierr,evid);
		else if(ierr < 0)
		{
			elog_complain(0,"estimate_slowness_vector failed for initial slowness estimate for evid %d\nData for this event will be skipped\n",
				evid);
			continue;
		}
		/* This routine returns the slowness vector and an arr of 
		estimated arrival times.  The slowness vector is saved
		in the mwpredslow table immediately below.  Arrival times
		are used to compute residuals later. */
		ierr = MWget_model_tt_slow(stations, refsta, phase,
			db_bundle, pf, &model_times, &model_slow);

		timeref = compute_time_reference(stations,arrivals,refsta,u);
		polarization0=estimate_initial_polarization(model_slow,stations,
			refsta,phase);

		modaz = atan2(model_slow.ux,model_slow.uy);

		if(dbaddv(dbmps,0,"sta",array_name,
			"evid",evid,
			"phase",phase,
			"time",timeref,
			"slo",hypot(model_slow.ux,model_slow.uy),
			"azimuth",deg(modaz),
			"majoraz",deg(polarization0.phi),
			"majorema",deg(polarization0.theta),
			"vmodel",pfget_string(pf,"TTmodel"),0) == dbINVALID)
		{
			elog_complain(0,"dbaddv error for evid %d on mwpredslow table\n",
				evid);
		}

		/* This function reads in the trace data for this event
		using time windows defined above */
		tr = mwap_readdata(dbgrp,arrivals,swinall, nwinall);
		if(tr.record == dbINVALID)
		{
			elog_complain(0,"Serious problems reading data for evid %d -- no data processed for this event\n",evid);
			continue;
		}
		tr = dblookup(tr,0,"trace",0,0);
		/* We first glue together any possible recording break
		generated entries -- common with continuous data.
		This also seems to require a resort because of the
		way data was read in.   */
/*
		tr = dbsort(tr,sortkeys,0,0);
*/
		trsplice(tr,0.1,0,0);

		/* We run trsplit to break up waveform segments at real gaps.
		I'm not sure later code will work correctly if it isn't an 
		all or nothing situations (e.g. gap in Z component, but 
		not in N or E).  In any case, we have to deal with 
		potential multiple segments later.  */
		trsplit(tr,0,0);

		trapply_calib(tr);
		trdemean_seg(tr);
		/* Now we have reorder the traces or this will not work
		correctly*/
		tr = dbsort(tr,sortkeys2,0,0);
		ierr = rotate_to_standard(tr,stdchans);
		if(ierr<0)
		{
			elog_complain(0,"rotate_to_standard failed processing evid %d -- no data processed for this event\n",
				evid);
			continue;
		}
		if(ierr>0)elog_complain(0,"rotate_to_standard failed for %d stations\n",
				ierr);

		/* This releases the space held by the raw data traces
		keeping only the rotate_to_standard outputs */
		free_noncardinal_traces(tr);

		elog_log(0,"Computing multiwavelet transform:  be\
 patient as this can take a while with many channels\n");
		/* This function computes the multiwavelet transform
		of all traces currently in tr for signals around arrival*/
		mwsig_arr = tr_mwtransform(tr,arrivals,swin,decfac,dec_objects,
				nbands,mw,nwavelets);

		/* We repeat the same thing for noise windows */
		mwnoise_arr = tr_mwtransform(tr,arrivals,nwin,decfac,
				dec_objects,nbands,mw,nwavelets);
		/* Now compute signal to noise ratio figures for all
		nbands storing the structures that define the results
		in an Arr keyed by station. Note this is actually 
		a vector of Arr pointers of length nbands.  Further
		note the following function creates this complicated
		object, and it must be freed after each event is 
		processed. */
		sn_ratios=compute_signal_to_noise(mwsig_arr,mwnoise_arr,
					stations,arrivals,swin,nwin,
					nbands,nwavelets);

		/* Now we get to the heart of this program.  This is
		the outer loop over frequency.  Note the loop goes
		backward because the lowest frequencies are the final
		row of the mw transform matrices of pointers */

		copy_MWslowness_vector(&u,&u0);
		if(numberpasses>0)
		{
			fprintf(MWpout,"NEWEVENT %d\n",evid);
		}
		for(i=nbands-1;i>=0;--i)
		{
			if(!accumulate)
				copy_arrival_array(arrival0,&arrivals);

			copy_arrival_array(arrivals,&arrival_new);
			fc = (mw[i].f0)/(2.0*si*decfac[i]);
			fwin = (mw[i].fw)/(2.0*si*decfac[i]);

			fprintf(stdout,"Processing begins on band %d with center frequency %lf\nWait for -Hit Accept button when ready- prompt\n",
				i,fc);

			/* This builds the basic working gathers for
			each wavelet and builds a shortcut of pointers
			to MWtraces that are related */
			for(j=0;j<nwavelets;++j)
			{
				gathers[j] = build_MWgather(i,j,
						mwsig_arr,stations,
						sn_ratios[i],pf);
			}
			fprintf(stdout,"Working gather for this band has %d stations\n",
				gathers[0]->nsta);
			/* Testing band 0 should be sufficient.  The
			signal-to-noise is averaged overall wavelets so
			the same stations should be deleted in all
			wavelet of the group */
			if(gathers[0]->nsta < 3)
			{
				elog_notify(0,"Insufficient data in band %d to passed signal-to-noise cutoff defined for this band for evid %d\nSkipping to next frequency band\n",
					i,evid);
				continue;
			}
			/* This may not be necessary, but it is certainly 
			important for debugging.  We check that all
			the gathers in the group have the same length.  
			If they aren't, we are in trouble because we use
			a single vector to hold moveout information */
			check_gather_consistency(gathers,nwavelets);

			/* Now we compute the moveout information assuming
			stations are in the same order in the gather for
			each wavelet */
			if(compute_total_moveout(*gathers,stations,refsta,
				u,refelev,phase,moveout))
			{
				elog_die(0,"Cannot find reference station to compute moveout:  Should not happen unless program overwrites itself\n");
			}

			if(numberpasses>0)
			{
				fprintf(MWpout,"NEWBAND %d\n",i);
				fflush(MWpout);
			}
			else
			{
				char ctmp[40];
				fprintf(stdout,"Starting processing of first event\nSelect and options and press the Start button when ready\n");
				fprintf(MWpout,"STARTUP %d %d\n", 
					evid,i);
				fflush(MWpout);
				fgets(ctmp,40,MWpin);
			}
			++numberpasses;

			/* This is placed here to allow changing the
			alignment options on the fly.  Choice may
			depend on data. */
			pfread(guipf,&pfcontrol);
			stack_alignment=get_stack_align_mode(pfcontrol);
			pffree(pfcontrol);
			
			/* kind of a odd loop construct here made 
			necessary by differences in stackalignment
			options.  If we align with theoretical value
			or use the vertical we do not need to repeat
			this loop and we fall out the bottom.  If we
			use the pm estimate, however, we have to 
			realign the stack rotated to the new major
			ellipse estimate.  In that case we have to
			repeat the whole procedure.*/
			loopback=2;
			do {
				MWstack *stack;
				switch(stack_alignment)
				{
				case PMTHEORY:
					copy_polarization(&polarization0,&polarization);
					loopback=0;
					break;
				case PMZ:
					copy_polarization(&polarz,&polarization);
					loopback=0;
					break;
				case PMESTIMATE:
				default:
				/* This uses theoretical version for the
				first pass then the estimate on the 
				second */
					if(loopback==2)
					  copy_polarization(&polarization0,
						&polarization);
				}
				stack=MWcompute_arrival_times(gathers,
    					   nwavelets,timeref,moveout,
    					   polarization,swin[i],
					   sn_ratios[i],guipf,
    					   &arrival_new,&static_result,
                                            &avgamp, &amperr, &ampndgf);
				if(stack==NULL)
				{
					/* I use a flag to avoid an
					evil goto here */
					band_exit = 1;
					/* This is strange but necessary
					to stop string of bogus errors from
					copy_arrival_array function when
					this loops back */
					if(arrival_new!=NULL)
						freearr(arrival_new,free);
					arrival_new = NULL;

					break;
				}
					
					
				/* Note this routine updates residual
				static values to new values relative to
				the new slowness vector estimate */
	                	ierr = estimate_slowness_vector(u0,
					arrival_new,stations,
                        		refsta, refelev, timeref, 
					phase, i, &u);
				/* We need to recompute the moveout to 
				now be relative to the new slowness vector
				estimate.  We then use this for particle 
				motion analysis which can change the 
				polarization vector */
				compute_total_moveout(*gathers,stations,refsta,
				u,refelev,phase,moveout);
				/* This segment converts particle motions
				for 3-c arrays.  */
				if(gathers[0]->ncomponents==3)
				{
					MWstack *spm; 
					Time_Window pmtwindow;
					double *timeweight;
					
					/* We extract the time window
					from a control parameter file which
					is assumed to be created by a GUI
					with tcl/tk */
					pfread(guipf,&pfcontrol);
					ts0=pfget_double(pfcontrol,"pm_ts0");
					ts1=pfget_double(pfcontrol,"pm_ts1");
					te1=pfget_double(pfcontrol,"pm_te1");
					te0=pfget_double(pfcontrol,"pm_te0");
					/* we need these below, not here */
					sts0=pfget_double(pfcontrol,"stack_ts0");
					ste0=pfget_double(pfcontrol,"stack_te0");
					twin = ste0-sts0;
					pffree(pfcontrol);
					pmtwindow.tstart = nint(ts0/(stack->dt));
					pmtwindow.tend = nint(te0/(stack->dt));

					spm = MWextract_stack_window(stack,
						&pmtwindow);
					if(spm==NULL)
						elog_die(0,
						  "Fatal error in MWextract_stack_window\n");
					/* Sets time weight function for 
					a trapezoidal window */
					timeweight=MWstack_set_trapezoidal_window(spm->tstart,
						spm->dt,spm->nt,
						ts0,ts1,te1,te0);
					dcopy(spm->nt,timeweight,1,spm->timeweight,1);
					free(timeweight);
					MWstack_apply_timeweight(spm);

					if(MWcompute_array_particle_motion(gathers,
					  nwavelets,spm,timeref,moveout,
					  up,&pmarray,&errarray, &pm_arr,&pmerr_arr) )
					{
					  elog_complain(0,"Errors in MWcompute_array_particle_motion\n");
					}
					avgpm = (Particle_Motion_Ellipse *)getarr(pmarray,pmtype_to_use);
					avgerr = (Particle_Motion_Error *)getarr(pmarray,pmtype_to_use);

					polarization
					  =unit_vector_to_spherical(avgpm->major);
					destroy_MWstack(spm);
				}
				peakcm=stack->coherence[idamax(
					stack->nt,
					stack->coherence,1)];
				copy_arrival_array(arrival_new,&arrivals);
				freearr(arrival_new,free);
				arrival_new = NULL;
				destroy_MWstack(stack);
				if(stack_alignment==PMESTIMATE)
						--loopback;
			}while(loopback>0);
			if(band_exit)
			{
				band_exit = 0;
				continue;
			}

			/* This routine computes the covariance of
			the estimated slowness vector */
			if(compute_slowness_covariance(stations,static_result,
				ucovariance) )
				elog_complain(0,"Problems computing slowness vector covariance estimate for evid %d and band %d\n",
					evid, i);
			/* routines below save a time window.  We compute
			the lag corrected start time at the reference station
			here as t0 to simplify this in functions that
			need this.*/
			t0 = timeref + sts0;
			/* This series of functions save results in a set
			of css3.0 extension tables.  */

			/* ampndgf+1 here is a cheap solution to the
			number of stations used in a solution.  This 
			confusion is necessary because autoediting reduces
			the data set.  Poor planning caused me to not
			force this to be saved explicitly, but ampndgf is
			an exact surrogate.  The +1 is needed because the
			calculation uses number_used - 1 since the average
			amplitude is extracted as a free parameter.
			*/
			if(MWdb_save_slowness_vector(phase,&u,t0,twin,
				array_name,evid,bankid,fc,fwin,
				ucovariance,ampndgf+1,3,
				coherence_type,peakcm,dbv))
					dbsave_error("mwslow",evid,i);
			if(MWdb_save_avgamp(array_name, evid, bankid, phase,
				fc, t0, twin, avgamp,amperr,ampndgf,
				dbv) )
					dbsave_error("mwavgamp",evid,i);
			if(MWdb_save_statics(evid, bankid, phase, fc, t0,
				twin,refelev,*gathers,moveout,static_result,
				stations,sn_ratios[i],
				arrivals, model_times,dbv))
					dbsave_error("mwtstatic:mwastatic:mwsnr",evid,i);
			t0=timeref+ts0;
			twin = te0-ts0;
			if(MWdb_save_pm(array_name,evid,bankid,phase,fc,t0,
				twin,*gathers,moveout,pm_arr,pmerr_arr,
				avgpm,avgerr,dbv)  )
					dbsave_error("mwpm",evid,i);
			/* We have to release the memory held in these
			associative arrays.  In the earlier loop the 
			function that creates them always clears them
			before continuing when they are not null.  
			The explicit NULL set after the free is done
			to make sure in looping back the particle
			motion routine clears these correctly.  */
			freearr(pm_arr,free);
			pm_arr = NULL;
			freearr(pmerr_arr,free);
			pmerr_arr = NULL;
			/* same for static arr */
			freearr(static_result,free);
			static_result = NULL;
		}
		/*release main work spaces with this series of complicated free routines.
		Here is where you really wish C had garbage collection */
		free_sn_ratios_arr(sn_ratios,nbands);
		free_MWtransform_arr(mwsig_arr,nbands,nwavelets);
		free_MWtransform_arr(mwnoise_arr,nbands,nwavelets);
		trdestroy(&tr);
		freearr(arrival0,free);
		freearr(arrivals,free);
		/* This may not be necessary, but better safe than sorry */
		arrivals = NULL;   arrival0 = NULL;   arrival_new = NULL;
	}
	free(moveout);
	free(swin);
	free(nwin);
	free(refsta);
}
