/* This is the main processing routine for mwap.  There
are a few internal functions defined first.  The main routine
here is mwap_process */

#include "multiwavelet.h"
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
			if(t==NULL)die(0,"get_arrivals malloc failure for simple double\n");
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
			die(0,"Mismatch in station count for wavelets in a single frequency band.  Wavelet %d gather has %d stations while wavelet %d has %d\nCannot continue -- exiting\n",
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
				elog_complain(0,"copy_arrival_array:  cannot find entry for station %s in output array\nLikely loss of data in higher frequency bands\n",
					key);
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
/* this short routine computes the L infinity norm (max absolute value)
of the vector formed by differencing the contents of two associative
arrays a1 and a0 passed as input.  This is used as a convergence
test and making it a function is useful as this is not the only
test that is sensible for terminating the loop it is used in below
It returns the entry in a1 and a2 that differ by the largest 
absolute value. */
double Linf_norm_arrival_diff(Arr *a1,Arr *a0)
{
	Tbl *t;
	double *t1,*t0;
	char *key;
	int i;
	double linf;

	t = keysarr(a1);
	for(i=0,linf=0.0;i<maxtbl(t);++i)
	{
		key = gettbl(t,i);
		t1 = (double *)getarr(a1,key);
		t0 = (double *)getarr(a0,key);
		if(t0 == NULL)
			elog_notify(0,"Linf_norm_arrival_diff:  station %s not found in reference time array\nProbably harmless, but check results\n",
				key);
		else
			linf = MAX(linf,fabs((*t1)-(*t0)));
	}
	return(linf);
}
/* small companion function to avoid repetitious error messages
from saving various special tables at the end of processing */
void dbsave_error(char *table,int evid, int band)
{
	elog_complain(0,"Error saving %s table for evid %d and band %d\n",
			table, evid, band);
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
#define MAX_MAIN_LOOP 10  /* maximum number of interations on statics 
				polarization, etc in each band */
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
	char *refsta;  /* Name of reference station */
	double refelev;  /* reference elevation from parameter file */
	int nsta;  /* number of stations */
	int ntest;
	Dbptr db;  /* generic db lookup parameter */
	Dbptr dbgrp;  /* evid group db pointer */
	Dbptr tr;  /* trace database */

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
	double rctm[9];  /*ray coordinate transformation matrix*/
	double timeref;  /* time reference at reference station */
	double time;
	double t0;
	double si;
	double fc;
	int evid;
	int lag;  /* optimal lab computed by coherence measure */
	double dtmax;
	/* For a given gather we set moveout computed moveout time in
	seconds relative to the reference station.  This time includes
	the combined current static estimates.  This is a vector workspace
	that is recycled for every gather.  It is alloced soon as we
	know the number of stations in the site table.  */
	double *moveout;
	MWgather **gathers;
	Particle_Motion_Ellipse avgpm;
	Particle_Motion_Error avgerr;
	Arr *pm_arr,*pmerr_arr;
	/* This vector defines the "up" direction.  For P waves this
	initialization is correct.  For S it may not be appropriate, but
	this is something to repair later */
	double up[3]={0.0,0.0,1.0};
	int bankid;  /* mutliwavelet group id */

	/* This is essential or copy_arrival_array can produce garbage */
	arrival0=NULL;
	arrivals = NULL;
	arrival_new=NULL;
	pm_arr = NULL;
	pmerr_arr = NULL;
	si = pfget_double(pf,"sample_interval");
	/* First we need to load the multiwavelet functions and the 
	associated decimators for the transform.  Each of these
	routines will die if serious problems occur and have no
	error returns.  Wavelet functions can be loaded from a parameter
	file or a db.  */
	if(pfget_boolean(pf,"get_wavelets_from_database"))
		mw = load_multiwavelets_db(pf,&nwavelets,&bankid);
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

	if(!strcmp(pfget_string(pf,"coherence_measure"),"coherence"))
	{
		elog_log(0,"Using singular value ratio based coherence measure\n");

		coherence_type = USE_COHERENCE;
	}
	else
	{
		elog_log(0,"Using semblance as coherence measure\n");
		coherence_type = USE_SEMBLANCE;
	}
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

	/* Now we loop through the outer loop event by event.  
	This is structured here by using a dbgroup defined db pointer
	that is passed through the argument list.  The db pointer 
	is incremented and then the bundle is taken apart to crack
	apart each group of traces (the gather).  Note we use
	a defined name to look up the evid grouped table. */
	dbgrp = dblookup(dbv,0,EVIDBDLNAME,0,0);
	if (dbgrp.record == dbINVALID)
		die(0,"Error in dblookup for named evid group table = %s\n",
			EVIDBDLNAME);
        dbquery(dbgrp,dbRECORD_COUNT,&nevents);
        elog_notify(0,"Processing begins for %d events\n",nevents);

	for(dbgrp.record=0;dbgrp.record<nevents;++dbgrp.record)
	{
		Dbptr db_bundle;
		int evid; 
		int is, ie; 
		int ierr;

		if(dbgetv(dbgrp,0,"evid", &evid,
                        "bundle", &db_bundle,0) == dbINVALID)
		{
                        elog_complain(1,"dbgetv error for row %d of event group\nAttempting to continue by skipping to next event\n",
                                dbgrp.record);
			continue;
		}

                dbget_range(db_bundle,&is,&ie);
		elog_notify(0,"Evid %d trace bundle = rows %d to %d\n",
			evid, is, ie);

		/* At least three stations an array make.  This won't
		always work right when there are missing channels.  This
		simple test assumes 3 components per station */
		if((ie-is+1)<9)
		{
			elog_complain(0,"Insufficient data to process for evid %d\nNeed at least three station -- found only %d\n",
				evid,(ie-is+1)/3);
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
			ierr,ie-is+1,evid);

		/* This routine loads an Arr of arrival times from 
		the input db to be used to compute initial slowness
		vector and initial statics.  */
		arrival0 = get_arrivals(db_bundle);

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
		timeref = compute_time_reference(stations,arrivals,refsta,u);
		polarization0=estimate_initial_polarization(u,stations,
			refsta,phase);

		/* copy this to the working value */
		copy_polarization(&polarization,&polarization0);

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
		generated entries -- common with continuous data */
		trsplice(tr,0.1,0,0);

		/* We run trsplit to break up waveform segments at real gaps.
		I'm not sure later code will work correctly if it isn't an 
		all or nothing situations (e.g. gap in Z component, but 
		not in N or E).  In any case, we have to deal with 
		potential multiple segments later.  */
		trsplit(tr,0,0);

		trapply_calib(tr);
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
/*
trdisp(tr,"Input trace data");
*/

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
		for(i=nbands-1;i>=0;--i)
		{
			if(!accumulate)
				copy_arrival_array(arrival0,&arrivals);

			copy_arrival_array(arrivals,&arrival_new);
			fc = (mw[i].f0)/(2.0*si*decfac[i]);

			/* This builds the basic working gathers for
			each wavelet and builds a shortcut of pointers
			to MWtraces that are related */
			for(j=0;j<nwavelets;++j)
			{
				gathers[j] = build_MWgather(i,j,
						mwsig_arr,stations,
						sn_ratios[i],pf);
/*
				MWgather_to_trace(gathers[j],tr,j,i,0);
*/
			}
/*
trplot_by_sta(tr,"sta =~ /BLUE/ || sta =~ /X300[ri]/");
*/
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

			/*Here we compute the time lag at which the main
			analysis will be performed.  Bear and Pavlis 1999
			used a semblance measure, but that choice is not
			necessarily optimal and future evolution could
			occur here easily by changing this function */
			lag = compute_optimal_lag(gathers,nwavelets,timeref,
					moveout,polarization,swin+i,
					coherence_type,i,pf);
			if(lag < 0)
			{
				elog_complain(0,"Data loss computing semblance\nSkipping evid %d\n",
					evid);
				continue;
			}

			/* We repeat the analysis at a fixed lag until
			the smallest time adjustment is less than one
			sample.  Note we simultaneously work on three 
			items:  slowness vector, residual statics, and
			the principal polarization direction.*/
			
			iterations = 0;
			do {
				compute_mw_arrival_times(gathers,
					nwavelets,timeref,moveout,lag,
					polarization,swin+i,
					&arrival_new,&static_result,
					&avgamp, &amperr, &ampndgf);
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
				compute_mw_particle_motion(gathers,
					nwavelets,timeref,moveout,lag,
					swin+i,up,&avgpm,&avgerr,
					&pm_arr,&pmerr_arr);
				dtmax = Linf_norm_arrival_diff(arrival_new,
							arrivals);
				copy_arrival_array(arrival_new,&arrivals);
				++iterations;
			}while ( ( dtmax > ((*gathers)->x1[0]->dt))
				&& (iterations < MAX_MAIN_LOOP) );

			/* This routine computes the covariance of
			the estimated slowness vector */
			if(compute_slowness_covariance(stations,static_result,
				swin[i].si, ucovariance) )
				elog_complain(0,"Problems computing slowness vector covariance estimate for evid %d and band %d\n",
					evid, i);
			/* routines below save a time window.  We compute
			the lag corrected start time at the reference station
			here as t0 to simplify this in functions that
			need this.*/
			t0 = timeref + (swin[i].si)*((double)lag);
			/* This series of functions save results in a set
			of css3.0 extension tables.  */

			/* ampndgf+1 here is a cheap solution to the
			number of stations used in a solution.  This 
			confusion is necessary because autoediting reduces
			the data set.  Poor planning caused me to not
			force this to be saved explicitly, but ampndgf is
			an exact surrogate.  The +1 is needed because the
			calculation uses number_used - 1 since the average
			amplitude is extracted as a free parameter.*/
			if(MWdb_save_slowness_vector(phase,&u,t0,
				swin+i,array_name,evid,bankid,fc,
				ucovariance,ampndgf+1,3,dbv))
					dbsave_error("mwslow",evid,i);
			if(MWdb_save_avgamp(array_name, evid, bankid, phase,
				fc, t0, swin+i, avgamp,amperr,ampndgf,
				dbv) )
					dbsave_error("mwavgamp",evid,i);
			if(MWdb_save_statics(evid, bankid, phase, fc, t0,
				swin+i,refelev,*gathers,moveout,static_result,
				stations,sn_ratios[i],dbv))
					dbsave_error("mwtstatic:mwastatic:mwsnr",evid,i);
			if(MWdb_save_pm(array_name,evid,bankid,phase,fc,t0,
				swin+i,*gathers,moveout,pm_arr,pmerr_arr,
				&avgpm,&avgerr,dbv)  )
					dbsave_error("mwpm",evid,i);
			
		}
		/*release main work spaces with this series of complicated free routines.
		Here is where you really wish C had garbage collection */
		free_sn_ratios_arr(sn_ratios,nwavelets);
		free_MWtransform_arr(mwsig_arr,nbands,nwavelets);
		free_MWtransform_arr(mwnoise_arr,nbands,nwavelets);
		trdestroy(tr);
		freearr(arrival0,free);
		freearr(arrivals,free);
		freearr(arrival_new,free);
		/* This may not be necessary, but better safe than sorry */
		arrivals = NULL;   arrival0 = NULL;   arrival_new = NULL;
	}
}
