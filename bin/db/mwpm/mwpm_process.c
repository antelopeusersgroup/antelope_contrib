#include "multiwavelet.h"
#include "tr.h"
/* This is a debug function used in conjunction with sun's workshop
visualization stuff.  We copy complex trace data into real, fixed 
length buffers to allow them to be graphed with workshop's gizmo.
void debug_mwtrace(Arr *d,char *sta, int nbands, int nwavelets)
{
        char *chans[3]={ EW, NS, VERTICAL };
	int i, j,k,l;
	float t[10000];
	MWtrace ***trace;
	char *key;
	int nz;

	for(j=0;j<nbands;++j)
	for(i=0;i<nwavelets;++i)
	for(k=0;k<3;++k)
	{
		key = make_mw_key(sta,chans[k]);
		trace = (MWtrace ***)getarr(d,key);
		free(key);
		nz = trace[0][j][k].nz;
		for(l=0;l<nz;++l)
		{
			t[l] = trace[0][j][k].z[l].r;
		}
		fprintf(stderr,"%s:%s real\n",sta,chans[k]);
		for(l=0;l<nz;++l)
		{
			t[l] = trace[0][j][k].z[l].i;
		}
		fprintf(stderr,"%s:%s imag\n",sta,chans[k]);
	}
}
*/
		

		

/* This function computes and returns a vector of time pad lengths 
(in samples) for multiwavelet transforms.  The time pad vector is alloced
here and returned as a int vector of length = to the number of frequency
bands in the transform.  It computes the time pad as the combined 
time of edge effects of wavelet and decimator filters.   This function
is closely related to a similar function used in mwap called just
compute_tpad.  That function differs only in adding a factor for 
array aperture.  It's argument list also is different because more
info needs to be passed to compute moveout.

Arguments:  
	d - Tbl array of of decimator objects 
	mw - MWbasis objects
	pf - parameter space 

Author:  G Pavlis
*/
int *ss_compute_tpad(Tbl **d, MWbasis *mw, Pf *pf)
{
	double aperture;
	double max_slow;
	double t_prop;
	double si;  /* base sample interval (before decimation) */
	int nbands;
	int decfac,dec_previous_band=1;
	int padmw, paddec, *pad;
	int i,j;

	si = pfget_double(pf, "sample_interval");

	/* now we compute the filter padding.  Mw is easy, decimators
	are much harder */
	padmw = ((mw[0].n)+1)/2;  /* We assume all basis functions have the 
			same length -- necessary condition for original
			definition in lilly and park */

	nbands = pfget_int(pf,"number_frequency_bands");
	pad = (int *) calloc(nbands,sizeof(int));
	if(pad == NULL) die(0,"Cannot alloc int tpad array of length %d\n",
				nbands);

	
	for(i=0,paddec=0;i<nbands;++i)
	{
		int nstages;
		nstages = maxtbl(d[i]);
		for(j=0,decfac=dec_previous_band;j<nstages;j++)
		{
			FIR_decimation *fir;
			fir = (FIR_decimation *)gettbl(d[i],j);

			decfac *= fir->decfac;
			paddec += decfac *((fir->ncoefs)/2); 
				/* exact is ncoefs-1, but we round up for safety*/
		}
		pad[i] = paddec;
		dec_previous_band = decfac;
		pad[i] += padmw;
	}
	return(pad);
}
/* This function computes and returns a particle motion ellipse estimate
and associated error estimates for a single, 3-component station.  Input
data is multiwavelet transformed data stored in the following complicated
stucture:
1.  The transformed data are stored in an Arr indexed by a key defined with 
a symbol sta_chan (e.g. AAK_BHZ).  The object returned from this arr is
a ***MWtrace object
2.  The ***MWtrace object is a matrix of pointers to MWtrace objects that 
define the multiwavelet transform implemented here (see man mwtransform(3)).

The algorithm used is a principle component method.  The program assembles
the three components of the multiwavelet tranformed, complex valued traces,
and applies a complex svd routine from sunperf.  This reduces to a special
case of the approach used in mwap for a single station.  (mwap is multichannel
and we used the phase of all eigenvector components to define particle 
motion ellipses for each station relative to the array average)
The phase estimates of the left singular vector are converted to a 
particle motion ellipse, and errors are computed using the redundancy of
the multiwavelet transformed wavelets.  

Arguments:
	band - frequency band of transform to compute estimates for
		(an index into ***MMTrace objects)
	nwavelets - number of wavelets in the transform
	sta - name of station being processed.
	mwdata - Arr containing indexed MWtransformed trace objects
	snrarr - Arr containing signal-to-noise objects index by sta
	tstart:tend - define time window to apply for analysis
		(Note the actual coverage has grey fringes of size
		given by half the wavelet length )
	up - 3 vector defining the "up" direction used to resolve
		the sign ambiguity of any particle motion ellipse 
	pf - parameter space object
	pmavg - estimated average particle motion result 
	pmearg - errors estimates for pmavg
	Note: pmavg and pmeavg are NOT alloced here.
Returns:
 negative values indicate an error led to no results (should call elog_complain)
 0 - all ok
 positive number means data discarded due to low signal to noise ratio 

Author:  Gary Pavlis
Written:  April 2000
*/
int ssmwpm(int band, int nwavelets, char *sta,
	Arr *mwdata,  Arr *snrarr, 
	double tstart, double tend,
	double *up, Pf *pf,
	Particle_Motion_Ellipse *pmavg,
	Particle_Motion_Error *pmeavg)
{
	char *key;
	MWtrace ***x1,***x2,***x3;
	Signal_to_Noise *snr;
	int i;
	int nz, nz_used;
	complex *A,*U,*Vt;
	Particle_Motion_Ellipse *pmwork;
	float svalues[3];
	int info;
	double starttime,si;
	int istart;
float tdebug[10000];
int j,jj;

	/* This extracts data from the mwdata arr */
	key = make_mw_key(sta,EW);
	x1 = (MWtrace ***)getarr(mwdata,key);
	free(key);
	key = make_mw_key(sta,NS);
	x2 = (MWtrace ***)getarr(mwdata,key);
	free(key);
	key = make_mw_key(sta,VERTICAL);
	x3 = (MWtrace ***)getarr(mwdata,key);
	free(key);
	if( (x1 == NULL) || (x2 == NULL) || (x3 == NULL) ) 
	{
		elog_notify(0,"Missing data for station %s\n",
			sta);
		return(-1);
	}
	/* mwtransform sets the nz field of any MWtrace to 0 to
	signal a null trace.  When this happens the contents of
	the structure are junk, so we must delete them at this
	stage */
	if( (((*x1)[band][0].nz) == 0)
		|| (((*x2)[band][0].nz) == 0)
		|| (((*x3)[band][0].nz) == 0) ) 
	{
		elog_notify(0,"Null data in band %d for station %s\n",
			band,sta);
		return(-2);
	}
	/* This is a little paranoid.  The current version shouldn't do this, but
	evolution could cause this to happen I suppose.  There should be a elog
	message here, but since I consider this error unlikely I thought it 
	unnecessary to clutter the code.  Normally all the elements of all
	traces in a band ought to have the same length, start time,
	and sample interval. */
	nz = (*x1)[band][0].nz;
	starttime = (*x1)[band][0].starttime;
	si = (*x1)[band][0].dt;
	for(i=0;i<nwavelets;++i)
	{
		if(((*x1)[band][i].nz) != nz) return(-3);
		if(((*x2)[band][i].nz) != nz) return(-3);
		if(((*x3)[band][i].nz) != nz) return(-3);
		if(((*x1)[band][i].starttime) != starttime) return(-4);
		if(((*x2)[band][i].starttime) != starttime) return(-4);
		if(((*x3)[band][i].starttime) != starttime) return(-4);
		if(((*x1)[band][i].dt) != si) return(-5);
		if(((*x2)[band][i].dt) != si) return(-5);
		if(((*x3)[band][i].dt) != si) return(-5);

	}
for(j=0;j<nz;++j) tdebug[j] = (*x3)[band][0].z[j].r;

	
	snr = (Signal_to_Noise *)getarr(snrarr,sta);
	if(snr_is_too_low(snr,pf)) return(1);

	istart = nint( (tstart - starttime)/si );
	if(istart < 0)
	{
		istart = 0;
		nz_used = nint( (tend - starttime)/si ) + 1;
		if(nz_used > 0)
			elog_notify(0,"Window truncation for station %s in band %d to %d samples\n",
				sta,band,nz_used); 
		else
		{
			elog_complain(0,"No data in time window request for station %s in band %d with startime %lf\n",
				sta,band,starttime);
			return(-6);
		}
	}
	/* We assume this will always work once we get here */
	nz_used = nint( (tend - tstart)/si ) + 1;

	allot(complex *,A,3*nz);
	allot(Particle_Motion_Ellipse *,pmwork,nwavelets);

	/* Now we compute particle motion ellipses for each wavelet in
	this band storing the results in vector of pointers pmwork*/
	for(i=0;i<nwavelets;++i)
	{
		ccopy(nz_used,((*x1)[band][i].z)+istart,1,A,3);
		ccopy(nz_used,((*x2)[band][i].z)+istart,1,A+1,3);
		ccopy(nz_used,((*x3)[band][i].z)+istart,1,A+2,3);
for(j=0,jj=0;j<nz_used;++j,jj+=3) tdebug[j] = A[jj].r;
		/* Note in this svd routine U and Vt are pure dummies
		because the o and n arguments say to overwrite A with 
		the left singular vectors and to just not compute the
		right singular vectors */
		cgesvd('o','n',3,nz_used,A,3,svalues,U,3,Vt,3,&info);

		set_pm_null(pmwork+i);
		pmwork[i] = compute_particle_motion (A[0],A[1],A[2],up);
	}

	pmvector_average(pmwork,nwavelets,pmavg,pmeavg);

	free(A);
	free(pmwork);
	return(0);
}

/* This is a hack function added to allow this code to directly utilize
some functions used in mwap that require the Arr of pointers to MWstation
objects keyed by the station name, sta.  

This little function creates blank entries of MWstation objects keyed
by the input station name sta.  It avoids memory leaks by first seeing
if an entry for sta already exists in which case it just returns 
immediately.  Otherwise it mallocs the MWstation structure and calls
the initializer routine. */
void check_sta_arr(Arr *stations,char *sta,int nbands)
{
	MWstation *s;

	s = (MWstation *)getarr(stations,sta);
	if(s == NULL) 
	{
		s = (MWstation *)malloc(sizeof(MWstation));
		if(s==NULL) die(0,"Cannot malloc MWstation object\n");
		initialize_MWstation(s,nbands);
		setarr(stations,sta,s);
	}
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
void mwpm_process(Dbptr dbv,char *phase,  Pf *pf) 
{
	MWbasis *mw;  /* Multiwavelet basis functions */
	Tbl **decimators;  /* List of loaded decimators used to construct
				multiwavelet transforms in lower bands */
        Tbl **dec_objects;  /*Actual decimation filter objects */
	/* Note:  mw and dec_objects define the multiwavelet transform */
	int nwavelets,nbands;

	Arr *stations;  /* This associative array holds MWstation objects
			that contain header like data that is station 
			dependent */
	int ntest;
	Dbptr db;  /* generic db lookup parameter */
	Dbptr dbgrp;  /* group db pointer */
	Dbptr dbpm;   /* pointer to mwpm table -- kept because used repeatedly*/
	Dbptr dbsnr;  /* pointer to mwsnr table */
	Dbptr tr;  /* trace database */

	int *pad;  /* vector of length nbands holding computed time padding
			lengths for each band in samples */
	int tpad;  /*time pad actually used  (max of *pad) */
	Time_Window *swin, *nwin;  /* arrays defining time windows
		for signal and noise respectively (relative to arrival)*/
	Time_Window swinall, nwinall;  /*define read time intervals (extracted
			from swin and nwin arrays */
	int *decfac;  /* array of decimation factors needed at times */
	int i,j;
	Arr *arrivals;  /* This is a hack to recycle older code.  Several
			functions in mwap used an arr of arrival times keyed
			by station.  Here this arr will normally have one
			entry */

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
	double time;
	double t0;
	double si;
	double fc;
	double dtmax;
	Particle_Motion_Ellipse avgpm;
	Particle_Motion_Error avgerr;
	/* This vector defines the "up" direction.  For P waves this
	initialization is correct.  For S it may not be appropriate, but
	this is something to repair later */
	double up[3]={0.0,0.0,1.0};
	int bankid;  /* mutliwavelet group id */
	int narrivals;
	char sta[10];
	double arrival_time,dtstart,dtend,tstart,tend,twin;
        Spherical_Coordinate scoor;
        double majaz, majema, minaz, minema;

	arrivals = newarr(0);
	stations = newarr(0);
	dbpm = dblookup(dbv,0,"mwpm",0,0);
	dbsnr = dblookup(dbv,0,"mwsnr",0,0);
	if((dbpm.record == dbINVALID) || (dbsnr.record == dbINVALID) )
	{
		die(0,"multiwavelet extensions to css3.0 not defined\nCannot continue\n");
	}
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
	if(dbgetv(dbv,0,"arrival.time",&time,0)==dbINVALID)
		die(0,"dbgetv read of arrival time fails on first row of database -- database must be corrupted\n");

	/* compute time pad lengths for each band of the mw transforms */
	pad = ss_compute_tpad(dec_objects, mw, pf);

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
	dtstart = MIN( Window_stime(swinall),Window_stime(nwinall));
	dtend = MAX(Window_etime(swinall), Window_etime(nwinall));

	/* Now we loop through the database view station by station
	using the station grouped view STABDLNAME.  This is actually
	unnecessary in the current implementation as dbv is passed
	as this group pointer, but this is safer. */  
	dbgrp = dblookup(dbv,0,STABDLNAME,0,0);
	if (dbgrp.record == dbINVALID)
		die(0,"Error in dblookup for named station group table = %s\n",
			STABDLNAME);
        dbquery(dbgrp,dbRECORD_COUNT,&narrivals);
        elog_notify(0,"Processing begins for %d arrivals\n",narrivals);

	for(dbgrp.record=0;dbgrp.record<narrivals;++dbgrp.record)
	{
		Dbptr db_bundle;
		int evid; 
		int is, ie; 
		int ierr;

		if(dbgetv(dbgrp,0,"sta", &sta,
                        "bundle", &db_bundle,0) == dbINVALID)
		{
                        elog_complain(1,"dbgetv error for row %d of station group\nAttempting to continue by skipping to next station\n",
                                dbgrp.record);
			continue;
		}
		/* This is an ugly trick done to avoid having to rewrite code
		from mwap to work in this program.  The stations variable in
		mwap is used to hold an associative array of MWstation objects.
		Here we fake this and create an empty MWstation object for
		each station as it is encountered.  Note this function wastes
		memory as it creates a stack of empty MWstation objects
		that gets bigger with each new station processed. */

		check_sta_arr(stations,sta,nbands);


                dbget_range(db_bundle,&is,&ie);

		/* Skip any bundle without at least 3 rows as this is 
		guaranteed to have a missing channel */
		if((ie-is)<3)
		{
			elog_complain(0,"Missing channels for station %s for rows %d to %d of database view\nData skipped\n",
				sta,is,ie);
			continue;
		}

		/* Time windows are laid down relative to the arrival
		time associated with this bundle */
		db_bundle.record = is;
		if(dbgetv(db_bundle,0,"arrival.time",&arrival_time,0))
		{
			elog_complain(0,"dbgetv error reading arrival time for station %s at row %d of station bundled view\n",
				sta,is);
			continue;
		}
		setarr(arrivals,sta,&arrival_time);
        	tr = trnew(NULL,NULL); if(tr.record == dbINVALID)
        	{
                	die(0,"trnew failure for station %s\n",sta);
		}
		tstart = arrival_time + dtstart;
		tend = arrival_time + dtend;
		ierr = mwap_load_stagrp(db_bundle,tr,tstart, tend);
		if(ierr)
		{
			elog_complain(0,"%d read errors while trying to load data for station %s on bundle starting at row %d\n",
				ierr,sta,is);
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

/* TO DO:  If these functions all work with a station count of 1, we can
call these routines directly */

		/* This function computes the multiwavelet transform
		of all traces currently in tr for signals around arrival*/
		mwsig_arr = tr_mwtransform(tr,arrivals,swin,decfac,dec_objects,
				nbands,mw,nwavelets);
/*
debug_mwtrace(mwsig_arr, sta, nbands, nwavelets);
*/

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
					stations,nbands,nwavelets);

		for(i=nbands-1;i>=0;--i)
		{
			Signal_to_Noise *snr;
			double wts, wte;
			fc = (mw[i].f0)/(2.0*si*decfac[i]);
			wts = arrival_time + (swin[i].si)*((double)(swin[i].tstart));
			wte = arrival_time + (swin[i].si)*((double)(swin[i].tend));
			ierr = ssmwpm(i,nwavelets,sta,mwsig_arr,sn_ratios[i],
				wts, wte, up,pf,&avgpm,&avgerr);
			scoor = unit_vector_to_spherical(avgpm.major);
			majaz = deg(scoor.phi);
			majema = deg(scoor.theta);
			scoor = unit_vector_to_spherical(avgpm.minor);
			minaz = deg(scoor.phi);
			minema = deg(scoor.theta);
			time = wts;
			twin = ((double)(swin[i].tend - swin[i].tstart))
					*(swin[i].si);
        		if( dbaddv(dbpm,0,
                		"sta",sta,
                		"bankid",bankid,
                		"fc",fc,
                		"phase",phase,
                		"time",time,
                		"twin",twin,
                		"pmtype","ss",
                		"majoraz",majaz,
                		"majorema",majema,
                		"minoraz",minaz,
                		"minorema",minema,
                		"rect",avgpm.rectilinearity,
                		"errmajaz",avgerr.dphi_major,
                		"errmajema",avgerr.dtheta_major,
                		"errminaz",avgerr.dphi_minor,
                		"errminema",avgerr.dtheta_minor,
                		"errrect",avgerr.delta_rect,
                		"majndgf",avgerr.ndgf_major,
                		"minndgf",avgerr.ndgf_minor,
                		"rectndgf",avgerr.ndgf_rect,
           			"algorithm","mwpm",0) < 0)
			{
				elog_complain(0,"dbaddv error on table mwpm for station %s\n",
					sta);
			}
			snr = (Signal_to_Noise *)getarr(sn_ratios[i],sta);
        		if( dbaddv(dbsnr,0,
                		"sta",sta,
                		"bankid",bankid,
                		"fc",fc,
                		"phase",phase,
	                        "nstime",snr->nstime,
	                        "netime",snr->netime,
	                        "sstime",snr->sstime,
	                        "setime",snr->setime,
	                        "snrz",snr->ratio_z,
	                        "snrn",snr->ratio_n,
	                        "snre",snr->ratio_e,
                        	"snr3c",snr->ratio_3c,

           			"algorithm","mwpm",0) < 0)
			{
				elog_complain(0,"dbaddv error on table mwsnr for station %s\n",
					sta);
			}
		}
		/*release main work spaces with this series of complicated free routines.
		Here is where you really wish C had garbage collection */

		free_sn_ratios_arr(sn_ratios,nbands);
		free_MWtransform_arr(mwsig_arr,nbands,nwavelets);
		free_MWtransform_arr(mwnoise_arr,nbands,nwavelets);
		trdestroy(&tr);
		delarr(arrivals,sta);
	}
}
