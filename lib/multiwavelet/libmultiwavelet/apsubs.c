/* This group of subroutines is are array processing functions used
for the multiwavelet processing code.  All were written by:

Gary Pavlis:$
*/

#include <stdlib.h>
#include <math.h>
#include <perf.h>
#include <ctype.h>
#include "multiwavelet.h"
#include "location.h"


/* This function computes the moveout time for station s 
relative to s0 .  We pass the pointers
to avoid copying all the baggage in these structures.  
This function is an expensive way to do this calculation, but
is used here because we loop through time as the most rapidly
changing variable and this calculation is only repeated when
the slowness vector increments.

*/  

double compute_moveout(MWstation *s, MWstation *s0, MWSlowness_vector *slow)
{
	double dt;
	dt = ((s->deast) - (s0->deast))*(slow->ux)
		+ ((s->dnorth) - (s0->dnorth))*(slow->uy);
	return(dt);
}



/* This routine scans the station geometry associative array, a, 
(it looks at every station) and determines a generous estimation of
the aperture of the array as the corners of a bounding box defined
by the max and min ew and ns stations.  This information is needed 
to properly compute time padding required to compensate for moveout.
The function returns the aperture estimate */

double array_aperture(Arr *a)
{
	Tbl *t;
	char *key;
	int i;
	double elow=0.0,ehigh=0.0,nlow=0.0,nhigh=0.0;
	double aperture;
	MWstation *s;

	t = keysarr(a);

	for(i=0;i<maxtbl(t);i++)
	{
		key = gettbl(t,i);
		s = (MWstation *)getarr(a,key);
		elow = MIN(elow,s->deast);
		ehigh = MAX(ehigh,s->deast);
		nlow = MIN(nlow,s->dnorth);
		nhigh = MAX(nhigh,s->dnorth);
	}
	freetbl(t,0);
	aperture = hypot(ehigh-elow, nhigh-nlow);
	return(aperture);
}
/* This function returns a vector of total decimation factors for
each of the bands used to define the multiwavelet transform.  The
vector if integers is alloced inside this routine and the function
returns a pointer to this vector.  

Arguments:
	d - array of Tbl pointers of length = number of frequency
		bands holding teh decimator object definitions.  
	pf - parameter space (length of d is defined in pf)

Author:  G Pavlis
*/
int *get_decimation_factors(Tbl **d, Pf *pf)
{
	int nbands;
	int i,j;
	int *decfac,dec_previous_band=1.0;

	nbands = pfget_int(pf, "number_frequency_bands");
	decfac = (int *) calloc(nbands,sizeof(int));
	if(decfac == NULL) 
		elog_die(0,"Cannot alloc int array of length %d\n",nbands);
	for(i=0;i<nbands;++i) decfac[i] = 1;

	for(i=0;i<nbands;++i)
	{
		int nstages;
		FIR_decimation *fir;
		nstages = maxtbl(d[i]);
		for(j=0,decfac[i]=dec_previous_band;j<nstages;j++)
		{
			fir = (FIR_decimation *)gettbl(d[i],j);
			decfac[i] *= fir->decfac;
		}
		dec_previous_band = decfac[i];
	}
	return(decfac);
}


/* This function computes and returns a vector of time pad lengths 
(in samples) for multiwavelet transforms.  The time pad vector is alloced
here and returned as a int vector of length = to the number of frequency
bands in the transform.  It computes the time pad as the combined 
time of edge effects of wavelet and decimator filters and a maximum 
moveout computed using the array aperture function (immediately above) 
and a maximum slowness parameter read from the pf object.  

Arguments:  
	d - Tbl array of of decimator objects 
	mw - MWbasis objects
	sta - Arr of MWstation objects 
	pf - parameter space 

Author:  G Pavlis
*/
int *compute_tpad(Tbl **d, MWbasis *mw, Arr *sta, Pf *pf)
{
	double aperture;
	double max_slow;
	double t_prop;
	double si;  /* base sample interval (before decimation) */
	int nbands;
	int pad_prop, padmw, paddec, *pad;
	int decfac,dec_previous_band=1;
	int i,j;

	/* first computer array aperture and maximum time lag */
	aperture = array_aperture(sta);
	max_slow = pfget_double(pf, "maximum_slowness");
	t_prop = max_slow * aperture;
	si = pfget_double(pf, "sample_interval");
	pad_prop = t_prop/si;  /* time pad for propagation at base sample interval*/

	/* now we compute the filter padding.  Mw is easy, decimators
	are much harder */
	padmw = ((mw[0].n)+1)/2;  /* We assume all basis functions have the 
			same length -- necessary condition for original
			definition in lilly and park */

	nbands = pfget_int(pf,"number_frequency_bands");
	pad = (int *) calloc(nbands,sizeof(int));
	if(pad == NULL) elog_die(0,"Cannot alloc int tpad array of length %d\n",
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
		pad[i] += (padmw + pad_prop);
	}
	return(pad);
}
/* These two related functions parse the parameter file pf to 
obtain time window information in each frequency band.  One
defines the signal analysis window and the other defines the 
noise analysis window.  Both use a pf &Tbl definition 
with columns of integers like:
name &Tbl{
-4 50 20 1 1
-4 30 20 1 1
-4 20 20 1 1
}
where in all cases they are in order of start, end, dt in samples.

pad is a vector of length nbands of time padding computed for
each multiwavelet band in original samples.  These are converted
to a time in seconds using the sample interval and set below.

The decfac array is passed (it must have been set previously and
be of lengthe nbands) and the sample interval element of the 
Time_window is set based on the parameter file input and decfac 
for the specified band.  

Both functions have a pf input and return pointer to an array of Time_Window
structures.  

Author:  G. Pavlis
*/
Time_Window *get_signal_windows(int *decfac, int *pad, Pf *pf)
{
	Tbl *t;
	char *line;
	int nbands,nt;
	Time_Window *win;
	int swin,ewin;  /* temporaries used to avoid indirection
			confusions with complicated structure*/
	int i;
	double si;

	nbands = pfget_int(pf,"number_frequency_bands");
	si = pfget_double(pf,"sample_interval");

	t = pfget_tbl(pf,"signal_analysis_windows");
	if(t == NULL) elog_die(0,"signal_analysis_windows parameter not found\n");

	win = (Time_Window *)calloc(nbands, sizeof(Time_Window));
	if(win == NULL) elog_die(0,"Cannot alloc memory for %d time window structures\n",
				nbands);

	nt = maxtbl(t);
	if(nt != nbands) elog_complain(0,
	  "Signal Time Window Definition mismatch\nExpected definitions for %d bands, found %d\nList will be truncated or expanded with the last entry\n",
			nbands,nt);
	if(nt>nbands) nt=nbands;

	for(i=0;i<nt;++i)
	{
		line = gettbl(t,i);
		sscanf(line,"%d%d%d%d%d",&swin,&ewin,
			&(win[i].length), &(win[i].stepsize),
			&(win[i].increment));
		win[i].tstart = swin;
		win[i].tend = ewin;
		win[i].tpad = si*((double)pad[i]);
		win[i].si = si*((double)decfac[i]);
	}
	for(i=nt;i<nbands;++i)
	{	
		win[i].tstart = win[nt-1].tstart;
		win[i].tend = win[nt-1].tend;
		win[i].tpad = win[nt-1].tpad;
		win[i].length = win[nt-1].length;
		win[i].stepsize = win[nt-1].stepsize;
		win[i].increment = win[nt-1].increment;
		win[i].si = si*((double)decfac[i]);
	}
	return(win);
}
/* This is a near duplicate of the above for noise window */
Time_Window *get_noise_windows(int *decfac, int *pad, Pf *pf)
{
	Tbl *t;
	char *line;
	int nbands,nt;
	Time_Window *win;
	int swin,ewin;  /* temporaries used to avoid indirection
				confusions with complicated structure*/
	int i;
	double si;

	nbands = pfget_int(pf,"number_frequency_bands");
	si = pfget_double(pf, "sample_interval");
	t = pfget_tbl(pf, "noise_analysis_windows");
	if(t == NULL) elog_die(0,"noise_analysis_windows parameter not found\n");

	win = (Time_Window *)calloc(nbands, sizeof(Time_Window));
	if(win == NULL) elog_die(0,"Cannot alloc memory for %d noise time window structures\n",
				nbands);

	nt = maxtbl(t);
	if(nt != nbands) elog_complain(0,
	  "Noise Time Window Definition mismatch\nExpected definitions for %d bands, found %d\nList will be truncated or expanded with the last entry\n",
			nbands,nt);
	if(nt>nbands) nt=nbands;

	for(i=0;i<nt;++i)
	{
		line = gettbl(t,i);
		sscanf(line,"%d%d%d%d%d",&swin,&ewin,
			&(win[i].length), &(win[i].stepsize),
			&(win[i].increment));
		win[i].tstart = swin;
		win[i].tend = ewin;
		win[i].tpad = si*((double)pad[i]);
		win[i].si = si*((double)decfac[i]);
	}
	for(i=nt;i<nbands;++i)
	{	
		win[i].tstart = win[nt-1].tstart;
		win[i].tend = win[nt-1].tend;
		win[i].tpad = win[nt-1].tpad;
		win[i].length = win[nt-1].length;
		win[i].stepsize = win[nt-1].stepsize;
		win[i].increment = win[nt-1].increment;
		win[i].si = si*((double)decfac[i]);
	}
	return(win);
}
double Window_stime(Time_Window win)
{
	double t;
	t = (win.si)*((double)win.tstart);
	t -= win.tpad;
	return(t);
}
double Window_etime(Time_Window win)
{
	double t;
	t = (win.si)*((double)win.tend);
	t += win.tpad;
	return(t);
}

	
	
/* This function scans an array of Time_Window structures and
a parallel array of decimation factors (decfac) returning 
a Time_Window structure containing the earliest start time
and latest end time in units of the original sample interval.  

Arguments:
	win - array of Time_Window structures to be scanned.
	decfac - parallel array of integer decimation factors.
	n - number of elements in win and decfac.
Author:  G Pavlis
*/
Time_Window compute_time_window(Time_Window *win, int *decfac, int n)
{
	int i;
	Time_Window t;
	double s,e;  /* temporaries for start and end time in seconds */

	if(n<1)elog_die(0,"Invalid array length %d passed to compute_time_window\n",
				n);
	/* Note tpad is time pad for each that the decimators and
	wavelet truncate the signal.  Hence we pad both left and
	right sides.*/
	for(i=1,
	s = Window_stime(win[0])-win[0].tpad,
	e = Window_etime(win[0])+win[0].tpad;
		i<n;i++)
	{
		s = MIN(s,Window_stime(win[i])-win[i].tpad);
		e = MAX(e,Window_etime(win[i])+win[i].tpad);
	}
	/* We use a nint because we assume there are other fudge factors
	that assure rounding makes no significant difference. */
	t.si = win[0].si;  /* We want to use the smallest si to minimize step size.
				The layout of the mw transform demands that win[0]
				have the smallest si */
	t.tstart = nint(s/(t.si));
	t.tend = nint(e/(t.si));
	t.tpad = 0.0;  /* We account for tpad in above computation */
	/* These aren't used at present, but
	better to set them */
	t.length = t.tend - t.tstart;
	t.stepsize =1;  t.increment=1;
	return(t);
}
/* Short little function returns true if it thinks the string pointed
to by phase defines a phase that is expected to emerge as an S phase.
For most phases this is is defined by the last letter in the string
being "S".  This doesn't work, however, for things like Sn.  To make
them work right, I strip any lower case trailing letters and look 
only at the last upper case letter.  Crude, but should work most
of the time.

Author:  G Pavlis
*/
int is_S(char *phase)
{
	int i;
	i = strlen(phase);
	--i;
	while( (i>0) && (!isupper((int)phase[i]))) --i;
	if(i<0) return(0);
	if(phase[i+1] == 'S') return(1);
	return(0);
}
/* This routine implements an approximation method used in mwap
to correct for wavefront curvature and spherical geometry effects
in large aperture arrays.  It computes the difference in time
computed using a whole earth model travel time calculator between
each station and a the reference station and that computed by 
using the slowness vector determined from the model at the
reference station propagated with the standard plane wave formula.
These are stored as the plane_wave_statics field in the MWstation
stuctures.

Arguments:
	s - pointer to MWstation structure to compute plane
		wave static for.
	s0 - pointer to MWstation structure of reference station.	
	source - source coordinates (TTPoint is defined in tt.h).
	phase - phase name (passed to travel time calculator so 
		it best be consistent with what it knows.)
	pf - parameter file (used to get method and model parameters
		passed to travel time calculator.

Normal return is 0.  
+1 - travel time error computing plane wave static.  Sets pwstatic field to 0
-1 - detected phase name change between reference and requested station.
	pwstatic field is set to zero, but data should probably not be
	used as this may indicate a travel time curve discontinuity 
	(e.g. at the core shadow)
Modified:  November 2000
Found we needed to send a special return if the phase name changed
between reference station and requested station.  Reason was that
bad things happened at the core shadow.
*/
static Hook *hook=0;  /*Used by ttcalc */
int compute_plane_wave_static(MWstation *s, 
	MWstation *s0, 
	TTPoint source,
	char *phase,
	Pf *pf)
{
	char *model;
	char *method;
	TTGeometry geometry;
	TTTime *atime;
	TTSlow *u0,*u;
	char refphase[TTPHASE_SIZE];
	MWSlowness_vector slow;
	int tresult,uresult;
	Tbl *treturn=NULL,*ureturn=NULL;

	double t0;  /* computed time and slowness at reference station */
	double tpw;  /*t0 + u dot x */

	model = pfget_string(pf,"TTmodel");
	method = pfget_string(pf,"TTmethod");
	if( (model == NULL) || (method == NULL) )
	{
		elog_complain(0,"TTmodel or TTmethod missing from parameter file\nPlane wave static corrections set to 0 for station %s\n",
			s->sta);
		s->plane_wave_static = 0.0;
		return(1);
	}
	/* It is inefficient to recompute the reference time
	and slowness for each station, but I judged this preferable
	to having this messy stuff appear in two different function. 
	Note that receiver elevation is set to 0.0.  This is done 
	because elevation statics are computed sperately here.  I 
	don't bother with using the reference elevation as datum because
	it will make no difference for this computation because we
	difference two times computed to a common datum.  This would
	only induce an error for sources very close to the array and
	even then it is probably second order.*/
	geometry.source.lat = source.lat;
	geometry.source.lon = source.lon;
	geometry.source.z = source.z;
	geometry.source.time = source.time;
	strcpy(geometry.source.name,"source");

	strcpy(geometry.receiver.name,s0->sta);
	geometry.receiver.lat = s0->lat;
	geometry.receiver.lon = s0->lon;
	geometry.receiver.z = 0.0;

	tresult = ttcalc(method,model,phase,0,&geometry,&treturn,&hook);
	uresult = ucalc(method,model,phase,0,&geometry,&ureturn,&hook);
	if(tresult || uresult) 
	{
		elog_complain(0,"Error in computing time or slowness for reference station for method=%s, model=%s\nCannot compute statics\n",
			method,model);
		s->plane_wave_static = 0.0;
		freetbl(treturn,free);
		freetbl(ureturn,free);
		return(1);
	}
	/* We can assume the phase code passed asks for only one 
	computed travel time = the one we want */
	atime = (TTTime *)gettbl(treturn,0);
	u0 = (TTSlow *)gettbl(ureturn,0);
	t0 = atime->value;
	slow.ux = u0->ux;
	slow.uy = u0->uy;
	strcpy(refphase,atime->phase);

	tpw = t0 + compute_moveout(s,s0,&slow);

	strcpy(geometry.receiver.name,s->sta);
	geometry.receiver.lat = s->lat;
	geometry.receiver.lon = s->lon;
	geometry.receiver.z = 0.0;

	tresult = ttcalc(method,model,phase,0,&geometry,&treturn,&hook);
	if(tresult)
	{
		elog_complain(0,"Error computing travel time from model %s using method %d for station %s\nStatic set to zero\n",
			model, method, s->sta);
		s->plane_wave_static = 0.0;
		freetbl(treturn,free);
		freetbl(ureturn,free);
		return(1);
	}
	if(strcmp(atime->phase,refphase))
	{
		elog_complain(0,"Warning:  error computing plane wave static for station %s\nReference station phase = %s while phase at this station = %s\nLikely travel time curve discontinuity.  Setting pwstatic to 0.0\n",
			s->sta,atime->phase,refphase);
		s->plane_wave_static = 0.0;
		freetbl(treturn,free);
		freetbl(ureturn,free);
                return(-1);
        }
	else
	{
		atime = (TTTime *)gettbl(treturn,0);
		s->plane_wave_static = (atime->value) - tpw;
	}
	freetbl(treturn,0);
	freetbl(ureturn,0);
	return(0);
}	

/* This function is a higher level version of the preceding function.
It repeatedly calls compute_plane_wave_static for every station
in the collection of MWstations indexed through the stations Arr.

Arguments:
	stations - Arr keyed by station names of pointers to MWstation
		structures
	refsta - name of reference station (used in lookup of stations arr)
	phase - phase name to use for travel time computations 
	db - db pointer to A SPECIFIC ROW of a datascope db (can be a view)
		that has origin attributes.  i.e. we will call dbgetv 
		directly with this pointer to extract a hypocenter from
		origin table attributes (lat,lon, depth, time).
	pf - parameter file pf object (passed down to compute_plane_wave_static
		function.

Normal return is 0.  Nonzero is count of failures in computing statics.

Author:  Gary Pavlis
*/
int set_pwstatics(Arr *stations,
	char *refsta, 
	char *phase,
	Dbptr db,
	Pf *pf)
{
	Tbl *t;  /* used to hold keys with keysarr */
	char *key;  /* key returned from Tbl *t */
	int number_errors=0;
	int i;
	MWstation *s, *s0; 
	TTPoint source; 
	int orig_error=0;

	s0 = (MWstation *)getarr(stations,refsta);
	if(s0 == NULL) elog_notify(0,"set_pwstatics cannot find geometry information for reference station %s\nStatics will all be set to 0\n",
		refsta);
	t = keysarr(stations);

	/* now get the origin information.  I intentionally do 
	not bother to set the name field in the source structure. */
	if(dbgetv(db,0,"origin.lat",&(source.lat),
		"origin.lon",&(source.lon),
		"origin.depth",&(source.z),
		"origin.time",&(source.time),0) == dbINVALID)
	{
		elog_notify(0,"set_pwstatics:  dbgetv error reading origin data\nStatics will all be set to zero\n");
		orig_error=1;
		number_errors = maxtbl(t);
	}
	for(i=0;i<maxtbl(t);i++)
	{
		key = gettbl(t,i);
		s = (MWstation *)getarr(stations,key);
		if((s0 == NULL) || orig_error)
			s->plane_wave_static = 0.0;
		else if(compute_plane_wave_static(s,s0,source,phase,pf))
			++number_errors;
	}
	freetbl(t,0);
	return(number_errors);
}
			
/* This function computes a elevation static from an exact 
slant path correction using v0 values stored in MWstation 
structure.

arguments:
	s - pointer to MWstation structure to compute and set
		elevation static in (i.e. we set s->elevation_static).
	u - slowness vector to use to compute elevation static
	elevref - elevation reference elevation to compute static
		relative to.
	phase - name of this seismic phase (used to decide whether to 
		use P or S surface velocity based on result of is_S function.

Author: G Pavlis
*/
double compute_elevation_static(MWstation *s,MWSlowness_vector u,
			double elevref, char *phase)
{
	double slowness;
	double delta_elev;
	double pv,theta;

	delta_elev = s->elev - elevref;
	slowness = hypot(u.ux,u.uy);
	if(is_S(phase))
		pv = slowness*(s->vs0);
	else
		pv = slowness*(s->vp0);
	theta = asin(pv);
	return(delta_elev*slowness*cos(theta));
}
/* It is necessary to initialize all residual static terms to 0.0
within the function below.  This routine does this.
The only argument is an Arr that contains indexed pointers to MWstation
structures.  Here we just use keysarr and run through the whole array.

Author:  Gary pavlis
*/
void initialize_residual_statics(Arr *s)
{
	Tbl *t;
	char *key;
	int i;
	MWstation *sta;

	t = keysarr(s);
	for(i=0;i<maxtbl(t);++i)
	{
		key = gettbl(t,i);
		sta = (MWstation *)getarr(s,key);
		sta->residual_static = 0.0;
	}
	freetbl(t,0);
}
/* This function is called once per event to set a time reference
that all the data are referenced to.  The algorithm used is to 
fix the time reference as the measured arrival at the station whose
name is passed via the argument refsta.  If refsta has no measured
arrival, it gets ugly.  There are lots of ways one might deal with
this, but I do this is a probably overly complex way.  The code
scans the arrival list finding the station closest to refsta.  
The measured arrival time for that station is corrected for 
plane wave moveout and the plane wave static term to produce a
pseudoarrival time at refsta.   Because of the light dependence
on the slowness vector with widely spaced arrays it may be prudent
to recall this routine as the slowness vector gets adjusted.  

The function returns and epoch time that should be used as the 
time reference.  It logs a message to elog whenever the reference
is not refsta.  

Author:  Gary L. Pavlis
Written:  Sept. 1999
*/
double compute_time_reference(Arr *stations, Arr *arrivals,
	char *refsta,MWSlowness_vector u)
{
	Tbl *t;
	int ndata;
	/*sref is saved as the pointer to the standard reference 
	station.  spseudo points to the inferred pseudo-reference 
	when needed*/
	MWstation *s,*sref,*spseudo;  
	double *reftime;
	double pref_atime;  /*arrival time at pseudoref station*/
	int i;
	char *key;
	double *atptr;
	double dist=HUGE_VAL;   /* infinity defined in math.h*/

	sref = (MWstation *)getarr(stations,refsta);
	reftime = (double *)getarr(arrivals, refsta);
	/* Normal return when refsta is found */
	if(reftime!=NULL) return(*reftime);

	/* Have to do this if refsta doesn't record this event */
	t = keysarr(arrivals);
	ndata = maxtbl(t);

	for(i=0;i<ndata;++i)
	{
		key = gettbl(t,i);
		atptr = (double *)getarr(arrivals,key);
		s = (MWstation *)getarr(stations,key);
		if(s==NULL) continue;
		if(hypot(s->deast,s->deast)<dist)
		{
			pref_atime = *atptr;
			spseudo = s;
		}
	}
	/* correct for moveout and plane_wave_static.  Note sign reversal*/
	pref_atime -= spseudo->plane_wave_static;
	pref_atime -= compute_moveout(spseudo,sref,&u);
	elog_log(0,"Reference station %s has no arrival for event at epoch time %lf\nUsing station %s arrival time corrected for moveout\n",
			refsta,pref_atime,spseudo->sta);
	freetbl(t,0);
	return(pref_atime);
}
	
#define MAXCON 1000.0  /*maximum condition number before sv truncation*/
#define USCALE 0.1  /* Slowness nondimensional scale factor in convergence
			loop.  Value here is for 10 km/s -- */
#define CONVERGE_TEST  0.00005  /* break robust loop when change in 
				slowness vector length / USCALE is less
				than this constant.  */
#define MAXIMUM_ITERATION 20
#define ESCALE_MIN 0.1   /* minimum error scale for residual vector.
			M-estimators require a minimum error scale to
			remain stable.  Otherwise the 1/error scaling 
			can yield NaN values (worst case) or a gradual
			downward spiral in weights.  Value here should
			be nominal scale in seconds (stuck with units
			here).  It is better for this algorithm to make
			this too big than too large.  Standard value
			used here is reasonable for teleseismic data */

/* This routine estimates a slowness vector from scratch based on the
current collection of arrival times (passed by arrivals associative
array) corrected using current statics stored in MWstations structures
(stored and indexed by stations associative array).  The algorithm
used will always first compute a simple least squares estimate 
using the current set of (static corrected) times followed by an m-estimator
to refine that estimate in an attempt to reduce the impact of outliers. 
Normal return is 0, nonzero returns indicate error messages are posted
by elog routines (see below). 


Note this is the base routine that also determines the set of static
corrections produced by the program.  These are dynamic and change
with every time the slowness vector is estimated.  Because that problem
is linear this is always done from a base starting point.  The
"initial_static" field is important as this is applied before any 
computations.  This is a useful way to keep large timing errors
from having a large impact on the estimated slowness vector as these
are subtracted before the initial estimate.  The method used to 
set the initial static field is under evolution.  Initially I expected
it to just be an input parameter for stations, but considering the 
amount of data with timing problems it might be useful to allow 
initial arrival estimates to set initial statics on stations with
timing problems.  (Not implemented as of march 2000).  Note also 
that the output residual static is the sum of the initial static and
the computed perturbation relative to that value.  

PS to above, March 2000:  The idea of "initial_static" here is 
implemented, but it is not recommended unless one is desperate to 
include data with a timing problem.  I started with that idea, but
moved to using a new parameter called "time_is_bad" to explicitly 
turn off stations with timing problems in this context.  The 
initial_static should only be used if a station with bad timing
is required to get a stable slowness vector solution (e.g. if
all the stations ended up colinear) 

Arguments:
	u0 - initial slowness vector estimate (returned value will
		be absolute, but interally will be a perturbation 
		from this value.
	arrivals - associative array of current arrival times 
	stations - associative array of MW station entities containing
		geometry and current static information. 
	refsta - reference station name 
	refelev - reference elevation used for geometric static 
		correction 
	phase - name of seismic phase being analyzed 
		(is_S_phase function is called defaulting to P)
	band - band index used to define correct station dependent
		weights.   That is, for each MWstation object s 
		the solution is weighted by s->weights[band].
		This allows variable aperture arrays with frequency
		and a mechanism to turn off stations with timing
		problems (setting weights to 0)
	slow - main return = new slowness vector estimate.  

Errors:
-  malloc errors cause the function to call die
-  If the the geometry for an "arrival" entry cannot be found
	in the stations array, a diagnostic is issued.  No statics
	will be estimated for that datum and this is considered 
	harmless unless it leads to a critical data loss.  The 
	noncritical errors are flagged by adding one to the
	return code (i.e. what the function returns) whenever
	this error occurs.  When less than 3 stations survive 
	this winnowing, the returned slow is set to the same
	as u0 (i.e. is does nothing) and returns and error
	code of -1.  The calling routine may want to handle this
	differently in different contexts.

History:  Originally written summer 1999

Modified March 2000 as follows:
1.  Recognizing a potential problem with timing problem in some 
data sets, I decided to fully implement station variable weights
passed through the weights vector stored in the MWstation object.
This required passing "band" to correctly index this array.  
2.  A cascade from 1 was that I recognized that the output should
contain information about the weight given a particular station
in a final solution.  Consequently, I added a current_weight field
to the MWstation object definition that is SET in this function.  
When a later routine is called to extract the static information from
the MWstation object it can then extract the current weight to be
presumably stored in an output database table.  

Author:  G Pavlis

History:
Originally written late 1999
Modified: August 2000
Added logic for a null space projector after each iteration.  Found
that previous version led to a biased slowness estimate that preserved
relics of picking errors in previous passes.  
*/
int estimate_slowness_vector(
			MWSlowness_vector u0,
			Arr *arrivals, 
			Arr *stations,
			char *refsta,
			double refelev,
			double time_ref,
			char *phase,
			int band,
			MWSlowness_vector *slow
			)
{
	double *atime;
	MWstation *s;
	MWstation **sptr;  /*Array of pointers derived from s used 
				internally to simplify things */

	double x[3];  /* x is total estimate vector, 
			x[0] = du_deast, x[1]=du_dnorth, x[2] = dtime */
	double *A,*Aw;  /* Matrices used in estimate.  Aw is row weighted*/
	double *b,*bw;  /*right hand side vectors.  bw is weighted vector */
	double *U, vt[9];  /*I use an SVD for numerical stability.  These hold
			orthogonal matrices in SVD estimate */
	double sval[3];  /* Singular values go here */
	double *Uraw;  /* We store the singular vectors of the unweighted
			matrix A to use to compute a null space projectot
			applied to the residual static vector after each
			iteration. */

	MWstation *sref;
	MW_scalar_statistics stats;
	double error_scale;
	double *weight;
	double *reswt;
	Tbl *t;  /* The arrival list table of keys returned by keysarr*/
	int m;  /* size of storage */
	int ndata;  /* Actual number of data points (can be less from errors)*/
	int i,j;
	int mused;
	char *key;
	int error_code=0;
	int nsvused;
	int iteration=0;
	double dtref;
	double dutest;
	int info;

	t = keysarr(arrivals);
	m = maxtbl(t);

	allot(double *,A,m*3);
	allot(double *,Aw,m*3);
	allot(double *,b,m);
	allot(double *,weight,m);
	allot(double *,reswt,m);
	allot(double *,bw,m);
	allot(double *,atime,m);
	allot(double *,U,m*3);
	allot(double *,Uraw,m*3);
	allot(MWstation **,sptr,m);

	/* It is necessary to always clear the residual static field
	in the MWstations structure and set all of them to 0 before
	entering this algorithm.  All need to be cleared rather than 
	doing it below because not all stations record all events */
	initialize_residual_statics(stations);

	slow->ux = u0.ux;
	slow->uy = u0.uy;
	slow->refsta = refsta;


	/* Now we loop through the arrival list forming A, b, and keeping
	arrival times (atime) for later use.  Notice that A is filled
	parallel with t entries to size m leaving 0s if an error occurs. 
	This means ndata is only a counter.  We also fill sptr 
	array of pointers with a parallel set of s pointers*/
	for(i=0,ndata=0;i<m;++i)
	{
		double *atptr;
		key = gettbl(t,i);
		atptr = (double *)getarr(arrivals,key);
		s = (MWstation *)getarr(stations,key);
		if(s == NULL)
		{
			elog_complain(0,"estimate_slowness_vector function cannot find geometry information for station %s\nData from this station will be ignored\n",key);
			A[i] = 0.0;
			A[i+m] = 0.0;
			A[i+2*m] = 0.0;
			atime[i] = *atptr;
			sptr[i] = NULL;
			++error_code;
		}
		else
		{
			A[i] = s->deast;
			A[i+m] = s->dnorth;
			A[i+2*m] = 1.0;
			atime[i] = *atptr;
			sptr[i] = s;
			++ndata;
		}
	}		
	    /* We cannot proceed unless we have 3 stations worth of data */
	if(ndata < 3)
	{
		elog_complain(0,"estimate_slowness_vector critical data loss\nMissing stations led to only %d stations\n",
			ndata);
		slow->ux = u0.ux;
		slow->uy = u0.uy;
		slow->refsta = u0.refsta; 
		error_code = -1;
	}
	else
	{
	    /* We compute the left singular vectors for the 
	    raw matrix and save these to compute the null projector
	    below */
	    dcopy(3*m,A,1,Uraw,1);
	    dgesvd('o','a',m,3,Uraw,m,sval,NULL,m,vt,3,&info);

	    /* We assume here that the reference station is defined
	    and we trapped this potential error earlier */
	    sref = (MWstation *)getarr(stations,refsta);

	    /* top of robust estimation loop */
	    do {
		for(i=0;i<m;i++) 
		{
			b[i] = atime[i] - time_ref;
			if((sptr[i]) != NULL)
			{
				b[i] -= sptr[i]->initial_static;
				b[i] -= sptr[i]->plane_wave_static;
			   	b[i] -= compute_moveout(sptr[i],sref,slow);
				b[i] -= compute_elevation_static(sptr[i],
						*slow,refelev,phase);
				b[i] -= (sptr[i]->residual_static);
				if(sptr[i]->clock_is_bad)
					weight[i] = 0.0;
				else
					weight[i] = sptr[i]->weights[band];
			}
			else
			{
				b[i] = 0.0;
				weight[i] = 0.0;
			}
		}
		dcopy(3*m,A,1,Aw,1);
		/* To properly compute statistics we have to throw
		out zero weight stations so we do this more complex
		copy from b to the bw work space */
		for(j=0,mused=0;j<m;++j)
		{
			/*We use an absolute test here because weights
			that are used here are assumed between 0 and 1.
			In practice weights smaller than about 0.0001 are
			effectively 0 anyway.  */
			if(weight[j]>0.0001)
			{
				bw[mused] = b[j];
				++mused;
			}
		}
		stats = MW_calc_statistics_double(bw,mused);
		error_scale = NORMAL_IQSCALE*((stats.q3_4)-(stats.q1_4));
		if(error_scale < ESCALE_MIN) error_scale = ESCALE_MIN;
		/* Here we just zero rows that have zero weight and 
		don't worry about it since it will have not effect other
		than wasting some computer time--minor concern unless
		a lot of data are killed */
		for(i=0;i<m;++i)
		{
			double total_weight;
			/* demean the data using the median on 
			the fly.  Since dc is dropped anyway this
			is needed since the dc will float about
			otherwise and lead to incorrect weights */
			bw[i] = b[i] - stats.median;
			reswt[i] = (double)huber(bw[i]/error_scale);
			total_weight = weight[i]*reswt[i];
			bw[i] *= total_weight;
			dscal(3,total_weight,Aw+i,m);
		}
		/* solve the simple least squares system.  We copy A to U because
		dgesvd destroyed the input matrix and replaces it with left singular
		vectors */
		dcopy(3*m,Aw,1,U,1);
		dgesvd('o','a',m,3,U,m,sval,NULL,m,vt,3,&info);

		nsvused = pseudo_inv_solver(U,vt,sval,m,3,bw,MAXCON,x);
		if(nsvused != 3) 
			elog_complain(0,"Singular value truncation required to stablize inverse for slowness vector\n");

		/* This computes the new slowness vector  */
		slow->ux  += x[0];
		slow->uy  += x[1];
		time_ref += x[2];

		/* Now we recompute the residuals with this new slowness 
		vector and use them to form the new residual statics.
		Note that because the problem is linear, we can do 
		this here by just recomputing the time between current
		values stored in atime and time corrected by current 
		moveout and elevation static (i.e. the residual based
		on the new slowness vector estimate.) Further we
		do this even for zero weight stations */
		for(i=0;i<m;i++) 
		{
			if(sptr[i] != NULL)
			{
				b[i] = atime[i] - time_ref;
				b[i] -= sptr[i]->initial_static;
				b[i] -= sptr[i]->plane_wave_static;
				/* both the moveout and elevation static change
				with slowness vector estimate so we recompute them.
				The elevation_static is probably unnecessary, but
				the computational cost is small. */
			   	b[i] -= compute_moveout(sptr[i],sref,slow);
				sptr[i]->elevation_static = compute_elevation_static(
								sptr[i],
                                                		*slow,refelev,phase);

				b[i] -= sptr[i]->elevation_static;
				/* IMPORTANT:  This sets this term for
				later in the program.  Signal processing
				routines use only this weight */
				sptr[i]->current_weight_base = 
							sptr[i]->weights[band];
			}
			else
				b[i] = 0.0;
		}
		/* Here we apply a projector that forces the set of 
		residual statics we compute to be unbiased wrt to the 
		slowness vector estimated.  This also minimizes the
		amount of information placed in statics relative to
		slowness.  Note we recycle bw here as a work space
		to hold the projected b vector. */
		if(ndata>3)
			null_project(Uraw,m,3,b,bw);
		else
		{
			double rmean;
			dcopy(m,b,1,bw,1);
			/* In this case it is necessary to remove the mean */
			for(i=0,rmean=0.0;i<m;++i)rmean+=bw[i];
			rmean /= ((double)ndata);
			for(i=0;i<m;i++)
				if(sptr[i] != NULL) bw[i] -= rmean;
		}
		for(i=0;i<m;i++) 
		{
			if(sptr[i] != NULL)
				sptr[i]->residual_static = bw[i];
		}
		/* We compute a convergence test as a ratio of the 
		correction to the slowness vector to fixed slowness
		vector scale.  We use this norm for several reasons:
		1.  A decent check on the residual static vector
			is a bit hard to deal with for several reasons.
		2.  We can't use a nondimensional slowness vector ratio
			(e.g. deltau/u0) very easily because u0 may
			be small.  
		The dark side of this is it leaves a unit dependence
		upon the slowness vector used.  See the define above
		for the units used.  Note the ndata test assures this
		loop is only executed once when ndata==3 */
		dutest = hypot(x[0],x[1])/USCALE;
		++iteration;
	    } while ( (dutest > CONVERGE_TEST) 
			&& (iteration<MAXIMUM_ITERATION) 
			&& (ndata>3) );
	    if(iteration >= MAXIMUM_ITERATION)elog_complain(0,
		"WARNING:  statics m-estimator inversion did not converge\n");
	}
	free(A);
	free(Aw);
	free(b);
	free(bw);
	free(weight);
	free(reswt);
	free(atime);
	free(U);
	free(Uraw);
	free(sptr);
	freetbl(t,0);

	return(error_code);
}
