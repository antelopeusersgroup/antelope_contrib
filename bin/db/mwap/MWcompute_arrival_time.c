#include <sunmath.h>
#include "multiwavelet.h"
#include "mwap.h"

#define DT_MAX_ITERATION 40
/* We terminate the loop when the final adjustment is this multiple
of the sample rate for this band. */ 
#define DT_FRACTION_TERMINATE 0.9
/* This value yields a minimum scale factor for angles of approximately
+ to 1 20 degrees.   The value equals 0.5 radians */
#define MIN_PHI_SCALE 0.50
/* large phase angle jumps are dangerous because of the way this algorithm
works.  computed phase angles larger than this value will be 
truncated to this size.  Note because we try to unwrap the phase the 
largest possible angle is 180 degrees.  Setting this too small can force
no iterations for a wavelet short enough that phi of one sample is
less than this limit.
*/
#define PHASE_ANGLE_LIMIT 2.3561945

/* This is a replacement for a function of a similar name in the older version of this
program that used the principal component method.  It takes an input gather and applies the
MWrobuststack function to compute a set of arrival times and amplitudes for the ensemble. 
It is a descendant of the function compute_mw_arrival_times that used a principal component
method instead of the robust stack function.  

Arguments:
	g - input multiwavelet transformed gather object
	nwavelets - number of wavelets in multiwavelet transform 
	timeref - absolute time of time zero reference
	moveout - vector of moveout values relative to timeref
	polarization - in 3C arrays data are rotated to this
		direction in space before stacking.
	win - Signal time window to use for analysis.  It is assumed
		this window is smaller to or equal to the time span of
		the data in g.
	snrarr - associative array of Signal_to_noise structures used
		for robust stacking.
	guipf - name of parameter file written by graphical user
		interface that controls stacking window
	arrival - (output)  associate array of absolute arrival
		times computed from this band.  This array is
		always cleared and created fresh.  Be careful
		that arrival is correctly initialized.  The routine
		checks for a null pointer and uses this as a hint to
		create a new Arr structure.  If the arrival pointer is
		not null an attempt is made to free this array.  This
		is slightly dangerous as it assumes arrival has been
		initialized to either a valid Arr or a null.  If it is
		a noninitialized value a seg fault is highly likely.
	results_array - (output) associate array of multiwavelet 
		auxiliary parameter estimates (mostly error estimates)
		(Same warning here as for arrival.  They are handled 
		the same.)
	avgamp, amperr, and ampndgf are (output) the average 
		amplitude, rms amplitude scatter, and degrees of
		freedom for amplitude estimate for entire array.
*/
MWstack *MWcompute_arrival_times(MWgather **g,int nwavelets,double timeref,
        double *moveout, Spherical_Coordinate polarization,
	Time_Window win,
	Arr *snrarr, char *guipf,
	Arr **arrival, Arr **results_array,
        double *avgamp, double *amperr, int *ampndgf)
{
        int nsta,nsta_used,nsta_test;
        int i,j,ii;
	complex *A=NULL;  /* Fortran style matrix buffer */
	/* coherence matrix and beam coherence */
	double **coh,*cohb=NULL;
        double U[9];  /* transformation matrix */
        MWgather **trans_gath;
	double **noise;  /* matrix of noise level estimates nwavelet by nsta */
	double noise_total;
	Signal_to_Noise *snr;
	double minimum_moveout; 
        double *current_moveout;  /* initially copy of moveout, but changes
                                        with updates in time shifts */
        int *lags=NULL;  /* base lag computed from moveout for each
                        station.  Stored in a parallel array to
                        moveout and elements of MWgather */
	int window_length;
	double *timeweight;
 
        complex *centroid;
	complex *cwork;
	float *work,*r,*phi,*iqr,*iqphi, *dt;
        float *work2;
        float *weights;
	double *dweights;
        MW_scalar_statistics stats;
        double dtmax;
        int iteration=0;
        double sigma_t;
	complex ***z;  /* buffer of complex traces to be stacked with MWstack.
			z[i][j][k] is the kth time sample for station j from 
			wavelet i */
	MWstack *stack=NULL;
    	double deltat;  
	Tbl *kill_list;
	Pf *pf;
	int keeptrying=1;
	int discard=0;
	char inbuf[10];
	double ts0,ts1,te0,te1;
	int reset_arrival_times=0;

	/* This is probably unnecessary, but better safe than sorry*/
	if((*results_array)!=NULL) freearr(*results_array,free);
	*results_array = newarr(0);
	if((*arrival)!=NULL) freearr(*arrival,free);
	*arrival=newarr(0);

	/* First we define a rotation matrix to ray coordinates and
	then form new gathers rotated into ray coordinates */
	ray_coordinate_trans(polarization,U);
	
	allot(MWgather **,trans_gath,nwavelets);
	for(i=0;i<nwavelets;++i)
	{
		if(g[i]->ncomponents < 3)
			trans_gath[i] = MWgather_copy(g[i]);
		else
			trans_gath[i] = MWgather_transformation(g[i],U);
	}
	nsta = trans_gath[0]->nsta;
	/* If the number of stations transformed is too small we
	don't want to proceed.*/
	if(nsta<3)
	{
		elog_complain(0,"compute_mw_arrival: number of stations in the input gather = %d\nAt least 3 stations are required to compute slowness and statics\n",
			nsta);
		for(i=0;i<nwavelets;++i) 
			free_MWgather(trans_gath[i]);
		return(NULL);
	}
	/* The robuststack function requires an estimate of the noise level.
	We derive it here from the Signal_to_noise objects where they were
	previously computed. */ 
	allot(double **,noise,nwavelets);
	for(j=0;j<nwavelets;++j) allot(double *,noise[j],nsta);
	for(j=0;j<nsta;++j)
	{
		snr = (Signal_to_Noise *)getarr(snrarr,trans_gath[0]->sta[j]->sta);
		if(snr==NULL) 
		{
			elog_complain(0,"Cannot locate signal-to-noise object for\
station %s\nTurning this station off\n",
				trans_gath[0]->sta[j]->sta);
			for(i=0;i<nwavelets;++i)
			{
				noise[i][j]=9.99e99;
			}
		}
		else
		{
			if(trans_gath[0]->ncomponents < 3)
				noise_total = snr->noise_z;
			else
			{
				noise_total = 0.0;
				noise_total += ((snr->noise_e)*U[6])
						* ((snr->noise_e)*U[6]);
				noise_total += ((snr->noise_n)*U[7])
						* ((snr->noise_n)*U[7]);
				noise_total += ((snr->noise_z)*U[8])
						* ((snr->noise_z)*U[8]);
				noise_total=sqrt(noise_total);
			}
			for(i=0;i<nwavelets;++i) noise[i][j]=noise_total;
		}
	}
	/* string of work spaces */
	window_length = win.length;
	allot(float *,weights,nsta);
	allot(double *,dweights,nsta);
	allot(double *,current_moveout,nsta);
	allot(complex *,centroid,nwavelets);
	allot(float *,work,nwavelets);
	allot(float *,r,nsta);
	allot(float *,phi,nsta);
	allot(float *,iqr,nsta);
	allot(float *,iqphi,nsta);
	allot(float *,dt,nsta);
	allot(float *,work2,nsta);
	allot(complex *,cwork,nsta);
	allot(double **,coh,nsta);
	for(j=0;j<nsta;++j) allot(double *,coh[j],window_length);
	allot(complex ***,z,nwavelets);
	for(i=0;i<nwavelets;++i) 
	{
		allot(complex **,z[i],nsta);
		for(j=0;j<nsta;++j) 
			allot(complex *,z[i][j],window_length);
	}
	allot(complex *,A,nsta*window_length);
	allot(double *,cohb,window_length);

	/* We work on a copy of moveout to allow resets */
	dcopy(nsta,moveout,1,current_moveout,1);
	keeptrying=1;
	do {
	    int iref;
	    double delta_time;  /* Relative time difference between timeref and
                                        first sample in the aligned gather.  Used as
                                        alignment point for time window */

	    deltat = trans_gath[0]->x3[0]->dt;
	    /* We compute moveout in samples with this little function.
		The result is a vector of ints parallel to the station list in
		the gather object.*/
	    if(lags != NULL) free(lags);
	    lags = compute_lag_in_samples(*trans_gath,current_moveout,
		timeref+((double)win.tstart)*win.si);

	    for(i=0,minimum_moveout=current_moveout[0];i<nsta;++i)
	    {
                if(fabs(current_moveout[i])<minimum_moveout)
                {
                        iref = i;
                        minimum_moveout = fabs(current_moveout[i]);
                }
		if(!strcmp(trans_gath[0]->sta[i]->sta,
			trans_gath[0]->sta[i]->refsta)) break;
	    }
	    if(i>=nsta)
		elog_notify(0,"WARNING(MWcompute_arrival_time):  reference\
 station ambiguity may cause a dc offset\nUsing station with \
minimum moveout (%s) in place of reference station %s as time \
reference.\n",
			trans_gath[0]->sta[iref]->sta,
			trans_gath[0]->sta[0]->refsta);
	    else
		iref=i;

/*
	    delta_time=trans_gath[0]->x3[iref]->starttime-timeref;
*/
	    delta_time=win.tstart*win.si;

	    for(i=0;i<nwavelets;++i)
	    {
		nsta_used = build_static_matrix(trans_gath[i],lags,&win,
					weights,A);
		for(j=0;j<nsta;++j) 
			ccopy(window_length,A+j,nsta,z[i][j],1);
			
	    }
	    for(j=0;j<nsta;++j) 
		dweights[j] = (double)weights[j];
	    /* This series of functions are designed to interaction with a 
	    changing parameter file created by a GUI with tcl/tk.  We first 
	    save the data from this gather that will be accessed by the GUI.
	    We then open the parameter file and extract parameters describing
	    the time window function to be applied to create the stack */
		MWsave_gather(trans_gath[0],
			z,nwavelets,nsta,window_length,delta_time,deltat);
/*RIGHT ABOUT HERE WE NEED TO ADD A SECTION THAT COMPUTES A COMPLEX 
CROSS-CORRELATION.  THIS FUNCTION COULD BE USEFUL TO AVOID CYCLE SKIP
PROBLEMS AND SHOULD BE PLOTTED ON THE SAME CURVE AS THE COHERENCE
PLOT*/
		if(iteration==0)
		{
			fflush(MWpout);
			fputs("NEWSTACK\n",MWpout);
			fflush(MWpout);
			/* contents of inbuf are ignored.  This just causes
			the program to block until told to continue 
			by the GUI */
			fprintf(stdout,"Top of stacking loop\nHit Accept button when ready\n");
			fgets(inbuf,10,MWpin);
		}
		if(pfread(guipf,&pf))
		   elog_die(0,"pfread error on %s\nCheck that GUI is running\n",
				guipf);
	
		kill_list = pfget_tbl(pf,"stations_to_kill");
		if(kill_list!=NULL)
		{
			int ikill;
			char *str;
			for(i=0;i<maxtbl(kill_list);++i)
			{
				str = gettbl(kill_list,i);
				sscanf(str,"%d",&ikill);
				if((ikill<0)||(ikill>=nsta))
					elog_complain(0,"Illegal station kill request: station number %d outside gather range of 0 to %d\n",
						ikill,nsta);
	
				else
					dweights[ikill]=0.0;
			}
	
		}
		freetbl(kill_list,free);
		ts0=pfget_double(pf,"stack_ts0");
		ts1=pfget_double(pf,"stack_ts1");
		te0=pfget_double(pf,"stack_te0");
		te1=pfget_double(pf,"stack_te1");
		timeweight=MWstack_set_trapezoidal_window(delta_time,
			deltat,window_length,ts0,ts1,te1,te0);
		pffree(pf);

	    /* Now call the stacking function.  Because it creates
		the MWstack object we have to avoid a memory leak first. */
	    if(stack!=NULL) destroy_MWstack(stack);
	    stack = MWrobuststack(z,nwavelets,nsta,window_length,
		delta_time,deltat,dweights,timeweight,noise);

	    /* This computes the coherence matrix and then saves it 
	    to a temporary file used by the GUI */
	    if(MWcoherence(stack,z,coh,cohb))
	    {
		/* We kill the program in this error condition for the 
		reason stated.  If the algorithm changes this should be
		changed.*/
		elog_die(0,"MWcoherence failed\nProgram access violation likely\n");
	    }
	    MWsave_coherence(trans_gath[0],coh,cohb,nsta,window_length,
			delta_time,deltat);

	    /* We need to release the timeweight vector now as it was
	    copied to a work space in the stack object */
	    free(timeweight);


	    /* we compute the center of the cloud of points in the complex
	    plane that define phase shifts and relative amplitudes for
	    each wavelet using an m-estimator.  This has to be done 
	    independently for each wavelet because we never could figure out
	    how to normalize the relative phase between wavelets. */ 
	    for(i=0;i<nwavelets;++i)
	    {
		ccopy(nsta,stack->amp[i],1,cwork,1);
		/* We have to remove null values defined by 0 weights */
		if(nsta != nsta_used) 
		{
			nsta_test = remove_null_complex(nsta,weights,1,A,1);
			if(nsta_test != nsta_used)
			{
				elog_complain(0,"Static matrix weight inconsistency:  expected %d nonzero weight stations but found %d\nReset to %d\n",
					nsta_used, nsta_test,nsta_test);
				nsta_used = nsta_test;
			}
		}
		/* This unnormalized estimator works of arrays, but could
		not be expected to work well for a "source array" type stack
		because the amplitude could vary by orders of magnitude */
		centroid[i] = M_estimator_complex(cwork,nsta_used);
	    }		

	    for(j=0;j<nsta;++j)
	    {
		/* Null answer for no data uses test against lags 
		array which sets lags value for problem rows negative 
		or a zero weight.  I use an unsafe test of weights
		against zero because I know it can't have been altered
		from 0.0 if it was set to zero  */
		if((lags[j]<0) || (weights[j] <= 0.0))
		{
			phi[j] = 0.0;
			r[j] = 0.0;
			iqphi[j] = 2.0*M_PI;
			iqr[j] = MAXFLOAT;
		}
		else
		{
			for(i=0;i<nwavelets;++i) 
				work[i] = unwrap_delta_phase(
					stack->amp[i][j],centroid[i]);
			phi[j] = M_estimator_float(work,nwavelets,
					IQ_SCALE_ABSOLUTE,MIN_PHI_SCALE);
			/* Now here I do assume work was not altered by the
			previous routine.  This is not always the correct
			way to do this, as one should compute the iqs from
			weighted residuals, but it won't matter unless the
			data are seriously messed up.*/
			stats = MW_calc_statistics_float(work,nwavelets);
			iqphi[j] = (stats.q3_4) - (stats.q1_4);
			/* Now we deal with amplitudes */
			for(i=0;i<nwavelets;++i)
			{				
				double dwork;
			 	dwork = hypot((double)(stack->amp[i][j].r),
					(double)(stack->amp[i][j].i));
				/* This converts to a relative amplitude
				by removing the modulus of the centroid
				for this wavelet.  (Amplitudes like phase
				are biased between wavelets)*/
				dwork /= hypot((double)centroid[i].r,
						(double)centroid[i].i);
				/* correct for weighting */
				dwork /= weights[j];
				work[i] = (float)log10(dwork);
			}
			/* IMPORTANT:  r and the computed interquartiles
			are left in log space here due to the log normal
			assumption.  The 0.01 sets the minimum relative
			scale at a somewhat arbitrary value that is
			fairly generous. */
			r[j] = M_estimator_float(work,nwavelets,
				IQ_SCALE_RELATIVE,0.01);
			stats = MW_calc_statistics_float(work,nwavelets);
			iqr[j] = (stats.q3_4) - (stats.q1_4);
		}
	    }
	    for(i=0;i<nsta;++i) 
	    {
		if(fabs((double)phi[i])>PHASE_ANGLE_LIMIT)
			phi[i] = (float)copysign(PHASE_ANGLE_LIMIT,
						(double)phi[i]);
		dt[i] = (float)phase_to_time((double)phi[i],deltat,
			trans_gath[0]->x3[0]->basis->f0);
	    }
	/* Because the dt is indeterminate, it is useful to 
	remove the median dt to keep things from jumping all over.
	Somewhat repetitious with how this is handled in the 
	slowness vector calculation, but better safe than sorry.
	The cost is low*/
	    scopy(nsta,dt,1,work2,1);
	    nsta_test = remove_null_float(nsta,weights,1,work2,1);
	    stats = MW_calc_statistics_float(work2,nsta_test);
	    for(i=0;i<nsta;++i) 
		if(weights[i]>0.0) 
		{
			dt[i] -= (stats.median);

			/* I somehow got the sign backwards in the 
			phase computation, so this has to be a subraction */
			if(lags[i]>=0) current_moveout[i] -= dt[i];
			
		}
	    /* We use the full dt here rather than work2 because we force
		zero weight stations to have zero dt */
	    dtmax = dt[isamax(nsta,dt,1)];
	    fprintf(stdout,"Stacking iteration %d:  maximum adjustment = %lf s\n",
			iteration,dtmax);
	    ++iteration;
	    if((fabs(dtmax) <= (DT_FRACTION_TERMINATE*deltat))
	      || (iteration>=DT_MAX_ITERATION))

	    {

		MWsave_gather(trans_gath[0],
			z,nwavelets,nsta,window_length,delta_time,deltat);
		fputs("REDRAW\n",MWpout);
		fflush(MWpout);

		if(iteration >= DT_MAX_ITERATION) 
			fprintf(stdout,"WARNING:  static shift computation did not converge in %d iterations.\nLarge errors in time estimates are likely\n",
				DT_MAX_ITERATION);
		else
			fprintf(stdout,"Static calculation converged in %d iterations\n",
						iteration);
		fprintf(stdout,"Push Accept button to continue\nTo reset to starting values and try again push Rerun\n");
		fgets(inbuf,10,MWpin);
		pfread(guipf,&pf);
		keeptrying=pfget_boolean(pf,"recompute");
		if(keeptrying)
		{
			iteration=0;
			reset_arrival_times=pfget_boolean(pf,
				"reset_arrival_times");
			if(reset_arrival_times)
			   dcopy(nsta,moveout,1,current_moveout,1);
		}
		/* We need to have the ability to abandon hope.  This
		switch does that */
		discard=pfget_boolean(pf,"discard_stack");
		pffree(pf);
	    }
	} while (keeptrying);

	/* This is kind of an odd construct and somewhat error prone
	so watch out.  The beam coherence vector is stored above in 
	a work vector cohb.  We copy it to the MWstack object here.
	The warning is that building a data object in pieces like
	this is always error prone.  We only need to copy since
	this vector is created as a component of the MWstack object.*/
	dcopy(window_length,cohb,1,stack->coherence,1);

	/* We have to compute and remove the mean value from the amplitude
	values.  We have irq values that measure uncertainty of amplitude
	at each station, but r at this point has a large dc offset that
	has to be removed.  We use the multiwavelet averages 
	and assume a log normal distribution and the central limit theorem.
	Maybe we should have used an m estimator here, but generally the
	degrees of freedom should be high enough that the median will be
	sufficient.  These calculations are skipped if the stack is to
	be discarded*/

	if(!discard)
	{
	scopy(nsta,r,1,work2,1);
	nsta_test = remove_null_float(nsta,weights,1,work2,1);
	stats = MW_calc_statistics_float(work2,nsta_test);
	*avgamp = stats.median;
	*amperr = ((stats.q3_4) - (stats.q1_4))*NORMAL_IQSCALE;
	*ampndgf = nsta_test - 1;
	
	/*here we load up the associative arrays that contain the results*/
	for(i=0;i<nsta;++i)
	{
		double *atime;
		MWstatic *mws;
		if((lags[i]>=0) && (weights[i]>=0.0))
		{
			allot(double *,atime,1);
			*atime = timeref + current_moveout[i];
			setarr(*arrival,trans_gath[0]->sta[i]->sta,atime);
			allot(MWstatic *,mws,1);
			mws->dt_final = dt[i];
			mws->t_raw = current_moveout[i] - moveout[i] 
				+ trans_gath[0]->sta[i]->residual_static;
			/* r is already in log space */
			mws->log10amp = r[i] - (*avgamp);
			/* We kept interquartiles as a phase angle so
			we need to convert it to a time */
			sigma_t = phase_to_time((double)iqphi[i],deltat,
				trans_gath[0]->x3[0]->basis->f0);
			mws->sigma_t =sigma_t*NORMAL_IQSCALE;
			mws->sigma_log10amp = iqr[i]*NORMAL_IQSCALE;
			mws->ndgf = nwavelets - 1;
			setarr(*results_array,trans_gath[0]->sta[i]->sta,mws);
		}
	}
	}
	/* release all these work spacees */

	free(current_moveout);
	free(lags);
	free(A);
	free(weights);
	free(centroid);
	free(work);
	free(work2);
	free(cwork);
	free(r);  free(phi);	free(iqr);	free(iqphi);
	free(dt);
	for(i=0;i<nwavelets;++i)
	{
		for(j=0;j<nsta;++j) free(z[i][j]);
		free(z[i]);
		free_MWgather(trans_gath[i]);
		free(noise[i]);
	}
	free(z);
	free(noise);
	free(cohb);
	for(j=0;j<nsta;++j) free(coh[j]);
	free(coh);
	if(discard)
	{
		destroy_MWstack(stack);
		return(NULL);
	}
	else
		return(stack);
}
