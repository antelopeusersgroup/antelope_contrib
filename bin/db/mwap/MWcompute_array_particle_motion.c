#include "multiwavelet.h"
/* for debug only */
/* some simple utility functions */
void copy_real(complex *z,float *r,int nz)
{
        int i;
        for(i=0;i<nz;++i) r[i]=z[i].r;
}

/*  Computes particle motion estimates from multiwavelet transformed
data using a suite of stacked traces as a reference.  This function
is a descendent of an earlier principal component method implementation.
This differs fundamentally in the algorithm.  The input stack is used
as a reference traces.  We find the correct stack for each wavelet and
compute complex phase/amplitudes wrt this waveform as a simple 
dot product.  (This assumes the stack is normalized or it may produce
incorrect results)  The particle motions are then computed from
the computed complex numbers using an analytic form for ellipsoids
in 3d.  
arguments:
	g - vector of gather structures, one for each wavelet.  
	nwavelets - number of wavelets = length of g
	s - robust stack of array to be used as reference trace for
		computing the pm estimates
	timeref - absolute time reference
	moveout - vector of length = number of stations in the gather 
		of moveout times relative to the reference time.  
	up - three vector (of doubles) used to defined positive 
		direction of particle motion ellipse.  For 
		P waves should be {0.0, 0.0, 1.0}   For S, 
		a more complex recipe is in order.
	pmarray - associate array of a set of average array estimates of
		particle motion ellipse.  (see below).
	errarray - parallel Arr to pmarray containing error estimates.
	pmsta - associative array of pointers to Particle_Motion_Ellipse
		structures keyed by station names.  
	errsta - comparable associative array to pmsta holding pointers
		to error estimates -- keyed by sta like pmsta.

Author:  Gary L. Pavlis
Written:  December 2001
Produced by hacking up the earlier principal component algorithm.  The
difference were surprisingly small.
January 5, 2002
Have concluded the particle motion analysis remains unstable. Since the cost
of computing pm estimates is tiny I elected to compute three different measures
of array particle motions:
bm - computed by correlation against the beam stack (initial algorithm) to produce
an effectively beam average phase angle estimates that are then converted to a 
particle motion ellipse.  
aa - array average.  That is we just compute a straight average of the individual
station estimates.   Well, not a simple average but the existing m estimator.
waa - weighted array average.  Instead of using an m estimator we take the station
weights computed from the robust stacking function and weight each vector in the 
sum by that amount.  

These are returned now in associative arrays pmarray and errarray keyed by
the above symbols.  The previous version returned a Particle_Motion_Ellipse and error
object in the same position.
*/
int MWcompute_array_particle_motion(MWgather **g,int nwavelets,MWstack *s,
	double timeref, double *moveout, double *up,
	Arr **pmarray, Arr **errarray,
	Arr **pmsta, Arr **errsta) 
{
	int nsta,nsta_used,nchan,nt,info;
	int i,j,ii,jj;
	int *lags;  /* base lag computed from moveout for each 
			station.  Stored in a parallel array to 
			moveout and elements of MWgather */
	Time_Window win;
	complex *A; 
	complex zx1,zx2,zx3; 
	complex *zsx1,*zsx2,*zsx3;  /* hold stack of three component in cardinal directions */
	Particle_Motion_Ellipse *pmstack;  /* particle motion vectors of stack (nwavelets length)*/
	Particle_Motion_Ellipse *ematrix;  /* this is set to be a 
				matrix of particle motion ellipse structures.
				These are indexed like the fortran type
				arrays A, eigenvectors, etc.  That is, 
				they really only have one subscript, but
				these index like fortran arrays to 
				yield pointers to particle motion structs */
	Particle_Motion_Ellipse *avgpm; 
	Particle_Motion_Error *avgerr;
	Particle_Motion_Ellipse *pmwork;  /* work space of pm pointers*/
	Particle_Motion_Ellipse *pmwaa;
	Particle_Motion_Error *errwaa;
	char **sta;  /* vector of character strings parallel to ematrix.  
			array.  NULL pointers define empty rows of ematrix */
	float *rweights;
	double sum_weights;
float rdebug[2000];
complex cdebug[2000];

	if(g[0]->ncomponents<3)
	{
		elog_complain(0,"Number of components in working gather is %d\nMust be 3 for particle motion analysis to be computed\n",
			g[0]->ncomponents);
		return(-1);
	}
	/* This is probably not necessary, but better safe than sorry */
	if((*pmsta)!= NULL) freearr(*pmsta,free);
	*pmsta = newarr(0);
	if((*errsta)!=NULL) freearr(*errsta,free);
	*errsta = newarr(0);
	if((*pmarray)!= NULL) freearr(*pmarray,free);
	*pmarray = newarr(0);
	if((*errarray)!=NULL) freearr(*errarray,free);
	*errarray = newarr(0);

	nsta = g[0]->nsta;
	nchan = 3*nsta;
	/* For this analysis we use the beams for each wavelet computed from
	a stack aligned to the peak particle motion direction.  All phase
	angles are measured relative to these waveforms by computing complex
	dot products.  This should be more stable than than conventional pc methods
        because array stack should provide a more accurate average waveform
        for comparing the phase of different components.  This function 
	assumes the stack passed into the function has been windowed to the
	desired range AND normalized to have unit L2 norm.   Thus all
	we do here is build a Time_Window structure consistent with the 
	stack passed to this function.*/
	win.tstart = nint((s->tstart)/(s->dt));
	win.tend = nint((s->tend)/(s->dt));
	win.length = win.tend - win.tstart + 1;
	win.increment=1;
	win.stepsize=1;
	nt = win.length;

	allot(complex *,A,nchan*nt);
	allot(float *,rweights,nchan);
	allot(Particle_Motion_Ellipse *,ematrix,nsta*nwavelets); 
	allot(Particle_Motion_Ellipse *,pmwork,nwavelets*nsta); 
	allot(Particle_Motion_Ellipse *,pmstack,nwavelets); 
	allot(Particle_Motion_Ellipse *,pmwaa,1);
	allot(Particle_Motion_Error *,errwaa,1);
	allot(char **,sta,nsta); 
	allot(complex *,zsx1,nt);
	allot(complex *,zsx2,nt);
	allot(complex *,zsx3,nt);
	for(i=0;i<nsta;++i) sta[i] = NULL;
	/* compute_lag_in_samples allocates lags,  This is bad form, but
	convenient for this code */
	lags = compute_lag_in_samples(*g,moveout,
		timeref+(win.tstart)*(s->dt) );

	for(i=0;i<nwavelets;++i)
	{
		/* this function isn't ideal anymore as it is recycled from the
		earlier program, but it is known to work */
		nsta_used = build_3c_matrix(g[i],lags,&win,rweights,A,sta);
		if(nsta_used != nsta) 
			elog_log(0,"compute_mw_particle_motion:  Data problems, computing particle motions for only %d of %d stations\n",
				nsta_used,nsta);

		MWapply_time_window(A,nchan,nchan,nt,s->timeweight);
		/* the windowed traces are stored in rows mod 3.  We need to accumulate
		the stacked traces in cardinal directions and that is done here.*/
		for(j=0;j<nt;++j)
		{
			zsx1[j].r=0.0;
			zsx2[j].r=0.0;
			zsx3[j].r=0.0;
			zsx1[j].i=0.0;
			zsx2[j].i=0.0;
			zsx3[j].i=0.0;
		}
		for(j=0,jj=0;j<nsta;++j,jj+=3)
		{
			complex cwork;
			/* This applies the residual weights computed by MWrobuststack.
			We don't worry about normalizing the stack below because for particle
			motion of the stack we only need to preserve relative amplitudes between
			components of each station */
			cwork.r = s->weights[j];
			cwork.i = 0.0;
			if(rweights[j]>=0.0)
			{
ccopy(nt,A+jj,nchan,cdebug,1);
copy_real(cdebug,rdebug,nt);
ccopy(nt,A+jj+1,nchan,cdebug,1);
copy_real(cdebug,rdebug,nt);
ccopy(nt,A+jj+2,nchan,cdebug,1);
copy_real(cdebug,rdebug,nt);

#ifdef SUNPERF6
				caxpy(nt,cwork,A+jj,nchan,zsx1,1);
				caxpy(nt,cwork,A+jj+1,nchan,zsx2,1);
				caxpy(nt,cwork,A+jj+2,nchan,zsx3,1);
#else
				caxpy(nt,&cwork,A+jj,nchan,zsx1,1);
				caxpy(nt,&cwork,A+jj+1,nchan,zsx2,1);
				caxpy(nt,&cwork,A+jj+2,nchan,zsx3,1);
#endif
			}
		}
		/* compute the particle motion of the stack */
		zx1 = cdotc(nt,zsx1,1,s->z[i],1);
		zx2 = cdotc(nt,zsx2,1,s->z[i],1);
		zx3 = cdotc(nt,zsx3,1,s->z[i],1);
copy_real(s->z[i],rdebug,nt);
copy_real(zsx1,rdebug,nt);
copy_real(zsx2,rdebug,nt);
copy_real(zsx3,rdebug,nt);
		pmstack[i] = compute_particle_motion(zx1,zx2,zx3,up);

		/* Now compute the individual station particle motions */
		for(j=0,jj=0;j<nsta;++j,jj+=3)
		{
			ii = j + i*nsta;
			if((sta[j] == NULL) || (rweights[jj]<0.001) )
			{
				set_pm_null(ematrix+ii);
			}
			else
			{
				zx1 = cdotc(nt,A+jj,nchan,s->z[i],1);
				zx2 = cdotc(nt,A+jj+1,nchan,s->z[i],1);
				zx3 = cdotc(nt,A+jj+2,nchan,s->z[i],1);
				ematrix[ii] = compute_particle_motion(zx1,
						zx2,zx3,up);
			}
		}
	}
	/* Now we produce particle motion estimates for each station
	computed by averaging multiwavelet estimates of ellipses for
	each station.  This loop also accumulates the weighted array
	average estimate for the full array */
	for(j=0;j<3;++j)
	{
		pmwaa->major[j]=0.0;
		pmwaa->minor[j]=0.0;
	}
	pmwaa->rectilinearity=0.0;
	for(j=0;j<nsta;++j)
	{
		Particle_Motion_Ellipse *pm;
		Particle_Motion_Error *pme;
		double total_weight;
		if((sta[j]!=NULL) && (rweights[3*j]>0.0))
		{
			pmvector_copy(nwavelets,ematrix+j,nsta,pmwork,1);
			allot(Particle_Motion_Ellipse *,pm,1);
			allot(Particle_Motion_Error *,pme,1);
			pmvector_average(pmwork,nwavelets,pm,pme);
			setarr(*pmsta,sta[j],pm);
			setarr(*errsta,sta[j],pme);
			total_weight = s->weights[j]*((double)rweights[3*j]);
			daxpy(3,total_weight,pm->major,1,pmwaa->major,1);
			daxpy(3,total_weight,pm->minor,1,pmwaa->minor,1);
			pmwaa->rectilinearity += total_weight*pm->rectilinearity;
			sum_weights += total_weight;
			
		}
	}
	/* now we compute the array beam average by averaging the individual
	wavelet estimates */
	allot(Particle_Motion_Ellipse *,avgpm,1);
	allot(Particle_Motion_Error *,avgerr,1);
	pmvector_average(pmstack,nwavelets,avgpm,avgerr);
	setarr(*pmarray,PMOTION_BEAM,avgpm);
	setarr(*errarray,PMOTION_BEAM,avgerr);

	/* Next we compute an m-estimator from the entire ensemble of single 
	station estimates */
	for(j=0,ii=0;j<nsta;++j)
	{
		if((sta[j]!=NULL) && (rweights[3*j]>0.0))
		{
			pmvector_copy(nwavelets,ematrix+j,nsta,pmwork+ii,1);
			ii += nwavelets;
		}
	}
	allot(Particle_Motion_Ellipse *,avgpm,1);
	allot(Particle_Motion_Error *,avgerr,1);
	pmvector_average(pmwork,ii,avgpm,avgerr);
	setarr(*pmarray,PMOTION_AA,avgpm);
	setarr(*errarray,PMOTION_AA,avgerr);

	/* now we compute the weighted array average estimate.  Major and minor
	axes are computed by a simple normalization to unit length while 
	rectilinearity requires a normalization by sum_weights.  */
	dscal(3,1.0/dnrm2(3,pmwaa->major,1),pmwaa->major,1);
	dscal(3,1.0/dnrm2(3,pmwaa->minor,1),pmwaa->minor,1);
	pmwaa->rectilinearity /= sum_weights;
	/* We simply copy the aa error estimates assuming they will not be very 
	different from this.  This is done somewhat in laziness because otherwise
	I'd have to code up an equivalent to pmvector_average (m-estimator
	version) for this different case.  It is quite different and I don't
	think it is worth the trouble as I can't conceive how they could 
	differ very dramatically.*/
	errwaa->dtheta_major=avgerr->dtheta_major;
	errwaa->dphi_major=avgerr->dphi_major;
	errwaa->dtheta_minor=avgerr->dtheta_minor;
	errwaa->dphi_major=avgerr->dphi_major;
	errwaa->delta_rect=avgerr->delta_rect;
	errwaa->ndgf_major=avgerr->ndgf_major;
	errwaa->ndgf_minor=avgerr->ndgf_minor;
	errwaa->ndgf_rect=avgerr->ndgf_rect;
	setarr(*pmarray,PMOTION_WAA,pmwaa);
	setarr(*errarray,PMOTION_WAA,errwaa);

	for(j=0;j<nsta;++j) if(sta[j] != NULL) free(sta[j]);
	free(sta);
	free(A);
	free(lags);
	free(ematrix);
	free(pmwork);
	free(rweights);
	return(0);
}
