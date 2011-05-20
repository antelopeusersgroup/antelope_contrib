#include <stdio.h>
#include <math.h>
#include <malloc.h>
#include <float.h>
#include "multiwavelet.h"
#define MAXDOUBLE DBL_MAX

/* Usage:

double dhuber(double x);

Returns Huber weight function for scaled residual x = r/d where
d is the scale factor (i.e. this routine assumes data are prescaled)

Author:  Gary Pavlis
Written:  June 1992, converted to dhuber Jan 2000
Reference:  Chave and Thompson (1989) JGR, 94, p. 14217.
History:  Descendent  of huber.c in libgenloc.  Sun's compiler
seems to botch the original float return for reasons I couldn't
quite understand.  Converted to full double to avoid mixed precisions.
*/
#define HUBER_PARAMETER_A 1.5 /* Parameter "a" in Huber weight formula */
double dhuber(double x)
{
	if(fabs(x)<=HUBER_PARAMETER_A) return(1.0);
	else 
		return(HUBER_PARAMETER_A/fabs(x));
}
/* Similar problem with huber required this variant of routine in
libgenloc */

/* Usage:

double dthomson(double x,double beta);

Returns weight for Thomson's redescending weight function 
given in equation (27) of Chave et al., 1987.
Assume x is a normalized sample value (i.e. for spectral values
x is the sample amplitude divided by the error scale factor 
calculated as described by Chave et al..
This weight function is controlled by the factor "beta" passed
as argument 2.  Note:  Chave et al. recommend setting beta to
the Nth quantile of the a Rayleigh distribution.  This is assumed
calculated externally for efficiency reasons.

Author:  Gary Pavlis
Written:  June 1992
Reference:  Chave, Thomson, and Ander (1987). JGR, 92, 633-648. 
*/
double dthomson(double x,double beta)
{
	double temp; /* used to make sure all calculations are done
			in double precision */
	temp = fabs (x);
	temp = exp( ( beta)*(temp -  beta));
	return(  exp( - temp));
	
}
	

/* Sort compare function for qsort function */
static int 
compare_float(const void *v1,const void *v2)
{
	float *x1,*x2;
	x1 = (float *)v1;
	x2 = (float *)v2;
        if(*x1<*x2)return (-1);
        else if(*x1==*x2) return (0);
        else return(1);
}
static int 
compare_double(const void *v1,const void *v2)
{
	double *x1,*x2;
	x1 = (double *)v1;
	x2 = (double *)v2;
        if(*x1<*x2)return (-1);
        else if(*x1==*x2) return (0);
        else return(1);
}


/*  MW_calc_statistics_float calculate robust statistics for float vector y.
i.e. median and quartiles.  
  Inputs:  
	y - input vector of length ny for which statistics are to 
		be calculated
Returns mean, median, upper, and lower quartiles and range 
in MW_scalar_statistics structure. 

Algorithm calculates median, and quartiles by sorting input y array. 
IMPORTANT:  THIS PROCESS IS DESTRUCTIVE, AND THE Y ARRAY IS REORDERED 
ON RETURN SINCE THE ROUTINE RECEIVE A POINTER TO y.

If ny < 2 returns with error signaled by setting elements of the 
returned structure to MAXDOUBLE (defined in values.h).  If ny = 2 or 3
the interquartiles are determined from the full range and not error is
signaled.  

Author:  Gary L. Pavlis, Indiana University
Written:  1994, Minor modified Feb. 1995 for location code.  Original 
included max and min of y, which were unnecessary here.  Also 
changed definition to ansi C.

Modified:  recycled for multiwavelet code, but this time I put back
the code to keep high and low values and added mean value calculation
*/
MW_scalar_statistics MW_calc_statistics_float(float *y,int ny)
{
	/* temporaries */
	float mean,median, low_quartile, high_quartile;
	MW_scalar_statistics stats;
	int i;
	
	/* We can't do anything if ny < 2 so return an error in this case */
	if(ny < 2) 
	{
		elog_log(0,"calculate_statistics:  Insufficient data to calculate meaningful statistics\nReceived only %d data to process\n",ny);
		stats.median = MAXDOUBLE;
		stats.q1_4 = MAXDOUBLE;
		stats.q3_4 = MAXDOUBLE;
		stats.high = MAXDOUBLE;
		stats.low = MAXDOUBLE;
		stats.mean = MAXDOUBLE;
		return(stats);
	}

	/* First we compute the simple mean */
	for(i=0,mean=0.0;i<ny;++i) mean += y[i];
	stats.mean = ((double)mean)/((double)ny);
	/* sort y using the qsort function */
	qsort((char *)y,ny,sizeof(float),compare_float);


	stats.low = y[0];
	stats.high = y[ny-1];
	/* These are done exactly using formulas appropriate for small samples*/
	if(ny%2)  /* this is case for odd number */
		median = *(y + ny/2);
	else
		median = (y[ny/2 - 1]+y[ny/2])/2.0;

	stats.median = (double) median;
	/* handle the case when ny < 4 specially to prevent seg faults */
	if(ny<4)
	{
		stats.q1_4 = y[0];
		stats.q3_4 = y[ny-1];
	}
	else
	{
		switch((ny-1)%4)
		{
		case(0):
			low_quartile = y[(ny-1)/4];
			high_quartile = y[3*(ny-1)/4];
			break;
		case(1):
			low_quartile = (0.75*y[(ny-1)/4]+0.25*y[((ny-1)/4)+1]);
			high_quartile = (0.75*y[3*(ny-1)/4]+0.25*y[(3*(ny-1)/4)+1]);
			break;
		case(2):
			low_quartile = (0.5*y[(ny-1)/4]+0.5*y[((ny-1)/4)+1]);
			high_quartile = (0.5*y[3*(ny-1)/4]+0.5*y[(3*(ny-1)/4)+1]);
			break;
		case(3):
			low_quartile = (0.25*y[(ny-1)/4]+0.75*y[((ny-1)/4)+1]);
			high_quartile = (0.25*y[3*(ny-1)/4]+0.75*y[(3*(ny-1)/4)+1]);
			break;
	
		}
		stats.q1_4 = (double) low_quartile;
		stats.q3_4 = (double) high_quartile;
	}
	return(stats);
}
/*exactly the same function for doubles */
MW_scalar_statistics MW_calc_statistics_double(double *y,int ny)
{
	/* temporaries */
	double mean,median, low_quartile, high_quartile;
	MW_scalar_statistics stats;
	int i;
	
	/* We can't do anything if ny < 2 so return an error in this case */
	if(ny < 2) 
	{
		elog_log(0,"calculate_statistics:  Insufficient data to calculate meaningful statistics\nReceived only %d data to process\n",ny);
		stats.median = MAXDOUBLE;
		stats.q1_4 = MAXDOUBLE;
		stats.q3_4 = MAXDOUBLE;
		stats.high = MAXDOUBLE;
		stats.low = MAXDOUBLE;
		stats.mean = MAXDOUBLE;
		return(stats);
	}

	/* First we compute the simple mean */
	for(i=0,mean=0.0;i<ny;++i) mean += y[i];
	stats.mean = mean/((double)ny);
	/* sort y using the qsort function */
	qsort((char *)y,ny,sizeof(double),compare_double);


	stats.low = y[0];
	stats.high = y[ny-1];
	/* These are done exactly using formulas appropriate for small samples*/
	if(ny%2)  /* this is case for odd number */
		median = *(y + ny/2);
	else
		median = (y[ny/2 - 1]+y[ny/2])/2.0;

	stats.median = median;
	/* handle the case when ny < 4 specially to prevent seg faults */
	if(ny<4)
	{
		stats.q1_4 = y[0];
		stats.q3_4 = y[ny-1];
	}
	else
	{
		switch((ny-1)%4)
		{
		case(0):
			low_quartile = y[(ny-1)/4];
			high_quartile = y[3*(ny-1)/4];
			break;
		case(1):
			low_quartile = (0.75*y[(ny-1)/4]+0.25*y[((ny-1)/4)+1]);
			high_quartile = (0.75*y[3*(ny-1)/4]+0.25*y[(3*(ny-1)/4)+1]);
			break;
		case(2):
			low_quartile = (0.5*y[(ny-1)/4]+0.5*y[((ny-1)/4)+1]);
			high_quartile = (0.5*y[3*(ny-1)/4]+0.5*y[(3*(ny-1)/4)+1]);
			break;
		case(3):
			low_quartile = (0.25*y[(ny-1)/4]+0.75*y[((ny-1)/4)+1]);
			high_quartile = (0.25*y[3*(ny-1)/4]+0.75*y[(3*(ny-1)/4)+1]);
			break;
	
		}
		stats.q1_4 = low_quartile;
		stats.q3_4 = high_quartile;
	}
	return(stats);
}
/* This function pair of functions are simple M-estimators used
in mwap.  The first function works on scalar vectors and the second
computes an m-estimate of center for complex vectors by applying
the scalar function to real and imaginary parts separately.  
This code is not nearly as general as it would be if I were trying
to write the ultimate M-estimator code.  It has three things
frozen in that are not always optimal:
1.  The scalar code assumes the inputs are normal deviates
2.  We always use the Huber formula.
3.  The initial estimate is always based on the median.  
Treating the real and imag parts this way is not always
the right approach.  We found it worked well in the multiwavelet
analysis because the emphasis is on phase measurement, not amplitudes.
Literature on power spectral estimates, for example, argues that
the penalty should key on only the amplitude and ignore the phase.  
Anyway, it's what we do here right or wrong.

arguments:

x-float vector of length x to compute m-estimator based mean from.  
	x is copied and not altered.
mode == IQ_SCALE_RELATIVE will compute minimum scale factor allowed
	as initial median value*minscale.
	If mode is anything else, the value will be used verbatim
	as the minimum scale factor
minscale = minimum scale factor.  M-estimators can downward spiral
	throwing out more and more data till nothing remains if
	this is not used (note mode above connection).  

Author:  G Pavlis following L Bear
Written:  Sept. 1999
Problems:  jan 2000 Probable incompatibility between compiler
revs forced me to grab the huber.c function from libgenloc
and produce a dhuber which was all double.  The original 
seemed to get botched returning a float.  This adds some
nasty mixed precision arithmetic, but it is a workaround.
Probably should clean this up later.

Revision:  January 2000
Changed M_estimator_float to be somewhat more general.  Added the stuff
written above about scale factors.  
*/
#define CONVERGE_FACTOR 0.0001  /* converge when delta mean is this fraction
					of mean value */
#define MAX_ITERATIONS 50

float M_estimator_float(float *x,int nx,int mode,double minscale)
{
	float *work,*wtres;
	float fmean,dfmean,sumwt=0.0,sumwres=0.0;
	double weight;
	double test;
	float scale;  /* scale factor computed from interquartiles */
	int i;
	int iterations = 0;
	MW_scalar_statistics stats;
	float fminsc;  /* used to avoid some mixed precision */

	allot(float *,work,nx);
	allot(float *,wtres,nx);
	scopy(nx,x,1,work,1);
	scopy(nx,work,1,wtres,1);
	stats=MW_calc_statistics_float(wtres,nx);
	fmean = stats.median;
	if(IQ_SCALE_RELATIVE)
		fminsc = fmean*((float)minscale);
	else
		fminsc = (float)minscale;
	/* We first run a few cycles with the huber formula */
	do{
		for(i=0;i<nx;++i) wtres[i] = work[i]-fmean;
		stats = MW_calc_statistics_float(wtres,nx);
		scale = NORMAL_IQSCALE*((stats.q3_4)-(stats.q1_4));
		if(scale < fminsc) scale = fminsc;
		for(i=0,sumwt=0.0,sumwres=0.0;i<nx;++i) 
		{
			wtres[i] = work[i]-fmean;
			weight = dhuber((double)(wtres[i]/scale));
			wtres[i] *= weight;
			sumwt += weight;
			sumwres += wtres[i];
		}
		dfmean = sumwres/sumwt;
		test = ((double)dfmean)/((double)fmean);
		fmean += dfmean;
		sumwt = 0.0;
		sumwres = 0.0;
		++iterations;
	} while( (fabs(test)>CONVERGE_FACTOR) 
			&& (iterations<MAX_ITERATIONS));  
	if(iterations >= MAX_ITERATIONS) elog_notify(0,"Warning:  convergence failure in M_estimator_float");
	free(work);
	free(wtres);
	return(fmean);
}	
#define MIN_SCALE_FRACTION  0.001 /*M_estimator complex works on 
			real and imag parts seperately and has 
			a variable scale factor minimum used to
			prevent downward spirals.  We freeze this
			here to be relative to the initial vector size */
complex M_estimator_complex(complex *z,int nz)
{
	complex mean;
	float *work;
	int i;

	allot(float *,work,nz);
	/*Work on real and imag parts separately with same algorithm.*/
	for(i=0;i<nz;++i) work[i] = z[i].r;
	mean.r = M_estimator_float(work,nz,IQ_SCALE_RELATIVE,MIN_SCALE_FRACTION);
	for(i=0;i<nz;++i) work[i] = z[i].i;
	mean.i = M_estimator_float(work,nz,
		IQ_SCALE_RELATIVE,MIN_SCALE_FRACTION);

	free(work);
	return(mean);
}
/* This pair of functions are compansions to the n-vector m-estimator
below.  The first does the repetitious task of computing a matrix
of residuals and the second applies these residuals to compute a
robust estimate of scale.  
*/

/* This function computes residauls from an n by nv matrix v 
by subracting the n vector mean from each column of the input
matrix and writing the results in the residual matrix.  
Matrices are stored in fortran order as a single vector
and no checking is made for dimensions being valid.  
*/
void compute_nvector_residuals(double *v, int n, int nv,
				double *residuals, double *mean)
{
	int i, j;

	for(j=0;j<nv;++j)
	{
		for(i=0;i<n;++i)
		{
			residuals[i+j*n] = v[i+j*n] - mean[i];
		}
	}
}
/* This function computes a robust estimate of scale from 
an n by nv matrix of residuals by computing the l2 norm
of each of the nv vectors and computing the interquartile 
distances of the resulting set of nv numbers. 
The scaling assumes a Rayleigh distribution as discussed
in Chave and Thomson's paper for transfer function estimates.
This is probably wrong form n>2, but for the intended use
of this function in mwap it should be an adequate approximation.

nrms is a vector of doubles of length nv used as a work space
to avoid repeated alloc/frees by this function below.
On exit nrms contains a sorted vector of l2 norms of each of the
nv vectors (Warning:  the sorting is only done on nrms, and
these numbers are not in sequence with the residual matrix 
given as input)
*/


/*Theoretical interquartile distance for Rayleigh distribution is
sqrt(2*ln(4)) - sqrt((2*ln4)/3) (Chave et al., 1987, p. 643) */

#define SIGMA_IQ 0.70375579

double compute_nvector_scale(double *residuals, int n, int nv,
				double *nrms)
{ 
	int j;
	MW_scalar_statistics stats;
	double scale;

	for(j=0;j<nv;++j) nrms[j] = dnrm2(n,residuals+j*n,1);
	stats = MW_calc_statistics_double(nrms,nv);
	scale = stats.q3_4 - stats.q1_4;
	scale /= SIGMA_IQ;
	return(scale);
}

	

/* This function uses m-estimators to estimate a three vector from 
a cloud of points in n-space.  The approach is comparable to 
computing the center of mass of cloud of equal mass particles.
We use a component median for the initial estimate of center, then compute
the m-estimator based on a Rayleigh distribution keyed on the 
l2 norm amplitude of the vector residual.  This is comparable to the 
estimation of phase in robust estimates of transfer functions
described by Chave and Thomson as we use a penalty function based on
a Rayleigh distribution.  It is not clear to the author if this is
valid for other than 2 vectors, but it surely is not a bad approximation
for 3-vectors.  Higher order spaces should use this with care. 

Arguments:

v -  n by nv matrix containing ensemble of data n-vectors.  These are assumed
	stored as in the blas in a pseudofortran sense as a continuous
	vector of floats with columns sequential (i.e. first column
	of v is elements v[0], v[1], ... , v[n-1] and second column
	starts at v[n].
n - length of vectors (number of rows in v)
nv - number of vectors (number of columns in v)
mode - switch (see below)
minscale - minimum error scale allowed.  Use depends on setting of 
	the mode variable with which it is associated.  Scale factor
	is determined here from interquartiles.  mode is as defined
	in M_estimator_float above using symbols SCALE_IQ_RELATIVE
	and SCALE_IQ_ABSOLUTE.  The former uses a relative scale derived
	from the scale factor determined on the first pass.  That is, 
	the minimum scale alllowed is vmag0*minscale (e.g. 0.01 would
	limit the minimum scale to 1% of the magnitude of the total n
	vector initial estimate.  Absolute scaling
	uses the limit straight up.    
mean - vector of length n to hold result.  Blindly assumed to be already
	allocated of length n.
weight - vector of length to hold final weights used in robust
	estimation.

Function returns a 3 vector it estimates from v.  This is an array
alloced in this function that needs to be managed externally.  

Author:  G Pavlis
Written:  January 2000
*/
#define EPSILON 0.01 /* convergence parameter*/
#define HUBER_LIMIT 2 /* Number of Huber weight iterations */
#define THOMPSON_LIMIT 25 /* Limit on number of iterations using Thompson formula */
#define MIN_DGF 10  /* I've seen the Thompson formula work badly with 
		small degrees of freedom.  When dgf are less than 
		this the Thomson section is skipped */
void M_estimator_double_n_vector(double *v, 
	int n, 
	int nv,
	int mode, 
	double minscale,
	double *mean,
	double *weight)
{
	double *col, *row;  /* work spaces for columns and rows respectively*/
	double sum_weights;
	double *residuals;
	double *delta_mean;  
	int i, j, iteration;
	double vmag,dvmag; 
	double scale, fminsc;
	int ndgf;
	MW_scalar_statistics stats;
	double beta;

	allot(double *,col,n);
	allot(double *,row,nv);
	allot(double *,delta_mean, n);
	allot(double *,residuals,n*nv);

	/* We compute component medians to obtain initial estimate 
	of vector */
	for(i=0;i<n;++i)
	{
		dcopy(nv,v+i,n,row,1);
		stats=MW_calc_statistics_double(row,nv);
		mean[i] = stats.median;
	}
	/* We first do a few passes with the huber formula
	which is less aggressive on outliers, but helps 
	establish a solid value for the scale factor.*/
	iteration =0;
	vmag = dnrm2(n,mean,1);
	for(j=0;j<nv;++j) weight[j] = 1.0; /* done to make sure scale
					is computed correctly on first pass*/
	if(mode==IQ_SCALE_RELATIVE)
		fminsc = vmag*minscale;
	else
		fminsc = minscale;
	do
	{
		compute_nvector_residuals(v,n,nv,residuals,mean);
		/* This produces weighted residuals -- requires
		weighting formula to use weights 0<w<1 */
		for(j=0;j<nv;++j) dscal(n,weight[i],residuals+j,1);
		scale = compute_nvector_scale(residuals,n,nv,row);
		if(scale < fminsc) scale = fminsc;
		for(i=0;i<n;++i) delta_mean[i] = 0.0;
		for(j=0;j<nv;++j)
		{
			dcopy(n,residuals+j*n,1,col,1);
			dvmag = dnrm2(n,col,1);
			weight[j] = dhuber(dvmag/scale);
			daxpy(n,weight[j],col,1,delta_mean,1);
			sum_weights += weight[j];
		}
		dscal(n,1.0/sum_weights,delta_mean,1);
		dvmag = dnrm2(n,delta_mean,1);
		for(i=0;i<n;++i) mean[i] += delta_mean[i];
		++iteration;
	}while( ((dvmag/scale) > EPSILON)
		&& (iteration < HUBER_LIMIT) );
	
	/* Now we use Thomson's redescending formula which is the 
	opposite of the huber formula being extremely aggressive 
	on outliers and works only if the scale factor is not 
	too out of line.  It also works badly with low degrees
	of freedom.  Consequently, we return immediately when
	degrees of freedom are below a frozen threshold*/

	ndgf = nv - n;
	if(ndgf<MIN_DGF)
	{
		free(col);
		free(row);
		free(delta_mean);
		free(delta_mean);
		return;
	}
	/* This is the value of beta recommended by chave and thomson, 1987,
	based on the nvth quantile of the Rayleigh distribution.  I use
	number of degrees of freedom here instead to perhaps more properly
	work with higher order spaces, but use a minimum on ndgf to 
	avoid making the formula unstable.  The thomson formula becomes
	exponential-like with low degrees of freedom, which we need to 
	avoid. */
	beta = sqrt(2.0*log(2.0*((double)ndgf)));
	iteration = 0;
	do
	{
		compute_nvector_residuals(v,n,nv,residuals,mean);
		for(i=0;i<n;++i) delta_mean[i] = 0.0;
		for(j=0;j<nv;++j)
		{
			dcopy(n,residuals+j*n,1,col,1);
			dvmag = dnrm2(n,col,1);
			weight[j] = dthomson(dvmag/scale,beta);
			daxpy(n,weight[j],col,1,delta_mean,1);
			sum_weights += weight[j];
		}
		dscal(n,1.0/sum_weights,delta_mean,1);
		dvmag = dnrm2(n,delta_mean,1);
		for(i=0;i<n;++i) mean[i] += delta_mean[i];
		++iteration;
	}while( ((dvmag/scale) > EPSILON)
		&& (iteration < THOMPSON_LIMIT) );

	free(col);
	free(row);
	free(delta_mean);
	free(delta_mean);
	free(residuals);
}
/* Delete one jackknife error estimator.  Uses simple formula 
of equation (4) of the following reference:

Efron and Gong, 1983, American Statistician, 37, p. 36.

Arguments:
	n - length of vector x to compute jackknife error
	x - vector of random variables to compute knife error
		estimate from.


Author:  Gary Pavlis
*/

double d1_jack_err(int n, double *x)
{
	double *pval;
	double xbardot;
	double jkerror;
	double mean;
	int i,j;
	double resid;

	allot(double *,pval,n);
	/* compute the delete 1 estimates of the mean */
	for(i=0,mean=0.0;i<n;++i)
	{
		for(j=0,pval[i]=0.0;j<n;++j)if(i!=j) pval[i]+=x[i];
		pval[i]/=((double)(n-1));
		mean += pval[i];
	}
	/* This is the average delete 1 mean */
	mean /= ((double)n);

	for(i=0,jkerror=0.0;i<n;++i)
	{
		resid = pval[i] - mean;
		jkerror += resid*resid;
	}
	jkerror *= ((double)(n-1))/((double)n);
	free(pval);
	return(sqrt(jkerror));
}
