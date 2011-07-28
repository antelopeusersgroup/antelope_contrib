#include "stock.h"
#include "arrays.h"
#include "location.h"
#include "coords.h"
#include "pf.h"

#define SUNPERF 1
#include "perf.h"

extern int      GenlocVerbose;
#undef register_error
#define register_error  if ( GenlocVerbose ) elog_notify

int 
difference_is_zero (float x, float y)
{
    double          test;
    test = (double) x - (double) y;
    if (fabs (test) <= FLT_EPSILON)
	return (1);
    return (0);
}

#define INTERQUARTILE_SDEV_FACTOR 1.349
/* This routine forms the set of weighted, linearized equations that
are solved in the locate function.  It is called only by ggnloc below.

Arguments:

mode - sets what is calculate.
	mode == ALL (defined above) calculates both matrix of partials
		and vector of residuals.
	mode == RESIDUALS_ONLY - calculates only the vector of residuals
		and returns bypassing about half this code.  In this situation
		it is assumed all one really wants it the rms of the raw
		residual.  This is used in Marquardt's method to test
		whether or not damp should be altered.
	else -  same as RESIDUALS_ONLY

current_location - current hypocenter location estimates used to form
	equations

attbl - Tbl structure stack of arrival time data.  tbl contains pointers
	to Arrival structure list.
uttbl - parallel tbl for slowness data.  tbl here contains pointers to
	Slowness_vector structures.
options - control parameter structure.  (see code to find what is used)
A - returned matrix of partial derivatives (weighted)
b - returned vector of weighted residuals
r - vector of raw residuals
w - vector of weights excluding residual weight
reswt - vector of residual weights

The total weight for each data point is given as the product w[i]*reswt[i].
Optionally slowness base weights can be altered through the option
parameter options.slowness_weight_scale_factor.  The function returns
a vector of error statistics.

Author:  Gary L. Pavlis, Indiana University
Written:  February 1996
Revised:  November 1996
Added trap for travel time or slowness failing.  (signaled through
an invalid flag)  When this happens, the w array value is set to
zero.  Zero weights set this way are used as a signal that these
data should be ignored.
*/
Robust_statistics 
form_equations (int mode, Hypocenter current_location,
		Tbl * attbl, Tbl * utbl,
		Location_options options,
	  float **A, float *b, float *r, float *w, float *reswt, int *nused)
{
    Arrival        *atimes;
    Slowness_vector *slow;
    int             natimes;
    int             nslow;
    int             ndata;
    float          *work;	       /* work array */
    double          beta;
    int             i,
                    j,
                    k,
                    ii;
    int             npar;	       /* npar is the number of free location
				        * parameters.  It is calculated by a
				        * messy combination of options.fix
				        * and the origin time is handled */
    Robust_statistics statistics;      /* this holds the return values */
    double          error_scale;       /* error scaled used in residual
				        * weighting */

    natimes = maxtbl (attbl);
    nslow = maxtbl (utbl);
    ndata = natimes + 2 * nslow;
    work = (float *) calloc (ndata, sizeof (float));
    /* determine npar using pattern of fix.  Note origin time recenter option
     * is always ignored and we always return a term for the origin time.
     * This is just turned off when recentering is actually used. */
    for (i = 0, npar = 0; i < 4; ++i)
	if (!options.fix[i])
	    ++npar;

    /* this is the main loop to form the equations */
    for (i = 0; i < natimes; ++i) {
	Travel_Time_Function_Output tto;
	atimes = (Arrival *) gettbl (attbl, i);
	tto = calculate_travel_time (*atimes, current_location, mode);
	if (tto.time == TIME_INVALID) {
	    r[i] = 0.0;
	    w[i] = 0.0;
	    b[i] = 0.0;
	    if (mode == ALL)
		for (k = 0; k < npar; ++k)
		    A[i][k] = 0.0;
	    elog_log(1, "Station: %s, Phase: %s Travel time calculator failed\n",
			    atimes->sta->name, atimes->phase->name);
	} else {
	    /* We have to handle phases like S-P specially because these do
	     * not require differencing the origin time.  We use the phase
	     * name to trigger this switch. */
	    if (strchr (atimes->phase->name, '-') != NULL)
		r[i] = (float) (atimes->time - tto.time);
	    else
		r[i] = (float) (atimes->time
				- current_location.time
				- tto.time);
	    w[i] = (float) (1.0 / atimes->deltat);
	    if (options.atime_distance_weight)
		w[i] *= distance_weight_time (*atimes, current_location);
	    b[i] = r[i];
	    b[i] *= w[i];
	    if (mode == ALL) {
		for (k = 0, ii = 0; k < 4; ++k) {
		    if (!options.fix[k]) {
			switch (k) {
			  case (0):
			    A[i][ii] = (float) tto.dtdx;
			    break;
			  case (1):
			    A[i][ii] = (float) tto.dtdy;
			    break;
			  case (2):
			    A[i][ii] = (float) tto.dtdz;
			    break;
			  case (3):
			    A[i][ii] = 1.0;
			    break;
			}
			++ii;
		    }
		}
	    }
	}
    }
    for (i = 0, j = natimes; i < nslow; ++i, j += 2) {
	Slowness_Function_Output u_calc;

	slow = (Slowness_vector *) gettbl (utbl, i);

	u_calc = calculate_slowness_vector (*slow,
					    current_location, mode);
	if (u_calc.ux == SLOWNESS_INVALID) {
	    r[j] = 0.0;
	    r[j + 1] = 0.0;
	    w[j] = 0.0;
	    w[j + 1] = 0.0;
	    b[j] = 0.0;
	    b[j + 1] = 0.0;
	    if (mode == ALL) {
		for (k = 0; k < npar; ++k) {
		    A[j][k] = 0.0;
		    A[j + 1][k] = 0.0;
		}
	    }
	    elog_log(1, "Station: %s, Phase: %s Slowness vector calculator failed\n",
			    atimes->sta->name, atimes->phase->name);
	} else {
	    r[j] = (float) (slow->ux - u_calc.ux);
	    r[j + 1] = (float) (slow->uy - u_calc.uy);
	    w[j] = 1.0 / slow->deltaux;
	    w[j + 1] = 1.0 / slow->deltauy;
	    if (options.slow_distance_weight) {
		w[j] *= distance_weight_ux (*slow, current_location);
		w[j + 1] *= distance_weight_uy (*slow, current_location);
	    }
	    b[j] = r[j];
	    b[j + 1] = r[j + 1];
	    b[j] *= w[j];
	    b[j + 1] *= w[j + 1];
	    if (mode == ALL) {
		for (k = 0, ii = 0; k < 4; ++k) {
		    if (!options.fix[k]) {

			switch (k) {
			  case (0):
			    A[j][ii] = (float) (u_calc.duxdx);
			    A[j + 1][ii] = (float) (u_calc.duydx);
			    break;
			  case (1):
			    A[j][ii] = (float) (u_calc.duxdy);
			    A[j + 1][ii] = (float) (u_calc.duydy);
			    break;
			  case (2):
			    A[j][ii] = (float) (u_calc.duxdz);
			    A[j + 1][ii] = (float) (u_calc.duydz);
			    break;
			  case (3):
			    /* phase velocities don't ever depend upon the
			     * origin time */
			    A[j][ii] = 0.0;
			    A[j + 1][ii] = 0.0;
			}
			++ii;
		    }
		}
	    }
	}
    }

    /* Use the interquartiles to determine the scale of errors in the
     * weighted residual vector assuming the residuals are normally
     * distributed.  We first copy the residual vector to work because
     * calc_statistics sorts the input vector in place.  Note we throw out
     * data with zero weights. */

    for (i = 0, ii = 0; i < ndata; ++i) {
	if (w[i] > 0.0) {
	    work[ii] = b[i];
	    ++ii;
	}
    }
    *nused = ii;
    /* calc_statistics returns a bogus values when nused < 2 We trap that
     * situation by setting the error scale to max and assume the calling
     * program will handle this problem correctly when it sees the value of
     * *nused */

    statistics = calc_statistics (work, *nused);
    if (*nused <= 2)
	error_scale = options.max_error_scale;
    else {
	error_scale = (statistics.q3_4 - statistics.q1_4)
	    / INTERQUARTILE_SDEV_FACTOR;
	/* Reset the error scale if it exceeds the allowed range.  This is
	 * essential with residual weighting to avoid instability */
	if (error_scale > options.max_error_scale)
	    error_scale = options.max_error_scale;
	if (error_scale < options.min_error_scale)
	    error_scale = options.min_error_scale;
	/* If the origin time is way off in early iterations we can get into
	 * real trouble if we don't adjust the error scale upward. That is,
	 * if there is a large dc offset, this small adjustment will prevent
	 * downweighting all residuals too much */
	if (fabs (statistics.median) > error_scale)
	    error_scale += fabs (statistics.median);

    }
    /* The Thomson formula is the only one that needs a factor that depends
     * on anything other than the error_scale.  Here we compute the ndata-th
     * quantile of a normal distribution as recommended by Chave and Thomson */
    if (((options.atime_residual_weight) == THOMSON)
	    || ((options.slow_residual_weight) == THOMSON)) {
	beta = normal_quantile (ndata, ndata);
    }
    /* We pass through this loop even if residual weighting is turned off.
     * In that situation we just set the reswt values all to one.  Otherwise
     * they are determined from the residual values.  We need two loops,
     * unfortunately, because we are allowing different schemes for arrival
     * times and slowness. Because of this mess we are also going to scale
     * the rows of the matrix A in the same pair of loops so we don't have to
     * have yet another pair of messy loops. */
    for (i = 0; i < natimes; ++i) {

	switch (options.atime_residual_weight) {
	  case (HUBER):
	    reswt[i] = huber ((float) (b[i] / error_scale));
	    break;
	  case (THOMSON):
	    reswt[i] = thomson ((float) (b[i] / error_scale),
				(float) beta);
	    break;
	  case (BISQUARE):
	    reswt[i] = bisquare ((float) (b[i] / error_scale));
	    break;
	  case (NONE):
	    reswt[i] = 1.0;
	    break;
	  default:
	    reswt[i] = 1.0;
	}
	b[i] *= reswt[i];
	if (mode == ALL)
	    for (k = 0; k < npar; ++k)
		A[i][k] *= (w[i] * reswt[i]);
    }
    for (i = natimes; i < ndata; ++i) {

	switch (options.slow_residual_weight) {
	  case (HUBER):
	    reswt[i] = huber ((float) (b[i] / error_scale));
	    break;
	  case (THOMSON):
	    reswt[i] = thomson ((float) (b[i] / error_scale), (float) beta);
	    break;
	  case (BISQUARE):
	    reswt[i] = bisquare ((float) (b[i] / error_scale));
	    break;
	  case (NONE):
	    reswt[i] = 1.0;
	    break;
	  default:
	    reswt[i] = 1.0;
	}
	b[i] *= options.slowness_weight_scale_factor * reswt[i];
	w[i] *= options.slowness_weight_scale_factor;
	if (mode == ALL)
	    for (k = 0; k < npar; ++k)
		A[i][k] *= w[i] * reswt[i]
		    * (options.slowness_weight_scale_factor);
    }

    free (work);
    return (statistics);
}

/* These two functions calculate raw and weighted residual rms respectively.
Note the second requires passing two parallel weight arrays.  When called
from ggnloc these are base weights and residual weights (rw) respectively,
but the order is actually irrelevant.  Note also for the weighted rms
function it is assumed that wd is the vector of weighted residuals.
In calculate_rms, in contrast, d is used raw. */

#ifdef SUNPERF
double 
calculate_rms (float *d, int n)
{
    double          value;
    value = (double) snrm2 (n, d, 1);
    return (value / sqrt ((double) n));
}

double 
calculate_weighted_rms (float *wd, float *w, float *rw, int n)
{
    int             i;
    double          value,
                    sumw;
    value = (double) snrm2 (n, wd, 1);
    for (i = 0, sumw = 0.0; i < n; ++i)
	sumw += (double) (w[i] * rw[i]);
    return (value / sqrt (sumw));
}

#else
double 
calculate_rms (float *d, int n)
{
    double          value;
    int             one = 1;
    value = (double) snrm2_ (&n, d, &one);
    return (value / sqrt ((double) n));
}

double 
calculate_weighted_rms (float *wd, float *w, float *rw, int n)
{
    int             i;
    double          value,
                    sumw;
    int             one = 1;
    value = (double) snrm2_ (&n, wd, &one);
    for (i = 0, sumw = 0.0; i < n; ++i)
	sumw += (double) (w[i] * rw[i]);
    return (value / sqrt (sumw));
}

#endif

/* initializes the hypocenter structure to stock defaults */
void 
initialize_hypocenter (Hypocenter * h)
{
    h->dx = 0.0;
    h->dy = 0.0;
    h->dz = 0.0;
    h->dt = 0.0;
    h->lat = 0.0;
    h->lon = 0.0;
    h->z = 0.0;
    h->lat0 = 0.0;
    h->lon0 = 0.0;
    h->z0 = 0.0;
    h->t0 = 0.0;
    h->rms_raw = 0.0;
    h->rms_weighted = 0.0;
    h->interquartile = 0.0;
    h->number_data = 0;
    h->degrees_of_freedom = 0;
    h->used = 1;
}

/* copies hypocenter structure from to */

void 
copy_hypocenter (Hypocenter * from, Hypocenter * to)
{
    to->dx = from->dx;
    to->dy = from->dy;
    to->dz = from->dz;
    to->lat = from->lat;
    to->lon = from->lon;
    to->z = from->z;
    to->time = from->time;
    to->lat0 = from->lat0;
    to->lon0 = from->lon0;
    to->z0 = from->z0;
    to->t0 = from->t0;
    to->rms_raw = from->rms_raw;
    to->rms_weighted = from->rms_weighted;
    to->interquartile = from->interquartile;
    to->number_data = from->number_data;
    to->degrees_of_freedom = from->degrees_of_freedom;
    to->used = from->used;
}

void 
step_length_damp (Hypocenter h, float *dx, int n, int iz,
		  Location_options options)
{
    float           scale = 1.0;
    float           ztrial;
    int             i;

    ztrial = h.z + dx[iz];

    if ((ztrial > options.depth_ceiling)
	    && (ztrial < options.depth_floor))
	return;

    /* block for events already at the ceiling or floor that would be
     * adjusted outside these bounds */
    if (difference_is_zero (h.z, options.depth_ceiling)
	    && (dx[iz] < 0.0)) {
	dx[iz] = 0.0;
	return;
    } else if (difference_is_zero (h.z, options.depth_floor)
	       && (dx[iz] > 0.0)) {
	dx[iz] = 0.0;
	return;
    } else {
	do {
	    if (scale < options.min_step_length_scale) {
		scale = options.min_step_length_scale;
		break;
	    }
	    scale *= options.step_length_scale_factor;
	    ztrial = h.z + scale * dx[iz];
	}
	while ((ztrial < options.depth_ceiling)
		|| (ztrial > options.depth_floor));

	for (i = 0; i < n; ++i)
	    dx[i] *= scale;

	/* when the step still is outside the bounds, we leave the other
	 * coordinates rescaled, and set the depth the ceiling of floor as
	 * appropriate */

	if (ztrial < options.depth_ceiling)
	    dx[iz] = options.depth_ceiling - h.z;
	else if (ztrial > options.depth_floor)
	    dx[iz] = options.depth_floor - h.z;

    }
}

/* adjust_hypocenter updates the full hypocenter structure of the
current location vector.  Note we store somewhat redundant information
in the dx,dy,dz adjustment vector as well as the current and
previous location estimates.  Since the stack of hypocenter should
never be very big, this is not a serious memory problem with modern
computers.  Note adjustments are always calculated in km and converted
to spherical coordinates.  This is done because it is numerically more
stable to keep all elements of the hypocenter adjustment vector in
comparable units.  Thus, x,y,z adjustments are all done in km while
the origin time is done is seconds.  Because seismic velocities are all
of order unity, we don't bother to rescale the origin time term.

Additional complexity enters here through the various "fix" and
how different options handle the origin time (recentering option).
This is handled implicitly here through counting up to npar and
simultaneously checking the fix array elements.

arguments:
	current_location - Hypocenter structure to be updated
	dx(npar) - adjustment vector of length npar to correct
		current_location with.
	fix - four element integer array setting pattern of variables to
		be "fixed".  Any nonzero element of fix implies a
		corresponding coordinate of the hypocenter is fixed.
		Here that means:  fix[0]=x=latitude, fix[1]=y=longitude,
		fix[2]=z=depth, fix[3]=t=origin time.

Author:  Gary L. Pavlis, Indiana University
Written:  March 1996
*/
#define RADIUS_EARTH 6370.8	       /* Radius of earth in km for lat-lon
				        * adjustment */

Hypocenter 
adjust_hypocenter (Hypocenter current_location, float *dx,
		   int npar, Location_options o)
{
    int             i,
                    k;
    Hypocenter      new_hypo;

    new_hypo.dx = 0.0;
    new_hypo.dy = 0.0;
    new_hypo.dz = 0.0;
    new_hypo.dt = 0.0;
    new_hypo.used = 1;
    /* the i< 4 is not required, but safer */
    for (i = 0, k = 0; (k < npar) && (i < 4); ++i) {
	if (!o.fix[i]) {
	    switch (i) {
	      case (0):
		new_hypo.dx = dx[k];
		break;
	      case (1):
		new_hypo.dy = dx[k];
		break;
	      case (2):
		new_hypo.dz = dx[k];
		break;
	      case (3):
		new_hypo.dt = dx[k];
		break;
	    }
	    ++k;
	}
    }

    if ((o.generalized_inverse == PSEUDO_RECENTERED)
	    || (o.generalized_inverse == DAMPED_RECENTERED))
	new_hypo.dt += current_location.dt;
    /* Now we convert correction for latitude and longitude using simple
     * spherical formula.  Note, this assumes lat and lon are in degrees. */
    new_hypo.lon = current_location.lon
	+ deg (cos (rad (current_location.lat)) * (new_hypo.dx) / RADIUS_EARTH);
    if (new_hypo.lon > 180.0)
	new_hypo.lon -= 360.0;
    if (new_hypo.lon < -180.0)
	new_hypo.lon += 360.0;


    new_hypo.lat = current_location.lat + deg ((new_hypo.dy) / RADIUS_EARTH);
    if (new_hypo.lat > 90.0) {
	new_hypo.lat = 180.0 - new_hypo.lat;
	new_hypo.lon += 180.0;
	if (new_hypo.lon > 180.0)
	    new_hypo.lon -= 360.0;
    } else if (new_hypo.lat < -90.0) {
	new_hypo.lat = -180.0 - new_hypo.lat;
	new_hypo.lon += 180.0;
	if (new_hypo.lon > 180.0)
	    new_hypo.lon -= 360.0;
    }
    new_hypo.z = current_location.z + new_hypo.dz;
    new_hypo.time = current_location.time + new_hypo.dt;
    /* reset origin then exit */
    new_hypo.lat0 = current_location.lat;
    new_hypo.lon0 = current_location.lon;
    new_hypo.z0 = current_location.z;
    new_hypo.t0 = current_location.time;
    /* Note we just copy the statistics.  For this reason the statistics
     * listed are actually based on a linear extrapolation, which will not be
     * totally accurate when the step size is large. */
    new_hypo.rms_raw = current_location.rms_raw;
    new_hypo.rms_weighted = current_location.rms_weighted;
    new_hypo.interquartile = current_location.interquartile;
    new_hypo.number_data = current_location.number_data;
    new_hypo.degrees_of_freedom = current_location.degrees_of_freedom;
    return (new_hypo);
}

/* Equation solver Levenberg-Marquart (damped) inverse solution.

None of the input vectors is altered to allowed repeated solutions with
the same matrix.  Only x is altered.

Arguments:
	U - matrix of left singular vectors returned by svdcmd, note
		pointers start at 1 not zero which complicates things.
	s - vector of singular values (starts at 0)
	V - vector of right singular vectors returned by svdcmd.
	b - rhs vector for solution
	m,n - original matrix A is assumed to have been of dimension mxn
	damp - damping parameter (IMPORTANT - this is RELATIVE see below)
	x - solution vector (length n)

Author:  Gary Pavlis
	Department of Geological Sciences
	Indiana University

Written:  March 1996
*/

void 
lminverse_solver (float **U, float *s, float **V, float *b,
		  int m, int n, float damp, float *x)
{

#ifndef SUNPERF
    int             one = 1;
#endif

    int             i,
                    j;		       /* counters */
    float          *work;	       /* work array alloced below */
    float           smax;	       /* largest singular value */

    /* Because we use relative damping, we have to find the largest singular
     * value, and rescale damp accordingly */
    smax = 0.0;
    for (i = 0; i < n; ++i)
	if (s[i] > smax)
	    smax = s[i];
    damp *= smax;

    if ((work = (float *) calloc (n, sizeof (float))) == NULL)
	elog_die(1, "Inverse solver cannot alloc work array of length %d\n",
	     n);

    for (j = 0; j < n; ++j) {
	x[j] = 0.0;
	for (i = 0; i < m; ++i)
	    x[j] += U[i][j] * b[i];
	x[j] *= s[j] / (s[j] * s[j] + damp * damp);
    }

#ifdef SUNPERF
    scopy (n, x, 1, work, 1);
#else
    scopy_ (&n, x, &one, work, &one);
#endif

    /* multiply by V */
    for (j = 0; j < n; ++j)

#ifdef SUNPERF
	x[j] = sdot (n, V[j], 1, work, 1);
#else
	x[j] = sdot_ (&n, V[j], &one, work, &one);
#endif

    free (work);
}

/* Equation solver for pseudoinverse solutions.  Routine uses a fairly
tricky coding that is necessary because svdcmd does not sort the singular
values.  We scan for the largest, and then just zero the small singular
values on the fly in forming the product of S-1 * UT.  (see below)

None of the input vectors is altered to allowed repeated solutions with
the same matrix.  Only x and the work vector are altered.

Arguments:
	U - matrix of left singular vectors returned by svdcmd, note
		pointers start at 1 not zero which complicates things.
	s - vector of singular values (starts at 0)
	V - vector of right singular vectors returned by svdcmd.
	b - rhs vector for solution
	m,n - original matrix A is assumed to have been of dimension mxn
	rsvc - key parameter used to determined sv
		cutoff.  Cutoff is set as max (s) * rsvc
	x - solution vector (length n)

Author:  Gary Pavlis
	Department of Geological Sciences
	Indiana University

Written:  March 1996
*/

int 
pseudoinverse_solver (float **U, float *s, float **V, float *b,
		      int m, int n, float rsvc, float *x)
{
    int             i,
                    j;		       /* counters */
    int             nsvused;	       /* number of nonzero singular values
				        * used in solution */
    float           sv_cutoff,
                    smax;
    float          *work;	       /* work space */

#ifndef SUNPERF
    int             one = 1;
#endif

    if ((work = (float *) calloc (n, sizeof (float))) == NULL)
	elog_die(1, "Inverse solver cannot alloc work array of length %d\n",
	     n);

    /* first find the larges singular value, then just zero all those smaller
     * than the cutoff determined as the ratio wrt to largest singular value */
    smax = 0.0;
    for (i = 0; i < n; ++i)
	if (s[i] > smax)
	    smax = s[i];
    sv_cutoff = smax * rsvc;


    /* multiply by S-1 * UT */
    nsvused = 0;
    for (j = 0; j < n; ++j) {
	x[j] = 0.0;
	if (s[j] < sv_cutoff)
	    continue;
	for (i = 0; i < m; ++i)
	    x[j] += U[i][j] * b[i];
	x[j] /= s[j];
	++nsvused;
    }

#ifdef SUNPERF
    scopy (n, x, 1, work, 1);
#else
    scopy_ (&n, x, &one, work, &one);
#endif

    /* multiply by V */
    for (j = 0; j < n; ++j)

#ifdef SUNPERF
	x[j] = sdot (n, V[j], 1, work, 1);
#else
	x[j] = sdot_ (&n, V[j], &one, work, &one);
#endif

    free (work);
    return (nsvused);
}

/* This is the main location routine.  It is a fairly complex but generic
location program that pretty much allows any implementation of nonlinear
least squares that has been tried to date.  It does not implement a few more
exotic approaches that have been tried, but are not used routinely.

This function accepts both a set of arrival time and array slowness measurements
for an arbitary list of phases.  It can handle anything it knows about through
the phase handles that have to be set externally.  All error checking for the
existence of unknown phases MUST be done externally.  This procedure will use
every data point, and assumes it will know how to calculate travel times or
slowness values for that datum.  If it does not, it will avoid the problem by
downweighting that datum to have a zero weight.

This procedure uses a flexible weighting scheme.  External documentation I
eventually plan to write needs to explain this as all the options are too complex
to relate here.

This function returns a Tbl that is a stack of pointers to a sequence of
locations defined by the iterative sequence.  Note the Hypocenter structures
to which this tbl points are alloced in this routine, and MUST be freed externally
or a memory leak is inevitable.

arguments:
	initial_location - starting hypocenter solution
	attbl, utbl - Tbl of pointers to Arrival and Slowness vector data
		structures
	options - structure of all program options
	Returns:
	convergence_history, reason_converged, and restbl

	convergence_history contains a stack of Hypocenter structures
	describing the convergence history.  This object is allocated
	here and should be freed externally.

	reason_converged - stack of strings describing reasons for
	convergence.

	restbl - formatted string of calculated residuals.  This is
	somewhat redundant as the Arrival and Slowness_vector structures
	now have residual entries that are alternative ways to access
	the same information.
	contents:  sta, phase, type, weighted residual, raw residual,
			weight, residual_weight
		where type is "time", "ux", or "uy"

Returns:  Normal return is 0.  Nonzero returns mean problems where
	encountered.  Positive numbers mean a location was returned.
	Negative numbers mean something bad happened and the residual
	stack should not be trusted.  A positive return is the count of
	the number of times the travel time calculator failed during
	this location (it may be greater than the number of data points
	because it continues to count from interation to iteration.
	Negative codes are as follows:

	-1 - insufficient data, immediate exit
	-2 - data loss due to travel time calculator errors, led to
		insufficient data to continue with makeqn.
	-3 - slight variant on -2.  Exit during adjustment steps in
		the Marquardt algorithm.
	-4 - failure of svd routine.  results undefined

Author:  Gary L. Pavlis, Indiana University
Written:  February 1996

Revised heavily:  November 1996
Arguments drastically changed.
Added restbl return of weights as a new argument.   restbl is an ascii table of
residual information.
April, 1997:  Major change in error return method.  Function now returns
an int error code (see above).
*/


int 
ggnloc (Hypocenter initial_location,
	Tbl * attbl, Tbl * utbl,
	Location_options options,
	Tbl ** convergence_history,
	Tbl ** reason_converged,
	Tbl ** restbl)
{
    int             natimes;
    int             nslow;

    float         **A,
                  **U,
                  **V,
                    s[4];	       /* SVD of A = USVT */
    float          *b;		       /* weighted residual vector (rhs of
				        * Ax=b) */
    float          *r,
                   *w;		       /* r=raw residual and w= vector of
				        * total weights */
    float          *reswt;	       /* we store residual weights
				        * seperately to calculate effective
				        * degrees of freedom */
    /* This group of vectors are parallel vectors to b,r,w,reswt required
     * when running a damped solution with Marquardt's method */
    float          *btmp,
                   *rtmp,
                   *wtmp,
                   *reswttmp;

    float          *work;	       /* work array the length of the data
				        * used in various places */
    float           dx[4];	       /* hypocenter adjustment vector in
				        * local coordinates */
    int             m;		       /* total number of data points =
				        * natimes+2*nslow */
    int             npar;	       /* actual number of adjustable
				        * parameters in solution. npar is
				        * determined by counting zero entries
				        * in the "fix" input array of
				        * integers in options */
    int             np;		       /* A stupid temporary needed because
				        * of FORTRAN */
    Hypocenter      current_location,
                    trial_location;
    int             iteration;	       /* count of number of iterations */
    Robust_statistics statistics,
                    raw_statistics;
    int             iz;		       /* iz is set to the column of A
				        * corresponding to z This can vary
				        * because of fix options */
    double          previous_wrms,
                    new_wrms,
                    test_wrms;	       /* different weighted rms statistics
				        * used in different contexts.
				        * previous = previous iteration
				        * value, new_wrms = sets new value
				        * for current iteration, test = used
				        * in marquardt damping adjustments */
    double          sum_reswt;	       /* holds sum of residual weights used
				        * to calculate effective degrees of
				        * freedom */

    Hypocenter     *hypo_history;
    int             converge;	       /* simple convergence flag */
    int             i,
                    j;
    float           damp;
    double          relative_rms_change,
                    nrm2_dx;
    char           *stack_mesg;
    int             recenter;	       /* set to one for recenter solutions */
    char            res_output[80];    /* holds each line of residual
				        * information returned in restbl */
    char           *stmp;	       /* holds pointer returned by strdup
				        * for each line formed in res_output */

    Arrival        *a;
    Slowness_vector *slow;
    char           *sta;
    char           *phase;

    int             ret_code = 0;

#ifndef SUNPERF
    int             one = 1;
#endif

    if ((options.generalized_inverse == PSEUDO_RECENTERED)
	    || (options.generalized_inverse == DAMPED_RECENTERED))
	recenter = 1;
    else
	recenter = 0;

    natimes = maxtbl (attbl);
    nslow = maxtbl (utbl);

    m = 2 * nslow + natimes;

    /* Initialize output tbl.  We do this immediately since all are returned
     * and the caller's responsibility to free */

    *convergence_history = newtbl (0);
    *reason_converged = newtbl (0);
    *restbl = newtbl (0);

    /* Determine npar and iz.  (iz points to column of A for depth variable) */
    for (i = 0, npar = 0; i < 4; ++i) {
	if (i == 2)
	    iz = npar;
	if (!options.fix[i])
	    ++npar;
    }
    if (m < npar) {
	elog_log(0, "Insufficient data to locate event at epoch time %f\n",
			initial_location.time);
	stack_mesg = strdup ("Error:  insufficient data");
	pushtbl (*reason_converged, stack_mesg);
	return (-1);
    }
    /* Alloc space for main working arrays.  Note we don't worry about fix
     * reducing necessary sizes because none of these arrays are that large. */
    A = matrix (0, m - 1, 0, 3);
    U = matrix (0, m - 1, 0, 3);
    V = matrix (0, 3, 0, 3);


    b = (float *) calloc (m, sizeof (float));
    r = (float *) calloc (m, sizeof (float));
    w = (float *) calloc (m, sizeof (float));
    reswt = (float *) calloc (m, sizeof (float));
    work = (float *) calloc (m, sizeof (float));
    btmp = (float *) calloc (m, sizeof (float));
    rtmp = (float *) calloc (m, sizeof (float));
    wtmp = (float *) calloc (m, sizeof (float));
    reswttmp = (float *) calloc (m, sizeof (float));

    /* After all those allocs we need to check if it all worked */
    if ((b == NULL) || (r == NULL)
	    || (w == NULL) || (reswt == NULL) || (work == NULL)
	    || (btmp == NULL) || (rtmp == NULL)
	    || (wtmp == NULL) || (reswttmp == NULL))
	elog_die(1, "Alloc errors in main location function\n");


    /* This initializes both of these work objects correctly */
    copy_hypocenter (&initial_location, &current_location);
    copy_hypocenter (&initial_location, &trial_location);
    iteration = 0;
    converge = 0;
    if ((options.generalized_inverse == DAMPED_RECENTERED)
	    || (options.generalized_inverse == DAMPED_INVERSE))
	damp = options.min_relative_damp;

    do {
	int             nsv_used,
	                ndata_used,
	                ndata_feq;
	Robust_statistics test;
	char            message[256];

	hypo_history = (Hypocenter *) malloc (sizeof (Hypocenter));
	if (hypo_history == NULL)
	    elog_die(1, "Malloc error in location routine\n");
	copy_hypocenter (&current_location, hypo_history);
	pushtbl (*convergence_history, hypo_history);

	statistics = form_equations (ALL, current_location, attbl, utbl,
				     options, A, b, r, w, reswt, &ndata_feq);
	if (ndata_feq < npar) {
	    elog_log(0, "Data loss leading to insufficient data for event %f\n",
			    current_location.time);
	    stack_mesg = strdup ("Error:  critical data loss");
	    pushtbl (*reason_converged, stack_mesg);
	    return (-2);
	}
	if (ndata_feq != m)
	    ret_code += (m - ndata_feq);

	previous_wrms = calculate_weighted_rms (b, w, reswt, m);

	/* For recentered solutions we determine an origin time correction
	 * from a weighted median of the arrival times. We handle that easily
	 * here by copying the first natimes elements to the work array, and
	 * then calculating the median.  Note this is NOT the same as the
	 * process used in form_equations above.  There we look at the whole
	 * vector (including slowness measurements) while here we look at
	 * only the arrival times.  The weighting is also different Here we
	 * only use the reswt values.  We do, however, still skip data
	 * flagged with w set to zero. Note variable np which is actual
	 * number of parameters inverted for with matrix solution */
	if (recenter && (options.fix[3] == 0)) {
	    np = npar - 1;
	    for (i = 0, ndata_used = 0; i < natimes; ++i) {
		if (w[i] > 0.0) {
		    work[ndata_used] = r[i] * reswt[i];
		    ++ndata_used;
		}
	    }
	    raw_statistics = calc_statistics (work, ndata_used);
	    current_location.dt = raw_statistics.median;
	    /* reset the arrival time residuals */
	    for (i = 0; i < natimes; ++i) {
		r[i] -= raw_statistics.median;
		b[i] = r[i] * w[i] * reswt[i];
	    }
	    previous_wrms = calculate_weighted_rms (b, w, reswt, m);
	} else
	    np = npar;
	/* Now compute the SVD of A.  We copy A to U for clarity, but we
	 * wouldn't absolutely have to do this. Error calculations I
	 * anticipate in the future need A.  (svdcmp overwrites U) */
	for (i = 0; i < m; ++i)
	    for (j = 0; j < np; ++j)
		U[i][j] = A[i][j];
	if (np > 0) {
	    i = svdcmp (U, m, np, s, V);
	    if (i < 0) {
		elog_complain(0, "ggnloc irreconcilable problem with svd routine\nCannot generate solution\n");
		return (-4);
	    } else if (i > 0) {
		elog_complain(0, "svd convergence error in iteration %d\nCannot compute solution\n",
			  iteration);
		return (-4);
	    }
	    /* Now how we construct a solution depends on the algorithm */

	    switch (options.generalized_inverse) {
	      case (PSEUDOINVERSE):
	      case (PSEUDO_RECENTERED):
		nsv_used = pseudoinverse_solver (U, s, V, b, m, np,
					    options.sv_relative_cutoff, dx);
		if (nsv_used != np && GenlocVerbose) {
		    elog_log (0, "ggnloc:  svdtrucation applied to invert equations of condition\n");
		    elog_log (0, "Using %d of %d singular values\n",
			      nsv_used, np);
		    elog_log (0, "Singular values: ");
		    for (i = 0; i < np; ++i)
			elog_log (0, " %f ", s[i]);
		    elog_log (0, "\n");
		}
		if (!options.fix[2])
		    step_length_damp (current_location,
				      dx, np, iz, options);

		trial_location = adjust_hypocenter (current_location,
						    dx, np, options);
		/* We recalculate residuals in tmp work spaces to allow
		 * parallel code to damped solver below */
		test = form_equations (RESIDUALS_ONLY,
			      trial_location, attbl, utbl, options, A, btmp,
				       rtmp, wtmp, reswttmp, &ndata_feq);
		if (ndata_feq != m)
		    ret_code += (m - ndata_feq);


		break;
	      case (DAMPED_INVERSE):
	      case (DAMPED_RECENTERED):
		do {
		    lminverse_solver (U, s, V, b, m, np, damp, dx);
		    if (!options.fix[2])
			step_length_damp (current_location,
					  dx, np, iz, options);
		    trial_location
			= adjust_hypocenter (current_location,
					     dx, np, options);

		    /* Note use of tmp arrays here.  This preserves original
		     * weights consistent with original matrix row weighting.
		     * We only look at the statistics of the result vector to
		     * decide if we are improving or not */

		    test = form_equations (RESIDUALS_ONLY,
					   trial_location, attbl, utbl,
					   options, A, btmp, rtmp, wtmp,
					   reswttmp, &ndata_feq);
		    if (ndata_feq != m)
			ret_code += (m - ndata_feq);
		    if (ndata_feq < np)
			break;

		    test_wrms = calculate_weighted_rms (btmp, wtmp,
							reswttmp, m);
		    damp *= options.damp_adjust_factor;
		}
		while ((damp < options.max_relative_damp)
			&& (test_wrms >= previous_wrms));

		/* The 2 here is needed because damp is adjusted at the
		 * bottom of the above loop.  Standard applicatoin of
		 * marquardt's method tries to continually reduce the damping
		 * constant.  Unless we do this, damp will only get larger. */

		damp /= (options.damp_adjust_factor) * 2.0;
		if (damp < options.min_relative_damp)
		    damp = options.min_relative_damp;
	    }
	    if (ndata_feq < np) {
		elog_log(0, "Data loss leading to insufficient data for event %f\n",
				current_location.time);
		stack_mesg = strdup ("Error:  critical data loss");
		pushtbl (*reason_converged, stack_mesg);
		return (-3);
	    }
	} else {
	    /* This is a special block is for all coordinates fixed */
	    current_location.rms_raw
		= calculate_rms (r, natimes);
	    current_location.rms_weighted
		= calculate_weighted_rms (b, w, reswt, m);
	    current_location.degrees_of_freedom = m;
	    current_location.number_data = m;
	    /* old bug here.  Need to copy these quantities */
	    scopy(m,b,1,btmp,1);
	    scopy(m,r,1,rtmp,1);
	    scopy(m,w,1,wtmp,1);
	    scopy(m,reswt,1,reswttmp,1);
	    
	    /* I'm leaving interquartile blank here hoping this won't cause
	     * problems downstream. */
	    stack_mesg = strdup ("All coordinates fixed");
	    pushtbl (*reason_converged, stack_mesg);
	    break;
	}

	/* We need to update the current location, and this includes copying
	 * the statistical quantities from the test structure */
	copy_hypocenter (&trial_location, &current_location);
	new_wrms = calculate_weighted_rms (btmp, wtmp, reswttmp, m);
	/* Note raw rms ONLY uses times to avoid mixing units */
	current_location.rms_raw = calculate_rms (rtmp, natimes);
	current_location.rms_weighted = new_wrms;
	current_location.interquartile = test.q3_4 - test.q1_4;
	/* Here we count the number of data points actually used which is
	 * defined as those points for which neither of the weights for that
	 * datum are zero.  */

        /* This loop was changed by JN to correct calculation of degrees_of_freedom */
	for (i = 0, current_location.number_data = 0, sum_reswt = 0.0; i < m; ++i) {
	    if ((wtmp[i] > 0.0) && (reswttmp[i] > 0.0)) {
		++(current_location.number_data);
	        sum_reswt += (double) reswttmp[i];
            }
	}
	current_location.degrees_of_freedom = rint (sum_reswt) - npar;

	relative_rms_change = fabs (new_wrms - previous_wrms)
	    / previous_wrms;
	++iteration;
	/* We don't include origin time in convergence check. This may not
	 * always be smart, but because it has units it is dumb to include it
	 * in the same norm measure. We could use a velocity scale factor,
	 * but in practice it should not matter.  Furthermore, recentering
	 * causes other complications to lengthy to discuss in code comments. */

#ifdef SUNPERF
	nrm2_dx = (double) snrm2 (np, dx, 1);
#else
	nrm2_dx = (double) snrm2_ (&np, dx, &one);
#endif

	if (nrm2_dx <= options.dx_convergence) {
	    converge = 1;
	    sprintf (message, "Correction vector is small.  L2 norm of final correction = %f", nrm2_dx);
	    stack_mesg = strdup (message);	/* strdup allocs new space
						 * before a copy.  WE need
						 * this here */
	    pushtbl (*reason_converged, stack_mesg);
	}
	if (iteration >= options.max_hypo_adjustments) {
	    converge = 1;
	    sprintf (message, "Location hit iteration count limit of %d",
		     options.max_hypo_adjustments);
	    stack_mesg = strdup (message);
	    pushtbl (*reason_converged, stack_mesg);
	}
	if (relative_rms_change < options.relative_rms_convergence) {
	    converge = 1;
	    sprintf (message, "Insignificant improvement in fit to these data.  Final relative rms change = %f\n", relative_rms_change);
	    stack_mesg = strdup (message);
	    pushtbl (*reason_converged, stack_mesg);
	}
    } while (converge == 0);
    /* Dont' forget to push the final hypocenter onto the convergence stack */
    hypo_history = (Hypocenter *) malloc (sizeof (Hypocenter));
    if (hypo_history == NULL)
	elog_die(1, "Malloc error in location routine\n");
    copy_hypocenter (&current_location, hypo_history);
    pushtbl (*convergence_history, hypo_history);

    /* Now we form restbl contents.  The only tricky part is cleanly
     * splitting array and arrival time observations */
    for (i = 0, j = 0; i < natimes; ++i, ++j) {
	a = (Arrival *) gettbl (attbl, i);
	sta = a->sta->name;
	phase = a->phase->name;

	a->res.weighted_residual = btmp[j];
	a->res.raw_residual = rtmp[j];
	a->res.residual_weight = reswttmp[j];
	a->res.other_weights = wtmp[j];

	sprintf (res_output, "%s %s time %g %g %g %g", sta, phase,
		 btmp[j], rtmp[j], wtmp[j], reswttmp[j]);
	stmp = strdup (res_output);
	pushtbl (*restbl, stmp);
    }

    for (i = 0; i < nslow; ++i, j += 2) {
	slow = (Slowness_vector *) gettbl (utbl, i);
	sta = slow->array->name;
	phase = slow->phase->name;

	slow->xres.weighted_residual = btmp[j];
	slow->xres.raw_residual = rtmp[j];
	slow->xres.residual_weight = reswttmp[j];
	slow->xres.other_weights = wtmp[j];

	sprintf (res_output, "%s %s ux %g %g %g %g", sta, phase,
		 btmp[j], rtmp[j], wtmp[j], reswttmp[j]);
	stmp = strdup (res_output);
	pushtbl (*restbl, stmp);

	slow->yres.weighted_residual = btmp[j + 1];
	slow->yres.raw_residual = rtmp[j + 1];
	slow->yres.residual_weight = reswttmp[j + 1];
	slow->yres.other_weights = wtmp[j + 1];

	sprintf (res_output, "%s %s uy %g %g %g %g", sta, phase,
		 btmp[j + 1], rtmp[j + 1], wtmp[j + 1], reswttmp[j + 1]);
	stmp = strdup (res_output);
	pushtbl (*restbl, stmp);
    }

    free_matrix ((char **) A, 0, m - 1, 0);
    free_matrix ((char **) U, 0, m - 1, 0);
    free_matrix ((char **) V, 0, 3, 0);
    free (b);
    free (r);
    free (w);
    free (reswt);
    free (work);
    free (btmp);
    free (rtmp);
    free (wtmp);
    free (reswttmp);
    return (ret_code);
}

/* This group of routines return grid points for different shaped regions
that can be used in a grid search algorithm.  All are passed a parameter
file pf pointer to get control information, and all return an array of
points.   */


/* The first one is a dumb, simple grid in latitude and longitude that
won't work properly near the poles.  In that case, it is better to use
the radial region module that follows.  Required parameter from the pf
file are:
	center_latitude, center_longitude, center_depth = coordinates of
		the center of the grid
	latitude_range, longitude_range, depth_range = full range, not 1/2
		range.  Depths above 0 are not allowed by this algorithm,
		and depths will be skewed if necessary.
	nlat, nlon, ndepths = points in each direction

NONE of the above are defaulted.  If any errors occur, the function
returns a nonzero value */

int 
lat_lon_grid_setup (Pf * pf, Point ** pts, int *gridsize)
{
    char           *s;		       /* string buffer */
    char            err[80] = "lat_lon_grid_setup can't find required parameter = ";
    double          center_latitude,
                    center_longitude,
                    center_depth;
    double          latitude_range,
                    longitude_range,
                    depth_range;
    int             nlat,
                    nlon,
                    ndepths;
    double          lat0,
                    lon0,
                    z0;
    double          dlat,
                    dlon,
                    dz;
    Point          *p;

    int             i,
                    j,
                    k,
                    n;		       /* counters */

    if ((s = pfget_string (pf, "center_latitude")) == NULL) {
	elog_log(1, "%s%s\n", err, "center_latitude");
	return (-1);
    }
    center_latitude = pfget_double (pf, "center_latitude");
    if ((s = pfget_string (pf, "center_longitude")) == NULL) {
	elog_log(1, "%s%s\n", err, "center_longitude");
	return (0);
    }
    center_longitude = pfget_double (pf, "center_longitude");
    if ((s = pfget_string (pf, "center_depth")) == NULL) {
	elog_log(1, "%s%s\n", err, "center_depth");
	return (-1);
    }
    center_depth = pfget_double (pf, "center_depth");
    if ((s = pfget_string (pf, "latitude_range")) == NULL) {
	elog_log(1, "%s%s\n", err, "latitude_range");
	return (-1);
    }
    latitude_range = pfget_double (pf, "latitude_range");
    if ((s = pfget_string (pf, "longitude_range")) == NULL) {
	elog_log(1, "%s%s\n", err, "longitude_range");
	return (-1);
    }
    longitude_range = pfget_double (pf, "longitude_range");
    if ((s = pfget_string (pf, "depth_range")) == NULL) {
	elog_log(1, "%s%s\n", err, "depth_range");
	return (-1);
    }
    depth_range = pfget_double (pf, "depth_range");
    if ((s = pfget_string (pf, "nlat")) == NULL) {
	elog_log(1, "%s%s\n", err, "nlat");
	return (-1);
    }
    nlat = pfget_int (pf, "nlat");
    if ((s = pfget_string (pf, "nlon")) == NULL) {
	elog_log(1, "%s%s\n", err, "nlon");
	return (-1);
    }
    nlon = pfget_int (pf, "nlon");
    if ((s = pfget_string (pf, "ndepths")) == NULL) {
	elog_log(1, "%s%s\n", err, "ndepths");
	return (-1);
    }
    ndepths = pfget_int (pf, "ndepths");
    lat0 = center_latitude - latitude_range / 2.0;
    dlat = latitude_range / ((double) nlat);
    lon0 = center_longitude - longitude_range / 2.0;
    dlon = longitude_range / ((double) nlon);
    if (ndepths <= 1)
	z0 = center_depth;
    else
	z0 = center_depth - depth_range / 2.0;
    if (z0 < 0.0) {
	elog_log(0, "Warning (lat_lon_grid_setup:  depth grid range reset to start at 0.0\n");
	z0 = 0.0;
    }
    if (ndepths <= 1)
	dz = 0.0;
    else
	dz = depth_range / ((double) (ndepths - 1));
    *gridsize = nlat * nlon * ndepths;
    /* the following simplifies indirection in the code, but assures we
     * return a proper pointer */
    *pts = (Point *) calloc (*gridsize, sizeof (Point));
    p = *pts;

    for (k = 0, n = 0; k < ndepths; ++k)
	for (j = 0; j < nlon; ++j)
	    for (i = 0; i < nlat; ++i) {
		p[n].lat = (double) i *dlat + lat0;
		if (p[n].lat > 180.0)
		    p[n].lat -= 360.0;
		if (p[n].lat < -180.0)
		    p[n].lat += 360.0;
		p[n].lon = (double) j *dlon + lon0;
		if ((p[n].lat > 90.0)
			|| (p[n].lat < -90.0)) {
		    free (p);
		    elog_log(1, "lat_lon_grid_setup[n]. cannot form grid over poles\n");
		    return (-1);
		}
		p[n].z = (double) k *dz + z0;
		++n;
	    }
    return (0);
}

/* This is a similar routine that defines an annular region around
a specified lat-lon point.  */
int 
radial_grid_setup (Pf * pf, Point ** pts, int *gridsize)
{
    char           *s;		       /* string buffer */
    char            err[80] = "radial_grid_setup can't find required parameter = ";
    double          center_latitude,
                    center_longitude,
                    center_depth;
    double          depth_range;
    double          rmin,
                    rmax;
    double          phimin,
                    phimax;
    int             nr,
                    nphi,
                    ndepths;
    double          z0;
    double          dr,
                    dphi,
                    dz;
    double          distance;
    Point          *p;

    int             i,
                    j,
                    k,
                    n;		       /* counters */
    if ((s = pfget_string (pf, "center_latitude")) == NULL) {
	elog_log(1, "%s%s\n", err, "center_latitude");
	return (-1);
    }
    center_latitude = pfget_double (pf, "center_latitude");
    if ((s = pfget_string (pf, "center_longitude")) == NULL) {
	elog_log(1, "%s%s\n", err, "center_longitude");
	return (-1);
    }
    center_longitude = pfget_double (pf, "center_longitude");
    if ((s = pfget_string (pf, "center_depth")) == NULL) {
	elog_log(1, "%s%s\n", err, "center_depth");
	return (-1);
    }
    center_depth = pfget_double (pf, "center_depth");
    if ((s = pfget_string (pf, "minimum_distance")) == NULL) {
	elog_log(1, "%s%s\n", err, "minimum_distance");
	return (-1);
    }
    rmin = pfget_double (pf, "minimum_distance");
    if ((s = pfget_string (pf, "maximum_distance")) == NULL) {
	elog_log(1, "%s%s\n", err, "maximum_distance");
	return (-1);
    }
    rmax = pfget_double (pf, "maximum_distance");
    if ((s = pfget_string (pf, "minimum_azimuth")) == NULL) {
	elog_log(1, "%s%s\n", err, "minimum_azimuth");
	return (-1);
    }
    phimin = pfget_double (pf, "minimum_azimuth");
    if ((s = pfget_string (pf, "maximum_azimuth")) == NULL) {
	elog_log(1, "%s%s\n", err, "maximum_azimuth");
	return (-1);
    }
    phimax = pfget_double (pf, "maximum_azimuth");
    if ((s = pfget_string (pf, "depth_range")) == NULL) {
	elog_log(1, "%s%s\n", err, "depth_range");
	return (-1);
    }
    depth_range = pfget_double (pf, "depth_range");
    if ((s = pfget_string (pf, "number_points_r")) == NULL) {
	elog_log(1, "%s%s\n", err, "number_points_r");
	return (-1);
    }
    nr = pfget_int (pf, "number_points_r");
    if ((s = pfget_string (pf, "number_points_azimuth")) == NULL) {
	elog_log(1, "%s%s\n", err, "number_points_azimuth");
	return (-1);
    }
    nphi = pfget_int (pf, "number_points_azimuth");
    if ((s = pfget_string (pf, "ndepths")) == NULL) {
	elog_log(1, "%s%s\n", err, "ndepths");
	return (-1);
    }
    ndepths = pfget_int (pf, "ndepths");
    /* error checks that yield a null return -- no recovery */
    if ((rmax < rmin) || (rmax <= 0.0) || (rmin <= 0.0)) {
	elog_log(1, "radial_grid_setup:  illegal specification of radii %g to %g\n",
			rmin, rmax);
	return (-1);
    }
    if (((phimax - phimin) > 360.0) || (phimin > phimax)) {
	elog_log(1, "radial_grid_setup:  illegal specification of azimuthal angles %g to %g\n",
			phimin, phimax);
	return (-1);
    }
    if (nphi <= 0)
	dphi = 0.0;
    else {
	/* This is needed because otherwise we would hit 0.0 azimuth twice
	 * when a full circle is specified. */
	if (difference_is_zero ((float) phimin, 0.0)
		&& difference_is_zero ((float) phimax, 360.0))
	    dphi = (phimax - phimin) / ((double) (nphi));
	else
	    dphi = (phimax - phimin) / ((double) (nphi - 1));
    }
    if (nr <= 1)
	dr = 0.0;
    else
	dr = (rmax - rmin) / ((double) (nr - 1));
    if (ndepths <= 1)
	z0 = center_depth;
    else
	z0 = center_depth - depth_range / 2.0;
    if (z0 < 0.0) {
	elog_log(0, "Warning (lat_lon_grid_setup:  depth grid range reset to start at 0.0\n");
	z0 = 0.0;
    }
    if (ndepths <= 1)
	dz = 0.0;
    else
	dz = depth_range / ((double) (ndepths - 1));
    /* We need to convert the center point to radians */
    center_latitude = rad (center_latitude);
    center_longitude = rad (center_longitude);
    *gridsize = nphi * nr * ndepths;
    *pts = (Point *) calloc (*gridsize, sizeof (Point));
    p = *pts;

    for (k = 0, n = 0; k < ndepths; ++k)
	for (j = 0; j < nr; ++j) {
	    distance = rmin + dr * ((double) j);
	    distance /= RADIUS_EARTH;  /* conversion to radians */
	    for (i = 0; i < nphi; ++i) {
		double          rlat,
		                rlon;  /* hold point in radians */
		double          azimuth;
		azimuth = rad (phimin + dphi * ((double) i));
		latlon (center_latitude, center_longitude,
			distance, azimuth, &rlat, &rlon);
		/* It is not efficient to convert these to degrees, but it is
		 * more consistent with everything else to keep these in
		 * degrees */

		p[n].lat = deg (rlat);
		p[n].lon = deg (rlon);
		p[n].z = (double) k *dz + z0;
		++n;
	    }
	}
    return (0);
}

/* general purpose grid search location procedure.

	attbl - arrival table
	utbl - slowness table
	grid - vector of Point structure elements of node points for
		grid search
	ngrid - number of points in grid
	use_raw - if nonzero, use raw residuals, if zero use weighted residuals
	options - passed to makeqn function (control)

This version works by simply working through the grid of points
and determining the point in the grid with the minimum rms residual.
This always works with a "recenter" algorithm in which the origin
time is effectively set as the median of the residuals.
The only option unique to this program is a switch to
use the raw or weighted residuals.  (Weighted here means distance
and arrival precision weights.  Residual weights are currently ignored. )
The procedure returns a Hypocenter structure with some
elements not filled in, but all unused values are set to 0.0

Author:  Gary L. Pavlis
Written:  November 1996
Modification:
	November 2002
Discovered a parasitic case that caused problems with the original
method used to set the origin time.  Previously the grid search let
the origin time float with the median of the residuals.  That worked
fine until I added the code to automatically handle timing problems
by automatically switchin to S-P for problem stations.  When more
stations had a timing problem than not, this algorithm failed because
the origin time got strongly distorted bouncing around in the grid
causing erratic behaviour.

Changed to use the first arrival in the input list as a reference.
The computed travel time for this station is subtracted from the
that time and used as an origin time estimate.  This actually
may give better results anyway as the floating reference used
before may have unintentionally introduced local minima.
*/

Hypocenter 
gridloc (Tbl * attbl, Tbl * utbl, Point * grid, int ngrid,
	 int use_raw, Location_options options)
{
    Hypocenter      hypo;
    int             natimes;
    int             nslow;
    float          *b;		       /* weighted residual vector (rhs of
				        * Ax=b) */
    float          *r,
                   *w;		       /* r=raw residual and w= vector of
				        * total weights */
    float          *reswt;	       /* we store residual weights
				        * seperately to calculate effective
				        * degrees of freedom */
    float          *work;
    int             i,
                    j;
    double          rms,
                    wrms;
    Robust_statistics statistics,
                    time_statistics;
    Arrival        *a;
    int             m,
                    nused;
    double          rmsmin;
    int             imin;
    double          dt;		       /* adjustment to base time for final
				        * solution */
    double          time_ref;	       /* global time reference grabbed from
				        * first entry */

    natimes = maxtbl (attbl);
    nslow = maxtbl (utbl);
    m = 2 * nslow + natimes;

    /* Initialize the Hypocenter structure */
    hypo.dx = 0.0;
    hypo.dy = 0.0;
    hypo.dz = 0.0;
    hypo.lat0 = 0.0;
    hypo.lon0 = 0.0;
    hypo.z0 = 0.0;
    hypo.t0 = 0.0;
    hypo.dx = 0.0;
    hypo.dx = 0.0;

    if ((natimes <= 0) || (ngrid <= 0)) {
	elog_log(1, "gridloc:  unrecoverable error.  Illegal input.\n");
	hypo.lat = 0.0;
	hypo.lon = 0.0;
	hypo.z = 0.0;
	hypo.time = 0.0;
	return (hypo);
    }
    b = (float *) calloc (m, sizeof (float));
    r = (float *) calloc (m, sizeof (float));
    w = (float *) calloc (m, sizeof (float));
    reswt = (float *) calloc (m, sizeof (float));
    work = (float *) calloc (m, sizeof (float));

    /* After all those allocs we need to check if it all worked */
    if ((b == NULL) || (r == NULL) || (reswt == NULL)
	    || (w == NULL) || (work == NULL))
	elog_die(1, "Alloc error in grid location function\n");

    /* Since we are working with a copy of the options structure, we will
     * just alter the control parameters we need that assure the
     * form_equations procedure behaves as we want it to.  */
    for (i = 0; i < 4; ++i)
	options.fix[i] = 0;
    options.atime_residual_weight = NONE;
    options.atime_distance_weight = 0;
    options.slow_residual_weight = NONE;
    options.slow_distance_weight = 0;

    /* Grab the first arrival time listed in the table to get a first order
     * origin time within the ballpark.  */
    a = (Arrival *) gettbl (attbl, 0);
    hypo.time = a->time;
    time_ref = a->time;
    /* This number should be huge enough to avoid this being the minimum rms
     * unless something is really bad. */
    rmsmin = 1.0e100;
    imin = -1;

    for (i = 0; i < ngrid; ++i) {
	float         **dummy = 0;
	Travel_Time_Function_Output tto;
	int             reset_ot;

	hypo.lat = grid[i].lat;
	hypo.lon = grid[i].lon;
	hypo.z = grid[i].z;
	hypo.time = time_ref;
	/* Do this to reset the time reference */
	tto = calculate_travel_time (*a, hypo, RESIDUALS_ONLY);
	if (tto.time == TIME_INVALID) {
	    reset_ot = 1;
	    elog_complain(1, "gridloc:  travel time calculator failed in computing reference time\nThis may cause convergence problems\n");
	} else {
	    reset_ot = 0;
	    hypo.time -= tto.time;
	}
	statistics = form_equations (RESIDUALS_ONLY, hypo,
				     attbl, utbl,
				     options, dummy, b, r, w, reswt, &nused);
	for (j = 0; j < natimes; ++j)
	    work[j] = r[j];
	time_statistics = calc_statistics (work, nused);
	if (reset_ot) {
	    for (j = 0; j < natimes; ++j) {
		r[j] -= time_statistics.median;
		b[j] = r[j] * w[j] * reswt[j];
	    }
	}
	if (use_raw) {
	    rms = calculate_rms (r, m);
	    /* rescale if nused != narrival */
	    if (nused < natimes)
		rms *= (double) m / ((double) (m - (natimes - nused)));
	    if (rms < rmsmin) {
		rmsmin = rms;
		imin = i;
		if (reset_ot)
		    dt = time_statistics.median;
		else
		    dt = tto.time + time_statistics.median;
		hypo.interquartile =
		    time_statistics.q3_4
		    - time_statistics.q1_4;
	    }
	} else {
	    wrms = calculate_weighted_rms (b, w, reswt, m);
	    if (wrms < rmsmin) {
		rmsmin = wrms;
		imin = i;
		if (reset_ot)
		    dt = time_statistics.median;
		else
		    dt = tto.time + time_statistics.median;
		hypo.interquartile =
		    time_statistics.q3_4
		    - time_statistics.q1_4;
	    }
	}
    }
    hypo.time = time_ref - dt;
    hypo.lat = grid[imin].lat;
    hypo.lon = grid[imin].lon;
    hypo.z = grid[imin].z;
    if (use_raw) {
	hypo.rms_raw = rmsmin;
	hypo.rms_weighted = 0.0;
    } else {
	hypo.rms_raw = 0.0;
	hypo.rms_weighted = rmsmin;
    }

    free (b);
    free (r);
    free (w);
    free (reswt);
    free (work);

    return (hypo);
}

/* $Id$ */
