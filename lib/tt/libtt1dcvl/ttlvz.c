/* ttlvz.f -- translated by f2c (version 19970219).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#define abs(x) ((x) >= 0 ? (x) : -(x))
#define dabs(x) (double)abs(x)
#define max(a,b) ((a) >= (b) ? (a) : (b))

/* Subroutine */ int ttlvz_(delta, hpz, n, v, z__, h__, term, t, rayp, up)
double *delta, *hpz;
int *n;
double *v, *z__, *h__, *term, *t, *rayp;
int *up;
{
    /* System generated locals */
    int i__1, i__2;
    double d__1, d__2;

    /* Builtin functions */
    double sqrt(), atan2(), sin();

    /* Local variables */
    static double sdel;
    static int jlim;
    static double sder, pmax, temp;
    static double vmax;
    static int lhpz;
    static double test;
    static double dmdp0;
    static double term1;
    static int j;
    static double p;
    static int count, lowlr;
    static double t1;
    static int istrt;
    static double offset, sum;
    static double hyz;

/*-- ttlvz calculates travel time of seismic wave from hypocenter       tt
lvz.3*/
/*-- earth model is plane parallel layers (n-1 layers over halfspace)   tt
lvz.4*/
/*-- n is number of layers including halfspace                          tt
lvz.5*/
/*-- v is array of length n containing layer velocities                 tt
lvz.6*/
/*-- z is array of length n containing depths to layer tops             tt
lvz.7*/
/*-- this routine is designed to work for all layer velocity combinationtt
lvz.8*/

/*  original code contained fixed size arrays set as follows */

/*    dimension h(40),term(40)                                          tt
lvz.9*/
/*    common /model/ n,v(20),z(20)                                      tt
lvz.10*/

/*  In this modification I pass the same variables in as arguments */
/*  I've also made all quantities double.  I also modified the original */
/*  code to now return the ray parameter as well as the travel time. */
/* The original ttlvz code was a fortran function.  Now it is a subroutine
.*/
/*  returns time in t and ray parameter in argument p.  I also added a */
/* integer variable "up".  Up is 1 when the ray is a direct ray.  otherwis
e*/
/*  it will be 0. */

/*  Original code came, I think, from Robert Crosson.  Modification */
/*  by G Pavlis, August 1996 */
/*  Editorial note:  I should have burned this and started from scratch. 
*/

/*  August 2002 */
/*  Another repair to deal with case described above.  Roundoff errors */
/* still occasionally caused problems.  Used an approximate fix described 
below.*/
/*  also added a special exit for delta=0,hpz=0 case. */


/*  test is used as iteration distance cutoff for upward traveling */
/*  ray convergence.  To avoid infinite loops, especially in the */
/*  presence of large delta where roundoff errors become a serious */
/*  problem we use a maxit parameter to limit total iterations */
/*  and a relative distance scale factor rdsf.  rdsf rescales */
/*  the convergence variable test for large delta.  See code below */
/*  Modified by glp:  August 1996 */


/* return immediately for 0 0 request */

    /* Parameter adjustments */
    --term;
    --h__;
    --z__;
    --v;

    /* Function Body */
    if (*delta <= 0. && *hpz == 0.) {
	*t = 0.;
	*rayp = 0.;
	*up = 1;
	return 0;
    }
    test = .001;
    if (*delta * 1e-5 > test) {
	test = *delta * 1e-5;
    }
    *up = 0;
    count = 0;
/*-- locate layer containing hypocenter and store index in lhpz         tt
lvz.12*/
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
	if (*hpz <= z__[j]) {
	    goto L111;
	}
/* L110: */
    }
    lhpz = *n;
    goto L112;
L111:
    lhpz = j - 1;
    if (lhpz == 0) {
	lhpz = 1;
    }
L112:
/*-- assign internal depths to layer tops including correction for      tt
lvz.21*/
/*   station elevation                                                  tt
lvz.22*/
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
	if ((i__2 = j - lhpz) < 0) {
	    goto L121;
	} else if (i__2 == 0) {
	    goto L122;
	} else {
	    goto L123;
	}
L121:
	h__[j] = z__[j + 1] - z__[j];
	goto L120;
L122:
	h__[j] = (d__1 = *hpz - z__[j], abs(d__1));
	goto L120;
L123:
	h__[j] = z__[j] - z__[j - 1];
L120:
	;
    }
    *t = 1e10;
    *rayp = 0.;
    if (lhpz == *n) {
	goto L360;
    }
    h__[lhpz + 1] = z__[lhpz + 1] - max(*hpz,z__[1]);
/*-- calculate smallest refracted wave time                             tt
lvz.34*/
    istrt = lhpz + 1;
    vmax = 0.;
    i__1 = lhpz;
    for (j = 1; j <= i__1; ++j) {
/* L760: */
/* Computing MAX */
	d__1 = v[j];
	vmax = max(d__1,(double) vmax);
    }
    i__1 = *n;
    for (lowlr = istrt; lowlr <= i__1; ++lowlr) {
/*-- check to see if ray exists                                       
  ttlvz.40*/
	if (v[lowlr] <= vmax) {
	    goto L190;
	}
	vmax = v[lowlr];
	jlim = lowlr - 1;
/*-- calculate offset distance                                        
  ttlvz.44*/
	p = 1. / v[lowlr];
	sum = 0.;
	i__2 = lhpz;
	for (j = 1; j <= i__2; ++j) {
/* Computing 2nd power */
	    d__1 = p * v[j];
	    term[j] = sqrt(1. - d__1 * d__1) + 1e-10;
/* L710: */
	    sum = h__[j] * v[j] / term[j] + sum;
	}
	i__2 = jlim;
	for (j = lhpz; j <= i__2; ++j) {
/* Computing 2nd power */
	    d__1 = p * v[j];
	    term[j] = sqrt(1. - d__1 * d__1) + 1e-10;
/* L720: */
	    sum = h__[j + 1] * 2. * v[j] / term[j] + sum;
	}
	offset = sum * p;
	if (offset - *delta <= 0.) {
	    goto L780;
	} else {
	    goto L190;
	}
/*-- calculate refraction path travel time for lowlr                  
  ttlvz.55*/
L780:
	sum = 0.;
	i__2 = lhpz;
	for (j = 1; j <= i__2; ++j) {
/* L730: */
	    sum = h__[j] * term[j] / v[j] + sum;
	}
	i__2 = jlim;
	for (j = lhpz; j <= i__2; ++j) {
/* L740: */
	    sum = h__[j + 1] * 2. * term[j] / v[j] + sum;
	}
	t1 = *delta * p + sum;
	if (t1 < *t) {
	    *t = t1;
	    *rayp = p;
	}
L190:
	;
    }

/*  special case for source in the first layer */

L360:
    if (lhpz == 1) {
/* Computing 2nd power */
	d__1 = *delta;
/* Computing 2nd power */
	d__2 = *hpz - z__[1];
	t1 = sqrt(d__1 * d__1 + d__2 * d__2) / v[1];
	p = sin(atan2(*delta, (d__1 = *hpz - z__[1], abs(d__1)))) / v[1];
	if (*hpz < z__[1]) {
	    p = -p;
	}
    } else {
/*-- calculate direct wave travel time                                
  ttlvz.64*/
	vmax = v[1];
	i__1 = lhpz;
	for (j = 2; j <= i__1; ++j) {
/* L175: */
/* Computing MAX */
	    d__1 = vmax, d__2 = v[j];
	    vmax = max(d__1,d__2);
	}
/* -- This loop seeks a ray parameter for a direct ray with */
/* -- delta > given distance.  The earlier version would sometimes ent
er */
/* -- an infinite loop if the given delta was large due to a machine 
*/
/* -- precision limitation.  We trap this now with a test for */
/* -- the condition that p == pmax.  In this condition, an error is */
/* -- returned that has to be handled by the caller.  Here this is */
/* -- signaled by setting the returned time to -1.0. */
/* -- */
/*-- Repaired August 2002.  This process sometimes failed when request
ing an*/
/*-- extremely large delta in some models.  This is a roundoff error p
roblem*/
/*-- caused by the situation of p==pmax, which makes the sqrt term bel
ow go*/
/*-- to zero.  To return a reasonable approximation of a valid travel 
time*/
/*-- in that situation we assume the time in layer vmax dominates so w
e just*/
/* -- compute this as r/vmax and return as if it were perfectly normal
. */
/* -- consequences are not yet clear. */
	pmax = 1. / vmax;
	p = pmax * .5;
L155:
	p = (p + pmax) / 2.;
	if (p >= pmax) {
	    *t = sqrt(*delta * *delta + hyz * hyz) / vmax;
	    return 0;
	}
	sdel = 0.;
	i__1 = lhpz;
	for (j = 1; j <= i__1; ++j) {
/* L160: */
/* Computing 2nd power */
	    d__1 = p * v[j];
	    sdel = v[j] * h__[j] / sqrt(1. - d__1 * d__1) + sdel;
	}
	if ((d__1 = *delta - p * sdel) < 0.) {
	    goto L166;
	} else if (d__1 == 0) {
	    goto L161;
	} else {
	    goto L155;
	}
/*-- now perform newton convergence from top down                     
  ttlvz.77*/
L166:
	sdel = 0.;
	sder = 0.;
	i__1 = lhpz;
	for (j = 1; j <= i__1; ++j) {
/* Computing 2nd power */
	    d__1 = p * v[j];
	    temp = sqrt(1. - d__1 * d__1);
	    term1 = v[j] * h__[j] / temp;
	    sdel = term1 + sdel;
/* Computing 2nd power */
	    d__1 = temp;
	    sder = term1 / (d__1 * d__1) + sder;
/* L162: */
	}
	dmdp0 = *delta - p * sdel;

/*  a warning should be issued if the count criteria breaks */
/*  this loop, but for use in a C program this isn't worth the mess */
/*  it causes.  It should not happen anyway, but it is always */
/*  necessary to avoid infinite loops like this. */

	if (dabs(dmdp0) < test || count > 50) {
	    goto L161;
	}
	p = dmdp0 / sder + p;
	++count;
	goto L166;
L161:
/*-- p has been determined to sufficient accuracy                     
  ttlvz.91*/
/*-- calculate direct wave travel time by summation                   
  ttlvz.92*/
	sum = 0.;
	i__1 = lhpz;
	for (j = 1; j <= i__1; ++j) {
/* L180: */
/* Computing 2nd power */
	    d__1 = p * v[j];
	    sum = h__[j] / (v[j] * sqrt(1. - d__1 * d__1)) + sum;
	}
	t1 = sum;
    }
    if (t1 < *t) {
	*t = t1;
	*rayp = p;
	*up = 1;
    }
    return 0;
} /* ttlvz_ */

