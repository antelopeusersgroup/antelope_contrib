#include "blaswrap.h"
#include "f2c.h"

/* Subroutine */ int dlarrb_(integer *n, doublereal *d__, doublereal *l, 
	doublereal *ld, doublereal *lld, integer *ifirst, integer *ilast, 
	doublereal *sigma, doublereal *reltol, doublereal *w, doublereal *
	wgap, doublereal *werr, doublereal *work, integer *iwork, integer *
	info)
{
/*  -- LAPACK auxiliary routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       June 30, 1999   


    Purpose   
    =======   

    Given the relatively robust representation(RRR) L D L^T, DLARRB   
    does ``limited'' bisection to locate the eigenvalues of L D L^T,   
    W( IFIRST ) thru' W( ILAST ), to more accuracy. Intervals   
    [left, right] are maintained by storing their mid-points and   
    semi-widths in the arrays W and WERR respectively.   

    Arguments   
    =========   

    N       (input) INTEGER   
            The order of the matrix.   

    D       (input) DOUBLE PRECISION array, dimension (N)   
            The n diagonal elements of the diagonal matrix D.   

    L       (input) DOUBLE PRECISION array, dimension (N-1)   
            The n-1 subdiagonal elements of the unit bidiagonal matrix L.   

    LD      (input) DOUBLE PRECISION array, dimension (N-1)   
            The n-1 elements L(i)*D(i).   

    LLD     (input) DOUBLE PRECISION array, dimension (N-1)   
            The n-1 elements L(i)*L(i)*D(i).   

    IFIRST  (input) INTEGER   
            The index of the first eigenvalue in the cluster.   

    ILAST   (input) INTEGER   
            The index of the last eigenvalue in the cluster.   

    SIGMA   (input) DOUBLE PRECISION   
            The shift used to form L D L^T (see DLARRF).   

    RELTOL  (input) DOUBLE PRECISION   
            The relative tolerance.   

    W       (input/output) DOUBLE PRECISION array, dimension (N)   
            On input, W( IFIRST ) thru' W( ILAST ) are estimates of the   
            corresponding eigenvalues of L D L^T.   
            On output, these estimates are ``refined''.   

    WGAP    (input/output) DOUBLE PRECISION array, dimension (N)   
            The gaps between the eigenvalues of L D L^T. Very small   
            gaps are changed on output.   

    WERR    (input/output) DOUBLE PRECISION array, dimension (N)   
            On input, WERR( IFIRST ) thru' WERR( ILAST ) are the errors   
            in the estimates W( IFIRST ) thru' W( ILAST ).   
            On output, these are the ``refined'' errors.   

   ****Reminder to Inder --- WORK is never used in this subroutine *****   
    WORK    (input) DOUBLE PRECISION array, dimension (???)   
            Workspace.   

    IWORK   (input) INTEGER array, dimension (2*N)   
            Workspace.   

   ****Reminder to Inder --- INFO is never set in this subroutine ******   
    INFO    (output) INTEGER   
            Error flag.   

    Further Details   
    ===============   

    Based on contributions by   
       Inderjit Dhillon, IBM Almaden, USA   
       Osni Marques, LBNL/NERSC, USA   

    =====================================================================   


       Parameter adjustments */
    /* System generated locals */
    integer i__1, i__2, i__3;
    doublereal d__1, d__2;
    /* Local variables */
    static integer neig;
    static doublereal left;
    static integer nint;
    static doublereal pert;
    static integer i__, j, k;
    static doublereal s, delta, right, width;
    static integer i1, i2, initi1, initi2;
    extern doublereal dlamch_(char *);
    static integer nright, ncnvrg;
    static doublereal thresh;
    static integer olnint;
    static doublereal gap, mid;
    static integer cnt;
    static doublereal eps, tmp;

    --iwork;
    --work;
    --werr;
    --wgap;
    --w;
    --lld;
    --ld;
    --l;
    --d__;

    /* Function Body */
    eps = dlamch_("Precision");
    i1 = *ifirst;
    i2 = *ifirst;
    neig = *ilast - *ifirst + 1;
    ncnvrg = 0;
    thresh = *reltol;
    i__1 = *ilast;
    for (i__ = *ifirst; i__ <= i__1; ++i__) {
	iwork[i__] = 0;
	pert = eps * (abs(*sigma) + (d__1 = w[i__], abs(d__1)));
	werr[i__] += pert;
	if (wgap[i__] < pert) {
	    wgap[i__] = pert;
	}
/* L10: */
    }
    i__1 = *ilast;
    for (i__ = i1; i__ <= i__1; ++i__) {
	if (i__ == 1) {
	    gap = wgap[i__];
	} else if (i__ == *n) {
	    gap = wgap[i__ - 1];
	} else {
/* Computing MIN */
	    d__1 = wgap[i__ - 1], d__2 = wgap[i__];
	    gap = min(d__1,d__2);
	}
	if (werr[i__] < thresh * gap) {
	    ++ncnvrg;
	    iwork[i__] = 1;
	    if (i1 == i__) {
		++i1;
	    }
	} else {
	    i2 = i__;
	}
/* L20: */
    }

/*     Initialize the unconverged intervals. */

    i__ = i1;
    nint = 0;
    right = 0.;
L30:
    if (i__ <= i2) {
	if (iwork[i__] == 0) {
	    delta = eps;
	    left = w[i__] - werr[i__];

/*           Do while( CNT(LEFT).GT.I-1 ) */

L40:
	    if (i__ > i1 && left <= right) {
		left = right;
		cnt = i__ - 1;
	    } else {
		s = -left;
		cnt = 0;
		i__1 = *n - 1;
		for (j = 1; j <= i__1; ++j) {
		    tmp = d__[j] + s;
		    s = s * (ld[j] / tmp) * l[j] - left;
		    if (tmp < 0.) {
			++cnt;
		    }
/* L50: */
		}
		tmp = d__[*n] + s;
		if (tmp < 0.) {
		    ++cnt;
		}
		if (cnt > i__ - 1) {
		    delta *= 2.;
		    left -= (abs(*sigma) + abs(left)) * delta;
		    goto L40;
		}
	    }
	    delta = eps;
	    right = w[i__] + werr[i__];

/*           Do while( CNT(RIGHT).LT.I ) */

L60:
	    s = -right;
	    cnt = 0;
	    i__1 = *n - 1;
	    for (j = 1; j <= i__1; ++j) {
		tmp = d__[j] + s;
		s = s * (ld[j] / tmp) * l[j] - right;
		if (tmp < 0.) {
		    ++cnt;
		}
/* L70: */
	    }
	    tmp = d__[*n] + s;
	    if (tmp < 0.) {
		++cnt;
	    }
	    if (cnt < i__) {
		delta *= 2.;
		right += (abs(*sigma) + abs(right)) * delta;
		goto L60;
	    }
	    werr[i__] = left;
	    w[i__] = right;
	    iwork[*n + i__] = cnt;
	    ++nint;
	    i__ = cnt + 1;
	} else {
	    ++i__;
	}
	goto L30;
    }

/*     While( NCNVRG.LT.NEIG ) */

    initi1 = i1;
    initi2 = i2;
L80:
    if (ncnvrg < neig) {
	olnint = nint;
	i__ = i1;
	i__1 = olnint;
	for (k = 1; k <= i__1; ++k) {
	    nright = iwork[*n + i__];
	    if (iwork[i__] == 0) {
		mid = (werr[i__] + w[i__]) * .5;
		s = -mid;
		cnt = 0;
		i__2 = *n - 1;
		for (j = 1; j <= i__2; ++j) {
		    tmp = d__[j] + s;
		    s = s * (ld[j] / tmp) * l[j] - mid;
		    if (tmp < 0.) {
			++cnt;
		    }
/* L90: */
		}
		tmp = d__[*n] + s;
		if (tmp < 0.) {
		    ++cnt;
		}
/* Computing MAX */
		i__2 = i__ - 1, i__3 = min(nright,cnt);
		cnt = max(i__2,i__3);
		if (i__ == nright) {
		    if (i__ == *ifirst) {
			gap = werr[i__ + 1] - w[i__];
		    } else if (i__ == *ilast) {
			gap = werr[i__] - w[i__ - 1];
		    } else {
/* Computing MIN */
			d__1 = werr[i__ + 1] - w[i__], d__2 = werr[i__] - w[
				i__ - 1];
			gap = min(d__1,d__2);
		    }
		    width = w[i__] - mid;
		    if (width < thresh * gap) {
			++ncnvrg;
			iwork[i__] = 1;
			if (i1 == i__) {
			    ++i1;
			    --nint;
			}
		    }
		}
		if (iwork[i__] == 0) {
		    i2 = k;
		}
		if (cnt == i__ - 1) {
		    werr[i__] = mid;
		} else if (cnt == nright) {
		    w[i__] = mid;
		} else {
		    iwork[*n + i__] = cnt;
		    ++nint;
		    werr[cnt + 1] = mid;
		    w[cnt + 1] = w[i__];
		    w[i__] = mid;
		    i__ = cnt + 1;
		    iwork[*n + i__] = nright;
		}
	    }
	    i__ = nright + 1;
/* L100: */
	}
	nint = nint - olnint + i2;
	goto L80;
    }
    i__1 = initi2;
    for (i__ = initi1; i__ <= i__1; ++i__) {
	w[i__] = (werr[i__] + w[i__]) * .5;
	werr[i__] = w[i__] - werr[i__];
/* L110: */
    }

    return 0;

/*     End of DLARRB */

} /* dlarrb_ */

