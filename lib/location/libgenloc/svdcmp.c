/* Usage:  svdcmp( float **a, int m, int n, float *w, float **v)

Singular value decomposition function modified from a code originally 
published in "Numerical Recipes in C - The Art of Scientific Computing".
Arguments:

	a - m by n matrix for which svd is to be computed.  a is overwritten
		by the left singular vectors on return.
	w - vector of singular values (unsorted).
	v - (returned) matrix of right singular vectors.  These are retured in
		the rows of v (i.e. this really is the matrix V of the SVD).

This code differs from the original numerical recipes code in several ways:
1.  It uses normal C convention that indexes beginning with 0 not 1
2.  The numerical recipes code requires a seperate matrix for A and U.
3.  It fixes a bug in the original code that causes the original code to 
    form an infinite loop with optimizing compilers (see comments in code below)
4.  Minor interface changes to adapt the code to Solaris and Dan Quinlan method
    of error handling.
5.  Uses a macro PYTHAG in place of dpythag function (should optimize better)
6.  Uses a different SIGN macro from numerical recipes = that in stock.h.


Major modification by Robert Pavlis, Department of Chemistry, Pittsburg
State University, Pittsburg, Kansas.  
Adapted for earthquake location code by Gary Pavlis, Indiana University.
*/


/* the constant FLT_EPSILON is need that is defined in float.h in Solaris 5.5 
Original code used a different constant that was changed here.  */
#include <float.h>

/* other standard include files */
#include <stdlib.h>
#include <math.h>
/* DSAP error handling code defined here.  We also use the MAX and SIGN 
macros that are defined here. */
#include "stock.h"
/* This macro replaces dpythag in numerical recipes code */
#define PYTHAG(a,b) ((fabs((double)a))>(fabs((double)b))?\
    (fabs((double)a)*sqrt((double)(1.0+(b*b)/(a*a)))): \
    ((b==0.0)?0.0:(fabs((double)b)*sqrt((double)(1.0+(a*a)/(b*b))))))


int svdcmp(float *a[], int m, int n, float w[], float *v[])
{
	int             flag, i, its, j, jj, k, l, nm;
	float          c, f, h, s, x, y, z, *rv1;
	float          anorm = 0.0, g = 0.0, scale = 0.0;
	float test;

	rv1 = (float *) calloc(n,sizeof(float));

	/* Householder reduction to bidiagonal form.			 */
	for (i = 0; i < n; i++) {
		l = i + 1;
		rv1[i] = scale * g;
		g = s = scale = 0.0;
		if (i < m) {
			for (k = i; k < m; k++)
				scale += fabs((double)a[k][i]);
			if (scale) {
				for (k = i; k < m; k++) {
					a[k][i] /= scale;
					s += a[k][i] * a[k][i];
				}
				f = a[i][i];
				g = -SIGN(f)*sqrt((double)s);
				h = f * g - s;
				a[i][i] = f - g;
				if (i != n - 1) {
					for (j = l; j < n; j++) {
						for (s = 0.0, k = i; k < m; k++)
							s += a[k][i] * a[k][j];
						f = s / h;
						for (k = i; k < m; k++)
							a[k][j] += f * a[k][i];
					}
				}
				for (k = i; k < m; k++)
					a[k][i] *= scale;
			}
		}
		w[i] = scale * g;
		g = s = scale = 0.0;
		if (i < m && i != n - 1) {
			for (k = l; k < n; k++)
				scale += fabs((double)a[i][k]);
			if (scale) {
				for (k = l; k < n; k++) {
					a[i][k] /= scale;
					s += a[i][k] * a[i][k];
				}
				f = a[i][l];
				g = -SIGN(f)*sqrt((double)s);
				h = f * g - s;
				a[i][l] = f - g;
				for (k = l; k < n; k++)
					rv1[k] = a[i][k] / h;
				if (i != m - 1) {
					for (j = l; j < m; j++) {
						for (s = 0.0, k = l; k < n; k++)
							s += a[j][k] * a[i][k];
						for (k = l; k < n; k++)
							a[j][k] += s * rv1[k];
					}
				}
				for (k = l; k < n; k++)
					a[i][k] *= scale;
			}
		}
		anorm = MAX(anorm, (fabs((double)w[i]) + fabs((double)rv1[i])));
	}
	/* Accumulation of right-hand transformations.			 */
	for (i = n - 1; 0 <= i; i--) {
		if (i < n - 1) {
			if (g) {
				for (j = l; j < n; j++)
					v[j][i] = (a[i][j] / a[i][l]) / g;
				/*
				 * Double division to avoid possible
				 * underflow:
				 */
				for (j = l; j < n; j++) {
					for (s = 0.0, k = l; k < n; k++)
						s += a[i][k] * v[k][j];
					for (k = l; k < n; k++)
						v[k][j] += s * v[k][i];
				}
			}
			for (j = l; j < n; j++)
				v[i][j] = v[j][i] = 0.0;
		}
		v[i][i] = 1.0;
		g = rv1[i];
		l = i;
	}
	/* Accumulation of left-hand transformations.			 */
	for (i = n - 1; 0 <= i; i--) {
		l = i + 1;
		g = w[i];
		if (i < n - 1)
			for (j = l; j < n; j++)
				a[i][j] = 0.0;
		if (g) {
			g = 1.0 / g;
			if (i != n - 1) {
				for (j = l; j < n; j++) {
					for (s = 0.0, k = l; k < m; k++)
						s += a[k][i] * a[k][j];
					f = (s / a[i][i]) * g;
					for (k = i; k < m; k++)
						a[k][j] += f * a[k][i];
				}
			}
			for (j = i; j < m; j++)
				a[j][i] *= g;
		} else
			for (j = i; j < m; j++)
				a[j][i] = 0.0;
		++a[i][i];
	}
	/* Diagonalization of the bidiagonal form.				 */
	for (k = n - 1; 0 <= k; k--) {	/* Loop over singular values.	 */
		for (its = 0; its < 30; its++) {	/* Loop over allowed
							 * iterations. */
			flag = 1;
			for (l = k; 0 < l; l--) {	/* Test for splitting:		 */
				nm = l - 1;	/* Note that rv1[0] is always
						 * zero. */
				/*
				 * the following step and two similar ones
				 * later are designed to check if a number
				 * added to anorm is so small so as not to
				 * change it. Note this is different from
				 * having the number be equal to 0.0!
				 */
/*
				if ((float)fabs((double)rv1[l]) 
					< FLT_EPSILON*anorm) {
					flag = 0;
					break;
				}
				if ((float)fabs((double)w[nm]) 
					< FLT_EPSILON*anorm)
					break;
*/
/* New test code */
				test = anorm + ((float)fabs((double)rv1[l]));
				if(anorm == test)
				{
					flag = 0;
					break;
				}
				test = anorm + ((float)fabs((double)w[nm]));
				if(anorm == test)
					break;
				
			}
			if (flag) {
				c = 0.0;	/* Cancellation of rv1[l], if
						 * l>0: */
				s = 1.0;
				for (i = l; i <= k; i++) {
					f = s * rv1[i];
					/*
					 * Ahead is differing code from
					 * Numerical recipes and a compact
					 * disk program
					 */
					rv1[i] *= c;
					test = anorm + ((float)fabs((double)f));
					if(test == anorm)
						break;
					g = w[i];
					h = PYTHAG(f, g);
					w[i] = h;
					h = 1.0 / h;
					c = g * h;
					s = (-f * h);
					for (j = 0; j < m; j++) {
						y = a[j][nm];
						z = a[j][i];
						a[j][nm] = y * c + z * s;
						a[j][i] = z * c - y * s;
					}
				}
			}
			z = w[k];
			if (l == k) {	/* Convergence.				 */
				if (z < 0.0) {	/* Singular value is made
						 * non-negative.	 */
					w[k] = -z;
					for (j = 0; j < n; j++)
						v[j][k] = (-v[j][k]);
				}
				break;
			}
			if (its == 29) {
				register_error(0,"svdcmp:  QR algorithm failed to converge in 30 iterations.  Gave up\n");
				return(1);				
			}
			x = w[l];	/* Shift from bottom 2-by-2 minor.	 */
			nm = k - 1;
			y = w[nm];
			g = rv1[nm];
			h = rv1[k];
			f = ((y - z) * (y + z) + (g - h) * (g + h)) / (2.0 * h * y);
			g = PYTHAG(f, 1.0);
			f = ((x - z) * (x + z) + h * ((y / (f + g*SIGN(f))) - h)) / x;
			/* Next QR transformation:					 */
			c = s = 1.0;
			for (j = l; j <= nm; j++) {
				i = j + 1;
				g = rv1[i];
				y = w[i];
				h = s * g;
				g = c * g;
				z = PYTHAG(f, h);
				rv1[j] = z;
				c = f / z;
				s = h / z;
				f = x * c + g * s;
				g = g * c - x * s;
				h = y * s;
				y = y * c;
				for (jj = 0; jj < n; jj++) {
					x = v[jj][j];
					z = v[jj][i];
					v[jj][j] = x * c + z * s;
					v[jj][i] = z * c - x * s;
				}
				z = PYTHAG(f, h);
				w[j] = z;	/* Rotation can be arbitrary
						 * if z = 0.	 */
				if (z) {
					z = 1.0 / z;
					c = f * z;
					s = h * z;
				}
				f = (c * g) + (s * y);
				x = (c * y) - (s * g);
				for (jj = 0; jj < m; jj++) {
					y = a[jj][j];
					z = a[jj][i];
					a[jj][j] = y * c + z * s;
					a[jj][i] = z * c - y * s;
				}
			}
			rv1[l] = 0.0;
			rv1[k] = f;
			w[k] = x;
		}
	}
	free(rv1);
	return(0);
}

/* $Id$ */
