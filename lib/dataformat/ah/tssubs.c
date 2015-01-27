/* tssubs.c
 *	time series and vector manipulation functions.
 *			-- witte 25 november 1985
 */

#include	<math.h>

#include "ahtss.h"

#define	PI	3.1415926535898
#define	TWOPI	6.2831853071796

#define	MAX( a, b )	( ((a) > (b)) ? (a) : (b) )
#define	MIN( a, b )	( ((a) > (b)) ? (b) : (a) )


/* dot_product
 *	returns the dot product of the vectors x, y.
 *	The vectors are assumed to be npts long, and
 *	the distance between values to be accessed
 *	is given by the ?step variables.
 * RETURNS:
 *	the dot product
 */
float 
dot_product (float *x, unsigned int xstep, float *y, unsigned int ystep, int npts)
{
    float           dp = 0.0;

    while (npts--) {
	dp += (*x) * (*y);
	x += xstep;
	y += ystep;
    }
    return (dp);
}


/* reverse_dot_product
 *	returns the reverse dot product of the vectors x, y.
 *	The vectors are assumed to be npts long, and
 *	the distance between values to be accessed
 *	is given by the ?step variables.
 * RETURNS:
 *	the reverse dot product
 */
float 
reverse_dot_product (float *x, unsigned int xstep, float *y, unsigned int ystep, int npts)
{
    float           rdp = 0.0;

    y += npts * ystep;

    while (npts--) {
	rdp += (*x) * (*y);
	x += xstep;
	y -= ystep;
    }
    return (rdp);
}


/* reverse_order
 *	reverses the order of the array "data".
 * RETURNS:
 *	just returns
 */
void 
reverse_order (float *data, int npts, unsigned int step)
{
    float          *d_lo,
                   *d_hi;
    float           temp;

    d_lo = data;
    d_hi = data + (npts - 1) * step;

    npts /= 2;
    while (npts--) {
	temp = *d_lo;
	*d_lo = *d_hi;
	d_lo += step;
	*d_hi = temp;
	d_hi -= step;
    }
    return;
}


/* norm_by_length
 *	normalizes the data vector by dividing by the length
 *	of the data vector.
 * RETURNS:
 *	just returns
 */
void 
norm_by_length (float *data, int npts, unsigned int step)
{
    int             n;
    float          *d;
    float           length = 0.0;

    d = data;
    n = npts;
    while (n--) {
	length += (*d) * (*d);
	d += step;
    }

    length = (float) sqrt ((double) length);

    while (npts--) {
	*data /= length;
	data += step;
    }

    return;
}


/* maxmag
 *	returns the maximum magnitude of the elements of the
 *	vector "data".
 * RETURNS
 *	maximum magnitude
 */
float 
maxmag (float *data, int npts, unsigned int step)
{
    float           max,
                    min;

    max = min = *data;
    while (npts--) {
	max = MAX (max, *data);
	min = MIN (min, *data);
	data += step;
    }

    if (fabs ((double) max) > fabs ((double) min))
	return (max);
    else
	return (-min);
}


/* max_value
 *	returns the maximum value of the elements in the vector "data"
 * RETURNS:
 *	maximum value
 */
float 
max_value (float *data, int npts, unsigned int step)
{
    float           max;

    max = *data;
    while (npts--) {
	max = MAX (max, *data);
	data += step;
    }
    return (max);
}



/* min_value
 *	returns the minimum value of the elements in the vector "data"
 * RETURNS:
 *	minimum value
 */
float 
min_value (float *data, int npts, unsigned int step)
{
    float           min;

    min = *data;
    while (npts--) {
	min = MIN (min, *data);
	data += step;
    }
    return (min);
}


/* scale
 *	multiplies each element of the vector "data" by the
 *	number "factor".
 * RETURNS:
 *	just returns
 */
void 
scale (float factor, float *data, int npts, unsigned int step)
{
    while (npts--) {
	*data *= factor;
	data += step;
    }
    return;
}


/* norm_by_maxmag
 *	divides each element of the vector "data" by the
 *	value of the element with the greatest magnitude.
 * RETURNS:
 *	just returns
 */
void 
norm_by_maxmag (float *data, int npts, unsigned int step)
{
    float           max;

    float           maxmag (float *data, int npts, unsigned int step);
    void            scale (float factor, float *data, int npts, unsigned int step);

    max = maxmag (data, npts, step);
    scale (1. / max, data, npts, step);

    return;
}


/* covariance
 *	computes the covariance of the vectors x and y for
 *	positive lags.  In order to obtain the covariance
 *	for negative lags, reverse the order of the vectors
 *	appearing as arguments.
 * RETURNS:
 *	just returns
 */
void 
covariance (float *x, int nx, unsigned int xstep, float *y, int ny, unsigned int ystep, float *c, int nc, unsigned int cstep)
{
    float           dot_product (float *x, unsigned int xstep, float *y, unsigned int ystep, int npts);
    int             scale;

    scale = nc - 1;

    while (nc--) {
	*c = dot_product (x, xstep, y, ystep, MIN (nx, ny)) / scale;
	c += cstep;
	x += xstep;
	--nx;
    }
    return;
}


/* correlation
 *	computes the correlation (normalized covariance) of the
 *	vectors x and y for positive lags.  In order to obtain the
 *	correlation for negative lags, reverse the order of the
 *	vectors appearing as arguments.
 * RETURNS:
 *	just returns
 */
void 
correlation (float *x, int nx, unsigned int xstep, float *y, int ny, unsigned int ystep, float *c, int nc, unsigned int cstep)
{
    void            covariance (float *x, int nx, unsigned int xstep, float *y, int ny, unsigned int ystep, float *c, int nc, unsigned int cstep),
                    scale (float factor, float *data, int npts, unsigned int step);

    covariance (x, nx, xstep, y, ny, ystep, c, nc, cstep);
    scale (1. / (*c), c, nc, cstep);

    return;
}


/* autocovariance
 *	computes the autocovariance of the vector x.
 * RETURNS:
 *	just returns
 */
void 
autocovariance (float *x, int nx, unsigned int xstep, float *a, int na, unsigned int astep)
{
    float          *x0;
    int             scale;
    float           dot_product (float *x, unsigned int xstep, float *y, unsigned int ystep, int npts);

    scale = na - 1;

    x0 = x;
    while (na--) {
	*a = dot_product (x, xstep, x0, xstep, nx) / scale;
	a += astep;
	x += xstep;
	--nx;
    }
    return;
}


/* autocorrelation
 *	computes the autocorrelation (normalized autocovariance) of the
 *	vector x.
 * RETURNS:
 *	just returns
 */
void 
autocorrelation (float *x, int nx, unsigned int xstep, float *a, int na, unsigned int astep)
{
    void            autocovariance (float *x, int nx, unsigned int xstep, float *a, int na, unsigned int astep),
                    scale (float factor, float *data, int npts, unsigned int step);

    autocovariance (x, nx, xstep, a, na, astep);
    scale (1. / (*a), a, na, astep);

    return;
}


/* $Id$ */
