#include <stdio.h>
#include <math.h>
#include "ahhead.h"		       /* to get definition of complex arrays */
#include "stock.h"
#include "ahfft.h"

/*
	two sets of C routines are used for real data;
		cfour(data,n,isign)
		discrete_fft(data,n,isign)
		
	and
		cfftr(data,n)
		cfftri(data,n)

	input to cfour and discrete_fft is an array of complex values either
	defined as a complex structure or a real array with real
	and imaginary parts alternating.  cfour makes no assumptions
	about the data and isign is either + or - 1 depending on
	whether you are taking a transform or inverse transform.
	cfftr assumes the input data is pure real.
	To get the correct amplitudes back from cfour after
	inverse transforming divide all values by the number of data
	points n.  To get the correct amplitudes back from cfftri
	divide all values by the number of data points divided by 2.

*/
/*
fortran protocols:
			call four( data, n, isign )

			call ifour( idata, n, isign )

			call fftr( data, n )

			call ifftr( idata, n )

			call fftri( data, n )

			call ifftri( idata, n )

all integers are integer*2
*/
/*
four and ifour
discrete fourier transform subroutines
based on the fast fourier transform algorithm
which computes the quantity :

              n-1
        x(j)=sum  x(k) exp( 2*pi*i*isign*k*j/n )
              k=0

if the data consist of the real and complex parts of
the signal stored in a real array the fortran call
is:
	call four( data, n, isign )
while if the data are stored in an integer*2 array
the call is:
	call ifour( data, n, isign )
in both cases the number of complex numbers, n and
the sign of the exponent, isign, are integer*2
n must be a integral power of 2.
beware of roundoff and overflow error if you use
ifour.

*/
#include <math.h>

void
four_ (float *data, int *n, int *isign)
{
    cfour (data, *n, *isign);
    return;
}

void
ifour_ (int *data, int *n, int *isign)
{
    cifour (data, *n, *isign);
    return;
}

void
cfour (float *data, int n, int isign)
{
    int             ip0,
                    ip1,
                    ip2,
                    ip3,
                    i3rev;
    int             i1,
                    i2a,
                    i2b,
                    i3;
    float           sinth,
                    wstpr,
                    wstpi,
                    wr,
                    wi,
                    tempr,
                    tempi,
                    theta;
    double          sin (double);
    ip0 = 2;
    ip3 = ip0 * n;
    i3rev = 1;
    for (i3 = 1; i3 <= ip3; i3 += ip0) {
	if (i3 < i3rev) {
	    tempr = data[i3 - 1];
	    tempi = data[i3];
	    data[i3 - 1] = data[i3rev - 1];
	    data[i3] = data[i3rev];
	    data[i3rev - 1] = tempr;
	    data[i3rev] = tempi;
	}
	ip1 = ip3 / 2;
	do {
	    if (i3rev <= ip1)
		break;
	    i3rev -= ip1;
	    ip1 /= 2;
	}
	while (ip1 >= ip0);
	i3rev += ip1;
    }
    ip1 = ip0;
    while (ip1 < ip3) {
	ip2 = ip1 * 2;
	theta = 6.283185 / ((float) (isign * ip2 / ip0));
	sinth = (float) sin ((double) (theta / 2.));
	wstpr = -2. * sinth * sinth;
	wstpi = (float) sin ((double) theta);
	wr = 1.;
	wi = 0.;
	for (i1 = 1; i1 <= ip1; i1 += ip0) {
	    for (i3 = i1; i3 < ip3; i3 += ip2) {
		i2a = i3;
		i2b = i2a + ip1;
		tempr = wr * data[i2b - 1] - wi * data[i2b];
		tempi = wr * data[i2b] + wi * data[i2b - 1];
		data[i2b - 1] = data[i2a - 1] - tempr;
		data[i2b] = data[i2a] - tempi;
		data[i2a - 1] += tempr;
		data[i2a] += tempi;
	    }
	    tempr = wr;
	    wr = wr * wstpr - wi * wstpi + wr;
	    wi = wi * wstpr + tempr * wstpi + wi;
	}
	ip1 = ip2;
    }
    return;
}

void
cifour (int *data, int n, int isign)
{
    int             ip0,
                    ip1,
                    ip2,
                    ip3,
                    i3rev,
                    itempr,
                    itempi;
    int             i1,
                    i2a,
                    i2b,
                    i3;
    float           sinth,
                    wstpr,
                    wstpi,
                    wr,
                    wi,
                    tempr,
                    tempi,
                    theta;
    double          sin (double);
    ip0 = 2;
    ip3 = ip0 * n;
    i3rev = 1;
    for (i3 = 1; i3 <= ip3; i3 += ip0) {
	if (i3 < i3rev) {
	    itempr = data[i3 - 1];
	    itempi = data[i3];
	    data[i3 - 1] = data[i3rev - 1];
	    data[i3] = data[i3rev];
	    data[i3rev - 1] = itempr;
	    data[i3rev] = itempi;
	}
	ip1 = ip3 / 2;
	do {
	    if (i3rev <= ip1)
		break;
	    i3rev -= ip1;
	    ip1 /= 2;
	}
	while (ip1 >= ip0);
	i3rev += ip1;
    }
    ip1 = ip0;
    while (ip1 < ip3) {
	ip2 = ip1 * 2;
	theta = 6.283185 / ((float) (isign * ip2 / ip0));
	sinth = (float) sin ((double) (theta / 2.));
	wstpr = -2. * sinth * sinth;
	wstpi = (float) sin ((double) theta);
	wr = 1.;
	wi = 0.;
	for (i1 = 1; i1 <= ip1; i1 += ip0) {
	    for (i3 = i1; i3 < ip3; i3 += ip2) {
		i2a = i3;
		i2b = i2a + ip1;
		itempr = ((int) (wr * ((float) data[i2b - 1]) - wi * ((float) data[i2b])));
		itempi = ((int) (wr * ((float) data[i2b]) + wi * ((float) data[i2b - 1])));
		data[i2b - 1] = data[i2a - 1] - itempr;
		data[i2b] = data[i2a] - itempi;
		data[i2a - 1] += itempr;
		data[i2a] += itempi;
	    }
	    tempr = wr;
	    wr = wr * wstpr - wi * wstpi + wr;
	    wi = wi * wstpr + tempr * wstpi + wi;
	}
	ip1 = ip2;
    }
    return;
}

/*
fftr and ifftr
these subroutines take a real time series and compute its
discrete fourier transform. the time series has n real points
and the transform has n/2+1 complex values starting with
frequency zero and ending at the nyquist frequency. parameters:
x ... data array, real for rfft and integer*2 for irfft
n ... the number of samples, an integral power of 2

the fortran calls are
	call fftr( x, n )
	call ifftr( x, n )
*/

void
fftr_ (float *x, int *n)
{
    cfftr (x, *n);
    return;
}

void
cfftr (float *x, int n)
{
    int             nn,
                    is,
                    nm,
                    j,
                    i;
    int             k1j,
                    k1i,
                    k2j,
                    k2i;
    float           s,
                    fn,
                    ex,
                    wr,
                    wi,
                    wwr,
                    wrr,
                    wwi,
                    a1,
                    a2,
                    b1,
                    b2;
    double          sin (double),
                    cos (double);
    nn = n / 2;
    is = 1;
    cfour (x, nn, is);
    nm = nn / 2;
    s = x[0];
    x[0] += x[1];
    x[n] = s - x[1];
    x[1] = 0.0;
    x[n + 1] = 0.0;
    x[nn + 1] = (-x[nn + 1]);
    fn = (float) n;
    ex = 6.2831853 / fn;
    j = nn;
    wr = 1.0;
    wi = 0.0;
    wwr = (float) cos ((double) ex);
    wwi = (float) (-sin ((double) ex));
    for (i = 2; i <= nm; i++) {
	wrr = wr * wwr - wi * wwi;
	wi = wr * wwi + wi * wwr;
	wr = wrr;
	k1j = 2 * j - 1;
	k1i = 2 * i - 1;
	k2j = 2 * j;
	k2i = 2 * i;
	a1 = 0.5 * (x[k1i - 1] + x[k1j - 1]);
	a2 = 0.5 * (x[k2i - 1] - x[k2j - 1]);
	b1 = 0.5 * (-x[k1i - 1] + x[k1j - 1]);
	b2 = 0.5 * (-x[k2i - 1] - x[k2j - 1]);
	s = b1;
	b1 = b1 * wr + b2 * wi;
	b2 = b2 * wr - s * wi;
	x[k1i - 1] = a1 - b2;
	x[k2i - 1] = (-a2 - b1);
	x[k1j - 1] = a1 + b2;
	x[k2j - 1] = a2 - b1;
	j -= 1;
    }
    return;
}

void
ifftr_ (int *x, int *n)
{
    cifftr (x, *n);
    return;
}

void
cifftr (int *x, int n)
{
    int             nn,
                    is,
                    nm,
                    j,
                    i,
                    sv;
    int             k1j,
                    k1i,
                    k2j,
                    k2i;
    float           s,
                    fn,
                    ex,
                    wr,
                    wi,
                    wwr,
                    wrr,
                    wwi,
                    a1,
                    a2,
                    b1,
                    b2;
    double          sin (double),
                    cos (double);
    nn = n / 2;
    is = 1;
    cifour (x, nn, is);
    nm = nn / 2;
    sv = x[0];
    x[0] += x[1];
    x[n] = sv - x[1];
    x[1] = 0.0;
    x[n + 1] = 0.0;
    x[nn + 1] = (-x[nn + 1]);
    fn = (float) n;
    ex = 6.2831853 / fn;
    j = nn;
    wr = 1.0;
    wi = 0.0;
    wwr = (float) cos ((double) ex);
    wwi = (float) (-sin ((double) ex));
    for (i = 2; i <= nm; i++) {
	wrr = wr * wwr - wi * wwi;
	wi = wr * wwi + wi * wwr;
	wr = wrr;
	k1j = 2 * j - 1;
	k1i = 2 * i - 1;
	k2j = 2 * j;
	k2i = 2 * i;
	a1 = 0.5 * (float) (x[k1i - 1] + x[k1j - 1]);
	a2 = 0.5 * (float) (x[k2i - 1] - x[k2j - 1]);
	b1 = 0.5 * (float) (-x[k1i - 1] + x[k1j - 1]);
	b2 = 0.5 * (float) (-x[k2i - 1] - x[k2j - 1]);
	s = b1;
	b1 = b1 * wr + b2 * wi;
	b2 = b2 * wr - s * wi;
	x[k1i - 1] = (int) (a1 - b2);
	x[k2i - 1] = (int) (-a2 - b1);
	x[k1j - 1] = (int) (a1 + b2);
	x[k2j - 1] = (int) (a2 - b1);
	j -= 1;
    }
    return;
}

/*
fftri and ifftri
these subroutines compute the inverse fast fourier transform
of a real time series.
n/2 + 1 complex frequency values are input and n real timeseries are
returned, where n is a power of two.
the complex frequencies are stores in the array x, with real and
imaginary parts alternating.
fortran calls:
	call fftri( x , n )
where x is a real array, and
	call ifftri( x, n)
where x is an integer*2 array
*/

void
fftri_ (float *x, int *n)
{
    cfftri (x, *n);
    return;
}

void
cfftri (float *x, int n)
{
    int             nn,
                    is,
                    nm,
                    j,
                    i,
                    k1j,
                    k1i,
                    k2j,
                    k2i;
    float           s,
                    fn,
                    ex,
                    wr,
                    wi,
                    wwr,
                    wwi,
                    wrr,
                    a1,
                    a2,
                    b1,
                    b2;
    double          sin (double),
                    cos (double);
    nn = n / 2;
    s = x[0];
    x[0] = 0.5 * (x[0] + x[n]);
    x[1] = 0.5 * (s - x[n]);
    x[nn + 1] = (-x[nn + 1]);
    is = -1;
    nm = nn / 2;
    fn = (float) n;
    ex = 6.2831853 / fn;
    j = nn;
    wr = 1.0;
    wi = 0.0;
    wwr = (float) cos ((double) ex);
    wwi = (float) (-sin ((double) ex));
    for (i = 2; i <= nm; i++) {
	wrr = wr * wwr - wi * wwi;
	wi = wr * wwi + wi * wwr;
	wr = wrr;
	k1j = 2 * j - 1;
	k1i = 2 * i - 1;
	k2j = 2 * j;
	k2i = 2 * i;
	a1 = 0.5 * (x[k1i - 1] + x[k1j - 1]);
	a2 = 0.5 * (x[k2i - 1] - x[k2j - 1]);
	b1 = 0.5 * (-x[k1i - 1] + x[k1j - 1]);
	b2 = 0.5 * (-x[k2i - 1] - x[k2j - 1]);
	s = b1;
	b1 = b1 * wr + b2 * wi;
	b2 = b2 * wr - s * wi;
	x[k1i - 1] = (a1 - b2);
	x[k2i - 1] = (-a2 - b1);
	x[k1j - 1] = (a1 + b2);
	x[k2j - 1] = (a2 - b1);
	j -= 1;
    }
    cfour (x, nn, is);
    return;
}

void
ifftri_ (int *x, int *n)
{
    cifftri (x, *n);
    return;
}

void
cifftri (int *x, int n)
{
    int             nn,
                    is,
                    nm,
                    j,
                    i,
                    k1j,
                    k1i,
                    k2j,
                    k2i,
                    sv;
    float           s,
                    fn,
                    ex,
                    wr,
                    wi,
                    wwr,
                    wwi,
                    wrr,
                    a1,
                    a2,
                    b1,
                    b2;
    double          sin (double),
                    cos (double);
    nn = n / 2;
    sv = x[0];
    x[0] = 0.5 * (x[0] + x[n]);
    x[1] = 0.5 * (sv - x[n]);
    x[nn + 1] = (-x[nn + 1]);
    is = -1;
    nm = nn / 2;
    fn = (float) n;
    ex = 6.2831853 / fn;
    j = nn;
    wr = 1.0;
    wi = 0.0;
    wwr = (float) cos ((double) ex);
    wwi = (float) (-sin ((double) ex));
    for (i = 2; i <= nm; i++) {
	wrr = wr * wwr - wi * wwi;
	wi = wr * wwi + wi * wwr;
	wr = wrr;
	k1j = 2 * j - 1;
	k1i = 2 * i - 1;
	k2j = 2 * j;
	k2i = 2 * i;
	a1 = 0.5 * (float) (x[k1i - 1] + x[k1j - 1]);
	a2 = 0.5 * (float) (x[k2i - 1] - x[k2j - 1]);
	b1 = 0.5 * (float) (-x[k1i - 1] + x[k1j - 1]);
	b2 = 0.5 * (float) (-x[k2i - 1] - x[k2j - 1]);
	s = b1;
	b1 = b1 * wr + b2 * wi;
	b2 = b2 * wr - s * wi;
	x[k1i - 1] = (int) (a1 - b2);
	x[k2i - 1] = (int) (-a2 - b1);
	x[k1j - 1] = (int) (a1 + b2);
	x[k2j - 1] = (int) (a2 - b1);
	j -= 1;
    }
    cifour (x, nn, is);
    return;
}


/* subroutine discrete_fft constructs the discrete fast fourier transform
 * of a time series using the Cooley-Tukey mixed Radix method.  Algorithum
 * is modified from Conte and de Boor, Elementary Numerical Analysis, page
 * 281.
 *
 * Input:
 *	z1=pointer to complex array of data to be fourier transformed
 *         Length of array should be n+1
 *	n=number of data points in data
 *	isign=sign of transform; 1 for fft, -1 for inverse fft
 *
 * Output:
 *	z1=pointer to complex array fourier transformed data
 *		note: input data array is over written
 *			Also if a data set is transformed and then
 *			inverse transformed, output amplitudes should
 *			be divided by the n
 */
void
discrete_fft (complex * z1, int n, int isign)
{

    complex        *z2;		       /* temporary work space */
    int             after,
                    now,
                    before,
                    next,
                    nextmx,
                    inz,
                    i;
    static int      prime[12] = {2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37};

    after = 1;
    before = n;
    next = 0;
    nextmx = 12;
    inz = 1;			       /* flip-flop variable to keep track of
				        * transform */

/* open up temporary work space */

    if ((z2 = (complex *) calloc ((unsigned) (n * 2 + 1), sizeof (float))) == NULL) {
	fprintf (stderr, "Error allocating space for fft\n");
	exit (-2);
    }
    while (before != 1) {
nextprime:if ((before / prime[next]) * prime[next] < before) {	/* find smallest prime */
	    ++next;
	    if (next < nextmx)
		goto nextprime;	       /* I know - I know its ugly */
	    now = before;	       /* set up variables for transform */
	    before = 1;
	} else {
	    now = prime[next];	       /* set up variables for transform */
	    before = before / prime[next];
	}
	if (inz == 1) {		       /* perform one step of transform */
	    fftstp (z1, after, now, before, z2, isign);
	} else {
	    fftstp (z2, after, now, before, z1, isign);
	}
	inz = 3 - (inz);	       /* keep track of output */
	after *= now;
    }

    if (inz == 2)
	memmove ((char *) z1, (char *) z2, sizeof (complex) * n);	/* output is in z2 move
									 * it to z1 */
    free (z2);			       /* free temporary space */
}

/* subroutine carries out one step of the discrete fourier transform
 */

void
fftstp (complex * zin, int after, int std_now, int before, complex * zout, int isign)
{

    int             ia,
                    ib,
                    in,
                    j,
                    inter_fact2,
                    inter_fact3;
    complex         arg,
                    omega,
                    value,
                    temp,
                   *pointer1,
                   *pointer2,
                   *array_end2;
    complex        *array_end1,
                   *pointer3;
    float           angle,
                    twopi = 6.283185307;

/* pointer1 & pointer2 -> zin ; pointer3 -> zout */

    angle = twopi / (float) (isign * now * after);
    omega.r = cos (angle);
    omega.i = (-1.0 * sin (angle));
    arg.r = 1.0;
    arg.i = 0.0;

    inter_fact2 = after * before;      /* array increment for zin */
    inter_fact3 = now * after;	       /* array increment for zout */

    for (j = 1; j <= now; j++) {
	for (ia = 1; ia <= after; ia++) {

	    array_end1 = zin + (ia - 1 + after * (before - 1 + before * (now - 1)));	/* loop end criteria for
											 * zin */
	    for (ib = 1, pointer3 = zout + ia - 1 + after * (j - 1), pointer1 = zin + ia - 1 + after * before * (now - 1); pointer1 <= array_end1; ib++, pointer3 += inter_fact3, pointer1 += after) {
		value.r = pointer1->r;
		value.i = pointer1->i;

		array_end2 = zin + (ia - 1 + after * (ib - 1));	/* loop end criteria for
								 * zin */
		for (pointer2 = zin + (ia - 1 + after * (ib - 1 + before * (now - 2))); pointer2 >= array_end2; pointer2 -= inter_fact2) {
		    temp.r = value.r * arg.r - value.i * arg.i;
		    temp.i = value.r * arg.i + value.i * arg.r;
		    value.r = temp.r + pointer2->r;
		    value.i = temp.i + pointer2->i;
		}
		pointer3->r = value.r;
		pointer3->i = value.i;
	    }
	    temp.r = arg.r * omega.r - arg.i * omega.i;
	    arg.i = arg.r * omega.i + arg.i * omega.r;
	    arg.r = temp.r;
	}
    }
}

/* real_dis_fft computes the discrete fourier transform of a real input data
 * array.
 *
 * Inputs;
 *		data=real array of input data
 *			Note: must be dimensioned to 2*(npts+1)
 *		npts=number of data points in data
 *		isign=sign of fourier transform 1 for transform -1 for inverse
 *
 * Outputs;
 *		data=array of transformed data
 */

void 
real_dis_fft (float *data, int npts, int isign)
{
    int             i;

    if (isign == 1) {		       /* convert real array to complex array
				        * with zero imag */
	for (i = npts; i >= 0; --i) {
	    data[i * 2] = data[i];
	    data[i * 2 + 1] = 0.0;
	}
    } else {			       /* complete complex array using inherent
				        * symmetry */
	for (i = 0; i < npts; i += 2) {
	    data[2 * (npts) - i] = data[i];
	    data[2 * (npts) - i + 1] = (-data[i + 1]);
	}
    }
    discrete_fft ((complex *) data, npts, isign);

    if (isign == -1) {		       /* collapse complex array to a real
				        * array */
	for (i = 0; i <= npts; i++) {
	    data[i] = data[i * 2];
	}
    }
}

