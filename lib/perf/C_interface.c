#include <signal.h>
#ifdef __linux__
#include <bits/siginfo.h>
#endif
#include "stock.h"
#include "f2c.h"
#include "perf.h"

complex 
cdotc (int n, complex *cx, int incx,  complex  *cy, int incy)
{
    complex result ; 
    cdotc_ ( &result, &n, cx, &incx, cy, &incy );
    return result ; 
}

void 
dscal(int n, double da, double *dx, int incx) 
{
    dscal_(&n, &da, dx, &incx) ; 
}

void 
csscal(int n, float sa, complex *cx, int incx) 
{
    csscal_ ( &n, &sa, cx, &incx );
}

void 
cscal(int n, complex *ca, complex *cx, int incx) 
{
    cscal_ ( &n, ca, cx, &incx );
}

void 
sscal(int n, float sa, float *sx, int incx) 
{
    sscal_ ( &n, &sa, sx, &incx );
}

void 
dspev(char jobz, char uplo, int n, double *dap,  double *w, 
		double *dz, int ldz, int *info) 
{
    double *work ; 
    allot ( double *, work, 3*n ) ; 
    dspev_ ( &jobz, &uplo, &n, dap, w, dz, &ldz, work, info );
    free(work) ;
}

float 
snrm2(int n, float *sx, int incx) 
{
    return snrm2_ ( &n, sx, &incx );
}

float 
sdot(int n, float *sx, int incx, float *sy, int incy)
{
    return sdot_ ( &n, sx, &incx, sy, &incy );
}

double 
dsdot(int n, float *sx, int  incx,  float  *sy,  int incy) 
{
    return dsdot_ ( &n, sx, &incx, sy, &incy );
}

void sgesvd(char jobu, char jobvt, int m, int n, float  *sa,
	    int lda, float *s, float *su, int ldu, float *svt,
	    int ldvt, int *info)
{
    int lwork ; 
    float *work ; 
    lwork = MAX(3*MIN(m,n)+MAX(m,n),5*MIN(m,n)-4) * 2 ;
    allot ( float *, work, lwork ) ; 
    sgesvd_ ( &jobu, &jobvt, &m, &n, sa, &lda, s, su, &ldu, svt, &ldvt, 
    		work, &lwork, info );
    free(work) ; 
}

void scopy(int n, float *sx, int incx, float *sy, int  incy) 
{
    scopy_ ( &n, sx, &incx, sy, &incy );
}

int idamax(int n, double *dx, int incx) 
{
    return idamax_ ( &n, dx, &incx );
}

void 
caxpy (int n, complex *za, complex *zx, int incx,  complex *zy, int incy)
{
    caxpy_ ( &n, za, zx, &incx, zy, &incy );
}

void 
daxpy(int n, double da, double *dx,  int  incx,  double *dy, int incy) 
{
    daxpy_ ( &n, &da, dx, &incx, dy, &incy );
}

void 
ccopy (int n, complex *cx, int incx, complex  *cy,  int incy)
{
    ccopy_ ( &n, cx, &incx, cy, &incy );
}

void 
dcopy(int n, double *dx,  int  incx,  double  *dy,  int incy) 
{
    dcopy_ ( &n, dx, &incx, dy, &incy );
}

double 
ddot(int n, double *dx, int  incx,  double  *dy,  int incy) 
{
    return ddot_ ( &n, dx, &incx, dy, &incy );
}

void cgesvd(char jobu, char jobvt, int  m,  int  n,  
	    complex *ca, int lda, float *s, complex *cu, int ldu, 
	    complex *cvt, int ldvt, int *info)
{
    complex *work;
    float *rwork ;
    int lwork, rwork_size ;

    lwork = 2*MIN(m,n)+MAX(m,n) * 4 ;
    allot ( complex *, work, lwork ) ;
    rwork_size = MAX(3*MIN(m,n),5*min(m,n)-4) ;
    allot ( float *, rwork, rwork_size ) ;
    cgesvd_ ( &jobu, &jobvt, &m, &n, ca, &lda, s, cu, &ldu, cvt, &ldvt, 
    		work, &lwork, rwork, info );
    free(work) ;
    free(rwork) ;
}

int 
isamax(int n, float *sx, int incx) 
{
    return isamax_ ( &n, sx, &incx );
}

void dgesvd(char jobu, char jobvt, int m, int n, double *da,
	    int  lda,  double  *s, double *du, int ldu, double
	   *dvt, int ldvt, int *info)
{
    double *work;
    int lwork ;

    lwork = MAX(3*MIN(m,n)+MAX(m,n),5*MIN(m,n)-4) * 2 ;
    allot ( double *, work, lwork ) ;
    dgesvd_ ( &jobu, &jobvt, &m, &n, da, &lda, s, du, &ldu, dvt, &ldvt, 
    	    work, &lwork, info );
    free(work) ;
}

int
nint ( double x ) 
{
    return (int) (x+0.5) ;
}

double 
dnrm2(int n, double *dx, int incx) 
{
    return dnrm2_ ( &n, dx, &incx );
}

void 
dpotrf(char uplo, int  n,  double  *da,  int  lda,  int *info) 
{
    dpotrf_ ( &uplo, &n, da, &lda, info );
}

void 
dpotri(char uplo, int  n,  double  *da,  int  lda,  int *info) 
{
    dpotri_ ( &uplo, &n, da, &lda, info );
}
