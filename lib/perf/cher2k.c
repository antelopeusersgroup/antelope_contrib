#include "blaswrap.h"
#include "f2c.h"

/* Subroutine */ int cher2k_(char *uplo, char *trans, integer *n, integer *k, 
	complex *alpha, complex *a, integer *lda, complex *b, integer *ldb, 
	real *beta, complex *c__, integer *ldc)
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, c_dim1, c_offset, i__1, i__2, 
	    i__3, i__4, i__5, i__6, i__7;
    real r__1;
    complex q__1, q__2, q__3, q__4, q__5, q__6;
    /* Builtin functions */
    void r_cnjg(complex *, complex *);
    /* Local variables */
    static integer info;
    static complex temp1, temp2;
    static integer i__, j, l;
    extern logical lsame_(char *, char *);
    static integer nrowa;
    static logical upper;
    extern /* Subroutine */ int xerbla_(char *, integer *);
#define a_subscr(a_1,a_2) (a_2)*a_dim1 + a_1
#define a_ref(a_1,a_2) a[a_subscr(a_1,a_2)]
#define b_subscr(a_1,a_2) (a_2)*b_dim1 + a_1
#define b_ref(a_1,a_2) b[b_subscr(a_1,a_2)]
#define c___subscr(a_1,a_2) (a_2)*c_dim1 + a_1
#define c___ref(a_1,a_2) c__[c___subscr(a_1,a_2)]
/*  Purpose   
    =======   
    CHER2K  performs one of the hermitian rank 2k operations   
       C := alpha*A*conjg( B' ) + conjg( alpha )*B*conjg( A' ) + beta*C,   
    or   
       C := alpha*conjg( A' )*B + conjg( alpha )*conjg( B' )*A + beta*C,   
    where  alpha and beta  are scalars with  beta  real,  C is an  n by n   
    hermitian matrix and  A and B  are  n by k matrices in the first case   
    and  k by n  matrices in the second case.   
    Parameters   
    ==========   
    UPLO   - CHARACTER*1.   
             On  entry,   UPLO  specifies  whether  the  upper  or  lower   
             triangular  part  of the  array  C  is to be  referenced  as   
             follows:   
                UPLO = 'U' or 'u'   Only the  upper triangular part of  C   
                                    is to be referenced.   
                UPLO = 'L' or 'l'   Only the  lower triangular part of  C   
                                    is to be referenced.   
             Unchanged on exit.   
    TRANS  - CHARACTER*1.   
             On entry,  TRANS  specifies the operation to be performed as   
             follows:   
                TRANS = 'N' or 'n'    C := alpha*A*conjg( B' )          +   
                                           conjg( alpha )*B*conjg( A' ) +   
                                           beta*C.   
                TRANS = 'C' or 'c'    C := alpha*conjg( A' )*B          +   
                                           conjg( alpha )*conjg( B' )*A +   
                                           beta*C.   
             Unchanged on exit.   
    N      - INTEGER.   
             On entry,  N specifies the order of the matrix C.  N must be   
             at least zero.   
             Unchanged on exit.   
    K      - INTEGER.   
             On entry with  TRANS = 'N' or 'n',  K  specifies  the number   
             of  columns  of the  matrices  A and B,  and on  entry  with   
             TRANS = 'C' or 'c',  K  specifies  the number of rows of the   
             matrices  A and B.  K must be at least zero.   
             Unchanged on exit.   
    ALPHA  - COMPLEX         .   
             On entry, ALPHA specifies the scalar alpha.   
             Unchanged on exit.   
    A      - COMPLEX          array of DIMENSION ( LDA, ka ), where ka is   
             k  when  TRANS = 'N' or 'n',  and is  n  otherwise.   
             Before entry with  TRANS = 'N' or 'n',  the  leading  n by k   
             part of the array  A  must contain the matrix  A,  otherwise   
             the leading  k by n  part of the array  A  must contain  the   
             matrix A.   
             Unchanged on exit.   
    LDA    - INTEGER.   
             On entry, LDA specifies the first dimension of A as declared   
             in  the  calling  (sub)  program.   When  TRANS = 'N' or 'n'   
             then  LDA must be at least  max( 1, n ), otherwise  LDA must   
             be at least  max( 1, k ).   
             Unchanged on exit.   
    B      - COMPLEX          array of DIMENSION ( LDB, kb ), where kb is   
             k  when  TRANS = 'N' or 'n',  and is  n  otherwise.   
             Before entry with  TRANS = 'N' or 'n',  the  leading  n by k   
             part of the array  B  must contain the matrix  B,  otherwise   
             the leading  k by n  part of the array  B  must contain  the   
             matrix B.   
             Unchanged on exit.   
    LDB    - INTEGER.   
             On entry, LDB specifies the first dimension of B as declared   
             in  the  calling  (sub)  program.   When  TRANS = 'N' or 'n'   
             then  LDB must be at least  max( 1, n ), otherwise  LDB must   
             be at least  max( 1, k ).   
             Unchanged on exit.   
    BETA   - REAL            .   
             On entry, BETA specifies the scalar beta.   
             Unchanged on exit.   
    C      - COMPLEX          array of DIMENSION ( LDC, n ).   
             Before entry  with  UPLO = 'U' or 'u',  the leading  n by n   
             upper triangular part of the array C must contain the upper   
             triangular part  of the  hermitian matrix  and the strictly   
             lower triangular part of C is not referenced.  On exit, the   
             upper triangular part of the array  C is overwritten by the   
             upper triangular part of the updated matrix.   
             Before entry  with  UPLO = 'L' or 'l',  the leading  n by n   
             lower triangular part of the array C must contain the lower   
             triangular part  of the  hermitian matrix  and the strictly   
             upper triangular part of C is not referenced.  On exit, the   
             lower triangular part of the array  C is overwritten by the   
             lower triangular part of the updated matrix.   
             Note that the imaginary parts of the diagonal elements need   
             not be set,  they are assumed to be zero,  and on exit they   
             are set to zero.   
    LDC    - INTEGER.   
             On entry, LDC specifies the first dimension of C as declared   
             in  the  calling  (sub)  program.   LDC  must  be  at  least   
             max( 1, n ).   
             Unchanged on exit.   
    Level 3 Blas routine.   
    -- Written on 8-February-1989.   
       Jack Dongarra, Argonne National Laboratory.   
       Iain Duff, AERE Harwell.   
       Jeremy Du Croz, Numerical Algorithms Group Ltd.   
       Sven Hammarling, Numerical Algorithms Group Ltd.   
    -- Modified 8-Nov-93 to set C(J,J) to REAL( C(J,J) ) when BETA = 1.   
       Ed Anderson, Cray Research Inc.   
       Test the input parameters.   
       Parameter adjustments */
    a_dim1 = *lda;
    a_offset = 1 + a_dim1 * 1;
    a -= a_offset;
    b_dim1 = *ldb;
    b_offset = 1 + b_dim1 * 1;
    b -= b_offset;
    c_dim1 = *ldc;
    c_offset = 1 + c_dim1 * 1;
    c__ -= c_offset;
    /* Function Body */
    if (lsame_(trans, "N")) {
	nrowa = *n;
    } else {
	nrowa = *k;
    }
    upper = lsame_(uplo, "U");
    info = 0;
    if (! upper && ! lsame_(uplo, "L")) {
	info = 1;
    } else if (! lsame_(trans, "N") && ! lsame_(trans, 
	    "C")) {
	info = 2;
    } else if (*n < 0) {
	info = 3;
    } else if (*k < 0) {
	info = 4;
    } else if (*lda < max(1,nrowa)) {
	info = 7;
    } else if (*ldb < max(1,nrowa)) {
	info = 9;
    } else if (*ldc < max(1,*n)) {
	info = 12;
    }
    if (info != 0) {
	xerbla_("CHER2K", &info);
	return 0;
    }
/*     Quick return if possible. */
    if (*n == 0 || (alpha->r == 0.f && alpha->i == 0.f || *k == 0) && *beta ==
	     1.f) {
	return 0;
    }
/*     And when  alpha.eq.zero. */
    if (alpha->r == 0.f && alpha->i == 0.f) {
	if (upper) {
	    if (*beta == 0.f) {
		i__1 = *n;
		for (j = 1; j <= i__1; ++j) {
		    i__2 = j;
		    for (i__ = 1; i__ <= i__2; ++i__) {
			i__3 = c___subscr(i__, j);
			c__[i__3].r = 0.f, c__[i__3].i = 0.f;
/* L10: */
		    }
/* L20: */
		}
	    } else {
		i__1 = *n;
		for (j = 1; j <= i__1; ++j) {
		    i__2 = j - 1;
		    for (i__ = 1; i__ <= i__2; ++i__) {
			i__3 = c___subscr(i__, j);
			i__4 = c___subscr(i__, j);
			q__1.r = *beta * c__[i__4].r, q__1.i = *beta * c__[
				i__4].i;
			c__[i__3].r = q__1.r, c__[i__3].i = q__1.i;
/* L30: */
		    }
		    i__2 = c___subscr(j, j);
		    i__3 = c___subscr(j, j);
		    r__1 = *beta * c__[i__3].r;
		    c__[i__2].r = r__1, c__[i__2].i = 0.f;
/* L40: */
		}
	    }
	} else {
	    if (*beta == 0.f) {
		i__1 = *n;
		for (j = 1; j <= i__1; ++j) {
		    i__2 = *n;
		    for (i__ = j; i__ <= i__2; ++i__) {
			i__3 = c___subscr(i__, j);
			c__[i__3].r = 0.f, c__[i__3].i = 0.f;
/* L50: */
		    }
/* L60: */
		}
	    } else {
		i__1 = *n;
		for (j = 1; j <= i__1; ++j) {
		    i__2 = c___subscr(j, j);
		    i__3 = c___subscr(j, j);
		    r__1 = *beta * c__[i__3].r;
		    c__[i__2].r = r__1, c__[i__2].i = 0.f;
		    i__2 = *n;
		    for (i__ = j + 1; i__ <= i__2; ++i__) {
			i__3 = c___subscr(i__, j);
			i__4 = c___subscr(i__, j);
			q__1.r = *beta * c__[i__4].r, q__1.i = *beta * c__[
				i__4].i;
			c__[i__3].r = q__1.r, c__[i__3].i = q__1.i;
/* L70: */
		    }
/* L80: */
		}
	    }
	}
	return 0;
    }
/*     Start the operations. */
    if (lsame_(trans, "N")) {
/*        Form  C := alpha*A*conjg( B' ) + conjg( alpha )*B*conjg( A' ) +   
                     C. */
	if (upper) {
	    i__1 = *n;
	    for (j = 1; j <= i__1; ++j) {
		if (*beta == 0.f) {
		    i__2 = j;
		    for (i__ = 1; i__ <= i__2; ++i__) {
			i__3 = c___subscr(i__, j);
			c__[i__3].r = 0.f, c__[i__3].i = 0.f;
/* L90: */
		    }
		} else if (*beta != 1.f) {
		    i__2 = j - 1;
		    for (i__ = 1; i__ <= i__2; ++i__) {
			i__3 = c___subscr(i__, j);
			i__4 = c___subscr(i__, j);
			q__1.r = *beta * c__[i__4].r, q__1.i = *beta * c__[
				i__4].i;
			c__[i__3].r = q__1.r, c__[i__3].i = q__1.i;
/* L100: */
		    }
		    i__2 = c___subscr(j, j);
		    i__3 = c___subscr(j, j);
		    r__1 = *beta * c__[i__3].r;
		    c__[i__2].r = r__1, c__[i__2].i = 0.f;
		} else {
		    i__2 = c___subscr(j, j);
		    i__3 = c___subscr(j, j);
		    r__1 = c__[i__3].r;
		    c__[i__2].r = r__1, c__[i__2].i = 0.f;
		}
		i__2 = *k;
		for (l = 1; l <= i__2; ++l) {
		    i__3 = a_subscr(j, l);
		    i__4 = b_subscr(j, l);
		    if (a[i__3].r != 0.f || a[i__3].i != 0.f || (b[i__4].r != 
			    0.f || b[i__4].i != 0.f)) {
			r_cnjg(&q__2, &b_ref(j, l));
			q__1.r = alpha->r * q__2.r - alpha->i * q__2.i, 
				q__1.i = alpha->r * q__2.i + alpha->i * 
				q__2.r;
			temp1.r = q__1.r, temp1.i = q__1.i;
			i__3 = a_subscr(j, l);
			q__2.r = alpha->r * a[i__3].r - alpha->i * a[i__3].i, 
				q__2.i = alpha->r * a[i__3].i + alpha->i * a[
				i__3].r;
			r_cnjg(&q__1, &q__2);
			temp2.r = q__1.r, temp2.i = q__1.i;
			i__3 = j - 1;
			for (i__ = 1; i__ <= i__3; ++i__) {
			    i__4 = c___subscr(i__, j);
			    i__5 = c___subscr(i__, j);
			    i__6 = a_subscr(i__, l);
			    q__3.r = a[i__6].r * temp1.r - a[i__6].i * 
				    temp1.i, q__3.i = a[i__6].r * temp1.i + a[
				    i__6].i * temp1.r;
			    q__2.r = c__[i__5].r + q__3.r, q__2.i = c__[i__5]
				    .i + q__3.i;
			    i__7 = b_subscr(i__, l);
			    q__4.r = b[i__7].r * temp2.r - b[i__7].i * 
				    temp2.i, q__4.i = b[i__7].r * temp2.i + b[
				    i__7].i * temp2.r;
			    q__1.r = q__2.r + q__4.r, q__1.i = q__2.i + 
				    q__4.i;
			    c__[i__4].r = q__1.r, c__[i__4].i = q__1.i;
/* L110: */
			}
			i__3 = c___subscr(j, j);
			i__4 = c___subscr(j, j);
			i__5 = a_subscr(j, l);
			q__2.r = a[i__5].r * temp1.r - a[i__5].i * temp1.i, 
				q__2.i = a[i__5].r * temp1.i + a[i__5].i * 
				temp1.r;
			i__6 = b_subscr(j, l);
			q__3.r = b[i__6].r * temp2.r - b[i__6].i * temp2.i, 
				q__3.i = b[i__6].r * temp2.i + b[i__6].i * 
				temp2.r;
			q__1.r = q__2.r + q__3.r, q__1.i = q__2.i + q__3.i;
			r__1 = c__[i__4].r + q__1.r;
			c__[i__3].r = r__1, c__[i__3].i = 0.f;
		    }
/* L120: */
		}
/* L130: */
	    }
	} else {
	    i__1 = *n;
	    for (j = 1; j <= i__1; ++j) {
		if (*beta == 0.f) {
		    i__2 = *n;
		    for (i__ = j; i__ <= i__2; ++i__) {
			i__3 = c___subscr(i__, j);
			c__[i__3].r = 0.f, c__[i__3].i = 0.f;
/* L140: */
		    }
		} else if (*beta != 1.f) {
		    i__2 = *n;
		    for (i__ = j + 1; i__ <= i__2; ++i__) {
			i__3 = c___subscr(i__, j);
			i__4 = c___subscr(i__, j);
			q__1.r = *beta * c__[i__4].r, q__1.i = *beta * c__[
				i__4].i;
			c__[i__3].r = q__1.r, c__[i__3].i = q__1.i;
/* L150: */
		    }
		    i__2 = c___subscr(j, j);
		    i__3 = c___subscr(j, j);
		    r__1 = *beta * c__[i__3].r;
		    c__[i__2].r = r__1, c__[i__2].i = 0.f;
		} else {
		    i__2 = c___subscr(j, j);
		    i__3 = c___subscr(j, j);
		    r__1 = c__[i__3].r;
		    c__[i__2].r = r__1, c__[i__2].i = 0.f;
		}
		i__2 = *k;
		for (l = 1; l <= i__2; ++l) {
		    i__3 = a_subscr(j, l);
		    i__4 = b_subscr(j, l);
		    if (a[i__3].r != 0.f || a[i__3].i != 0.f || (b[i__4].r != 
			    0.f || b[i__4].i != 0.f)) {
			r_cnjg(&q__2, &b_ref(j, l));
			q__1.r = alpha->r * q__2.r - alpha->i * q__2.i, 
				q__1.i = alpha->r * q__2.i + alpha->i * 
				q__2.r;
			temp1.r = q__1.r, temp1.i = q__1.i;
			i__3 = a_subscr(j, l);
			q__2.r = alpha->r * a[i__3].r - alpha->i * a[i__3].i, 
				q__2.i = alpha->r * a[i__3].i + alpha->i * a[
				i__3].r;
			r_cnjg(&q__1, &q__2);
			temp2.r = q__1.r, temp2.i = q__1.i;
			i__3 = *n;
			for (i__ = j + 1; i__ <= i__3; ++i__) {
			    i__4 = c___subscr(i__, j);
			    i__5 = c___subscr(i__, j);
			    i__6 = a_subscr(i__, l);
			    q__3.r = a[i__6].r * temp1.r - a[i__6].i * 
				    temp1.i, q__3.i = a[i__6].r * temp1.i + a[
				    i__6].i * temp1.r;
			    q__2.r = c__[i__5].r + q__3.r, q__2.i = c__[i__5]
				    .i + q__3.i;
			    i__7 = b_subscr(i__, l);
			    q__4.r = b[i__7].r * temp2.r - b[i__7].i * 
				    temp2.i, q__4.i = b[i__7].r * temp2.i + b[
				    i__7].i * temp2.r;
			    q__1.r = q__2.r + q__4.r, q__1.i = q__2.i + 
				    q__4.i;
			    c__[i__4].r = q__1.r, c__[i__4].i = q__1.i;
/* L160: */
			}
			i__3 = c___subscr(j, j);
			i__4 = c___subscr(j, j);
			i__5 = a_subscr(j, l);
			q__2.r = a[i__5].r * temp1.r - a[i__5].i * temp1.i, 
				q__2.i = a[i__5].r * temp1.i + a[i__5].i * 
				temp1.r;
			i__6 = b_subscr(j, l);
			q__3.r = b[i__6].r * temp2.r - b[i__6].i * temp2.i, 
				q__3.i = b[i__6].r * temp2.i + b[i__6].i * 
				temp2.r;
			q__1.r = q__2.r + q__3.r, q__1.i = q__2.i + q__3.i;
			r__1 = c__[i__4].r + q__1.r;
			c__[i__3].r = r__1, c__[i__3].i = 0.f;
		    }
/* L170: */
		}
/* L180: */
	    }
	}
    } else {
/*        Form  C := alpha*conjg( A' )*B + conjg( alpha )*conjg( B' )*A +   
                     C. */
	if (upper) {
	    i__1 = *n;
	    for (j = 1; j <= i__1; ++j) {
		i__2 = j;
		for (i__ = 1; i__ <= i__2; ++i__) {
		    temp1.r = 0.f, temp1.i = 0.f;
		    temp2.r = 0.f, temp2.i = 0.f;
		    i__3 = *k;
		    for (l = 1; l <= i__3; ++l) {
			r_cnjg(&q__3, &a_ref(l, i__));
			i__4 = b_subscr(l, j);
			q__2.r = q__3.r * b[i__4].r - q__3.i * b[i__4].i, 
				q__2.i = q__3.r * b[i__4].i + q__3.i * b[i__4]
				.r;
			q__1.r = temp1.r + q__2.r, q__1.i = temp1.i + q__2.i;
			temp1.r = q__1.r, temp1.i = q__1.i;
			r_cnjg(&q__3, &b_ref(l, i__));
			i__4 = a_subscr(l, j);
			q__2.r = q__3.r * a[i__4].r - q__3.i * a[i__4].i, 
				q__2.i = q__3.r * a[i__4].i + q__3.i * a[i__4]
				.r;
			q__1.r = temp2.r + q__2.r, q__1.i = temp2.i + q__2.i;
			temp2.r = q__1.r, temp2.i = q__1.i;
/* L190: */
		    }
		    if (i__ == j) {
			if (*beta == 0.f) {
			    i__3 = c___subscr(j, j);
			    q__2.r = alpha->r * temp1.r - alpha->i * temp1.i, 
				    q__2.i = alpha->r * temp1.i + alpha->i * 
				    temp1.r;
			    r_cnjg(&q__4, alpha);
			    q__3.r = q__4.r * temp2.r - q__4.i * temp2.i, 
				    q__3.i = q__4.r * temp2.i + q__4.i * 
				    temp2.r;
			    q__1.r = q__2.r + q__3.r, q__1.i = q__2.i + 
				    q__3.i;
			    r__1 = q__1.r;
			    c__[i__3].r = r__1, c__[i__3].i = 0.f;
			} else {
			    i__3 = c___subscr(j, j);
			    i__4 = c___subscr(j, j);
			    q__2.r = alpha->r * temp1.r - alpha->i * temp1.i, 
				    q__2.i = alpha->r * temp1.i + alpha->i * 
				    temp1.r;
			    r_cnjg(&q__4, alpha);
			    q__3.r = q__4.r * temp2.r - q__4.i * temp2.i, 
				    q__3.i = q__4.r * temp2.i + q__4.i * 
				    temp2.r;
			    q__1.r = q__2.r + q__3.r, q__1.i = q__2.i + 
				    q__3.i;
			    r__1 = *beta * c__[i__4].r + q__1.r;
			    c__[i__3].r = r__1, c__[i__3].i = 0.f;
			}
		    } else {
			if (*beta == 0.f) {
			    i__3 = c___subscr(i__, j);
			    q__2.r = alpha->r * temp1.r - alpha->i * temp1.i, 
				    q__2.i = alpha->r * temp1.i + alpha->i * 
				    temp1.r;
			    r_cnjg(&q__4, alpha);
			    q__3.r = q__4.r * temp2.r - q__4.i * temp2.i, 
				    q__3.i = q__4.r * temp2.i + q__4.i * 
				    temp2.r;
			    q__1.r = q__2.r + q__3.r, q__1.i = q__2.i + 
				    q__3.i;
			    c__[i__3].r = q__1.r, c__[i__3].i = q__1.i;
			} else {
			    i__3 = c___subscr(i__, j);
			    i__4 = c___subscr(i__, j);
			    q__3.r = *beta * c__[i__4].r, q__3.i = *beta * 
				    c__[i__4].i;
			    q__4.r = alpha->r * temp1.r - alpha->i * temp1.i, 
				    q__4.i = alpha->r * temp1.i + alpha->i * 
				    temp1.r;
			    q__2.r = q__3.r + q__4.r, q__2.i = q__3.i + 
				    q__4.i;
			    r_cnjg(&q__6, alpha);
			    q__5.r = q__6.r * temp2.r - q__6.i * temp2.i, 
				    q__5.i = q__6.r * temp2.i + q__6.i * 
				    temp2.r;
			    q__1.r = q__2.r + q__5.r, q__1.i = q__2.i + 
				    q__5.i;
			    c__[i__3].r = q__1.r, c__[i__3].i = q__1.i;
			}
		    }
/* L200: */
		}
/* L210: */
	    }
	} else {
	    i__1 = *n;
	    for (j = 1; j <= i__1; ++j) {
		i__2 = *n;
		for (i__ = j; i__ <= i__2; ++i__) {
		    temp1.r = 0.f, temp1.i = 0.f;
		    temp2.r = 0.f, temp2.i = 0.f;
		    i__3 = *k;
		    for (l = 1; l <= i__3; ++l) {
			r_cnjg(&q__3, &a_ref(l, i__));
			i__4 = b_subscr(l, j);
			q__2.r = q__3.r * b[i__4].r - q__3.i * b[i__4].i, 
				q__2.i = q__3.r * b[i__4].i + q__3.i * b[i__4]
				.r;
			q__1.r = temp1.r + q__2.r, q__1.i = temp1.i + q__2.i;
			temp1.r = q__1.r, temp1.i = q__1.i;
			r_cnjg(&q__3, &b_ref(l, i__));
			i__4 = a_subscr(l, j);
			q__2.r = q__3.r * a[i__4].r - q__3.i * a[i__4].i, 
				q__2.i = q__3.r * a[i__4].i + q__3.i * a[i__4]
				.r;
			q__1.r = temp2.r + q__2.r, q__1.i = temp2.i + q__2.i;
			temp2.r = q__1.r, temp2.i = q__1.i;
/* L220: */
		    }
		    if (i__ == j) {
			if (*beta == 0.f) {
			    i__3 = c___subscr(j, j);
			    q__2.r = alpha->r * temp1.r - alpha->i * temp1.i, 
				    q__2.i = alpha->r * temp1.i + alpha->i * 
				    temp1.r;
			    r_cnjg(&q__4, alpha);
			    q__3.r = q__4.r * temp2.r - q__4.i * temp2.i, 
				    q__3.i = q__4.r * temp2.i + q__4.i * 
				    temp2.r;
			    q__1.r = q__2.r + q__3.r, q__1.i = q__2.i + 
				    q__3.i;
			    r__1 = q__1.r;
			    c__[i__3].r = r__1, c__[i__3].i = 0.f;
			} else {
			    i__3 = c___subscr(j, j);
			    i__4 = c___subscr(j, j);
			    q__2.r = alpha->r * temp1.r - alpha->i * temp1.i, 
				    q__2.i = alpha->r * temp1.i + alpha->i * 
				    temp1.r;
			    r_cnjg(&q__4, alpha);
			    q__3.r = q__4.r * temp2.r - q__4.i * temp2.i, 
				    q__3.i = q__4.r * temp2.i + q__4.i * 
				    temp2.r;
			    q__1.r = q__2.r + q__3.r, q__1.i = q__2.i + 
				    q__3.i;
			    r__1 = *beta * c__[i__4].r + q__1.r;
			    c__[i__3].r = r__1, c__[i__3].i = 0.f;
			}
		    } else {
			if (*beta == 0.f) {
			    i__3 = c___subscr(i__, j);
			    q__2.r = alpha->r * temp1.r - alpha->i * temp1.i, 
				    q__2.i = alpha->r * temp1.i + alpha->i * 
				    temp1.r;
			    r_cnjg(&q__4, alpha);
			    q__3.r = q__4.r * temp2.r - q__4.i * temp2.i, 
				    q__3.i = q__4.r * temp2.i + q__4.i * 
				    temp2.r;
			    q__1.r = q__2.r + q__3.r, q__1.i = q__2.i + 
				    q__3.i;
			    c__[i__3].r = q__1.r, c__[i__3].i = q__1.i;
			} else {
			    i__3 = c___subscr(i__, j);
			    i__4 = c___subscr(i__, j);
			    q__3.r = *beta * c__[i__4].r, q__3.i = *beta * 
				    c__[i__4].i;
			    q__4.r = alpha->r * temp1.r - alpha->i * temp1.i, 
				    q__4.i = alpha->r * temp1.i + alpha->i * 
				    temp1.r;
			    q__2.r = q__3.r + q__4.r, q__2.i = q__3.i + 
				    q__4.i;
			    r_cnjg(&q__6, alpha);
			    q__5.r = q__6.r * temp2.r - q__6.i * temp2.i, 
				    q__5.i = q__6.r * temp2.i + q__6.i * 
				    temp2.r;
			    q__1.r = q__2.r + q__5.r, q__1.i = q__2.i + 
				    q__5.i;
			    c__[i__3].r = q__1.r, c__[i__3].i = q__1.i;
			}
		    }
/* L230: */
		}
/* L240: */
	    }
	}
    }
    return 0;
/*     End of CHER2K. */
} /* cher2k_ */
#undef c___ref
#undef c___subscr
#undef b_ref
#undef b_subscr
#undef a_ref
#undef a_subscr

