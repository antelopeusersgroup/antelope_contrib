#include "blaswrap.h"
#include "f2c.h"

/* Subroutine */ int dlahrd_(integer *n, integer *k, integer *nb, doublereal *
	a, integer *lda, doublereal *tau, doublereal *t, integer *ldt, 
	doublereal *y, integer *ldy)
{
/*  -- LAPACK auxiliary routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       June 30, 1999   


    Purpose   
    =======   

    DLAHRD reduces the first NB columns of a real general n-by-(n-k+1)   
    matrix A so that elements below the k-th subdiagonal are zero. The   
    reduction is performed by an orthogonal similarity transformation   
    Q' * A * Q. The routine returns the matrices V and T which determine   
    Q as a block reflector I - V*T*V', and also the matrix Y = A * V * T.   

    This is an auxiliary routine called by DGEHRD.   

    Arguments   
    =========   

    N       (input) INTEGER   
            The order of the matrix A.   

    K       (input) INTEGER   
            The offset for the reduction. Elements below the k-th   
            subdiagonal in the first NB columns are reduced to zero.   

    NB      (input) INTEGER   
            The number of columns to be reduced.   

    A       (input/output) DOUBLE PRECISION array, dimension (LDA,N-K+1)   
            On entry, the n-by-(n-k+1) general matrix A.   
            On exit, the elements on and above the k-th subdiagonal in   
            the first NB columns are overwritten with the corresponding   
            elements of the reduced matrix; the elements below the k-th   
            subdiagonal, with the array TAU, represent the matrix Q as a   
            product of elementary reflectors. The other columns of A are   
            unchanged. See Further Details.   

    LDA     (input) INTEGER   
            The leading dimension of the array A.  LDA >= max(1,N).   

    TAU     (output) DOUBLE PRECISION array, dimension (NB)   
            The scalar factors of the elementary reflectors. See Further   
            Details.   

    T       (output) DOUBLE PRECISION array, dimension (LDT,NB)   
            The upper triangular matrix T.   

    LDT     (input) INTEGER   
            The leading dimension of the array T.  LDT >= NB.   

    Y       (output) DOUBLE PRECISION array, dimension (LDY,NB)   
            The n-by-nb matrix Y.   

    LDY     (input) INTEGER   
            The leading dimension of the array Y. LDY >= N.   

    Further Details   
    ===============   

    The matrix Q is represented as a product of nb elementary reflectors   

       Q = H(1) H(2) . . . H(nb).   

    Each H(i) has the form   

       H(i) = I - tau * v * v'   

    where tau is a real scalar, and v is a real vector with   
    v(1:i+k-1) = 0, v(i+k) = 1; v(i+k+1:n) is stored on exit in   
    A(i+k+1:n,i), and tau in TAU(i).   

    The elements of the vectors v together form the (n-k+1)-by-nb matrix   
    V which is needed, with T and Y, to apply the transformation to the   
    unreduced part of the matrix, using an update of the form:   
    A := (I - V*T*V') * (A - Y*V').   

    The contents of A on exit are illustrated by the following example   
    with n = 7, k = 3 and nb = 2:   

       ( a   h   a   a   a )   
       ( a   h   a   a   a )   
       ( a   h   a   a   a )   
       ( h   h   a   a   a )   
       ( v1  h   a   a   a )   
       ( v1  v2  a   a   a )   
       ( v1  v2  a   a   a )   

    where a denotes an element of the original matrix A, h denotes a   
    modified element of the upper Hessenberg matrix H, and vi denotes an   
    element of the vector defining H(i).   

    =====================================================================   


       Quick return if possible   

       Parameter adjustments */
    /* Table of constant values */
    static doublereal c_b4 = -1.;
    static doublereal c_b5 = 1.;
    static integer c__1 = 1;
    static doublereal c_b38 = 0.;
    
    /* System generated locals */
    integer a_dim1, a_offset, t_dim1, t_offset, y_dim1, y_offset, i__1, i__2, 
	    i__3;
    doublereal d__1;
    /* Local variables */
    static integer i__;
    extern /* Subroutine */ int dscal_(integer *, doublereal *, doublereal *, 
	    integer *), dgemv_(char *, integer *, integer *, doublereal *, 
	    doublereal *, integer *, doublereal *, integer *, doublereal *, 
	    doublereal *, integer *), dcopy_(integer *, doublereal *, 
	    integer *, doublereal *, integer *), daxpy_(integer *, doublereal 
	    *, doublereal *, integer *, doublereal *, integer *), dtrmv_(char 
	    *, char *, char *, integer *, doublereal *, integer *, doublereal 
	    *, integer *);
    static doublereal ei;
    extern /* Subroutine */ int dlarfg_(integer *, doublereal *, doublereal *,
	     integer *, doublereal *);
#define a_ref(a_1,a_2) a[(a_2)*a_dim1 + a_1]
#define t_ref(a_1,a_2) t[(a_2)*t_dim1 + a_1]
#define y_ref(a_1,a_2) y[(a_2)*y_dim1 + a_1]


    --tau;
    a_dim1 = *lda;
    a_offset = 1 + a_dim1 * 1;
    a -= a_offset;
    t_dim1 = *ldt;
    t_offset = 1 + t_dim1 * 1;
    t -= t_offset;
    y_dim1 = *ldy;
    y_offset = 1 + y_dim1 * 1;
    y -= y_offset;

    /* Function Body */
    if (*n <= 1) {
	return 0;
    }

    i__1 = *nb;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (i__ > 1) {

/*           Update A(1:n,i)   

             Compute i-th column of A - Y * V' */

	    i__2 = i__ - 1;
	    dgemv_("No transpose", n, &i__2, &c_b4, &y[y_offset], ldy, &a_ref(
		    *k + i__ - 1, 1), lda, &c_b5, &a_ref(1, i__), &c__1);

/*           Apply I - V * T' * V' to this column (call it b) from the   
             left, using the last column of T as workspace   

             Let  V = ( V1 )   and   b = ( b1 )   (first I-1 rows)   
                      ( V2 )             ( b2 )   

             where V1 is unit lower triangular   

             w := V1' * b1 */

	    i__2 = i__ - 1;
	    dcopy_(&i__2, &a_ref(*k + 1, i__), &c__1, &t_ref(1, *nb), &c__1);
	    i__2 = i__ - 1;
	    dtrmv_("Lower", "Transpose", "Unit", &i__2, &a_ref(*k + 1, 1), 
		    lda, &t_ref(1, *nb), &c__1);

/*           w := w + V2'*b2 */

	    i__2 = *n - *k - i__ + 1;
	    i__3 = i__ - 1;
	    dgemv_("Transpose", &i__2, &i__3, &c_b5, &a_ref(*k + i__, 1), lda,
		     &a_ref(*k + i__, i__), &c__1, &c_b5, &t_ref(1, *nb), &
		    c__1);

/*           w := T'*w */

	    i__2 = i__ - 1;
	    dtrmv_("Upper", "Transpose", "Non-unit", &i__2, &t[t_offset], ldt,
		     &t_ref(1, *nb), &c__1);

/*           b2 := b2 - V2*w */

	    i__2 = *n - *k - i__ + 1;
	    i__3 = i__ - 1;
	    dgemv_("No transpose", &i__2, &i__3, &c_b4, &a_ref(*k + i__, 1), 
		    lda, &t_ref(1, *nb), &c__1, &c_b5, &a_ref(*k + i__, i__), 
		    &c__1);

/*           b1 := b1 - V1*w */

	    i__2 = i__ - 1;
	    dtrmv_("Lower", "No transpose", "Unit", &i__2, &a_ref(*k + 1, 1), 
		    lda, &t_ref(1, *nb), &c__1);
	    i__2 = i__ - 1;
	    daxpy_(&i__2, &c_b4, &t_ref(1, *nb), &c__1, &a_ref(*k + 1, i__), &
		    c__1);

	    a_ref(*k + i__ - 1, i__ - 1) = ei;
	}

/*        Generate the elementary reflector H(i) to annihilate   
          A(k+i+1:n,i)   

   Computing MIN */
	i__2 = *k + i__ + 1;
	i__3 = *n - *k - i__ + 1;
	dlarfg_(&i__3, &a_ref(*k + i__, i__), &a_ref(min(i__2,*n), i__), &
		c__1, &tau[i__]);
	ei = a_ref(*k + i__, i__);
	a_ref(*k + i__, i__) = 1.;

/*        Compute  Y(1:n,i) */

	i__2 = *n - *k - i__ + 1;
	dgemv_("No transpose", n, &i__2, &c_b5, &a_ref(1, i__ + 1), lda, &
		a_ref(*k + i__, i__), &c__1, &c_b38, &y_ref(1, i__), &c__1);
	i__2 = *n - *k - i__ + 1;
	i__3 = i__ - 1;
	dgemv_("Transpose", &i__2, &i__3, &c_b5, &a_ref(*k + i__, 1), lda, &
		a_ref(*k + i__, i__), &c__1, &c_b38, &t_ref(1, i__), &c__1);
	i__2 = i__ - 1;
	dgemv_("No transpose", n, &i__2, &c_b4, &y[y_offset], ldy, &t_ref(1, 
		i__), &c__1, &c_b5, &y_ref(1, i__), &c__1);
	dscal_(n, &tau[i__], &y_ref(1, i__), &c__1);

/*        Compute T(1:i,i) */

	i__2 = i__ - 1;
	d__1 = -tau[i__];
	dscal_(&i__2, &d__1, &t_ref(1, i__), &c__1);
	i__2 = i__ - 1;
	dtrmv_("Upper", "No transpose", "Non-unit", &i__2, &t[t_offset], ldt, 
		&t_ref(1, i__), &c__1);
	t_ref(i__, i__) = tau[i__];

/* L10: */
    }
    a_ref(*k + *nb, *nb) = ei;

    return 0;

/*     End of DLAHRD */

} /* dlahrd_ */

#undef y_ref
#undef t_ref
#undef a_ref


