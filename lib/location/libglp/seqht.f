      subroutine seqht(a,lda,m,n,l,b,ldb,ncb) 
c------------------------------------------------------------------ 
c        does sequential reduction of a matrix to upper triangular
c  form using householder transformations.  this algorithm is 
c  a fortran implimentation of an algorithm by the same name published  
c  in "solving least squares problems", lawson and hanson,
c  prentice hall, 1974.  this routine does steps 5 and 6 of their 
c  algorithm with one exception.  this version does not calculate 
c  sum of squared residuals but instead projects b onto null of a.
c  consequently b is assumed to lie in array b.  multiple right 
c  hand sides are allowed through parameter ncb (see below).  
c 
c  arguments- 
c     a   - m by n array to be reduced to upper triangular form.
c           m .lt. n case is accepted in which case a is reduced to 
c           an upper trapezoidal matrix.  
c     lda - leading dimension of a in calling program.
c     m   - total number of rows in a.
c     n   - number of columns in a. 
c     l   - routine assumes rows 1 to l have already been reduced 
c           to upper trapezoidal form by previous calls to this or a
c           similar routine.
c     b   - array of right hand side vectors.  on output b is 
c           overwritten by the matrix product q(transpose)*b where
c           q is the matrix defined by householder transformations
c           used to reduce a to upper trapezoidal form. 
c     ldb - leading dimension of b in calling program.
c     ncb - number of columns in array b = number of right hand sides 
c 
c  required subprograms-
c    h12 - householder transformation code from lawson and hanson.
c    test0 - tests for zero columns or rows 
c  language - 1977 ansi standard fortran  
c  author   - gary l. pavlis
c  written  - may 1981
c------------------------------------------------------------------ 
      real a(lda,n),b(ldb,ncb)
      integer i,lone  
      real up 
      external test0  
      logical test0 
      do 100 i=1,min(n,m-1) 
            lone = max(i+1,l+1) 
            if(.not.test0(m-lone+1,a(lone,i),1)) then 
                  call h12(1,i,lone,m,a(1,i),1,up,a(1,i+1),1,lda,n-i) 
                  call h12(2,i,lone,m,a(1,i),1,up,b,1,ldb,ncb)
            endif 
  100 continue
      return
      end 

c $Id$ 
