      subroutine sqrf(a,mda,m,n,b,mdb,nb) 
c---------------------------------------------------------------------- 
c       calculates qr factorization of a matrix a by using householder
c  transformations.  on output a is replaced by the upper triangular
c  matrix r and array b is overwritten by the product q(transpose)b.
c  this is a suitable way of solving well conditioned least squares 
c  problems.  (see lawson and hanson, 1974) 
c        this algorithm has only one frill.  vectors used to construct  
c  householder transformations in the reduction are checked to see
c  if all elements of the vector are zero.  a householder 
c  transformation of a zero vector is an identity transformation. 
c  this routine skips these vectors in the reduction since the
c  reduction already complete for that column.  this is primarily 
c  useful for problems where b has many columns as it is more 
c  efficient in that case.
c 
c  arguments- 
c     a  - m by n matrix to be factored.  on output a will contain
c          the upper trapezoidal matrix (upper triangular when
c          m>n ) r of the qr factorization. 
c          ( m>n or n<m permitted ) 
c     mda- leading dimension of a 
c     m  - number of rows in a. 
c     n  - number of columns in a.
c     b  - m by nb array of right hand sides for the least squares
c          problem to be solved.  on output b is overwritten by the 
c          matrix product q(transpose)b.
c     mdb- leading dimension of b.
c     nb - number of columns in b = number of right hand sides to 
c          solve for. 
c 
c  required subprograms 
c     h12  -routine to calculate and implement householder
c           transformations.  (identical to subroutine h12 of 
c           lawson and hanson, 1974)
c     test0-routine used to test for zero vectors.  
c 
c  language - 1977 ansi standard fortran  
c  author   - gary l. pavlis
c  written  - may 1981
c---------------------------------------------------------------------- 
      integer mda,m,n,mdb,nb
      real a(mda,n),b(mdb,nb) 
      external test0  
      logical test0 
c--used to hold scaling factor for householder transformations. 
      real scale
c--j is a loop control variable 
c--l is used to terminate decomposition properly for arbitrary
c--rectangular matrix.
      integer j,l 
      l = min(m-1,n)
c--routine test0 tests for zero vectors.  returns a true value
c--if vector is all zeros.
c--first call to h12 calculates vector needed for implementing
c--a householder transformation and scaling constant. 
c--second call to h12 implements the householder transformation 
c--defined by first call. 
      do 150 j=1,l
            if(.not.test0(m-j+1,a(j,j),1)) then 
                call h12(1,j,j+1,m,a(1,j),1,scale,a(1,j+1),1,mda,n-j) 
                call h12(2,j,j+1,m,a(1,j),1,scale,b,1,mdb,nb) 
            endif 
  150 continue
      return
      end 

c $Id$ 
