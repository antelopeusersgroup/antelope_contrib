      subroutine makertr(a,lda,n) 
c---------------------------------------------------------------------- 
c      converts an upper triangular matrix, a, to the matrix  
c  product a(transpose)*a with the product overwritting a.
c  normally, this is an absurd thing to do as the inverse of a
c  can be had directly from the upper triangular form.(cholesky 
c  or qr decomposition give this form.)  this approach is useful
c  for space saving algorthms that accumulate a large ata type
c  matrix for least squares problem in a blockwise fashion
c  to save space.  the algorithm used is more efficient than a
c  pure brute force approach.  routine keeps the values of a in 
c  the upper triangle and writes elements of ata inner products 
c  in the lower triangle of a.  when finished the lower triangle
c  is copied onto the upper triangle destroying the information 
c  that was there.
c 
c  arguments- 
c 
c    a  - n by n upper triangle matrix that on output is replace
c         by the matrix product a(transpose)*a
c    lda- leading dimension of a in calling program.
c    n  - order of a. 
c 
c  required subprograms-
c      sumsq - real function calculates sum of squares of a vector
c              (blas type routine written by glp) 
c      sdot  - blas routine to calculate inner products 
c      cpltut- copies strict lower triangle of a to strict upper
c              triangle.
c  language - 1977 ansi standard fortran (algorithm assumes zero
c             trip do loops)
c  author   - gary l. pavlis
c  written  - may 1981
c 
c---------------------------------------------------------------------- 
      integer lda,n 
      real a(lda,n) 
c--local variables used as loop counters  
      integer i,j 
c--used to hold a(1,1) diagonal element 
      real aone 
      if(n.le.1) return 
c--the first row is treated specially 
      aone = a(1,1) 
      do 100 i=1,n
            a(i,1) = aone*a(1,i)
  100 continue
c--warning the zero trip do loop convention is assumed here.  
      do 200 j=2,n
            do 150 i=j+1,n
                  a(i,j) = sdot(j,a(1,i),1,a(1,j),1)
  150       continue  
            a(j,j) = sumsq(j,a(1,j),1)
  200 continue
      call cpltut(a,lda,n)
      return
      end 

c $Id$ 
