      subroutine mtrxmul(a,b,n1a,n2a,n1b,l,m,n,wk)
c---------------------------------------------------------------------- 
c  multiplication-full storage mode, space economizer solution
c  utilizing blas.  calculates the matrix product a(transpose)b with
c  the product overwriting b.  inner products are accumulated in
c  double precision.
c 
c  arguments- 
c 
c     a-first matrix(a is mxl and hence a(traspose) is lxm) 
c     b-second matrix (b is mxn)
c       (on output b will contain the lxn matrix a(trans)b. 
c        note-calling program should test to be certain l is less than  
c             or equal to n1b)  
c     n1a-first dimension of a in calling program 
c     n2a-second dimension of a in calling program  
c     n1b-first dimension of b in calling program 
c     l-number of columns in a  
c     m-number of rows in a and b 
c     n-number of columns in b. 
c     wk-vector work space of length at least l.
c 
c  this program requires a library that contains the blas subroutines.
c---------------------------------------------------------------------- 
      dimension a(n1a,n2a),b(n1b,n),wk(l) 
c--the value of inc is used by the blas for greater efficiency with 
c--array processors.  the ideal value is machine dependent. 
      data inc/1/ 
      do 200 j=1,n
           do 100 i=1,l 
  100           wk(i) = sdot(m,a(1,i),inc,b(1,j),inc) 
  200      call scopy(l,wk,inc,b(1,j),inc)
      return
      end 

c $Id$ 
