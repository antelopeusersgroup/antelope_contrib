      subroutine vscal(n,a,x,incx,y,incy) 
c---------------------------------------------------------------------- 
c       scales vector y by vector ax.  that is components of y(i) are 
c  overwritten by a*x(i)*y(i) where a is a constant.
c 
c  arguments- 
c 
c     n-length of x and y 
c     a-scaling constant
c     x-scaling vector (left unaltered) 
c     incx-increment parameter used by array processor or machines
c          utilizing virtual memory (currently not used but included
c          for compatiblity with standard blas. 
c     y-vector to be scaled.  on output components of y(i) are
c       overwritten by a*x(i)*y(i). 
c     incy-similar to incx.  (see above) (also currently not used)
c---------------------------------------------------------------------- 
      dimension x(n),y(n) 
      do 100 i=1,n
  100      y(i) = a*x(i)*y(i) 
      return
      end 

c $Id$ 
