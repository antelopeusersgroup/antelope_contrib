      subroutine acal(a,n1dim,nrow,nfunc,wk,dz) 
c---------------------------------------------------------------------
c      acal calculates the weights used in the sum for obtaining
c  model estimates in the backus gilbert inversion method for the 
c  special case of integration quelling with orthonormal kernel 
c  functions.  in this case the weights can be calculated as simple 
c  sums.  the algorithm used here is a space economizer in that the 
c  matrix of weights will overwrite the input matrix of discretized 
c  kernels functions. 
c 
c  arguments- 
c     a-an nrow x nfunc matrix  
c       on input a is assumed to contain the discretized orthonormal
c       kernels functions in it's first nfunc columns.  these functions 
c       are assumed to have been weighted by subroutine weight previous 
c       to entry into this subroutine.  on output a will contain the
c       weights.  a(i,j) will contain the weight appropriate for the
c       jth function at the ith depth point.
c     n1dim-first dimension of a in calling program 
c     nrow-number of rows in a  
c     nfunc-number of columns in a
c     wk-work vector of length at least nrow
c 
c  modifications- 
c 
c     june,1980-replaced earlier version that did not properly
c     take account of endpoint conditions but blindly did only a
c     simple sum.  this version is now restricted to use with 
c     quadrature weights appropriate to simpson's rule. 
c 
c---------------------------------------------------------------------
      dimension a(n1dim,nfunc),wk(nrow) 
c--inc is used by the blas
      data inc/1/ 
      do 100 i=1,nrow 
  100      wk(i) = 1.0
      call weight(wk,nrow,nrow,1,dz)
      do 200 j=1,nfunc
           do 200 i=1,nrow
  200           a(i,j) = wk(i)*a(i,j) 
c--now sum and shift  
c--endpoint corrections dependent upon the quadrature scheme  
c--being used.  this version uses simpson's rule with 
c--trapezoidal endpoint corrections at even numbered points.  
      do 400 j=1,nfunc
           call scopy(nrow,a(1,j),inc,wk,inc) 
           a(1,j)=0.0 
c--do odd numbered points first.  they require no correction with 
c--simpson's rule.
           wk(1)=2.0*wk(1)
           do 300 i=3,nrow,2
  300           a(i,j)=a(i-2,j)+wk(i-1)+(wk(i)+wk(i-2))/2.0 
c--now pick up odd numbered points by using a trapezoidal endpoint. 
           do 350 i=2,nrow,2
  350           a(i,j) = a(i-1,j) + 0.75*wk(i-1) + 0.375*wk(i)
  400 continue
      return
      end 

c $Id$ 
