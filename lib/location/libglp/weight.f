      subroutine weight(a,n1a,nrow,ncol,dx) 
c-------------------------------------------------------------------- 
c     this program weights the a matrix prior to singular value 
c  decomposition in a manner appropriate to the quadrature scheme 
c  used.  this version yields the equivalent of integration by
c  simpson's rule.  the routine assumes the rows of a contain the 
c  discretized functions.  if the number of rows of a is not an odd 
c  number (a requirement of simpson's rule) the effective range over
c  which the integration is performed is decreased by one.  this is 
c  permisable for the travel time problem for which this code was 
c  written but in general it may not be acceptable. 
c 
c  arguments- 
c 
c     a-matrix of discretized functions 
c     n1a-first dimension of a in calling program 
c     nrow-actual number of rows of a 
c     ncol-number of columns in a 
c     dx-discretizing interval  
c  alternate entry point-unweigh
c     unweigh does the inverse of weight.  that is it returns the 
c     functions to their unscaled discretized form  
c-------------------------------------------------------------------- 
      dimension a(n1a,ncol) 
      even=sqrt(4.*dx/3.) 
      odd=sqrt(2.*dx/3.)
      epoint=sqrt(dx/3.)
c--make sure the number of points is odd. 
      if(mod(nrow,2).eq.0) then 
           nrange=nrow-1
      else  
           nrange=nrow
      endif 
c--now do the scaling.
      do 200 j=1,ncol 
           a(1,j)=a(1,j)*epoint 
           do 100 i=2,nrange-1
                if(mod(i,2).eq.0) then
                     a(i,j)=even*a(i,j) 
                else  
                     a(i,j)=odd*a(i,j)
                endif 
  100      continue 
           a(nrange,j)=a(nrange,j)*epoint 
  200 continue
      return
      entry unweigh(a,n1a,nrow,ncol,dx) 
      even=sqrt(4.*dx/3.) 
      odd=sqrt(2.*dx/3.)
      epoint=sqrt(dx/3.)
c--make sure the number of points is odd. 
      if(mod(nrow,2).eq.0) then 
           nrange=nrow-1
      else  
           nrange=nrow
      endif 
c--now do the scaling.
      do 400 j=1,ncol 
           a(1,j)=a(1,j)/epoint 
           do 300 i=2,nrange-1
                if(mod(i,2).eq.0) then
                     a(i,j)=a(i,j)/even 
                else  
                     a(i,j)=a(i,j)/odd
                endif 
  300      continue 
           a(nrange,j)=a(nrange,j)/epoint 
  400 continue
      return
      end 

c $Id$ 
