      subroutine ywar(n,nar,data,dphi,x)
c
c  computes ar prewhitening filter
c
c  inputs:
c    n    - number of data values
c    nar  - length of ar filter exclusive of leading 1
c    data - real vector of data
c    dphi - real work vector of length nar+1
c
c  outputs:
c
c   x     - real vector of filter coefficients
c           to convert to an ar filter, a leading 1 is required and
c           the sign of x must be changed
c
c  calls cross
c
      dimension data(1),dphi(0:nar),x(1)
      call cross(n,data,nar,dphi,0.)
      do 20 i=1,nar
   20   dphi(i)=dphi(i)/dphi(0)
      x(1)=dphi(1)
      y=dphi(1)
      s=(1.-y)*(1.+y)
      do 50 k=1,nar-1
        sum=0.
        do 30 j=1,k
   30     sum=sum+x(j)*dphi(k+1-j)
        y=(dphi(k+1)-sum)/s
        x(k+1)=y
        kk=k/2
        if(kk.eq.0)goto 45
        do 40 j=1,kk
          t=x(j)
          x(j)=x(j)-y*x(k+1-j)
  40      x(k+1-j)=x(k+1-j)-y*t
  45    if(2*kk.ne.k)x(kk+1)=x(kk+1)*(1-y)
  50    s=s*(1.-y)*(1.+y)
      return
      end

c $Id$ 
