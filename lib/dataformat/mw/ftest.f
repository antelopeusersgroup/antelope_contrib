      subroutine ftest(npts,ne,kspec,mxdata,dpss,
     $                  mxfreq,yk,dpsw0,f)
c
c  computes f-test for single spectral line components
c
c
c  inputs:
c    npts  - length of data series
c    ne    - number of elements in yk to test
c    kspec - number of prolate windows
c    mxdata- leading dimension of dpss
c    dpss  - double precision array containing kspec windows 
c            of length npts each
c    mxfreq- leading dimension of yk
c    yk    - complex array containing kspec ffts of tapered data
c            of length nfft each
c    dpsw0 - work vector of length at least 2*kspec
c
c  outputs:
c    f     - vector of f-test values
c
c*****************************************************************
c
      double precision dpss,dp0sum,dpsw0,sumd
      double complex xmu
      complex yk
      dimension dpss(mxdata,kspec),dpsw0(kspec),yk(mxfreq,kspec),
     $          f(ne)
c
      xkspec1=kspec-1
      dp0sum=0.
      do 20 j=1,kspec
        dpsw0(j)=0.
        do 10 i=1,npts
   10     dpsw0(j)=dpsw0(j)+dpss(i,j)
   20   dp0sum=dp0sum+dpsw0(j)**2
      do 50 i=1,ne
        xmu=0.
        do 30 j=1,kspec
   30     xmu=xmu+dpsw0(j)*yk(i,j)
        xmu=xmu/dp0sum
        sumd=0.
        do 40 j=1,kspec
   40     sumd=sumd+abs(yk(i,j)-xmu*dpsw0(j))**2
   50   f(i)=xkspec1*dp0sum*abs(xmu)**2/sumd
      return
      end

c $Id$ 
