      subroutine mwtr(ne,kspec,nser,jack,evalu,mxfreq,mxspec,
     $                yk,mxwt,wt,work,c2,trans,c2lo,c2hi,trerr)
c
c  subroutine to compute transfer function and squared multiple
c  coherence between n input and one output time series using
c  multiple prolate window expansion along with jackknife estimates
c  of their standard error and 95% confidence limits respectively
c
c  inputs:
c    ne     - number of elements in yk to process
c    kspec  - number of windows
c    nser   - number of series including output
c    jack   - logical switch for computation of jackknife error
c             estimates on transfer functions and coherence,
c             .true. to compute
c    evalu  - double precision vector of the dpss eigenvalues
c    mxfreq - leading dimension of yk
c    mxspec - second dimension of yk
c    yk     - complex array containing kspec ffts
c             for the nser time series
c    mxwt   - leading dimension of wt, trans, and trerr
c    wt     - real array containing normalized adaptive weights
c    work   - work vector of length at least 10*kspec*nser+
c             8*kspec+9*nser+8*nser**2
c
c  outputs:
c
c    c2     - squared multiple coherence
c    trans  - complex array containing the transfer functions
c             between the output and nser-1 input time series
c    c2lo   - lower 95% confidence limit on the coherence
c    c2hi   - upper 95% confidence limit on the coherence
c    trerr  - one standard error for the transfer functions
c
c  calls mwtr1
c
c*****************************************************************
c
      double precision evalu
      complex yk,trans
      logical jack
      dimension evalu(kspec),yk(mxfreq,mxspec,nser),
     $          wt(mxwt,mxspec,nser),work(1),c2(ne),
     $          trans(mxwt,nser-1),c2lo(ne),
     $          c2hi(ne),trerr(mxwt,nser-1)
      nsqev=2*kspec
      na=nsqev+8*kspec*nser
      nb=na+4*kspec
      nx=nb+4*nser
      nrr=nx+8*nser*nser
      ntrd1=nrr+2*nser*kspec
      ntrd1s=ntrd1+2*nser
      ntrd1ws=ntrd1s+2*nser
      nwtj=ntrd1ws+kspec
      call mwtr1(ne,kspec,nser,jack,evalu,mxfreq,mxspec,yk,mxwt,
     $           wt,work,work(nsqev+1),work(na+1),
     $           work(nb+1),work(nx+1),work(nrr+1),work(ntrd1+1),
     $           work(ntrd1s+1),work(ntrd1ws+1),
     $           work(nwtj+1),c2,trans,c2lo,c2hi,trerr)
      return
      end

c $Id$ 
