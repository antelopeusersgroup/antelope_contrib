      subroutine coher2(ne,kspec,evalu,mxfreq,mxspec,yk,spec,
     $                  mxwt,wt,work,c2,phase,c2lo,c2hi,pherr)
c
c  subroutine to calculate magnitude squared coherence and phase
c  between two series using multiple prolate window expansion
c  along with jackknife estimates of their 95% confidence limits
c
c  inputs:
c    ne     - number of elements in yk to process
c    kspec  - number of windows 
c    evalu  - double precision vector containing the eigenvalues
c    mxfreq - leading dimension of yk
c    mxspec - second dimension of yk
c    yk     - complex array containing kspec ffts for the two
c             time series
c    spec   - real array containing adaptively weighted spectra for
c             the two time series
c    mxwt   - leading dimension of wt
c    wt     - real array containing the normalized weights
c    work   - work vector of length at least 5*kspec+2
c
c  outputs:
c    c2     - magnitude squared coherence
c    phase  - phase in radians 
c    c2lo   - jackknife estimate of coherence 95% lower confidence limit
c    c2hi   - jackknife estimate of coherence 95% upper confidence limit
c    pherr  - jackknife estimate of phase standard error
c
c  calls coher21
c
c**************************************************************
c
      double precision evalu
      complex yk
      dimension evalu(kspec),yk(mxfreq,mxspec,2),
     $          spec(mxfreq,2),wt(mxwt,mxspec,2),
     $          work(5*kspec+2),c2(ne),phase(ne),
     $          c2lo(ne),c2hi(ne),pherr(ne)
      call coher21(ne,kspec,evalu,mxfreq,mxspec,yk,spec,mxwt,wt,
     $             work,work(2*kspec+1),work(4*kspec+1),
     $             c2,phase,c2lo,c2hi,pherr)
      return
      end

c $Id$ 
