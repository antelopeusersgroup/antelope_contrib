      subroutine adaptwt(nfft,ne,kspec,dvar,evalu1,mxfreq,yk,
     $                   work,mxwt,spec,se,wt,err,jack)
c
c  subroutine to calculate adaptively weighted power spectrum
c  and jackknifed 95% confidence limits
c
c  inputs:
c    nfft  - number of points in fft's
c    ne    - number of elements in fft to process, normally nfft/2+1
c            for real data and nfft for complex data
c    kspec - number of prolate windows used
c    dvar  - variance of data
c    evalu1- double precision vector containing 1 minus eigenvalues
c    mxfreq- leading dimension of yk
c    yk    - complex array containing kspec ffts of length nfft
c    work  - work vector of length 8*kspec
c    mxwt  - leading dimension of wt
c    jack  - logical variable true if you want jackknife estimates
c
c  outputs:
c    spec - real vector containing adaptively weighted spectrum
c    se   - real vector containing the number of degrees of freedom
c           for the spectral estimate at each frequency.
c    wt   - real array containing the ne weights for kspec 
c           eigenspectra normalized so that the sum of squares over
c           the kspec eigenspectra is one
c    err  - real vector of jackknife error estimates
c
c  calls adaptwt1
c
c****************************************************************
c
      double precision evalu1
      logical jack
      complex yk
      dimension evalu1(kspec),yk(mxfreq,kspec),se(ne),
     $          spec(ne),wt(mxwt,kspec),
     $          err(ne),work(8*kspec)
      call adaptwt1(nfft,ne,kspec,dvar,evalu1,mxfreq,yk,
     $              work,work(2*kspec+1),work(4*kspec+1),
     $              work(5*kspec+1),work(6*kspec+1),
     $              work(7*kspec+1),mxwt,spec,se,wt,err,jack)
      return
      end

c $Id$ 
