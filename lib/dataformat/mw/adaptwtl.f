      subroutine adaptwtl(nfft,nlin,kspec,dvar,evalu1,ilin,
     $                    mxlin,slin,work,specl,sel,wtl,errl)
c
c  subroutine to calculate adaptively weighted power spectrum
c  and jackknifed 95% confidence limits for line components
c
c  inputs:
c    nfft  - length of fft
c    nlin  - number of lines
c    kspec - number of prolate windows used
c    dvar  - variance of data
c    evalu1- double precision vector containing 1 minus eigenvalues
c    ilin  - vector of indices for lines
c    mxlin - leading dimension of slin and wt
c    slin  - real array containing power in each line for each window
c    work  - work vector of length 8*kspec
c
c  outputs:
c    specl- real vector containing adaptively weighted spectrum
c    sel  - real vector containing the number of degrees of freedom
c           for the estimate at each frequency
c    wtl  - real array containing the nlin weights normalized so
c           that the sum of squares over the kspec estimates is one

c    errl - real vector of jackknife error estimates
c
c  calls adaptwtl1
c
c****************************************************************
c
      double precision evalu1
      dimension evalu1(kspec),slin(mxlin,kspec),
     $          sel(nlin),specl(nlin),wtl(mxlin,kspec),
     $          errl(nlin),work(8*kspec),ilin(nlin)
      call adaptwtl1(nfft,kspec,nlin,dvar,evalu1,ilin,mxlin,slin,
     $               work,work(2*kspec+1),work(4*kspec+1),
     $               work(5*kspec+1),work(6*kspec+1),work(7*kspec+1),
     $               specl,sel,wtl,errl)
      return
      end

c $Id$ 
