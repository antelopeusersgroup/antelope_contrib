      subroutine mwsvd(nf,kspec,nser,jack,scal,swch,evalu,mxfreq,
     $                mxspec,yk,mxwt,wt,work,mxser,sv2,sv2j,apc,
     $                apcj,ppc,ppcj,c2,c2lo,c2hi)
c
c  subroutine to compute principal components or eofs of the
c  spectral matrix obtained with multiple window expansion
c  using the singular value decomposition, along with jackknife
c  estimates of their standard error.
c
c  inputs:
c    nf     - index of the frequency to analyze
c    kspec  - number of windows
c    nser   - number of series
c    jack   - logical switch for computation of jackknife error
c             estimates, .true. to compute
c    scal   - logical switch for normalization of the columns of
c             the spectral matrix by the power, .true. for normalization
c    swch   - logical switch, set to .true. to compute singular values
c             and vectors, .false. to only compute singular values
c    evalu  - double precision vector of the dpss eigenvalues
c    mxfreq - leading dimension of yk
c    mxspec - second dimension of yk and wt
c    yk     - complex array containing kspec ffts
c             for the nser time series
c    mxwt   - leading dimension of wt
c    wt     - real array containing normalized adaptive weights
c    work   - work vector of length at least 6*nser*kspec+
c             7*kspec+10*nser+4*nser**2*(1+kspec)
c    mxser  - leading dimension of apc,apcj,ppc,ppcj,c2,c2lo,
c             and c2hi
c
c  outputs:
c
c    sv2    - real vector containing nser squared singular values
c    sv2j   - real vector containing standard error estimates on sv2
c    apc    - real array containing nser eof amplitude functions for
c             the nser series
c    apcj   - real array containing nser estimates of the standard error
c             of the eof amplitude functions for the nser series
c    ppc    - real array containing nser eof phases for the nser series
c    ppcj   - real array containing nser estimates of the standard
c             error of the eof phase for the nser series
c    c2     - real array containing the squared coherence between
c             the nser eofs and the nser series
c    c2lo   - real array containing the lower 95% confidence limits
c             on c2
c    c2hi   - real array containing the upper 95% confidence limit
c             on c2
c
c  calls mwsvd1
c
c*****************************************************************
c
      double precision evalu
      complex yk
      logical jack,scal,swch
      dimension evalu(kspec),yk(mxfreq,mxspec,nser),
     $          wt(mxwt,mxspec,nser),work(1),sv2(nser),sv2j(nser),
     $          apc(mxser,nser),apcj(mxser,nser),ppc(mxser,nser),
     $          ppcj(mxser,nser),c2(mxser,nser),
     $          c2lo(mxser,nser),c2hi(mxser,nser)
      nsqev=2*kspec
      nx=nsqev+4*kspec*nser
      nwork=nx+4*kspec
      ns=nwork+4*nser
      ne=ns+4*nser
      nv=ne+4*nser*nser
      nvd=nv+4*nser*nser*kspec
      nsv=nvd+nser
      nsvd=nsv+nser*kspec
      nspec=nsvd+nser
      nspecd=nspec+nser*kspec
      call mwsvd1(nf,kspec,nser,jack,scal,swch,evalu,mxfreq,mxspec,
     $            yk,mxwt,wt,mxser,work,work(nsqev+1),work(nx+1),
     $            work(nwork+1),work(ns+1),work(ne+1),work(nv+1),
     $            work(nvd+1),work(nsv+1),work(nsvd+1),work(nspec+1),
     $            work(nspecd+1),sv2,sv2j,apc,apcj,ppc,ppcj,
     $            c2,c2lo,c2hi)
      return
      end

c $Id$ 
