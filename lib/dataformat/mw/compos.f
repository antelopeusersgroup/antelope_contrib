      subroutine compos(npts,nfft,ne,kspec,tbw,evalu,mxdata,dpss,se,
     $                  spec,mxfreq,yk,mxwt,wt,work,sc)
c
c  computes composite estimate from free parameter expansion
c
c  inputs:
c    npts   - number entries in dpss
c    nfft   - length of the fft
c    ne     - number of elements to process, must be either <= nfft/2+1
c             or nfft
c    kspec  - number of spectra
c    tbw    - time-bandwidth product
c    evalu  - double precision vector of eigenvalues
c    mxdata - leading dimension of dpss
c    dpss   - double precision array containing kspec tapers
c    se     - real vector of degrees of freedom for each frequency
c    spec   - real vector containing adaptively weighted spectrum
c    mxfreq - leading dimension of yk
c    yk     - complex array containing kspec ffts of length nfft
c    mxwt   - leading dimension of wt
c    wt     - real array containing the normalized adaptive weights
c    work   - real work vector of length 2*(nfft/npts+1)*kspec**2+nfft
c
c  outputs:
c    sc     - real vector containing composite spectrum
c
c  calls compos1
c
      double precision evalu,dpss
      complex yk
      dimension evalu(kspec),dpss(mxdata,kspec),se(ne),spec(ne),
     $          yk(mxfreq,kspec),wt(mxwt,kspec),work(1),sc(ne)
      call compos1(npts,nfft,ne,kspec,tbw,evalu,mxdata,dpss,se,spec,
     $                  mxfreq,yk,mxwt,wt,work,work(nfft+1),sc)
      return
      end

c $Id$ 
