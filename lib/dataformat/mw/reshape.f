      subroutine reshape(npts,nfft,kspec,kl,mxdata,dpss,il,xmul,
     $                   mxfreq,yk,wsave,dpsw,mxlin,slin)
c
c  reshapes eigen-ft's around significant spectral lines
c
c  inputs:
c    npts  - number of data points
c    nfft  - number of points in fft's
c    kspec - number of windows
c    kl    - number of spectral line components
c    mxdata- leading dimension of dpss
c    dpss  - double precision array containing kspec tapers
c            of length npts
c    il    - integer vector containing indices of the lines
c    xmul  - complex vector containing the mean values of the lines
c    mxfreq- leading dimension of yk
c    yk    - complex array containing ffts of tapered data
c    wsave - real work vector of dimension at least 4*nfft+15 that
c            contains the fft complex exponentials. these are
c            assumed to have been computed by calling cffti.
c    dpsw  - real work vector of dimension at least 4*nfft
c    mxlin - leading dimension of slin
c
c  outputs:
c    slin  - real array containing total power removed from
c            kspec eigen-ft's at kl frequencies
c
c  calls cfftf
c
c*******************************************************************
c
      double precision dpss,sum1,sum2
      complex yk,t0,t1,xmul,dpsw
      dimension il(kl),wsave(4*nfft+15),yk(mxfreq,kspec),xmul(kl),
     $          dpsw(nfft,2),slin(mxlin,kspec),dpss(mxdata,kspec)
      nfft2=nfft/2
c
c  compute discrete prolate spheroidal wavefunctions by taking
c  fft's of the real dpss.
c  the fft's are computed in pairs by placing adjacent dpss's in the
c  real and imaginary parts of dpsw and sorting the result using
c  the symmetry properties of the fft.
c  the dpsw are normalized so that the integral from -1/2 to 1/2 of
c  dpsw**2 is one. since the fft is unnormalized and the sum of the
c  squares of the dpss is one, this condition is automatically met.
      do 40 j=1,kspec-1,2
        do 10 i=1,npts
   10     dpsw(i,1)=cmplx(dpss(i,j),dpss(i,j+1))
        do 11 i=npts+1,nfft
   11     dpsw(i,1)=0.
        call cfftf(nfft,dpsw,wsave)
        dpsw(1,2)=cmplx(aimag(dpsw(1,1)),0.)
        dpsw(1,1)=cmplx(real(dpsw(1,1)),0.)
        do 20 i=2,nfft2+1
          dpsw(i,2)=cmplx(0.,-0.5)*(dpsw(i,1)-conjg(dpsw(nfft-i+2,1)))
   20     dpsw(i,1)=0.5*(dpsw(i,1)+conjg(dpsw(nfft-i+2,1)))
cvd$ nodepchk
        do 21 i=nfft2+2,nfft
          dpsw(i,1)=conjg(dpsw(nfft-i+2,1))
   21     dpsw(i,2)=conjg(dpsw(nfft-i+2,2))
c  compute and remove mean value for each spectral line
        do 40 k=1,kl
          sum1=0.
          sum2=0.
          do 30 i=1,nfft
            ii=i-il(k)+1
            if(ii.lt.1)ii=ii+nfft
            t0=xmul(k)*dpsw(ii,1)
            yk(i,j)=yk(i,j)-t0
            t1=xmul(k)*dpsw(ii,2)
            yk(i,j+1)=yk(i,j+1)-t1
            sum1=sum1+abs(t0)**2+2.*real(t0*conjg(yk(i,j)))
   30       sum2=sum2+abs(t1)**2+2.*real(t1*conjg(yk(i,j+1)))
          slin(k,j)=sum1
   40     slin(k,j+1)=sum2
c  reshape last eigen-ft if kspec is odd
      if(mod(kspec,2).eq.0)return
      do 60 i=1,npts
   60   dpsw(i,1)=cmplx(dpss(i,kspec),0.d0)
      do 61 i=npts+1,nfft
   61   dpsw(i,1)=0.
      call cfftf(nfft,dpsw,wsave)
      do 80 k=1,kl
        sum1=0.
        do 70 i=1,nfft
          ii=i-il(k)+1
          if(ii.lt.1)ii=ii+nfft
          t0=xmul(k)*dpsw(ii,1)
          yk(i,kspec)=yk(i,kspec)-t0
   70     sum1=sum1+abs(t0)**2+2.*real(t0*conjg(yk(i,kspec)))
   80   slin(k,kspec)=sum1
      return
      end

c $Id$ 
