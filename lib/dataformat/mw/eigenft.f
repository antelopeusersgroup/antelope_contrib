      subroutine eigenft(npts,nfft,kspec,data,mxdata,dpss,
     $                   wsave,mxfreq,yk)
c
c  computes eigen-ft's by windowing real data with dpss and taking ffts
c  note that fft is unnormalized and window is such that
c  its sum of squares is one, so that psd=yk**2
c  the fft's are computed in pairs by placing adjacent dpss*data
c  in the real and imaginary parts of fft and sorting the result
c  using the symmetry properties of the fft
c
c  inputs:
c    npts  - number of data points in array data
c    nfft  - length of the fft
c    kspec - number of spectra to compute
c    data  - real array containing data series
c    mxdata- leading dimension of dpss
c    dpss  - double precision array containing kspec tapers
c            of length npts
c    wsave - real work vector of length at least 4*nfft+15
c    mxfreq- leading dimension of yk
c
c  outputs:
c    yk    - complex array containing kspec ffts of tapered data
c            of length nfft each
c
c  calls cffti,cfftf
c
c****************************************************************
c
      double precision dpss
      complex yk
      dimension data(npts),wsave(4*nfft+15),dpss(mxdata,kspec),
     $          yk(mxfreq,kspec)
c
      nfft2=nfft/2
      call cffti(nfft,wsave)
      do 40 j=1,kspec-1,2
        do 10 i=1,npts
   10     yk(i,j)=cmplx(dpss(i,j),dpss(i,j+1))*data(i)
        do 11 i=npts+1,nfft
   11     yk(i,j)=0.
        call cfftf(nfft,yk(1,j),wsave)
        yk(1,j+1)=cmplx(aimag(yk(1,j)),0.)
        yk(1,j)=cmplx(real(yk(1,j)),0.)
        do 20 i=2,nfft2+1	
          yk(i,j+1)=cmplx(0.,-0.5)*(yk(i,j)-conjg(yk(nfft-i+2,j)))
   20     yk(i,j)=0.5*(yk(i,j)+conjg(yk(nfft-i+2,j)))
        do 30 i=nfft2+2,nfft
          yk(i,j)=conjg(yk(nfft-i+2,j))
   30     yk(i,j+1)=conjg(yk(nfft-i+2,j+1))
   40   continue
      if(mod(kspec,2).eq.0)return
c  get last fft if kspec is odd
      do 50 i=1,npts
   50   yk(i,kspec)=cmplx(dpss(i,kspec),0.d0)*data(i)
      do 51 i=npts+1,nfft
   51   yk(i,kspec)=0.
      call cfftf(nfft,yk(1,kspec),wsave)
      return
      end

c $Id$ 
