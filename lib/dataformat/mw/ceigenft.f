      subroutine ceigenft(npts,nfft,kspec,mxdata,data,dpss,
     $                    wsave,mxfreq,yk)
c
c  computes eigen-ft's by windowing data with dpss and taking ffts
c  note that fft is unnormalized and window must be normalized such that
c  its sum of squares is one, so that psd=yk**2
c
c  inputs:
c    npts  - number of data points in array data
c    nfft  - length of the fft
c    kspec - number of spectra to compute
c    mxdata- leading dimension of data and dpss
c    data  - real array containing data series
c    dpss  - double precision array containing kspec tapers
c            of length npts
c    wsave - real work vector of length at least 4*nfft+15
c    mxfreq- leading dimension of yk
c
c  outputs:
c    yk    - complex array containing kspec ffts of tapered data
c            of length nfft
c
c  calls cffti,cfftf
c
c****************************************************************
c
      double precision dpss
      complex yk
      dimension data(mxdata,2),wsave(4*nfft+15),dpss(mxdata,kspec),
     $          yk(mxfreq,kspec)
c
      nfft2=nfft/2
      call cffti(nfft,wsave)
      do 20 j=1,kspec
        do 10 i=1,npts
   10     yk(i,j)=dpss(i,j)*cmplx(data(i,1),data(i,2))
        do 11 i=npts+1,nfft
   11     yk(i,j)=0.
   20   call cfftf(nfft,yk(1,j),wsave)
      return
      end

c $Id$ 
