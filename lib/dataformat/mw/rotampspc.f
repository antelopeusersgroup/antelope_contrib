      subroutine rotampspc(nyq,ampspc,freqhz,expon)
c
c  subroutine to differentiate or integrate an amplitude spectrum.
c
c  inputs:
c    nyq     - number of frequency points
c    ampspec - amplitude spectrum
c    freqhz  - frequencies in hertz
c    exp     - exponent to rotate spectrum
c              -2 integrate twice
c              -1 integrate once
c              +1 differentiate once
c              +2 differentiate twice
c
c
      dimension ampspc(1),freqhz(1)
      logical lzero
      parameter (pi=3.14159265,tpi=2.*pi)
      data lzero/.false./
c
      if (expon .eq. 0.) return
      nstart = 1
      if (freqhz(1).eq.0.) then
        nstart = 2
        lzero  = .true.
      end if
      do 100 ifrq = nstart,nyq
        factor = expon * log( tpi * freqhz(ifrq))
        ampspc(ifrq) =  factor + log(ampspc(ifrq))
        ampspc(ifrq) =  exp(ampspc(ifrq))
  100 continue
      if (.not.lzero) return
      if (expon.gt.0) ampspc(1) = ampspc(2)
      if (expon.lt.0) ampspc(1) = 0.
      return
      end

c $Id$ 
