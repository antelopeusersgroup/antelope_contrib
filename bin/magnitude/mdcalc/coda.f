c
c input:              * ****** **** ****** *******
      subroutine coda(y,jstart,jend,xnoise,xfactor,jcoda,r,jerr)
c output:                                          +++++ ++++
c
c  November 1989, trying to determine coda lengths
c  at UNR in a manner consistent with former practice.
c  Given the array Y; Fit an exponential on the samples
c  between JSTART and JEND.  Fit an exponential to RMS
c  estimates (window length NRMS) in this range.  Return
c  as JCODA the sample number at which this exponential
c  attains an amplitude twice XNOISE, the pre-event noise
c  estimate.  JERR = 0 if procedure works.  XFACTOR
c  defines the "end" of the coda.  The subroutine tries
c  to find the point at which the coda has fallen to a
c  level which is "xfactor" times the amplitude of the
c  pre-event noise.  Experimentation shows that good numbers
c  appear to be to compute the averages over 300 points
c  and to take the start of the coda for the calculation
c  to be well back of the S wave, and certainly back of
c  anyplace where the record is clipped.  For UNR, in order
c  to produce numbers similar to those obtained on the
c  PING system, it seems that XFACTOR should be about 6.
c
      real x(256),yy(256)
      real*4 y(*)
      nave = 300
      n = jend - jstart - 1
c     write(6,*) 'n = ',n
      if(n.lt.500)then
        jerr=-2
        return
      endif
c
      call rms(y,jstart + nave/2+1,nave,temp1)
      call rms(y,jend   - nave/2+1,nave,temp2)
      yy(1) = alog(temp1) 
      yy(2) = alog(temp2)
      x(1) = float(jstart+nave/2+1)
      x(2) = float(jend-nave/2-1)
      noffs = nave/2 + 1
      nr = 2
      samps = float(jend-jstart-nave)
c     write(6,*) 'samps = ',samps
c
c  This many iterations seems to give enough stability
c
      do j=1,6
        dx = samps/float(2**(j))
        ncells = 2**j
        npercell = (n-nave)/ncells
        do k=1,2**(j-1)
          nr = nr+1
          index = jstart + noffs + (2*k-1)*npercell
          x(nr) = float(index)
          call rms(y,index,nave,temp)
          yy(nr) = alog(temp) 
        enddo
      enddo
      call lstsqr(x,yy,nr,b,a,r)
      arg = xfactor*xnoise
      if(arg.le.0.)then
        write(6,*)' Cannot compute coda, arg not positive: ',arg
        return
      endif
      jcoda = int( (alog(arg) - b)/a )      
      jerr = 0
      return
      end
