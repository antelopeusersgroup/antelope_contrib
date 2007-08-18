      subroutine get_tau(y1,y2,npts,sr,is,js,laglen,tau,ccc,cccp,
     &ireturn)      
c
c     This subroutine cross-correlates seismograms.  
c     It is assumed that waveforms are real*4 in y1 and y2.
c
c     y1,y2:       data series to be cross-correlated
c     npts:        length of the time series
c     sr:          sampling rates of the series
c     is,js:       station numbers for this cross-correlation
c     laglen:      maximum amount (seconds) of offset for cross-correlation
c                      laglen << npts/sr (laglen ~ 0.2 npts/sr is typical)
c     tau:         signal time diff. (epoch seconds) of first - second
c                  (This closely reflects the origin time difference.)
c     ccc:         cross-correlation coefficient (0 <= ccc <= 1)
c     cccp:        relative amplitude of first event over second event
c     ireturn:     error flag (0 means no error)
c                  1 -- real sample rate too far from integer value
c                  2 -- incompatible sample rates 
c                    (isr1 != isr2) && (isr1 != 2*isr2) && (isr2 != 2*isr1)
c                  3 -- number of requested lags is larger than dimensioned,
c                       so need to recompile with larger maxlag -- be sure 
c                       to change in all parameter statements
c
      parameter   (maxlag=1001)
      real        y1(*),y2(*)
      real        z(-maxlag:maxlag)
      real*8      sr,laglen,tau

c     tol is the tolerance to allow the real sampling rate to deviate from an
c     integer value; for instance for tol = 0.01, 99.99 and 100.01 are
c     acceptable and are taken to be nominally 100.  This value is set here
c     to 0.01, and will produce only up to 1/10 of a millisecond error in 1 sec.
      data        tol/0.01/

      ireturn = 0
c     write(7,'(a,2i4)') "i,j= ",is,js
c     write(7,*) y1(1),y2(1),y1(npts),y2(npts),npts

      lags = nint(laglen*sr)
c     write(7,*) ' sr,npts,lags = ',sr,npts,lags
      if (lags.gt.maxlag) then
        ireturn = 3
        return
      endif
      del = 1.0/sr

c     Get cross-correlation in time domain and normalize by standard deviations.
c     Compute the delay.  A positive tmax means the 2nd signal is delayed
c     wrt 1st by that amount.
c     The positive maximum (rmax) is returned, assuming the signals all have
c     the same polarity.
      call stats(y1,npts,2,mean1,stdev1,dummy,dummy)
      call stats(y2,npts,2,mean2,stdev2,dummy,dummy)
      call correl(y1,y2,z,npts,lags,rmax,kmax)
      ccc=rmax/(stdev1*stdev2)
      cccp=rmax/(stdev2*stdev2)
c     Occasional cases may arise where this leads to CCF > 1, so reset it 
c     to +1 if greater.
      if (ccc.gt. 1.0) ccc =  1.0
c     write(6,*) kmax,del
      tmax=kmax*del
      do k=-lags,lags
        z(k)=z(k)/(stdev1*stdev2)
      enddo
c     write(7,*) 'ccc,kmax = ',ccc,kmax
c     write(7,'(5e12.3)') (z(k),k=-lags,lags)
c     Pick out 3 points around maximum and compute best-fit parabola.
      ym=z(kmax-1)
      y0=z(kmax)
      yp=z(kmax+1)
      a = ( ym/2 + yp/2 - y0)/del**2
      b = (-ym/2 + yp/2)/del
      c = y0
c     Peak is at relative time where derivative = 0.
      tpeak = -b/(2*a)
c     Get total time shift.
      tau=tmax+tpeak
c     write(7,*) "tmax,tpeak,tau = ",tmax,tpeak,tau
c     If the 2nd is later than the 1st, then tau is positive.
      return
      end


      subroutine interp(x,n)
   
      real x(*)

      x(n*2-1) = x(n)
      do i = n-1,1,-1
        x(i*2-1) = x(i)
        x(i*2) = (x(i) + x(i+1))/2 
      enddo
      n = n*2 - 1

      return
      end

 
      subroutine correl(x,y,z,n,lags,rmax,kmax)
c
c     This returns the crosscorrelation function, normalized by the number
c     of points used but not by the stdev's of the two time series.
c
c     rmax is the maximum of the correlation function (positive). 
c     kmax is the point of this maximum
c     Note: The zeroth lag will be the midpoint of the returned series z.
c
      parameter   (maxlag=1001)
      real x(*),y(*),z(-maxlag:maxlag)
      rmax=0.0
      do j=-lags,lags
        z(j)=0.0
        do i=1,n
          if(i+j.ge.1 .and. i+j.le.n) z(j)=z(j)+x(i)*y(i+j)
        enddo
c       Normalize the CCF by the actual number of points used.  This accounts
c       for using less of the time series as the shift increases.  
        z(j)=z(j)/(n - abs(j))
        if (z(j).gt.rmax) then
          rmax=z(j)
          kmax=j
        endif
      enddo
      return
      end

      subroutine stats(x,n,k,amean,stdev,skew,kurtosis)
c
c     Computes statistics of a data vector.  How many stats is determined by
c     the value of k.
c
c     k = 1     mean
c     k = 2     also standard deviation
c     k = 3     also skew
c     k = 4     also kurtosis
c
      real*4 x(n)

      amean=0.0
      stdev=0.0
      skew=0.0
      kurtosis=0.0

      if (k.eq.0) return

      sum=0.0

      do i=1,n
        sum=sum+x(i)
      enddo

      amean=sum/n

      if (k.eq.1) return

      sumsq=0.0

      do i=1,n
        sumsq=sumsq+(x(i)-amean)**2
      enddo

      stdev=sqrt(sumsq/n)

      if (k.eq.2) return

      sumcu=0.0

      do i=1,n
        sumcu=sumcu+(x(i)-amean)**3
      enddo

      skew=(sumcu/n)/stdev**3

      if (k.eq.3) return

      sumqu=0.0

      do i=1,n
        sumqu=sumqu+(x(i)-amean)**4
      enddo

      kurtosis=(sumqu/n)/stdev**4

      return
      end
