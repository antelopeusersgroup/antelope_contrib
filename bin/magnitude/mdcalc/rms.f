c
c input:           * ** ****
      subroutine rms(y,ny,nrms,xnoise)
c
c  For the array Y, evaluate the RMS average of
c  the noise sample in "nrms" points about the
c  location "ny".
c
      real*4 y(*)

      jstart = ny - nrms/2
      jstart = max0(1,jstart)
      jend = jstart+nrms-1
      if(jend.le.jstart)return
      xmean = 0.
      do j=jstart,jend
        xmean = xmean+y(j)
      enddo
      xmean=xmean/float(jend-jstart+1)
      sum = 0.
      do j=jstart,jend
        sum = sum + (y(j)-xmean)**2
      enddo
      xnoise = sqrt( sum/float(jend-jstart+1) )
      return
      end
