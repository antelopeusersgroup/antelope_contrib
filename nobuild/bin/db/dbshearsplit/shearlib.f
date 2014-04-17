      subroutine addtim(dyr,ddy,dhh,dmm,dsc,syr,sdy,shh,smm,ssc,secs)
C  ADDTIM  --  Add a time, in seconds, to a time expressed as year, jday,
C              hour, minute, seconds and make it work.
C
C  Assumes:
C     syr, sdy - Beginning year, julian day
C     shh, smm, ssc - Beginning hour, minute, second
C     secs - seconds to add
C  Returns:
C     dyr, ddy - Resulting year, julian day
C     dhh, dmm, dsc - Resulting hour, minute, second
C
C  By G. Helffrich/DTM based on code written by P. Shearer.

      integer  dyr,ddy,dhh,dmm, syr,sdy,shh,smm
      real dsc, ssc, secs

      dyr = syr
      ddy = sdy
      dhh = shh
      dmm = smm
      dsc = ssc+secs
      call CLEANTIME(dyr,ddy,dhh,dmm,dsc)
      end


c---------------------------------------------

      subroutine CLEANTIME(yr,jdy,hr,mn,sc)
C   CLEANTIME  --  cleans up oddball yr,jdy,hr,mn,sc times

      integer yr,jdy,hr,mn,dmn,dhr,ddy,dyr

      dmn=int(sc/60.)
      sc=sc-60.*float(dmn)
      if (sc.lt.0.) then
         sc=sc+60.
         dmn=dmn-1
      end if
      mn=mn+dmn
c
      dhr=mn/60
      mn=mn-60*dhr
      if (mn.lt.0) then
         mn=mn+60
         dhr=dhr-1
      end if
      hr=hr+dhr
c
      ddy=hr/24
      hr=hr-24*ddy
      if (hr.lt.0) then
         hr=hr+24
         ddy=ddy-1
      end if
      jdy = jdy+ddy
c
      if (mod(yr,4) .eq. 0 .and. mod(yr,400) .ne. 0) then
         idyr = 366
      else
         idyr = 365
      endif
      dyr=jdy/idyr
      jdy = jdy - idyr*dyr
      if (jdy .lt. 0) then
         jdy = jdy + idyr
         dyr = dyr-1
      endif
      yr = yr+dyr
      end


c---------------------------------------------

      subroutine aniput(r,t,rshift,tshift,npts,dt,tlag,azimuth,baz)
c       routine to put in anisotropy with fast azimuth angle (in degrees
c       clockwise from north) and timeshift deltat. 
c       and reconstruct r and t seismograms.    
c       baz is back azimuth in degrees.
c       the rotation is done so that the same arrays can be used in
c       r,rshift and t, tshift  
      parameter (nmax=250000)
      dimension r(1),t(1),rshift(1),tshift(1),ue(nmax),un(nmax),
     &            f(nmax),s(nmax)
      data pi/3.141592654/,dt_int/.05/
      if(npts.gt.nmax)then
          write(*,*)'npts gt nmax',npts,nmax,' pgm aborted'
          call exit(-1)
      endif
      rad=180./pi
      bangle=baz/rad
c       note: azimuth is clockwise from north. angle is counterclockwise
c       angle from east, taken to be x coord (n is y)
        angle=(90.-azimuth)/rad 
c       derotate to n,e
        call rotsub(r,t,un,ue,npts,bangle)
c       rotate into fast and slow direction. Remember, azimuth is clockwise
c       from north, baz is clockwise from north.        
        call rotcomp(ue,un,f,s,npts,angle)
c       shift f with respect to s
c       shift seismograms by tlag to add anisotropy and rotate into
c       e-w n-s coord   
        jshift0=tlag/dt          
        call rotcomp(f(1+jshift0),s,
     +    ue,un,npts,-angle)
c       then to radial,transverse
        call rotsub(un,ue,rshift,tshift,npts,bangle)
        return
        end

c---------------------------------------------

        subroutine crosscorr2(s1,s2,n,ss)
c       routine to compute the cross correlation function
c       between two time series of length n.  It is normalized
c       by sqrt(acf1(0)*acf2(0)) if norm =1, or not if norm=0.
c       It is normalized by default.
c       acf1(0) and acf2(0) are stored in common
c       The result ss is of length 2n+1.  The zero-lag point
c       is in position n+1. ss(1) and ss(2n+1) are set to zero.
c       This is a test of shearsp by doing the cc in time domain
c       Assumes the signal is zero outside the boundaries of the array
c       
        dimension s1(n),s2(n),ss(2*n+1)
        common/acfcom/acfs1,acfs2,norm
        data norm/1/
        n2=2*n
        indx=1
        ss(1)=0.
        ss(n2)=0.
        do lag=-(n-1),n-1,1
          indx=indx+1
          ss(indx)=0.
c         find overlap
          iover=n-iabs(lag)
          if(lag.le.0)then
            do i=1,iover
              ss(indx)=ss(indx)+s1(i)*s2(i-lag)
            enddo
          else
            do i=1,iover
              ss(indx)=ss(indx)+s1(i+lag)*s2(i)
            enddo
          endif
        enddo
        acfs1=0.
        acfs2=0.
        do i=1,n        
          acfs1=acfs1+s1(i)**2
          acfs2=acfs2+s2(i)**2
        enddo
        fac=sqrt(acfs1*acfs2)
c       normalize cc
        if(norm.eq.1)then
          call scalar(ss,n2,1./fac)     
        endif
        return
        end
                               
c---------------------------------------------------------

      function acosf(arg)
      real*8 arg
      real*8 acosf

      if (Dabs(arg) .gt. 1.d-8) goto 1
          acosf=1.570796
          goto 3

1     if (arg .gt. 0.) goto 2
          if (arg .lt. -1.) arg=-.9999999
          acosf=3.141593-Datan((Dsqrt(1.-arg**2))/Dabs(arg))
          goto 3

2     if (arg .gt. 1.) arg=.9999999
      acosf=Datan((Dsqrt(1.-arg**2))/arg)

3     return
      end

cc---------------------------------------------
      subroutine disaz(elat,elon,slat,slon,azm,bzm,dkm,ddg)
      real*8 elat,elon,slat,slon,azm,bzm,ddg,dkm
      real*8 c1,c2,c3,c4,a,b,c,d,e,s,delo,del,acosf


c     ----- end variable declarations -----

      c1=57.29578
      c2=1.570796
      c3=.9932313
      c4=.0174533
      a=c2-Datan(c3*Dtan(c4*elat))
      b=-elon
      c=c2-Datan(c3*Dtan(c4*slat))
      d=-slon
      delo=b-d
      if (delo .lt. -180.) delo=360.+delo
      if (delo .gt.  180.) delo=delo-360.

      delo=delo*c4

c     Special case: two events are right on top of each other.
c     Set values explicitly to avoid a divide by zero error.
      if (delo .eq. 0.0) then
          dkm = 0.0
          ddg = 0.0
          azm = 0.0
          bzm = 180.0
          return
      endif
          
      del = acosf(Dcos(a)*Dcos(c)+Dsin(a)*Dsin(c)*Dcos(delo))
      ddg=c1*del
      dkm=(6371.227*(1.+.0033785*(1./3.-Dcos((a+c)/2.)**2)))*del
      e=acosf((Dcos(c)-Dcos(a)*Dcos(del))/(Dsin(a)*Dsin(del)))
      s=acosf((Dcos(a)-Dcos(c)*Dcos(del))/(Dsin(c)*Dsin(del)))

      if (delo .lt. 0.) then
          azm=360.-c1*e
          bzm=c1*s
      else
          azm=c1*e
          bzm=360.-c1*s
      endif

      return
      end

c---------------------------------------------------------


        subroutine e_ev_mult(ss_ee,ss_nn,ss_en,n0,
     +    e_ev_array,pol_array,na,nlag,na1,nlag1,ratio)
c       subroutine to compute energy on minimum-eigenvalue component for angles
c       0 through 180 and for lags of +-jshift (defined below)
c       ss... are the cross and auto correlations of the original
c       e-w, n-s seismograms (rotated form the radial and transverse)and n0 is
c       the index of the zero-lag point.
        dimension e_ev_array(nlag1,na1),pol_array(nlag1,na1),
     +  ss_ee(n0+nlag),ss_nn(n0+nlag),ss_en(n0+nlag),c(2,2)
        data pi/3.141592654/
        rad=180./pi
c       check if nlag is odd
        if(mod(nlag,2).eq.0)then
          write(*,*)'nlag must be odd, pgm aborted: nlag',nlag
          stop
        endif
        jshift=(nlag-1)/2
        alammin=1.e+22
        do iangle=0,180,1
          theta=float(iangle)/rad
          c2 =cos(theta)**2
          s2 =sin(theta)**2
          sc =sin(theta)*cos(theta) 
c         calculate lag-independent quantities
          ss_epep = ss_ee(n0)*c2 + 2.*ss_en(n0)*sc + ss_nn(n0)*s2
          ss_npnp = ss_ee(n0)*s2 - 2.*ss_en(n0)*sc + ss_nn(n0)*c2
          ilag=0
          do lag=n0-jshift,n0+jshift,1    
            ilag=ilag+1 
c           calculate lag dependent term
            ss_epnp = -ss_ee(lag)*sc - ss_en(2*n0-lag)*s2 +
     +                 ss_en(lag)*c2+ ss_nn(lag)*sc
c           matrix elements
            c(1,1) = ss_epep 
            c(2,2) = ss_npnp 
            c(1,2) = ss_epnp 
            c(2,1)=c(1,2)
            trace= c(1,1) + c(2,2)
            euc  = c(1,2)**2 - c(1,1)*c(2,2)
            alam1 = .5*(trace+sqrt(trace**2+4.*euc))                
            alam2 = .5*(trace-sqrt(trace**2+4.*euc))                
c           write(*,*)'alam1, alam2 = ',alam1,alam2
            trace_check=ss_ee(n0)+ss_nn(n0)
            e_ev_array(ilag,iangle+1) = alam2
c           polarization in azimuth degrees(clockwise from north)
c           note, vectors are in 'primed' coord frame and must add
c           theta to the angle.
            pol_array(ilag,iangle+1)=
     +        90.-rad*(atan2((alam1-c(1,1)),c(1,2))+theta)
            if(alam2.le.alammin)then
              alammin=alam2
              alammax=alam1
              rmin=alam2/alam1
            endif
            if(lag.eq.n0.and.iangle.eq.0)rorig=alam2/alam1
            if(e_ev_array(ilag,iangle+1).lt.0.)then
              write(*,*)'e_ev_array lt 0:',e_ev_array(ilag,iangle+1)
              write(*,*)'lag,iangle,e_ev_0',lag,iangle,e_ev_0
              write(*,*)'alam1,alam2,trace,euc',
     +          alam1,alam2,trace,euc
            endif
          enddo
        enddo
        ratio=rmin/rorig
c       cjw normalize by noise level
        do iangle=0,180,1
          ilag=0
          do lag=n0-jshift,n0+jshift,1  
            ilag=ilag+1   
            e_ev_array(ilag,iangle+1)=e_ev_array(ilag,iangle+1)/alammin
          enddo
        enddo 
        return
        end

c---------------------------------------------

        subroutine e_t_mult(ss_ee,ss_nn,ss_en,n0,
     +    e_t_array,na,nlag,na1,nlag1,ebaz)
c       subroutine to compute energy on shear component for angles
c       0 through 180 and for lags of +-jshift (defined below)
c       ss... are the cross and auto correlations and n0 is
c       the index of the zero-lag point.
        dimension e_t_array(nlag1,na1),ss_ee(n0+nlag),
     +  ss_nn(n0+nlag),ss_en(n0+nlag)
        data pi/3.141592654/
        rad=180./pi
c       check if nlag is odd
        if(mod(nlag,2).eq.0)then
            write(*,*)'nlag must be odd, pgm aborted: nlag',nlag
            stop
        endif

        anoisemin=1.e+22
        jshift=(nlag-1)/2

        do iangle=0,180,1
            theta=float(iangle)/rad
            baz=ebaz/rad
            co_t =cos(theta)
            si_t =sin(theta)
            co_tb=cos(theta+baz)
            si_tb=sin(theta+baz)
            co_2tb=cos(2.*theta+baz)  
c           !for a test
            si_2tb=sin(2.*theta+baz)  
c           !for a test
            e_t_0= co_tb**2*(ss_ee(n0)*co_t**2   +
     +             ss_nn(n0)*si_t**2   +                             
     +             2.*ss_en(n0)*si_t*co_t)    +
     +             si_tb**2*(ss_ee(n0)*si_t**2   +
     +             ss_nn(n0)*co_t**2   -                             
     +             2.*ss_en(n0)*si_t*co_t) 
            ilag=0
            do lag=n0-jshift,n0+jshift,1    
                ilag=ilag+1 
                e_t_array(ilag,iangle+1)=e_t_0-
     +             2.*si_tb*co_tb*((-ss_ee(lag)+ss_nn(lag))*si_t*co_t +
     +             ss_en(lag)*co_t**2   -        
     +             ss_en(2*n0-lag)*si_t**2)  

c               cjw find minimum of e_t_array 
                if(e_t_array(ilag,iangle+1) .lt. anoisemin) then
                    anoisemin=e_t_array(ilag,iangle+1)
                endif
                if(e_t_array(ilag,iangle+1).lt.0.)then
                    write(*,*)'e_t_array lt 0:',e_t_array(ilag,iangle+1)
                    write(*,*)'lag,iangle,e_t_0',lag,iangle,e_t_0
                    if(lag.eq.n0)then
                        e_t_test=ss_ee(n0)*co_2tb**2 +
     +                           ss_nn(n0)*si_2tb**2 +
     +                           2.*ss_en(n0)*si_2tb*co_2tb
                        write(*,*)'lag=n0, recalculating using simple',
     +                            'formula'
                        write(*,*)'e_t_test=',e_t_test
                        write(*,*)'ss_ee,ss_nn,ss_en',
     +                            ss_ee(n0),ss_nn(n0),ss_en(n0)
                    endif
                endif
            enddo
        enddo
c       cjw normalize by minimum noise level
        do iangle=0,180,1
          ilag=0
          do lag=n0-jshift,n0+jshift,1    
            ilag=ilag+1 
            e_t_array(ilag,iangle+1)=
     #       e_t_array(ilag,iangle+1)/anoisemin
           enddo
         enddo
        return
        end

c---------------------------------------------
      function f_test2(ndf)
!     this routine calculates the critical value of the
!     variance ratio of two statistical distributions for the
!     95% confidence level by cubic spline interpolation of 
!     table values.  It is specialized to the case of 2 degrees of freedom.
!     returns the critical degrees of freedom up to ndf=999. for
!     ndf gt 999, it prints a caution and then returns the value
!     for 999.
!     It extrapolates using cubic spline interpolation.
!     call spline routines rspln and rsple in utilib

      logical first_time
      parameter (ndim=29,ndfmax=999)
      dimension var_rat(ndim),x(ndim),q(3,ndim),f(3,ndim)
      save first_time
      data first_time/.true./
      data x/1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,22,
     &       24,26,28,30,40,60,120,999/

      ! Fortran77 has no provisions for assigning constant arrays of
      ! real numbers, so we have to assign each value individually

      var_rat(1)=199.50
      var_rat(2)=19.00
      var_rat(3)=9.55
      var_rat(4)=6.94
      var_rat(5)=5.79
      var_rat(6)=5.14
      var_rat(7)=4.74
      var_rat(8)=4.46
      var_rat(9)=4.26
      var_rat(10)=4.10
      var_rat(11)=3.98
      var_rat(12)=3.89
      var_rat(13)=3.81
      var_rat(14)=3.74
      var_rat(15)=3.68
      var_rat(16)=3.63
      var_rat(17)=3.59
      var_rat(18)=3.55
      var_rat(19)=3.52
      var_rat(20)=3.49
      var_rat(21)=3.44
      var_rat(22)=3.40
      var_rat(23)=3.37
      var_rat(24)=3.34
      var_rat(25)=3.32
      var_rat(26)=3.23
      var_rat(27)=3.15
      var_rat(28)=3.07
      var_rat(29)=3.00
     
      if(ndf.gt.ndfmax)then
            write(*,*)'ndf exceeds ndfmax of table',ndf,ndfmax
            write(*,*)'ndf set to ndfmax'
            ndf1=ndfmax
      else
            ndf1=ndf      
      endif

      n = 29
      call rspln(1,n,x,var_rat,q,f)

      f_test2=rsple(1,n,x,var_rat,q,float(ndf1))
      return
      end

c---------------------------------------------
        function ftest_mse_2(n)
!       a version of ftest_mse that is specialized to the case of 2 parameters
!       the 95% confidence level.  It computes the value of the ftest by
!       interpolation of a table.  Not recommended above n=120.
!       this function provides the critical value of the ratio of mean
!       squared misfit function at the minimum to any other value, such
!       the values inside the region defined by ratio<ftest_mse_2 corresponds
!       to a confidence interval at the 95% confidence level.  
!       'n' is the "effective" number
!       of data, ie the number of degrees of freedom, assumed to be an integer.
        ftest_mse_2= 1. + 2.*f_test2(n-2)/float(n-2)
        return
        end

c---------------------------------------------
        FUNCTION GAUSS(IRAN)
        DATA N/25/
        GAUSS=0.0
        DO I=1,N
c          GAUSS=GAUSS+(RAND(IRAN)-0.5)
c cjw this function has changed on the suns.
c need to initiallize RAND(iseed) in main program
c use RAND(0) for values after
           GAUSS=GAUSS+(RAND(0)-0.5)
        ENDDO
        GAUSS=sqrt(12./FLOAT(N))*GAUSS
        RETURN
        END
c

c---------------------------------------------
c   function lenstr returns the actual length (excluding
c   trailing blanks) of a character string
c
      function lenstr(string)
c
      character string *(*)

      lenstr = 0
      do i=len(string),1,-1
          if (string(i:i) .ne. ' ') then 
              if (string(i:i .ne. '')) then
                  lenstr = i
                  return 
              end if
          end if
      enddo
      return 
      end


c---------------------------------------------
!     function leadingblanks returns the number of blanks
!     occuring before the first real character in the string.
!     If no blanks occur, 1 is returned.  This routine is suitable
!     for determining where the start of a substring should occur
!     to remove leading blanks from a string.

      function leadingblanks(string)
      character string*(*)
      leadingblanks = 0
      do i=1,len(string)
          if (string(i:i) .ne. ' ') then 
              leadingblanks = i
              return
          end if
      enddo
      return 
      end

!---------------------------------------------

      function strchr(base,pattern)
      character base*(*)
      character pattern
      integer strchr,i

      strchr = 0
      do i=1,len(base)
          if ( base(i:i) .eq. pattern ) then
              strchr = i
              return
          end if
      enddo

      return 
      end

!---------------------------------------------

      function strrchr(base,pattern)
      character base*(*)
      character pattern
      integer strrchr,i

      strrchr = 0
      do i=len(base),1,-1
          if ( base(i:i) .eq. pattern ) then
              strrchr = i
              return
          end if
      enddo

      return 
      end

!---------------------------------------------

      integer function strstr(base,pattern)
      character base*(*)
      character pattern*(*)
      integer slen,patlen,i,j,lenstr

      slen = lenstr(base)
      patlen = lenstr(pattern)
      strstr = 0
      do i=1,( slen - patlen + 1 )
          if ( base(i:i+patlen-1) .eq. pattern) then
              strstr = i
              return
          end if
      enddo
          
      return 
      end

!---------------------------------------------

      function strrstr(base,pattern)
      character base*(*)
      character pattern*(*)
      integer slen,patlen,i,j

      slen = len(base)
      patlen = lenstr(pattern)
      strrstr = 0

      do i=slen-patlen+1, 1, -1
          if ( base(i:i+patlen-1) .eq. pattern ) then
              strrstr = i
              return
          end if
      enddo
          
      return 
      end


c---------------------------------------------
!
!     subroutine int2str takes an integer and converts it into
!     a string.  str must have allocated enough space to 
!     contain i.  To avoid the danger of overwriting memory,
!     if str is not large enough to hold any integer*4 value,
!     str will be returned as an empty string.

      subroutine int2str( i, str )
      integer i
      character str *(*)

      if ( len(str) .lt. 11 ) then
          write(str,'(1x)')
          return
      endif
          
      write(str,*) i
      ! This second step is necessary to get rid of leading blanks
      write(str,'(a)') str( leadingblanks(str):lenstr(str) )

      end

c---------------------------------------------
        function ndf_fun2(a,n,norig) 
!+      A array is assumed to contain a possibly interpolated time series
!       of length n.  The length of the original uninterpolated time series
!       is norig.  ndf_spect computes the effective
!       number of degrees of freedom in a, which for a gaussian white noise
!       process should be equal to norig.  The true ndf should be less than
!       norig.
        parameter (ndim=250000)
        dimension a(*), b(ndim)
!***    following two lines for test, remove when done
        complex b_comp(ndim/2)
        equivalence (b,b_comp)
!***    end of test
        if(n.gt.ndim-2)then
          write(*,*)'n bigger than nmax, abort', n,ndim-2
          call exit(-1)
        endif
        nadd=n
!       zero out work array
4       call zeroarray(b,1,ndim)
        call tscopy(a,b,n)
        call fftl(b,nadd,-1,ierr)
        if(ierr.eq.1)then
          nadd=nadd+1
          go to 4 
        endif
!       determine number of frequency points for original time series.
!
        nf=(norig+2)/2
!*****************************
!*******test block
!       as a test, apply a filter to the spectrum
!       write(*,*)'in ndf_fun2, testing with a filter. Please remove'
!       do i=1,nf
!         b_comp(i)=b_comp(i)*float(i-1)
!       enddo
!*******end of test block
        ndf_fun2=ndf_spect(b,nf)
        !write(*,*)'orig data pts, ndf:', norig,ndf_fun2
        if(ndf_fun2.gt.norig)then
          write(*,*)'caution:ndf gt norig'
        endif
        return
        end

c---------------------------------------------
        function ndf_spect(a,nf)
c+      This function calculates the number of degrees of freedom
c       appropriate for a summed and squared time series, whose spectrum
c       is array A. A is assumed to be a complex function of frequency with 
c       A(1) corresponding to zero frequency and A(nf) corresponding to
c       the Nyquist frequency.  If A(t) is gaussian distributed
c       then ndf_spect should be the points in the original
c       timer series 2*(nf-1).
c-
        complex a(*)
        f2=0.
        f4=0.
        do i=1,nf
          temp=cabs(a(i))
          f2=f2+temp**2
          f4=f4+temp**4
c         for zero frequency and for Nyquist:
          if(i.eq.1.or.i.eq.nf)then
            f2=f2-.5*temp**2
c
            f4=f4-.5*temp**4
          endif
        enddo
c       based on theory, the following expression should yield
c       the correct number of degrees of freedom.(see appendix of silver
c       and chan, 1990.  In practise, it gives a value that gives
c       a df that is slightly too large.  eg 101 for an original time
c       series of 100.  This is due to the fact that their is a slight
c       positive bias to the estimate using only the linear term.  
        ndf_spect=2.*(2.*f2**2/f4 -1.)  
        return
        end

c---------------------------------------------

        subroutine rotcomp(x,y,xrot,yrot,npts,angle)
!       rotate seismograms into coord system xrot,yrot from x, y. Angle
!       assumed to be in radians.  Angle is counter-clockwise angle from
!       the positive x axis in radians
!       the rotation is done so that the same arrays can be used in
!       x,xrot and y,yrot. ei x,y overwritten with xrot,yrot
        dimension x(npts),y(npts),xrot(npts),yrot(npts)
        si=sin(angle)
        co=cos(angle)
        do i=1,npts
          xrot_temp= x(i)*co+y(i)*si
          yrot_temp=-x(i)*si+y(i)*co 
          xrot(i)=xrot_temp
          yrot(i)=yrot_temp
        enddo
        return
        end

c---------------------------------------------

        SUBROUTINE ROTSUB(AN,AE,AR,AT,NPTS,BAZ)
c+
c       SUBROUTINE ROTSUB(AN,AE,AR,AT,NPTS,BAZ)
c
c       AN and AE are northsouth and eastwest time series respectively;
c       NPTS = number of points; AR and AT are radial and transverse
c       time series returned from this routine; BAZ is backazimuth
c
c       Subroutine to rotate into radial and transverse components.
c       BAZ is the back-azimuth, that is, the clockwise angle 
c       measured from north to the earthquake with the station as the
c       vertex. The positive radial direction points in the direction of
c       the earthquake from the station. The positive transverse direction
c       is to the left of the radial direction.  This assures a right-handed
c       coordinate system with the z component pointing up. 
c       Note that this convention is not the same as AKI and RICHARDS
c       page 114-115.  They use a coordinate system where Z is down.
c       At some point it may be advantageous to switch the signs of
c       both the radial and transverse components so that they will
c       have the same convention as the program WKBJ.  In that program,
c       x points to the station from the earthquake, y is in transverse
c       direction, with positive y to the left of x.
c
c       Has been modified by pgs 6/29/89 so that the same two arrays
c       can be used for input and output
c
c-
        DIMENSION AN(NPTS),AE(NPTS),AR(NPTS),AT(NPTS)
        SI=SIN(BAZ)
        CO=COS(BAZ)
        DO 5 I=1,NPTS
        AR_temp=AE(I)*SI+AN(I)*CO 
c         !pgs
        AT_temp=-(AE(I)*CO-AN(I)*SI)
c         !pgs
        AR(I)=AR_temp 
c         !pgs
5       AT(I)=AT_temp 
c         !pgs
        RETURN
        END

c---------------------------------------------
c
c     function rsple(i1,i2,x,y,q,s)
c $$$$$ calls only library routines $$$$$
c
c   rsple returns the value of the function y(x) evaluated at point s
c   using the cubic spline coefficients computed by rspln and saved i
c   q.  if s is outside the interval (x(i1),x(i2)) rsple extrapolates
c   using the first or last interpolation polynomial.  the arrays mus
c   be dimensioned at least - x(i2), y(i2), and q(3,i2).
c
c                                                     -rpb
      function rsple(i1, i2, x, y, q, s)
      parameter (ndim=250000)
      dimension x(ndim), y(ndim), q(3, ndim)
c   guarantee i within bounds.
      data i / 1 /
!# 15 "rsple.for"
      ii = i2 - 1
!# 17 "rsple.for"
      i = max0(i,i1)
c   see if x is increasing or decreasing.
!# 18 "rsple.for"
      i = min0(i,ii)
c   x is decreasing.  change i as necessary.
!# 20 "rsple.for"
      if (x(i2) - x(i1)) 1, 2, 2
!# 22 "rsple.for"
    1 if (s - x(i)) 3, 3, 4
    4 i = i - 1
      if (i - i1) 11, 6, 1
    3 if (s - x(i + 1)) 5, 6, 6
    5 i = i + 1
c   x is increasing.  change i as necessary.
!# 27 "rsple.for"
      if (i - ii) 3, 6, 7
!# 29 "rsple.for"
    2 if (s - x(i + 1)) 8, 8, 9
    9 i = i + 1
      if (i - ii) 2, 6, 7
    8 if (s - x(i)) 10, 6, 6
   10 i = i - 1
      if (i - i1) 11, 6, 8
    7 i = ii
      goto 6
c   calculate rsple using spline coefficients in y and q.
!# 37 "rsple.for"
   11 i = i1
!# 39 "rsple.for"
    6 h = s - x(i)
      rsple = y(i) + (h * (q(1,i) + (h * (q(2,i) + (h * q(3,i))))))
      return 
      end
c

c---------------------------------------------

c     subroutine rspln(i1,i2,x,y,q,f)
c $$$$$ calls only library routines $$$$$
c
c   subroutine rspln computes cubic spline interpolation coefficients
c   for y(x) between grid points i1 and i2 saving them in q.  the
c   interpolation is continuous with continuous first and second
c   derivitives.  it agrees exactly with y at grid points and with the
c   three point first derivitives at both end points (i1 and i2).
c   x must be monotonic but if two successive values of x are equal
c   a discontinuity is assumed and seperate interpolation is done on
c   each strictly monotonic segment.  the arrays must be dimensioned at
c   least - x(i2), y(i2), q(3,i2), and f(3,i2).  f is working storage
c   for rspln.
c                                                     -rpb
c

      subroutine rspln(i1, i2, x, y, q, f)
      parameter (ndim=250000)
      dimension x(ndim), y(ndim), q(3, ndim), f(3, ndim), yy(3)
      equivalence (y0, yy(1))
      data yy / 3*0. /
      data tol / 1.e-13 /
!# 21 "rspln.for"
      j1 = i1 + 1
c   bail out if there are less than two points total.
!# 22 "rspln.for"
      y0 = 0.
!# 24 "rspln.for"
      if (i2 - i1) 13, 17, 8
c   search for discontinuities.
!# 25 "rspln.for"
    8 a0 = x(j1 - 1)
!# 27 "rspln.for"
      do 3 i = j1, i2
      b0 = a0
      a0 = x(i)
      if (abs(a0 - b0) - tol) 4, 4, 3
    3 continue
   17 j1 = j1 - 1
      j2 = i2 - 2
      goto 5
    4 j1 = j1 - 1
c   see if there are enough points to interpolate (at least three).
!# 36 "rspln.for"
      j2 = i - 3
c   only two points.  use linear interpolation.
!# 38 "rspln.for"
    5 if ((j2 + 1) - j1) 9, 10, 11
!# 40 "rspln.for"
   10 j2 = j2 + 2
      y0 = (y(j2) - y(j1)) / (x(j2) - x(j1))
      do 15 j = 1, 3
      q(j,j1) = yy(j)
   15 q(j,j2) = yy(j)
c   more than two points.  do spline interpolation.
!# 45 "rspln.for"
      goto 12
!# 47 "rspln.for"
   11 a0 = 0.
      h = x(j1 + 1) - x(j1)
      h2 = x(j1 + 2) - x(j1)
      y0 = (h * h2) * (h2 - h)
      h = h * h
c   calculate derivitive at near end.
!# 52 "rspln.for"
      h2 = h2 * h2
!# 54 "rspln.for"
      b0 = (((y(j1) * (h - h2)) + (y(j1 + 1) * h2)) - (y(j1 + 2) * h))
     & / y0
c   explicitly reduce banded matrix to an upper banded matrix.
!# 55 "rspln.for"
      b1 = b0
!# 57 "rspln.for"
      do 1 i = j1, j2
      h = x(i + 1) - x(i)
      y0 = y(i + 1) - y(i)
      h2 = h * h
      ha = h - a0
      h2a = h - (2. * a0)
      h3a = (2. * h) - (3. * a0)
      h2b = h2 * b0
      q(1,i) = h2 / ha
      q(2,i) = - (ha / (h2a * h2))
      q(3,i) = - ((h * h2a) / h3a)
      f(1,i) = (y0 - (h * b0)) / (h * ha)
      f(2,i) = (h2b - (y0 * ((2. * h) - a0))) / ((h * h2) * h2a)
      f(3,i) = - ((h2b - ((3. * y0) * ha)) / (h * h3a))
      a0 = q(3,i)
c   take care of last two rows.
!# 72 "rspln.for"
    1 b0 = f(3,i)
!# 74 "rspln.for"
      i = j2 + 1
      h = x(i + 1) - x(i)
      y0 = y(i + 1) - y(i)
      h2 = h * h
      ha = h - a0
      h2a = h * ha
      h2b = (h2 * b0) - (y0 * ((2. * h) - a0))
      q(1,i) = h2 / ha
      f(1,i) = (y0 - (h * b0)) / h2a
      ha = x(j2) - x(i + 1)
      y0 = - ((h * ha) * (ha + h))
c   calculate derivitive at far end.
!# 85 "rspln.for"
      ha = ha * ha
!# 87 "rspln.for"
      y0 = (((y(i + 1) * (h2 - ha)) + (y(i) * ha)) - (y(j2) * h2)) / y0
      q(3,i) = ((y0 * h2a) + h2b) / ((h * h2) * (h - (2. * a0)))
c   solve upper banded matrix by reverse iteration.
!# 89 "rspln.for"
      q(2,i) = f(1,i) - (q(1,i) * q(3,i))
!# 91 "rspln.for"
      do 2 j = j1, j2
      k = i - 1
      q(1,i) = f(3,k) - (q(3,k) * q(2,i))
      q(3,k) = f(2,k) - (q(2,k) * q(1,i))
      q(2,k) = f(1,k) - (q(1,k) * q(3,k))
    2 i = k
c   fill in the last point with a linear extrapolation.
!# 97 "rspln.for"
      q(1,i) = b1
!# 99 "rspln.for"
    9 j2 = j2 + 2
      do 14 j = 1, 3
c   see if this discontinuity is the last.
!# 101 "rspln.for"
   14 q(j,j2) = yy(j)
c   no.  go back for more.
!# 103 "rspln.for"
   12 if (j2 - i2) 6, 13, 13
!# 105 "rspln.for"
    6 j1 = j2 + 2
c   there is only one point left after the latest discontinuity.
!# 106 "rspln.for"
      if (j1 - i2) 8, 8, 7
!# 108 "rspln.for"
    7 do 16 j = 1, 3
c   fini.
!# 109 "rspln.for"
   16 q(j,i2) = yy(j)
!# 111 "rspln.for"
   13 return 
      end

c---------------------------------------------

        subroutine shear_bars_ev(var_array,varmax,na,nlag,na1,nlag1,
     +  dt,anglemin,tlagmin,angle_bars,tlag_bars,varmin)    
!       version of shear_bars to use with shearmain_ev
        dimension var_array(nlag1,na1),tlag_bars(2),angle_bars(2)
!       common/varplt/tlagpl(15000),anglepl(15000),npoints
!       common to return array indices of minimum variance location
        common/indxcm/lagmin,ianglemin
        logical first_time
        data eps/1e-10/
!       assumes that nlag is odd =2*jshift+1, and that the zero-lag point
!       is in the jshift+1 position.  This is not checked so be careful
!       Also assumes that first angle is 0 deg and last is 180 (1 degree
!       increments
!       note, the error bars correspond to 1 standard deviation in that
!       twice the error bars corresponds to the 95% confidence interval.

!       initialize npoints
        npoints=1
        jshift=(nlag-1)/2+1
!       look for minimum point of var_array
        varmin=1e20 
        do i=1,nlag
          do j=1,na
            if(var_array(i,j).lt.varmin)then
              varmin=var_array(i,j)
              lagmin=i
              ianglemin=j               
            endif
          enddo
        enddo
        tlagmin=(lagmin-jshift)*dt
        anglemin=float(ianglemin-1)
!       normalize var_array by varmin for ftest
!       check for varmin=0 (arises in synthetic cases) and set to eps
        if(varmin.eq.0.)varmin=eps
        do i=1,na 
          call scalar(var_array(1,i),nlag,1./varmin)
        enddo
!       tlagpl(npoints)=tlagmin 
!       anglepl(npoints)=anglemin 
        first_time=.true.
        jshift=(nlag-1)/2+1
        iangle1=ianglemin-45
        iangle2=ianglemin+45
        do i=iangle1,iangle2
!         check for end effects
          if(i.lt.1)then
            ii=180+i
          elseif(i.gt.180)then
            ii=i-180
          else
            ii=i
          endif
          angle0=i-1
          do j=1,nlag 
            if(var_array(j,ii).lt.varmax)then
              if(first_time.eq..true.)then
                amin=angle0
                amax=amin
                nlagmin=j 
                nlagmax=nlagmin
                first_time=.false.
              else
                if(angle0.lt.amin)amin=angle0
                if(angle0.gt.amax)amax=angle0
                nlagmin=min0(nlagmin,j)
                nlagmax=max0(nlagmax,j)
              endif
!             npoints=npoints+1
!             if(npoints.gt.15000)then
!               write(*,*)'npoints has gotten too large, abort'
!               call exit
!             endif
!             tlagpl(npoints)=dt*(j-jshift-1) 
!             anglepl(npoints)=angle0
            endif    
          enddo                  
        enddo
        tlag_bars(1)= .5*((nlagmin-jshift-1)*dt-tlagmin)
        tlag_bars(2)= .5*(tlagmin-(nlagmax-jshift-1)*dt)
        angle_bars(1)=.5*( amin-anglemin)
        angle_bars(2)=.5*(anglemin-amax)
        return
        end

c---------------------------------------------

        subroutine shear_min(var_array,na,nlag,na1,nlag1,
     +  dt,anglemin,tlagmin)    
        dimension var_array(nlag1,na1) 
        common/indxcm/lagmin,ianglemin
!       finds anglemin,tlagmin corresponding to minimum value of transverse
!       component.
!       assumes that nlag is odd =2*jshift+1, and that the zero-lag point
!       is in the jshift+1 position.  This is not checked so be careful
!       Also assumes that first angle is 0 deg and last is 180 (1 degree
!       increments

        jshift=(nlag-1)/2+1
!       look for minimum point of var_array
        varmin=1e20 
        do i=1,nlag
          do j=1,na
            if(var_array(i,j).lt.varmin)then
              varmin=var_array(i,j)
              lagmin=i
              ianglemin=j               
            endif
          enddo
        enddo     
        tlagmin=(lagmin-jshift)*dt
        anglemin=float(ianglemin-1)
        return
        end

c---------------------------------------------

        subroutine spline_int(y,y_int,fac,npts,npts_int)
c       subroutine to cubic-spline-interpolate y by a factor of fac.
c       y assumed to be equally spaced in x.
c       calls rspln,rsple
        parameter (ndim=250000)
        dimension y(npts),y_int(*)
        dimension q(3,ndim),f(3,ndim),x(ndim)
        if(npts.gt.ndim)then
          write(*,*)'error,you have reached limit: ndim,npts',ndim,npts
          stop
        endif
c       fill up x array
        do i=1,npts
          x(i)=float(i-1)
        enddo
        call rspln(1,npts,x,y,q,f)
        dx=1./fac       
        npts_int=float(npts-1)*fac+1.
        do i=1,npts_int
          s=float(i-1)*dx
          y_int(i)=rsple(1,npts,x,y,q,s)
        enddo
        return
        end

c---------------------------------------------

        subroutine unspline(y,fac,npts_int)
c       subroutine to decimate data array y by nfac. Put back in y
        dimension y(npts_int)
        indx=0
        do i=1,npts_int,nfac
          indx=indx+1
          y(indx)= y(i)
        enddo
        return
        end

c---------------------------------------------

      subroutine zerochar(string, n)
      character string*(*)
      do i = 1, n
        if(string(i:i).eq.' ')string(i:i)='0'
      end do
      return 
      end
