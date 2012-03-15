c
c
c ------------------------ moment --------------------------------
c
c
      subroutine gmoment(rad,m0,ounit)
c
c     The following coordinate system is assumed:
c     1 -> east
c     2 -> north
c     3 -> up
c
c     gmoment - routine to rotate the moment tensor into its
c              principle axes, find the greatest double couple
c              component, and resulting orientation parameters
c              for the two nodal planes.
c
c     rad(i) =>  i=1(m11);=2(m22);=3(m12);=4(m13);=5(m23).
c                =6(m33)
c
      dimension ev(3,5),v(3,3),p1(3),p2(3),rad(6)
      real*4 m(3,3),md1,md3,merr,m0
      real tv(3)
      real moment_mag, scalar_moment,isotropic
      integer ounit
      real rad2deg

c
c     initialize some variables
c     
      t1=1.0e-05
      t2=1.0e-04
      nsiz=3
      s2=1.414213562
      pi=3.141593
      rad2deg = 57.29578

      write(ounit,200)

c     Store the moment tensor in 2d array

      m(1,1)=rad(1)
      m(2,2)=rad(2)
      m(3,3)=rad(6)

      m(1,2)=rad(3)
      m(2,1)=m(1,2)

      m(1,3)=rad(4)
      m(3,1)=m(1,3)

      m(2,3)=rad(5)
      m(3,2)=m(2,3)
      
c
c     output the Input moment tensor
      write(ounit,100)
      do 05 i=1,3
         write(ounit,101) (m(i,j),j=1,3)
05    continue
      write(ounit,*)'m0 = ',m0
c
c     remove the isotropic component
c
      isotropic = 0.333333*(rad(1)+rad(2)+rad(6))
      write(ounit,*)' '
      write(ounit,*)'Isotropic Component: ',isotropic
      m(1,1)=rad(1)-isotropic
      m(2,2)=rad(2)-isotropic
      m(3,3)=rad(6)-isotropic
      write(ounit,*) 'Subtracted out the isotropic component.'
c
c     output the moment tensor
      write(ounit,*)' '
      write(ounit,*)'The Deviatoric Moment Tensor:'
      do 10 i=1,3
         write(ounit,101) (m(i,j),j=1,3)
10    continue
c
c     compute the eigenvalues and eigenvectors of m
      call hsehld(nsiz,nsiz,m,v,ev)
c     
c       make sure that the eigenvectors form a right handed
c          coordinate system v3 = v1 x v2.
c
      v(1,3)=v(2,1)*v(3,2) - v(3,1)*v(2,2)
      v(2,3)=v(3,1)*v(1,2) - v(1,1)*v(3,2)
      v(3,3)=v(1,1)*v(2,2) - v(2,1)*v(1,2)
c
c     Output the eigenvalues and eigenvectors of m
c
      write(ounit,102)
      write(ounit,101) (ev(i,1),i=1,3)
      write(ounit,103)
      do 11 i=1,3
         write(ounit,101) (v(i,j),j=1,3)
11    continue
c
c     Compute the azimuth and plunge
c
      write(ounit,*) ' ' 
      write(ounit,*) 'Eigenvector 1:'
      tv(1) = v(1,1)
      tv(2) = v(2,1)
      tv(3) = v(3,1)
      call getaz_plunge(tv,az,plunge)
      write(ounit,*)'Az = ',az,' Plunge = ',plunge

      write(ounit,*) 'Eigenvector 2:'
      tv(1) = v(1,2)
      tv(2) = v(2,2)
      tv(3) = v(3,2)
      call getaz_plunge(tv,az,plunge)
      write(ounit,*)'Az = ',az,' Plunge = ',plunge

      write(ounit,*) 'Eigenvector 3:'
      tv(1) = v(1,3)
      tv(2) = v(2,3)
      tv(3) = v(3,3)
      call getaz_plunge(tv,az,plunge)
      write(ounit,*)'Az = ',az,' Plunge = ',plunge
      write(ounit,*) ' ' 
c
c     Output the Scalar moment estimate of silver and jordan
c
      temp = (isotropic+ev(1,1))*(isotropic+ev(1,1))
      temp = temp+(isotropic+ev(2,1))*(isotropic+ev(2,1))
      temp = temp+(isotropic+ev(3,1))*(isotropic+ev(3,1))
      scalar_moment = sqrt(temp / 2.0)*m0
      moment_mag = 0.6667*log10(scalar_moment) - 10.7
      write(ounit,*) ' '
      write(ounit,*)
     &'The Total Scalar Moment (Silver&Jordan,1982) is '
      write(ounit,'(e10.3)')scalar_moment
      write(ounit,*) 
     &'The Total Moment Magnitude (Kananmori(1979)) is '
      write(ounit,'(f5.1)') moment_mag

      temp = ev(1,1)*ev(1,1)
      temp = temp+ev(2,1)*ev(2,1)
      temp = temp+ev(3,1)*ev(3,1)
      scalar_moment = sqrt(temp / 2.0)*m0
      moment_mag = 0.6667*log10(scalar_moment) - 10.7
     
      write(ounit,*) ' '
      write(ounit,*) 
     &'The Deviatoric Scalar Moment (Silver&Jordan,1982) is '
      write(ounit,'(e10.3)')scalar_moment
      write(ounit,*) 
     &'The Deviatoric Moment Magnitude (Kananmori(1979)) is '
      write(ounit,'(f5.1)') moment_mag
c
c
c     Compute the percent clvd (relative to double couple)
c
      md1=(ev(1,1)/abs(ev(1,1)))*(abs(ev(1,1))+abs(ev(3,1)))/2.
      md3=(ev(3,1)/abs(ev(3,1)))*abs(md1)
      merr=abs(ev(2,1)/md1)*100.0
      
      tmin = abs(ev(1,1))
      if(abs(ev(2,1)) .lt. tmin) tmin = abs(ev(2,1))
      if(abs(ev(3,1)) .lt. tmin) tmin = abs(ev(3,1))
      
      tmax = abs(ev(1,1))
      if(abs(ev(2,1)) .gt. tmax) tmax = abs(ev(2,1))
      if(abs(ev(3,1)) .gt. tmax) tmax = abs(ev(3,1))      
      epsilon = tmin / tmax
      
      merr = 200 * epsilon
      
      
      write(ounit,*) ' '
      write(ounit,104) md1,md3
      write(ounit,105) merr
      write(ounit,*) ' '
c
c     Compute the major double couple associated with m
c
c     calculate moment tensor elements for dislocation assuming
c       eigenvalues are +1, -1.
c
c     DM = V E V^T
c
      dm11=v(1,1)*v(1,1) - v(1,3)*v(1,3)
      dm22=v(2,1)*v(2,1) - v(2,3)*v(2,3)
      dm33=v(3,1)*v(3,1) - v(3,3)*v(3,3)
      dm12=v(1,1)*v(2,1) - v(2,3)*v(1,3)
      dm13=v(3,1)*v(1,1) - v(3,3)*v(1,3)
      dm23=v(3,1)*v(2,1) - v(3,3)*v(2,3)
c
      write(ounit,111) dm11,dm22,dm33,dm12,dm13,dm23
c
c     rotate the pressure and tension axes to find poles of
c          p nodal planes
c
      p2(1)=(v(1,1) + v(1,3))/s2
      p2(2)=(v(2,1) + v(2,3))/s2
      p2(3)=(v(3,1) + v(3,3))/s2
      
      p1(1)=(-v(1,1) + v(1,3))/s2
      p1(2)=(-v(2,1) + v(2,3))/s2
      p1(3)=(-v(3,1) + v(3,3))/s2
      
      c1=1.0
      c2=1.0
      if(p1(3).lt.0.) c1=-1.0
      if(p2(3).lt.0.) c2=-1.0
      
      do 12 i=1,3
         p1(i)=p1(i)*c1
         p2(i)=p2(i)*c2
12    continue

      write(ounit,106)
      write(ounit,101) (p1(i),i=1,3)
      write(ounit,101) (p2(i),i=1,3)
      
c     azimuth and plunge of pole 1
c
      if(abs(p1(1)).le.t1) go to 13
      az1=atan2(p1(2),p1(1))
      go to 14
      
13    if(p1(2).ge.0.) az1=1.570796
      if(p1(2).lt.0.) az1=4.712389
14    if(az1.lt.0.) az1=az1 + 6.283185
      plng=sqrt(p1(1)*p1(1) + p1(2)*p1(2))
      if(plng.le.t1) go to 15
      pl1=atan2(p1(3),plng)
      go to 16
15    pl1=1.570796
16    continue

      write(ounit,107) az1*rad2deg,pl1*rad2deg
c      
c     azimuth and plunge of pole #2
c
      if(abs(p2(1)).le.t1) then      
         if(p2(2).ge.0.) az2=1.570796
         if(p2(2).lt.0.) az2=4.712389      
      else
         az2=atan2(p2(2),p2(1))
      end if
      
      if(az2.lt.0.) az2=az2 + 6.283185
      plng=sqrt(p2(1)*p2(1) + p2(2)*p2(2))
   
      if(plng.le.t1) then
         pl2=1.570796
      else
         pl2=atan2(p2(3),plng)
      end if

      write(ounit,108) az2*rad2deg,pl2*rad2deg
c      
c     Find the nodal plane angle parameters
c
      strk1=az1 - 4.712389
      if(strk1.lt.0.) strk1=strk1 + 6.283185
      strk2=az2 - 4.712389
      if(strk2.lt.0.) strk2=strk2 + 6.283185
      dip1=1.570796 - pl1
      dip2=1.570796 - pl2
c
c     Find the rake of plane #1
c
      if(dip1 .lt. t1) go to 21
        cd2sd1=cos(dip2)/sin(dip1)
        if(abs(cd2sd1).gt. 1.0) cd2sd1=1.0
        rake=asin(cd2sd1)
        merr=1000.
        do 22 j=1,4
          if(j.eq.1) rakt=rake
          if(j.eq.2) rakt= -rake + pi
          if(j.eq.3) rakt= +rake + pi
          if(j.eq.4) rakt= -rake
          call mtest(strk1,dip1,rakt,
     +               dm11,dm22,dm12,dm13,dm23,err)
          if(err.gt.merr) go to 22
          rake1=rakt
          merr=err
22     continue
c
c     Find the rake of plane #2
c
      if(dip2.lt.t1) go to 30
      cd1sd2=cos(dip1)/sin(dip2)
      if(abs(cd1sd2).gt. 1.0) cd1sd2=1.0
      rake=asin(cd1sd2)
      merr=1000.
      do 31 j=1,4
        if(j.eq.1) rakt= rake
        if(j.eq.2) rakt= -rake + pi
        if(j.eq.3) rakt= +rake + pi
        if(j.eq.4) rakt= -rake
        call mtest(strk2,dip2,rakt,dm11,dm22,dm12,dm13,dm23,err)
        if(err.gt.merr) go to 31
        rake2=rakt
        merr=err
31    continue
      go to 24
21    continue
c
c     degenerate case when dip1=0
c
      rake1=0.
      rake=v(3,1) + v(3,3)
      if(rake.gt.0.) rake2=-1.570796
      if(rake.lt.0.) rake2=+1.570796
c
      go to 24
c
30    continue
c
c     Handle the degenerate case when dip2=0
c
      rake2=0.
24    continue
      write(ounit,109)
      t3=360.0/6.283185
      dip1=dip1*t3
      rake1=rake1*t3
      strk1=strk1*t3
      dip2=dip2*t3
      rake2=rake2*t3
      strk2=strk2*t3
      if (dip1.gt.89) then
        dip1=89.00
      end if
      if (dip2.gt.89) then
        dip2=89.00
      end if
      if (rake1.gt.180) then
	rake1=rake1-360
      end if
      if (rake2.gt.180) then
	rake2=rake2-360
      end if
      write(ounit,110) strk1,dip1,rake1
      write(ounit,110) strk2,dip2,rake2
      write(ounit,201)
c
c
100   format(1x,'Input moment tensor:')
101   format(1x,3(e10.3,5x))
102   format(/,1x,'eigenvalues of the moment tensor')
103   format(/,1x,'and associated eigenvectors (in the columns)')
104   format(1x,'double couple eigenvalues=',e10.3,5x,e10.3)
105   format(1x,'error (clvd/dc)*100=',f10.3)
106   format(1x,'pole vectors p1 and p2')
107   format(/,1x,'azimuth and plunge of pole #1;',5x,'az1=',f07.2,5x,'p
     +l1=',f07.2)
108   format(1x,'azimuth and plunge of pole #2;',5x,'az2=',f07.2,5x,'pl2
     +=',f07.2)
109   format(/,1x,'fault angle parameters',/,2x,
     +       'strike',6x,'dip',5x,'rake')
110   format(1x,3(f7.2,3x))
111   format(/,1x,'moment tensor for the major dislocation'/,
     +1x,5(f10.4,3x),/)
200   format
     +(/,1x,'--------- From Subroutine Moment ---------------',/)
201   format
     +(/,1x,'--------------------------------------------------',/)
c
c
      return
      end
c
c
c ------------------------ mtest ---------------------------------
c
      subroutine mtest(s,d,r,dm11,dm22,dm12,dm13,dm23,err)
c
c       this routine calculates the theoretical moment tensor expected
c       from values of strike,dip,rake, and compares it with the
c       observed.  rake is changed to rake + pi if the comparison
c       fails.
c
      tm11=sin(s)*sin(s)*sin(r)*sin(2.*d)+sin(2.*s)*cos(r)*sin(d)
      tm22=cos(s)*cos(s)*sin(r)*sin(2.*d)-sin(2.*s)*cos(r)*sin(d)
      tm12=-0.5*sin(2.*s)*sin(r)*sin(2.*d)-cos(2.*s)*cos(r)*sin(d)
      tm13=cos(s)*cos(r)*cos(d)+sin(s)*sin(r)*cos(2.*d)
      tm23=-cos(s)*sin(r)*cos(2.*d)+sin(s)*cos(r)*cos(d)
      err=(tm11-dm11)**2 + (tm22-dm22)**2 + (tm12-dm12)**2 +
     *  (tm13-dm13)**2 + (tm23-dm23)**2
      err=sqrt(err)
      return
      end

c
c ------------------------ getaz_plunge  ------------------------------
c
c     v(1) = east    v(2) = north   v(3) = down
c
      subroutine getaz_plunge(v,az,plunge)
      real v(3), az, plunge

      real north,south,east,west
      real pi, pi_over_2, two_pi
      integer stdout

      stdout = 6


      pi_over_2 = 1.5707963
      pi = 3.1415927
      two_pi = 6.2831853

      north = 0
      south = pi
      east = pi_over_2
      west = 3*pi_over_2

      vsmall=1.0e-06
      rad2deg = 57.29578

      if(abs(v(2)) .le. vsmall)then
        if(abs(v(1)) .gt. vsmall)then
          if(v(1) .ge. 0.0) az1=east
          if(v(1) .lt. 0.0) az1=west
        else
          az1 = 0.0
          write(stdout,*) 'Vertical vector.'
        end if
      else 
        if(abs(v(1)) .le. vsmall)then
	       if(v(2) .ge. 0.0) az1=north
	       if(v(2) .lt. 0.0) az1=south
        else
            az1=pi_over_2 - atan2(v(1), v(2))
        end if
      end if

      if(az1 .lt. 0.) az1=az1 + two_pi
c
c     compute the plunge
c
      r=sqrt(v(1)*v(1) + v(2)*v(2))
c
      if(abs(v(3)) .le. vsmall)then
        pl1 = 0.0
      else
        if(r .lt. vsmall)then
          if(v(3) .gt. 0) pl1 = -pi_over2
          if(v(3) .lt. 0) pl1 = pi_over2
        else
          temp = v(3)
          pl1 = atan2(temp,r)
        end if
      end if
c
c     convert to degrees
c
      az = az1*rad2deg
      if(az .gt. 360.0) az = az - 360.0
c
      plunge = pl1*rad2deg

      if(plunge .lt. 0.0)then
        plunge = -plunge
        az = az + 180
        if(az .gt. 360.0) az = az - 360.0
      end if
c
      if(abs(plunge) .gt. 90.0)then

       if(plunge .gt. 90.0)then
        plunge = plunge - 90.0
        az = az + 180.0
        if(az .gt. 360.0) az = az - 360.0
       else if (plunge .lt. -90.0)then
        plunge = plunge + 90.0
        az = az + 180.0
         if(az .gt. 360.0) az = az - 360.0
      end if

      end if

      return
      end
