      program project_ellipse
      integer iaaz(3),idip(3)
      real se(3)
      character temp*20

      if(iargc().ne.7) then
	print*,"Usage: project_ellipse azim1 dip1 se1 azim2 dip2 se2 se3"
	stop
      endif

      call getarg(1,temp)
      read(temp,*) iaaz(1)
      call getarg(2,temp)
      read(temp,*) idip(1)
      call getarg(3,temp)
      read(temp,*) se(1)
      call getarg(4,temp)
      read(temp,*) iaaz(2)
      call getarg(5,temp)
      read(temp,*) idip(2)
      call getarg(6,temp)
      read(temp,*) se(2)
      call getarg(7,temp)
      read(temp,*) se(3)

c      write(*, 123) iaaz(1), iaaz(2), iaaz(3)
c 123  format(3i8)
c      write(*, 123) idip(1), idip(2), idip(3)
c      write(*, 124) se(1), se(2), se(3)
c 124  format(3f7.3)

      call projax (iaaz, idip, se, ax1, ax2, axz, aze1, aze2)

      if(ax1.gt.ax2) then
	smajax = ax1
	sminax = ax2
	strike = aze1
      else
	smajax = ax2
	sminax = ax1
	strike = aze2
      endif

      if(strike.lt.0) then
	strike = strike + 360
      endif

      sdepth = axz

      write(*, 126) smajax, sminax, sdepth, strike
 126  format(f9.4, 1x, f9.4, 1x, f9.4, 1x, f8.3)

      stop
      end

      subroutine projax (iaaz, idip, se, ax1, ax2, axz, aze1, aze2)
c
c  find projection of error ellipse axes (iaaz, idip, se) onto horizontal plane
c  (ax1 with strike aze1, ax2 with strike aze2) and vertical axis (axz)
c
c  calls subroutine eigen
c
      parameter (rpd = 1.74533e-2)

      dimension iaaz(3),idip(3),se(3)
      dimension pauv(3,3),v(3,3),a(3,3),vec(2,2),ael(3)

      ax1 = 0.
      ax2 = 0.
      ax3 = 0.
      axz = 0.

c  early return if error ellipse not specified
      if (se(1)+se(2)+se(3)+iaaz(1)+iaaz(2)+idip(1)+idip(2) .eq. 0.)
     *     return

c------- rotate ss matrix back into (lon,lat,up) coordinates.
      do 720 i = 1,3
      do 720 j = 1,3
      a(i,j) = 0.0
      pauv(i,j) = 0.0
  720 v(i,j) = 0.0
      do 721 i = 1,3
      if(se(i) .eq. 0.0) se(i) = 99.
      a(i,i) = 1.0/(se(i)**2)
c     write(out,719) (a(i,j),j=1,3)
  721 continue
      do 725 i = 1,2
        dp = idip(i)*rpd
        bz = iaaz(i)*rpd
        pauv(i,1) = cos(dp)*sin(bz)
        pauv(i,2) = cos(dp)*cos(bz)
        pauv(i,3) = -sin(dp)
  725 continue
c     decide which way to cross multiply so that result is downward dipping
      irev = 1
      if(iaaz(2) .lt. iaaz(1)) irev = -irev
      dif = iabs(iaaz(2) - iaaz(1))
      if(dif .gt. 180.) irev = -irev
      ii = 1
      jj = 2
      if(irev .eq. 1) go to 727
      ii = 2
      jj = 1
  727 pauv(3,1) = pauv(ii,2)*pauv(jj,3)-pauv(jj,2)*pauv(ii,3)
      pauv(3,2) = pauv(jj,1)*pauv(ii,3)-pauv(ii,1)*pauv(jj,3)
      pauv(3,3) = pauv(ii,1)*pauv(jj,2)-pauv(jj,1)*pauv(ii,2)
      az = 0.
      if((pauv(3,1) .ne. 0.) .or. (pauv(3,2) .ne. 0))
     1 az = atan2(pauv(3,1),pauv(3,2))/rpd
      iaaz(3) = az + sign(0.5,az)
      if((pauv(3,1) .eq. 0.) .and. (pauv(3,2) .eq. 0.))
     1 iaaz(3) = 0
      if(iaaz(3) .lt. 0) iaaz(3) = iaaz(3) + 360
c  following line added to avoid truncation errors when dips 1 and 2 are zero
      if ((idip(1) .eq. 0) .and. (idip(2) .eq. 0)) pauv(3,3) = -1.
      dp = asin(-pauv(3,3))/rpd
      idip(3) = dp + sign(0.5,dp)
      do 735 i = 1,3
      do 735 j = i,3
        do 730 k = 1,3
        do 730 l = 1,3
          v(i,j) = v(i,j) + pauv(k,i)*pauv(l,j)*a(k,l)
  730   continue
        v(j,i) = v(i,j)
  735 continue
c------ find shadow on surface.
      ael(1) = (v(1,1)-v(1,3)**2/v(3,3))
      ael(2) =    (v(1,2)-v(1,3)*v(2,3)/v(3,3))
      ael(3) = (v(2,2)-v(2,3)**2/v(3,3))
c      write(out,719) ael
c  719 format (3f15.5/)
c------ compute principal axes.
      call eigen(ael,vec ,2)
      if(ael(1) .lt. 0.000009) ael(1) = 0.000009
      if(ael(3) .lt. 0.000009) ael(3) = 0.000009
c     do 825 i = 1,2
c 825 write(out,826) (vec(i,j),j=1,2)
c 826 format(2f15.5/)
      ax2 = .8095 /sqrt(ael(1))
      ax1 = .8095 /sqrt(ael(3))
c------ compute strike of principal axes
      aze2 = atan2(-vec(1,1),vec(2,1))/rpd
      aze1 = atan2(-vec(1,2),vec(2,2))/rpd
c------ compute maximum in z.
      axz = .5338 /sqrt(v(3,3)-v(1,3)**2/v(1,1)
     1      -(v(2,3)-v(1,3)*v(1,2)/v(1,1))**2
     2      /(v(2,2) - v(1,2)**2/v(1,1)))
      return
      end

      subroutine eigen(a,r,n)
      dimension a(3),r(4)
c
c...for double precision version see i.b.m. ssp manual h20-0205.....
c
c        generate identity matrix
c
      iq=-n
      do 20 j=1,n
      iq=iq+n
      do 20 i=1,n
      ij=iq+i
      r(ij)=0.0
      if(i-j) 20,15,20
   15 r(ij)=1.0
   20 continue
c
c        compute initial and final norms (anorm and anormx)
c
      anorm=0.0
      do 35 i=1,n
      do 35 j=i,n
      if(i-j) 30,35,30
   30 ia=i+(j*j-j)/2
      anorm=anorm+a(ia)*a(ia)
   35 continue
      if(anorm) 165,165,40
   40 anorm=1.414*sqrt(anorm)
      anrmx=anorm*1.0e-6/float(n)
c
c        initialize indicators and compute threshold, thr
c
      ind=0
      thr=anorm
   45 thr=thr/float(n)
   50 l=1
   55 m=l+1
c
c        compute sin and cos
c
   60 mq=(m*m-m)/2
      lq=(l*l-l)/2
      lm=l+mq
      if( abs(a(lm))-thr) 130,65,65
   65 ind=1
      ll=l+lq
      mm=m+mq
      x=0.5*(a(ll)-a(mm))
      y=-a(lm)/ sqrt(a(lm)*a(lm)+x*x)
      if(x) 70,75,75
   70 y=-y
   75 sinx=y/ sqrt(2.0*(1.0+( sqrt(1.0-y*y))))
      sinx2=sinx*sinx
      cosx= sqrt(1.0-sinx2)
      cosx2=cosx*cosx
      sincs =sinx*cosx
c
c        rotate l and m columns
c
      ilq=n*(l-1)
      imq=n*(m-1)
      do 125 i=1,n
      iq=(i*i-i)/2
      if(i-l) 80,120,80
   80 if(i-m) 85,120,90
   85 im=i+mq
      go to 95
   90 im=m+iq
   95 if(i-l) 100,105,105
  100 il=i+lq
      go to 110
  105 il=l+iq
  110 x=a(il)*cosx-a(im)*sinx
      a(im)=a(il)*sinx+a(im)*cosx
      a(il)=x
  120 ilr=ilq+i
      imr=imq+i
      x=r(ilr)*cosx-r(imr)*sinx
      r(imr)=r(ilr)*sinx+r(imr)*cosx
      r(ilr)=x
  125 continue
      x=2.0*a(lm)*sincs
      y=a(ll)*cosx2+a(mm)*sinx2-x
      x=a(ll)*sinx2+a(mm)*cosx2+x
      a(lm)=(a(ll)-a(mm))*sincs+a(lm)*(cosx2-sinx2)
      a(ll)=y
      a(mm)=x
c
c        tests for completion
c
c        test for m = last column
c
  130 if(m-n) 135,140,135
  135 m=m+1
      go to 60
c
c        test for l = second from last column
c
  140 if(l-(n-1)) 145,150,145
  145 l=l+1
      go to 55
  150 if(ind-1) 160,155,160
  155 ind=0
      go to 50
c
c        compare threshold with final norm
c
  160 if(thr-anrmx) 165,165,45
c
c        sort eigenvalues and eigenvectors
c
  165 iq=-n
      do 185 i=1,n
      iq=iq+n
      ll=i+(i*i-i)/2
      jq=n*(i-2)
      do 185 j=i,n
      jq=jq+n
      mm=j+(j*j-j)/2
      if(a(ll)-a(mm)) 170,185,185
  170 x=a(ll)
      a(ll)=a(mm)
      a(mm)=x
      do 180 k=1,n
      ilr=iq+k
      imr=jq+k
      x=r(ilr)
      r(ilr)=r(imr)
  180 r(imr)=x
  185 continue
      return
      end
