      subroutine mtcomp(m0,strike,rake,dip,scale,mxx,mxy,mxz,
     1     myy,myz,mzz)
c 
c ...... compute components of moment tensor given the 
c m0, strike, rake & dip for a double couple source
c 
c see Box 4.4 & eqn (14.18) in A&R
c 
      implicit real*4 (a-z)
      integer*4 iexpon
      data half,two/0.5,2.0/
c 
      rpd=atan(1.)/45.
      delta=rpd*dip
      lambda=rpd*rake
      phis=rpd*strike
      sd=sin(delta)
      cd=cos(delta)
      sl=sin(lambda)
      cl=cos(lambda)
      sp=sin(phis)
      cp=cos(phis)
      td=two*delta
      tp=two*phis
      s2d=sin(td)
      c2d=cos(td)
      s2p=sin(tp)
      c2p=cos(tp)
      mxx = -m0*(sd*cl*s2p+s2d*sl*sp*sp)
      mxy =  m0*(sd*cl*c2p+half*s2d*sl*s2p)
      mxz = -m0*(cd*cl*cp+c2d*sl*sp)
      myy =  m0*(sd*cl*s2p-s2d*sl*cp*cp)
      myz = -m0*(cd*cl*sp-c2d*sl*cp)
      mzz =  m0*(s2d*sl)
      scale=1.0
      argmax=abs(mxx)
      if(abs(mxy).gt.argmax) argmax=abs(mxy)
      if(abs(mxz).gt.argmax) argmax=abs(mxz)
      if(abs(myy).gt.argmax) argmax=abs(myy)
      if(abs(myz).gt.argmax) argmax=abs(myz)
      if(abs(mzz).gt.argmax) argmax=abs(mzz)
      iexpon=0
    1 iexpon=iexpon+1
      arg=10.**(iexpon)
      if(arg.le.argmax) go to 1
      scale=10.**(iexpon-1)
      mxx=mxx/scale
      mxy=mxy/scale
      mxz=mxz/scale
      myy=myy/scale
      myz=myz/scale
      mzz=mzz/scale
c 
      return
      end

      subroutine m0dcf(mtrep,mf,mt,d,v,miso,mdc,mclvd,m0,mw,axis,
     1     plunge,azimuth,strike,dip,slip,pcdc,pcclvd,pciso)
c 
c ...... decompose moment tensor into various representations
c 
c input parameters -
c 
c mtrep - moment tensor representation
c = 0 for spherical coordinates (r,d,p)
c ( Up, Away, South )
c = 1 for cartesian coordinates (x,y,z)
c ( North, East, Down )
c = 2 for f1,...,f6 notation
c 
c mf or mt - components of moment tensor
c | mrr mrd mrp |    | mxx mxy mxz |    | f1 f2 f3 |
c | mdr mdd mdp | or | myx myy myz | or | f2 f4 f5 |
c | mpr mpd mpp |    | mzx mzy mzz |    | f3 f5 f6 |
c 
c NOTE:
c f1 = mrr =  mzz
c f2 = mdd =  mxx
c f3 = mpp =  myy
c f4 = mrd =  mxz
c f5 = mrp = -myz
c f6 = mdp = -mxy
c 
c output parameters -
c 
c d       - eigenvalues
c v       - principal eigenvectors
c miso    - isotropic moment tensor
c mdc     - double-couple moment tensor
c mclvd   - compensated linear vector dipole moment tensor
c m0      - scalar seismic moment
c axis    - component axis ( P, N, T )
c plunge  - principle eigenvector plunge 
c (degrees below horizontal)
c azimuth - principle eigenvector azimuth
c (degrees clockwise from North)
c strike  - strike of fault plane
c (degrees clockwise from North)
c dip     - dip of fault plane
c (degrees below horizontal)
c slip    - slip of fault plane
c (degrees from horizontal)
c pciso   - percent isotropic source
c pcdc    - percent double-couple source
c pcclvd  - percent CLVD source
c 
      implicit real*8 (a-h,o-z)
      implicit integer*4 (i-n)
      character*1 axis(3)
      real*8 mt(3,3),d(3),v(3,3),miso(3,3),mdc(3,3),mclvd(3,3),m0,mw
      real*8 plunge(3),azimuth(3),strike(3),dip(3),slip(3)
      real*8 pcdc,pcclvd,a(3,3),p(3),t(3),mf(6)
      n=3
      np=3
c 
      call mtsrce(mtrep,mf,mt,a)
      call mdcmp(a,n,np,v,d,miso,mdc,mclvd)
      call pcnts(d,pcdc,pcclvd)
      call plaz(d,v,plunge,azimuth,axis)
      call m0find(d,m0)
c 
c ...... calculate percentage of DC, CLVD & Iso in source
c 
      pcdc=pcdc/100.d0
      pcclvd=pcclvd/100.d0
      pciso=dabs(miso(1,1))/m0
      pcsum=pcdc+pcclvd+pciso
      pcdc=100.d0*pcdc/pcsum
      pcclvd=100.d0*pcclvd/pcsum
      pciso=100.d0*pciso/pcsum
c 
c ...... compute moment magnitude
c 
      mw=dlog10(m0)/1.5-10.73
c 
      call ptfind(v,axis,p,t)
      call fpsol(p,t,strike,dip,slip) 
c 
c write(*,*) ' '
c write(*,'(a,1p3e12.2)') 'eigenvalues = ',(d(i),i=1,n)
c write(*,*) ' '
c write(*,'(a,1p3e12.2)') 'eigenvector = ',(v(1,i),i=1,n)
c write(*,'(a,1p3e12.2)') 'eigenvector = ',(v(2,i),i=1,n)
c write(*,'(a,1p3e12.2)') 'eigenvector = ',(v(3,i),i=1,n)
c write(*,*) ' '
c write(*,'(a,1p3e12.2)') 'miso        = ',(miso(1,i),i=1,n)
c write(*,'(a,1p3e12.2)') 'miso        = ',(miso(2,i),i=1,n)
c write(*,'(a,1p3e12.2)') 'miso        = ',(miso(3,i),i=1,n)
c write(*,*) ' '
c write(*,'(a,1p3e12.2)') 'mdc         = ',(mdc(1,i),i=1,n)
c write(*,'(a,1p3e12.2)') 'mdc         = ',(mdc(2,i),i=1,n)
c write(*,'(a,1p3e12.2)') 'mdc         = ',(mdc(3,i),i=1,n)
c write(*,*) ' '
c write(*,'(a,1p3e12.2)') 'mclvd       = ',(mclvd(1,i),i=1,n)
c write(*,'(a,1p3e12.2)') 'mclvd       = ',(mclvd(2,i),i=1,n)
c write(*,'(a,1p3e12.2)') 'mclvd       = ',(mclvd(3,i),i=1,n)
c write(*,*) ' '
c write(*,*) ' '
c write(*,'(a,1pe10.2)') 'Scalar Moment    = ',m0
c write(*,*) ' '
c write(*,'(a,  f10.2)') 'Moment Magnitude = ',mw
c write(*,*) ' '
c write(*,'(a,7x,a1,2(9x,a1))') 'Axis             = ',
c 1         (axis(i),i=1,n)
c write(*,*) ' '
c write(*,'(a,3f10.2)') 'Plunge           = ',(plunge(i),i=1,n)
c write(*,*) ' '
c write(*,'(a,3f10.2)') 'Azimuth          = ',(azimuth(i),i=1,n)
c write(*,*) ' '
c write(*,*) ' '
c write(*,'(2a)')       'Plane            = ',
c 1           '      P1        P2'
c write(*,*) ' '
c write(*,'(a,2f10.2)') 'Strike           = ',(strike(i),i=1,n-1)
c write(*,*) ' '
c write(*,'(a,2f10.2)') 'Dip              = ',(dip(i),i=1,n-1)
c write(*,*) ' '
c write(*,'(a,2f10.2)') 'Slip             = ',(slip(i),i=1,n-1)
c write(*,*) ' '
c write(*,*) ' '
c write(*,*)            'Source Composition     Percent'
c write(*,*) ' '
c write(*,'(a, f10.2)') 'Isotropic        = ',pciso
c write(*,*) ' '
c write(*,'(a, f10.2)') 'Double-Couple    = ',pcdc
c write(*,*) ' '
c write(*,'(a, f10.2)') 'CLVD             = ',pcclvd
c write(*,*) ' '
c 
      return
      end

      subroutine pcnts(d,pcdc,pcclvd)
c 
c ...... calculate percentages of double-couple, CLVD & iso
c in seismic source
c 
      implicit real*8 (a-h,o-z)
      implicit integer*4 (i-n)
      real*8 d(3),dd(3)
      do 1 i=1,3
         dd(i)=dabs(d(i))
    1 continue
      do 3 i=1,3
         do 2 j=i,3
            if(dd(j).lt.dd(i)) then
               temp=dd(i)
               dd(i)=dd(j)
               dd(j)=temp
            endif
 2       continue
    3 continue
      eps=dd(1)/dd(3)
      pcdc=100.d0*(1.d0-2.d0*eps)
      pcclvd=200.d0*eps
      return
      end

      subroutine m0find(d,m0)
c 
c ...... find scalar moment M0 
c 
      implicit real*8 (a-h,o-z)
      implicit integer*4 (i-n)
      real*8 d(3),m0
      emin=d(1)
      emax=d(1)
      do 1 i=2,3
         if(d(i).lt.emin) emin=d(i)
         if(d(i).gt.emax) emax=d(i)
    1 continue
      m0=(dabs(emin)+dabs(emax))/2.d0
      return
      end

      subroutine ptfind(v,axis,p,t)
c 
c ...... find P & T vectors
c 
      implicit real*8 (a-h,o-z)
      implicit integer*4 (i-n)
      real*8 v(3,3),p(3),t(3)
      character*1 axis(3),pch,tch
      pch='P'
      tch='T'
      do 3 i=1,3
         if(axis(i).eq.pch) then
            do 1 j=1,3
               p(j)=v(j,i)
 1          continue
         elseif(axis(i).eq.tch) then
            do 2 j=1,3
               t(j)=v(j,i)
 2          continue
         endif
    3 continue
      return
      end

      subroutine mtsrce(mtrep,mf,mt,a)
c 
c ...... set up appropriate moment tensor components from input
c moment tensor representation
c 
      implicit real*8 (a-h,o-z)
      implicit integer*4 (i-n)
      real*8 mf(6),mt(3,3),a(3,3)
      if(mtrep.eq.0) then
         a(1,1)= mt(2,2)
         a(1,2)=-mt(2,3)
         a(1,3)= mt(2,1)
         a(2,1)=-mt(2,3)
         a(2,2)= mt(3,3)
         a(2,3)=-mt(3,1)
         a(3,1)= mt(2,1)
         a(3,2)=-mt(3,1)
         a(3,3)= mt(1,1)
      elseif(mtrep.eq.1) then
         a(1,1)=mt(1,1)
         a(1,2)=mt(1,2)
         a(1,3)=mt(1,3)
         a(2,1)=mt(1,2)
         a(2,2)=mt(2,2)
         a(2,3)=mt(2,3)
         a(3,1)=mt(1,3)
         a(3,2)=mt(2,3)
         a(3,3)=mt(3,3)
      else
         a(1,1)= mf(2)
         a(1,2)=-mf(6)
         a(1,3)= mf(4)
         a(2,1)=-mf(6)
         a(2,2)= mf(3)
         a(2,3)=-mf(5)
         a(3,1)= mf(4)
         a(3,2)=-mf(5)
         a(3,3)= mf(1)
      endif
      return
      end
      
      subroutine fpsol(p,t,strike,dip,slip)
      use trigd
c 
c ...... calculate strike, dip & slip of fault planes
c from the P & T vectors
c 
      implicit real*8 (a-h,o-z)
      implicit integer*4 (i-n)
      real*8 p(3),t(3),strike(2),dip(2),slip(2)
      real*8 u(2,3),nu(2,3),lambda
      integer*4 idarg(2)
      con=1.d0/dsqrt(2.d0)
      do 1 i=1,3
         u(1,i)=con*(t(i)+p(i))
         nu(1,i)=con*(t(i)-p(i))
         u(2,i)=con*(t(i)-p(i))
         nu(2,i)=con*(t(i)+p(i))
    1 continue
c write(*,11) (p(i),i=1,3)
c 11 format(1h ,'P-vector = ',3f10.6)
c write(*,12) (t(i),i=1,3)
c 12 format(1h ,'T-vector = ',3f10.6)
      idarg(1)=0
      idarg(2)=0      
      do 2 i=1,2
         dip(i)=dacosd(-nu(i,3))
         if((nu(i,1).eq.0.d0).and.(nu(i,2).eq.0.d0)) then
            strike(i)=0.d0
         else
            strike(i)=datan2d(-nu(i,1),nu(i,2))
         endif
    2 continue
      do 3 i=1,2
         sstr=dsind(strike(i))
         cstr=dcosd(strike(i))
         sdip=dsind(dip(i))
         cdip=dcosd(dip(i))
         if(dabs(sdip).gt.0.d0) then
            lambda=dasind(-u(i,3)/dsind(dip(i)))
         else
            arg1=1.d0
            arg2=u(i,3)
            arg=dsign(arg1,arg2)
            if(arg.lt.0.d0) then
               lambda=180.d0
            else
               lambda=0.d0
            endif
         endif
         slambda=dsind(lambda)
         cdsl=cdip*slambda
         if(dabs(sstr).gt.dabs(cstr)) then
            clambda=(u(i,2)+cdsl*cstr)/sstr
         else
            clambda=(u(i,1)-cdsl*sstr)/cstr
         endif
         if((slambda.eq.0.d0).and.(clambda.eq.0.d0)) then
            slip(i)=0.d0
         else
            slip(i)=datan2d(slambda,clambda)
         endif
         if(dip(i).gt.90.d0) then
            dip(i)=180.d0-dip(i)
            strike(i)=strike(i)+180.d0
            slip(i)=360.d0-slip(i)
         endif
         if(strike(i).lt.0.d0) strike(i)=strike(i)+360.d0
         if(slip(i).ge.180.d0) slip(i)=slip(i)-360.d0
    3 continue
      return
      end

      subroutine plaz(d,v,plunge,azimuth,axis)
      use trigd
c 
c ...... calculate plunge & azimuth of eigenvectors
c in degrees
c 
      implicit real*8 (a-h,o-z)
      implicit integer*4 (i-n)
      real*8 d(3),v(3,3),plunge(3),azimuth(3)
      character*1 axis(3)
      do 1 i=1,3
         if(v(3,i).lt.0.) then
            v(1,i)=-v(1,i)
            v(2,i)=-v(2,i)
            v(3,i)=-v(3,i)
         endif
    1 continue
      do 2 i=1,3
         if((v(2,i).eq.0.d0).and.(v(1,i).eq.0.d0)) then
            azimuth(i)=0.d0
         else
            azimuth(i)=datan2d(v(2,i),v(1,i))
         endif
         if(azimuth(i).lt.0.) azimuth(i)=azimuth(i)+360.0
         r=dsqrt(v(1,i)*v(1,i)+v(2,i)*v(2,i))
         if((v(3,i).eq.0.d0).and.(r.eq.0.d0)) then
            plunge(i)=0.d0
         else
            plunge(i)=datan2d(v(3,i),r)
         endif
    2 continue
      axis(1)='N'
      if(d(2).gt.d(3)) then
         axis(2)='T'
         axis(3)='P'
      else
         axis(2)='P'
         axis(3)='T'
      endif
      return
      end

      subroutine mdcmp(mij,n,np,v,d,miso,mdc,mclvd)
c 
c ...... calculate components of an arbitrary moment tensor
c 
      implicit real*8 (a-h,m,o-z)
      implicit integer*4 (i-l,n)
      real*8 mij(np,np),miso(np,np),mdc(np,np),mclvd(np,np)
      real*8 a(3,3),v(3,3),d(3)
      real*8 a1a1(3,3),a2a2(3,3),a3a3(3,3)
      do 2 i=1,n
         do 1 j=1,n
            a(i,j)=mij(i,j)
 1       continue
    2 continue
      trace=a(1,1)+a(2,2)+a(3,3)
      tr3=trace/3.0d0
      do 3 i=1,n
         a(i,i)=a(i,i)-tr3
    3 continue
      do 5 i=1,n
         do 4 j=1,n
            if(i.eq.j) then
               miso(i,j)=tr3
            else
               miso(i,j)=0.d0
            endif
 4       continue
    5 continue
      call jacobi(a,n,np,d,v,nrot)
c write(*,*) 'jacobi - nrot = ',nrot
      call eigsrt(d,v,n,np)
      f=-d(1)/d(3)
      c=d(3)*(1.d0-2.d0*f)
      call dyadic(v,3,3,c,n,np,a3a3)
      c=-c
      call dyadic(v,2,2,c,n,np,a2a2)
      call matsum(a3a3,a2a2,n,np,mdc)
      c=2.d0*d(3)*f
      call dyadic(v,3,3,c,n,np,a3a3)
      c=-d(3)*f
      call dyadic(v,2,2,c,n,np,a2a2)
      call dyadic(v,1,1,c,n,np,a1a1)
      call matsum(a3a3,a2a2,n,np,mclvd)
      call matsum(mclvd,a1a1,n,np,mclvd)
      return
      end

      subroutine dyadic(v,n1,n2,c,n,np,d)
c 
c ...... calculate dyadic of eigenvectors v(i,n1)*v(j,n2)
c scaled by c
c 
      implicit real*8 (a-h,o-z) 
      implicit integer*4 (i-n)
      real*8 v(np,np),d(np,np) 
      do 2 i=1,n
         do 1 j=1,n
            d(i,j)=v(i,n1)*v(j,n2)*c
 1       continue
    2 continue
      return
      end

      subroutine matsum(a,b,n,np,c)
c 
c ...... calculate matrix sum c = a + b
c 
      implicit real*8 (a-h,o-z)
      implicit integer*4 (i-n)
      real*8 a(np,np),b(np,np),c(np,np)
      do 2 i=1,n
         do 1 j=1,n
            c(i,j)=a(i,j)+b(i,j)
 1       continue 
    2 continue
      return
      end

      subroutine rotate(v,n,np,a,b)
c 
c ...... rotate matrix
c T
c b = v * a * v
c 
      implicit real*8 (a-h,o-z)
      implicit integer*4 (i-n)
      real*8 v(np,np),a(np,np),b(np,np),c(3,3)
      do 3 i=1,n
         do 2 j=1,n
            sum=0.d0
            do 1 k=1,n
               sum=sum+a(i,k)*v(j,k)
 1          continue
            c(i,j)=sum
 2       continue
    3 continue
      do 6 i=1,n
         do 5 j=1,n
            sum=0.d0
            do 4 k=1,n
               sum=sum+v(i,k)*c(k,j)
 4          continue
            b(i,j)=sum
 5       continue
    6 continue
      return
      end

      subroutine jacobi(a,n,np,d,v,nrot)
c 
c ...... Computes all eigenvalues and eigenvectors of a real 
c symmetric matrix A, which is of size N by N, stored
c in a physical np by np array.  On output, elements
c of A above the diagonal are destroyed.  D returns
c the eigenvalues of A in its first N elements.  V is
c a matrix with the same logical and physical dimensions
c as A whose columns contain, on output, the normalized
c eigenvectors of A.  NROT returns the number of Jacobi
c rotations which were required.
c 
      implicit real*8 (a-h,o-z)
      implicit integer*4 (i-n)
      parameter (nmax=100)
      real*8 a(np,np),d(np),v(np,np),b(nmax),z(nmax)
      do 12 ip=1,n
         do 11 iq=1,n
            v(ip,iq)=0.d0
 11      continue
         v(ip,ip)=1.d0
 12   continue
      do 13 ip=1,n
         b(ip)=a(ip,ip)
         d(ip)=b(ip)
         z(ip)=0.d0
 13   continue
      nrot=0
      do 24 i=1,500
         sm=0.d0
         do 15 ip=1,n-1
            do 14 iq=ip+1,n
               sm=sm+dabs(a(ip,iq))
 14         continue
 15      continue
         if(sm.eq.0.d0) return
         if(i.lt.1) then
            thresh=0.2d0*sm/dble(n**2)
         else
            thresh=0.d0
         endif
         do 22 ip=1,n-1
            do 21 iq=ip+1,n
               g=100.d0*dabs(a(ip,iq))
               if((i.gt.4).and.(dabs(d(ip))+g.eq.dabs(d(ip)))
     1              .and.(dabs(d(iq))+g.eq.dabs(d(iq)))) then
                  a(ip,iq)=0.d0
               elseif(dabs(a(ip,iq)).gt.thresh) then
                  h=d(iq)-d(ip)
                  if(dabs(h)+g.eq.dabs(h)) then
                     t=a(ip,iq)/h
                  else
                     theta=0.5d0*h/a(ip,iq)
                     t=1.d0/(dabs(theta)+dsqrt(1.d0+theta**2))
                     if(theta.lt.0.d0) t=-t
                  endif
                  c=1.d0/dsqrt(1.d0+t**2)
                  s=t*c
                  tau=s/(1.d0+c)
                  h=t*a(ip,iq)
                  z(ip)=z(ip)-h
                  z(iq)=z(iq)+h
                  d(ip)=d(ip)-h
                  d(iq)=d(iq)+h
                  a(ip,iq)=0.d0
                  do 16 j=1,ip-1
                     g=a(j,ip)
                     h=a(j,iq)
                     a(j,ip)=g-s*(h+g*tau)
                     a(j,iq)=h+s*(g-h*tau)
 16               continue
                  do 17 j=ip+1,iq-1
                     g=a(ip,j)
                     h=a(j,iq)
                     a(ip,j)=g-s*(h+g*tau)
                     a(j,iq)=h+s*(g-h*tau)
 17               continue
                  do 18 j=iq+1,n
                     g=a(ip,j)
                     h=a(iq,j)
                     a(ip,j)=g-s*(h+g*tau)
                     a(iq,j)=h+s*(g-h*tau)
 18               continue
                  do 19 j=1,n
                     g=v(j,ip)
                     h=v(j,iq)
                     v(j,ip)=g-s*(h+g*tau)
                     v(j,iq)=h+s*(g-h*tau)
 19               continue
                  nrot=nrot+1
               endif
 21         continue
 22      continue
         do 23 ip=1,n
            b(ip)=b(ip)+z(ip)
            d(ip)=b(ip)
            z(ip)=0.d0
 23      continue
 24   continue
      return
      end
      
      subroutine eigsrt(d,v,n,np)
c 
c ...... Given the eigenvalues D and eigenvectors V as output from
c JACOBI, this routine sorts the eigenvalues into increasing
c absolute order and rearranges the columns of V 
c correspondingly.  The mothod is straight insertion.
c 
      implicit real*8 (a-h,o-z)
      implicit integer*4 (i-n)
      real*8 d(np),v(np,np)
      do 13 i=1,n-1
         k=i
         p=d(i)
         do 11 j=i+1,n
            if(dabs(d(j)).lt.dabs(p)) then
               k=j
               p=d(j)
            endif
 11      continue
         if(k.ne.i) then
            d(k)=d(i)
            d(i)=p
            do 12 j=1,n
               p=v(j,i)
               v(j,i)=v(j,k)
               v(j,k)=p
 12         continue
         endif
 13   continue
      return
      end
