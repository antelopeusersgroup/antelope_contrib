c last modified oct 11 1999 to change ivel field from 1 to 100
      parameter (lin=5,lot=6,nl=70,nder=1)
      character*80 rep*1,ivel*100
      common/source/z(8192)
      common/aup/gg(100,4097,10)
      common/model/d(nl),a(nl),b(nl),rho(nl),mmax,qa(nl),qb(nl)
      complex data(8192),gg
      dimension isrc(10),omega(4097)
      dimension range(100),vred(100),t0(100)
      real*8 aa,bb
      real temp(4097),totint,prtint
      real dataout(4097)
c
      open(12,file='GREEN.1',status='old',access='sequential',
     $             form='unformatted')
c
      read(12) alpha,depth,fl,fu,dt,n1,n2,df,nyq,nrange,nskip
      read(12) isrc
      read(12) d,a,b,rho,mmax,qa,qb
      read(12) range,vred,t0
c
      if(nskip.gt.1) open(13,file='GREEN.2',status='old',
     $access='sequential',form='unformatted')
      if(nskip.gt.2) open(14,file='GREEN.3',status='old',
     $access='sequential',form='unformatted')
      if(nskip.gt.3) open(15,file='GREEN.4',status='old',
     $access='sequential',form='unformatted')
c
      open(11,file="junk",status='unknown',access='sequential'
     $      ,form='unformatted')
c
c
      if(nskip.gt.1) read(13) alpha,depth,fl,fu,dt,n1,n2,df,nyq,
     $nrange,nskip
      if(nskip.gt.1) read(13) isrc
      if(nskip.gt.1) read(13) d,a,b,rho,mmax,qa,qb
      if(nskip.gt.1) read(13) range,vred,t0
c
      if(nskip.gt.2) read(14) alpha,depth,fl,fu,dt,n1,n2,df,nyq,
     $nrange,nskip
      if(nskip.gt.2) read(14) isrc
      if(nskip.gt.2) read(14) d,a,b,rho,mmax,qa,qb
      if(nskip.gt.2) read(14) range,vred,t0
c
      if(nskip.gt.3) read(15) alpha,depth,fl,fu,dt,n1,n2,df,nyq,
     $nrange,nskip
      if(nskip.gt.3) read(15) isrc
      if(nskip.gt.3) read(15) d,a,b,rho,mmax,qa,qb
      if(nskip.gt.3) read(15) range,vred,t0
c
      nm=n2
      rep='n'
      ivel='d'
      print*,' Do you want: displ (d) or velocity (v)' 
      read(lin,'(a100)') ivel
      if(ivel.eq.'D') ivel='d'
      if(ivel.eq.'V') ivel='v'
c
      n=2*(nyq-1)
c
c     npoint=n2
      npoint=n
      tau = dt
c
c
      fmax = fu
      inst = 0
c
c    beginning of distance loop
c
      pi=acos(-1.0)
      twpi=2.*pi
      do 1 i=1,nm,nskip
                     read(12) omega(i),nkk
      if((nskip.gt.1).and.((i+1).le.nm)) read(13) omega(i+1),nkk
      if((nskip.gt.2).and.((i+2).le.nm)) read(14) omega(i+2),nkk
      if((nskip.gt.3).and.((i+3).le.nm)) read(15) omega(i+3),nkk
      do 2 j=1,nrange
      do 3 k=1,10
      if(isrc(k).eq.1) then
                                 read(12) aa,bb
                                 gg(j,i,k)=cmplx(sngl(aa),sngl(bb))
      if((nskip.gt.1).and.((i+1).le.nm))
     &                          read(13) aa,bb
      if((nskip.gt.1).and.((i+1).le.nm))
     &			        gg(j,i+1,k)=cmplx(sngl(aa),sngl(bb))
      if((nskip.gt.2).and.((i+2).le.nm))
     &			        read(14) aa,bb
      if((nskip.gt.2).and.((i+2).le.nm))
     &			        gg(j,i+2,k)=cmplx(sngl(aa),sngl(bb))
      if((nskip.gt.3).and.((i+3).le.nm))
     &				read(15) aa,bb
      if((nskip.gt.3).and.((i+3).le.nm))
     &				gg(j,i+3,k)=cmplx(sngl(aa),sngl(bb))
      endif
    3 continue
    2 continue
    1 continue
      close(12)
      close(13)
      close(14)
      close(15)
c
      do 4 nd=1,nrange
      t0x=range(nd)/vred(nd)
      yr = 0.0
c
      write(11) range(nd),yr,depth,npoint,t0x,dt,tau
c
      do 5 l=1,10
      if(isrc(l).ne.1) go to 444
      do 6 j=1,nm
    6 data(j)=gg(nd,j,l)
      do 7 j=nm+1,nyq
      data(j)=cmplx(0.0,0.0)
    7 continue
      do 8 j=1,nm
      freq=(j-1)*df
      if(freq.lt.df) freq=0.01*df
      om=twpi*freq
      if(j.ne.1)data(n+2-j)=conjg(data(j))
    8 continue
      do 9 i=nm+1,nyq
      data(i)=(0.,0.)
    9 data(n+2-i)=conjg(data(i))
c
      data(1)=cmplx(real(data(1)),0.0)
      data(nyq)=cmplx(real(data(nyq)),0.0)
      data(1)=(0.0,0.0)
      data(nyq)=(0.0,0.0)
      call four(data,n,+1,dt,df)
c     correct for damping
      fac = exp(alpha*t0x)
      dfac = exp(alpha*dt)
      do 10 i = 1,npoint
      data(i)= data(i) * fac
      fac = fac * dfac
   10 continue
      go to 122
  444 do 12 j=1,n
   12 data(j)=(0.0,0.0)
c     Time Domain Integration
  122 if(ivel.eq.'d') then
      totint=0.0
      do 123 i=1,npoint
      temp(i)=real(data(i))
  123 continue
      do 124 i=1,npoint-1
      prtint=0.5*dt*(temp(i)+temp(i+1))
      totint=totint+prtint
      temp(i)=totint
  124 continue
      do 125, i=1,npoint-1
c     data(i)=temp(i+1) - temp(1)
      data(i)=temp(i) - temp(1)
  125 continue
      endif
c 
      if(l.eq.1) then
      do 200 i=1,npoint
      data(i)=data(i)*(-1.0)
  200 continue
      write(*,126) npoint,dt
  126 format("NPTS ",I5,"  DT ",f6.4)
      endif
      if(l.eq.4) then
      do 201 i=1,npoint
      data(i)=data(i)*(-1.0)
  201 continue
      endif
      if(l.eq.6) then
      do 202 i=1,npoint
      data(i)=data(i)*(-1.0)
  202 continue
      endif
      if(l.eq.8) then
      do 203 i=1,npoint
      data(i)=data(i)*(-1.0)
  203 continue
      endif
      do 204 i=1,npoint
      dataout(i)=real(data(i))
  204 continue
      call cwrite(dataout,npoint,nd,l)
    5 continue
    4 continue
      close(11)
      close(10)
      stop
      end
c
c
      subroutine four(data,nn,isign,dt,df)
      dimension data(1)
      n = 2 * nn
      if(dt.eq.0.0) dt = 1./(nn*df)
      if(df.eq.0.0) df = 1./(nn*dt)
      if(dt.ne.(nn*df)) df = 1./(nn*dt)
      j = 1
      do 5 i=1,n,2
      if(i-j)1,2,2
    1 tempr = data(j)
      tempi = data(j+1)
      data(j) = data(i)
      data(j+1)=data(i+1)
      data(i) = tempr
      data(i+1) = tempi
    2 m = n/2
    3 if(j-m) 5,5,4
    4 j = j-m
      m = m/2
      if(m-2)5,3,3
    5 j=j+m
      mmax = 2
    6 if(mmax-n) 7,10,10
    7 istep= 2 *mmax
      theta = 6.283185307/float(isign*mmax)
      sinth=sin(theta/2.)
      wstpr=-2.*sinth*sinth
      wstpi=sin(theta)
      wr=1.0
      wi=0.0
      do 9 m=1,mmax,2
      do 8 i=m,n,istep
      j=i+mmax
      tempr=wr*data(j)-wi*data(j+1)
      tempi=wr*data(j+1)+wi*data(j)
      data(j)=data(i)-tempr
      data(j+1)=data(i+1)-tempi
      data(i)=data(i)+tempr
    8 data(i+1) = data(i+1)+tempi
      tempr = wr
      wr = wr*wstpr-wi*wstpi + wr
    9 wi = wi*wstpr+tempr*wstpi + wi
      mmax = istep
      go to 6
   10 continue
      if(isign.lt.0) go to 1002
c     frequency to time domain
      do 1001 iiii = 1,n
 1001 data(iiii) = data(iiii) * df
      return
 1002 continue
c     time to frequency domain
      do 1003 iiii = 1,n
 1003 data(iiii) = data(iiii) * dt
      return
      end
