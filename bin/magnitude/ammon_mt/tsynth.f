c     tsynth.f takes output spectrum from kennett.f and computes
c     synthetic seismograms      
      integer mxsrc,maxnr,maxf,mxdep
      parameter (mxsrc=6,mxdep=5,maxnr=50,maxf=4097)
      character*30 sacu0,sacw0,sactn,sacu0f,sacw0f,sactnf
      character*10 rngstr,depstr,mech(mxsrc),fmech(3)
      complex u0(maxf),w0(maxf),tn(maxf),utot,wtot,tntot,ws
      complex u0f(2049),w0f(2049),tnf(2049),utotf,wtotf,tntotf,wsf
      real u0tf(2048),w0tf(2048),tntf(2048)
      real fmin,fmax,delf,fny,pi,dt,r,vred,tdly,tred,flp,f
      real tlp,fr,up,wp,tp,d,scale
      integer nft,ifmin,ifmax,nfppts,ir,is,i,iff,nfpts,nerr
      integer inunit,iounit,nr,ii,isl,nsrc,id,ndep,iid,iargc,nargc
      real twopi,rchk,dchk,dummy
      logical pntfrc,displ,veloc,stepstf,impstf
      complex cmplx,cexp
      intrinsic cos,cmplx,cexp
      real rr(maxnr),dd(mxdep),u0t(maxf),w0t(maxf),tnt(maxf)
c
      inunit=5
      iounit=6
      displ = .false.
      veloc = .true.
      stepstf = .true.
      impstf = .false.
c
      nargc = iargc()
      if ( nargc .gt. 0 ) then
	 call chkopts(nargc,displ,veloc,stepstf,impstf)
      endif
c
      open( unit = 7, file = 'mspec' , form = 'unformatted')
      read(7) fmin, fmax, delf, nfppts, fny, nfpts,nr,nsrc,ndep
      read(7) (rr(i),i=1,nr)
      read(7) (dd(i),i=1,ndep)
      inquire( file= 'fspec' , exist = pntfrc )
      if ( pntfrc ) then
	open( unit = 8, file = 'fspec' , form = 'unformatted')
	read(8) fmin, fmax, delf, nfppts, fny, nfpts,nr,ndep
	read(8) (rr(i),i=1,nr)
        read(8) (dd(i),i=1,ndep)
      endif
      pi = 4.0*atan(1.0)
      twopi = 2.0*pi
      nft =  2 * ( nfpts - 1 )
      dt = 1. / ( 2 * fny )
      ifmin = fmin / delf
      ifmax = fmax / delf
      nfppts = ifmax - ifmin + 1
      write(iounit,*) 'enter reducing velocity, tdly: '
      read(inunit,*)  vred, tdly
c     write(iounit,*) ' enter lp frequency cutoff, or 0. for no lp'
c     read(inunit,*) flp
c     if(flp.eq.0.) flp=1.e9
c
      mech(1) = '_mzz '
      mech(2) = '_mxy '
      mech(3) = '_mxz '
      mech(4) = '_mxx '
      mech(5) = '_myz '
      mech(6) = '_myy '
      if ( nsrc .eq. 1 ) mech(1) = '_mij '
c
      fmech(1) = '_fz '
      fmech(2) = '_fx '
      fmech(3) = '_fy '
c
      do 30 ii = 1,nr
      r = rr(ii)
      tred = r / vred - tdly
      ir = r
c
c     check to see if r is fraction of a km
c     note that this is not fool proof
c
      rchk = float(ir) - r
      if(rchk .eq. 0.) then
       write(rngstr,'(i4.4)') ir
      elseif (10. .le. r .and. r .lt. 100.) then
       write(rngstr,'(f4.1)') r
      elseif (r .ge. 100.) then
       write(rngstr,'(f5.1)') r
      endif
c
      do 30 id = 1, ndep
      d = dd(id)
      iid = d
c
c     check to see if r is fraction of a km
c     note that this is not fool proof
c
      dchk = float(iid) - d
      write(iounit,*) dchk
      write(iounit,*) '  '
      write(iounit,*) float(iid)
      write(iounit,*) '  '
      write(iounit,*) d
      write(iounit,*) '  '
      if(dchk .eq. 0.) then
       write(depstr,'(i4.4)') iid
      elseif (10. .le. d .and. d .lt. 100.) then
       write(depstr,'(f4.1)') d
      elseif (d .ge. 100.) then
       write(depstr,'(f5.1)') d
      endif
c
      do 1 is = 1, nsrc
c
      do 5 i = 1, 2*nfpts
         u0(i) = (0.,0.)
         w0(i) = (0.,0.)
         tn(i) = (0.,0.)
         if ( pntfrc .and. (is .ge. 1) .and. (is .le. 3) ) then
           u0f(i) = (0.,0.)
           w0f(i) = (0.,0.)
           tnf(i) = (0.,0.)
         endif
5      continue
c
c
      do 10 i = ifmin, ifmax
         read(7) utot, wtot, tntot
         iff = i-ifmin+1
         u0(iff) = utot
         w0(iff) = wtot
         tn(iff) = tntot
         if ( pntfrc .and. (is .ge. 1) .and. (is .le. 3) ) then
           read(8) utotf, wtotf, tntotf
           u0f(iff) = utotf
           w0f(iff) = wtotf
           tnf(iff) = tntotf
         endif
10    continue
      up = tred * 2*3.14159*delf
      wp = tred * 2*3.14159*delf
      tp = tred * 2*3.14159*delf
      f=fmin
      do 15 i = 1,nfpts
         tlp=1.0
c        if(f.gt.flp) tlp=0.5*(1.0+cos(pi*(f-flp)/(fny-flp)))
         u0(i) = u0(i) * cexp( cmplx(0., (i-1)*up) ) * tlp
         w0(i) = w0(i) * cexp( cmplx(0., (i-1)*wp) ) * tlp
         tn(i) = tn(i) * cexp( cmplx(0., (i-1)*tp) ) * tlp
         if ( pntfrc .and. (is .ge. 1) .and. (is .le. 3) ) then
           u0f(i) = u0f(i) * cexp( cmplx(0., (i-1)*up) ) * tlp
           w0f(i) = w0f(i) * cexp( cmplx(0., (i-1)*wp) ) * tlp
           tnf(i) = tnf(i) * cexp( cmplx(0., (i-1)*tp) ) * tlp
         endif
         f=f+delf
15    continue
c
c     if ( u0(nfpts) .ne. (0.,0.) ) then
c        up = atan2( aimag(u0(nfpts)), real(u0(nfpts)) )/(nfpts - 1)
c       else
c        up = 0.
c       endif
c     if ( w0(nfpts) .ne. (0.,0.) ) then
c        wp = atan2( aimag(w0(nfpts)), real(w0(nfpts)) )/(nfpts - 1)
c       else
c        wp = 0.
c       endif
c     if ( tn(nfpts) .ne. (0.,0.) ) then
c        tp = atan2( aimag(tn(nfpts)), real(tn(nfpts)) )/(nfpts - 1)
c       else
c        tp = 0.
c       endif
c
c Displacement Spectrum has a factor of omeaga**2 from integration of k*dk
c   the Kennett integration is over slowenss p*dp and leaves
c   the remaining factor of omega**2 and and source time spectrum
c   to be included here,
c   Mij moment sources have omega**2 but Fk force sources only omega
c   because the point force source has 1/omega to include
c   Step Source Time Function  is 1/(-i*omega)
c   Impulse Source Time Function is 1
c Velocity spectrum has a factor of (i*omega) * omega**2 [d/dt displ]
c
c
      do 20 i = 1,nfpts
         fr = (i-1)*delf
         if ( displ .and. stepstf ) then
      	    ws = (0.,1.)*fr*twopi
      	    wsf = (0.,1.)
	 endif
         if ( displ .and. impstf ) then
      	    ws = fr*twopi*fr*twopi
      	    wsf = fr*twopi
	 endif
         if ( veloc .and. stepstf ) then
      	    ws = fr*twopi*fr*twopi
      	    wsf = fr*twopi
	 endif
         if ( veloc .and. impstf ) then
      	    ws = (0.,1.)*fr*twopi*fr*twopi*fr*twopi
      	    wsf = (0.,1.)*fr*twopi*fr*twopi
	 endif
         u0(i) = ws * u0(i)
         w0(i) = ws * w0(i)
         tn(i) = ws * tn(i)
         if ( i .gt. 1 .and. i .lt. nfpts ) then
            iff = nft - i + 2
            u0(iff) = conjg( u0(i) )
            w0(iff) = conjg( w0(i) )
            tn(iff) = conjg( tn(i) )
            if ( pntfrc .and. (is .ge. 1) .and. (is .le. 3) ) then
	      u0f(i) = wsf * u0f(i)
              u0f(iff) = conjg( u0f(i) )
	      w0f(i) = wsf * w0f(i)
              w0f(iff) = conjg( w0f(i) )
	      tnf(i) = wsf * tnf(i)
              tnf(iff) = conjg( tnf(i) )
            endif
         endif
20    continue
      call fft(u0,nft,1)
      call fft(w0,nft,1)
      call fft(tn,nft,1)
        if ( pntfrc .and. (is .ge. 1) .and. (is .le. 3) ) then
        call fft(u0f,nft,1)
        call fft(w0f,nft,1)
        call fft(tnf,nft,1)
        endif
      scale = 1 / ( nft * dt * 4 * pi )
      do 40 i = 1,nft
        u0t(i) = real( u0(i) ) * scale
        w0t(i) = real( w0(i) ) * scale
        tnt(i) = real( tn(i) ) * scale
        if ( pntfrc .and. (is .ge. 1) .and. (is .le. 3) ) then
          u0tf(i) = real( u0f(i) ) * scale
          w0tf(i) = real( w0f(i) ) * scale
          tntf(i) = real( tnf(i) ) * scale
        endif
40    continue
c
      isl = is
      sacu0 = 'z_' // rngstr(1:4) // "_" // depstr(1:4) // mech(isl)(1:4) 
      sacw0 = 'r_' // rngstr(1:4) // "_" // depstr(1:4) // mech(isl)(1:4) 
      sactn = 't_' // rngstr(1:4) // "_" // depstr(1:4) // mech(isl)(1:4) 
      if ( pntfrc .and. (is .ge. 1) .and. (is .le. 3) ) then
        sacu0f = 'z_' // rngstr(1:4) // "_"  // depstr(1:4) // fmech(is)(1:3) 
        sacw0f = 'r_' // rngstr(1:4) // "_"  // depstr(1:4) // fmech(is)(1:3) 
        sactnf = 't_' // rngstr(1:4) // "_"  // depstr(1:4) // fmech(is)(1:3) 
c       call wsac1(sacu0,u0tf,nft,0.,dt,nerr)
c       call wsac1(sacw0,w0tf,nft,0.,dt,nerr)
c       call wsac1(sactn,tntf,nft,0.,dt,nerr)
      endif
c
      write(iounit,*) sacu0,sacw0,sactn
       call newhdr
       call setnhv('npts',nft,nerr)
       call setlhv('leven',.true.,nerr)
       call setfhv('b',0.,nerr)
       call setfhv('delta',dt,nerr)
       call setfhv('dist',r,nerr)
       call setfhv('evdp',d,nerr)
       call setfhv('user1',tred,nerr)
       call setfhv('user2',vred,nerr)
       call setfhv('user3',tdly,nerr)
       call wsac0(sacu0,dummy,u0t,nerr)
       call wsac0(sacw0,dummy,w0t,nerr)
       call wsac0(sactn,dummy,tnt,nerr)
       if ( pntfrc .and. (is .ge. 1) .and. (is .le. 3) ) then
	  call wsac0(sacu0f,dummy,u0tf,nerr)
	  call wsac0(sacw0f,dummy,w0tf,nerr)
	  call wsac0(sactnf,dummy,tntf,nerr)
       endif
c
c      call wsac1(sacu0,u0,nft,0.,dt,nerr)
c      call wsac1(sacw0,w0,nft,0.,dt,nerr)
c      call wsac1(sactn,tn,nft,0.,dt,nerr)
c
1     continue
30    continue
c
      close(7)
      if ( pntfrc ) close(8)
c
      stop
      end
c
      subroutine chkopts(nargc,displ,veloc,stepstf,impstf)
      integer nargc,icmd
      logical displ,veloc,stepstf,impstf
      character*70 cmdlin
      do 10  icmd = 1,nargc
	 call getarg( icmd, cmdlin )
	 if ( cmdlin(1:2) .eq. "-v" ) then
	   veloc = .true.
	   displ = .false.
	 else if ( cmdlin(1:2) .eq. "-d" ) then
	   veloc = .false.
	   displ = .true. 
	 else if ( cmdlin(1:2) .eq. "-s" ) then
	   stepstf = .true.
	   impstf = .false.
	 else if ( cmdlin(1:2) .eq. "-i" ) then
	   stepstf = .false.
	   impstf = .true. 
	 else 
	   write(*,*) 'Illegal Command Line Argument',cmdlin
	 endif
10    continue
      return
      end
