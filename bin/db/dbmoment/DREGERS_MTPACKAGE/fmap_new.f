      program fmap
c 
c ...... fps ASCII plot generator
c (lower hemisphere projection)
c 
c ...... Bob Uhrhammer   -   Version 2.0 - 06 March 1997
c 
c Modified Pete Lombard, 25 June, 2005
c 
      implicit real*4 (a-c,e-h,o-z)
      implicit real*8 (d)
      implicit integer*4 (i-n)
c 
      real*4 radius,scale,neval,nplng,nazim,lam1,lam2
      real*4 mxx,mxy,mxz,myy,myz,mzz,m0,mw,eval(3),del1,del2
      character*80 eq_ident,data_used,out_file,depth,datetime
c 
      character*1 d_axis
      integer*4 d_mtrep, pipe, quiet
      real*8 d_mf,d_mt,d_d,d_v,d_miso,d_mdc,d_mclvd,d_m0,d_mw
      real*8 d_plunge,d_azimuth,d_strike,d_dip,d_slip
      real*8 d_pcdc,d_pcclvd,d_a,d_p,d_t,d_pciso
c 
      common /dcf1/ d_axis(3)
      common /dcf2/ d_mf(6),d_mt(3,3),d_d(3),d_v(3,3),d_miso(3,3),
     1     d_mdc(3,3),d_mclvd(3,3),d_m0,d_mw,d_plunge(3),
     2     d_azimuth(3),d_strike(3),d_dip(3),d_slip(3),
     3     d_pcdc,d_pcclvd,d_a(3,3),d_p(3),d_t(3)
      data_used='                                                 '
c 
      call clintrp(eq_ident,ieq,data_used,idu,radius,scale,out_file,
     !     depth,ide,datetime,idate,mxx,mxy,mxz,myy,myz,mzz,pipe,quiet)
c 
      d_mtrep=1
      d_mt(1,1)=dble(scale*mxx)
      d_mt(1,2)=dble(scale*mxy)
      d_mt(1,3)=dble(scale*mxz)
      d_mt(2,1)=dble(scale*mxy)
      d_mt(2,2)=dble(scale*myy)
      d_mt(2,3)=dble(scale*myz)
      d_mt(3,1)=dble(scale*mxz)
      d_mt(3,2)=dble(scale*myz)
      d_mt(3,3)=dble(scale*mzz)
c 
      call m0dcf(d_mtrep,d_mf,d_mt,d_d,d_v,d_miso,d_mdc,d_mclvd,
     1     d_m0,d_mw,d_axis,d_plunge,d_azimuth,d_strike,d_dip,
     2     d_slip,d_pcdc,d_pcclvd,d_pciso)
c 
      m0=sngl(d_m0)
      mw=2.*alog10(m0)/3.-10.73
      mxx=sngl(d_mt(1,1)/d_m0)
      mxy=sngl(d_mt(1,2)/d_m0)
      mxz=sngl(d_mt(1,3)/d_m0)
      myy=sngl(d_mt(2,2)/d_m0)
      myz=sngl(d_mt(2,3)/d_m0)
      mzz=sngl(d_mt(3,3)/d_m0)
      do 1 i=1,3
         if(d_axis(i).eq.'P') then
            pplng=sngl(d_plunge(i))
            pazim=sngl(d_azimuth(i))
         elseif(d_axis(i).eq.'T') then
            tplng=sngl(d_plunge(i))
            tazim=sngl(d_azimuth(i))
         elseif(d_axis(i).eq.'N') then
            nplng=sngl(d_plunge(i))
            nazim=sngl(d_azimuth(i))
         endif
    1 continue
      phi1=sngl(d_strike(1))
      lam1=sngl(d_slip(1))
      del1=sngl(d_dip(1))
      phi2=sngl(d_strike(2))
      lam2=sngl(d_slip(2))
      del2=sngl(d_dip(2))
      eval(1)=sngl(d_d(1))
      eval(2)=sngl(d_d(2))
      eval(3)=sngl(d_d(3))
      if(eval(2).gt.eval(1)) then
         etmp=eval(1)
         eval(1)=eval(2)
         eval(2)=etmp
      endif
      if(eval(3).gt.eval(1)) then
         etmp=eval(1)
         eval(1)=eval(3)
         eval(3)=etmp
      endif
      if(eval(3).gt.eval(2)) then
         etmp=eval(2)
         eval(2)=eval(3)
         eval(3)=etmp
      endif
      teval=eval(1)
      neval=eval(2)
      peval=eval(3)
      pcdc=sngl(d_pcdc)
      pcclvd=sngl(d_pcclvd)
      pciso=sngl(d_pciso)
      if(pipe.lt.1) then
         call fps_gen(eq_ident,ieq,radius,m0,mw,peval,pplng,pazim,
     1        neval,nplng,nazim,teval,tplng,tazim,
     2        phi1,lam1,del1,phi2,lam2,del2,
     3        mxx,mxy,mxz,myy,myz,mzz,
     4        pcdc,pcclvd,pciso,data_used,idu,
     5        out_file,depth,ide,datetime,idate)
      else
         write(*,'(1pe10.1,0p6f10.0)')
     1        m0,phi1,del1,lam1,phi2,del2,lam2
      endif
c 
      stop
      end

      subroutine clintrp(eq_ident,ieq,data_used,idu,radius,scale,
     1     out_file,depth,ide,datetime,idate,mxx,mxy,mxz,myy,myz,mzz,
     2     pipe,quiet)
c 
c ...... command line interpreter
c 
      real*4 radius,scale
      real*4 mxx,mxy,mxz,myy,myz,mzz,m0
      character*80 eq_ident,data_used,out_file,depth,datetime
      integer*4 pipe,quiet
c 
      character*80 ficarg(20)
      integer*4 nfic
c 
      pipe=0
      nfic=iargc()
      if(nfic.gt.22) then
         write(*,*) 'Invalid number of command line parameters'
         stop
      endif
      if(nfic.eq.0) then
         call fp_info
         stop
      endif
      do 1 i=1,nfic
         call getarg(i,ficarg(i))
    1 continue
      radius=1.72
      itype=0
      ifile=0
      ii=0
    2 ii=ii+1
      lfa=lnblnk(ficarg(ii))
      if(ficarg(ii)(1:lfa).eq.'-p') then
         pipe=1
      elseif(ficarg(ii)(1:lfa).eq.'-q') then
         quiet=1
      elseif(ficarg(ii)(1:lfa).eq.'-h') then
         call fp_info
         if(nfic.eq.1) stop
      elseif(ficarg(ii)(1:lfa).eq.'-a') then
         if(itype.eq.0) then
            itype=1
         else
            write(*,*) 'Illegal to input both -a & -m data'
            stop
         endif
         ii=ii+1
         lfa=lnblnk(ficarg(ii))
         read(ficarg(ii)(1:lfa),*,err=10) m0
         ii=ii+1
         lfa=lnblnk(ficarg(ii))
         read(ficarg(ii)(1:lfa),*,err=10) strike
         ii=ii+1
         lfa=lnblnk(ficarg(ii))
         read(ficarg(ii)(1:lfa),*,err=10) rake
         ii=ii+1
         lfa=lnblnk(ficarg(ii))
         read(ficarg(ii)(1:lfa),*,err=10) dip
         if(pipe.lt.1.and.quiet.lt.1) then
            write(*,'(a,1pe10.2,0p3f10.1)') 
     1           '-a: m0, strike, rake & dip = ',
     2           m0,strike,rake,dip
         endif
         call mtcomp(m0,strike,rake,dip,scale,mxx,mxy,mxz,
     1        myy,myz,mzz)
      elseif(ficarg(ii)(1:lfa).eq.'-m') then
         if(itype.eq.0) then
            itype=2
         else
            write(*,*) 'Illegal to input both -a & -m data'
            stop
         endif
         ii=ii+1
         lfa=lnblnk(ficarg(ii))
         read(ficarg(ii)(1:lfa),*,err=10) scale
         ii=ii+1
         lfa=lnblnk(ficarg(ii))
         read(ficarg(ii)(1:lfa),*,err=10) mxx
         ii=ii+1
         lfa=lnblnk(ficarg(ii))
         read(ficarg(ii)(1:lfa),*,err=10) mxy
         ii=ii+1
         lfa=lnblnk(ficarg(ii))
         read(ficarg(ii)(1:lfa),*,err=10) mxz
         ii=ii+1
         lfa=lnblnk(ficarg(ii))
         read(ficarg(ii)(1:lfa),*,err=10) myy
         ii=ii+1
         lfa=lnblnk(ficarg(ii))
         read(ficarg(ii)(1:lfa),*,err=10) myz
         ii=ii+1
         lfa=lnblnk(ficarg(ii))
         read(ficarg(ii)(1:lfa),*,err=10) mzz
         if(pipe.lt.1.and.quiet.lt.1) then
            write(*,'(a,1pe10.2,0p3f10.5)') 
     1           '-m: scale, mxx, mxy & mxz = ',
     2           scale,mxx,mxy,mxz
            write(*,'(a,10x,0p3f10.5)')     
     1           '           myy, myz & mzz = ',
     1           myy,myz,mzz
         endif
      elseif(ficarg(ii)(1:lfa).eq.'-s') then
         ii=ii+1
         lfa=lnblnk(ficarg(ii))
         read(ficarg(ii)(1:lfa),*,err=10) radius
         if(pipe.lt.1.and.quiet.lt.1) then
            write(*,'(a,f10.2)') '-s: radius = ',radius
         endif
         if(radius.lt.0.25) then
            radius=1.0
         elseif(radius.gt.2.5) then
            radius=2.5
         endif
      elseif(ficarg(ii)(1:lfa).eq.'-f') then
         ifile=1
         ii=ii+1
         lfa=lnblnk(ficarg(ii))
         read(ficarg(ii)(1:lfa),'(a)',err=10) out_file
         if(pipe.lt.1.and.quiet.lt.1) write(*,'(2a)') 
     1        '-f: outfile = ',out_file(1:lnblnk(out_file))
      elseif(ficarg(ii)(1:lfa).eq.'-d') then
         ii=ii+1
         lfa=lnblnk(ficarg(ii))
         read(ficarg(ii)(1:lfa),'(a)',err=10) data_used
         idu=1
         if(pipe.lt.1.and.quiet.lt.1) write(*,'(2a)') 
     1       '-d: data_used = ',data_used(1:lnblnk(data_used))
      elseif(ficarg(ii)(1:lfa).eq.'-t') then
         ii=ii+1
         lfa=lnblnk(ficarg(ii))
         read(ficarg(ii)(1:lfa),'(a)',err=10) datetime
         idate=1
         if(pipe.lt.1.and.quiet.lt.1) write(*,'(2a)') 
     1       '-t: datetime = ',datetime(1:lnblnk(datetime))
      elseif(ficarg(ii)(1:lfa).eq.'-z') then
         ii=ii+1
         lfa=lnblnk(ficarg(ii))
         read(ficarg(ii)(1:lfa),'(a)',err=10) depth
         ide=1
         if(pipe.lt.1.and.quiet.lt.1) write(*,'(2a)') 
     1       '-z: depth = ',depth(1:lnblnk(depth))
      elseif(ficarg(ii)(1:lfa).eq.'-e') then
         ii=ii+1
         lfa=lnblnk(ficarg(ii))
         read(ficarg(ii)(1:lfa),'(a)',err=10) eq_ident
         ieq=1
         if(pipe.lt.1.and.quiet.lt.1) write(*,'(2a)') 
     1       '-e: eventid = ',eq_ident(1:lnblnk(eq_ident))
      else
         write(*,*) 'Invalid parameter order'
         do 3 i=1,nfic
            write(*,'(a,i2,2a)') 'ficarg(',i,') = ',
     1           ficarg(i)(1:lnblnk(ficarg(i)))
 3       continue
         stop
      endif
      if(ii.lt.nfic) go to 2
      if(itype.eq.0) then
         write(*,*) 'Invalid input: either -a or -m MUST be specified'
         stop
      endif
      if((ifile.eq.0).and.(pipe.eq.0)) then
         write(*,*) 'Invalid input: -f must be specified'
         stop
      endif
c 
      return
c 
 10   continue
      write(*,*) 'Parameter ficarg(',ii,') = ',ficarg(ii)(1:lfa),
     1     ' is illegal'
      stop
c 
      end

      subroutine fp_info
c 
c ...... print out information about fps_pr
c 
      implicit real*8 (a-h,o-z)
      implicit integer*4 (i-n)
c 
      write(*,'(1h ,a)') 
     1     'fmap - Focal Mechanism ASCII Plot File Program'
c 
      write(*,'(1h ,a)') 'Syntax:'
      write(*,'(1h ,7x,a)') 'fmap [-p] [-q] -a m0 strike rake dip | '
      write(*,'(1h ,7x,a)') '     -m scale mxx mxy mxz myy myz mzz'
      write(*,'(1h ,7x,a)') '     [-h] [-s radius] [-d data_used]'
      write(*,'(1h, 7x,a)') '     [-z depth] [-t datetime]'
      write(*,'(1h ,7x,a)') '     [-e eventID] -f outfile'
c 
      write(*,'(1h ,a)') 'where:'
      write(*,'(1h ,7x,a)') 
     1     '-p     Write only m0, str1, dip1, rak1, str2, dip2, rak2'
      write(*,'(1h ,14x,a)') 
     1     'to standard output (must be first command line argument)'
      write(*,'(1h ,7x,a)') 
     1     '-a     Angle Input - m0, strike, rake & dip'
      write(*,'(1h ,7x,a)') '-m     Moment Tensor Input - scale, mxx, '
      write(*,'(1h ,7x,a)') '              mxy, mxz, myy, myz & mzz'
      write(*,'(1h ,7x,a)') 
     1     '-h     Help - prints this help message'
      write(*,'(1h ,7x,a)') 
     1     '-s     Size Input - radius'
      write(*,'(1h ,7x,a)') 
     1     '-d     Data Used - data_used'
      write(*,'(1h ,7x,a)') 
     1     '-e     Event ID - eventid'
      write(*,'(1h ,7x,a)') 
     1     '-f     Output File Base Name - outfile'
      write(*,'(1h ,7x,a)') 
     1     '-q     Suppress screen output'
      write(*,'(1h ,7x,a)') 
     1     '-t     Event Date and time - datetime'
      write(*,'(1h ,7x,a)') 
     1     '-z     MT Depth - depth'
c 
c xxx1xxxxxxxxx2xxxxxxxxx3xxxxxxxxx4xxxxxxxxx5xxxxxxxxx6xxxxxxxxx7xx
c 
      write(*,'(1h ,a)') 'Notes:'
      write(*,'(1h ,7x,a)') 'The scalar moment (m0) is given in '
      write(*,'(1h ,7x,a)') '       dyne-cm and the strike, rake'
      write(*,'(1h ,7x,a)') '       and dip are given in degrees.'
      write(*,'(1h ,7x,a)') 'The moment input is given in dyne-cm,'
      write(*,'(1h ,7x,a)') '       and the scale factor is a'
      write(*,'(1h ,7x,a)') '       power of ten.'
      write(*,'(1h ,7x,a)') 'The radius of the resulting fps lower'
      write(*,'(1h ,7x,a)') '       hemisphere projection is given'
      write(*,'(1h ,7x,a)') '       in inches (default radius = 1.72).'
      write(*,'(1h ,7x,a)') 'The data information is used to indicate'
      write(*,'(1h ,7x,a)') '       what type of data was inverted to'
      write(*,'(1h ,7x,a)') '       obtain the moment tensor.'
      write(*,'(1h ,7x,a)') 'The datetime, eventID, data information'
      write(*,'(1h ,7x,a)') '       MUST have "_" substituted for " "'
      write(*,'(1h ,7x,a)') '       to be interpreted correctly.'
      write(*,'(1h ,7x,a)') 'Either -a or -m MUST be specified.'
c 
      write(*,'(1h ,a)') 'Examples:'
      write(*,'(1h ,7x,a)')  'fmap -a 8.8e+20 315. -164. 86.'
      write(*,'(1h ,7x,2a)') '     -d BDSN_PKD1_Near-Field ',
     1     '-e 930461711 -f fmap.out'
      write(*,'(1h ,7x,2a)') 'fmap -m 1.e+20 -7.280 0.1474 1.806 ',
     1     '9.565 1.493 -0.6972'
      write(*,'(1h ,7x,a)')  '     -e 930461711 -f fmap.out'
      write(*,'(1h ,7x,2a)') 'fmap -a 8.8e+20 315. -164. 86. -s 2.1',
     1     ' -e 930461711 -f fmap.out'
c 
      write(*,'(1h ,a)') 'Output:'
      write(*,'(1h ,7x,a)') 'rfmap.out.ap'
      write(*,'(1h ,14x,a)') 'ASCII lower hemisphere projection'
      write(*,'(1h ,14x,a)') 'fault plane solution file (suitable'
      write(*,'(1h ,14x,a)') 'for email etc).'
c 
      return
      end

      subroutine fps_gen(eq_ident,ieq,radius,scale,mw,peval,pplng,pazim,
     1     neval,nplng,nazim,teval,tplng,tazim,
     2     phi1,lam1,del1,phi2,lam2,del2,
     3     mxx,mxy,mxz,myy,myz,mzz,
     4     pcdc,pcclvd,pciso,data_used,idu,
     5     out_file,depth,ide,datetime,idate)

      use trigd
c 
c ...... generate printer plot rendition of lower hemisphere 
c equal area projection
c 
c ...... include file 'nf_impl.f'
c 
      implicit real*4(a-h,o-z)
      implicit integer*4 (i-n)
c 
      real*8 d_mt(3,3)
      real*4 radius,scale,mw,neval,nplng,nazim,lam1,lam2
      real*4 mxx,mxy,mxz,myy,myz,mzz,polar
      character*80 eq_ident,ofname,scstr,data_used,out_file,depth
      character*80 datetime
      integer*4 system
c 
c ...... local parameters
c 
      character*1 ach(39,72),aplus,aminus,apaxis,ataxis,ablank
      data aplus,aminus,apaxis,ataxis,ablank /'#','-','P','T',' '/
c 
c ...... output source parameters
c 
      ofname='r'//out_file(1:lnblnk(out_file))//'.ap'
      open(10,file=ofname(1:lnblnk(ofname)),status='unknown')
      write(10,'(1h )')
      write(10,'(1h ,a)')  'Berkeley Moment Tensor Solution'
      write(10,'(1h )')
c 
      dcm0=scale
      write(10,'(1h ,a)') 'Best Fitting Double-Couple:'
      write(10,'(1h ,a,1pe8.2,a)') '   Mo = ',dcm0,' Dyne-cm'
      write(10,'(1h ,a,f4.2)')     '   Mw = ',mw
      if (ide.eq.1) then
         ld=lnblnk(depth)
         if (ld.gt.2) then
            do 299 i = 2,ld-1
               if(depth(i:i).eq.'_') then
                  depth=depth(1:i-1)//' '//depth(i+1:ld)
               endif
 299        continue
         endif
         write(10,'(1h ,2a)')         '   Z  = ',depth(1:ld)
      endif
c
      d_mt(1,1)=dble(scale*mxx)
      d_mt(1,2)=dble(scale*mxy)
      d_mt(1,3)=dble(scale*mxz)
      d_mt(2,1)=dble(scale*mxy)
      d_mt(2,2)=dble(scale*myy)
      d_mt(2,3)=dble(scale*myz)
      d_mt(3,1)=dble(scale*mxz)
      d_mt(3,2)=dble(scale*myz)
      d_mt(3,3)=dble(scale*mzz)
      argmax=abs(sngl(d_mt(1,1)))
      do 22 i=1,3
         do 21 j=1,3
            if (abs(sngl(d_mt(i,j))).gt.argmax)
     $           argmax=abs(sngl(d_mt(i,j)))
 21      continue
 22   continue
      iexpon=0
 23   iexpon=iexpon+1
      arg=10.**(iexpon)
      if(arg.le.argmax) go to 23
      asclf=10.**(iexpon-1)
      write(10,'(1h ,a)')      '   Plane   Strike   Rake   Dip'
      write(10,'(1h ,4x,"NP1",5x,i4,4x,i4,4x,i2)') nint(phi1),
     1     nint(lam1),nint(del1)
      write(10,'(1h ,4x,"NP2",5x,i4,4x,i4,4x,i2)') nint(phi2),
     1     nint(lam2),nint(del2)
      write(10,'(1h )')
      write(10,'(1h ,a)')      'Principal Axes:'
      write(10,'(1h ,a)')      '   Axis    Value   Plunge   Azimuth'
      write(10,'(1h ,5x,"T",3x,f7.3,6x,i2,6x,i3)') teval/asclf,
     1     nint(tplng),nint(tazim)
      write(10,'(1h ,5x,"N",3x,f7.3,6x,i2,6x,i3)') neval/asclf,
     1     nint(nplng),nint(nazim)
      write(10,'(1h ,5x,"P",3x,f7.3,6x,i2,6x,i3)') peval/asclf,
     1     nint(pplng),nint(pazim)
      write(10,'(1h )')
c 
      if(pcdc.lt.99.) then
         write(10,'(1h ,a)') 'Source Composition:'
         write(10,'(1h ,a)') '   Type   Percent'
         write(10,'(1h ,4x,a,3x,f5.1)') 'DC  ',pcdc
         write(10,'(1h ,4x,a,3x,f5.1)') 'CLVD',pcclvd
         write(10,'(1h ,4x,a,3x,f5.1)') 'Iso ',pciso
         write(10,'(1h )')
      endif
c 
      if (idate.eq.1) then
         ld=lnblnk(datetime)
         if(ld.gt.2) then
            do 298 i=2,ld-1
               if(datetime(i:i).eq.'_') then
                  datetime=datetime(1:i-1)//' '//datetime(i+1:ld)
               endif
 298        continue
         endif
         write(10,'(1h ,2a)') 'Event Date/Time: ',
     1        datetime(1:lnblnk(datetime))
      endif
c
      if (ieq.eq.1) then
         ld=lnblnk(eq_ident)
         if(ld.gt.2) then
            do 297 i=2,ld-1
               if(eq_ident(i:i).eq.'_') then
                  eq_ident=eq_ident(1:i-1)//' '//eq_ident(i+1:ld)
               endif
 297        continue
         endif
         write(10,'(1h ,2a)') 'Event ID:        ',
     1        eq_ident(1:lnblnk(eq_ident))
      endif
c 
      if (idu.eq.1) then
         ldu=lnblnk(data_used)
         if(ldu.gt.2) then
            do 29 i=2,ldu-1
               if(data_used(i:i).eq.'_') then
                  data_used=data_used(1:i-1)//' '//data_used(i+1:ldu)
               endif
 29         continue
            write(10,'(1h ,2a)')  'Data Used: ',data_used(1:ldu)
         endif
         write(10,'(1h )')
      endif
c
      write(10,'(1h ,a,i2,a)') 'Moment Tensor: Scale = 10**',
     1     iexpon-1,' Dyne-cm'
      write(10,'(1h ,a)') '   Component   Value'
      write(10,'(1h ,6x,a,5x,f6.3)') 'Mxx',sngl(d_mt(1,1))/asclf
      write(10,'(1h ,6x,a,5x,f6.3)') 'Mxy',sngl(d_mt(1,2))/asclf
      write(10,'(1h ,6x,a,5x,f6.3)') 'Mxz',sngl(d_mt(1,3))/asclf
      write(10,'(1h ,6x,a,5x,f6.3)') 'Myy',sngl(d_mt(2,2))/asclf
      write(10,'(1h ,6x,a,5x,f6.3)') 'Myz',sngl(d_mt(2,3))/asclf
      write(10,'(1h ,6x,a,5x,f6.3)') 'Mzz',sngl(d_mt(3,3))/asclf
      write(10,'(1h )')
c 
c ...... construct lower hemisphere fps 
c 
      r0=radius
      x0=r0+0.250
      y0=r0+0.500
      ix0=12.*x0
      iy0=6.5*y0
      do 3 i=1,2*ix0
         do 2 j=1,2*iy0
            dx=real(i-ix0)/12.
            dy=-real(j-iy0)/6.5
            dd=dx*dx+dy*dy
            if(dd.gt.0.) then
               del=sqrt(dd)
            else
               del=0.
            endif
            if((dx.eq.0.).and.(dy.eq.0.)) then
               theta=0.
            else
               theta=atan2d(dx,dy)
            endif
            if(del.gt.r0) then
               ach(j,i)=ablank
               go to 1
            endif
            if(del.ge.r0) then
               aoi=90.0
            else
               aoi=90.*del/r0
            endif
            if(polar(d_mt,aoi,theta).gt.0.) then
               ach(j,i)=aplus
            else
               ach(j,i)=aminus
            endif
 1          continue
 2       continue
    3 continue
c 
c ...... add P & T axis
c 
      ixp=nint(r0*12.*(90.-pplng)*sind(pazim)/90.+real(ix0))
      iyp=nint(-r0*6.5*(90.-pplng)*cosd(pazim)/90.+real(iy0))
      do 5 i=ixp-1,ixp+1
         do 4 j=iyp-1,iyp+1
            ach(j,i)=ablank
 4       continue
    5 continue
      ach(iyp,ixp)=apaxis
      ixt=nint(r0*12.*(90.-tplng)*sind(tazim)/90.+real(ix0))
      iyt=nint(-r0*6.5*(90.-tplng)*cosd(tazim)/90.+real(iy0))
      do 7 i=ixt-1,ixt+1
         do 6 j=iyt-1,iyt+1
            ach(j,i)=ablank
 6       continue
    7 continue
      ach(iyt,ixt)=ataxis
c 
c ...... add fps plot
c 
      do 8 i=1,2*iy0-2
         write(10,'(1h ,72a1)') (ach(i,j),j=1,2*ix0)
    8 continue
c 
c ...... add projection info
c 
      write(10,'(1h ,a)') '    Lower Hemisphere Equiangle Projection'
      write(10,'(1h )')
c 
      close(10)
c 
c ...... cat file
c 
      if(quiet.lt.1) then
         scstr='cat '//ofname(1:lnblnk(ofname))//' '
         i=system(scstr(1:lnblnk(scstr)))
      endif
c 
      return
      end

      real*4 function polar(dcmt,aoi,theta)
      use trigd
c 
c ...... compute first motion polarity as a function of aoi & theta
c for an arbitrary moment tensor
c 
      implicit real*4 (a-h,o-z)
      implicit integer*4 (i-n)
      real*8 dcmt(3,3)
      real*4 mxx,mxy,mxz,myy,myz,mzz
      data ncall /0/
c 
      if(ncall.lt.1) then
         mxx=sngl(dcmt(1,1))
         mxy=sngl(dcmt(1,2))
         mxz=sngl(dcmt(1,3))
         myy=sngl(dcmt(2,2))
         myz=sngl(dcmt(2,3))
         mzz=sngl(dcmt(3,3))
         smax=0.
         if(abs(mxx).gt.smax) smax=abs(mxx)
         if(abs(mxy).gt.smax) smax=abs(mxy)
         if(abs(mxz).gt.smax) smax=abs(mxz)
         if(abs(myy).gt.smax) smax=abs(myy)
         if(abs(myz).gt.smax) smax=abs(myz)
         if(abs(mzz).gt.smax) smax=abs(mzz)
         mxx=mxx/smax
         mxy=mxy/smax
         mxz=mxz/smax
         myy=myy/smax
         myz=myz/smax
         mzz=mzz/smax
         ncall=1
      endif
c 
      fa = cosd(theta)*cosd(theta)*mxx +
     1     2.*sind(theta)*cosd(theta)*mxy +
     2     sind(theta)*sind(theta)*myy -
     3     mzz
      fb = cosd(theta)*mxz + sind(theta)*myz
c 
      polar = sind(aoi)*sind(aoi)*fa + 
     1     2.*sind(aoi)*cosd(aoi)*fb +
     2     mzz
c 
      return
      end

