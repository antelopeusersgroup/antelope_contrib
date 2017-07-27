      program mtmanip
c
c Manipulate moment sensors: convert between various MT representations
c Based on fmap subroutines by Bob Uhrhammer
c .... Pete Lombard, 20 June 2007
c
      character*1 axis(3)
      integer*4 mtrep,iouta,ioute,ioutk,ioutm,ioutr,ioutt,
     $     ioutp,ioutw,iexp
      real*8 d_mf(6),d_mt_in(3,3),d_mt_out(3,3),d_d(3),d_v(3,3)
      real*8 d_miso(3,3),d_mdc(3,3),d_mclvd(3,3),d_m0,d_mw,scale
      real*8 d_plunge(3),d_azimuth(3),d_strike(2),d_dip(2),d_slip(2)
      real*8 d_pcdc,d_pcclvd,d_pciso
      real*4 mt(6),r_iso(6),r_dc(6),r_clvd(6)
      character*3 label(6)
      character*80 k_conv, a_conv, s_conv

      k_conv = '(Kanamori convention)'
      a_conv = '(Aki convention)'
      s_conv = '(Spherical coordinates)'
c 
c Handle commandline inputs and conversion to tensor representation
      call clintrp(d_mt_in,mtrep,iouta,ioute,ioutk,ioutm,ioutr,ioutt,
     $     ioutp,ioutw)
c
c Compute all other representations from tensor
      call m0dcf(mtrep,d_mf,d_mt_in,d_d,d_v,d_miso,d_mdc,d_mclvd,
     1     d_m0,d_mw,axis,d_plunge,d_azimuth,d_strike,d_dip,
     2     d_slip,d_pcdc,d_pcclvd,d_pciso)
c

c Output the requested representations
      call findscale(d_mt_in,iexp,scale,d_miso, d_mdc, d_mclvd)

      if (iouta .eq. 1) then
         write(6,'(1h ,a)')      '   Plane   Strike   Rake   Dip'
         write(6,'(1h ,4x,"NP1",5x,i4,4x,i4,4x,i2)')
     $        idnint(d_strike(1)),idnint(d_slip(1)),idnint(d_dip(1))
         write(6,'(1h ,4x,"NP2",5x,i4,4x,i4,4x,i2)')
     $        idnint(d_strike(2)),idnint(d_slip(2)),idnint(d_dip(2))
         write(6,'(1h )')
      endif
      if (ioute .eq. 1) then
         do 100 i = 1,3
            if (axis(i) .eq. 'T') then
               ixt = i
            elseif (axis(i) .eq. 'N') then
               ixn = i
            elseif (axis(i) .eq. 'P') then
               ixp = i
            endif
 100     continue
         write(6,'(1h ,a,i2,a)') 'Scale = 1.0e+', iexp,' Dyne-cm'
         write(6,'(1h ,a)')      'Principal Axes:'
         write(6,'(1h ,a)')
     $        '   Axis Value(dev) Value(tot)  Plunge   Azimuth'
         write(6,'(1h ,5x,"T",3x,f7.3,4x,f7.3,6x,i2,6x,i3)')
     $        d_d(ixt)/scale, d_d(ixt) / scale + d_miso(1,1),
     $        idnint(d_plunge(ixt)),idnint(d_azimuth(ixt))
         write(6,'(1h ,5x,"N",3x,f7.3,4x,f7.3,6x,i2,6x,i3)')
     $        d_d(ixn)/scale, d_d(ixn) / scale + d_miso(1,1),
     $        idnint(d_plunge(ixn)),idnint(d_azimuth(ixn))
         write(6,'(1h ,5x,"P",3x,f7.3,4x,f7.3,6x,i2,6x,i3)')
     $        d_d(ixp)/scale, d_d(ixp) /scale + d_miso(1,1),
     $        idnint(d_plunge(ixp)),idnint(d_azimuth(ixp))
         write(6,'(1h )')
      endif   
      if (ioutp .eq. 1) then
         write(6,'(1h ,a)') 'Source Composition:'
         write(6,'(1h ,a)') '   Type   Percent'
         write(6,'(1h ,4x,a,3x,f5.1)') 'DC  ',sngl(d_pcdc)
         write(6,'(1h ,4x,a,3x,f5.1)') 'CLVD',sngl(d_pcclvd)
         write(6,'(1h ,4x,a,3x,f5.1)') 'Iso ',sngl(d_pciso)
         write(6,'(1h )')
      endif
      if (ioutw .eq. 1) then
         write(6,'(1h ,a,f4.2,a,e8.2,a)')
     $        '   Mw = ',sngl(d_mw),'   Mo = ',sngl(d_m0),' Dyne-cm'
         write(6,'(1h )')
      endif

c d_mt_in is in "mtrep" format; convert to "standard" format      
      call mtsrce(mtrep,d_mf,d_mt_in,d_mt_out)

      if (ioutk .eq. 1) then   ! Kanamori convention
         call convent(d_mt_out,mt,d_miso,r_iso,d_mdc,r_dc,d_mclvd,
     $        r_clvd,label,1)
         call prmt(mt,r_iso,r_dc,r_clvd,label,ioutt,iexp,k_conv)
      endif
      if (ioutm .eq. 1) then   ! Aki convention
         call convent(d_mt_out,mt,d_miso,r_iso,d_mdc,r_dc,d_mclvd,
     $        r_clvd,label,2)
         call prmt(mt,r_iso,r_dc,r_clvd,label,ioutt,iexp,a_conv)
      endif
      if (ioutr .eq. 1) then   ! Spherical coordinates
         call convent(d_mt_out,mt,d_miso,r_iso,d_mdc,r_dc,d_mclvd,
     $        r_clvd,label,3)
         call prmt(mt,r_iso,r_dc,r_clvd,label,ioutt,iexp,s_conv)
      endif
c
      stop
      end


      subroutine clintrp(d_mt,mtrep,iouta,ioute,ioutk,ioutm,ioutr,ioutt,
     $     ioutp,ioutw)
c 
c ...... command line interpreter
c 
      real*8 d_mt(3,3), d(3), plunge(3), azimuth(3)
      real*4 scale,mxx,mxy,mxz,myy,myz,mzz,m0
      real*4 vp, vn, vt, pp, pn, pt, ap, an, at
      real*4 mrr, mtt, mpp, mrt, mrp, mtp
      integer*4 mtrep,iouta,ioute,ioutk,ioutm,ioutr,ioutt,
     $     ioutp,ioutw
c 
      character*80 ficarg(20)
      integer*4 nfic
c 
      itype = 0
      nfic=iargc()
      if(nfic.gt.22) then
         write(*,*) 'Invalid number of command line parameters'
         stop
      endif
      if(nfic.eq.0) then
         call mtm_info
         stop
      endif
      do 1 i=1,nfic
         call getarg(i,ficarg(i))
    1 continue
      ii=0
    2 ii=ii+1
      lfa=lnblnk(ficarg(ii))
      if(ficarg(ii)(1:lfa).eq.'-h') then
         call mtm_info
         if(nfic.eq.1) stop
      elseif(ficarg(ii)(1:lfa).eq.'-a') then
         if(itype.eq.0) then
            itype=1
         else
            write(*,*)
     $           'Illegal to input more than one of -a, -e, -k, -m, -r d
     $ata'
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
         call mtcomp(m0,strike,rake,dip,scale,mxx,mxy,mxz,
     1        myy,myz,mzz)
         mtrep=1
         d_mt(1,1)=dble(scale*mxx)
         d_mt(1,2)=dble(scale*mxy)
         d_mt(1,3)=dble(scale*mxz)
         d_mt(2,1)=dble(scale*mxy)
         d_mt(2,2)=dble(scale*myy)
         d_mt(2,3)=dble(scale*myz)
         d_mt(3,1)=dble(scale*mxz)
         d_mt(3,2)=dble(scale*myz)
         d_mt(3,3)=dble(scale*mzz)
      elseif(ficarg(ii)(1:lfa).eq.'-e') then
         if(itype.eq.0) then
            itype=2
         else
            write(*,*)
     $           'Illegal to input more than one of -a, -e, -k, -m, -r d
     $ata'
            stop
         endif
         ii=ii+1
         lfa=lnblnk(ficarg(ii))
         read(ficarg(ii)(1:lfa),*,err=10) vp
         ii=ii+1
         lfa=lnblnk(ficarg(ii))
         read(ficarg(ii)(1:lfa),*,err=10) vn
         ii=ii+1
         lfa=lnblnk(ficarg(ii))
         read(ficarg(ii)(1:lfa),*,err=10) vt
         ii=ii+1
         lfa=lnblnk(ficarg(ii))
         read(ficarg(ii)(1:lfa),*,err=10) pp
         ii=ii+1
         lfa=lnblnk(ficarg(ii))
         read(ficarg(ii)(1:lfa),*,err=10) pn
         ii=ii+1
         lfa=lnblnk(ficarg(ii))
         read(ficarg(ii)(1:lfa),*,err=10) pt
         ii=ii+1
         lfa=lnblnk(ficarg(ii))
         read(ficarg(ii)(1:lfa),*,err=10) ap
         ii=ii+1
         lfa=lnblnk(ficarg(ii))
         read(ficarg(ii)(1:lfa),*,err=10) an
         ii=ii+1
         lfa=lnblnk(ficarg(ii))
         read(ficarg(ii)(1:lfa),*,err=10) at

         mtrep=1
         d(1)=dble(vp)
         d(2)=dble(vn)
         d(3)=dble(vt)
         plunge(1)=dble(pp)
         plunge(2)=dble(pn)
         plunge(3)=dble(pt)
         azimuth(1)=dble(ap)
         azimuth(2)=dble(an)
         azimuth(3)=dble(at)
         call epa2mt(d,plunge,azimuth,d_mt)

      elseif(ficarg(ii)(1:lfa).eq.'-m') then
         if(itype.eq.0) then
            itype=2
         else
            write(*,*)
     $           'Illegal to input more than one of -a, -e, -k, -m, -r d
     $ata'
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

         mtrep=1
         d_mt(1,1)=dble(scale*mxx)
         d_mt(1,2)=dble(scale*mxy)
         d_mt(1,3)=dble(scale*mxz)
         d_mt(2,1)=dble(scale*mxy)
         d_mt(2,2)=dble(scale*myy)
         d_mt(2,3)=dble(scale*myz)
         d_mt(3,1)=dble(scale*mxz)
         d_mt(3,2)=dble(scale*myz)
         d_mt(3,3)=dble(scale*mzz)
      elseif(ficarg(ii)(1:lfa).eq.'-k') then
         if(itype.eq.0) then
            itype=2
         else
            write(*,*)
     $           'Illegal to input more than one of -a, -e, -k, -m, -r d
     $ata'
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

         mtrep=1
         d_mt(1,1)= -dble(scale*mxx)
         d_mt(1,2)= -dble(scale*mxy)
         d_mt(1,3)= -dble(scale*mxz)
         d_mt(2,1)= -dble(scale*mxy)
         d_mt(2,2)= -dble(scale*myy)
         d_mt(2,3)= -dble(scale*myz)
         d_mt(3,1)= -dble(scale*mxz)
         d_mt(3,2)= -dble(scale*myz)
         d_mt(3,3)= -dble(scale*mzz)
      elseif(ficarg(ii)(1:lfa).eq.'-r') then
         if(itype.eq.0) then
            itype=2
         else
            write(*,*)
     $           'Illegal to input more than one of -a, -e, -k, -m, -r d
     $ata'
            stop
         endif
         ii=ii+1
         lfa=lnblnk(ficarg(ii))
         read(ficarg(ii)(1:lfa),*,err=10) scale
         ii=ii+1
         lfa=lnblnk(ficarg(ii))
         read(ficarg(ii)(1:lfa),*,err=10) mrr
         ii=ii+1
         lfa=lnblnk(ficarg(ii))
         read(ficarg(ii)(1:lfa),*,err=10) mrt
         ii=ii+1
         lfa=lnblnk(ficarg(ii))
         read(ficarg(ii)(1:lfa),*,err=10) mrp
         ii=ii+1
         lfa=lnblnk(ficarg(ii))
         read(ficarg(ii)(1:lfa),*,err=10) mtt
         ii=ii+1
         lfa=lnblnk(ficarg(ii))
         read(ficarg(ii)(1:lfa),*,err=10) mtp
         ii=ii+1
         lfa=lnblnk(ficarg(ii))
         read(ficarg(ii)(1:lfa),*,err=10) mpp

         mtrep=0
         d_mt(1,1)=dble(scale*mrr)
         d_mt(1,2)=dble(scale*mrt)
         d_mt(1,3)=dble(scale*mrp)
         d_mt(2,1)=dble(scale*mrt)
         d_mt(2,2)=dble(scale*mtt)
         d_mt(2,3)=dble(scale*mtp)
         d_mt(3,1)=dble(scale*mrp)
         d_mt(3,2)=dble(scale*mtp)
         d_mt(3,3)=dble(scale*mpp)
      elseif(ficarg(ii)(1:lfa).eq.'-A') then
         iouta = 1
      elseif(ficarg(ii)(1:lfa).eq.'-E') then
         ioute = 1
      elseif(ficarg(ii)(1:lfa).eq.'-K') then
         ioutk = 1
      elseif(ficarg(ii)(1:lfa).eq.'-M') then
         ioutm = 1
      elseif(ficarg(ii)(1:lfa).eq.'-P') then
         ioutp = 1
      elseif(ficarg(ii)(1:lfa).eq.'-R') then
         ioutr = 1
      elseif(ficarg(ii)(1:lfa).eq.'-T') then
         ioutt = 1
      elseif(ficarg(ii)(1:lfa).eq.'-W') then
         ioutw = 1
      elseif(ficarg(ii)(1:lfa).eq.'-Z') then
         iouta = 1
         ioute = 1
         ioutk = 1
         ioutm = 1
         ioutp = 1
         ioutr = 1
         ioutt = 1
         ioutw = 1
      else
         write(*,*) 'Invalid parameter '
         do 3 i=1,nfic
            write(*,'(a,i2,2a)') 'ficarg(',i,') = ',
     1           ficarg(i)(1:lnblnk(ficarg(i)))
 3       continue
         stop
      endif
      if(ii.lt.nfic) go to 2

      if(itype.eq.0) then
         write(*,*)
     $        'Invalid input: one of -a, -e, -k, -m, -r is REQUIRED'
         call mtm_info
         stop
      endif
c 
      return
c 
 10   continue
      write(*,*) 'Parameter ficarg(',ii,') = ',ficarg(ii)(1:lfa),
     1     ' is illegal'
      call mtm_info
      stop
c 
      end

      subroutine mtm_info
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
      write(*,'(1h ,7x,a)') 'mtmanip -a m0 strike rake dip |'
      write(*,'(1h ,7x,a)')
     $     '     -e P N T plngP plngN plngT azP azN azT | '
      write(*,'(1h ,7x,a)') '     -k scale mxx mxy mxz myy myz mzz |'
      write(*,'(1h ,7x,a)') '     -m scale mxx mxy mxz myy myz mzz |'
      write(*,'(1h ,7x,a)') '     -r scale mrr mrt mrp mtt mtp mpp '
      write(*,'(1h ,7x,a)')
     $     '   [-A] [-E] [-K | -M | -R [-T]] [-P] [-W] [-Z]'
      write(*,'(1h ,7x,a)') '   [-h]'
c 
      write(*,'(1h ,7x,a)') 'where input is one of:'
      write(*,'(1h ,7x,a)')
     1     '-a   Angle Input - m0, strike, rake & dip'
      write(*,'(1h ,7x,a)')
     1     '-e   Eigenvalue Input - P value, N value, T value'
      write(*,'(1h ,7x,a)')
     1     '                       P plunge N plunge T plunge'
      write(*,'(1h ,7x,a)')
     1     '                       P azimuth N azimuth T azimuth'
      write(*,'(1h ,7x,a)')
     $     '-k   MT Input (Kanamori convention) - scale, mxx, '
      write(*,'(1h ,7x,a)') '              mxy, mxz, myy, myz & mzz'
      write(*,'(1h ,7x,a)')
     $     '-m   MT Input (Aki convention) - scale, mxx, '
      write(*,'(1h ,7x,a)') '              mxy, mxz, myy, myz & mzz'
      write(*,'(1h ,7x,a)')
     $     '-r   MT Input (Spherical coordinates) - scale, mrr, '
      write(*,'(1h ,7x,a)') '              mrt, mrp, mtt, mtp & mpp'
      write(*,'(1h ,7x,a)')
     $     '   where r -> radial, t -> theta, p -> phi components'
      write(*,'(1h ,7x,a)') 
      write(*,'(1h ,7x,a)') 'and output is one or more of:'
      write(*,'(1h ,7x,a)')
     $     '-A   Angle Output - m0, strike1, rake1, dip1, strike2, rake2
     $     , dip2' 
      write(*,'(1h ,7x,a)')'-E   Eigenvalue, Azimuth and Plunge output'
      write(*,'(1h ,7x,a)')
     $     '-K   MT Output (Kanamori convention) - m0, mxx, '
      write(*,'(1h ,7x,a)') '              mxy, mxz, myy, myz & mzz'
      write(*,'(1h ,7x,a)')
     $     '-M   MT Output (Aki convention) - m0, mxx, '
      write(*,'(1h ,7x,a)') '              mxy, mxz, myy, myz & mzz'
      write(*,'(1h ,7x,a)')
     $     '-R   MT Output (Spherical coordinates) - m0, mrr, '
      write(*,'(1h ,7x,a)') '              mrt, mrp, mtt, mtp & mpp'
      write(*,'(1h ,7x,a)')
     $     '-T  split any of tensor outputs into double couple, CLVD'
      write(*,'(1h ,7x,a)') '    and isotropic tensors'
      write(*,'(1h ,7x,a)')
     $     '-P  percent double couple, CLVD and isotropic'
      write(*,'(1h ,7x,a)') '-W  Mw output'
      write(*,'(1h ,7x,a)') '-Z  the whole enchilada (all outputs)'
      write(*,'(1h ,7x,a)')
     1     '-h     Help - prints this help message'
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
c 
      return
      end




      subroutine epa2mt(d,plunge,azimuth,mt)
c
c Given three eigenvalues, plunges and azimuths, compute moment tensor
c
      real*8 d(3),plunge(3),azimuth(3),mt(3,3)
c
      integer*4 n,i,j
      real*8 v(3,3),work(3,3),diag(3,3)
      n = 3

      call zalp(plunge, azimuth, v)
      do 20 i = 1,3
         do 10 j = 1,3
            if (i .eq. j) then
               diag(i,j) = d(i)
            else
               diag(i,j) = 0.0
            endif
 10      continue
 20   continue

      call transpose(v,n)
      call mmulquad(v,diag,mt,work,n,n)

      return
      end

      subroutine zalp(plunge,azimuth,v)
      use trigd
c Compute orthonormal eigenvectors from azimuth and plunge
c Fatal error if vector described by azimuth and plunge are not orthogonal
      real*8 plunge(3), azimuth(3), v(3,3),sum(3), tol
      integer*4 i

      data tol/1.0d-4/
      
      do 10 i = 1, 3
         sum(i) = 0.0d0
         v(3,i) = dsind(plunge(i))
         v(2,i) = dcosd(plunge(i)) * dsind(azimuth(i))
         v(1,i) = dcosd(plunge(i)) * dcosd(azimuth(i))
 10   continue
      do 20 i = 1,3
         sum(1) = sum(1) + v(i,1) * v(i,2)
         sum(2) = sum(2) + v(i,2) * v(i,3)
         sum(3) = sum(3) + v(i,3) * v(i,1)
 20   continue
      do 30 i = 1,3
         if (dabs(sum(i)) .gt. tol) then
            write(*,*) 'Plunge/azimuth vectors are not orthogonal'
            stop
         endif
 30   continue
c
      return
      end



C******************************************************************
C
C    SUBROUTINE:  MMULQUAD
C
C    SYNOPSIS:  Computes the matrix product C = A'*B*A where
C        full storage is used for the symmetric matrix C.   
C
C    PARAMETERS:
C      A    MxN double precision matrix
C      B    MxM double precision matrix
C      C    NxN double precision matrix
C      WORK  NxM double precision matrix
C
C******************************************************************


      subroutine mmulquad(a,b,c,work,m,n)
      integer*4  m,n
      real*8  a(m,n),b(m,m),c(n,n),work(n,m)
c
c
c  do the multiplications
      call mmul2 (a,b,work,n,m,m)
      call mmulmmsf (work,a, c, n,m)
c  
c  finish
      return
      end



C******************************************************************
C
C   SUBROUTINE: MMUL2
C
C   SYNOPSIS: Computes the matrix product C = A'*B
C
C   PARAMETERS:
C     A   N*M double precision matrix
C     B   N*P double precision matrix
C     C   MxP double precision matrix
C     M   column dimension of A and row dimension of C
C     N   row dimension of A and row dimension of B
C     P   column dimension of B and coulmn dimesnion of C
C
C*******************************************************************
 
 
      subroutine mmul2(a,b,c,m,n,p)
      integer*4  m,n,p
      real*8  a(n,m),b(n,p),c(m,p)
c
C   Local Declarations
      integer*4 i,j,k
      real*8 temp
C
C  The main loop
      do 30 i=1,m
        do 20 j=1,p
          temp =0
          do 10 k=1,n
            temp = temp+a(k,i)*b(k,j)
   10     continue
          c(i,j) = temp
   20   continue
   30 continue
C
C finish
      return
      end


C******************************************************************
C
C    SUBROUTINE:  MMULMMSF
C
C    SYNOPSIS: Computes the matrix product C = AxB , when A is
C        assumed to be symmetric. C is the only parameter that is
C        changed upon exit.
C
C    PARAMETERS:
C      A    MxN double precision matrix
C      B    NxM double precision matrix
C      C    MxM double precision matrix
C      M    Row dimension of A and C.  Column dimension of B and C.
C      N    Column dimension of A.  Row dimension of B.
C
C******************************************************************


      subroutine mmulmmsf(a,b,c,m,n)
      integer*4  m,n
      real*8  a(m,n),b(n,m),c(m,m)
c
C   Local Declarations
      integer*4 i,j,k
      real*8  temp
c
C  The main loop
      do 30 i=1,m
        do 20 j=i,m
          temp =0
          do 10 k=1,n
            temp = temp+a(i,k)*b(k,j)
   10     continue
          c(i,j) = temp
          c(j,i) = temp
   20   continue
   30 continue
c
C finish
      return
      end


      subroutine transpose(v,n)
      integer*4 n, i, j
      real*8 v(n,n), tmp

      if (n .lt. 2) return

      do 20 i = 1, n
         do 10 j = i+1, n
            tmp = v(i,j)
            v(i,j) = v(j,i)
            v(j,i) = tmp
 10      continue
 20   continue
      return
      end

      subroutine findscale(d_mt,iexp,scale,d_miso, d_mdc, d_mclvd)
c Compute a scaling factor that is a power of ten
c Apply the scaling to d_mt
      real*8 d_mt(3,3),d_miso(3,3), d_mdc(3,3), d_mclvd(3,3)
      real*8 scale, argmax
      integer*4 iexp, i, j

      argmax=dabs(d_mt(1,1))
      do 22 i=1,3
         do 21 j=1,3
            if (dabs(d_mt(i,j)) .gt. argmax)
     $           argmax = abs(d_mt(i,j))
 21      continue
 22   continue
      
      iexp=0
 23   iexp=iexp+1
      scale = 10.0d0 ** iexp
      if (scale .le. argmax) goto 23
      iexp=iexp-1
      scale = 10.0d0 ** iexp

      do 25 i = 1,3
         do 24 j = 1,3
            d_mt(i,j) = d_mt(i,j) / scale
            d_miso(i,j) = d_miso(i,j) / scale
            d_mdc(i,j) = d_mdc(i,j) / scale
            d_mclvd(i,j) = d_mclvd(i,j) / scale
 24      continue
 25   continue
      return
      end


      subroutine convent(d_mt,mt,d_miso,r_iso,d_mdc,r_dc,d_mclvd,r_clvd,
     $        label,iconv)
      real*8 d_mt(3,3),d_miso(3,3),d_mdc(3,3),d_mclvd(3,3)
      real*4 mt(6),r_iso(6),r_dc(6),r_clvd(6)
      character*3 label(6)

      if (iconv .eq. 1) then     ! Kanamori convention
         label(1)  = 'mxx'
         mt(1)     = -d_mt(1,1)
         r_iso(1)  = -d_miso(1,1)
         r_dc(1)   = -d_mdc(1,1)
         r_clvd(1) = -d_mclvd(1,1)

         label(2)  = 'mxy'
         mt(2)     = -d_mt(1,2)
         r_iso(2)  = -d_miso(1,2)
         r_dc(2)   = -d_mdc(1,2)
         r_clvd(2) = -d_mclvd(1,2)

         label(3)  = 'mxz'
         mt(3)     = -d_mt(1,3)
         r_iso(3)  = -d_miso(1,3)
         r_dc(3)   = -d_mdc(1,3)
         r_clvd(3) = -d_mclvd(1,3)

         label(4)  = 'myy'
         mt(4)     = -d_mt(2,2)
         r_iso(4)  = -d_miso(2,2)
         r_dc(4)   = -d_mdc(2,2)
         r_clvd(4) = -d_mclvd(2,2)

         label(5)  = 'myz'
         mt(5)     = -d_mt(2,3)
         r_iso(5)  = -d_miso(2,3)
         r_dc(5)   = -d_mdc(2,3)
         r_clvd(5) = -d_mclvd(2,3)

         label(6)  = 'mzz'
         mt(6)     = -d_mt(3,3)
         r_iso(6)  = -d_miso(3,3)
         r_dc(6)   = -d_mdc(3,3)
         r_clvd(6) = -d_mclvd(3,3)
      elseif (iconv .eq. 2) then ! Aki convention
         label(1)  = 'mxx'
         mt(1)     = d_mt(1,1)
         r_iso(1)  = d_miso(1,1)
         r_dc(1)   = d_mdc(1,1)
         r_clvd(1) = d_mclvd(1,1)
         label(2)  = 'mxy'
         mt(2)     = d_mt(1,2)
         r_iso(2)  = d_miso(1,2)
         r_dc(2)   = d_mdc(1,2)
         r_clvd(2) = d_mclvd(1,2)
         label(3)  = 'mxz'
         mt(3)     = d_mt(1,3)
         r_iso(3)  = d_miso(1,3)
         r_dc(3)   = d_mdc(1,3)
         r_clvd(3) = d_mclvd(1,3)
         label(4)  = 'myy'
         mt(4)     = d_mt(2,2)
         r_iso(4)  = d_miso(2,2)
         r_dc(4)   = d_mdc(2,2)
         r_clvd(4) = d_mclvd(2,2)
         label(5)  = 'myz'
         mt(5)     = d_mt(2,3)
         r_iso(5)  = d_miso(2,3)
         r_dc(5)   = d_mdc(2,3)
         r_clvd(5) = d_mclvd(2,3)
         label(6)  = 'mzz'
         mt(6)     = d_mt(3,3)
         r_iso(6)  = d_miso(3,3)
         r_dc(6)   = d_mdc(3,3)
         r_clvd(6) = d_mclvd(3,3)
      elseif (iconv .eq. 3) then ! Spherical coordinates
         label(1)  = 'mrr'
         mt(1)     = d_mt(3,3)
         r_iso(1)  = d_miso(3,3)
         r_dc(1)   = d_mdc(3,3)
         r_clvd(1) = d_mclvd(3,3)
         label(2)  = 'mrt'
         mt(2)     = d_mt(1,3)
         r_iso(2)  = d_miso(1,3)
         r_dc(2)   = d_mdc(1,3)
         r_clvd(2) = d_mclvd(1,3)
         label(3)  = 'mrp'
         mt(3)     = -d_mt(2,3)
         r_iso(3)  = -d_miso(2,3)
         r_dc(3)   = -d_mdc(2,3)
         r_clvd(3) = -d_mclvd(2,3)
         label(4)  = 'mtt'
         mt(4)     = d_mt(1,1)
         r_iso(4)  = d_miso(1,1)
         r_dc(4)   = d_mdc(1,1)
         r_clvd(4) = d_mclvd(1,1)
         label(5)  = 'mtp'
         mt(5)     = -d_mt(1,2)
         r_iso(5)  = -d_miso(1,2)
         r_dc(5)   = -d_mdc(1,2)
         r_clvd(5) = -d_mclvd(1,2)
         label(6)  = 'mpp'
         mt(6)     = d_mt(2,2)
         r_iso(6)  = d_miso(2,2)
         r_dc(6)   = d_mdc(2,2)
         r_clvd(6) = d_mclvd(2,2)
      endif
      return
      end

      subroutine prmt(mt,r_iso,r_dc,r_clvd,label,ioutt,iexp,conv)
      real*4 mt(6),r_iso(6),r_dc(6),r_clvd(6)
      character*3 label(6)
      character*(*) conv
      
      write(6,'(1h ,a,i2,a,a)') 'Moment Tensor: Scale = 10**',
     1     iexp,' Dyne-cm ', conv
      if (ioutt .eq. 1) then
         write(6,'(1h ,a)')
     $        '   Component   iso      dc      clvd    total'
      else
         write(6,'(1h ,a)') '   Component   Value'
      endif
      do 100 i = 1,6
         if (ioutt .eq. 1) then
            write(6,'(1h ,6x,a,4(2x,f7.3))') label(i),r_iso(i),
     $           r_dc(i),r_clvd(i),mt(i)
         else
            write(6,'(1h ,6x,a,5x,f7.3)') label(i),mt(i)
         endif
 100  continue
      write(6,'(1h )')


      return
      end
