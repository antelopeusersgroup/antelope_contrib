c====================================================================================
c
c  Synthetic Seismogram code for complete synthetics using spectral technique 
c    for Moment Tensor and Point Force sources in flat layered media
c
c    Funded by Treaty Verification Program, Lawrence Livermore National Lab
c
c    George Randall, Dept. of Geological Sciences, University of South Carolina
c           ger@tigger.seis.scarolina.edu
c    Steve Taylor, Los Alamos National Lab (formerly at Lawrence Livermore National Lab)
c           taylor@beta.lanl.gov
c
c        based on 
c       "Seismic Wave Propagation in Stratified Media" B.L.N. Kennett, 
c        Cambridge University Press, 1983
c
c    Many useful discussions with and testing by
c    Chuck Ammon, Harley Benz, and Bill Walter
c    are gratefully acknowledged.
c
c
c====================================================================================
c
c  Version 1.0   February  1  1993
c
c     kennett.f generates regional synthetics following kennett (1983)
c     the units for the kennett synthetic with the specified input
c     units below are:
c     u(cm) = Gij(t)*Fj(t)10-15 + Gij,k*Mjk(t)10-20
c     for F in dynes, M in dyne-cm, * - convolution
c     so a force of 10**15 dynes and a moment of 10**20 dyne-cm
c     are both weighting the respective Greens function or derivative
c     of the Greens function by terms of order 1
c     kennett synthetic will provide displacement in cm with a factor
c     of 10**-20, so Mo of 5*10**20 dyne * cm will be computed
c     by taking 5 times the raw kennett response (for a delta function
c     Mo(t) response) so the 10**-20 scales down the moment figure.
c     (or output in velocity for a step function Mo(t) response)
c     for F(t) in dyne ( or F(w) in dyne * sec )
c     kennett synthetic will provide displacement in cm with a factor
c     of 10**-15
c     comments in main and subroutines correspond to Kennett (1983)
c     options to compute 7 generic sources at one range, or 1 source
c     at a number of ranges
c     source 7 is for general symmetric moment tensor
c
c     FF >= Max number of Frequencies
c     RR >= Max number of Ranges
c     MM >= Max number of Moment Tensor Sources
c     PP >= Max number of Point Forces
c     DD >= Max number of source depths or Max number of Differentials
c           Differentials Seismograms are handled as depths in arrays
c
c     sometime change ABM input so it is fl,fh and convert to tau
c
#ifdef DIFFSEIS
      include 'kennet.diff'
#else
      include 'kennet.syn'
#endif
#ifdef PARALLELP4
      include 'p4f.h'
      integer P4BFSZ,ANYNODE,ALLNODES,MASTERNODE,INTSIZ,FLTSIZ
      integer SCALARS,RANGES,DEPTHS,MIJS,ALFASS,BETASS,RHOSS,THIKS,DEPS
      integer QPS,QSS,TP1S,TP2S,TS1S,TS2S,SDEPS,LDEPS,PERTS
      integer U0TM,W0TM,TNTM,U0TF,W0TF,TNTF
      parameter ( P4BFSZ=mxlr )
      parameter ( ANYNODE=-1, ALLNODES=-1, MASTERNODE=0 )
      parameter ( INTSIZ=4 , FLTSIZ=4 )
      parameter ( SCALARS=20, RANGES=21, DEPTHS=22, MIJS=23 )
      parameter ( ALFASS=24, BETASS=25, RHOSS=26, THIKS=27, DEPS=28 )
      parameter ( QPS=29, QSS=30, TP1S=31, TP2S=32, TS1S=33, TS2S=34 )
      parameter ( SDEPS=35, LDEPS=36, PERTS=37 )
      integer mynod, nodes, p4mtyp, p4nod, retcod, rcvlen, nelem
      integer lnppts, lpnnppts, lpnpmin, lpnpmax, lnpmin, lnpmax
      real epfac
      integer ip4buf(P4BFSZ)
      real rp4buf(P4BFSZ)
      equivalence ( ip4buf, rp4buf )
#endif
      integer RR,FF,MM,PP,LL,DD
c     parameter ( FF=2049, RR=50, MM=mxsrc, PP=fsrc, LL=mxlr, DD=mxdep )
      parameter ( FF=2049 )
      parameter ( RRR=150 )
      parameter ( MM=mxsrc, PP=fsrc, LL=mxlr, DD=mxdep )
      real al0(LL),be0(LL),qpm(LL),qsm(LL),dens(LL),th(LL),deptop(LL)
      real*8 tp1(LL),tp2(LL),ts1(LL),ts2(LL)
      real*8 qabm, vabm,  qa, qb, wq
#ifndef MULTIDEP
#define ID
#ifdef MULTISRC
      parameter ( RR=RRR )
      complex pup(MM),pdn(MM),svup(MM),svdn(MM),shup(MM),shdn(MM)
      complex u0(FF,MM), w0(FF,MM), tn(FF,MM)
      complex u0t(FF,MM,RR), w0t(FF,MM,RR), tnt(FF,MM,RR)
#ifdef PNTFRC
      complex fpup(PP),fpdn(PP),fsvup(PP),fsvdn(PP),fshup(PP),fshdn(PP)
      complex u0f(FF,PP), w0f(FF,PP), tnf(FF,PP)
      complex u0ft(FF,PP,RR), w0ft(FF,PP,RR), tnft(FF,PP,RR)
#endif
#endif
#ifdef SNGLSRC
      parameter ( RR=RRR*MM )
      complex pupg0,pdng0,svupg0,svdng0,shupg0,shdng0
      complex pupg1,pdng1,svupg1,svdng1,shupg1,shdng1
      complex u0fg0(FF), w0fg0(FF), tnfg0(FF)
      complex u0fg1(FF), w0fg1(FF), tnfg1(FF)
      complex u0t(FF,RR), w0t(FF,RR), tnt(FF,RR)
#endif
      complex dvpupf(FF),dvsupf(FF),drpupf(FF),drsupf(FF)
      complex dvpdnf(FF),dvsdnf(FF),drpdnf(FF),drsdnf(FF)
      complex dtsupf(FF),dtsdnf(FF)
#endif
#ifdef MULTIDEP
#define ID ,id
#ifdef MULTISRC
      parameter ( RR=RRR/DD )
      complex pup(MM),pdn(MM),svup(MM),svdn(MM),shup(MM),shdn(MM)
      complex u0(FF,MM,DD), w0(FF,MM,DD), tn(FF,MM,DD)
      complex u0t(FF,MM,DD,RR), w0t(FF,MM,DD,RR), tnt(FF,MM,DD,RR)
#ifdef PNTFRC
      complex fpup(PP),fpdn(PP),fsvup(PP),fsvdn(PP),fshup(PP),fshdn(PP)
      complex u0f(FF,PP,DD), w0f(FF,PP,DD), tnf(FF,PP,DD)
      complex u0ft(FF,PP,DD,RR), w0ft(FF,PP,DD,RR), tnft(FF,PP,DD,RR)
#endif
#endif
#ifdef SNGLSRC
      parameter ( RR=RRR*MM/DD )


      complex pupg0,pdng0,svupg0,svdng0,shupg0,shdng0
      complex pupg1,pdng1,svupg1,svdng1,shupg1,shdng1
      complex u0fg0(FF,DD), w0fg0(FF,DD), tnfg0(FF,DD)
      complex u0fg1(FF,DD), w0fg1(FF,DD), tnfg1(FF,DD)
      complex u0t(FF,DD,RR), w0t(FF,DD,RR), tnt(FF,DD,RR)
#endif
#ifdef DIFFSEIS
      complex dpupg0,dpdng0,dsvupg0,dsvdng0,dshupg0,dshdng0
      complex dpupg1,dpdng1,dsvupg1,dsvdng1,dshupg1,dshdng1
      real perta,pertb,pertr,diffnorm(DD)
      integer perturb
#endif
      complex dvpupf(FF,DD),dvsupf(FF,DD),drpupf(FF,DD),drsupf(FF,DD)
      complex dvpdnf(FF,DD),dvsdnf(FF,DD),drpdnf(FF,DD),drsdnf(FF,DD)
      complex dtsupf(FF,DD),dtsdnf(FF,DD)
#endif
      complex hnklf0(FF), hnklf1(FF)
      real  karg(FF),kargh(FF),rh(RR),depth(DD),ldepth(DD),distmx,twopi
      complex u0ts,w0ts,tnts
      complex expiaz, expmiaz, expi2az, expmi2az
      real pwr,pwl,gfilon,delp
      complex freq, pc, c1, cd, zero
      character*40 infil
      integer inunit,iounit,maxarr,nlyrs,l,ifrq,if,j,jj,ir,i,ip,k
      integer nft,nffpts,ifmax,ifmin,nppts,npmax,npmin,nr
      integer nfpts,isrcl,irlyr,nsrc,nd,id
      real pi,pmin,plc,puc,pmax,pfac,fmin,fmax,fny,r,azif,azis,rmin,dr,r1
      real pc1,pc2,pr1,pr2,dt,delf,recdep,lrecdep,azi,sinazi,cosazi
      real sin2az,cos2az,a2,b2,rmu,rlamb,cosaz2,sinaz2,p,t
      intrinsic exp,sin,cos
      inunit=5
      iounit=6
c
c     input format is as follows:
c     number of layers (including 0 km thik half space)
c       vel model ....
c     thik (km),vp,vs(km/s),density(g/cm3),Qp,Qs,taup,s for ABM
c        taup,s are in units of 2*pi*tau (so for 1 Hz tau = 0.159)
c     p1,p2,p3,p4,pfac (s/km) where
c       p1 - min slowness,p4 max, p2 to p3 range between taper
c       i.e. taper between p1 and p2 and p3 to p4
c       pfac is dimensionless, > 0, controls delta p sampling
c       pfac < 1, coarser than normal delta p
c       pfac = 1, normal delta p
c       pfac > 1, finer than normal delta p
c     f0,fmax,fnyq,npts
c     dist, az
c
c       if dist=0, next line has
c     mindist, delta dist, no. distances
c       if dist < 0. next 2 lines are
c        no. distances (nr)
c        r1,r2,...,rn
c     source layer, depth in source layer, Zs (km)
c     receiver layer, sta depth in receiver layer, Zr (km)
c       Also, Zs .ge. Zr
c
c
C
C=====================================================
C Add p4 library Initialization Here
C How Many processors total
C Which processor is this
C Is it Master or Slave
C Does Master do Part of the slowness integral or not
C call routines  p4init, p4myid, p4ntotids, p4nslaves ...
#ifdef PARALLELP4
      call p4init()
      call p4crpg()
      mynod = p4myid()
      nodes = p4ntotids()
#endif
C=====================================================
C
C Begin Master Code to read in input model parameters
C=====================================================
C
      twopi = 8.*atan(1.)
      pi = 0.5*twopi
      zero = (0.,0.)
c
c     define max number of freq points, maxarr
c
      maxarr=FF
c
c     general announcement
c
#ifdef PARALLELP4
      if ( mynod .eq. 0 ) then
#endif
      write(iounit,*) ' Synthetic Seismogram code for complete synthetics using spectral technique '
#ifndef PNTFRC
      write(iounit,*) '   for Moment Tensor sources in flat layered media '
#endif
#ifdef PNTFRC
      write(iounit,*) '   for Moment Tensor and Point Force sources in flat layered media '
#endif
#ifdef DIFFSEIS
      write(iounit,*) ' Incorporating Efficient Differential Seismogram Technique '
      write(iounit,*) ' for sensitivity to the earth model velocities '
#endif
      write(iounit,*) ' George Randall, Uinversity of South Carolina '
      write(iounit,*) ' ger@tigger.seis.scarolina.edu '
      write(iounit,*) ' Steve Taylor, Los Alamos National Lab'
      write(iounit,*) ' taylor@beta.lanl.gov'
      write(iounit,*) ' Funded by Treaty Verification Program, Lawrence Livermore National Lab '
#ifdef DIFFSEIS
      write(iounit,*) ' Efficient Differential Seismogram Technique funded '
      write(iounit,*) ' by Dept. of Geological Sciences, University of South Carolina '
#endif
      write(iounit,*) ' based on '
      write(iounit,*) ' Seismic Wave Propagation in Stratified Media, by B.L.N. Kennett '
      write(iounit,*) ' Cambridge University Press, 1983'
c
      write(iounit,*) ' input filename: '
      read(inunit,'(a40)') infil
      open(unit=8,file=infil)
      write(iounit,*) ' '
      write(iounit,*) ' Input from file: ', infil
      write(iounit,*) ' '
c
c
      read(8,*) nlyrs
      if ( nlyrs .gt. LL ) then
	write(iounit,*) ' Input Error:  Too many layers; requested, max:', nlyrs, LL
	stop
      endif
      write(iounit,*) ' '
      write(iounit,*) ' Velocity Model'
      write(iounit,*) ' number of layers is: ', nlyrs, ' (max layers ', LL, ' )'
c     write(iounit,604)
  604 format(' for each layer: th,al0,be0,dens,qpm,qsm,tp1,tp2,ts1,ts2? ')
      read(8,*) (th(l),al0(l),be0(l),dens(l),qpm(l),qsm(l),
     $tp1(l),tp2(l),ts1(l),ts2(l),l=1,nlyrs)
      write(iounit,6663)
 6663 format('  l th     al0    be0   dens   qpm   qsm    tau p,s')
      write(iounit,6661) (l,th(l),al0(l),be0(l),dens(l),qpm(l),qsm(l),

     $tp1(l),tp2(l),ts1(l),ts2(l),l=1,nlyrs)
 6661 format(' ',i3,1x,4f6.3,2f7.1,4e10.3)
      do 6000 l = 1, nlyrs
         if ( l .eq. 1 ) then
	    deptop(1) = 0.0
	 else
	    deptop(l) = deptop(l-1) + th(l-1)
	 endif
	 if (  (th(l) .le. 0.) .and. (l .lt. nlyrs) ) then
	    write(iounit,*) ' Input Error:  layer ',l,' th <= 0 '
	    stop
	 endif
	 if (  al0(l) .le. 0. ) then
	    write(iounit,*) ' Input Error:  layer ',l,' al0 <= 0 '
	    stop
	 endif
	 if (  be0(l) .le. 0. ) then
	    write(iounit,*) ' Input Error:  layer ',l,' be0 <= 0 '
	    stop
	 endif
	 if (  dens(l) .le. 0. ) then
	    write(iounit,*) ' Input Error:  layer ',l,' dens <= 0 '
	    stop
	 endif
	 if (  qpm(l) .le. 0. ) then
	    write(iounit,*) ' Input Error:  layer ',l,' qpm <= 0 '
	    stop
	 endif
	 if (  qsm(l) .le. 0. ) then
	    write(iounit,*) ' Input Error:  layer ',l,' qps <= 0 '
	    stop
	 endif
 6000 continue
c
      open( unit = 7, file = 'mspec  ',form = 'unformatted')
#ifdef PNTFRC
      open( unit = 10, file = 'fspec  ',form = 'unformatted')
#endif
c     open( unit = 9, file = 'rwpint',form = 'unformatted')
c     write(iounit,*) ' output spectrum into file spec'
      read(8,*) pmin,plc,puc,pmax,pfac
      if ( pmin .ge. pmax ) then
	write(iounit,*) ' Input Error: pmin >= pmax : ', pmin, pmax
	stop
      endif
      if ( plc .ge. puc ) then
	write(iounit,*) ' Input Error: plc >= puc : ', plc, puc
	stop
      endif
      if ( ( plc .ge. pmax ) .or. ( puc .le. pmin ) ) then
	write(iounit,*) ' Input Error: plc >= pmax OR puc <= pmin : ', pmin, plc, puc, pmax
	stop
      endif
      if ( pfac .le. 0.0 ) then
	write(iounit,*) ' Input Error: pfac <= 0.0 : ', pfac
	stop
      endif
c     write(iounit,*) ' range of p (slowness) values: ', pmin,plc,puc,pmax
c     write(iounit,*) ' pfac for delta p  ', pfac
      read(8,*) fmin,fmax,fny,nft
      if ( fmin .ge. fmax ) then
	write(iounit,*) ' Input Error: fmin >= fmax : ', fmin, fmax
	stop
      endif
      if ( fmax .gt. fny ) then
	write(iounit,*) ' Input Error: fmax >= fny : ', fmax, fny
	stop
      endif
      if ( ( nft/2 + 1 ) .gt. FF ) then
	write(iounit,*) ' Input Error: nft/2 + 1 >= FF : ', (nft/2 + 1), FF
	stop
      endif
c     write(iounit,*) ' fmin,fmax,fny,nft :' , fmin,fmax,fny,nft
c
c     Range input
c      input r > 0 for a single range
c      input r = 0 to compute profile for number of ranges: rmin,dr,nr
c      input r < 0 to compute profile for number of ranges: nr, (rh(i),i=1,nr)
c     read(8,*) r, azif, azis
      read(8,*) r, azis
c
      if (r .eq. 0.) then
       read(8,*) rmin,dr,nr
       if ( nr .gt. RR ) then
	  write(iounit,*) ' Input Error: nr > RR : ', nr, RR
	  stop
       endif
       r1 = rmin
       do 25 i = 1,nr
       rh(i) = r1
25     r1 = r1 + dr
      else if (r .lt. 0.) then
       read(8,*) nr
       if ( nr .gt. RR ) then
	  write(iounit,*) ' Input Error: nr > RR : ', nr, RR
	  stop
       endif
       read(8,*) (rh(i),i=1,nr)
      else
       rmin = r
       nr = 1
       rh(1) = r
      endif
c
c     find Maximum range for delta p calculation
c
      distmx = -1.0
      do 26 i = 1, nr
	if ( rh(i) .le. 0.0 ) then
	  write(iounit,*) ' Input Error: range(i) <= 0. : ', i, rh(i)
	  stop
	endif
	if ( rh(i) .gt. distmx ) distmx = rh(i)
26    continue
c
c     Source layers and depth(s)
c
#ifndef MULTIDEP
c     read(8,*) depth(1)
      read(8,*) depth(1)
      nd = 1
#endif
#ifdef MULTIDEP
c     read(8,*) isrcl,nd
      read(8,*) nd
      if ( nd .gt. DD ) then
	write(iounit,*) ' Input Error: too many source depths :', nd , DD
	stop
      endif
      read(8,*) (depth(id), id=1,nd)
#endif
c     read(8,*) irlyr, recdep
      read(8,*) recdep
c     
c     Make sure depth(id)  is in source layer
c
      do 261 l = 1, nlyrs-1
	 if ( ( depth(1) .gt. deptop(l) ) .and. ( depth(1) .lt. deptop(l+1) ) ) isrcl = l
	 if ( ( recdep .gt. deptop(l) ) .and. ( recdep .lt. deptop(l+1) ) ) irlyr = l
 261  continue
      if ( recdep .eq. 0.0 ) irlyr = 1
      if ( isrcl .ge. nlyrs ) then
	write(iounit,*) ' Input Error: source layer in half space'
	stop
      endif
      do 27 i = 1, nd
      if ( ( depth(i) .le. deptop(isrcl) ) .or. ( depth(i) .ge. deptop(isrcl+1) ) ) then
	write(iounit,*) ' Input Error: Source outside source layer', i, depth(i)
	stop
      endif
27    continue
c     
c     Make sure Zs .ge. Zr
c
c     if ( ( recdep .le. deptop(irlyr) ) .or. ( recdep .ge. deptop(irlyr) ) ) then
c	write(iounit,*) ' Input Error: Receiver outside receiver layer', irlyr, srcdep
c	stop
c     endif
      if(irlyr .gt. isrcl) then
       write(iounit,*) ' Input Error: Receiver Depth Zr > Source Depth Zs, that is a no-no'
       stop
      endif
      do 28 i = 1, nd
      if(irlyr .eq. isrcl .and. recdep .gt. depth(i)) then
       write(iounit,*) ' Input Error: Receiver Depth Zr > Source Depth Zs, that is a no-no'
       stop
      endif
28    continue
c
c
c
      write(iounit,*) ' '
      write(iounit,*) ' Ranges and Azimuths computed '
      if (r .gt. 0.) then
c     write(iounit,*) ' range and azimuth fault, sta. : ', r, azif, azis
      write(iounit,*) ' range and azimuth station : ', r, azis
      else
c     write(iounit,*) ' azimuth fault, sta.  ',azif, azis
      write(iounit,*) ' azimuth  station  ',azis
      write(iounit,*)
     *     'seismograms calculated at following distances (km)'
      write(iounit,155) (rh(i),i=1,nr)
  155 format(8(f7.2,1x))
      endif
      write(iounit,*) ' '
      write(iounit,*) ' Sources and Receiver Depths '
      write(iounit,*) ' source layer and depth :', isrcl,depth
      write(iounit,*) ' receiver layer and depth :', irlyr,recdep
      lrecdep = recdep - deptop(irlyr)
      do 29 l = 1, nd
	 ldepth(l) = depth(l) - deptop(isrcl)
 29   continue
c
c     Compute slowness and frequency parameters
c
      pc1 = ( pmin + plc ) * 0.5
      pr1 = pi / ( plc - pmin )
      pc2 = ( pmax + puc ) * 0.5
      pr2 = pi / ( pmax - puc )
c
c     pfac insures adequate sampling in slowness, larger pfac
c     gives smaller delta p, delp (finer sampling)
c
      dt = 1/(2.*fny)
      t = nft * dt
      delf = 2. * fny / nft
      nfpts = nft/2 + 1
      ifmin = fmin / delf
      ifmax = fmax / delf
      nffpts = ifmax - ifmin + 1
c
      write(iounit,*) ' '
      write(iounit,*) ' Frequency Domain and Time Domain parameters '
      write(iounit,*) ' fmin,fmax,fny,nft :' , fmin,fmax,fny,nft
      write(iounit,*) ' Sampling rate (dt) and total trace length (T) : ', dt, t
      write(iounit,*) ' Number of Frequencies values computed : ',nffpts
c
c     delp - delta p
      delp = 1./(fmax * 2.*pfac * distmx)
      npmin = pmin / delp
      npmax = pmax / delp
      nppts = npmax - npmin + 1
#ifdef PARALLELP4
      lnppts = int( nppts / nodes ) * nodes
      if ( lnppts .ne. nppts ) then
	lnppts = ( int( nppts / nodes ) + 1 ) * nodes
	delp = ( pmax - pmin ) / lnppts
	nppts = lnppts
	npmin = pmin / delp
	npmax = pmax / delp
      endif
      epfac = 1. / ( 2. * fmax * distmx * delp )
#endif
c
      write(iounit,*) ' '
      write(iounit,*) ' Slowness integration parameters '
      write(iounit,*) ' range of p (slowness) values: ', pmin,plc,puc,pmax
      write(iounit,*) ' range of c (phase velocity) values: ', 1/pmax,1/puc,1/plc,1/pmin
      write(iounit,*) ' pfac for delta p  ', pfac
      write(iounit,*) ' Number of slownesses and dp: ', nppts, delp
#ifdef PARALLELP4
      write(iounit,*) ' Effective parallel pfac for delta p  ', epfac
      call p4flush
      endif
#endif
C
C=====================================================
C Broadcast Model parameters to all Slaves
C End Master Input & Broadcast
#ifdef PARALLELP4
      if ( mynod .eq. 0 ) then
      ip4buf(1) = nlyrs
      rp4buf(2) = pmin
      rp4buf(3) = plc
      rp4buf(4) = puc
      rp4buf(5) = pmax
      rp4buf(6) = pfac
      rp4buf(7) = fmin
      rp4buf(8) = fmax
      rp4buf(9) = fny
      ip4buf(10) = nft
      ip4buf(11) = nr
      rp4buf(12) = azis
      ip4buf(13) = isrcl
      ip4buf(14) = nd
      rp4buf(15) = recdep
      ip4buf(16) = irlyr
      rp4buf(17) = dt
      rp4buf(18) = t
      rp4buf(19) = delf
      ip4buf(20) = nfpts
      ip4buf(21) = ifmin
      ip4buf(22) = ifmax
      ip4buf(23) = nffpts
      rp4buf(24) = delp
      ip4buf(25) = npmin
      ip4buf(26) = npmax
      ip4buf(27) = nppts
      rp4buf(28) = lrecdep
      rp4buf(29) = pc1
      rp4buf(30) = pr1
      rp4buf(31) = pc2
      rp4buf(32) = pr2
      p4mtyp=SCALARS
      call p4brdcst(p4mtyp,ip4buf,32*INTSIZ,retcod)
      p4mtyp=ALFASS
      call p4brdcst(p4mtyp,al0,nlyrs*FLTSIZ,retcod)
      p4mtyp=BETASS
      call p4brdcst(p4mtyp,be0,nlyrs*FLTSIZ,retcod)
      p4mtyp=RHOSS
      call p4brdcst(p4mtyp,dens,nlyrs*FLTSIZ,retcod)
      p4mtyp=THIKS
      call p4brdcst(p4mtyp,th,nlyrs*FLTSIZ,retcod)
      p4mtyp=QPS
      call p4brdcst(p4mtyp,qpm,nlyrs*FLTSIZ,retcod)
      p4mtyp=QSS
      call p4brdcst(p4mtyp,qsm,nlyrs*FLTSIZ,retcod)
      p4mtyp=TP1S
      call p4brdcst(p4mtyp,tp1,nlyrs*FLTSIZ,retcod)
      p4mtyp=TP2S
      call p4brdcst(p4mtyp,tp2,nlyrs*FLTSIZ,retcod)
      p4mtyp=TS1S
      call p4brdcst(p4mtyp,ts1,nlyrs*FLTSIZ,retcod)
      p4mtyp=TS2S
      call p4brdcst(p4mtyp,ts2,nlyrs*FLTSIZ,retcod)
      p4mtyp=DEPS
      call p4brdcst(p4mtyp,deptop,nlyrs*FLTSIZ,retcod)
      p4mtyp=RANGES
      call p4brdcst(p4mtyp,rh,nr*FLTSIZ,retcod)
      p4mtyp=DEPTHS
      call p4brdcst(p4mtyp,depth,nd*FLTSIZ,retcod)
      p4mtyp=LDEPS
      call p4brdcst(p4mtyp,ldepth,nd*FLTSIZ,retcod)
      endif
#endif
C=====================================================
C Slaves Receive Parameters Braodcast from master
C Slaves figure out what fraction of P loop to do 
C Based on How many total # processors and Which processor
C resulting in p0 and p1 integral limts for the processor
#ifdef PARALLELP4
      if ( mynod .ne. 0 ) then
      p4mtyp=SCALARS
      p4nod=MASTERNODE
      call p4recv(p4mtyp,p4nod,ip4buf,32*INTSIZ,rcvlen,retcod)
      nlyrs = ip4buf(1) 
      pmin = rp4buf(2) 
      plc = rp4buf(3) 
      puc = rp4buf(4) 
      pmax = rp4buf(5) 
      pfac = rp4buf(6) 
      fmin = rp4buf(7) 
      fmax = rp4buf(8) 
      fny = rp4buf(9) 
      nft = ip4buf(10) 
      nr = ip4buf(11) 
      azis = rp4buf(12) 
      isrcl = ip4buf(13) 
      nd = ip4buf(14) 
      recdep = rp4buf(15) 
      irlyr = ip4buf(16) 
      dt = rp4buf(17) 
      t = rp4buf(18) 
      delf = rp4buf(19) 
      nfpts = ip4buf(20) 
      ifmin = ip4buf(21) 
      ifmax = ip4buf(22) 
      nffpts = ip4buf(23) 
      delp = rp4buf(24) 
      npmin = ip4buf(25) 
      npmax = ip4buf(26) 
      nppts = ip4buf(27) 
      lrecdep = rp4buf(28) 
      pc1 = rp4buf(29)
      pr1 = rp4buf(30)
      pc2 = rp4buf(31)
      pr2 = rp4buf(32)
      p4mtyp=ALFASS
      call p4recv(p4mtyp,p4nod,al0,nlyrs*FLTSIZ,rcvlen,retcod)
      p4mtyp=BETASS
      call p4recv(p4mtyp,p4nod,be0,nlyrs*FLTSIZ,rcvlen,retcod)
      p4mtyp=RHOSS
      call p4recv(p4mtyp,p4nod,dens,nlyrs*FLTSIZ,rcvlen,retcod)
      p4mtyp=THIKS
      call p4recv(p4mtyp,p4nod,th,nlyrs*FLTSIZ,rcvlen,retcod)
      p4mtyp=QPS
      call p4recv(p4mtyp,p4nod,qpm,nlyrs*FLTSIZ,rcvlen,retcod)
      p4mtyp=QSS
      call p4recv(p4mtyp,p4nod,qsm,nlyrs*FLTSIZ,rcvlen,retcod)
      p4mtyp=TP1S
      call p4recv(p4mtyp,p4nod,tp1,nlyrs*FLTSIZ,rcvlen,retcod)
      p4mtyp=TP2S
      call p4recv(p4mtyp,p4nod,tp2,nlyrs*FLTSIZ,rcvlen,retcod)
      p4mtyp=Ts1S
      call p4recv(p4mtyp,p4nod,ts1,nlyrs*FLTSIZ,rcvlen,retcod)
      p4mtyp=TS2S
      call p4recv(p4mtyp,p4nod,ts2,nlyrs*FLTSIZ,rcvlen,retcod)
      p4mtyp=DEPS
      call p4recv(p4mtyp,p4nod,deptop,nlyrs*FLTSIZ,rcvlen,retcod)
      p4mtyp=RANGES
      call p4recv(p4mtyp,p4nod,rh,nr*FLTSIZ,rcvlen,retcod)
      p4mtyp=DEPTHS
      call p4recv(p4mtyp,p4nod,depth,nd*FLTSIZ,rcvlen,retcod)
      p4mtyp=LDEPS
      call p4recv(p4mtyp,p4nod,ldepth,nd*FLTSIZ,rcvlen,retcod)
      endif
      lpnnppts = nppts / nodes
      lpnpmin = npmin + lpnnppts * ( mynod )
      lpnpmax = npmin + lpnnppts * ( mynod + 1) - 1
      write(iounit,*) ' '
      write(iounit,*) ' Per Node slowness info '
      write(iounit,*) ' Node #, delp, np, npmin, npmax ', mynod,delp,lpnnppts,lpnpmin,lpnpmax
      write(iounit,*) ' Node #, pmin, pmax ', mynod,delp*lpnpmin,delp*lpnpmax
      nppts = lpnnppts
      npmin = lpnpmin
      npmax = lpnpmax
#endif
C=====================================================
C Slaves (and Master?) start the slowness loop for 
C synthetic calc.
C=====================================================
C
c
c     Compute azimuthal terms (fault orientation now obsolete)
c
c     azi = pi*azif/180.
c     sinazi = sin(azi)
c     cosazi = cos(azi)
c     sin2az = sin(2.*azi)
c     cos2az = cos(2.*azi)
c     cosaz2 = cosazi*cosazi
c     sinaz2 = sinazi*sinazi
      azi = pi*azis/180.
      expiaz = cexp( (0.,1.) * azi )
      expmiaz = cexp( (0.,-1.) * azi )
      expi2az = expiaz * expiaz
      expmi2az = expmiaz * expmiaz
c
c     calculate Lames constants for source layer
c
      srclyr = isrcl
      a2 = al0(srclyr)**2
      b2 = be0(srclyr)**2
      rmu = dens(srclyr)*b2
      rlamb = dens(srclyr)*(a2 - 2.0*b2)
c
c---------------------------------------------------------------------------------
c     source representation follows Aki and Richards (1980) Box 4.4
c     and Kennett (1983) (4.59 - 4.60)
c     Note: if the sources are changed, calls to srcvec may have
c           to change to get proper order numbers and construction
c           of u0,w0,tn arrays must also change to reflect order
c
c     Use simple Mij terms, and use a postprocessor to recombine for
c
c         an arbitrary orientation Moment Tensor ala A&R Box 4.4
c
#ifdef MULTISRC
c---------------------------------------------------------------------------------
c **** Individual moment tensor solutions follow ***
c
c     Mzz  (order 0)
c
      mxx(1) = 0.
      mxy(1) = 0.
      mxz(1) = 0.
      myy(1) = 0.
      myz(1) = 0.
      mzz(1) = 1.
c
c     Mxy  (order +2,-2)
c
      mxx(2) = 0.
      mxy(2) = 1.
      mxz(2) = 0.
      myy(2) = 0.
      myz(2) = 0.
      mzz(2) = 0.
c
c     Mxz (order +1,-1)
c
      mxx(3) = 0
      mxy(3) = 0.
      mxz(3) = 1.
      myy(3) = 0.
      myz(3) = 0.
      mzz(3) = 0.
c
c     Mxx  (order 0,+2,-2)
c
      mxx(4) = 1.
      mxy(4) = 0.
      mxz(4) = 0.
      myy(4) = 0.
      myz(4) = 0.
      mzz(4) = 0.
c
c     Myz  (order +1,-1)
c
      mxx(5) = 0
      mxy(5) = 0.
      mxz(5) = 0.
      myy(5) = 0.
      myz(5) = 1.
      mzz(5) = 0.
c
c     Myy  (order 0,+2,-2)
c
      mxx(6) = 0.
      mxy(6) = 0.
      mxz(6) = 0.
      myy(6) = 1.
      myz(6) = 0.
      mzz(6) = 0.
c
	write(iounit,*) ' '
	write(iounit,*) ' Calculating 6 Moment Tensor Sources: Mxx, Mxy, Mxz, Myy, Myz, Mzz '
c
#endif
#ifdef SNGLSRC
c
c     Mij General moment Tensor
c
#ifdef PARALLELP4
	if ( mynod .eq. 0 ) then
#endif
        read(8,*) mxx(1),mxy(1),mxz(1),myy(1),myz(1),mzz(1)
	write(iounit,*) ' '
	write(iounit,*) ' Calculating Moment Tensor: Mxx, Mxy, Mxz, Myy, Myz, Mzz '
	write(iounit,*) mxx(1),mxy(1),mxz(1),myy(1),myz(1),mzz(1)
#ifdef PARALLELP4
	rp4buf(1) = mxx(1)
	rp4buf(2) = mxy(1)
	rp4buf(3) = mxz(1)
	rp4buf(4) = myy(1)
	rp4buf(5) = myz(1)
	rp4buf(6) = mzz(1)
	p4mtyp=MIJS
	call p4brdcst(p4mtyp,rp4buf,6*FLTSIZ,retcod)
	endif
	if ( mynod .ne. 0 ) then
	p4mtyp=MIJS
	call p4recv(p4mtyp,p4nod,rp4buf,6*FLTSIZ,rcvlen,retcod)
	mxx(1) = rp4buf(1) 
	mxy(1) = rp4buf(2) 
	mxz(1) = rp4buf(3) 
	myy(1) = rp4buf(4) 
	myz(1) = rp4buf(5) 
	mzz(1) = rp4buf(6) 
	endif
	call p4flush
#endif
#endif
#ifdef DIFFSEIS
#ifdef PARALLELP4
	if ( mynod .eq. 0 ) then
#endif
        read(8,*) perturb
	if (( perturb .lt. 1 ) .or. ( perturb .gt. 4 )) then
	  write(iounit,*) ' Input Error: illegal perturbation code: ', perturb
	  write(iounit,*) ' Perturbation code must be between 1 and 4 '
	  stop
	endif
	if ( perturb .eq. 1 ) then
	  write(iounit,*) ' Computing Partials for P wave velocity'
	  perta = 1.02
	  pertb = 1.00
	  pertr = 1.00
	  do 1001 i = 1, nlyrs
	    diffnorm(i) = al0(i) * .02
1001      continue
	endif
	if ( perturb .eq. 2 ) then
	  write(iounit,*) ' Computing Partials for S wave velocity'
	  perta = 1.00
	  pertb = 1.02
	  pertr = 1.00
	  do 1002 i = 1, nlyrs
	    diffnorm(i) = be0(i) * .02
1002      continue
	endif
	if ( perturb .eq. 3 ) then
	  write(iounit,*) ' Computing Partials for P wave velocity, S and Rho tied to P'
	  perta = 1.01
	  pertb = 1.01
	  pertr = 1.01
	  do 1003 i = 1, nlyrs
	    diffnorm(i) = al0(i) * .01
1003      continue
	endif
	if ( perturb .eq. 4 ) then
	  write(iounit,*) ' Computing Partials for P wave velocity, P and Rho tied to S'
	  perta = 1.01
	  pertb = 1.01
	  pertr = 1.01
	  do 1004 i = 1, nlyrs
	    diffnorm(i) = be0(i) * .01
1004      continue
	endif
#ifdef PARALLELP4
	rp4buf(1) = perta
	rp4buf(2) = pertb
	rp4buf(3) = pertr
	p4mtyp=PERTS
	call p4brdcst(p4mtyp,rp4buf,3*FLTSIZ,retcod)
        endif
	if ( mynod .ne. 0 ) then
	p4mtyp=PERTS
	call p4recv(p4mtyp,p4nod,rp4buf,3*FLTSIZ,rcvlen,retcod)
	perta = rp4buf(1) 
	pertb = rp4buf(2) 
	pertr = rp4buf(3) 
        endif
        call p4flush
#endif
#endif
#ifdef PNTFRC
c
c Point Forces, Fz, Fx, Fy
c
      fx(1) = 0.
      fy(1) = 0.
      fz(1) = 1.
c
      fx(2) = 1.
      fy(2) = 0.
      fz(2) = 0.
c
      fx(3) = 0.
      fy(3) = 1.
      fz(3) = 0.
c
	write(iounit,*) ' '
	write(iounit,*) ' Calculating 3 Point Forces Fx, Fy, Fz'
#endif
c---------------------------------------------------------------------------------
c
c     compute q, alfa, and beta at 1 hz for absorption band
c     allphs - compute ref/trans terms for all conversions
c     prmphs - no converted phases
c     cnvphs - only converted phases
c
c
      wq = twopi
      do 5 i = 1, nlyrs
         qa = qpm(i)
         qb = qsm(i)
c        alfa(i) = al0(i) * vabm(wq,tp1(i),tp2(i),qa)
c        beta(i) = be0(i) * vabm(wq,ts1(i),ts2(i),qb)
c        qa = qabm(wq,tp1(i),tp2(i),qa)
c        qb = qabm(wq,ts1(i),ts2(i),qb)
c        alfa(i) = alfa(i)*( 1. + (0.,0.5)/qa)
c        beta(i) = beta(i)*( 1. + (0.,0.5)/qb)
         alfa(i) = al0(i)*( 1. + (0.,0.5)/qa)
         beta(i) = be0(i)*( 1. + (0.,0.5)/qb)
         cnvrsn(i) = allphs
c        cnvrsn(i) = prmphs
         reverb(i) = allrvb
         rho(i) = dens(i)
 5       thik(i) = th(i)
         cnvrsn(0) = cnvrsn(1)
      write(iounit,*) ' '
      write(iounit,*) ' Level of Completeness '
      if(cnvrsn(1) .eq. allphs) then
        write(iounit,*)
     *  ' Compute refl/trans for all phases plus conversions'
      else if(cnvrsn(1) .eq. prmphs) then
        write(iounit,*) ' No converted phases'
      else if(cnvrsn(1) .eq. cnvphs) then
        write(iounit,*) ' Compute refl/trans for only converted phases'
      endif
c
#ifndef MULTIDEP
#ifdef MULTISRC
      nsrc = mxsrc
      do 2 k = 1, nr
      do 2 j = 1, nsrc
      do 1 i = 1,maxarr 
         u0t(i,j,k) = (0.,0.)
         w0t(i,j,k) = (0.,0.)
         tnt(i,j,k) = (0.,0.)
#ifdef PNTFRC
         if ( (j .ge. 1) .and. (j .le. 3) ) then
           u0ft(i,j,k) = (0.,0.)
           w0ft(i,j,k) = (0.,0.)
           tnft(i,j,k) = (0.,0.)
          endif
#endif
#endif
#ifdef SNGLSRC
      nsrc = 1
      do 2 k = 1, nr
      do 1 i = 1,maxarr 
         u0t(i,k) = (0.,0.)
         w0t(i,k) = (0.,0.)
         tnt(i,k) = (0.,0.)
#endif
#endif
#ifdef MULTIDEP
#ifdef MULTISRC
      nsrc = mxsrc
      do 2 k = 1, nr
      do 2 id =1, nd
      do 2 j = 1, nsrc
      do 1 i = 1,maxarr 
         u0t(i,j,id,k) = (0.,0.)
         w0t(i,j,id,k) = (0.,0.)
         tnt(i,j,id,k) = (0.,0.)
#ifdef PNTFRC
         if ( (j .ge. 1) .and. (j .le. 3) ) then
           u0ft(i,j,id,k) = (0.,0.)
           w0ft(i,j,id,k) = (0.,0.)
           tnft(i,j,id,k) = (0.,0.)
          endif
#endif
#endif
#ifdef SNGLSRC
      nsrc = 1
      do 2 k = 1, nr
      do 2 id =1, nd
      do 1 i = 1,maxarr 
         u0t(i,id,k) = (0.,0.)
         w0t(i,id,k) = (0.,0.)
         tnt(i,id,k) = (0.,0.)
#endif
#endif
1     continue
2     continue
c
#ifdef PARALLELP4
      if ( mynod .eq. 0 ) then
#endif




#ifndef DIFFSEIS
      write(7) fmin,fmax,delf,nffpts,fny,nfpts,nr,nsrc,nd,azis
      write(7) (rh(i),i=1,nr)
      write(7) (depth(i),i=1,nd)
#endif
#ifdef DIFFSEIS
      write(7) fmin,fmax,delf,nffpts,fny,nfpts,nr,nsrc,nlyrs+1,azis
      write(7) (rh(i),i=1,nr)
      do 1000 i = 1, nlyrs
	 depth(i+1) = 1000 + i
 1000 continue
      write(7) (depth(i),i=1,nlyrs+1)
#endif
#ifdef PNTFRC
      write(10) fmin,fmax,delf,nffpts,fny,nfpts,nr,nd,azis
      write(10) (rh(i),i=1,nr)
      write(10) (depth(i),i=1,nd)
#endif
#ifdef PARALLELP4
      endif
      call p4flush
#endif
c     write(9) fmin,fmax,delf,nffpts,fny,nfpts
c     write(9) pmin,pmax,delp,npmin,npmax,nppts
c
c---------------------------------------------------------------------------------
c
c***** begin slowness loop *****************
c
      do 7 ip = npmin, npmax
        p = ip * delp
        pc = p
        freq = (1.,0.)
c
c       for each p, find frequency-independent reflection/tranmission 
c       coefficients for each interface (see p.106 kennett)
c
        call ifmat(1,pc,freq,nlyrs)
#ifdef DIFFSEIS
        call delifm(pc,freq,nlyrs,perta,pertb,pertr)
#endif
c
c	if ( ip .eq. npmin ) write(9) alfas,betas,rhos
c       call srcmat(pc,freq)
c
c       compute upward and downward P and S waves from source for
c       angular order m, slowness pc, and source number specified above
c       note: make sure order number corresponds to moment tensor
c             source defined above
c
#ifdef MULTISRC
c
c     Mzz  (order 0)
c
        call srcvec(0,pc,1)
	pup(1) =pup0(1)
	pdn(1) =pdn0(1)
	svup(1) =svup0(1)
	svdn(1) =svdn0(1)
c
c     Mxy  (order +2,-2)
c
        call srcvec(2,pc,2)
        call srcvec(-2,pc,2)
	pup(2) =  pupp2(2) * expi2az + pupm2(2) * expmi2az
	pdn(2) =  pdnp2(2) * expi2az + pdnm2(2) * expmi2az
	svup(2) = svupp2(2) * expi2az + svupm2(2) * expmi2az
	svdn(2) = svdnp2(2) * expi2az + svdnm2(2) * expmi2az
	shup(2) = shupp2(2) * expi2az + shupm2(2) * expmi2az
	shdn(2) = shdnp2(2) * expi2az + shdnm2(2) * expmi2az
c
c     Mxz (order +1,-1)
c
        call srcvec(1,pc,3)
        call srcvec(-1,pc,3)
	pup(3) = pupp1(3) * expiaz - pupm1(3) * expmiaz
	pdn(3) = pdnp1(3) * expiaz - pdnm1(3) * expmiaz
	svup(3) = svupp1(3) * expiaz - svupm1(3) * expmiaz
	svdn(3) = svdnp1(3) * expiaz - svdnm1(3) * expmiaz
	shup(3) = shupp1(3) * expiaz - shupm1(3) * expmiaz
	shdn(3) = shdnp1(3) * expiaz - shdnm1(3) * expmiaz
c
c     Mxx  (order 0,+2,-2)
c
        call srcvec(0,pc,4)
        call srcvec(2,pc,4)
        call srcvec(-2,pc,4)
	pup(4) = pup0(4) - pupp2(4) * expi2az - pupm2(4) * expmi2az
	pdn(4) = pdn0(4) - pdnp2(4) * expi2az - pdnm2(4) * expmi2az
	svup(4) = svup0(4) - svupp2(4) * expi2az - svupm2(4) * expmi2az
	svdn(4) = svdn0(4) - svdnp2(4) * expi2az - svdnm2(4) * expmi2az
	shup(4) = shup0(4) - shupp2(4) * expi2az - shupm2(4) * expmi2az
	shdn(4) = shdn0(4) - shdnp2(4) * expi2az - shdnm2(4) * expmi2az
c
c     Myz  (order +1,-1)
c
        call srcvec(1,pc,5)
        call srcvec(-1,pc,5)
	pup(5) = pupp1(5) * expiaz - pupm1(5) * expmiaz
	pdn(5) = pdnp1(5) * expiaz - pdnm1(5) * expmiaz
	svup(5) = svupp1(5) * expiaz - svupm1(5) * expmiaz
	svdn(5) = svdnp1(5) * expiaz - svdnm1(5) * expmiaz
	shup(5) = shupp1(5) * expiaz - shupm1(5) * expmiaz
	shdn(5) = shdnp1(5) * expiaz - shdnm1(5) * expmiaz
c
c     Myy  (order 0,+2,-2)
c
        call srcvec(0,pc,6)
        call srcvec(2,pc,6)
        call srcvec(-2,pc,6)
	pup(6) = pup0(6) - pupp2(6) * expi2az - pupm2(6) * expmi2az
	pdn(6) = pdn0(6) - pdnp2(6) * expi2az - pdnm2(6) * expmi2az
	svup(6) = svup0(6) - svupp2(6) * expi2az - svupm2(6) * expmi2az
	svdn(6) = svdn0(6) - svdnp2(6) * expi2az - svdnm2(6) * expmi2az
	shup(6) = shup0(6) - shupp2(6) * expi2az - shupm2(6) * expmi2az
	shdn(6) = shdn0(6) - shdnp2(6) * expi2az - shdnm2(6) * expmi2az
#ifdef PNTFRC
c
c     Fz  (order 0)
c
        call frcvec(0,pc,1)
	fpup(1) = fpup0(1)
	fpdn(1) = fpdn0(1)
	fsvup(1) = fsvup0(1)
	fsvdn(1) = fsvdn0(1)
c
c     Fx (order +1,-1)
c
        call frcvec(-1,pc,2)
        call frcvec(1,pc,2)
	fpup(2) = fpupp1(2) * expiaz - fpupm1(2) * expmiaz
	fpdn(2) = fpdnp1(2) * expiaz - fpdnm1(2) * expmiaz
	fsvup(2) = fsvupp1(2) * expiaz - fsvupm1(2) * expmiaz
	fsvdn(2) = fsvdnp1(2) * expiaz - fsvdnm1(2) * expmiaz
	fshup(2) = fshupp1(2) * expiaz - fshupm1(2) * expmiaz
	fshdn(2) = fshdnp1(2) * expiaz - fshdnm1(2) * expmiaz
c
c     Fy (order +1,-1)
c
        call frcvec(-1,pc,3)
        call frcvec(1,pc,3)
	fpup(3) = fpupp1(3) * expiaz - fpupm1(3) * expmiaz
	fpdn(3) = fpdnp1(3) * expiaz - fpdnm1(3) * expmiaz
	fsvup(3) = fsvupp1(3) * expiaz - fsvupm1(3) * expmiaz
	fsvdn(3) = fsvdnp1(3) * expiaz - fsvdnm1(3) * expmiaz
	fshup(3) = fshupp1(3) * expiaz - fshupm1(3) * expmiaz
	fshdn(3) = fshdnp1(3) * expiaz - fshdnm1(3) * expmiaz
#endif
#endif
#ifdef SNGLSRC
c
c       source is general moment tensor, handling is different, all orders used
c       use source 1 as storage, only source used in this case
c       order 0, +1, -1, +2, -2
c
        call srcvec(0,pc,1)
        call srcvec(1,pc,1)
        call srcvec(-1,pc,1)
        call srcvec(2,pc,1)
        call srcvec(-2,pc,1)
	pupg0 = pup0(1) - pupp2(1) * expi2az - pupm2(1) * expmi2az
	pdng0 = pdn0(1) - pdnp2(1) * expi2az - pdnm2(1) * expmi2az
	svupg0 = svup0(1) - svupp2(1) * expi2az - svupm2(1) * expmi2az
	svdng0 = svdn0(1) - svdnp2(1) * expi2az - svdnm2(1) * expmi2az
	shupg0 = shup0(1) - shupp2(1) * expi2az - shupm2(1) * expmi2az
	shdng0 = shdn0(1) - shdnp2(1) * expi2az - shdnm2(1) * expmi2az
	pupg1 = pupp1(1) * expiaz - pupm1(1) * expmiaz
	pdng1 = pdnp1(1) * expiaz - pdnm1(1) * expmiaz
	svupg1 = svupp1(1) * expiaz - svupm1(1) * expmiaz
	svdng1 = svdnp1(1) * expiaz - svdnm1(1) * expmiaz
	shupg1 = shupp1(1) * expiaz - shupm1(1) * expmiaz
	shdng1 = shdnp1(1) * expiaz - shdnm1(1) * expmiaz
#endif
#ifdef DIFFSEIS
c
c       for the perturbed velocity structure:
c       source is general moment tensor, handling is different, all orders used
c       use source 1 as storage, only source used in this case
c       order 0, +1, -1, +2, -2
c
        call delsrcvec(0,pc,1)
        call delsrcvec(1,pc,1)
        call delsrcvec(-1,pc,1)
        call delsrcvec(2,pc,1)
        call delsrcvec(-2,pc,1)
	dpupg0 = dpup0(1) - dpupp2(1) * expi2az - dpupm2(1) * expmi2az
	dpdng0 = dpdn0(1) - dpdnp2(1) * expi2az - dpdnm2(1) * expmi2az
	dsvupg0 = dsvup0(1) - dsvupp2(1) * expi2az - dsvupm2(1) * expmi2az
	dsvdng0 = dsvdn0(1) - dsvdnp2(1) * expi2az - dsvdnm2(1) * expmi2az
	dshupg0 = dshup0(1) - dshupp2(1) * expi2az - dshupm2(1) * expmi2az
	dshdng0 = dshdn0(1) - dshdnp2(1) * expi2az - dshdnm2(1) * expmi2az
	dpupg1 = dpupp1(1) * expiaz - dpupm1(1) * expmiaz
	dpdng1 = dpdnp1(1) * expiaz - dpdnm1(1) * expmiaz
	dsvupg1 = dsvupp1(1) * expiaz - dsvupm1(1) * expmiaz
	dsvdng1 = dsvdnp1(1) * expiaz - dsvdnm1(1) * expmiaz
	dshupg1 = dshupp1(1) * expiaz - dshupm1(1) * expmiaz
	dshdng1 = dshdnp1(1) * expiaz - dshdnm1(1) * expmiaz
#endif
c
c---------------------------------------------------------------------------------
c
c***** begin frequency loop ***************
c
         do 100 ifrq = ifmin, ifmax
         freq =  ifrq * delf
         if = ifrq - ifmin + 1
         wq = twopi * freq
c
c        karg(if) = p * twopi * freq * r
         karg(if) = p * twopi * freq 
c        do 6 i = 1, nlyrs
c           qa = qpm(i)
c           qb = qsm(i)
c           alfa(i) = al0(i) * vabm(wq,tp1(i),tp2(i),qa)
c           beta(i) = be0(i) * vabm(wq,ts1(i),ts2(i),qb)
c           qa = qabm(wq,tp1(i),tp2(i),qa)
c           qb = qabm(wq,ts1(i),ts2(i),qb)
c           alfa(i) = alfa(i)*( 1. + (0.,0.5)/qa)
c           beta(i) = beta(i)*( 1. + (0.,0.5)/qb)
c6       continue
c        this statement attempts to attenuate late arrivals that
c        may wrap around, should account for them in tsynth?
c        see Bouchon code
c        t is signal length
c        freq = freq - (0.,0.5)/t
c
c        compute (7.36) complete response for all layers with buried
c        source W~[I - Rd(0s)R~]-1 Tu(0s) [I-Rd(sL)Ru(fs]-1
c        d{v,r,t}{p,s}up is above
c        d{v,r,t}{p,s}dn is same*Rd(sL)
c        after disptf, (7.36) is complete except mult by Sigma(u,d)
c
#ifdef DIFFSEIS
         call disptf(pc,freq,isrcl,ldepth,1,irlyr,lrecdep,nlyrs)
#endif
#ifndef DIFFSEIS
         call disptf(pc,freq,isrcl,ldepth,nd,irlyr,lrecdep,nlyrs)
#endif
c        write(9) dvpup,dvsup,drpup,drsup,dtsup,
c    *            dvpdn,dvsdn,drpdn,drsdn,dtsdn
#ifndef DIFFSEIS
#ifndef MULTIDEP
         dvpupf(if) = dvpup(1)
         dvsupf(if) = dvsup(1)
         drpupf(if) = drpup(1)
         drsupf(if) = drsup(1)
         dtsupf(if) = dtsup(1)

         dvpdnf(if) = dvpdn(1)
         dvsdnf(if) = dvsdn(1)
         drpdnf(if) = drpdn(1)
         drsdnf(if) = drsdn(1)
         dtsdnf(if) = dtsdn(1)
#endif
#ifdef MULTIDEP
	 do 99 id = 1, nd
         dvpupf(if,id) = dvpup(id)
         dvsupf(if,id) = dvsup(id)
         drpupf(if,id) = drpup(id)
         drsupf(if,id) = drsup(id)
         dtsupf(if,id) = dtsup(id)

         dvpdnf(if,id) = dvpdn(id)
         dvsdnf(if,id) = dvsdn(id)
         drpdnf(if,id) = drpdn(id)
         drsdnf(if,id) = drsdn(id)
         dtsdnf(if,id) = dtsdn(id)
99       continue
#endif
#endif
#ifdef DIFFSEIS
         dvpupf(if,1) = dvpup(1)
         dvsupf(if,1) = dvsup(1)
         drpupf(if,1) = drpup(1)
         drsupf(if,1) = drsup(1)
         dtsupf(if,1) = dtsup(1)

         dvpdnf(if,1) = dvpdn(1)
         dvsdnf(if,1) = dvsdn(1)
         drpdnf(if,1) = drpdn(1)
         drsdnf(if,1) = drsdn(1)
         dtsdnf(if,1) = dtsdn(1)

	 call deldisp(p,freq,isrcl,ldepth,nlyrs)
	 do 99 id = 1, nlyrs
         dvpupf(if,id+1) = dvpup(id)
         dvsupf(if,id+1) = dvsup(id)
         drpupf(if,id+1) = drpup(id)
         drsupf(if,id+1) = drsup(id)
         dtsupf(if,id+1) = dtsup(id)

         dvpdnf(if,id+1) = dvpdn(id)
         dvsdnf(if,id+1) = dvsdn(id)
         drpdnf(if,id+1) = drpdn(id)
         drsdnf(if,id+1) = drsdn(id)
         dtsdnf(if,id+1) = dtsdn(id)
99       continue

#endif
c
100      continue
c
c***** end freq loop for structure ***************
c
c---------------------------------------------------------------------------------
c
c***** begin frequency loops for sources **********
c      access the multi-dimensional arrays sequentially
c      for virtual memory/cached systems
c
c
c        find responses for different sources, mult by Sigma(u,d)
c        to complete (7.36)
c
c
#ifndef DIFFSEIS
#ifdef MULTIDEP
	   do 150 id = 1, nd
#endif
#endif
#ifdef DIFFSEIS
	   do 150 id = 1, nlyrs+1
#endif
#ifdef MULTISRC
	 do 101 ifrq = ifmin, ifmax
	 if = ifrq - ifmin + 1
	 u0(if,1 ID)=dvpupf(if ID)*pup(1)+dvsupf(if ID)*svup(1)
     *         +dvpdnf(if ID)*pdn(1)+dvsdnf(if ID)*svdn(1)
	 w0(if,1 ID)=-drpupf(if ID)*pup(1)-drsupf(if ID)*svup(1)
     *          -drpdnf(if ID)*pdn(1)-drsdnf(if ID)*svdn(1)
c	 tn(if,1 ID)= zero
101      continue
c
	 do 102 ifrq = ifmin, ifmax
	 if = ifrq - ifmin + 1
	 u0(if,2 ID)=(-dvpupf(if ID)*pup(2)-dvsupf(if ID)*svup(2)
     *          -dvpdnf(if ID)*pdn(2)-dvsdnf(if ID)*svdn(2))
	 w0(if,2 ID)=(drpupf(if ID)*pup(2)+drsupf(if ID)*svup(2)
     *          +drpdnf(if ID)*pdn(2)+drsdnf(if ID)*svdn(2))
	 tn(if,2 ID)=(-dtsupf(if ID)*shup(2)-dtsdnf(if ID)*shdn(2))
102      continue
c
	 do 103 ifrq = ifmin, ifmax
	 if = ifrq - ifmin + 1
	 u0(if,3 ID)=(dvpupf(if ID)*pup(3)+dvsupf(if ID)*svup(3)
     *          +dvpdnf(if ID)*pdn(3)+dvsdnf(if ID)*svdn(3))
	 w0(if,3 ID)=(drpupf(if ID)*pup(3)+drsupf(if ID)*svup(3)
     *          +drpdnf(if ID)*pdn(3)+drsdnf(if ID)*svdn(3))
	 tn(if,3 ID)=(-dtsupf(if ID)*shup(3)-dtsdnf(if ID)*shdn(3))
103      continue
c
	 do 104 ifrq = ifmin, ifmax
	 if = ifrq - ifmin + 1
	 u0(if,4 ID)=dvpupf(if ID)*pup(4)+dvsupf(if ID)*svup(4)
     *          +dvpdnf(if ID)*pdn(4)+dvsdnf(if ID)*svdn(4)
	 w0(if,4 ID)=-drpupf(if ID)*pup(4)-drsupf(if ID)*svup(4)
     *          -drpdnf(if ID)*pdn(4)-drsdnf(if ID)*svdn(4)
	 tn(if,4 ID)=dtsupf(if ID)*shup(4)+dtsdnf(if ID)*shdn(4)
104       continue
c
	 do 105 ifrq = ifmin, ifmax
	 if = ifrq - ifmin + 1
	 u0(if,5 ID)=(dvpupf(if ID)*pup(5)+dvsupf(if ID)*svup(5)
     *          +dvpdnf(if ID)*pdn(5)+dvsdnf(if ID)*svdn(5))
	 w0(if,5 ID)=(drpupf(if ID)*pup(5)+drsupf(if ID)*svup(5)
     *          +drpdnf(if ID)*pdn(5)+drsdnf(if ID)*svdn(5))
	 tn(if,5 ID)=(-dtsupf(if ID)*shup(5)-dtsdnf(if ID)*shdn(5))
105      continue
c
	 do 106 ifrq = ifmin, ifmax
	 if = ifrq - ifmin + 1
	 u0(if,6 ID)=dvpupf(if ID)*pup(6)+dvsupf(if ID)*svup(6)
     *          +dvpdnf(if ID)*pdn(6)+dvsdnf(if ID)*svdn(6)
	 w0(if,6 ID)=-drpupf(if ID)*pup(6)-drsupf(if ID)*svup(6)
     *          -drpdnf(if ID)*pdn(6)-drsdnf(if ID)*svdn(6)
	 tn(if,6 ID)=dtsupf(if ID)*shup(6)+dtsdnf(if ID)*shdn(6)
106      continue
#endif
#ifdef PNTFRC
c
c  point force responses
c
	 do 110 ifrq = ifmin, ifmax
	 if = ifrq - ifmin + 1
	 u0f(if,1 ID)=(dvpupf(if ID)*fpup(1)+dvsupf(if ID)*fsvup(1)
     *          +dvpdnf(if ID)*fpdn(1)+dvsdnf(if ID)*fsvdn(1))
	 w0f(if,1 ID)=(-drpupf(if ID)*fpup(1)-drsupf(if ID)*fsvup(1)
     *          -drpdnf(if ID)*fpdn(1)-drsdnf(if ID)*fsvdn(1))
c	 tnf(if,1 ID)=(0.,0.)
110      continue
c
	 do 111 ifrq = ifmin, ifmax
	 if = ifrq - ifmin + 1
	 u0f(if,2 ID)=(dvpupf(if ID)*fpup(2)+dvsupf(if ID)*fsvup(2)
     *          +dvpdnf(if ID)*fpdn(2)+dvsdnf(if ID)*fsvdn(2))
	 w0f(if,2 ID)=(drpupf(if ID)*fpup(2)+drsupf(if ID)*fsvup(2)
     *          +drpdnf(if ID)*fpdn(2)+drsdnf(if ID)*fsvdn(2))
	 tnf(if,2 ID)=+(-dtsupf(if ID)*fshup(2)-dtsdnf(if ID)*fshdn(2))
111      continue
c
	 do 112 ifrq = ifmin, ifmax
	 if = ifrq - ifmin + 1
	 u0f(if,3 ID)=(dvpupf(if ID)*fpup(3)+dvsupf(if ID)*fsvup(3)
     *          +dvpdnf(if ID)*fpdn(3)+dvsdnf(if ID)*fsvdn(3))
	 w0f(if,3 ID)=(drpupf(if ID)*fpup(3)+drsupf(if ID)*fsvup(3)
     *          +drpdnf(if ID)*fpdn(3)+drsdnf(if ID)*fsvdn(3))
	 tnf(if,3 ID)=+(-dtsupf(if ID)*fshup(3)-dtsdnf(if ID)*fshdn(3))
112      continue
#endif
#ifdef SNGLSRC
c
c
c        The General Source, frequency only assembly
c
	 do 120 i = ifmin, ifmax
	 if = i - ifmin + 1
#ifdef DIFFSEIS
         if ( id .ne. srclyr+1 ) then
#endif
	 u0fg0(if ID) = (dvpupf(if ID)*pupg0+dvsupf(if ID)*svupg0
     *          +dvpdnf(if ID)*pdng0+dvsdnf(if ID)*svdng0)
	 u0fg1(if ID) = (dvpupf(if ID)*pupg1+dvsupf(if ID)*svupg1
     *          +dvpdnf(if ID)*pdng1+dvsdnf(if ID)*svdng1)
	 w0fg1(if ID) = (-drpupf(if ID)*pupg0-drsupf(if ID)*svupg0
     *          -drpdnf(if ID)*pdng0-drsdnf(if ID)*svdng0)
	 w0fg0(if ID) = (drpupf(if ID)*pupg1+drsupf(if ID)*svupg1
     *          +drpdnf(if ID)*pdng1+drsdnf(if ID)*svdng1)
	 tnfg1(if ID) = (dtsupf(if ID)*shupg0+dtsdnf(if ID)*shdng0)
	 tnfg0(if ID) =(-dtsupf(if ID)*shupg1-dtsdnf(if ID)*shdng1)
#ifdef DIFFSEIS
         else 
	 u0fg0(if ID) = (dvpupf(if ID)*dpupg0+dvsupf(if ID)*dsvupg0
     *          +dvpdnf(if ID)*dpdng0+dvsdnf(if ID)*dsvdng0)
	 u0fg1(if ID) = (dvpupf(if ID)*dpupg1+dvsupf(if ID)*dsvupg1
     *          +dvpdnf(if ID)*dpdng1+dvsdnf(if ID)*dsvdng1)
	 w0fg1(if ID) = (-drpupf(if ID)*dpupg0-drsupf(if ID)*dsvupg0
     *          -drpdnf(if ID)*dpdng0-drsdnf(if ID)*dsvdng0)
	 w0fg0(if ID) = (drpupf(if ID)*dpupg1+drsupf(if ID)*dsvupg1
     *          +drpdnf(if ID)*dpdng1+drsdnf(if ID)*dsvdng1)
	 tnfg1(if ID) = (dtsupf(if ID)*dshupg0+dtsdnf(if ID)*dshdng0)
	 tnfg0(if ID) =(-dtsupf(if ID)*dshupg1-dtsdnf(if ID)*dshdng1)
	 endif
#endif
120      continue
#endif
#ifdef MULTIDEP
150      continue
#endif
c---------------------------------------------------------------------------------
c
c
c****** end frequency loops for simple sources ***************
c
c
c
c        karg = omega*p*r
c        start range loop
c
c        apply slowness window taper to hankel functions
c
	pwl = 0.5*(sin((p-pc1)*pr1)+1.)
	pwr = 0.5*(sin((pc2-p)*pr2)+1.)
c
c***** Loop over Ranges
	do 40 ir=1,nr
	 r = rh(ir)
	 c1 = exp( (0.,-1.) * twopi * fmin * p * r )
	 cd = exp( (0.,-1.) * twopi * delf * p * r )
	 do 45 i=1,nffpts
45        kargh(i) = karg(i) * r
	 call hanklf( 0., hnklf0, kargh, nffpts )
	 call hanklf( 1., hnklf1, kargh, nffpts )
	 if(p .lt. plc) then
	   do 46 if = 1, nffpts
		hnklf0(if) = hnklf0(if) * pwl
		hnklf1(if) = hnklf1(if) * pwl
46       continue
	 endif
	 if(p .gt. puc) then
	    do 47 if = 1, nffpts
		hnklf0(if) = hnklf0(if) * pwr
		hnklf1(if) = hnklf1(if) * pwr
47       continue
	 endif
#ifndef DIFFSEIS
#ifdef MULTIDEP
c
c***** Loop over Depths
c
	 do 35 id = 1, nd
#endif
#endif
#ifdef DIFFSEIS
c
c***** Loop over Depths
c
	 do 35 id = 1, nlyrs+1
#endif
#ifdef MULTISRC
c
c***** Loop over Sources
c
         do 21 j = 1, nsrc
c
         c1 = exp( (0.,-1.) * twopi * fmin * p * r )
         do 200 i = ifmin, ifmax
          if = i - ifmin + 1
          if ( j .eq. 3 .or. j .eq. 5) then
	   u0t(if,j ID,ir) = u0t(if,j ID,ir) +  p * u0(if,j ID) * hnklf1(if) * c1
	   w0t(if,j ID,ir) = w0t(if,j ID,ir) +  p * w0(if,j ID) * hnklf0(if) * c1
	   tnt(if,j ID,ir) = tnt(if,j ID,ir) +  p * tn(if,j ID) * hnklf0(if) * c1
           else
	   u0t(if,j ID,ir) = u0t(if,j ID,ir) +  p * u0(if,j ID) * hnklf0(if) * c1
	   w0t(if,j ID,ir) = w0t(if,j ID,ir) +  p * w0(if,j ID) * hnklf1(if) * c1
	   tnt(if,j ID,ir) = tnt(if,j ID,ir) +  p * tn(if,j ID) * hnklf1(if) * c1
            endif
          c1 = c1 * cd
200      continue
#ifdef PNTFRC
c
c   Point Force Sources
c
         if ( ( j .ge. 1 ) .and. ( j .le. 3 ) ) then
         c1 = exp( (0.,-1.) * twopi * fmin * p * r )
         do 201 i = ifmin, ifmax
          if = i - ifmin + 1
            if ( j .eq. 1 ) then
	    u0ft(if,j ID,ir) = u0ft(if,j ID,ir) +  p * u0f(if,j ID) * hnklf0(if) * c1
	    w0ft(if,j ID,ir) = w0ft(if,j ID,ir) +  p * w0f(if,j ID) * hnklf1(if) * c1
	    tnft(if,j ID,ir) = tnft(if,j ID,ir) +  p * tnf(if,j ID) * hnklf1(if) * c1
            else
	    u0ft(if,j ID,ir) = u0ft(if,j ID,ir) +  p * u0f(if,j ID) * hnklf1(if) * c1
	    w0ft(if,j ID,ir) = w0ft(if,j ID,ir) +  p * w0f(if,j ID) * hnklf0(if) * c1
	    tnft(if,j ID,ir) = tnft(if,j ID,ir) +  p * tnf(if,j ID) * hnklf0(if) * c1
            endif
          c1 = c1 * cd
201      continue
        endif
#endif
c
c
21      continue
c
c***** End of Loop over sources
c
#endif
#ifdef SNGLSRC
c
c        The General Source, complete assembly, using source 1
c
	 j = 1
         c1 = exp( (0.,-1.) * twopi * fmin * p * r )
         do 202 i = ifmin, ifmax
          if = i - ifmin + 1
         u0t(if ID,ir) = u0t(if ID,ir) + (u0fg0(if ID) * hnklf0(if) +
     *                   u0fg1(if ID) * hnklf1(if)) * p * c1 
         w0t(if ID,ir) = w0t(if ID,ir) + (w0fg0(if ID) * hnklf0(if) +
     *                   w0fg1(if ID) * hnklf1(if)) * p * c1 
         tnt(if ID,ir) = tnt(if ID,ir) + (tnfg0(if ID) * hnklf0(if) +
     *                   tnfg1(if ID) * hnklf1(if)) * p * c1
          c1 = c1 * cd
202      continue
#endif
#ifndef DIFFSEIS
#ifdef MULTIDEP
35    continue
#endif
#endif
#ifdef DIFFSEIS
35    continue
#endif
c
40    continue
c 
c***** End of Loop over Ranges
c
c
#ifdef PARALLELP4
      if ( mynod .eq. 0 ) then
        if (mod(ip,int((npmax-npmin)/12.0+.5)) .eq. 0 ) then
          write(iounit,*) 'slowness ',ip,' from ', npmin,' to ', npmax
        endif
      endif
c      write(iounit,*) ' node, ip, p ', mynod, ip, p
c      call p4flush
#endif
#ifdef PARALLELP4
        if ( mynod .eq. 0 ) then
#endif
        if (mod(ip,int((npmax-npmin)/12.0+.5)) .eq. 0 ) then
          write(iounit,*) 'slowness ',ip, npmin, npmax
        endif
#ifdef PARALLELP4
        endif
#endif
 7    continue
c
c****** end slowness loop *******************
c
C
C=====================================================
C End of slave sloweness synthetic loop
C Use p4globop with p4fltsumop 
C to total the results from all slownesses into
C Grand Total
C call  p4globop(...,p4fltsumop,...)
C=====================================================
C Master does the final weighting and i/o to
C file for results
C=====================================================
#ifdef PARALLELP4
      U0TM = 100000
      W0TM = 200000
      TNTM = 300000
      U0TF = 400000
      W0TF = 500000
      TNTF = 600000
      Nelem = 2 * ( ifmax - ifmin + 1 )
c     write(iounit,*) 'Node#, uot,w0t,tnt ', mynod,ifmin,u0t(5,1,1,1),w0t(5,1,1,1),tnt(5,1,1,1)
c     call p4flush
      do 1050 ir = 1, nr
#ifndef PARALLELP4
#ifdef MULTIDEP
      do 1031 id = 1, nd
#endif
#endif

#ifdef PARALLELP4
#ifdef MULTIDEP
#ifndef DIFFSEIS
      do 1031 id = 1, nd
#endif
#ifdef DIFFSEIS
      do 1031 id = 1, nlyrs+1
#endif
#endif
#endif
      do 1031 jj = 1, nsrc
#ifdef SNGLSRC
      call p4globop(U0TM,u0t(ifmin+1 ID,ir),Nelem,FLTSIZ,p4fltsumop,P4FLT,retcod)
      call p4globop(W0TM,w0t(ifmin+1 ID,ir),Nelem,FLTSIZ,p4fltsumop,P4FLT,retcod)
      call p4globop(TNTM,tnt(ifmin+1 ID,ir),Nelem,FLTSIZ,p4fltsumop,P4FLT,retcod)
      U0TM = U0TM + 1
      W0TM = W0TM + 1
      TNTM = TNTM + 1
#endif
#ifdef MULTISRC
      call p4globop(U0TM,u0t(ifmin+1,jj ID,ir),Nelem,FLTSIZ,p4fltsumop,P4FLT,retcod)
      call p4globop(W0TM,w0t(ifmin+1,jj ID,ir),Nelem,FLTSIZ,p4fltsumop,P4FLT,retcod)
      call p4globop(TNTM,tnt(ifmin+1,jj ID,ir),Nelem,FLTSIZ,p4fltsumop,P4FLT,retcod)
      U0TM = U0TM + 1
      W0TM = W0TM + 1
      TNTM = TNTM + 1
#ifdef PNTFRC
      call p4globop(U0TF,u0ft(ifmin+1,jj ID,ir),Nelem,FLTSIZ,p4fltsumop,P4FLT,retcod)
      call p4globop(W0TF,w0ft(ifmin+1,jj ID,ir),Nelem,FLTSIZ,p4fltsumop,P4FLT,retcod)
      call p4globop(TNTF,tnft(ifmin+1,jj ID,ir),Nelem,FLTSIZ,p4fltsumop,P4FLT,retcod)
      U0TF = U0TF + 1
      W0TF = W0TF + 1
      TNTF = TNTF + 1
#endif
#endif
1031  continue
1050  continue
c     write(iounit,*) 'Node#, Globop counts: ', mynod,U0TM,W0TM,TNTM,U0TF,W0TF,TNTF
c     write(iounit,*) 'Node#, uot,w0t,tnt ', mynod,ifmin,u0t(5,1,1,1),w0t(5,1,1,1),tnt(5,1,1,1)
c     call p4flush
#endif
C
C=====================================================
C
c     Filon integration (p.179)
c
#ifdef PARALLELP4
      if( mynod .eq. 0 ) then
#endif
      do 50 ir = 1,nr
      r = rh(ir)
#ifndef DIFFSEIS
#ifdef MULTIDEP
      do 31 id = 1, nd
#endif
#endif
#ifdef DIFFSEIS
      do 31 id = 1, nlyrs+1
#endif
      do 31 jj = 1, nsrc
      do 30 i = ifmin, ifmax
         freq = twopi * i * delf
         if ( i .ne. 0 ) then
           gfilon = 2.*( 1. - cos(freq*r*delp))/(freq*freq*r*r*delp)
          else
           gfilon = delp
         endif
c
#ifdef SNGLSRC
#ifndef DIFFSEIS
         u0ts = u0t(i+1 ID,ir)*gfilon
         w0ts = w0t(i+1 ID,ir)*gfilon
         tnts = tnt(i+1 ID,ir)*gfilon
#endif
#ifdef DIFFSEIS
         if ( id .gt. 1 ) then
     	    u0ts = ( u0t(i+1 ID,ir) - u0t(i+1,1,ir) ) * gfilon / diffnorm(id-1)
            w0ts = ( w0t(i+1 ID,ir) - w0t(i+1,1,ir) ) * gfilon / diffnorm(id-1)
            tnts = ( tnt(i+1 ID,ir) - tnt(i+1,1,ir) ) * gfilon / diffnorm(id-1)
   	  else
	    u0ts = u0t(i+1 ID,ir)*gfilon
	    w0ts = w0t(i+1 ID,ir)*gfilon
	    tnts = tnt(i+1 ID,ir)*gfilon
 	endif
#endif
         write(7) u0ts,w0ts,tnts
#endif
#ifdef MULTISRC
         u0ts = u0t(i+1,jj ID,ir)*gfilon
         w0ts = w0t(i+1,jj ID,ir)*gfilon
         tnts = tnt(i+1,jj ID,ir)*gfilon
         write(7) u0ts,w0ts,tnts
#ifdef PNTFRC
         if ( ( jj .ge. 1 ) .and. ( jj .le. 3 ) ) then
           u0ts = u0ft(i+1,jj ID,ir)*gfilon
           w0ts = w0ft(i+1,jj ID,ir)*gfilon
           tnts = tnft(i+1,jj ID,ir)*gfilon
           write(10) u0ts,w0ts,tnts
         endif
#endif
#endif
   30 continue
   31 continue
   50 continue
c
c
c     close( 9 )
      close( 7 )
#ifdef PNTFRC
      close( 10 )
#endif
#ifdef PARALLELP4
      endif
      call p4cleanup()
#endif
c
      stop
      end

