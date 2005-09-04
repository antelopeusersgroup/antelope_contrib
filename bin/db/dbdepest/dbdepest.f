c estimates depth to a single interface from an input receiver function
c  GAA 11/93
C   GAA 1/03  upgraded for Antelope 4.3u
	parameter (NMAX=16386,NMAX2=2*NMAX)
	character*80 argu, pfile

	dimension datin(NMAX),dat(NMAX),syn(NMAX2),res(NMAX),
     *    synsave(NMAX2)
	complex zfilt(NMAX),tranf(NMAX)
	real*8 alphm(10),betam(10),rhom(10),thkm(10)
	real*8 vp2,vs2,rho2,phvel
	character*1 astar(2), cdum
	character*80 atest

c DB stuff

	integer db(4), dbrf(4)
	real*8 t0_req, t1_req, t0, t1, srate
	character*10 sta, chan
	real zero
	include 'db.i'
c	include 'coords.i'

	equivalence(syn(1),tranf(1))

        data pi/3.141592653/
	data vp2/8.0d0/,vs2/4.62d0/,rho2/3.3d0/
	data alpbasin/3.9/,rhobasin/2.4/
c	data betbasin/2.3/
	data astar/" ","*"/,cdum/"\0"/
	data luniasp/21/, lunsyn/22/, lundat/23/

	write (0,*) "  DBDEPEST:  version 01.01.19GAA "

	zero = 0.
	izero = 0
	dpds = sqrt(3.0)
	dpdssed = sqrt(3.0)
c read command-line arguments
	if (iargc().lt.13) then
		write(0,*) 
     -	"Usage: depest z1 z2 dz Vcrust Fmin Fmax Delta Tfit Zbasin VpVs 
     -tshift inwf_file subset_rf [depest.pf]"
	    write(0,*) " subset_rf: dbsubset string to select desired RF"
	    write(0,*) "[depest.pf] can reset: vpm, vsm, rhom (mantle),"
 	    write(0,*) "    vpb,vsb,rhob (top-layer), vpvs, vpvssed"
c23456789012345678901234567890123456789012345678901234567890123456789012
c	    write(0,*) "NOTE:  Ain is now Del (distance, degrees)"

		stop
	end if
	call getarg(1,argu)
	read(argu,1000) z1
1000	format(f10.5)
	call getarg(2,argu)
	read(argu,1000) z2
	call getarg(3,argu)
	read(argu,1000) delz
	call getarg(4,argu)
	read(argu,1000) alphcr
	call getarg(5,argu) 
	read(argu,1000) fmin
	call getarg(6,argu) 
	read(argu,1000) fmax
	call getarg(7,argu) 
	read(argu,1000) delta
	call getarg(8,argu)
	read(argu,1000) tfitmax
	call getarg(9,argu)
	read(argu,1000) zbasin
	call getarg(10,argu)
	read(argu,1000) dpdsin
	call getarg(11,argu)
	read(argu,*) tshift
	call getarg(13,atest)
	call getarg(12,argu)
	lb=lnblnk(argu)+1
	argu(lb:lb)='\0'
	if (iargc().gt.13) then
	  call getarg(14, pfile)
	  call readpf(pfile, vp2,vs2,rho2,alpbasin,betbasin,rhobasin,
     -		dpds, dpdssed)

	endif

c constants (uses my GETIASP for now)
	rpd = pi/180.
	call readiasp(luniasp)
	zsrc = 33.
	call getiasp(delta, zsrc, pt,st,dpdx,dpddep,dsdx,dsddep)
	raypar = dpdx/111.19
	phvel = 1./raypar
c	ainc = asin(raypar*vp2)
c	phvel = vp2/sin(ainc)

c Vp/Vs:  if there's a basin dpds refers to basin, otherwise to crust
	if (dpdsin.gt.0.1) then
	  if (zbasin.le.0.) then 
	    dpds = dpdsin
	  else
	    dpdssed = dpdsin
	  endif
	endif
	betacr= alphcr/dpds
	if (zbasin.gt.0.) then
	    nl = 3
	    alphm(1) = alpbasin
	    betam(1) = alpbasin/dpdssed
	    rhom(1) = rhobasin
	    thkm(1) = zbasin
	else
	    nl = 2
	end if
	nlcr = nl - 1
	alphm(nlcr) = alphcr
	betam(nlcr) = betacr
c rough fit to Nafe-Drake
	rhocr = 2.7 + 0.2*(alphcr - 6.0)
	rhom(nlcr) = rhocr
	zupper = 0.
	if (nlcr.gt.1) then
	  do i=1, nlcr-1
	    zupper = zupper + thkm(i)
	  end do
	  if (zupper.gt.z2) then
	    write (0,*) " ERROR:  Seds thicker than z2"
	    stop
	  else if (zupper.gt.z1) then
	    write (0,*) "Warning: resetting z1 to ", zupper
	    z1 = zupper + 0.1
	  endif
	end if

c open db and get time series
	if ( dbopen_table ( argu, "r", db ) .lt. 0 )
     *         call die ( 0, "Can't open wfdisc table" )
	call dbsubset(dbrf, db, atest(1:lnblnk(atest)), "\0")
	call dbquery(dbrf, dbRECORD_COUNT, n)
	if (n.le.0) call die (0, "No records found")
	dbrf(4) = 0
c--- read wfdisc line
	if( dbgetv ( dbrf, "",
     * 	  "sta", sta, "chan", chan, "time", t0_req, "samprate", srate,
     *		"nsamp", numdb, 0) .lt. 0)
     *    call complain (0, "dbgetv error" )
	write (0,*) "station ",sta(1:lnblnk(sta)), 
     -		" chan: ",chan(1:lnblnk(chan))," samprate ",srate
	
	if (numdb.gt.NMAX) then
	    write (0,*) "Too many points in wfdisc, just using ",NMAX
	    numdb = NMAX
	endif
	t1_req=t0_req + float(numdb-1)/srate
	delt = 1./srate
c--- get data (last 2 entries are "fill" and fill-value
	call trgetwf ( dbrf, cdum, datin, NMAX, t0_req, t1_req,
     *         t0, t1, npts, izero, zero )
	if ( npts .lt. 1 ) call die ( 0, "Couldnt read waveform")
	write(0,*) " Read in ",npts," data points starting ",t0
	numdb = npts

c-- avoid wraparound
	if (npts.lt.NMAX/2) then
	  numdb=2*npts
	  do i=npts+1,numdb
		datin(i) = 0.
	  end do
	endif

c -- construct 3-pole zero-phase butterworth filter and apply to data
	if (fmax.gt.0..or.fmin.gt.0.) then
     	    call mkbutter3(delt,df,numdb,nfilt,fmin,fmax,zfilt)
	    call filt(delt,df,numdb,nfilt,zfilt,datin,dat)
	else
	    do i=1,numdb
		dat(i) = datin(i)
	    end do
	    do i=numdb,nfilt
		dat(i) = 0.
	    end do
	end if

c debug output
	open(lundat,file="dat.filt")
	do i=1,numdb
	    write(lundat,*) float(i-1)*delt, dat(i)
	end do
	close(lundat)

c start up statistics
	    drms = 0.
	    nfit = min(numdb,1 + int(tfitmax/delt))
	    do i=1,nfit
	      drms = drms + dat(i)*dat(i)
	    end do
	    drms = sqrt(drms/float(nfit-1))

c -- solve the system
	nz = (z2-z1)/delz + 1
	zz = z1 - delz
	n2 = nfilt/2 
	tleng = float(nfilt-1)*delt

c modify zfilt by a phase-shift
	ff = df
	do i=2,n2
	    zfilt(i)=zfilt(i)*cexp(cmplx(0.,-2.*pi*df*float(i)*tshift))
	enddo

	thkm(nlcr) = z1 - zupper
	do i=1, nl-1
	  write (0,2000) alphm(i), betam(i), rhom(i), thkm(i)
	enddo
	write (0,2000) vp2, vs2, rho2, 0.
2000	format("Vp: ",f6.3, " Vs: ", f6.3, " rho:", f5.3, 
     -		" Thk:", f7.3)

	do k=1,nz
	    zz = zz + delz
	    thkm(nlcr) = zz - zupper

	    do i=1,nfilt
		tranf(i) = cmplx(0.,0.)
	    end do
          call rayle (n2,tleng,vp2,vs2,rho2,phvel, alphm,betam,rhom,
     2                   thkm, nl,tranf,3)
	    if (fmin.gt.0..or.fmax.gt.0.) then
	      do i=1,n2
		tranf(i) = tranf(i)*zfilt(i)
	      end do
	    end if
c force real output, and set Nyquist and DC response to zero

	    do i=2,n2
	 	tranf(nfilt-i+2)=conjg(tranf(i))
	    end do
            tranf(n2)=cmplx(real(tranf(n2+1)), 0.0)
            tranf(1)=cmplx(0.,0.)
c   recover time-domain response by IFFT, then normalize by 1/dt (?)
c  note --- this if forward:    call fast (x,n)
            call fsst (syn,nfilt)


c --- set residuals
c  rrms:  un-normalized rms of misfit
c  nrms:  rrms/rms of data
c  rms2:  rms misfit where data and synthetic are each normalized 
c ** USE rms2 FOR BEST FIT HERe!
	    rrms = 0.
	    srms = 0.
	    do i=1,nfit
	      rrms = rrms + (dat(i)-syn(i))**2
	      srms = srms + syn(i)*syn(i)
	    end do
	    rrms = sqrt(rrms/float(nfit-1))
	    srms = sqrt(srms/float(nfit-1))
	    rms2 = 0.
	    do i=1,nfit
		rms2 = rms2 + (dat(i)/drms - syn(i)/srms)**2
	    end do
	    rms2 = sqrt(rms2/float(nfilt-1))
c  4/15/98:  next line fixed to not re-sqrt, after AFS suggestion
	    rmsn = rrms/drms
	    write(*,*) zz,rrms,rmsn,rms2
	    res(k) = rrms
	    if (k.eq.1.or.rms2.lt.rmsmin) then
	      do i=1,numdb
		synsave(i) = syn(i)*drms/srms
	      end do
	      rmsmin = rms2
	    end if
	end do

c debug output of best-fitting synthetic
	open(lunsyn,file="syn.filt")
	do i=1,numdb
	 	write(lunsyn,*) float(i-1)*delt, synsave(i)
	end do
	close(lunsyn)

	stop
	end
