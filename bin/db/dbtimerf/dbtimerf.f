c	Modified from: timedecm2.f
c	  (1) stupid questions removed GA 3/96
c	  (2) made a db-routine

c  sample time-domain deconvolution by damped least-squares inversion
c  this version does gauss-jordan
c  version does multiple stations simultaneously:  normalization by pwr in H
c --- GAA, 9/19/91
c     modified by afs 1/8/92

	parameter (NOUTMAX=512, NOUTMAX1=513)
	parameter (NDATMAX=2048, NBUFMAX=25000)
	dimension amat(NDATMAX,NOUTMAX),dat(NDATMAX)
c -- undisturbed copies of amat, dat:
	dimension amat2(NDATMAX,NOUTMAX), dat2(NDATMAX)
c --- thisone for dumping "A" matrix
c	dimension indump(NOUTMAX)
c	character*22 adump 
c --- gauss-jordan I/O matricies
	dimension ata(NOUTMAX,NOUTMAX)
	dimension gjdat(NOUTMAX,NOUTMAX1)
	real*8 ataji, diagmax, gtd
c  resolution, error output matricies
	dimension resmat(NOUTMAX,NOUTMAX)
	dimension varout(NOUTMAX), resout(NOUTMAX)
c  spread-functions
	dimension sprout(NOUTMAX),sprbg(NOUTMAX)
c  Data buffers (!) -- from ahiof lib
        real*4 buf1(NBUFMAX),buf2(NBUFMAX),buf3(NBUFMAX) 
	
C  DB stuff
	integer db(4), dbw(4), dbo(4), dbz(4), dbr(4), tr(4)
	integer wfid_z, wfid_r
	character*64 subset_z, subset_r
	character*6 sta2, sta1, instype
	character*8 chan2, chan1
	character*32 dfile
	character*64 dir
	character*2 datatype
	real*8 t0_req, t1_req, t0, t1, t0r,  srate1, srate2
	real*8 dtime, calib
	real zero
	integer nout, izero
	include 'db.i'

	character argu*80, dbout*80, iyesno*1
	external fill
c--------------
c	data luah1/9/,luah2/10/,luahout/11/
c --- for dumping "A"
c	data adump/"ZYXWVUTSR-0+123456789*"/
	data calib/1.d0/, dir/"wfrfcn"/, izero/0/, izero2/0/
	data instype/"TDrfcn"/

	ndatamax=NBUFMAX

	print *,'dbtimerf: Solves G*f=H for f, G=Greens fcn (Z-comp)'

	if (iargc().lt.6) then
	  write(0,*) "Usage: dbtimerf dbin dbout tshift tout apm eps 
     &subset_z subset_r [kres]"
	  write(0,*) "tshift = shift of R rel. to G"
	  write(0,*) "tout = time length for output (should be <Ndata)"
	  write(0,*) "apm = prior modl uncert.= ap1/sqrt(NOUT) [1.] "
	  write(0,*) "eps = prior thry+data uncert.= eps*RMS(R) [1.]"
	  write(0,*) "subset_z, subset_r = dbselect logic strings for",
     -		" z,r component"
	  write(0,*) "kres = 1 to o/p resolution + spread, 0 otherwise"

	  stop
	endif
	
	call getarg(1, argu)
	ierr =  dbopen(argu(1:lnblnk(argu)), "r", db)
	if (ierr.eq.dbINVALID) call die (0,"Could not open db ")
	call getarg(2, dbout)
	ierr =  dbopen(dbout(1:lnblnk(dbout)), "r+", dbo)
	if (ierr.eq.dbINVALID) call die (0,"Could not open dbout ")
	call getarg(3, argu)
	read(argu,*) ppsec
	call getarg(4, argu)
	read(argu,*) tout
	call getarg(5,argu) 
	read(argu,*) aprior1
	call getarg(6, argu)
	read(argu,*) eps
	call getarg(7, subset_z)
	call getarg(8, subset_r)
	if (iargc().gt.8) then
	  call getarg(9, argu)
	  read(argu,*) kresmat
	else
	  kresmat = 0
	endif


c---  inversion parameters (weighty weighting stuff)
	ppsec0 = ppsec
	dmom = aprior1
	if (eps.eq.0.) eps = 1.
	
c
c---open input wfdiscs
c
	call dblookup(dbw, db, "\0", "wfdisc", "\0", "\0" )
	call dbquery ( dbw, dbRECORD_COUNT, n )
	if (n.le.0) call die (0, " wfdisc empty or not found")
	call dbsubset(dbz, dbw, subset_z(1:lnblnk(subset_z)), "\0" )
	call dbquery ( dbz, dbRECORD_COUNT, nz )
	if (nz.le.0) call die (0, " z-subset empty or not found")
	call dbsubset(dbr, dbw, subset_r(1:lnblnk(subset_r)), "\0" )
	call dbquery ( dbr, dbRECORD_COUNT, nr )
	if (nr.le.0) call die (0, " r-subset empty or not found")
	if (nr.ne.nz) call die(0, " R and Z subsets don't match")
	nwfin = nz

c --- pointer to output db
	call dblookup(dbo, dbo, "\0", "wfdisc", "\0", "\0" )
	call dbquery ( dbo, dbRECORD_COUNT, n )
	write(0,*) n," records already in output db ",dbout
	nrecin = 0
	ndatin = 0
	negfin = 0
	sumwt = 0.
	sumegf = 0.
	ndata1 = 0
	ndata2 = 0

c	start reading in seismograms 

	iquitflag = 0
	do 1 iwf=1,nwfin
	  if (ndatin.ge.NDATMAX.or.iquitflag.eq.1) then
	    print *,"too much input data; truncating at rec",nrecin
	    go to 2
	  end if

c--- read header G (Z)

	dbz(4) = iwf - 1
	if( dbgetv ( dbz, "",
     * 	  "sta", sta1, "chan", chan1, 
     *    "time", t0_req, "samprate", srate1,
     *		"nsamp", ndata1, 0) .lt. 0)
     *    call complain (0, "dbgetv error" )

	   write (0,1000) sta1(1:lnblnk(sta1)), 
     -		chan1(1:lnblnk(chan1)),srate1
1000	format( "Z station ",a6," chan: ",a8," samprate ",f8.3)

	ndata1=min(ndata1, NBUFMAX)
	t1_req=t0_req + dble(ndata1-1)/srate1
	
c---read data G (Z)
c

c	call trgetwf ( dbz, 0, buf1, ndatamax, t0_req, t1_req,
c     *         t0, t1, npts, izero, izero2 )
	call readmywf(dbz, t0_req, t1_req, tr, t0, t1,
     *		npts, buf1)

	if ( npts .lt. 1 ) call die ( 0, "Couldnt read Zwaveform")
	write(0,1002) npts, t0
1002	format(" Z: read in ",i6," data points starting ",f14.3)
	ndata1=npts

c--- read header H (R)
	dbr(4) = iwf - 1
	if( dbgetv ( dbr, "",
     * 	  "sta", sta2, "chan", chan2, 
     *    "time", t0_req, "samprate", srate2,
     *		"nsamp", ndata2, 0) .lt. 0)
     *    call complain (0, "dbgetv 2 error" )

	   write (0,1001) sta2(1:lnblnk(sta2)), 
     -		chan2(1:lnblnk(chan2)),srate2
1001	format( "R station ",a6," chan: ",a8," samprate ",f8.3)

	ndata2=min(ndata2, NBUFMAX)
	t1_req=t0_req + dble(ndata2-1)/srate2
c---read data H (R)
c
c	call trgetwf ( dbr, 0, buf2, ndatamax, t0_req, t1_req,
c     *         t0r, t1, npts, izero, izero2 )
	call readmywf(dbr, t0_req, t1_req, tr, t0r, t1,
     *		npts, buf2)
	if ( npts .lt. 1 ) call die ( 0, "Couldnt read waveform")
	write(0,1003) npts, t0r
1003	format(" R: read in ",i6," data points starting ",f14.3)
	ndata2=npts

c
c--- check to see if things are similar enough to deconvolve

	if (srate2.ne.srate1) then
	  print *,'Samprates dont match; skipping'	
	  print *,'delta-G = ',srate1,' delta-H = ',srate2
	  go to 1
	end if
	delta2 = 1./srate2
	if (dabs(t0-t0r).gt.delta2) then
	  print *,'Start times dont match; skipping'	
	  print *,'t0-z = ',t0,' t0-r = ',t0r
	  go to 1
	end if

c--- get rms(h) and data weight
	rmsh=0.
	sumh=0.
	do i=1,ndata2
	  sumh=sumh+buf2(i)
	  rmsh=rmsh+buf2(i)*buf2(i)
	end do
	if (ndata2.lt.2) then
	  rmsh = abs(buf2(1))
	else
	  en=ndata2
	  rmsh=(rmsh-sumh*sumh/en)/(en-1.)
	  if (rmsh.lt.0.) rmsh=1.
	  rmsh=sqrt(rmsh)
	end if
	datwt = 1./(rmsh*eps)


c --- pre-pad H, if necessary
	if (ppsec.gt.0.) then
  	  npad = ppsec/delta2
	  t0r = t0r - npad*delta2
	  nout2 = ndata2+npad
	  do i=ndata2,1,-1
	    buf2(i+npad) = buf2(i)
	  end do
	  do i=1,npad
	    buf2(i) = 0.
	  end do
	  ndata2 = nout2
	end if

c --- check for too much data
	if (ndata2+ndatin.ge.NDATMAX) then
	  print *,'Too much data; truncating to ',NDATMAX
	  ndata2 = NDATMAX - ndatin-1
	  iquitflag = 1
	end if
	if (ndata1+ndatin.ge.NDATMAX) ndata1=NDATMAX-ndatin-1

c ---- set number of output points, on first seismogram
	if (nrecin.eq.0) then
	  nout = tout*srate2 + 1
	  if (nout.gt.NOUTMAX) nout=NOUTMAX
	  print *,nout,' points for output'
	end if

c --- build matrix to decompose (weight by rms-H)
c --- NOTE:  this should directly calculate aTa and gjdat; it would
c		be a lot more efficient

	do j=1,nout
	    if (j.gt.1.and.ndata2.gt.1) then
		jm1 = j - 1
	 	do i=1,jm1
		  ii = i + ndatin
		  if (ii.le.NDATMAX) amat(ii,j) = 0.
		end do
	    end if
	    n2 = j + ndata1 - 1
	    if (n2.gt.ndata2) n2 = ndata2
	    do i=j,n2
	      ii = i + ndatin
	      if (ii.le.NDATMAX) amat(ii,j) = buf1(i-j+1)*datwt
	    end do
	    if (n2.lt.ndata2) then
		do i=n2,ndata2
		  ii = i + ndatin
		  if (ii.le.NDATMAX) amat(ii,j) = 0.
		end do
	    end if
	end do
c--- get pwr in g*wt, to check stability
	rmsg = 0.
	do i=1,ndata1
	  rmsg=rmsg+buf1(i)*buf1(i)*datwt*datwt
	end do

c --- build data array
	do i=1,ndata2
	  ii = i + ndatin
	  dat(ii) = buf2(i)*datwt
	end do

	sumwt = sumwt + float(ndata2-1)*datwt*datwt
	sumegf = sumegf + rmsg
	negfin = negfin + ndata1
	ndatin = ndatin + ndata2
	nrecin = nrecin + 1
1	continue

c -- END OF DATA INPUT LOOP
2	continue
	wtavg = sqrt(sumwt/float(ndatin))
	egfavg = sqrt(sumegf/float(negfin))
	print *,"Average weight = ",wtavg," rms of weighted A-mat=",egfavg

	dmom = dmom/sqrt(real(nout))
	print *,"Est. prior uncert. in rfcn = ",dmom

c -- save input matricies
	do i=1,ndatin
	  dat2(i) = dat(i)
	  do j=1,nout
	    amat2(i,j) = amat(i,j)
	  end do
	end do
c
c--- calculate Cmm-1
	  cmm1 = 0.
	  if (dmom.gt.0.) cmm1=(1./dmom)**2
	  print *,"inverse variance diag Cmm-1 = ",cmm1,
     -		" A priori error = ",dmom," root-damper=",1/dmom

c--- do gauss-jordan elimination
	  print *,"Making AtA+Cm-1 and At[d|AtA] matricies....."
	  nosvd = 1

c--- build g-j matricies: ata = normal eq'ns, gjdat=[d|AtA] = data
	  diagmax = 0.
	  do i=1,nout
	    do j=1,i
	      ataji = 0.d0
	      do k=1,ndatin
		ataji=ataji+amat2(k,i)*amat2(k,j)
	      end do
	      if (kresmat.eq.1) then
	 	gjdat(j,i+1)=ataji
		gjdat(i,j+1) = ataji
	      end if
	      ata(j,i)=ataji
	      ata(i,j)=ataji
	      if (j.eq.i) ata(j,i) = ataji + cmm1
	      if (i.eq.j.and.ataji.gt.diagmax) diagmax=ataji
	    end do
	    gtd = 0.d0
	    do k=1,ndatin
	      gtd=gtd+dble(amat2(k,i))*dble(dat(k))
	    end do
	    gjdat(i,1) = gtd
c	    gjdat1(i) = gtd
	  end do
	  print *,'AtA: max-diag=',diagmax,' effective CN=',
     -		diagmax/dble(cmm1)+1.d0
c-- dump AtA matrix  
c	open (41,file="a.dump")
c	do i=1,nout
c	  do j=1,nout
c	    ii = int((ata(i,j)/diagmax)*10.d0)+11
c	    if (ata(i,j).lt.0.d0) ii=ii-1
c	    if (ata(i,j).gt.0.d0) ii=ii+1
c	    if (ii.lt.1) ii=1
c	    if (ii.gt.22) ii=22
c	    indump(j) = ii
c	  end do
c	  write(41,"(132a1)") (adump(indump(j):indump(j)),j=1,nout)
c	end do
c	close(41)

	  nout1 = 1
	  if (kresmat.eq.1) nout1=nout+1

c--- invert using numerical recipes subroutine [SVD encountered ERRORS]
c  A note on speed:  This method should be comparable to all others as long
c  as the entire inverse matrix ata-1 is desired.  If complete resolution and error
c  info is not desired, this may not be necessary.  ALso, it will crash if singular.

	  call gaussj(ata,nout,NOUTMAX,gjdat,nout1,NOUTMAX1,infogj)
	  
	  if (infogj.ne.0) then
	    print *,"GAUSSJ failed, status = ",info," **STOPPING**"
	    stop
	  else
	    print *,"GAUSSJ done, successful"
	  end if

c--- do gauss-jordan solution
	  sumbuf = 0.
	  do i=1,nout
	    buf3(i) = gjdat(i,1)
	    sumbuf = sumbuf + buf3(i)
	  end do
	print *,"Integrated STF = ",sumbuf

c--- get errors, possibly resolutions, from Gauss-jordan
c     using Total or a posteriori error estimate = inv(AtA+inv(Cm))
	  resmax = 0.
	  varmax = 0.
	  do i=1,nout
	    varout(i) = (sqrt(ata(i,i)))
	    if (varout(i).gt.varmax) varmax = varout(i)
	  end do
	  if (kresmat.eq.1) then
	    do i=1,nout
	      ip1 = i+1
	      resout(i) = (gjdat(i,ip1))
	      if (resout(i).gt.resmax) resmax=resout(i)
	      do j=1,nout
		resmat(j,i) = (gjdat(j,ip1))
	      end do
	    end do
	  end if

c--- calculate spread functions
	if (kresmat.eq.1) then
	  do i=1,nout
	    spread1 = 0.
	    spread2 = 0.
	    spread3 = 0.
	    sprout(i) = 0.
	    sprbg(i) = 0.
	    do k=1,nout
	      spread1=spread1+resmat(i,k)
	      ss2 = resmat(i,k)**2
	      spread2=spread2+ss2
	      spread3=spread3+ss2*float((i-k)*(i-k))
	    end do
	    if (spread2.gt.0.) then
c --- this spread-measure works well for sinx/x distributions
	      sprout(i)=spread1*spread1/spread2*delta2
c --- this is a Backus-Gilbert-type measure
	      sprbg(i)=sqrt(spread3/spread2)*delta2
	    end if
	  end do
	end if

c--- calculate residuals
	write (*,"('Calculating residuals...')")
	nd = ndatin
	ressum = 0.
	datsum = 0.
	do i=1,nd
	  resi = 0.
	  do j=1,nout
	    resi=resi+amat2(i,j)*buf3(j)
	  end do
	  resi = dat2(i)-resi
	  ressum = ressum + resi*resi
	  datsum = datsum + dat2(i)*dat2(i)
	end do
	ressum = ressum/sumwt
	datsum = datsum/sumwt
	fracout = ressum/datsum
	sigdav = sqrt(float(nd)/sumwt)
	print *," Final RMS Residual              = ",sqrt(ressum)
     	print *," var(res)/var(dat)               = ",fracout
  	print *," mean a priori data+theory error = ",sigdav

c correct sign for receiver-function and rotation conventions....
	do i=1,nout
	    buf3(i) = -buf3(i)
	enddo
c  write header

	call dblookup(dbo, dbo, "\0", "wfdisc", "\0", "\0" )
	chan2="tdrf\0"
	call e2h(t0, iyr, idy, ihr, imn, sec)
	dtime=(dble(iyr*1000+idy)*100d0+dble(ihr))*100.d0+dble(imn)
	dtime=dtime+dble(int(sec))/60.
	write(dfile,'(f14.2,".",a3,".",a4)') dtime,sta2(1:3),chan2(1:4)
	
	call writemywf(dbo,  sta2, chan2, 
     -		t0r, srate2, nout, calib,  
     -		instype, dir, dfile, buf3)

c--- write standard error
	chan2="terr\0"
	write(dfile,'(f14.2,".",a3,".",a4)') dtime,sta2(1:3),chan2(1:4)
	call writemywf(dbo,  sta2, chan2, 
     -		t0r, srate2, nout, calib,  
     -		instype, dir, dfile, varout)

c--- write resolution diagonals
	if (kresmat.eq.1) then
	  print *,"res-matrix output to timedec.resmat"
	  chan2="res\0"
	write(dfile,'(f14.2,".",a3,".",a3)') dtime,sta2(1:3),chan2(1:3)
	call writemywf(dbo,  sta2, chan2, 
     -		t0r, srate2, nout, calib,  
     -		instype, dir, dfile, resout)

c--- write resolution spread widths (sinc)
	  chan2="sprd\0"
	write(dfile,'(f14.2,".",a3,".",a3)') dtime,sta2(1:3),chan2(1:4)
	call writemywf(dbo,  sta2, chan2, 
     -		t0r, srate2, nout, calib,  
     -		instype, dir, dfile, sprout)

c--- write Backus-Gilbert spread
	  chan2="BGsp\0"
	write(dfile,'(f14.2,".",a3,".",a3)') dtime,sta2(1:3),chan2(1:4)
	call writemywf(dbo,  sta2, chan2, 
     -		t0r, srate2, nout, calib,  
     -		instype, dir, dfile, sprbg)

c -- output full resolution matrix in rmm-format (ASCII)
	  open(22,file="timedec.resmat")
	  nelem=nout*(nout+1)/2
	  write(22,"(5i6,f10.3)") 1,1,0,2,nelem,1.
	  write(22,"(10e11.3)") ((resmat(j,i),j=1,i),i=1,nout)
	  close(22)
	endif

	stop
	end


          subroutine fill ( db, data, i0, i1, imax, value )
          implicit none
          integer db(4)
          integer i, i0, i1, imax
          real data(imax)
          real value
           do i=i0+1, i1
             data(i) = value
	   end do
	end
