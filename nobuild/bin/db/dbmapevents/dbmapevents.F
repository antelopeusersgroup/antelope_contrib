
      program MAIN

      real*4 xlat1, xlon1, xlat2, xlon2, xmb, ymb
      real*8 evtime
      integer length
      character*80 dbnam, sta, arg, xarg, auth(20), sym(20)
      character*80 tstart, tend, dname
      real*4 symhue(20), symlit(20), symsat(20)
      parameter  (MAXEVENTS = 100000)
      integer*8 ievs(MAXEVENTS)
      integer*8 idb
      character*1 ans
c
#include "EV_f.i"
#include "dbl2.i"
c
      data  pi / 3.14159265358979323846d0 /
      data  tstart / ' ' /
      data  tend / ' ' /
      data  dname / '' /
c
      if (iargc() .lt. 3) then
	print *,
     +'usage: dbmapevents dbname {sta | orid | lat:lon} range ',
     +'                   [-c] [-d display]',
     +'                   [auth1 sym1 hue1 lit1 sat1 ...]'
	stop
      end if
c     call set_ieeehandlers
c
c    Parse command line
c
      call getarg (1, dbnam)
      call getarg (2, sta)
      do 1  i = 1, len(sta)
	if (sta(i:i) .eq. ':') go to 2
    1 continue
      xlat1 = -100.0d0
      xlon1 = -100.0d0
      go to 3
    2 continue
      read (sta(1:i-1), *, err=58) xlat1
      read (sta(i+1:ilen(sta)), *, err=58) xlon1
      if (xlon1 .gt. 180.d0) xlon1 = xlon1 - 360.0d0
      if (xlon1 .lt. -180.d0) xlon1 = xlon1 + 360.0d0
    3 continue
      call getarg (3, arg)
      do 5  i = 1, len(arg)
	if (arg(i:i) .eq. ':') go to 6
    5 continue
      read (arg, *) range
      xmax = range
      xmin = -range
      ymax = range
      ymin = -range
      go to 7
    6 continue
      read (arg(1:i-1), *, err=58) xlat2
      length = ilen(arg) 
      read (arg(i+1:length), *, err=58) xlon2
      if (xlon2 .gt. 180.d0) xlon2 = xlon2 - 360.0d0
      if (xlon2 .lt. -180.d0) xlon2 = xlon2 + 360.0d0
      if (xlat1 .gt. -100.0d0) then
        xmax =  0.5d0*(xlon2-xlon1)
        xmin = -0.5d0*(xlon2-xlon1)
        ymax =  0.5d0*(xlat2-xlat1)
        ymin = -0.5d0*(xlat2-xlat1)
        xlat1 = 0.5d0*(xlat2+xlat1)
        xlon1 = 0.5d0*(xlon2+xlon1)
      end if
    7 continue
      n = iargc()
      nauth = 0
      i = 4
      inter = 0
   66 continue
	if (i .gt. n) go to 67
	if (i .eq. 4) then
          call getarg (4, arg)
	  if (arg .eq. '-c') then
	    inter = 1
	    i = i + 1
	    go to 66
	  else if (arg .eq. '-d') then
	    i = i + 1
      	    call getarg (i, dname)
	    i = i + 1
	    go to 66
	  end if
	end if
	if (n-i .lt. 4) then
	  print *,'dbmapevents: Too few arguments'
	  stop
	end if
	nauth = nauth + 1
	call getarg (i, auth(nauth))
	call getarg (i+1, sym(nauth))
	call getarg (i+2, arg)
	read (arg, *) symhue(nauth)
	call getarg (i+3, arg)
	read (arg, *) symlit(nauth)
	call getarg (i+4, arg)
	read (arg, *) symsat(nauth)
	i = i + 5
      go to 66
   67 continue
c
c    Open database
c
      call db30create (dbnam, idb)
      if (idb .eq. 0) then
	print *,'dbmapevents: db30create error'
	stop
      end if
c
c    Read in site data and find reference station
c
      jorid = -1
      call dbgetntuples (idb, RELID_SITE, ntuples)
      if (ntuples .lt. 1) then
	go to 64
      end if
      do 55  i = 1, ntuples
	call dbgetattrs (idb, RELID_SITE, i,
     +                   ATTRID_LON, xlon,
     +                   ATTRID_LAT, xlat,
     +                   ATTRID_STA, arg,
     +			 0)
	if (sta .eq. arg) then
	  xlat1 = xlat
	  xlon1 = xlon
	  if (xlon1 .gt. 180.d0) xlon1 = xlon1 - 360.0d0
	  if (xlon1 .lt. -180.d0) xlon1 = xlon1 + 360.0d0
	  go to 56
	end if
   55 continue
   64 do 65  i = 1, len(sta)
	if (sta(i:i) .eq. ':') go to 68
   65 continue
      read (sta, *) iorid
      call dbgetntuples (idb, RELID_ORIGIN, ntuples)
      if (ntuples .lt. 1) then
	print *,'dbmapevents: No origin tuples'
	stop
      end if
      do 57  i = 1, ntuples
	call dbgetattrs (idb, RELID_ORIGIN, i,
     +                   ATTRID_LON, xlon,
     +                   ATTRID_LAT, xlat,
     +                   ATTRID_ORID, jorid,
     +                   ATTRID_TIME, evtime,
     +			 0)
	if (jorid .eq. iorid) then
	  write (sta, '(a,i4)') 'orid = ',iorid
	  xlat1 = xlat
	  xlon1 = xlon
	  if (xlon1 .gt. 180.d0) xlon1 = xlon1 - 360.0d0
	  if (xlon1 .lt. -180.d0) xlon1 = xlon1 + 360.0d0
	  go to 56
	end if
   57 continue
   58 print *,'dbmapevents: reference station or origin not found'
      stop
   68 continue
   56 continue
c
c    Set up plotting
c
      itran = 0
      xdim = 7.3
      ydim = 7.3
      xlow = 0.1
      ylow = 2.00
      nevs = 0
      x = 0.5*(xmin+xmax)
      y = 0.5*(ymin+ymax)
      istaplt = 1
      istnam = 0
      ipdeplt = 1
      ipdepth = 0
      iporid = 0
      ipmag = 1
      idcirc = 1
      ifin = 1
c
c    Make plot
c
  100 call plotit (dname, itran, xdim, ydim,  xlow, ylow,
     +                   xmax, xmin, ymax, ymin, dbnam,
     +                   sta, jorid, xlat1, xlon1, evtime,
     +                   nauth, auth, sym, symhue, symlit, symsat,
     +                   idb, nevs, ievs, iporid, ipdepth,
     +                   istaplt, istnam,
     +                   ipdeplt, ipmag, idcirc, tstart, tend)
c
c    Check for cursor entries
c
      if (inter .eq. 0) then
        call finitt
        stop
      end if
  110 call cursor (x,y,ans)
      if (ans .eq. 'q') then
        call finitt
	stop
      else if (ans .eq. 'Q') then
        call finitt
	call hdkild
	stop
      else if (ans .eq. '?') then
	write (6, '(a)') 'Key  Action'
	write (6, '(a)') ' ?   help'
	write (6, '(a)') ' q   quit and keep window'
	write (6, '(a)') ' Q   quit and destroy window'
	write (6, '(a)') ' i   small zoom in'
	write (6, '(a)') ' I   big zoom in'
	write (6, '(a)') ' o   small zoom out'
	write (6, '(a)') ' O   big zoom out'
	write (6, '(a)') ' p   pan'
	write (6, '(a)') ' E   great circle to closest event'
	write (6, '(a)') ' e   identify closest event'
	write (6, '(a)') ' s   identify closest station'
	write (6, '(a)') ' t   enter time range'
	write (6, '(a)') ' 1   orid labels on/off'
	write (6, '(a)') ' 2   depth coloring on/off'
	write (6, '(a)') ' 3   shallow depth coloring on/off'
	write (6, '(a)') ' 4   shallow depth diff coloring on/off'
	write (6, '(a)') ' 5   sta symbols on/off'
	write (6, '(a)') ' 6   mark pdevs on/off'
	write (6, '(a)') ' 7   mag size on/off'
        write (6, '(a)') ' 8   distance circles on/off'
        write (6, '(a)') ' 9   sta names on/off'
	go to 110
      else if (ans .eq. 'i') then
	xx = 0.5*(xmax - xmin)
	yy = 0.5*(ymax - ymin)
	xmax = x + 0.8*xx
	xmin = x - 0.8*xx
	ymax = y + 0.8*yy
	ymin = y - 0.8*yy
        x = 0.5*(xmin+xmax)
        y = 0.5*(ymin+ymax)
	go to 100
      else if (ans .eq. 'I') then
	xx = 0.5*(xmax - xmin)
	yy = 0.5*(ymax - ymin)
	xmax = x + 0.2*xx
	xmin = x - 0.2*xx
	ymax = y + 0.2*yy
	ymin = y - 0.2*yy
        x = 0.5*(xmin+xmax)
        y = 0.5*(ymin+ymax)
	go to 100
      else if (ans .eq. 'o') then
	xx = 0.5*(xmax - xmin)
	yy = 0.5*(ymax - ymin)
	xmax = x + 1.25*xx
	xmin = x - 1.25*xx
	ymax = y + 1.25*yy
	ymin = y - 1.25*yy
        x = 0.5*(xmin+xmax)
        y = 0.5*(ymin+ymax)
	go to 100
      else if (ans .eq. 'O') then
	xx = 0.5*(xmax - xmin)
	yy = 0.5*(ymax - ymin)
	xmax = x + 5.0*xx
	xmin = x - 5.0*xx
	ymax = y + 5.0*yy
	ymin = y - 5.0*yy
        x = 0.5*(xmin+xmax)
        y = 0.5*(ymin+ymax)
	go to 100
      else if (ans .eq. 'p') then
	xx = 0.5*(xmax - xmin)
	yy = 0.5*(ymax - ymin)
	xmax = x + xx
	xmin = x - xx
	ymax = y + yy
	ymin = y - yy
        x = 0.5*(xmin+xmax)
        y = 0.5*(ymin+ymax)
	go to 100
      else if (ans .eq. 'E') then
        write (6, '(a,$)') 'Enter offset azimuth: '
        read (5, *) az
        az = az*3.14159/180.0
	call idevent (xlat1, xlon1, x, y, nevs, ievs, 
     +                nauth, auth)
        r = sqrt(x*x+y*y)
        az = az + atan2(x,y)
        x = r*sin(az)
        y = r*cos(az)
        call line (0.0, 0.0, x, y, 0.0, 0, 0)
        go to 110
      else if (ans .eq. 'e') then
	call idevent (xlat1, xlon1, x, y, nevs, ievs, 
     +                nauth, auth)
	go to 110
      else if (ans .eq. 's') then
	call idstation (xlat1, xlon1, x, y, idb,
     +                nauth, auth)
	go to 110
      else if (ans .eq. 't') then
        write (6, '(a,$)') 'Enter start time: '
        read (5, '(a)') tstart
        write (6, '(a,$)') 'Enter end time: '
        read (5, '(a)') tend
        nevs = 0
	go to 100
      else if (ans .eq. '1') then
	if (iporid .eq. 1) then
	  write (6, '(a)') 'Turning orid labeling off'
	  iporid = 0
	else
	  write (6, '(a)') 'Turning orid labeling on'
	  iporid = 1
	end if
	go to 100
      else if (ans .eq. '2') then
	if (ipdepth .gt. 0) then
	  write (6, '(a)') 'Turning depth coloring off'
	  ipdepth = 0
	else
	  write (6, '(a)') 'Turning depth coloring on'
	  ipdepth = 1
	end if
	go to 100
      else if (ans .eq. '3') then
	if (ipdepth .gt. 0) then
	  write (6, '(a)') 'Turning shallow depth coloring off'
	  ipdepth = 0
	else
	  write (6, '(a)') 'Turning shallow depth coloring on'
	  ipdepth = 2
	end if
	go to 100
      else if (ans .eq. '4') then
	if (ipdepth .gt. 0) then
	  write (6, '(a)') 'Turning shallow depth diff coloring off'
	  ipdepth = 0
	else
	  write (6, '(a)') 'Turning shallow depth diff coloring on'
	  ipdepth = 3
	end if
	go to 100
      else if (ans .eq. '5') then
	if (istaplt .gt. 0) then
	  write (6, '(a)') 
     +           'Turning unassociated station symbol plotting off'
	  istaplt = 0
	else
	  write (6, '(a)') 
     +           'Turning unassociated station symbol plotting on'
	  istaplt = 1
	end if
	go to 100
      else if (ans .eq. '6') then
	if (ipdeplt .gt. 0) then
	  write (6, '(a)') 
     +           'Not marking PDE events'
	  ipdeplt = 0
	else
	  write (6, '(a)') 
     +           'Marking PDE events'
	  ipdeplt = 1
	end if
	go to 100
      else if (ans .eq. '7') then
	if (ipmag .gt. 0) then
	  write (6, '(a)') 
     +           'Not marking magnitudes'
	  ipmag = 0
	else
	  write (6, '(a)') 
     +           'Marking magnitudes'
	  ipmag = 1
	end if
	go to 100
      else if (ans .eq. '8') then
        if (idcirc .gt. 0) then
          write (6, '(a)')
     +           'Not plotting distance circles'
          idcirc = 0
        else
          write (6, '(a)')
     +           'Plotting distance circles'
          idcirc = 1
        end if
        go to 100
      else if (ans .eq. '9') then
        if (istnam .gt. 0) then
          write (6, '(a)')
     +           'Not plotting station names'
          istnam = 0
        else
          write (6, '(a)')
     +           'Plotting station names'
          istnam = 1
        end if
        go to 100
      end if
c
      go to 110
      end
      subroutine plotit (dname, itran, xdim, ydim,  xlow, ylow,
     +                   xmax, xmin, ymax, ymin, dbnam,
     +                   sta, jorid, xlatc, xlonc, evtime,
     +                   nauth, auth, sym, symhue, symlit, symsat,
     +                   idb, nevs, ievs, iporid, ipdepth,
     +                   istaplt, istnam,
     +                   ipdeplt, ipmag, idcirc, tstrt,
     +                   tnd)
c
      parameter  (MAXEVENTS = 100000)
      parameter  (MAXPOINTS = 200000)
      real*4 xlat1, xlon1, xlat2, xlon2, pi, xlatc, xlonc
      real*8 depth, deptho, depthc, evtime, elat, elon, slat,slon
      character*80 dbnam, sta, xarg, arg, auth(20), sym(20), title
      character*80 region, type, loc, tstart, tend, symbl, string
      real*4 symhue(20), symlit(20), symsat(20)
      real*8 slatmndp, slatmxdp, slonmndp, slonmxdp
      integer*8 ievs(MAXEVENTS)
      integer*8 idb
      character*(*) dname, tstrt, tnd
c
#include "EV_f.i"
#include "dbl2.i"
c
      data  pi / 3.14159265358979323846d0 /
      data symsiz / 0.05 /
c
c    Initialize plot
c
      call initt (itran, 'dbmapevents.ps', dname, 
     +                   'dbmapevents', 0.9, 0.0, 0.0)
      if (itran .eq. 0) then
        call setdim (7.5, 10.0, 0.0, 0.0)
      else
        call setdim (10.0, 7.5, 0.0, 0.0)
      end if
      call setscl (0.,1.,0.,1.)
c     call box (0.,1.,0.,1.,0.,0,0)
      title = 'Events from ' // dbnam(1:ilen(dbnam))
     +        // ', centered at ' // sta
c
c    Plot map
c
      if (idcirc .eq. 1) then
        dcircle = 0.0
      else
        dcircle = -1.0
      end if
      call edprojmap (xdim, ydim, xlow, ylow,
     +                      xlatc, xlonc, xmin, xmax,
     +                      ymin, ymax, dcircle, title)

c
c    Plot symbol legend
c
      if (jorid .lt. 0) then
      if (nauth .eq. 0) then
	call setscl (0.0, xdim, 0.0, ydim)
	call setfor (0.0, 0.0, 0.0)
	xplt1 = 0.5
	yplt1 = -0.2
	call text (0.5, -0.2, 0.0, 1, 'All events plotted', 1)
	if (ipdeplt .eq. 1) then
	  xplt1 = 0.5
	  yplt1 = yplt1 - 0.2
	  call setfor (0.0, 0.0, 0.0)
	  call setbac (120.0, 0.5, 1.0)
	  call symbol ('box', xplt1, yplt1, 3*symsiz, 0.0, 1, 1)
	  call setfor (0.0, 0.0, 0.0)
	  xplt1 = 0.8
	  call text (xplt1, yplt1, 0.0, 1, 'Event in the PDE', 1)
	  xplt1 = 0.5
	  yplt1 = yplt1 - 0.2
	  call setbac (240.0, 0.5, 1.0)
	  call symbol ('box', xplt1, yplt1, 3*symsiz, 0.0, 1, 1)
	  call setfor (0.0, 0.0, 0.0)
	  call setbac (0.0, 1.0, 0.0)
	  xplt1 = 0.8
	  call text (xplt1, yplt1, 0.0, 1, 'Event not in the PDE', 1)
	end if
	if (ipmag .eq. 1) then
	    if (xmb .gt. -999.0) then
	      ss = symsiz*(1.0 + 2.0*(xmb-2.0)/4.0)
	      symbl = 'box'
            else
	      ss = symsiz
	      symbl = 'hexagon'
            end if
	  xplt1 = 0.5
	  yplt1 = yplt1 - 0.2
	  call setfor (0.0, 0.0, 0.0)
	  call setbac (240.0, 0.5, 1.0)
	  call symbol ('hexagon', xplt1, yplt1, 3*symsiz, 0.0, 1, 1)
	  call setfor (0.0, 0.0, 0.0)
	  xplt1 = 0.8
	  call text (xplt1, yplt1, 0.0, 1, 'No Mb', 1)
	  do 33  i = 2, 5
	    xplt1 = 0.5
	    yplt1 = yplt1 - 0.2
	    call setbac (240.0, 0.5, 1.0)
	    xmb = i
	    ss = symsiz*(1.0 + 2.0*(xmb-2.0)/4.0)
	    call symbol ('box', xplt1, yplt1, ss, 0.0, 1, 1)
	    call setfor (0.0, 0.0, 0.0)
	    call setbac (0.0, 1.0, 0.0)
	    xplt1 = 0.8
	    write (string, '(a,f3.1)') 'Mb = ',xmb
	    call text (xplt1, yplt1, 0.0, 1, string, 1)
   33     continue
	end if
	if (ipdepth .eq. 1) then
	  xplt1 = 0.5
	  yplt1 = yplt1 - 0.2
	  call setfor (0.0, 0.0, 0.0)
	  call setbac (0.0, 0.7, 0.0)
	  call symbol ('box', xplt1, yplt1, 3*symsiz, 0.0, 1, 1)
	  call setfor (0.0, 0.0, 0.0)
	  xplt1 = 0.8
	  call text (xplt1, yplt1, 0.0, 1, 'Default depth', 1)
	  xplt1 = 0.5
	  yplt1 = yplt1 - 0.2
	  call setbac (240.0, 0.4, 1.0)
	  call symbol ('box', xplt1, yplt1, 3*symsiz, 0.0, 1, 1)
	  call setfor (0.0, 0.0, 0.0)
	  xplt1 = 0.8
	  call text (xplt1, yplt1, 0.0, 1, 'Depth <= 10', 1)
	  xplt1 = 0.5
	  yplt1 = yplt1 - 0.2
	  call setbac (120.0, 0.4, 1.0)
	  call symbol ('box', xplt1, yplt1, 3*symsiz, 0.0, 1, 1)
	  call setfor (0.0, 0.0, 0.0)
	  xplt1 = 0.8
	  call text (xplt1, yplt1, 0.0, 1, '10 < Depth <= 50', 1)
	  xplt1 = 0.5
	  yplt1 = yplt1 - 0.2
	  call setbac (0.0, 0.4, 1.0)
	  call symbol ('box', xplt1, yplt1, 3*symsiz, 0.0, 1, 1)
	  call setfor (0.0, 0.0, 0.0)
	  xplt1 = 0.8
	  call text (xplt1, yplt1, 0.0, 1, '50 < Depth <= 150', 1)
	  xplt1 = 0.5
	  yplt1 = yplt1 - 0.2
	  call setbac (300.0, 0.5, 1.0)
	  call symbol ('box', xplt1, yplt1, 3*symsiz, 0.0, 1, 1)
	  call setfor (0.0, 0.0, 0.0)
	  call setbac (0.0, 1.0, 0.0)
	  xplt1 = 0.8
	  call text (xplt1, yplt1, 0.0, 1, 'Depth > 150', 1)
	else if (ipdepth .eq. 2) then
	  xplt1 = 0.5
	  yplt1 = yplt1 - 0.2
	  call setbac (240.0, 0.4, 1.0)
	  call symbol ('box', xplt1, yplt1, 3*symsiz, 0.0, 1, 1)
	  call setfor (0.0, 0.0, 0.0)
	  xplt1 = 0.8
	  call text (xplt1, yplt1, 0.0, 1, 'Depth <= 5', 1)
	  xplt1 = 0.5
	  yplt1 = yplt1 - 0.2
	  call setbac (180.0, 0.4, 1.0)
	  call symbol ('box', xplt1, yplt1, 3*symsiz, 0.0, 1, 1)
	  call setfor (0.0, 0.0, 0.0)
	  xplt1 = 0.8
	  call text (xplt1, yplt1, 0.0, 1, '5 < Depth <= 10', 1)
	  xplt1 = 0.5
	  yplt1 = yplt1 - 0.2
	  call setbac (120.0, 0.4, 1.0)
	  call symbol ('box', xplt1, yplt1, 3*symsiz, 0.0, 1, 1)
	  call setfor (0.0, 0.0, 0.0)
	  xplt1 = 0.8
	  call text (xplt1, yplt1, 0.0, 1, '10 < Depth <= 15', 1)
	  xplt1 = 0.5
	  yplt1 = yplt1 - 0.2
	  call setbac (30.0, 0.4, 1.0)
	  call symbol ('box', xplt1, yplt1, 3*symsiz, 0.0, 1, 1)
	  call setfor (0.0, 0.0, 0.0)
	  xplt1 = 0.8
	  call text (xplt1, yplt1, 0.0, 1, '15 < Depth <= 20', 1)
	  xplt1 = 0.5
	  yplt1 = yplt1 - 0.2
	  call setbac (0.0, 0.5, 1.0)
	  call symbol ('box', xplt1, yplt1, 3*symsiz, 0.0, 1, 1)
	  call setfor (0.0, 0.0, 0.0)
	  call setbac (0.0, 1.0, 0.0)
	  xplt1 = 0.8
	  call text (xplt1, yplt1, 0.0, 1, 'Depth > 20', 1)
	end if
	call setscl (xmin, xmax, ymin, ymax)
      else
	call setscl (0.0, xdim, 0.0, ydim)
	if (ipdepth .eq. 3) then
	  xplt1 = 0.5
	  yplt1 = -0.4
	  call setbac (240.0, 0.4, 1.0)
	  call symbol ('box', xplt1, yplt1, 3*symsiz, 0.0, 1, 1)
	  call setfor (0.0, 0.0, 0.0)
	  xplt1 = 0.8
	  call text (xplt1, yplt1, 0.0, 1, 'Delta depth < -1', 1)
	  xplt1 = 0.5
	  yplt1 = -0.6
	  call setbac (180.0, 0.4, 1.0)
	  call symbol ('box', xplt1, yplt1, 3*symsiz, 0.0, 1, 1)
	  call setfor (0.0, 0.0, 0.0)
	  xplt1 = 0.8
	  call text (xplt1, yplt1,0.0,1,'-2 < Delta depth < -0.2', 
     +               1)
	  xplt1 = 0.5
	  yplt1 = -0.8
	  call setbac (120.0, 0.4, 1.0)
	  call symbol ('box', xplt1, yplt1, 3*symsiz, 0.0, 1, 1)
	  call setfor (0.0, 0.0, 0.0)
	  xplt1 = 0.8
	  call text (xplt1,yplt1,0.0,1,'-0.2 < Delta depth < 0.2', 
     +               1)
	  xplt1 = 0.5
	  yplt1 = -1.0
	  call setbac (30.0, 0.4, 1.0)
	  call symbol ('box', xplt1, yplt1, 3*symsiz, 0.0, 1, 1)
	  call setfor (0.0, 0.0, 0.0)
	  xplt1 = 0.8
	  call text (xplt1, yplt1, 0.0, 1, '0.2 < Delta depth < 1', 
     +               1)
	  xplt1 = 0.5
	  yplt1 = -1.2
	  call setbac (0.0, 0.5, 1.0)
	  call symbol ('box', xplt1, yplt1, 3*symsiz, 0.0, 1, 1)
	  call setfor (0.0, 0.0, 0.0)
	  call setbac (0.0, 1.0, 0.0)
	  xplt1 = 0.8
	  call text (xplt1, yplt1, 0.0, 1, '1 < Delta depth', 1)
	else
	do 210  i = 1, nauth
	  call setbac (symhue(i), symlit(i), symsat(i))
	  xplt1 = 0.5
	  yplt1 = -0.2 - (i-1)*0.20
	  if (sym(i) .eq. 'NONE') then
	  else
	    call symbol (sym(i), xplt1, yplt1, symsiz, 0.0, 1, 1)
	  end if
	  call setbac (0.0, 1.0, 0.0)
	  xplt1 = 0.8
	  call text (xplt1, yplt1, 0.0, 1, auth(i), 1)
  210   continue
	end if
	call setscl (xmin, xmax, ymin, ymax)
      end if
      end if
c
c    Open event view
c
      call delbox2llbox (xlatc, xlonc, xmin, xmax, 
     +                         ymin, ymax,
     +                         xlatmn, xlatmx, xlonmn, xlonmx)
      slatmndp = xlatmn - 5.0
      slatmxdp = xlatmx + 5.0
      slonmndp = xlonmn - 5.0
      slonmxdp = xlonmx + 5.0
      if (jorid .lt. 0) then
        if (nevs .eq. 0) then
          nevs = MAXEVENTS
          tstart = tstrt
          tend = tnd
          call evcreate ('css3.0', 1, dbnam, tstart, tend, 
     +       slatmndp, slatmxdp, slonmndp, slonmxdp, nevs, ievs)
          if (nevs .lt. 0) then
	    print *,'dbmapevents: Error return from evcreate'
          end if
          if (nevs .eq. 0) then
	    print *,'dbmapevents: No events within range'
          end if
	  if (nevs .eq. MAXEVENTS) then
	    print *,'dbmapevents: Attempt to exceed events dimension'
	  end if
        end if
      else
        if (nevs .eq. 0) then
          nevs = MAXEVENTS
	  write (tstart, '(f17.5)') evtime-0.001
	  write (tend, '(f17.5)') evtime+0.001
          slatmndp = xlatc-0.001
          slatmxdp = xlatc+0.001
          slonmndp = xlonc-0.001
          slonmxdp = xlonc+0.001
          call evcreate ('css3.0', 1, dbnam, tstart, tend, 
     +       slatmndp, slatmxdp, slonmndp, slonmxdp, nevs, ievs)
          if (nevs .lt. 0) then
	    print *,'dbmapevents: Error return from evcreate'
          end if
          if (nevs .eq. 0) then
	    print *,'dbmapevents: No events within range'
          end if
	  if (nevs .eq. MAXEVENTS) then
	    print *,'dbmapevents: Attempt to exceed events dimension'
	  end if
        end if
      end if
c
c    Read in station locations and plot 
c
      if (istaplt .eq. 1) then
        call dbgetntuples (idb, RELID_SITE, ntuples)
        call setfor (0.0, 0.0, 0.0)
        call chrsiz (0.08, 1.0, 0.0)
        do 75  i = 1, ntuples
	  call dbgetattrs (idb, RELID_SITE, i,
     +                   ATTRID_LON, xlon,
     +                   ATTRID_LAT, xlat,
     +                   ATTRID_STA, arg,
     +			 0)
	  call latlon2xydel (xlatc, xlonc, xlat, xlon,
     +                     xplt1, yplt1)
          call setbac (0.0, 0.8, 0.0)
	  call symbol ('triangle', xplt1, yplt1, 2*symsiz, 0.0, 0, 1)
          call setbac (0.0, 1.0, 0.0)
          if (istnam .eq. 1) then
            xarg = ' ' // arg 
            call text (xplt1, yplt1, 0.0, 1, xarg, 0)
          endif
   75   continue
      end if
      if (jorid .ge. 0) then
        do 400  i = 1, nevs
	  call evget (ievs(i), EV_NHYPOS, nhypos,
     +                       EV_PREFHYPO, ipref,
     +                       EV_EVID, ievid,
     +			 0)
	  if (ipref .lt. 1) ipref = 1
	  call evget (ievs(i), 
     +                       EV_HYPO_ORID, ipref, iorid,
     +                       EV_HYPO_NSTAS, ipref, nstas,
     +                       0)
	  if (iorid .eq. jorid) go to 410
  400   continue
  410   continue
	if (i .gt. nevs) then
	  write (6, '(a)') 'Orid not found.'
	  stop
	end if
	do 475  j = 1, nstas
	  call evget (ievs(i), 
     +                       EV_HYPO_STA_STA, ipref, j, arg,
     +                       EV_HYPO_STA_LAT, ipref, j, slat,
     +                       EV_HYPO_STA_LON, ipref, j, slon,
     +                       EV_HYPO_STA_NASSOCS, ipref, j, nassocs,
     +                       0)
	  jflg = 0
          xlat = slat
          xlon = slon
	  do 476  k = 1, nassocs
	    call evget (ievs(i), 
     +              EV_HYPO_STA_ASSOC_ASSOCFLAG, ipref, j, k, iflg,
     +              0)
	    jflg = jflg + iflg
  476     continue
	  call latlon2xydel (xlatc, xlonc, xlat, xlon,
     +                     xplt1, yplt1)
	  if (jflg .eq. 0) then
	    call setbac (0.0, 0.3, 0.0)
            call chrsiz (0.04, 1.0, 0.0)
	    call symbol ('cross', xplt1, yplt1, 0.8*symsiz, 0.0, 0, 1)
	    call setbac (0.0, 1.0, 0.0)
            xarg = ' ' // arg  
 	    call text (xplt1, yplt1, 0.0, 1, xarg, 0)
	  else
	    call setbac (0.0, 0.0, 0.0)
            call chrsiz (0.06, 1.0, 0.0)
	    call symbol ('triangle', xplt1, yplt1, symsiz, 0.0, 0, 1)
            xarg = ' ' // arg  
 	    call text (xplt1, yplt1, 0.0, 1, xarg, 0)
	    call setbac (0.0, 1.0, 0.0)
	  end if
  475   continue
      end if
c
c    Read in hypocentral data and plot it
c
      call setfor (0.0, 0.0, 0.0)
      if (jorid .lt. 0) then
      do 500  i = 1, nevs
	call evget (ievs(i), EV_NHYPOS, nhypos,
     +                       EV_PREFHYPO, ipref,
     +                       EV_EVID, ievid,
     +			 0)
        ipde = 0
        xmb = -999.0
        do 555  j = 1, nhypos
          call evget (ievs(i), EV_HYPO_AUTH, j, arg,
     +                       EV_HYPO_MB, j, ymb,
     +                0)
          if (ymb .gt. -999.0) xmb = ymb
          if (arg .eq. 'PDE') ipde = 1
  555   continue
	iplt = 1
	if (nauth .eq. 0) then
	  i1 = ipref
	  i2 = ipref
	else
	  i1 = 1
	  i2 = nhypos
	end if
	if (nauth .gt. 1) then
	  do 491  l = 1, nauth
            do 492 k = 1, nhypos
	      call evget (ievs(i), EV_HYPO_AUTH, k, arg,
     +                       0)
              if (arg .eq. auth(l)) go to 491
  492       continue
            go to 500
  491     continue
	end if
	if (nauth .gt. 0) then
	  deptho = 0.0
	  depthc = 0.0
	  do 490 k = i1, i2
	    call evget (ievs(i), EV_HYPO_LAT, k, elat,
     +                       EV_HYPO_LON, k, elon,
     +                       EV_HYPO_DEPTH, k, depth,
     +                       EV_HYPO_AUTH, k, arg,
     +                       EV_HYPO_MB, k, ymb,
     +                       0)
            if (arg .eq. auth(1)) deptho = depth
            if (arg .eq. auth(2)) depthc = depth
  490     continue
	  dep = depthc-deptho
	end if
 	do 505  k = i1, i2
	call evget (ievs(i), EV_HYPO_LAT, k, elat,
     +                       EV_HYPO_LON, k, elon,
     +                       EV_HYPO_DEPTH, k, depth,
     +                       EV_HYPO_AUTH, k, arg,
     +                       EV_HYPO_ORID, k, iorid,
     +                       EV_HYPO_MB, k, ymb,
     +                       EV_HYPO_NSTAS, k, nstas,
     +                       0)
        if (ymb .gt. -999.0) xmb = ymb
c       xmb = nstas
	xlat = elat
	xlon = elon
	if (nauth .eq. 0) then
	  call latlon2xydel (xlatc, xlonc, xlat, xlon,
     +                     xplt1, yplt1)
          symbl = 'box'
	  if (ipdepth .eq. 1) then
	    if (depth .eq. 33.0d0) then
	      call setbac (0.0, 0.7, 0.0)
	    else if (depth .le. 10.0d0) then
	      call setbac (240.0, 0.4, 1.0)
	    else if (depth .le. 50.0d0) then
	      call setbac (120.0, 0.4, 1.0)
	    else if (depth .le. 150.0d0) then
	      call setbac (0.0, 0.4, 1.0)
	    else
	      call setbac (300.0, 0.5, 1.0)
	    end if
	  else if (ipdepth .eq. 2) then
	    if (depth .le. 5.0d0) then
	      call setbac (240.0, 0.4, 1.0)
	    else if (depth .le. 10.0d0) then
	      call setbac (180.0, 0.4, 1.0)
	    else if (depth .le. 15.0d0) then
	      call setbac (120.0, 0.4, 1.0)
	    else if (depth .le. 20.0d0) then
	      call setbac (30.0, 0.4, 1.0)
	    else
	      call setbac (0.0, 0.5, 1.0)
	    end if
	  else if (ipdeplt .eq. 1) then
	    if (ipde .eq. 1) then
	      call setbac (120.0, 0.5, 1.0)
	    else
	      call setbac (240.0, 0.5, 1.0)
	    end if
	  else
	    call setbac (0.0, 0.5, 0.0)
	  end if
	  if (ipmag .ne. 0) then
	    if (xmb .gt. -999.0) then
	      ss = symsiz*(1.0 + 2.0*(xmb-2.0)/4.0)
	      symbl = 'box'
            else
	      ss = symsiz
	      symbl = 'hexagon'
            end if
	  else
	    ss = symsiz
	    symbl = 'box'
	  end if
	  call symbol (symbl, xplt1, yplt1, ss, 0.0, 0, 1)
	  call setbac (0.0, 1.0, 0.0)
	else
	  do 510  j = 1, nauth
	    if (arg .eq. auth(j)) go to 511
  510     continue
	  go to 500
  511     continue
	  call latlon2xydel (xlatc, xlonc, xlat, xlon,
     +                     xplt1, yplt1)
	  if (ipdepth .eq. 3) then
	    if (dep .lt. -1.0) then
	      call setbac (240.0, 0.4, 1.0)
	    else if (dep .le. -0.2) then
	      call setbac (180.0, 0.4, 1.0)
	    else if (dep .le. 0.2) then
	      call setbac (120.0, 0.4, 1.0)
	    else if (dep .le. 1.0) then
	      call setbac (30.0, 0.4, 1.0)
	    else
	      call setbac (0.0, 0.5, 1.0)
	    end if
	  else
	    call setbac (symhue(j), symlit(j), symsat(j))
	  end if
          call symbol (sym(j), xplt1, yplt1, symsiz, 0.0, 0, 1)
          call setbac (0.0, 1.0, 0.0)
	end if
	if (iplt .gt. 1) then
          call line (xplt1, yplt1, xplt2, yplt2, 0.0, 0, 0)
	else
	  if (iporid .eq. 1) then
	    write (title, '(i15)') iorid
	    do 513 l = 1, len(title)
	      if (title(l:l) .ne. ' ') go to 514
  513       continue
  514       l = l - 5
            call chrsiz (0.04, 1.0, 0.0)
 	    call text (xplt1, yplt1, 0.0, 1, title(l:len(title)), 
     +               0)
	  end if
	end if
	iplt = iplt + 1
	xplt2 = xplt1
	yplt2 = yplt1
  505 continue
  500 continue
      call setfor (0.0, 0.0, 0.0)
      call setbac (0.0, 1.0, 0.0)
      end if
c
      return
      end
      subroutine idevent (xlat0, xlon0, x, y, nevs, ievs,
     +                    nauth, auth)
c
      real*4 xlat0, xlon0
      real*4  x, y
      integer nevs
      integer*8 ievs(nevs)
      integer nauth
      character*(*) auth(nauth)
c
#include "EV_f.i"
      real*8 elat, elon, time, depth, sec
      character*80 author, alg
c
      call xydel2latlon (xlat0, xlon0, x, y,
     +                         xlat2, xlon2)
      amin = 1.e30
      do 100  i = 1, nevs
	call evget (ievs(i), EV_NHYPOS, nhypos,
     +                       EV_PREFHYPO, ipref,
     +                       EV_EVID, ievid,
     +			 0)
	do 200  j = 1, nhypos
	  call evget (ievs(i), EV_HYPO_LAT, j, elat,
     +                       EV_HYPO_LON, j, elon,
     +                       EV_HYPO_AUTH, j, author,
     +                       0)
          xlat = elat
          xlon = elon
	  if (nauth .eq. 0) then
	  else
	    do 510  k = 1, nauth
	      if (author .eq. auth(k)) go to 511
  510       continue
	    go to 200
  511       continue
	  end if
          call latlon2xydel (xlat, xlon, xlat2, xlon2,
     +                         xdel, ydel)
	  dist = sqrt(xdel**2 + ydel**2)
	  if (dist .lt. amin) then
	    amin = dist
	    ihold = i
	    jhold = j
	  end if
  200   continue
  100 continue
      i = ihold
      j = jhold
      call evget (ievs(i), EV_HYPO_LAT, j, elat,
     +                       EV_HYPO_LON, j, elon,
     +                       EV_HYPO_DEPTH, j, depth,
     +                       EV_HYPO_TIME, j, time,
     +                       EV_HYPO_MB, j, xmb,
     +                       EV_HYPO_MS, j, xms,
     +                       EV_HYPO_ML, j, xml,
     +                       EV_HYPO_AUTH, j, author,
     +                       EV_HYPO_ORID, j, iorid,
     +                       0)
      call e2h(time, iyear, iday, ihour, imin, sec)
      write (6, '(a)') ' '
      write (6, '(a,a)')    'AUTH:   ',author(1:ilen(author))
      write (6, '(a,i8)')   'ORID:   ',iorid
      write (6, '(a,i4,i3,1x,i2.2,a,i2.2,a,f5.2)')   
     +                      'TIME:   ',iyear,iday,ihour,':',
     +                   imin,':',sec
      write (6, '(a,f9.4)') 'LAT:    ',elat
      write (6, '(a,f9.4)') 'LON:    ',elon
      write (6, '(a,f9.4)') 'DEPTH:  ',depth
      write (6, '(a,f9.4)') 'MB:     ',xmb
      write (6, '(a,f9.4)') 'MS:     ',xms
      write (6, '(a,f9.4)') 'ML:     ',xml
      xlat2 = elat
      xlon2 = elon
      call latlon2xydel (xlat0, xlon0, xlat2, xlon2,
     +                         x, y)
c    
      return
      end
      subroutine idstation (xlat0, xlon0, x, y, idb,
     +                    nauth, auth)
c
      real*4 xlat0, xlon0
      real*4  x, y
      integer*8 idb
      integer nauth
      character*(*) auth(nauth)
c
#include "EV_f.i"
#include "dbl2.i"
      real*8 elat, elon
      character*80 sta, staname
      real*4 xlat4, xlon4, elev
c
      call xydel2latlon (xlat0, xlon0, x, y,
     +                         xlat2, xlon2)
      amin = 1.e30
      call dbgetntuples (idb, RELID_SITE, ntuples)
      do 100  i = 1, ntuples
	call dbgetattrs (idb, RELID_SITE, i,
     +                   ATTRID_LON, xlon4,
     +                   ATTRID_LAT, xlat4,
     +			 0)
        xlat = xlat4
        xlon = xlon4
        call latlon2xydel (xlat, xlon, xlat2, xlon2,
     +                         xdel, ydel)
        dist = sqrt(xdel**2 + ydel**2)
	if (dist .lt. amin) then
	  amin = dist
	  ihold = i
	end if
  100 continue
      i = ihold
      call dbgetattrs (idb, RELID_SITE, i,
     +                   ATTRID_STA, sta,
     +                   ATTRID_LON, xlon4,
     +                   ATTRID_LAT, xlat4,
     +                   ATTRID_ELEV, elev,
     +			 0)
      write (6, '(a)') ' '
      write (6, '(a,a)')    'STA:    ',sta(1:ilen(sta))
      write (6, '(a,f9.4)') 'LAT:    ',xlat4
      write (6, '(a,f9.4)') 'LON:    ',xlon4
      write (6, '(a,f9.4)') 'ELEV:   ',elev
      xlat2 = xlat4
      xlon2 = xlon4
      call latlon2xydel (xlat0, xlon0, xlat2, xlon2,
     +                         x, y)
c    
      return
      end
      subroutine symbol (type, x, y, size, thick, iclip, ifill)
      character*(*)      type
c
c     common /pscl/ xmin,xmax,ymin,ymax,xscale,yscale,xrange,yrange
c
      real*4 xplt(10), yplt(10)
      real*8 xmap, ymap, rxmap, rymap
      character*8 xtype, ytype
c
      xr = xmap(x)
      yr = ymap(y)
      xscale = 1.0
      yscale = 1.0
      call gettype (xtype, ytype)
      call getdim (xdim, ydim, xlow, ylow)
      call getscl (xxmin, xxmax, yymin, yymax)
      call ntype ('LIN', 'LIN')
      call setdim (xdim, ydim, xlow, ylow)
      call setscl (0.0, xdim, 0.0, ydim)
      xx = rxmap(xr)
      yy = rymap(yr)
      if (ifill .eq. 1) call nfillon
      if (type .eq. 'box') then
        x1 = xx - size*0.5 / xscale
        y1 = yy - size*0.5 / yscale
        x2 = xx + size*0.5 / xscale
        y2 = yy + size*0.5 / yscale
        call box (x1, x2, y1, y2, thick, 0, iclip)
      else if (type .eq. 'triangle') then
	xplt(1) = xx
	yplt(1) = yy + size/sqrt(3.0)/yscale
	xplt(2) = xx + size*0.5/xscale
	yplt(2) = yy - size*0.5/sqrt(3.0)/yscale
	xplt(3) = xx - size*0.5/xscale
	yplt(3) = yplt(2)
	xplt(4) = xplt(1)
	yplt(4) = yplt(1)
	xplt(5) = xplt(2)
	yplt(5) = yplt(2)
	call nplot (5, xplt, yplt, 0, iclip, thick, 0, ' ')
      else if (type .eq. 'hexagon') then
	xplt(1) = xx - size*0.25/xscale
	yplt(1) = yy + size*0.5*sqrt(0.75)/yscale
	xplt(2) = xx + size*0.25/xscale
	yplt(2) = yplt(1)
	xplt(3) = xx + size*0.5/xscale
	yplt(3) = yy
	xplt(4) = xplt(2)
	yplt(4) = yy - size*0.5*sqrt(0.75)/yscale
	xplt(5) = xplt(1)
	yplt(5) = yplt(4)
	xplt(6) = xx - size*0.5/xscale
	yplt(6) = yplt(3)
	xplt(7) = xplt(1)
	yplt(7) = yplt(1)
	xplt(8) = xplt(2)
	yplt(8) = yplt(2)
	call nplot (8, xplt, yplt, 0, iclip, thick, 0, ' ')
      else if (type .eq. 'cross') then
        x1 = xx - size*0.5 / xscale
        y1 = yy - size*0.5 / yscale
        x2 = xx + size*0.5 / xscale
        y2 = yy + size*0.5 / yscale
        call line (x1, yy, x2, yy, thick, 0, iclip)
        call line (xx, y1, xx, y2, thick, 0, iclip)
      end if
      if (ifill .eq. 1) call nfilloff
      call ntype (xtype, ytype)
      call setdim (xdim, ydim, xlow, ylow)
      call setscl (xxmin, xxmax, yymin, yymax)
c
      return
      end
c
c******************************************************************************
c
c    Integer function ILEN
c
c******************************************************************************
c
      INTEGER FUNCTION ILEN(STRING)
c
c    Function ILEN will return the number of non-blank characters
c    in character string STRING.
c
      CHARACTER*(*) STRING
c
      L = LEN(STRING)
      ILEN = 0
      DO 10  I = 1, L
        IF (STRING(I:I) .NE. ' ') ILEN = I
   10 CONTINUE
c
      RETURN
c
c    End of function ILEN
c
      END

c $Id$ 

c $Id$ 
