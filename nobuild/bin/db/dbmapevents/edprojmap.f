


c
c*******************************************************************************
c
c    Subroutine edprojmap
c
c*******************************************************************************
c
      subroutine edprojmap (xdim, ydim, xlow, ylow,
     +                      xlatc, xlonc, xdelmn, xdelmx,
     +                      ydelmn, ydelmx, dcircle, title)
c
      real*4                xdim, ydim, xlow, ylow
      real*4                xlatc, xlonc, xdelmn, xdelmx
      real*4                ydelmn, ydelmx, dcircle
      character*(*)                                  title
c
c    edprojmap will make an equal distance projection map of the world.
c
c    Inputs -	xdim,ydim	= horizon and vertical dimensions (in inches)
c				  of map.
c		xlow,ylow	= X-Y location in inches of lower laft hand
c				  corner of map relative to lower left hand
c				  corner of paper.
c		xlatc,xlonc	= Latitude and longitude in degrees of
c				  center of projection.
c		xdelmn,xdelmx	= Min and max relative X-distance values for
c				  scaling.
c		ydelmn,ydelmx	= Min and max relative Y-distance values for
c				  scaling. If ydelmn=ydelmx, then the
c				  values are determined automatically to
c				  preserve isotropic scaling with ydelmn
c				  at the center of the plot.
c		dcircle		= Constant distance circle annotation increment
c				  in degrees. If < 0.0, then no circles are
c				  drawn. If == 0.0, then the circle increments
c				  are determined automatically.
c		title		= Title for plot.
c
      parameter (MAXPOINTS = 200000)
      character*80 region, type, loc
      real*4 xlat(MAXPOINTS), xlon(MAXPOINTS)
      real*4 xbuf(MAXPOINTS), ybuf(MAXPOINTS)
      integer ibuf(MAXPOINTS)
c
c    Initialize map stuff
c
      call mapopen(ierr)
      if (ierr .gt. 0) then
	write (6, '(a)') 'edprojmap: mapopen error.'
	return
      end if
c
c    Initialize plot
c
      call setaxf (132)
      call setfor (0.0, 0.0, 0.0)
      call setbac (0.0, 1.0, 0.0)
      call setdim (xdim, ydim, xlow, ylow)
      if (ydelmn .eq. ydelmx) then
        ydelmn = ydelmx + 0.5*(xdelmx-xdelmn)*ydim/xdim
        ydelmx = ydelmx - 0.5*(xdelmx-xdelmn)*ydim/xdim
      end if
      call setscl (xdelmn, xdelmx, ydelmn, ydelmx)
      call clrrgn (xdelmn, xdelmx, ydelmn, ydelmx)
c
c    Plot map
c
      if ((xdelmx-xdelmn) .lt. 10.0) then
	dr = 1.0
	i1 = 180
	i2 = 360
	region = 'continents'
	type = 'fill coasts political rivers states counties'
      else if ((xdelmx-xdelmn) .lt. 60.0) then
	dr = 5.0
	i1 = 36
	i2 = 72
	region = 'continents'
	type = 'fill coasts political states'
      else if ((xdelmx-xdelmn) .lt. 100.0) then
	dr = 10.0
	i1 = 18
	i2 = 36
	region = 'world'
	type = 'fill coasts political'
      else
	dr = 10.0
	i1 = 18
	i2 = 36
	region = 'world'
	type = 'fill coasts'
      end if
      call setfor (0.0, 0.0, 0.0)
      call delbox2llbox (xlatc, xlonc, xdelmn, xdelmx, 
     +                         ydelmn, ydelmx,
     +                         xlatmn, xlatmx, xlonmn, xlonmx)
      call mapselect (region, type,
     +                xlatmn, xlatmx, xlonmn, xlonmx, nsegs, ierr)
      if (ierr .ne. 0) then
        print *,'edprojmap: mapselect() error.'
        return
      end if
      do 100  i = 1, nsegs
        npts = MAXPOINTS
        call mapnextseg (npts, xlat, xlon, loc, type, ierr)
        if (ierr .gt. 0) then
          print *,'edprojmap: mapnextseg() error',ierr
          stop
        end if
        do 200  j = 1, npts
          call latlon2xydel (xlatc, xlonc, xlat(j), xlon(j),
     +                       xlon(j), xlat(j))
  200   continue
        if (type .eq. 'political') then
          call setfor (0.0, 0.5, 1.0)
	  thick = 0.01
        else if (type .eq. 'river') then
          call setfor (240.0, 0.5, 1.0)
	  thick = 0.0
        else if (type .eq. 'county') then
          call setfor (300.0, 0.5, 1.0)
	  thick = 0.0
        else if (type .eq. 'state') then
          call setfor (120.0, 0.3, 1.0)
	  thick = 0.01
        else if (type .eq. 'level1') then
          call setbac (30.0, 0.95, 1.0)
          nbuf = MAXPOINTS
          call npolyfill (npts, xlon, xlat, nbuf, xbuf, ybuf, ibuf)
          call setbac (0.0, 1.0, 0.0)
          call setfor (0.0, 0.0, 0.0)
	  thick = 0.0
	  go to 100
        else if (type .eq. 'level2') then
          call setbac (240.0, 0.995, 1.0)
          call setbac (0.0, 1.0, 0.0)
          nbuf = MAXPOINTS
          call npolyfill (npts, xlon, xlat, nbuf, xbuf, ybuf, ibuf)
          call setbac (0.0, 1.0, 0.0)
          call setfor (0.0, 0.0, 0.0)
	  thick = 0.0
	  go to 100
        else if (type .eq. 'level3') then
          call setbac (30.0, 0.95, 1.0)
          nbuf = MAXPOINTS
          call npolyfill (npts, xlon, xlat, nbuf, xbuf, ybuf, ibuf)
          call setbac (0.0, 1.0, 0.0)
          call setfor (0.0, 0.0, 0.0)
	  thick = 0.0
	  go to 100
        else
          call setfor (0.0, 0.0, 0.0)
	  thick = 0.0
        end if
	if (region .eq. 'world') thick = 0.0
        call nplot (npts, xlon, xlat, 0, 0, thick, 0, ' ')
  100 continue
c
c    Plot lat-lon grids
c
      call setfor (0.0, 0.6, 0.0)
      do 5 i = 1, i1
	xlat1 = -90.0 + (i-1)*dr
	if (xlat1 .lt. xlatmn) go to 5
	if (xlat1 .gt. xlatmx) go to 5
	do 6 j = 1, 361
	  xlon1 = j - 1
	  call latlon2xydel (xlatc, xlonc, xlat1, xlon1,
     +                       xlon(j), xlat(j))
    6   continue
	call nplot (361, xlon, xlat, 0, 0, 0.0, 0, ' ')
    5 continue
      do 7 i = 1, i2
	xlon1 = -180.0 + (i-1)*dr
	if (xlon1 .lt. xlonmn) go to 7
	if (xlon1 .gt. xlonmx) go to 7
	do 8 j = 1, 181
	  xlat1 = -90.0 + j - 1
	  call latlon2xydel (xlatc, xlonc, xlat1, xlon1,
     +                       xlon(j), xlat(j))
    8   continue
	call nplot (181, xlon, xlat, 0, 0, 0.0, 0, ' ')
    7 continue
c
c    Plot cross-hairs
c
      call ltype (0)
      call setfor (0.0, 0.0, 0.0)
      call line (xdelmn, 0.0, xdelmx, 0.0, 0.0, 0, 0)
      call line (0.0, ydelmn, 0.0, ydelmx, 0.0, 0, 0)
c
c    Plot distance circles
c
      if (dcircle .ge. 0.0) then
        if (dcircle .ne. 0.0) then
          dr = dcircle
        end if
        d1 = sqrt(xdelmn**2 + ydelmn**2)
        d2 = sqrt(xdelmn**2 + ydelmx**2)
        d3 = sqrt(xdelmx**2 + ydelmn**2)
        d4 = sqrt(xdelmx**2 + ydelmx**2)
        if (d2 .gt. d1) d1 = d2
        if (d3 .gt. d1) d1 = d3
        if (d4 .gt. d1) d1 = d4
        nr = d1 / dr + 1.5
        if (dr .eq. 10.0 .and. dcircle .eq. 0.0) nr = 0
        do 18  i = 1, nr
          r = dr*i
   18   call circle (0.0, 0.0, r, 360, 0, 0, 0.0, 0)
      end if
c
c    Plot box and title
c
      call setfor (0.0, 0.0, 0.0)
      call axis (xdim, ydim, 0.1, 0.1, xlow, ylow,
     1           xdelmx, xdelmn, ydelmx, ydelmn, 0.0, 0.0,
     2           0.0, 0.0, '(none)', '(none)',
     3           '', '', title, 0)
c
c    Normal exit.
c
      return
      end
      subroutine latlon2xydel (xlat1, xlon1, xlat2, xlon2,
     +                         xdel, ydel)
c
      real*4 xlat1, xlon1
      real*4 xlat2, xlon2, xdel, ydel
c
      real*8 xlt1, xln1, xlt2, xln2, del, az, pi
c
      data  pi / 3.14159265358979323846d0 /
c
      if (xlat1 .eq. xlat2 .and. xlon1 .eq. xlon2) then
        xdel = 0.0
        ydel = 0.0
        return
      else
        xlt1 = xlat1 * pi / 180.0d0
        xln1 = xlon1 * pi / 180.0d0
        xlt2 = xlat2 * pi / 180.0d0
        xln2 = xlon2 * pi / 180.0d0
        call dist (xlt1, xln1, xlt2, xln2, del, az)
      end if
      del = del * 180.0d0 / pi
      xdel = del * dsin (az)
      ydel = del * dcos (az)
c
      return
      end
      subroutine xydel2latlon (xlat1, xlon1, xdel, ydel,
     +                         xlat2, xlon2)
c
      real*4 xlat1, xlon1
      real*4 xlat2, xlon2, xdel, ydel
c
      real*8 xlt1, xln1, xlt2, xln2, del, az, pi
c
      data  pi / 3.14159265358979323846d0 /
c
      if (xdel .eq. 0.0 .and. ydel .eq. 0.0) then
        xlat2 = xlat1
        xlon2 = xlon1
        return
      else
        xlt1 = xlat1 * pi / 180.0d0
        xln1 = xlon1 * pi / 180.0d0
        del = sqrt(xdel**2 + ydel**2)
        del = del * pi / 180.0d0
        az = atan2 ( xdel, ydel )
        call latlon (xlt1, xln1, del, az, xlt2, xln2)
      end if
      xlat2 = xlt2 * 180.0 / pi
      xlon2 = xln2 * 180.0 / pi
c
      return
      end
      subroutine delbox2llbox (xlatc, xlonc, xdelmn, xdelmx, 
     +                         ydelmn, ydelmx,
     +                         xlatmn, xlatmx, xlonmn, xlonmx)
c
      if (xdelmx-xdelmn .gt. 90.0) then
        xlatmn = -90.0
        xlatmx = 90.0
        xlonmn = -360.0
        xlonmx = 360.0
        return
      end if
      call xydel2latlon (xlatc, xlonc, xdelmn, ydelmn, xlat, xlon)
      if (xlonc+xdelmx .gt. 180.0) then
        xlonmn = -360.0
        xlonmx = 360.0
      else if (xlonc+xdelmn .lt. -180.0) then
        xlonmn = -360.0
        xlonmx = 360.0
      else
        xlonmn = xlon
        xlonmx = xlon
      end if
      xlatmn = xlat
      xlatmx = xlat
      dx = (xdelmx-xdelmn)*0.1
      do 10  i = 1, 10
        x = xdelmn + dx*i
        call xydel2latlon (xlatc, xlonc, x, ydelmn, xlat, xlon)
        if (xlat .lt. xlatmn) xlatmn = xlat
        if (xlon .lt. xlonmn) xlonmn = xlon
        if (xlat .gt. xlatmx) xlatmx = xlat
        if (xlon .gt. xlonmx) xlonmx = xlon
   10 continue
      dy = (ydelmx-ydelmn)*0.1
      do 20  i = 1, 10
        y = ydelmn + dy*i
        call xydel2latlon (xlatc, xlonc, xdelmx, y, xlat, xlon)
        if (xlat .lt. xlatmn) xlatmn = xlat
        if (xlon .lt. xlonmn) xlonmn = xlon
        if (xlat .gt. xlatmx) xlatmx = xlat
        if (xlon .gt. xlonmx) xlonmx = xlon
   20 continue
      dx = (xdelmx-xdelmn)*0.1
      do 30  i = 1,10
        x = xdelmn + dx*i
        call xydel2latlon (xlatc, xlonc, x, ydelmx, xlat, xlon)
        if (xlat .lt. xlatmn) xlatmn = xlat
        if (xlon .lt. xlonmn) xlonmn = xlon
        if (xlat .gt. xlatmx) xlatmx = xlat
        if (xlon .gt. xlonmx) xlonmx = xlon
   30 continue
      dy = (ydelmx-ydelmn)*0.1
      do 40  i = 1, 10
        y = ydelmn + dy*i
        call xydel2latlon (xlatc, xlonc, xdelmn, y, xlat, xlon)
        if (xlat .lt. xlatmn) xlatmn = xlat
        if (xlon .lt. xlonmn) xlonmn = xlon
        if (xlat .gt. xlatmx) xlatmx = xlat
        if (xlon .gt. xlonmx) xlonmx = xlon
   40 continue
c
      return
      end

c $Id$ 
