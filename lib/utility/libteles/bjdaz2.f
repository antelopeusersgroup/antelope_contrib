	subroutine bjdaz2 (zlat0,alon0,zlat,alon,delt,az1,az2,lattype)  
c----------------------------------------------------------------------  
c  calculate geocentric distance, azimuth, and back azimuth from a  
c  reference point to another point on the earth's surface.  
c  
c	bruce julian	 29 dec 1976  
c	modified by d gubbins 23 july 1977  
c	further modified by s roecker 1981  
C	 This version allows inputs of lat or colat
c  
c  input arguments:
c	if lattype = 1 then  
c	zlat0 = geocentric latitude of reference point. 
c	zlat = geocentric latitude of point   
c	if lattype = 0 then  
c	zlat0 = geocentric colatitude of reference point.
c	zlat = geocentric colatitude of point   
c  
c	alon0 = longitude of reference point.  
c	alon = longitude of point  
c  
c  output arguments:  
c	delt = epicentral distance  
c	az1 =  azimuth of the point from the reference point.  
c	az2 =  azimuth of the reference point from the point.  
c  
c  ***  note  ***  
c	   all angles are in radians.  
c	   north latitude and east longitude are positive.  
c	   azimuth is measured clockwise from north.  
c	   if one point is at north or south pole, azimuth from that  
c		 point will be the limit of the azimuth as the pole is  
c		 approached along the meridian whose longitude is given.  
c----------------------------------------------------------------------  
	implicit double precision (a-h,o-z)
	real*4 zlat0,alon0,zlat,alon,delt,az1,az2
	data pii/3.141592653/
	twopi = pii*2.d0
	if (lattype.eq.1) then
	  alat0=pii*0.5d0-dble(zlat0)
	  alat=pii*0.5d0-dble(zlat)
	else
	  alat0 = dble(zlat0)
	  alat = dble(zlat)
	end if
	st0 = dsin(alat0)  
	ct0 = dcos(alat0)  
	ct1 = dcos(alat)  
	s0c1 = st0*ct1  
	st1 = dsin(alat)  
	s1c0 = st1*ct0  
	dlon = dble(alon) - dble(alon0)
	sdlon = dsin(dlon)  
	cdlon = dcos(dlon)  
	cdelt = st0*st1*cdlon + ct0*ct1  
	b = s0c1 - s1c0*cdlon  
	a = st1*sdlon  
	sdelt = dsqrt(b*b+a*a)  
	ddelt = datan2(sdelt, cdelt)  
	aze = 0.d0
	if (sdelt.ne.0.d0) aze = datan2(a,b)  
c------calculate back azimuth  
	a = -sdlon*st0  
	b = s1c0 - s0c1*cdlon  
	azs =pii
	if (sdelt.ne.0.d0) azs = datan2(a,b)  
c------make  0 < azimuth < twopi  
	if(aze .lt. 0.d0) aze = aze + twopi  
	if(azs .lt. 0.d0) azs = azs + twopi  
	delt = ddelt
	az1 = aze  
	az2 = azs  
	return  
	end  

