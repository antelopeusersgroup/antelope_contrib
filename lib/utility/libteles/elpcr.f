c
c    A set of subroutines useful for spherical-earth calculations
c    ellipticity subs modified from ken creager's subs
c
	subroutine elpcr (elat,delta,azdg,depth,elpc,iphase,ic)
c
c   Compute ellipticity correction from dziewonski and gilbert
c   Geophys. J. R. Astr. Soc. (1976) 44, 7-17.
c   elat   =  epicentral latitude
c   delta  =  distance in degrees
c   azdg   =  azimuth from epicenter to receiver in degrees
c   depth  =  earthquake depth in km.
c   iphase =  0 for p or 1 for s
c   elpc   =  ellipticity correction in seconds
c   if input latitude is in geographic (?geocentric?) degrees   ic=0
c   if input latitude is in geocentric radians   ic=1
c
	common /elcor/  ecpd(37), ecp0(37,3), ecp1(37,3), ecp2(37,3),
     +    ecsd(22), ecs0(22,3), ecs1(22,3), ecs2(22,3)
	pi = 3.1415927
	if ( delta .lt. 0.0 )   go to 999
	if ( delta .gt. 180.0 ) go to 999

c   interpolate over distance and depth to get tau0, tau1, and tau2.

	if (iphase.eq.1) then 
	   if ( delta .gt. 105. )  go to 999
	   call find ( ecsd, delta, 22, idl )
	   y = ( delta - ecsd(idl) ) / ( ecsd(idl+1) - ecsd(idl) )
	else
	   if ( delta .gt. 180. )  go to 999
	   call find ( ecpd, delta, 37, idl )
	   y = ( delta - ecpd(idl) ) / ( ecpd(idl+1) - ecpd(idl) )
	end if

	if ( depth .le. 300. )  then
	   idt = 1
	   x = depth / 300.
	   if (x .lt. 0.0) x=0.0
	else
	   idt = 2
	   x = (depth -  300. ) /   350.
	   if ( x .gt. 1.0 ) x = 1.0
	end if
	a = 1 - x - y + x*y
	b = x - x*y
	c = y - x*y
	d = x*y
	if (iphase.eq.1) then

	  tau0 = ecs0(idl,idt)*a + ecs0(idl,idt+1)*b + ecs0(idl+1,idt)*c +
     #	   ecs0(idl+1,idt+1)*d
	  tau1 = ecs1(idl,idt)*a + ecs1(idl,idt+1)*b + ecs1(idl+1,idt)*c +
     #	   ecs1(idl+1,idt+1)*d
	  tau2 = ecs2(idl,idt)*a + ecs2(idl,idt+1)*b + ecs2(idl+1,idt)*c +
     #	   ecs2(idl+1,idt+1)*d
    
	else

	  tau0 = ecp0(idl,idt)*a + ecp0(idl,idt+1)*b + ecp0(idl+1,idt)*c +
     #	   ecp0(idl+1,idt+1)*d
	  tau1 = ecp1(idl,idt)*a + ecp1(idl,idt+1)*b + ecp1(idl+1,idt)*c +
     #	   ecp1(idl+1,idt+1)*d
	  tau2 = ecp2(idl,idt)*a + ecp2(idl,idt+1)*b + ecp2(idl+1,idt)*c +
     #	   ecp2(idl+1,idt+1)*d
    
	endif

	theta = elat
	if (ic .eq. 0)  theta = theta * pi/180.
	theta = pi/2. - theta
	az = azdg * pi/180.
	c0 = ( 1. + 3. * cos(2.*theta) ) /4.
	c1 = sin(2.*theta) * cos(az) * sqrt(3.) / 2.
	c2 = sin(theta)*sin(theta) * cos(2.*az) * sqrt(3.) / 2.
	elpc = c0*tau0 + c1*tau1 + c2*tau2
	return
999	print 160 , delta,phase
160	format('delta = ',f8.1,'  which is out of the ellipticity',
     1'   correction range for ',a4,'phase')
	elpc = 0.0
	return
	end
