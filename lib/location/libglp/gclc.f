      subroutine gclc(phio,lamo,phi,lam,x,y)
c -- gclc converts from geodetic coordinates, phi,lam to local
c    coordinates, x,y 
c    correction is made for the change of radius with ellipticity 
c    and for the difference between geodetic and geocentric 
c    coordinates
c    accuracy should be to within 10 meters in 100 km for 
c    latitudes up to about 70 degrees 
c        phio - lat. of coordinate origin 
c        lamo - long. of coordinate origin
c        phi - lat. of point to convert 
c        lam - long. of point to convert  
c        x - x coordinate (north) of converted point
c        y - y coordinate (east) of converted point 
      real phio(3),lamo(3),phi(3),lam(3)
      data r,e,po180/6378.163,0.0033670033,0.017453293/ 
      fr(t) = 1.-e*sin(t)**2
      conv(x,y,z) = (x+y/60.+z/3600.)*po180 
      xlat = conv(phi(1),phi(2),phi(3)) 
      ylon = conv(lam(1),lam(2),lam(3)) 
      jmp = 1 
      go to 120 
c 
 100  continue
      y = rho*(zlon-ylon)*cos(xlat-crlat) 
      x = rho*(xlat-zlat)/cdist 
      return
      entry lcgc(phio,lamo,phi,lam,x,y) 
      jmp = 2 
      go to 120 
c 
 110  continue
      xlat = x*cdist/rho+zlat 
      xl = xlat/po180 
      t1 = abs(xl)+.5e-09 
      xl = sign(t1,xl)
      phi(1) = aint(xl) 
      rmin = (xl-phi(1))*60.
      phi(2) = aint(rmin) 
      phi(3) = (rmin-phi(2))*60.
      ylon = zlon-y/(rho*cos(xlat-crlat)) 
      yl = ylon/po180 
      t1 = abs(yl)+.5e-09 
      yl = sign(t1,yl)
      lam(1) = aint(yl) 
      rmin = (yl-lam(1))*60.
      lam(2) = aint(rmin) 
      lam(3) = (rmin-lam(2))*60.
      return
 120  continue
      zlat = conv(phio(1),phio(2),phio(3))
      zlon = conv(lamo(1),lamo(2),lamo(3))
      crlat = atan(e*sin(2.*zlat)/fr(zlat)) 
      zgcl = zlat-crlat 
      a = fr(zgcl)
      rho = r*a 
      b = (e*sin(2.*zgcl)/a)**2+1.
      c = 2.*e*cos(2.*zgcl)*a+(e*sin(2.*zgcl))**2 
      cdist = c/(a**2*b)+1. 
      go to (100,110), jmp
      end 

c $Id$ 
