      subroutine ttlvz(delta,hpz,n,v,z,h,term,t,rayp,up)
c -- ttlvz calculates travel time of seismic wave from hypocenter       ttlvz.3 
c -- earth model is plane parallel layers (n-1 layers over halfspace)   ttlvz.4 
c -- n is number of layers including halfspace                          ttlvz.5 
c -- v is array of length n containing layer velocities                 ttlvz.6 
c -- z is array of length n containing depths to layer tops             ttlvz.7 
c -- this routine is designed to work for all layer velocity combinationttlvz.8 
c
c  original code contained fixed size arrays set as follows
c
c     dimension h(40),term(40)                                          ttlvz.9 
c     common /model/ n,v(20),z(20)                                      ttlvz.10 
c
c  In this modification I pass the same variables in as arguments 
c  I've also made all quantities double.  I also modified the original
c  code to now return the ray parameter as well as the travel time. 
c  The original ttlvz code was a fortran function.  Now it is a subroutine.
c  returns time in t and ray parameter in argument p.  I also added a 
c  integer variable "up".  Up is 1 when the ray is a direct ray.  otherwise
c  it will be 0.  
c
c  Original code came, I think, from Robert Crosson.  Modification
c  by G Pavlis, August 1996
c  Editorial note:  I should have burned this and started from scratch.
c
      integer n
      double precision delta, hpz
      double precision v(n),z(n)
      double precision h(2*n), term(2*n)
      double precision t, rayp
      integer up

      double precision offset
      double precision t1, sum, pmax, sdel, term1, sder, temp
      double precision p
c
c  test is used as iteration distance cutoff for upward traveling
c  ray convergence.  To avoid infinite loops, especially in the
c  presence of large delta where roundoff errors become a serious
c  problem we use a maxit parameter to limit total iterations
c  and a relative distance scale factor rdsf.  rdsf rescales
c  the convergence variable test for large delta.  See code below
c  Modified by glp:  August 1996
c
      integer count,maxit
      double precision test, rdsf

      parameter(maxit = 50, rdsf = 1.0e-5)
      test = 0.001
      if(delta*rdsf .gt. test) test = delta*rdsf

      up = 0
      count = 0
 
c -- locate layer containing hypocenter and store index in lhpz         ttlvz.12  
      do 110 j=1,n                                                      ttlvz.13  
      if (hpz.le.z(j)) go to 111                                        ttlvz.14  
110   continue                                                          ttlvz.15  
      lhpz = n                                                          ttlvz.16  
      go to 112                                                         ttlvz.17  
111   lhpz = j-1                                                        ttlvz.18  
      if (lhpz.eq.0) lhpz = 1                                           ttlvz.19  
112   continue                                                          ttlvz.20  
c -- assign internal depths to layer tops including correction for      ttlvz.21  
c    station elevation                                                  ttlvz.22  
      do 120 j=1,n                                                      ttlvz.23  
      if (j-lhpz) 121,122,123                                           ttlvz.24  
121   h(j) = z(j+1)-z(j)                                                ttlvz.25  
      go to 120                                                         ttlvz.26  
122   h(j) = abs(hpz-z(j))                                              ttlvz.27  
      go to 120                                                         ttlvz.28  
123   h(j) = z(j)-z(j-1)                                                ttlvz.29  
120   continue                                                          ttlvz.30  
      t = 1.00e+10                                                      ttlvz.31  
      rayp = 0.0
      if(lhpz.eq.n) go to 360                                           ttlvz.32  
      h(lhpz+1) = z(lhpz+1)-amax1(hpz,z(1))                             ttlvz.33  
c -- calculate smallest refracted wave time                             ttlvz.34  
      istrt = lhpz+1                                                    ttlvz.35  
      vmax = 0.                                                         ttlvz.36  
      do 760 j=1,lhpz                                                   ttlvz.37  
760   vmax = amax1(v(j),vmax)                                           ttlvz.38  
      do 190 lowlr=istrt,n                                              ttlvz.39  
c -- check to see if ray exists                                         ttlvz.40  
      if(v(lowlr).le.vmax) go to 190                                    ttlvz.41  
      vmax = v(lowlr)                                                   ttlvz.42  
      jlim = lowlr-1                                                    ttlvz.43  
c -- calculate offset distance                                          ttlvz.44  
      p = 1./v(lowlr)                                                   ttlvz.45  
      sum = 0.                                                          ttlvz.46  
      do 710 j=1,lhpz                                                   ttlvz.47  
      term(j) = sqrt(1.-(p*v(j))**2)+1.e-10                             ttlvz.48  
710   sum = h(j)*v(j)/term(j)+sum                                       ttlvz.49  
      do 720 j=lhpz,jlim                                                ttlvz.50  
      term(j) = sqrt(1.-(p*v(j))**2)+1.e-10                             ttlvz.51  
720   sum = 2.*h(j+1)*v(j)/term(j)+sum                                  ttlvz.52  
      offset = sum*p                                                    ttlvz.53  
      if (offset-delta) 780,780,190                                     ttlvz.54  
c -- calculate refraction path travel time for lowlr                    ttlvz.55  
780   sum = 0.                                                          ttlvz.56  
      do 730 j=1,lhpz                                                   ttlvz.57  
730   sum = h(j)*term(j)/v(j)+sum                                       ttlvz.58  
      do 740 j=lhpz,jlim                                                ttlvz.59  
740   sum = 2.*h(j+1)*term(j)/v(j)+sum                                  ttlvz.60  
      t1 = delta*p+sum                                                  ttlvz.61  
      if(t1 .lt. t) then
            t = t1
            rayp = p
      endif 

190   continue                                                          ttlvz.63
c
c  special case for source in the first layer 
c  
360   if (lhpz.eq.1) then
            t1 = sqrt(delta**2+(hpz-z(1))**2)/v(1)
            p = sin( atan2(delta,abs(hpz-z(1))) )/v(1)
            if(hpz .lt. z(1) ) p = -p
      else 
c -- calculate direct wave travel time                                  ttlvz.64  
        vmax=v(1)                                                         ttlvz.66  
        do 175 j=2,lhpz                                                   ttlvz.67  
175     vmax=amax1(vmax,v(j))                                             ttlvz.68  
c -- This loop seeks a ray parameter for a direct ray with 
c -- delta > given distance.  The earlier version would sometimes enter
c -- an infinite loop if the given delta was large due to a machine 
c -- precision limitation.  We trap this now with a test for 
c -- the condition that p == pmax.  In this condition, an error is
c -- returned that has to be handled by the caller.  Here this is
c -- signaled by setting the returned time to -1.0.
        pmax = 1./vmax                                                    ttlvz.70  
        p = 0.5*pmax                                                      ttlvz.71  
155     p = (p+pmax)/2.
	if(p.eq.pmax)then
		t = -1.0
		return
	endif

        sdel = 0.                                                         ttlvz.73  
        do 160 j=1,lhpz                                                   ttlvz.74  
160     sdel = v(j)*h(j)/sqrt(1.-(p*v(j))**2)+sdel                        ttlvz.75  
        if (delta-p*sdel) 166,161,155                                     ttlvz.76  
c -- now perform newton convergence from top down                       ttlvz.77  
166     continue                                                          ttlvz.78  
        sdel = 0. 
        sder = 0. 
      do 162 j=1,lhpz                                                   ttlvz.80  
        temp = sqrt(1.-(p*v(j))**2)                                       ttlvz.81  
        term1 = v(j)*h(j)/temp                                            ttlvz.82  
        sdel = term1+sdel                                                 ttlvz.83  
        sder = term1/temp**2+sder                                         ttlvz.84  
162     continue                                                          ttlvz.85  
        dmdp0 = delta-p*sdel                                              ttlvz.86  
c
c  a warning should be issued if the count criteria breaks
c  this loop, but for use in a C program this isn't worth the mess
c  it causes.  It should not happen anyway, but it is always 
c  necessary to avoid infinite loops like this.
c
        if( (abs(dmdp0).lt.test) .or. (count.gt.maxit) )go to 161
        p = dmdp0/sder+p                                                  ttlvz.88  
	count = count + 1
        go to 166                                                         ttlvz.89  
161     continue                                                          ttlvz.90  
c -- p has been determined to sufficient accuracy                       ttlvz.91  
c -- calculate direct wave travel time by summation                     ttlvz.92  
        sum = 0.                                                          ttlvz.93  
        do 180 j=1,lhpz                                                   ttlvz.94  
180     sum = h(j)/(v(j)*sqrt(1.-(p*v(j))**2))+sum                        ttlvz.95  
        t1=sum                                                            ttlvz.96
      endif
      if(t1.lt.t) then
            t = t1
            rayp = p
            up = 1
      endif
      return  
      end                                                               ttlvz.10  

c $Id$ 
