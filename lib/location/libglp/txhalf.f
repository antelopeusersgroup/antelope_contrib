      subroutine txhalf(p,vel,dep,nvel,t,x,ibottom) 
c---------------------------------------------------------------------
c     this routine calculates travel times and distances to the 
c  point at which a ray with ray parameter p would bottom assuming
c  a linear gradient between successive velocity grid points. 
c  very small gradients are treated as constant velocities to 
c  prevent overflow.  
c 
c  arguements-
c 
c     p=(input)=ray parameter of this ray.
c 
c     vel,dep,nvel-(inputs)-vel and dep are real arrays of length nvel. 
c                  vel(i) is the velocity at depth dep(i).
c                  dep(1) can be arbitray but the routine assumes 
c                  dep(i) is less than dep(i+1) for all i.
c                  procedure returns immediately if nvel.le.1 
c                  and sets t=x=0 
c 
c     t - travel time from surface to ray bottom. 
c     x - epicentral distance from surface to ray bottom. 
c     ibottom=(output)=this ray was found to bottum between 
c             depth(ibottom) and depth(ibottum+1).  if ibottum.eq.nvel
c             the ray did not bottom. 
c 
c  written-july 1981 as a minor modification of subroutine dttime 
c  author-gary l. pavlis
c  language-1977 ansi standard fortran
c  32bit floating point word version
c 
c-------------------------------------------------------------------- 
      real vel(nvel),dep(nvel)
      double precision si1,si2,rad1,rad2
      parameter(gradmin=0.0001) 
c--initialize 
      t = 0.0 
      x = 0.0 
      if(nvel.le.1) return
c--be careful of completely illegal values of p 
      if((p*vel(1)).ge.1.0) then
          ibottom=0 
          return
      endif 
      si1 = p*vel(1)  
      rad1=dsqrt(1.-si1)*dsqrt(1.+si1)
      do 200 i=1,nvel-1 
          si2=p*vel(i+1)
          vgrad=(vel(i+1)-vel(i))/(dep(i+1)-dep(i)) 
c--return when the bottom of the ray is reached.
c--that point is special
          if(si2.ge.1) then 
                 ibottom=i
                 if(p.eq.0.) then 
                      x = 0.0 
                 else 
                      x = x + rad1/(vgrad*p)
                 endif
                 t = t + (1./vgrad)*dlog((1.+rad1)/si1) 
                 return 
          endif 
c--test for small gradients.  if the gradient is too small treat that 
c--layer as a constant velocity to prevent overflow.
          if(abs(vgrad).lt.gradmin) then  
                 ddep=dep(i+1)-dep(i) 
                 x = x + p*vel(i)*ddep/rad1 
                 t = t + ddep/(vel(i)*rad1) 
                 rad2 = rad1
          else
                 rad2=dsqrt(1.-si2)*dsqrt(1.+si2) 
                 x = x + (rad1-rad2)/(vgrad*p)
                 t = t + (1./vgrad)*dlog((dble(vel(i+1))/dble(vel(i)))* 
     $                               ((1.+rad1)/(1.+rad2))) 
          endif 
          si1 = si2 
          rad1 = rad2 
  200 continue
      ibottom=nvel
      if(p.eq.0.) then
           x = 0.0
      endif 
      return
      end 

c $Id$ 
