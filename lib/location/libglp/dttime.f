      subroutine dttime(p,vel,dep,nvel,dt,dx,ibottom) 
c---------------------------------------------------------------------
c     this routine calculates incremental travel times assuming 
c  a linear gradient between successive velocity grid points. 
c  very small gradients are treated as constant velocities to 
c  prevent overflow.  
c 
c  arguments- 
c 
c     p=(input)=ray parameter of this ray.
c 
c     vel,dep,nvel-(inputs)-vel and dep are real arrays of length nvel. 
c                  vel(i) is the velocity at depth dep(i).
c                  dep(1) can be arbitray but the routine assumes 
c                  dep(i) is less than dep(i+1) for all i.
c                  nvel must be at least two or a mode  error will
c                  almost surely follow.  
c 
c     dt=(output)=array of incremental travel times.  dt(i) gives the 
c                 travel time spent by the ray of ray parameter p in
c                 layer i.
c 
c     dx=(output)=array of incremental distances. 
c                 dx(i) gives the horizontal distance added to the
c                 ray as the ray passes through layer i.
c 
c     ibottom=(output)=this ray was found to bottom between 
c             depth(ibottom) and depth(ibottom+1).  if ibottom.eq.nvel
c             the ray did not bottom. 
c 
c  written-january 1980 
c  author-gary l. pavlis
c  language-1977 ansi standard fortran
c  modified - november 1980 
c       earlier version calculated travel times and distances 
c    incorrectly for negative gradients.
c    also removed inefficiency of old version which explicitly
c    recalculated si1 and rad1 each time through the main loop
c    this is unnecessarily slow because square roots are fairly slow
c    to compute.
c 
c-------------------------------------------------------------------- 
      real vel(nvel),dep(nvel)
      real dt(nvel),dx(nvel)
      double precision si1,si2,rad1,rad2
      parameter(gradmin=0.0001) 
c--initialize 
      do 100 i=1,nvel 
      dx(i) = 0.
  100 dt(i)=0.
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
                      dx(i) = 0.
                 else 
                      dx(i) = rad1/(vgrad*p)
                 endif
                 dt(i)=(1./vgrad)*dlog((1.+rad1)/si1) 
                 return 
          endif 
c--test for small gradients.  if the gradient is too small treat that 
c--layer as a constant velocity to prevent overflow.
          if(abs(vgrad).lt.gradmin) then  
                 ddep=dep(i+1)-dep(i) 
                 dx(i) = p*vel(i)*ddep/rad1 
                 dt(i)=ddep/(vel(i)*rad1) 
                 rad2 = rad1
          else
                 rad2=dsqrt(1.-si2)*dsqrt(1.+si2) 
c--now calculate dx and dt being careful with the p=0 case
                 if(p.le.0.) then 
                       dx(i) = 0. 
                 else 
                       dx(i) = (rad1-rad2)/(vgrad*p)
                 endif
                 dt(i)=(1.0/vgrad)*dlog((dble(vel(i+1))/dble(vel(i)))*
     $                               ((1.+rad1)/(1.+rad2))) 
          endif 
          si1 = si2 
          rad1 = rad2 
  200 continue
      ibottom=nvel
      return
      end 

c $Id$ 
