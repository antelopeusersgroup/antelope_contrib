      subroutine txref(p,zhypo,vel,dep,nvel,t,x,ibottum)
c-----------------------------------------------------------------
c      calculates travel times for ray with ray parameter p at
c  source depth zhypo.  this procedure has two entry points 
c  corresponding to the two possible rays for a given joining a 
c  source at depth zhypo to the surface each with ray parameter p 
c  they are 
c 
c    txref - calculates travel time and distance for refracted
c            ray from source at zhypo to the surface
c    txup  - calculates travel time and distance for direct (upward)
c            ray from source at zhypo to the surface
c 
c  both entry points work by shooting a ray with ray parameter
c  p using the velocity model that is obtained by connecting the
c  velocity, depth grid points (in arrays vel and dep) by linear
c  gradients. 
c 
c  arguements-
c    p    - ray parameter of ray to calculate t and x for 
c    zhypo- source depth
c    vel  - array of length nvel holding velocity model 
c    dep  - array of length nvel holding depths corresponding to
c           the entries in vel array. (dep is assumed to be in
c           ascending order)
c    nvel - length of vel and dep arrays. 
c    t    - travel time calculated
c    x    - epicentral distance calculated
c    ibottum - index defining bottum of ray.  ibottum has a different 
c              meaning for the two different entry points.
c 
c             entry txref 
c             dep(ibottum) lies directly above the ray bottuming point  
c 
c             entry txup
c             dep(ibottum) is directly above the source depth 
c 
c  diagnostics- 
c   (1)if p is an illegal ray parameter for this source depth 
c       t and x are set to a large number 
c   (2) if zhypo is too deep so that zhypo.gt.dep(nvel) both  
c       entry points return the time and distance for a ray of
c       ray parameter p to traverse the model.  however, the  
c       sign of t and x is reversed in this case so t and x 
c       are both negative.  this feature can be exploited to  
c       calculate crustal travel times for distance sources, for
c       example.
c  written - july 1981
c  language- 1977 ansi fortran  
c  author  - gary l. pavlis 
c-----------------------------------------------------------------
      integer nvel
      real vel(nvel),dep(nvel)
      real p,t,x,zhypo
c--up is used to identify entry point 
      logical up
      real vwork(2),dwork(2)
c--needed to test for error return from txint or txhalf 
      parameter(flag=1.0e20)
      up = .false.
      call txhalf(p,vel,dep,nvel,thalf,xhalf,ibottum) 
      go to 100 
      entry txup(p,zhypo,vel,dep,nvel,t,x,ibottum)
      up = .true. 
  100 continue
c--calculate time and distance from the surface to the source 
c--for direct ray.
c--scan will mark iz as the point directly above the source 
      iz = izptr(zhypo,dep,nvel)
      call txint(p,vel,dep,iz,tiz,xiz)
      if((iz.ge.nvel).and.(zhypo.ne.dep(nvel))) then
            t = - tiz 
            x = - xiz 
            return
      endif 
      if(zhypo.eq.dep(iz)) then 
            dt = 0.0  
            dx = 0.0  
      else  
            vwork(1) = vel(iz)  
            dwork(1) = dep(iz)  
            vwork(2) = vel(iz) + (zhypo-dep(iz))*(vel(iz+1)-vel(iz))
     $                           /(dep(iz+1)-dep(iz)) 
            dwork(2) = zhypo
            call txint(p,vwork,dwork,2,dt,dx) 
      endif 
      tdown = tiz + dt
      xdown = xiz + dx
      if(tdown.gt.flag) then
            t = tdown 
            x = xdown 
            return
      endif 
      if(up) then 
            t = tdown 
            x = xdown 
            ibottum = iz
      else  
            t = 2.0*thalf - tdown 
            x = 2.0*xhalf - xdown 
      endif 
      return
      end 

c $Id$ 
