      subroutine txint(p,vel,dep,j,t,x) 
c---------------------------------------------------------------------- 
c  this routine calculates travel time-t and distance-x for a ray 
c  of ray parameter p.   these are calculated using a linear gradient 
c  approximation and accumulating incremental travel times for the top  
c  j layers.  if p and j are inconsistent t and x are given the large 
c  value flag.  this is a convenient means of flagging low velocity 
c  zones. 
c 
c  arguements-
c    p  -ray parameter of ray for which travel time and distance
c        is to be calculated. 
c    vel-array of velocities
c    dep-array of depths corresponding to entries in vel. 
c    j  -travel times and distances are acculated for a ray traveling 
c        from the surface down to depth of dep(j).  
c    t  -travel time  
c    x  -distance 
c 
c  modifications- 
c    sept. 1980 - almost completely written in structure to use if else 
c                 construction to run 1977 ansi standard fortran. 
c    nov. 1980  - earlier version did not calculate travel times and
c                 distance correctly when the velocity gradient was 
c                 negative.  (i.e. at the top of a low velocity zone) 
c                 replaced slight inefficiency of earlier version in
c                 which square roots (variable names rad1 and rad2) 
c                 where unnecessarily calculated twice. 
c   March 1997 - fixed bug that occasionally caused problems when a
c                low gradient zone was present.  This caused a
c		divide by zero error.  Fixed by adding condtional
c		on rad2 (see below)
c---------------------------------------------------------------------- 
      real vel(j),dep(j)
      double precision si1,si2,rad1,rad2
      parameter(cutoff=1.0 e-10) 
c--cutoff is a processor dependent constant used to prevent 
c--unintentional roundoff errors in calculating ray parameters
c--as the reciprocal of vel(j) points.  cutoff should be set  
c--to about  1.0 e-(nsigf-2) where nsigf is the number of significant 
c--figures for a single precision floating point number.
c--CORRECTION:  double precsion, not single since  si1,si2,rad1,and 
c--rad2 are doubles
      parameter(flag=1.0e20)
      parameter(gradmin=0.0001) 
c--special branch for first entry 
      if(j.le.1) then 
           t=0. 
           x=0. 
           return 
      endif 
c--calculate travel times and distance
c--careful of totally illegal ray values. 
      ndt=j-1 
      dx=0. 
      dt=0. 
      si1=p*vel(1)
      if(si1.gt.1.0) go to 600  
      rad1=dsqrt(1.0-si1)*dsqrt(1.0+si1)
      do 500 i=1,ndt  
           si2=p*vel(i+1) 
           vgrad=(vel(i+1)-vel(i))/(dep(i+1)-dep(i))
c--watch out for rounding errors in calculating si2 
           if(abs(si2-1.0) .lt. cutoff) si2 = 1.0 
c--branch out for lvz 
           if(si2.gt.1.0) go to 600 
           if(abs(vgrad).lt.gradmin) then 
c--branch for very small gradient layers  
                ddep=dep(i+1)-dep(i)
                dx=dx+p*vel(i)*ddep/rad1
                dt=dt+ddep/(vel(i)*rad1)
                rad2 = rad1 
           else 
c--come here for normal velocity points 
c--note forms for dt and dx give the same answer for positive or
c--negative vgrad given the same pairs of velocity, depth points. 
                rad2=dsqrt(1.0-si2)*dsqrt(1.0+si2)
                if(p.gt.0.) dx=dx+(rad1-rad2)/(vgrad*p) 
                dt=dt+(1./vgrad)*dlog((dble(vel(i+1))/dble(vel(i))) 
     $                       *((1.+rad1)/(1.+rad2)))
           endif
c--the bottom velocity grid point will be the top grid point when 
c--we loop back so reset constants si1,and rad1 appropriately.
c--this is slightly more efficient than recalculating the square roots  
c--over each time.
c--march 1997:  added conditional to avoid ever letting rad1 be le 0.0 
           si1 = si2  
           if(rad2.gt.0.0) then
           	rad1 = rad2
           else
		rad1 = cutoff
           endif
  500      continue 
      t=dt  
      x=dx  
      return
c--come here for rays trapped in lvz
  600 t=flag
      x=flag
      return
      end 

c $Id$ 
