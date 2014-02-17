      subroutine ttable 
c-----------------------------------------------------------------------
c       this subroutine is a secondary driver.  the subroutine is one 
c  large loop within which the source depth is incremented by dz
c  on each pass.  the section of the velocity model within this dz
c  increment is placed in the work spaces contained in the common 
c  block called window.  this window is then used when ttime is called  
c  to increment the /upward/ arrays to travel times and distances to
c  this source depth. 
c-----------------------------------------------------------------------
	include 'switch.common'
	include 'table.common' 
	include 'surface.common' 
	include 'control.common' 
	include 'window.common'
      character bnorm,revs,shad 
      parameter(bnorm='d',revs='r',shad='s')
      parameter(flaglvz=1.0e37) 
      parameter(dzcut=1.0e-4) 
      ip=1  
      z=0.  
c--first define branch for surface focus  
      branch(1)=bnorm 
      do 50 i=2,ns
          if(t(i).gt.flaglvz) then
               branch(i)=shad 
          elseif(x(i).gt.x(i-1))then
               branch(i)=bnorm
          else
               branch(i)=revs 
          endif 
   50 continue
      call gridtp(z,p,t,x,branch,dx,ns) 
c--loop on depths 
          do 100 iz=1,nz-1
          z=float(iz)*dz
c--test for special case on first pass
          if(iz.eq.1) then
               vwork(1) = v(1)  
               dwork(1) = d(1)  
          else
               vwork(1)=vwork(nwindow)
               dwork(1)=dwork(nwindow)
          endif 
c--find window and store in work spaces 
c--test for endpoint  
          if(ip.ge.ns) then 
               print 1000 
               return 
          endif 
c--back up the pointer if necessary.
   55     if(z.gt.d(ip)) go to 60 
               ip=ip-1
               go to 55 
   60     continue
          ip=ip+1 
          k=2 
   65     if((z.lt.d(ip)).or.(abs(d(ip)-z).lt.dzcut)) go to 70
               vwork(k)=v(ip) 
               dwork(k)=d(ip) 
               ip=ip+1
               k=k+1  
c--is this a legal depth point.  if not exit gracefully 
               if(ip.eq.ns) then
                    if(abs(z-d(ip)).le.dzcut) then  
                         vwork(k)=v(ip) 
                         dwork(k)=d(ip) 
                         go to 75 
                    else
                        print 1000
 1000               format('/////non-fatal error. nz and dz are not', 
     $                     'consistent.  program exits gracefully/////')
                         return 
                    endif 
               endif  
               go to 65 
   70     continue
c--back the pointer up one so that this source point lies 
c--between d(ip) and d(ip+1). 
          ip = ip - 1 
c--interpolate and add one point to window corresponding to the 
c--remaining increment to the source depth if this is not too close 
c--to the next velocity model point 
           if((d(ip+1)-z).lt.dzcut) then  
               vwork(k)=v(ip+1) 
               dwork(k)=d(ip+1) 
               ip=ip+1
          else
               vwork(k)=v(ip)+(z-d(ip))*(v(ip+1)-v(ip)) 
     $                  /(d(ip+1)-d(ip))
               dwork(k)=z 
          endif 
   75     nwindow=k 
c--calculate the travel time curve
          call ttime(ip)
          call gridtp(z,ptab,ttab,xtab,branch,dx,ntab)
  100     continue
      return
      end 

c $Id$ 
