      subroutine getp(vel,dep,nvel,step)
c---------------------------------------------------------------------- 
c       replaces earlier version called step2 written by george mcmechan
c  this routine takes the velocity model stored in the arrays vel and 
c  dep and generates a larger model which is stored in the arrays 
c  named v and d by doing a linear interpolation between the points.
c  spacing of the larger model is designed to produce a relatively
c  uniform depth spacing of rays shot with ray parameter 1.0/v(i).
c  arguments- 
c 
c    vel  - array containing discretized velocity model input.
c    dep  - array containing discretized depth points for velocity model
c    nvel - number of points input velocity model is discretized at.
c    step-input variable used to control spacing.  the basic algorithm  
c          is simply that if dep(i)-dep(i-1) .gt.step  the routine
c          interpolates to produce rays bottoming about step apart in 
c          depth.  if dep(i)-dep(i-1).lt.step the routine splits the
c          interval into a fixed number of points (nsplit). 
c    ns-output variable giving final size of vel and dep arrays.
c 
c  written - may 1980 
c  modifications
c     june,1981  -  minor changes in constant velocity block to fix 
c                   bug in older version. 
c---------------------------------------------------------------------- 
	include 'surface.common' 
      real vel(nvel),dep(nvel)
      parameter(gradmax=10.0) 
      parameter(nsplit=20)
c--nsplit is the splitting interval for very large or infinite gradients
      parameter(bigmult=2.0)
c--bigmult determines step size in very low gradient areas
      parameter(dzmin=0.001)
c--dzmin is used to prevent division by zero for large gradients. 
      parameter(flag=1.0e10)
c--gradient is set to flag if dz.lt.dzmin.
      parameter(gradmin=.0001,dvmin=.00001) 
c--parameters used to detect constant or numerically nearly constant
c--velocity layers. 
      v(1)=vel(1) 
      d(1)=dep(1) 
      bigstep=bigmult*step
      i=2 
      j=1 
  50  if((i.gt.nvel).or.(j.ge.nsmax)) go to 150 
           dz=dep(i)-dep(i-1) 
           dv=vel(i)-vel(i-1) 
c--skip this point if it is identical to the last one.  (avoids data
c--errors.) 
           if(((dv+1.0).eq.1.0).and.((dz+1.0).eq.1.0)) go to 100
c--avoid division by zero or indefinite quantities
           if((dz+1.0).eq.1.0) then 
                grad=flag 
           else 
                grad=dv/dz
           endif
           if(abs(grad).lt.gradmin) then  
c--block for small gradient layers. 
c--be extra careful of truly constant velocity layers.
c--the model for our use must be distorted slightly for the constant
c--velocity case because of certainty of floating point overflow. 
                jsplit=nint(dz/bigstep) + 1 
                if(jsplit.le.1) jsplit = 2
                if((j+jsplit).gt.nsmax) then
                     j=j+1
                     if(abs(dv).lt.dvmin) then
                          v(j) = vel(i-1) + dvmin 
                          d(j) = dep(i) 
                     else 
                          v(j) = vel(i) 
                          d(j) = dep(i) 
                     endif
                     ns=j 
                     return 
                endif 
                if(abs(dv).gt.dvmin)  then
                     call split(v(j),d(j),vel(i-1),dep(i-1),
     $                         vel(i),dep(i),jsplit)
                else  
c--block for truely constant velocity grid points.  
c--model will be distorted by adding dvmin to all grid points until one is
c--found that exceeds the check value.
                    do 80 k=i,nvel
                         if(abs(vel(k)-vel(i-1)).gt.dvmin) go to 90 
                         vel(k) = vel(k) + dvmin
   80               continue
   90               continue
                    call split(v(j),d(j),vel(i-1),dep(i-1), 
     $                         vel(i),dep(i),jsplit)
                endif 
                j = j + jsplit - 1
           elseif(grad.ge.flag) then
                if((j+nsplit).gt.nsmax) then
                     j=j+1
                     v(j)=vel(i)
                     d(j)=dep(i) + dzmin  
                     ns=j 
                     return 
                else  
                     call split(v(j),d(j),vel(i-1),dep(i-1),
     $                    vel(i),dep(i)+dzmin,nsplit) 
                     j=j+nsplit-1 
c--make certain d(j) remains an increasing sequence 
                      if(d(j).gt.dep(i)) then 
   60                     if(d(j).le.dep(i)) go to 70 
                               dep(i)=d(j)
                               i=i+1
                               if(i.gt.nvel) go to 70 
                          go to 60
   70                     continue
                           i=i-1
                      endif 
                endif 
           elseif((dz.lt.step)
     $            .or.((grad.gt.gradmax).and.(grad.lt.flag))) then
                if((j+nsplit).gt.nsmax) then
                     j=j+1
                     v(j)=vel(i)
                     d(j)=dep(i)
                     ns=j 
                     return 
                else  
                     call split(v(j),d(j),vel(i-1),dep(i-1),
     $                           vel(i),dep(i),nsplit)
                     j=j+nsplit-1 
                endif 
           else 
                jsplit=nint(dz/step) + 1  
                if((j+jsplit).gt.nsmax) then
                     j=j+1
                     v(j)=vel(i)
                     d(j)=dep(i)
                     ns=j 
                     return 
                elseif(jsplit.eq.1) then  
                     j=j+1
                     v(j) = vel(i)
                     d(j) = dep(i)
                else  
                     call split(v(j),d(j),vel(i-1),dep(i-1),
     $                           vel(i),dep(i),jsplit)
                     j=j+jsplit-1 
                endif 
           endif
  100 continue
      i=i+1 
      go to 50
  150 continue
      ns=j  
      return
      end 

c $Id$ 
