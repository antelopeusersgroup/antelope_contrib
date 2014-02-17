      subroutine surftt(vel,dep,nvel,step)
c-----------------------------------------------------------------------
c        this routine calculates travel times for surface focus 
c  sources.  travel times, distances, and ray parameters from 
c  this routine are used later to calculate travel times from buried
c  sources. 
c 
c  arguments- 
c    vel  - array containing discretized velocity model input.
c    dep  - array containing discretized depth points for velocity model
c    nvel - number of points input velocity model is discretized at.
c           (travel times are calculated by connecting vel-dep
c            discretized velocity model with linear gradients.) 
c    step - parameter specifying depth increment of ray bottoms.
c           this routine chooses ray parameters so that rays bottom in  
c           depth increments of step (approximately). 
c-----------------------------------------------------------------------
      real vel(nvel),dep(nvel)
	include 'surface.common' 
c--flaglvz should be less than value set in txint to flag low 
c--velocity zones but sufficiently large to avoid problems from 
c--low gradient areas.
      parameter(flaglvz=1.0e20) 
c--dxmax is used to flag large jumps in distance that rays emerge.
c--number given here is appropriate for local network dimensions in 
c--kilometers 
c--no action is taken on this problem except for a warning diagnostic.
      parameter(dxmax=50.0) 
      real xlast
      save xlast
c--getp spits up vel-dep input to determine ray parameters
      call getp(vel,dep,nvel,step)
      if(ns.eq.nsmax) then
           print *,' warning-model may be truncated', 
     $           ' or too coarsely traced at the bottom.' 
           print *,' tracing arrays have overflowed'
      endif 
      print 1030
 1030 format(1h1) 
      print '(1h )' 
      print 1060
      print 1070
 1060 format(5x,'vbot',11x,'zbot',9x,'p',11x,'distance',7x, 
     $         'travel time') 
 1070 format(3x,'(km/sec)',9x,'(km)',6x,'(1/sec)',8x,'(km)',15x,'(sec)')
c--loop on ray parameters 
      xlast = 0.0 
           do 100 j=1,ns
           p(j)=1./v(j) 
           call txint(p(j),v,d,j,t(j),x(j)) 
           t(j)=2.*t(j) 
           x(j)=2.*x(j) 
           print 1080, v(j),d(j),p(j),x(j),t(j) 
           xtest = abs(x(j)-xlast)
           if((xtest.gt.dxmax).and.(xtest.lt.flaglvz)) print *, 
     $          '/////warning(surftt)   large distance jump here/////'
           xlast = x(j) 
  100      continue 
 1080 format(2f15.5,f15.10,2f15.5)
      end 

c $Id$ 
