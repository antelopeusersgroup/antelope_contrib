      subroutine shootup
c-----------------------------------------------------------------------
c       this routine calculates a set of ray parameters for use with
c  deep sources.  none of the rays found in this way would ever 
c  bottom in the velocity section.  the routine spaces the emergent 
c  rays roughly xmin (enters through common) in distance.  complicated
c  models may produce erratic spacing as the algorithm connects the 
c  first and final velocity points in the array v(i) with a linear
c  gradient and calculates ray parameters which would lead to constant  
c  spacing xmin of rays shot from the bottom of that linear model.
c  if xmin is made too small the program will write a diagnostic and
c  stop when the pup array begins to overflow.
c 
c  modified-
c 
c    december 1980
c      earlier version assumed the final point in the model had the 
c      highest velocity.  the routine failed when that happened because 
c      that point is then flagged as a shadow zone. 
c      this has complicated a previously simple code. 
c-----------------------------------------------------------------------
	include 'table.common' 
	include 'title.common' 
	include 'upward.common'
	include 'surface.common' 
	include 'control.common' 
c--dp is used if bottom of model is in a lvz. 
c--a ray of ray parameter pmin-dp is shot from the bottom of the model. 
      parameter(dp=1.0e-5)
c--vgdef is the default gradient to use in case the 
c--gradient derived from the model is negative. 
      parameter(vgdef=0.05) 
c--fill first part of pup from p
c--scan for minimum p value = maximum velocity in model.
      pmin = p(1) 
      ipmin = 1 
      do 100 i=1,ns 
          if(p(i).le.pmin) then 
               pmin=p(i)
               ipmin = i
          endif 
  100 pup(i)=p(i) 
      np=ns+1 
c--set initial values 
      if(ipmin.eq.ns) then
c--normal case when last point has the highest velocity 
           xlast=x(ns)/2. 
      else  
c--when the last point is in a lvz we have to shoot a ray to  
c--determine the end of the upward branches for a source at the 
c--very bottom of the model.
           pmin=pmin-dp 
           call txint(pmin,v,d,ns,tbot,xlast) 
      endif 
c--always use the bottom point to determine the gradient
c--but be careful that vgrad stays positive.
      zbot=d(ns)
      vgrad=(v(ns)-v(1))/(d(ns)-d(1)) 
      if(vgrad.lt.0.0) vgrad = vgdef
      vzero=v(1)
  200     continue
               if(xlast.le.xmin) go to 300
               xnew=xlast-xmin
               rsq=xnew*xnew+zbot*zbot
               pup(np)=2.*xnew/sqrt((vgrad*rsq)**2+4.*vzero*rsq*
     $                         (vzero+vgrad*zbot))
c--be certain pup values calculated are non bottoming rays. 
c--throw them out if they are not.
               if(pup(np).lt.pmin) np = np + 1
               if(np.gt.npmax) then 
                    print 1000  
 1000               format("0/////fatal error-xmin too small.  ", 
     $                   "execution terminated/////") 
                    stop
               endif  
               xlast=xnew 
               go to 200
  300     continue
      pup(np)=0.
      return
      end 

c $Id$ 
