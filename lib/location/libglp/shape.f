      function shape1(xlow,ylow,xhigh,yhigh,f,x,y) 
c-----------------------------------------------------------------------
c       interpolates the function f(x,y) at the point (x,y) within the  
c  interior of a rectangular region for which the value of f is known 
c  at the corners.  interpolation is by linear serendipity shape
c  functions ala finite elements.  (see zienkiewicz, 'the finite element
c  method in engineering science', page 107 of the second edition)
c 
c  arguments- 
c 
c     (xlow,ylow)-x,y coordinates of lower left corner of element 
c     (xhigh,yhigh)-x,y coordinates of upper right hand corner of 
c                   element.
c              (note this requires xhigh.gt.xlow and yhigh.gt.ylow) 
c     f-four dimensional array of functional values at the corners
c         f(1)=f(xlow,ylow) 
c         f(2)=f(xhigh,ylow)
c         f(3)=f(xlow,yhigh)
c         f(4)=f(xhigh,yhigh) 
c 
c     (x,y)-x,y coordinates at which f is to be evaluated.
c 
c  alternate entry point-shape2 
c     evaluates f(x,y) using interpolation coefficients calculated in 
c     previous call to shape. 
c-----------------------------------------------------------------------
      dimension f(4),shapefc(4) 
      save shapefc
c--convert to local coordinates 
      xzero=(xlow+xhigh)/2. 
      yzero=(ylow+yhigh)/2. 
      dx=xhigh-xzero
      dy=yhigh-yzero
      zeta=(x-xzero)/dx 
      eta=(y-yzero)/dy
c--evaluate shape functions at (x,y) to obtain interpolation
c--coefficients 
      shapefc(1)=(1.-zeta)*(1.-eta)/4.
      shapefc(2)=(1.+zeta)*(1.-eta)/4.
      shapefc(3)=(1.-zeta)*(1.+eta)/4.
      shapefc(4)=(1.+zeta)*(1.+eta)/4.
      entry shape2(xlow,ylow,xhigh,yhigh,f,x,y) 
      sum=0.
      do 100 i=1,4
          sum=sum+shapefc(i)*f(i) 
  100 continue
      shape=sum 
      shape2=sum
      return
      end 

c $Id$ 
