      function quad(x,y,u)
c-------------------------------------------------------------------
c     quad does a simple quadratic interpolation by lagranges 
c  formula  
c 
c  arguments- 
c 
c     x-array of abscissa values (length at least 3)
c     y-corresponding ordinate values 
c       (quad fits a parabola through the three given point of x and y) 
c     u-abscissa value to evaluate y value at 
c---------------------------------------------------------------------- 
      dimension x(3),y(3) 
      dx31=x(3)-x(1)
      dx32=x(3)-x(2)
      dx21=x(2)-x(1)
      dux3=u-x(3) 
      dux2=u-x(2) 
      dux1=u-x(1) 
      quad = ((dux2*dux3)/(dx21*dx31)) * y(1) 
     1     - ((dux1*dux3)/(dx21*dx32)) * y(2) 
     2     + ((dux1*dux2)/(dx31*dx32)) * y(3) 
      return
      end 

c $Id$ 
