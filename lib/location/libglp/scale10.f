      function scale10(x,nx,xpinch,size)
c---------------------------------------------------------------------- 
c     scales input array by increments of powers of ten.  that is 
c  the scaling is increased by higher and higher powers of ten until
c  the input array would be plotting as a substantial size. 
c  warning - the routine is designed mostly for plots with the x
c  axis near the center of the plot because scaling is done with
c  respect to the absolute value of the x values. 
c 
c  arguments- 
c 
c      x-input array to be rescaled.
c      nx - length of x 
c      xpinch - initial x per inch scale.  the scaling constant 
c           returned is obtained by deceasing this value by orders
c           of magnitude to a limit set in a data statement.
c           (currently 50 orders of magnitude)
c      size - size in inches from x=0.0 to closest plot boundary. 
c---------------------------------------------------------------------- 
      dimension x(nx) 
      data maxtry/50/ 
      xmax=0.0
c--scan for the largest x in absolute value.
      do 100 i=1,nx 
  100      xmax=amax1(xmax,abs(x(i))) 
      xmax=xmax/xpinch
c--scale to a plot size one order of magnitude smaller than that
c--which would cause clipping of the plot.
      do 200 i=1,maxtry 
           dec = 10.0**i
           test=xmax*dec
           if(test.gt.size) then
                scale10=xpinch/(10.0**(i-1))
                return
           endif
  200 continue
      scale10=xpinch
      return
      end 

c $Id$ 
