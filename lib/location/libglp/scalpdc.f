      subroutine scalpdc(x,nx,xzero,xpinch,xmin,xmax) 
c---------------------------------------------------------------------- 
c       scaling routine for converting numbers to physical plotter
c  inches.  
c 
c  arguments- 
c 
c     x-array to be xpinchd to inches 
c     nx-length of x  
c     xzero-dc shift in inches to be added to the numbers in x. 
c     xpinch-scaling parameter to convert the numbers in x to inches
c            on plotting surface. 
c     xmin-minimum allowed x value (smaller values will be set to xmin) 
c     xmax-maximum allowed x value (larger values will be set to xmax)  
c          (both xmin and xmax are used to prevent going outside
c           allowed plotting area)
c---------------------------------------------------------------------- 
      dimension x(nx) 
      do 100 i=1,nx 
           x(i)=xzero+x(i)/xpinch 
           if(x(i).gt.xmax) x(i)=xmax 
           if(x(i).lt.xmin) x(i)=xmin 
  100 continue
      return
      end 

c $Id$ 
