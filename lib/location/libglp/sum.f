      function sum(x,nx)
c-------------------------------------------------------------------
c      finds the sum of the first nx elements of vector x 
c-------------------------------------------------------------------
      dimension x(nx) 
      sum=0.0 
      do 100 i=1,nx 
  100 sum=sum+x(i)
      return
      end 

c $Id$ 
