      subroutine iquell(x,nx) 
c---------------------------------------------------------------------
c     iquell does quelling by integration on the array x.  this 
c  version is designed for the travel time problem so it uses only a
c  simple sum.
c---------------------------------------------------------------------
      dimension x(nx) 
      do 100 i=1,nx-1 
          ii = nx - i 
          x(ii) = x(ii) + x(ii+1) 
  100 continue
      return
      end 

c $Id$ 
