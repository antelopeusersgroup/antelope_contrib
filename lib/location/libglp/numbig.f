      integer function numbig(x,nx,xmin)
c---------------------------------------------------------------------- 
c        scans array x for first element less than or equal to
c  cutoff value xmin.  useful primarily for arrays arranged in a
c  decreasing sequence.  returns nx if all elements are larger than 
c  xmin as expected.  
c 
c  arguments- 
c    x - array to be scanned
c    nx - length of x 
c    xmin - cutoff value to test against. 
c---------------------------------------------------------------------- 
      integer nx
      real x(nx)
      real xmin 
      integer i 
      do 100 i=1,nx 
      if(x(i).lt.xmin) then 
            numbig = i - 1
            return
      endif 
  100 continue
      numbig = nx 
      return
      end 

c $Id$ 
