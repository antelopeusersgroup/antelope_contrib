      logical function test0(n,x,inc) 
c------------------------------------------------------------------ 
c       tests a vector x and returns a true value if x is all zeroes. 
c  returns immediately with a value of .false. as soon as a 
c  nonzero element is found.
c 
c  arguments- 
c     n  - length of vector x 
c     x  - vector to be tested  
c     inc- storage increment of vector x ala blas. this argement
c          is useful for numbers stored in two dimensional arrays.
c          inc=1 if x is a normal vector of stored in the rows of 
c          an array.  inc = first dimension of some array if x is 
c          stored in the columns of that array. 
c 
c  required subprograms 
c        none 
c  language - 1977 ansi standard fortran  
c  author   - gary l. pavlis
c  written  - may 1981
c------------------------------------------------------------------ 
      real x(n) 
      integer nend
      nend = (n-1)*inc + 1
      do 100 i=1,nend,inc 
            if(x(i).ne.0.0) then
                  test0 = .false. 
                  return
            endif 
  100 continue
      test0 = .true.  
      return
      end 

c $Id$ 
