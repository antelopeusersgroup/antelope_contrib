      integer function linsrc(a,n,inc,test) 
c-------------------------------------------------------------------- 
c       does a linear search of the first n elements of vector a. 
c  returns position of the first element of a less than or equal to 
c  the input variable "test".  position returned is relative to 
c  starting point in units of input variable "inc".  (e.g. if 
c  linscr returns 3 and inc=2 then the actual position in a is
c  a(5).)  inc may be positive of negative but it is the
c  caller's responsibility to be sure the bounds of a are not 
c  violated.
c 
c  arguments- 
c    a - array to be scanned.  must be of length at least 
c        1 + inc*(n-1)
c    n - maximum number of actual elements of a to scan 
c    inc-skipping increment through array a.  the only normal 
c        choices are +1, -1, or the leading dimension of some 
c        two dimensional array. 
c    test-test value.  linscr returns when if finds a number  
c         less than or equal to test. 
c 
c  written - june 1982
c  author - gary l. pavlis
c           geophysics program ak-50
c           university of washington
c           seattle, wa  98195  
c  language - 1977 ansi standard fortran  
c-------------------------------------------------------------------- 
      integer n,inc 
      real a(n),test  
      do 100 i=1,n
            ii = inc*(i-1) + 1  
            if(a(ii).le.test) then
                  linsrc = i
                  return
            endif 
  100 continue
      linsrc = n
      return
      end 

c $Id$ 
