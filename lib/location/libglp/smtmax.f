      real function smtmax(x,ldx,nrow,ncol) 
c-------------------------------------------------------------------- 
c        scan two dimensional floating point array x and returns
c  largest or smallest value. 
c    entry points-
c      smtmax - returns maximum element of array x. 
c      smtmin - returns minimum element of array x. 
c 
c  arguments- 
c      x   - real nrow by ncol array to be scanned  
c      ldx - first dimension of x in calling program. 
c      nrow- number of rows in x
c      ncol- number of columns in x.
c 
c  language - 1977 ansi standard fortran  
c  written  - july 1981 
c  author   - gary l. pavlis
c------------------------------------------------------------------ 
      integer nrow,ncol,ldx 
      real x(ldx,ncol)
      smtmax = x(1,1) 
      do 150 j=1,ncol 
            do 100 i=1,nrow 
                  smtmax = amax1(x(i,j),smtmax) 
  100       continue  
  150 continue
      return
      entry smtmin(x,ldx,nrow,ncol) 
      smtmin = x(1,1) 
      do 250 j=1,ncol 
            do 200 i=1,nrow 
                  smtmin = amin1(x(i,j),smtmin) 
  200       continue  
  250 continue
      return
      end 

c $Id$ 
