      subroutine zerolt(a,n1a,n)
c---------------------------------------------------------------
c        sets lower triangle of n x n matrix stored in the
c  array a to zeros.  
c 
c  arguments- 
c    a  - two dimensional array containing matrix to operate on.
c    n1a- first dimension of a in calling program 
c    n  - number of rows and columns in a 
c 
c  author 
c    gary l. pavlis 
c    geophysics program ak-50 
c    university of washington 
c    seattle, wa  98195 
c 
c  written   nov. 1982
c---------------------------------------------------------------
      real a(n1a,n) 
      integer n1a,n 
      integer i,j 
      do 200 j=1,n-1  
           do 100 i=j+1,n 
                a(i,j) = 0.0
  100      continue 
  200 continue
      return
      end 

c $Id$ 
