      subroutine utmul(a,n1a,m,b,n1b,n,wk)
c-----------------------------------------------------------------
c        calculates matrix product of upper triangular matrix 
c  stored in array a and matrix stored in array b.  product 
c  actually calculated is 
c    a(transpose)b
c  procedure is a space economizer as the product overwrites b. 
c  lower triangle of a is never referenced. 
c 
c  a  - m by m upper triangular array to multiply b by. 
c  n1a- first dimension of a in calling program 
c  m  - number of rows and columns in a and number of rows in 
c       matrix b. 
c  b  - m by n matrix to be multiplied.  on output b is overwritten 
c       by the product  a(transpose)b 
c  n  - number of columns in b. 
c  wk - work space vector of length at least m. 
c 
c  required subprograms 
c     sdot and scopy  -  two routine of blas
c  language - 1977 ansi standard fortran  
c  author   - gary l. pavlis
c  written  - may 1981
c-----------------------------------------------------------------
      real a(n1a,m),b(n1b,n)
      real wk(m)
      do 200 j=1,n
            do 100 i=1,m
                  wk(i) = sdot(i,a(1,i),1,b(1,j),1) 
  100       continue  
            call scopy(m,wk,1,b(1,j),1) 
  200 continue
      return
      end 

c $Id$ 
