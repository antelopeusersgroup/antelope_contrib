      subroutine sxpy(n,x,incx,y,incy)
c-------------------------------------------------------------------- 
c       blas type routine to do vector operation y <- x + y 
c  for vectors x and y of length n. 
c 
c  arguments- 
c    n - length of vectors x and y
c    x - vector to be added to y
c    incx - storage increment of x
c    y - vector to which x is to be added (overwitten by x+y).
c    incy - storage increment of y. 
c 
c  note   incx and incy are usually either 1 or the leading 
c         dimension of a two dimensional array. 
c 
c  written - june 1982
c  author - gary l. pavlis
c           geophysics program ak-50
c           university of washington
c           seattle, wa  98195  
c  language - 1977 ansi standard fortran  
c-------------------------------------------------------------------- 
      integer n,incx,incy 
      real x(n),y(n)  
      if((incx.eq.1).and.(incy.eq.1)) then
c--special block for vectors stored in sequential storage.
c--most compilers will generate lightning fast code for this case.
            do 100 i=1,n
                  y(i) = x(i) + y(i)
  100       continue  
      else  
            do 200 i=1,n
                  ix = (i-1)*incx + 1 
                  iy = (i-1)*incy + 1 
                  y(iy) = x(ix) + y(iy) 
  200       continue  
      endif 
      return
      end 

c $Id$ 
