      subroutine cputlt(a,lda,n)
c------------------------------------------------------------------ 
c        copies upper triangle to lower triangle of a square  
c  matrix or vice versa.  useful in conjunction with matrix 
c  routine that reference only half of a symmetric matrix.
c 
c  entry points-
c    cputlt - copies upper triangle to lower. 
c    cpltut - copies lower triangle to upper. 
c 
c  arguments- 
c    a   - n by n matrix to do copy to. 
c    lda - leading dimension of a 
c    n   - number of rows and columns of matrix a.  
c------------------------------------------------------------------ 
      real a(lda,n) 
      do 150 j=1,n-1  
            do 100 i=j+1,n
                  a(i,j) = a(j,i) 
  100       continue  
  150 continue
      return
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++
      entry cpltut(a,lda,n) 
      do 250 j=1,n-1  
            do 200 i=j+1,n
                  a(j,i) = a(i,j) 
  200       continue  
  250 continue
      return
      end 

c $Id$ 
