      subroutine mtrxadd(a,b,n1a,n1b,nrow,ncol) 
c---------------------------------------------------------------------- 
c       add matrix a and matrix b with sum overwriting b.  makes no 
c  check for consistency of dimensions. 
c  arguments- 
c 
c     a-nrow x ncol array 
c     b-nrow x ncol array.  matrix sum a+b overwrites b on output.
c     n1a-leading dimension of a
c     n1b-leading dimension of b
c     nrow-number of rows in a and b
c     ncol-number of columns in a and b.  
c 
c  utilizes blas
c---------------------------------------------------------------------- 
      dimension a(n1a,ncol),b(n1b,ncol) 
      data inc/1/ 
      do 100 j=1,ncol 
  100      call saxpy(nrow,1.0,a(1,j),inc,b(1,j),inc) 
      return
      end 

c $Id$ 
