      subroutine mtrx0(a,n1a,nrowa,ncola) 
c---------------------------------------------------------------------
c        initializes two dimensional array a to zero. 
c 
c  arguements-
c 
c   a - array to be initialized.
c   n1a - leading dimension of a
c   nrowa - number of rows of a to be initialized.
c   ncola - number of columns of a to be intialized.
c-------------------------------------------------------------------- 
      dimension a(n1a,ncola)
      do 100 j=1,ncola
            do 100 i=1,nrowa
  100             a(i,j) = 0.0  
      return
      end 

c $Id$ 
