      subroutine mtrans(a,lda,m,n)
c---------------------------------------------------------------------
c        in place matrix transpose routine.  routine here is simple 
c  and slow.  it could be speeded up considerably with unrolled loops.  
c 
c  arguments- 
c    a - array containing matrix to be transposed.
c        this array must be of size equivalent to at least the following
c          real a(lda,max(m,n)) 
c    lda - first dimension of a in calling program. 
c    m - actual number of rows in a 
c    n - number of columns in a.
c 
c  author   gary l. pavlis
c           geophysics program ak-50
c           univ. of wash.
c           seattle, wa  98195  
c 
c  written   march 1983 
c---------------------------------------------------------------------
      integer m,n,lda 
      real a(1) 
      do 150 j=1,n
           do 100 i=j,m 
                irow = i + lda*(j-1)
                icol = j + lda*(i-1)
                if(irow.ne.icol) then 
                     temp = a(irow) 
                     a(irow) = a(icol)
                     a(icol) = temp 
                endif 
  100      continue 
  150 continue
      return
      end 

c $Id$ 
