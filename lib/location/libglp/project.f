      subroutine project(v,n1v,nrow,nv,x,work)
c---------------------------------------------------------------
c        projects vector x onto subspace spanned by the columns of
c   the array v by forming the matrix product v*v(trans)x.
c  (note   remember to be a projector the columns of v must 
c  be orthonormal.) 
c 
c  arguments- 
c    v - array of containing the basis vectors defining the 
c        subspace x is to be projected onto.
c        these vectors are assumed to be stored in the columns of v.
c    n1v-first dimension of v in calling program. 
c    nrow-number of elements in the rows of v and x.
c    nv-number of vectors in v.  (i.e. number of columns in v)
c    x-vector to be projected.  it is overwritten on return by
c      the requested projection.
c    work - work vector of length at least nv.
c 
c  author   gary l. pavlis
c           geophysics program ak-50
c           university of washington
c           seattle, wa  98195  
c 
c  written   march 1983 
c---------------------------------------------------------------------
      integer n1v,nrow,nv 
      real v(n1v,nv),x(nrow),work(nv) 
      integer j 
      do 100 j=1,nv 
           work(j) = sdot(nrow,v(1,j),1,x,1)
  100 continue
      do 150 j=1,nrow 
           x(j) = 0.0 
  150 continue
      do 200 j=1,nv 
           call saxpy(nrow,work(j),v(1,j),1,x,1)
  200 continue
      return
      end 

c $Id$ 
