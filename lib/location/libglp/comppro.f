      subroutine comppro(v,n1v,nrow,nv,x) 
c---------------------------------------------------------------
c        projects vector x onto subspace perpendicular to 
c  orthonormal basis vectors stored in array v. (i.e. its orthogonal
c  complement). 
c 
c  arguments- 
c    v - array of containing orthonormal basis vectors defining the 
c        subspace whose complement x is to be projected onto. 
c        these vectors are assumed to be stored in the columns of v.
c    n1v-first dimension of v in calling program. 
c    nrow-number of elements in the rows of v and x.
c    nv-number of vectors in v.  (i.e. number of columns in v)
c    x-vector to be projected.  it is overwritten on return by
c      the requested projection.
c 
c  author   gary l. pavlis
c           geophysics program ak-50
c           university of washington
c           seattle, wa  98195  
c 
c  written   january 1983 
c---------------------------------------------------------------------
      integer n1v,nrow,nv 
      real v(n1v,nv),x(nrow)
      integer j 
      real scale
      do 100 j=1,nv 
           scale = sdot(nrow,v(1,j),1,x,1)
           call saxpy(nrow,-scale,v(1,j),1,x,1) 
  100 continue
      return
      end 

c $Id$ 
