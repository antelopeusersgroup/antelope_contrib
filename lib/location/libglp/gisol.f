      subroutine gisol(v,n1v,nrow,nsval,sval,x,utb,svdcut,nkeep)
c---------------------------------------------------------------- 
c        this procedure solves a system of least squares
c  equations by a trucated singular value decomposition.
c  this is approximately equivalent to the moore-penrose
c  generalized inverse solution.  procedure is intimately 
c  connected with subroutine lsvdf in imsl software library in
c  that it uses the output of that routine to form the solution.
c  warning - routine assumes singular values are ordered in a 
c  decreasing sequence. 
c 
c  arguments- 
c     v - array containing matrix v of singular value decomposition 
c         ( svd of a matrix a = u*s*v(trans)
c     n1v - leading dimension of v
c     nrow - number of rows in v. 
c     nsval - number of singular values of original matrix
c             decomposed by lsvdf.
c     sval - vector of length at least nsval containing singular
c            values.  
c     x - vector of length at least nsval to contain solution.
c     utb - data vector found as the product u(trans)*b by
c           routine lsvdf.  (b is the initial rhs vector in 
c           the initial set of equations ax=b)
c           on return the first nkeep elements of vector utb are
c           overwritten by utb(i)/sval(i) .  this is equivalent 
c           to the matrix multiplication lambda(-1)*utb.
c     svdcut - cutoff value on singular values.  only the 
c              singular values larger than svdcut are used in the 
c              final solution.  
c     nkeep - number of singular values actually used to obtain 
c             the solution.  (i.e. on return this routine found 
c             nkeep singular values larger than svdcut) 
c 
c  language - 1977 ansi standard fortran  
c  written - march 1981 
c---------------------------------------------------------------- 
      real v(n1v,nsval),sval(nsval),utb(nsval),x(nsval) 
      nkeep = numbig(sval,nsval,svdcut) 
      do 150 i=1,nkeep
          utb(i) = utb(i)/sval(i) 
  150 continue
      do 200 i=1,nrow 
            x(i) = sdot(nkeep,utb,1,v(i,1),n1v) 
  200 continue
      return
      end 

c $Id$ 
