      subroutine afree(ndata,a,inca,g,incg,gssq)
c-------------------------------------------------------------------
c        imposes constraint on weights a such that the inner  
c  product of vector a and gsurf is unity.  useful routine for
c  adding constraint in inverse problem using integration 
c  quelling to allow value of the model at 0 to be a free parameter.
c 
c  arguments- 
c 
c    ndata - length of vectors a and g
c    a     - array of weights that are to be rescaled to satisfy
c            constraint.
c    inca  - storage increment of a array ala blas.  use 1 if 
c            a is stored in column fashion.  use first dimension
c            of a if values are stored in rows of a two dimensional 
c            array. 
c    g     - constraint vector of length ndata.  in inversion 
c            this is the value of all data kernels at 0.
c    incg  - storage increment of g array ala blas.  (see above)
c    gssq  - square of l2 norm of gsurf array.  passed for
c            efficiency reason as this routine is expected
c            normally to be called several times within a loop
c            over several sets of weight vectors, a.
c 
c  language - 1977 ansi standard fortran  
c  author   - gary l. pavlis
c  required software support - library containing blas
c  written  - april 1981
c-------------------------------------------------------------------
      real a(ndata),g(ndata)
      real gssq 
      integer ndata 
      real alm
      alm = (sdot(ndata,a,inca,g,incg) - 1.0)/gssq  
      call saxpy(ndata,-alm,g,incg,a,inca)
      return
      end 

c $Id$ 
