      real function sumsq(nx,x,inc)                                      sumsq
c---------------------------------------------------------------------   sumsq
c    blas type routine to calculate sum of squared values for vector x.  sumsq
c  does no checking to avoid floating point overflow or underflow        sumsq
c  as does it's blas sibling snrm2.                                      sumsq
c  arguments-                                                           sumsq 
c                                                                        sumsq
c     nx - length of vector x.  (if nx.lt.1 this routine returns         sumsq
c          immediately returning a value of 0)                           sumsq
c                                                                        sumsq
c     x - real vector for which sum of squares of components is to be    sumsq
c         calculated.                                                    sumsq
c                                                                        sumsq
c     inc - storage increments ala blas.                                 sumsq
c           components of x are assumed to be separated by increments of sumsq
c           inc.  useful for working with columns in two dimensional     sumsq
c           arrays.(e.g.  if inc=5, the routine will use x(1),x(6),etc.) sumsq
c           fastest computation uses inc = 1 because most compilers will sumsq
c           optimize the code to use unrolled loops in that case.        sumsq
c           negative inc are permitted and                               sumsq
c           will work the way one expects because of the fact that       sumsq
c           negative increments are permitted on do loops in the new     sumsq
c           standard.  inc=0 forces an immediate return to avoid an      sumsq
c           infinite loop.  in that case sumsq is returned as 0.0        sumsq
c  language - 1977 ansi standard fortran                                 sumsq
c  author - gary l. pavlis                                               sumsq
c----------------------------------------------------------------------  sumsq
      dimension x(nx)                                                    sumsq
      sumsq = 0.0                                                        sumsq
      if((nx.lt.1).or.(inc.eq.0)) return                                 sumsq
      if(inc.eq.1) then                                                  sumsq
            do 100 i=1,nx                                                sumsq
  100             sumsq = sumsq + x(i)**2                                sumsq
      else                                                               sumsq
            do 200 i=1,(nx-1)*inc + 1,inc                                sumsq
  200             sumsq = sumsq + x(i)**2                                sumsq
      endif                                                              sumsq
      return                                                             sumsq
      end                                                                sumsq

c $Id$ 
