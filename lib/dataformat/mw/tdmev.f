      subroutine tdmev(fv1,fv2,fv3,n,ndim,ev,efn,m,kerr,work)
c
c  driver to find larger eigenvalues of a real symmetric
c  tridiagonal matrix
c
c  calls tridib,tinvit from eispack
c
      implicit double precision (a-h,o-z)
      dimension fv1(ndim),fv2(ndim),fv3(ndim),efn(ndim,m),ev(m),
     $          work(1)
      nndim=ndim
      mm=m
      nn=n
      nv4=1
      nv5=nn+nv4
      nv6=nn+nv5
      nv7=nn+nv6
      nv8=nn+nv7
      iv1=nn+nv8
c
c  compute the m largest eigenfunctions efn and associated
c  eigenvalues of the symmetric tridiagonal matrix
c  input is in fv1,fv2,fv3 in usual eispack form, see 2.1.17
c
      eps1=0.
      m11=n-m+1
      call tridib(nn,eps1,fv1,fv2,fv3,evlb,evub,m11,mm,ev,
     $            work(iv1),ierr,work(nv4),work(nv5))
      if(ierr.ne.0)kerr=1
      call tinvit(nndim,nn,fv1,fv2,fv3,mm,ev,work(iv1),efn,ierr,
     $            work(nv4),work(nv5),work(nv6),work(nv7),work(nv8))
      if(ierr.ne.0)kerr=2
      return
      end

c $Id$ 
