      subroutine upefrr(pefn,ndata,m,sym,spefn,flag)
c
c  unpack eigenfunction, real-to-real
c
      implicit double precision (a-h,o-z)
      dimension pefn(m),spefn(ndata)
      logical flag
      nr=ndata
      do 100 n=1,m
        spefn(n)=pefn(n)
        spefn(nr)=sym*pefn(n)
  100   nr=nr-1
      if(flag)spefn(m+1)=0.
      return
      end

c $Id$ 
