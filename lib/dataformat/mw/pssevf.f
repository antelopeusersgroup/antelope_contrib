      double precision function pssevf(x,work,n)
c
c  core function for pssev
c
c  calls sft
c
      implicit double precision (a-h,o-z)
      parameter (pi=3.141592653589793d0,tpi=2.*pi)
      om=tpi*x
      call sft(work,n,om,ct,st)
      pssevf=ct*ct+st*st
      return
      end

c $Id$ 
