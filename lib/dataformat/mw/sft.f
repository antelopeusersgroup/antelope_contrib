      subroutine sft(x,n,om,ct,st)
c
c  calculates fourier transform of real sequence x(i),i=1,...n
c  at angular frequency om normalized so that nyquist=pi. the sine
c  transform is returned in st and the cosine transform in ct.
c  algorithm is that of goertzal with modifications by
c  gentleman, comp.j. 1969
c  transform is not normalized
c  to normalize one-sided ft, divide by sqrt(data length)
c  for positive om, the ft is defined as ct-(0.,1.)st or like slatec
c  cfftf
c
      implicit double precision (a-h,o-z)
      parameter (pi=3.141592653589793238d0,tp=2.d0*pi)
      dimension x(n)
      np1=n+1
      l=6.d0*om/tp
      s=sin(om)
      a=0.d0
      c=0.d0
      d=0.d0
      e=0.d0
      if(l.eq.0)then
c  recursion for low frequencies (.lt. nyq/3)
        b=-4.d0*sin(om/2.d0)**2
        do 10 k=1,n
          c=a
          d=e
          a=x(np1-k)+b*d+c
   10     e=a+d
      elseif(l.eq.1)then
c  regular goertzal algorithm for intermediate frequencies
        b=2.d0*cos(om)
        do 20 k=1,n
          a=x(np1-k)+b*e-d
          d=e
   20     e=a
      else
c  recursion for high frequencies (.gt. 2*nyq/3)
        b=4.d0*cos(om/2.d0)**2
        do 30 k=1,n
          c=a
          d=e
          a=x(np1-k)+b*d-c
   30     e=a-d
      endif
      st=-s*d
      ct=a-b*d/2.d0
      return
      end

c $Id$ 
