
      double precision function dot(n,x1,inc1,x2,inc2)
c  computes dot product of two d.p. vectors with nonunit
c  incrementing allowed. replacement for blas subroutine sdot.
      implicit double precision (a-h,o-z)
      dimension x1(1),x2(1)
      if(inc2.gt.0)then
        k=1
      else
        k=n*abs(inc2)
      endif
      dot=0.0
      if(inc1.gt.0)then
        do 10 i=1,n,inc1
          dot=dot+x1(i)*x2(k)
          k=k+inc2
   10   continue
      else
        do 20 i=n,1,inc1
          dot=dot+x1(i)*x2(k)
          k=k+inc2
   20   continue
      endif
      return
      end

c $Id$ 
