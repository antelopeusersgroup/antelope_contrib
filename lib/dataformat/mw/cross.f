      subroutine cross(n,data,nar,phi,av)
c
c  calls dotp
c
      dimension data(1),phi(0:nar)
      do 10 i=1,n
   10   data(i)=data(i)-av
      do 20 i=0,nar
   20   call dotp(i,n,phi(i),data)
      return
      end

c $Id$ 
