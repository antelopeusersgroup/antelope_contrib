      subroutine efnnr(efn,ndata,m,ndmax,flag)
c
c  eigenfunction normalization
c
      implicit double precision (a-h,o-z)
      dimension efn(ndmax)
      logical flag
      sn=0.
      do 200 n=1,ndata
  200   sn=sn+efn(n)*efn(n)
      sn=1./sqrt(sn)
      if((flag.and.(efn(m+2).lt.0.)).or.
     $   ((.not.flag).and.(efn(m+1).lt.0.)))sn=-sn
      do 300 n=1,ndmax
  300   efn(n)=sn*efn(n)
      return
      end

c $Id$ 
