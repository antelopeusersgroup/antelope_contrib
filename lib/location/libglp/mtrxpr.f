      subroutine mtrxpr(a,nrows,ncol,n1dim,n2dim) 
c------------------------------------------------------------------------ 
c     this subroutine outputs an arbitrary 2x2 matrix in groups of
c     ten columns seperated by double spaces.  the arguments are, 
c         a------matrix to be output (a must be a real array) 
c         nrows--number of rows in a
c         ncol---number of columns in a 
c         n1dim--first dimension of a 
c         n2dim--second dimension of a
c  this version is identical to another subroutine called mtrxout 
c  except this one uses "print " as opposed to the antique "write(6," 
c-------------------------------------------------------------------------
      dimension a(n1dim,n2dim)
 1000 format(10g12.5) 
      if(n2dim.ge.10) go to 110 
          do 100 i=1,nrows
          print 1000, (a(i,j),j=1,ncol) 
  100     continue
      return
  110     continue
      nblocks=ncol/10 
      if(10*(nblocks+1).gt.n2dim) go to 130 
      if(10*nblocks.lt.ncol) nblocks=nblocks+1
          do 120 ib=1,nblocks 
          nstart=10*(ib-1)+1
          nstop=10*(ib-1)+10
          print 1000,((a(i,j),j=nstart,nstop),i=1,nrows)
           print 1001 
 1001     format(1h0) 
  120     continue
      return
  130 continue
          do 140 ib=1,nblocks 
          nstart=10*(ib-1)+1
          nstop=10*(ib-1)+10
          print 1000,((a(i,j),j=nstart,nstop),i=1,nrows)
           print 1001 
  140     continue
      nremain=n2dim-10*nblocks
      nstart=n2dim-nremain   +1 
          do 150 i=1,nrows
          print 1000,(a(i,j),j=nstart,n2dim)
  150     continue
      return
      end 

c $Id$ 
