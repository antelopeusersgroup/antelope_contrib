      subroutine rmeq(a,na,ir)
c-------------------------------------------------------------------- 
c        removes elements of a that are redundant.  (i.e. equal)
c  routine assumes array a has been sorted into ascending order.
c  this can be accomplished via imsl routine vsrtr to which this
c  routine is a useful companion.  a second entry point rmeq2 is
c  provided to use the index key array, ir, to preform only the deletion
c  operation on array a.  this is useful for plotting applications
c  to delete redundant x,y data where a typical sequence would be 
c      .
c      .
c      .
c  call rmeq(x,nx,i)
c  call rmeq2(y,nx,i) 
c      .
c      .
c      .
c 
c  arguments- 
c     a  - vector to remove redundant elements from.  a is assumed
c          to be in an ascending sequence.
c     na - length of a
c     ir - integer vector of length at least na to hold deletion
c          information.  points that where deleted have ir values 
c          of zero.  other points form an ascending sequence  
c          1,2,...,npoint where npoint is the number of nonredudant 
c          points.
c 
c  language - 1977 ansi standard fortran  
c  author   - gary l. pavlis
c  required software support
c     none  
c  written  - july 1981 
c-------------------------------------------------------------------- 
      integer na
      real a(na)
      integer ir(na)  
      integer i,npts  
      if(na.le.1) return
      do 100 i=1,na 
            ir(i) = 0 
  100 continue
      npts = 1
      ir(1) = 1 
      do 150 i=2,na 
            if(a(i).ne.a(i-1)) then 
                  npts = npts + 1 
                  ir(i) = npts  
            endif 
  150 continue
      entry rmeq2(a,na,ir)
      do 200 i=2,na 
            if((ir(i).gt.0).and.(ir(i).ne.i)) a(ir(i)) = a(i) 
  200 continue
      return
      end 

c $Id$ 
