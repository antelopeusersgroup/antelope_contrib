      subroutine ttout(n,z,p,t,x,branch,jprint) 
c-------------------------------------------------------------------- 
c        ttout is an output routine.  printed output is optional. 
c  table is always written to unit lun (passed through common)
c  by an unformatted write. 
c 
c  arguments- 
c     n-number of points
c     z-depth 
c     p-ray parameter 
c     t-travel time 
c     x-distance
c     branch-name of the branch 
c     jprint.ne.0  printed output 
c           .eq.0  no printed output
c 
c-------------------------------------------------------------------- 
	include 'lun.common' 
      real p(n),t(n),x(n) 
      character branch(n) 
      parameter(flaglvz=1.0e38) 
      if(jprint.ne.0) then
          print 1000, z 
 1000     format('0travel times for source at depth=',f10.5)
          print 1010  
 1010     format(1h0,4x,'p',10x,'distance',6x,'travel time',4x, 
     $           'branch type') 
               do 100 i=1,n 
               if(t(i).lt.flaglvz) then 
                    print 1020, p(i),x(i),t(i),branch(i)
 1020               format(3(f10.5,5x),a) 
               else 
                    print 1030  
 1030               format(20x,'/////shadow zone/////') 
               endif  
  100          continue 
      endif 
      write(lun) n,z  
      write(lun) (p(i),x(i),t(i),branch(i),i=1,n) 
      return
      end 

c $Id$ 
