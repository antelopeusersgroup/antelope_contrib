      subroutine split(vel,dep,vs,ds,vf,df,npts)
c---------------------------------------------------------------------- 
c       splits the line between (vs,ds) and (vf,df) in space (x,y)
c  into npts equal intervals.  the routine fills the vel and dep arrays 
c  with the endpoints of the intervals in a sequence going from (vs,ds) 
c  to (vf,df).  the routine always sets (vel(1),dep(1)) = (vs,ds) and 
c  (vel(npts),dep(npts))=vf,df) unless npts.le.1 in which case only 
c  vel(1) and dep(1) are set. 
c-----------------------------------------------------------------------
      dimension vel(npts),dep(npts) 
      vel(1)=vs 
      dep(1)=ds 
      if(npts.le.1) return
      grad=(vf-vs)/(df-ds)
      ddep=(df-ds)/float(npts-1)
      do 100 i=2,npts 
           dep(i)=ds+ddep*float(i-1)
  100      vel(i)=vs+(dep(i)-ds)*grad 
      return
      end 

c $Id$ 
