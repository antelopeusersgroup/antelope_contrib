      subroutine vmodin(v,d,n,nin,header,lun) 
c---------------------------------------------------------------------- 
c    ansi standard version replacing three earlier versions of the same 
c  basic routine.  cautiously reads velocity model in prejudiced format.
c 
c  arguements-
c 
c     v-array of length n that on output contains velocity values read  
c       in. 
c     d-array of length n of corresponding depths.  
c     n-length of v and d arrays in calling program.  if length of model
c       to be read in is larger than n it will be truncated to n points.
c     nin-actual size of model read in.  never larger than n. 
c     header-character string variable that must be specified of length 
c            at least 80 characters in calling program. 
c            on output header is the descriptive character string used  
c            on this velocity model.
c     lun-logical unit number associated with velocity model file when  
c         file was opened.
c---------------------------------------------------------------------- 
      dimension v(n),d(n) 
      character*80 header 
      read(lun,'(a)') header
      read(lun,1100) nin
 1100 format(i5)
      if(nin.gt.n) nin=n
      read(lun,1200) (v(i),d(i),i=1,nin)
 1200 format(2f10.5)
      return
      end 

c $Id$ 
