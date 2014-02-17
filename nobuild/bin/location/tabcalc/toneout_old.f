*deck toneout 
      subroutine toneout(dx,dz,lun) 
c-------------------------------------------------------------------- 
c  writes first arrival table to lun in prejudiced format.
c  travel times written out are reduced by total distance (not just 
c  epicentral distance) from source to receiver.
c 
c  arguments- 
c   dx - distance increment of grid 
c   dz - depth increment of grid
c   lun- fortran "logical unit number" of file to which table is to be  
c        written. 
c 
c  modifications - july 1979
c    the ptab and branch name tables, pone and brone respectively 
c    are also now appended. 
c 
c    dec. 1982
c    file structure of table changed.  added header 
c    variable size velocity model.
c---------------------------------------------------------------------
	include 'model.common' 
	include 'first.common' 
	include 'title.common' 
c--convert to reduced traveltime
c--use last velocity grid point for reducing velocity 
      vreduce = vel(nvel) 
      do 100 j=1,50 
          r=dx*float(j-1) 
          do 100 i=1,30 
               z=dz*float(i-1)
               t1=sqrt(r*r+z*z)/vreduce 
  100 tone(i,j)=tone(i,j)-t1
      write(lun) header 
      write(lun) nvel,nztab,nxtab 
      write(lun) dx,dz,vreduce
      write(lun) (dep(i),vel(i),i=1,nvel) 
      write(lun) ((tone(i,j),i=1,nztab),j=1,nxtab)
      write(lun) ((pone(i,j),i=1,nztab),j=1,nxtab)
      write(lun) ((brone(i,j),i=1,nztab),j=1,nxtab) 
      return
      end 

c $Id$ 
