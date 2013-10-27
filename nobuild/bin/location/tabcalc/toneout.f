      subroutine toneout(dx,dz,nxone,lun) 
c-------------------------------------------------------------------- 
c  writes first arrival table to parameter file used by genloc
c  uniform travel time calculator function.  This is basically an
c  interface from this old set of programs to genloc that allows
c  one to use complex 1D models for locations. 
c 
c  arguments- 
c   dx - distance increment of grid 
c   dz - depth increment of grid
c   lun- fortran "logical unit number" of file to which table is to be  
c        written. 
c
c  Main table are passed through the fortran abomination of common
c  blocks.  
c 
c  Written:  February 1997 as replacement for old function that
c  	wrote a binary file used by selm, pmel, etc.  
c---------------------------------------------------------------------
	include 'model.common' 
	include 'first.common' 
	include 'title.common' 
	real KM2DEG
	parameter(KM2DEG=111.19)
	character branch
	real z
	real dvdz
c--write out the grid description parameters 
	write(lun,"('nx ',i10)") nxone
	write(lun,"('nz ',i10)") nztab
	write(lun,"('z0 0.0')")
	write(lun,"('x0 0.0')")
	write(lun,"('dx ',g15.5)") dx/KM2DEG
	write(lun,"('dz ',g15.5)") dz

c--Now write the table.  We are not going to try to calculate
c--the dp/dr constants, but simpley set them all to zero here.
c--This could be done by finite differences, but it will not
c--matter a great deal except for very close to an array.
c--The parameter file structure scanes depth by depth, nxone 
c--values per scan
	write(lun,*)"uniform_grid_time_slowness_table &Tbl{"

	do 150 i=1,nztab
		write(lun,"('# Depth = ',f15.5)") dz*(i-1)
		do 100 j=1,nxone
c--genloc uses a different set of branch names than those
c--define here.  We simply translate them one for one on
c--the fly
			if(brone(i,j).eq.'d') then
				branch = 't'
			else if(brone(i,j).eq.'c') then
				branch = 'c'
			else if(brone(i,j).eq.'u') then
				branch = 'u'
			else if(brone(i,j).eq.'l') then
				branch = 't'
			else if(brone(i,j).eq.'e') then
				branch = 't'
			else
				write(0,*)"Fatal error: toneout"
				write(0,*)"Unknown branch code ",brone(i,j)
				stop
			endif
			write(lun,"(2G20.13,' 0.0 ',a)")tone(i,j),pone(i,j),branch
  100		continue
  150	continue
	write(lun,*)"}"
c
c  We need to write the velocities at the table depth points
c  I always search for bracketing points from the top down. 
c  slower, but simpler and less error prone 
c
	write(lun,*)"velocities &Tbl{"
	write(lun,*)vel(1)
	do 300 i=2,nztab
		z = (i-1)*dz
		do 200 j=2,nvel
			if( (z.ge.dep(j-1)).and.(z.lt.dep(j)).or.(j.eq.nvel)) then
				dvdz=(vel(j)-vel(j-1))/(dep(j)-dep(j-1))
				write(lun,*) vel(j-1)+(z-dep(j-1))*dvdz
				goto 250
			endif
  200		continue
  250		continue
  300	continue
	write(lun,*)"}"
	return
	end	

c $Id$ 
