      program tabcalc
c---------------------------------------------------------------------- 
c 
c                      released version 1.0 
c                      author   gary l. pavlis
c                               geophysics program ak-50
c                               university of washington
c                               seattle, wa  98195  
c 
c 
c        tabcalc calculates travel time tables for arbitrary  
c  one-dimensional velocity models specified on an irregular grid 
c  of depth points.  the table that is produced by this program 
c  includes forward and reverse branches.  in addition, locations of
c  discontinuities from low velocity zones are noted.  different
c  branches are denoted by a single character code that lives in an 
c  array named "branch".  the codes used are
c    u - upward or direct ray paths from a buried source
c    d - downward point ray path.  that is, a refracted arrival on
c        a forward branch.
c    r - reversed branch
c    s - shadow zone discontinuity flag 
c  travel times, distances, and ray parameters are calculated using 
c  a linear interpolation between velocity grid points.  times and
c  distances are calculated based on the analytic forms for linear
c  velocity gradients.  surface focus travel times, distances, and
c  ray parameters are first calculated for rays that bottom relatively
c  uniformly in depth.  (this information is stored in the
c  common block named /surface/.)  buried source travel times and 
c  distances are then calculated from these rays supplemented with
c  rays having small ray parameters in a downward accumulation mode.
c  that is, times for refracted (downward starting rays) are  
c  calculated as tsurface - t(z=0 -> zsource) and direct arrival
c  times are calculated as t(z=0 -> zsource).  (similarly for distance) 
c  these randomly spaced rays are then interpolated onto a regular
c  grid with large gaps filled with extra rays shot to fill them. 
c  (note   layered models are accepted by this routine (specified as
c  two velocities at the same depth.), but they are not recommended 
c  since they are always distorted somewhat internally.)
c 
c  files- 
c 
c   input - standard input file.  all input is free form.  the
c           following sequence is expected. 
c      line 1 - step  (floating point)
c               specifies depth interval that rays are to bottom in.
c               warning   exection time depends heavily on the size 
c               this parameter is set to.  experience indicates speed 
c               is optimized if step is chosen as the maximum depth 
c               in the velocity model divided by about 200 to 300.
c      line 2 - xmin  (floating point)
c               determines approximate spacing of direct rays shot
c               to supplement surface focus ray parameters determined 
c               by step.  a good choice is to set xmin=dx 
c               (dx is defined below).
c      line 3 - dx,dz  (floating point) 
c               desired spacing of table grid in distance (dx) and
c               depth (dz). 
c      line 4 - nz (integer)
c               number of depths at which travel time tables are
c               to be calculated.  (note   nz*dz can be greater than
c               the deepest velocity point specified but this is not
c               highly recommended and will produce a diagnostic.)
c      line 5 - jprt.eq.0 table not written to output file. 
c                      .ne.0 table will be printed on output file.
c               (warning   printing the full table is not normally
c                recommended unless a problem is suspected as the 
c                output produced is usually verbose.) 
c   vmodel - formatted file containing discretized velocity model.
c            the following structure is expected. 
c      line 1 - 80 character header label 
c      line 2 - n (i5)  number of points  
c      line 3 to n+2 - velocity, depth  (2f10.5) ordered pairs
c           (warning   they must be arranged in a nondeceasing
c                      sequence in depth or chaos may result.)
c 
c   output - standard output file.  normal listing is the irregular 
c            time,distance, ray parameter table calculated for surface  
c            focus sources.  full listing is available by throwing
c            the switch jprt
c 
c   txpxall - unformatted file to which final table is written. 
c             the structure of this file is as follows
c      rec 1 - 80 character header label  
c      rec 2 - nz, dz, dx (see above) 
c      these are followed by nz blocks of the form  
c              rec 1   write () n,z 
c              rec 2   write () (p(i),x(i),t(i),branch(i),i=1,n)
c 
c 
c  written-april 1979 
c 
c  modifications- 
c  june 1979 -  minor changes in i/o
c 
c  august 1980-two major changes
c     (1) printed and permanent file output of the verbose table
c         were made separate options to reduce i/o overhead.  
c     (2) gridtp was modified slightly to work with layer models. 
c 
c  july,1981 - major modification 
c         interpolation changed drastically to fix earlier versions 
c         constant problems.  procedure now takes raw randomly spaced 
c         x,t,p data and converts it to pseudo regularly spaced 
c         table in x before interpolating onto a regular grid in gridtp.
c         this is accomplished by deleting some rays and adding some
c         where needed.  this may slow execution somewhat but should
c         be compensated for by requiring fewer rays initially to 
c         define the curve adequately.  hence a couple of hundred rays  
c         should normally now be adequate.
c 
c  march 1982 - major changes for release version 
c         (1) this code is an assimilation of what used to be two 
c             programs called "flatray" and "deeptt".  flatray is 
c             now a subroutine called ttsurf.  the name of this 
c             main program has been changed accordingly.
c         (2) subroutine gridtp's while-endwhile structure bastardized
c             into an awful sequence of goto statements.
c         (3) many i/o options in earlier version were abandoned. 
c         (4) "branch" array labels converted to single characters. 
c         (5) table output file changed to unformatted to speed i/o.
c      most subroutines except gridtp where changed only slighly.  the  
c      driver is virtually all new, however.
c 
c  language-1977 ansi fortran 
c---------------------------------------------------------------------
	include 'switch.common'
	include 'control.common' 
	include 'title.common' 
	include 'model.common' 
	include 'surface.common' 
	include 'window.common'
	include 'table.common' 
	include 'upward.common'
      character*(*) modfile, tabfile
      parameter(modfile='vmodel',tabfile='txpxall') 
	include 'lun.common' 
c--initialize 
      lun = 11
      do 100 i=1,npmax
      tup(i)=0. 
  100 xup(i)=0. 
      print '(120(1h+))'
      print *,'          program tabcalc (version 1.0)' 
      print '(120(1h+))'
c--control variables are read from input stream 
      read *,step 
      read *,xmin 
      read *,dx,dz
      read *,nz 
      read *,jprt 
      print *,'control parameters read from input'  
      print '(1h )' 
      print *,'depth increment of ray bottoms=',step
      print *,'upward branch distance cutoff=',xmin 
      print *,'table horizontal distance increment=',dx 
      print *,'table depth increment=',dz 
      print *,'number of depth increments=',nz
      if(jprt.eq.0) print *,'full table printing suppressed'
      if(jprt.ne.0) print *,'full table will be printed'
c--open and read file containing discretized velocity model 
c--vwork and dwork arrays are used here to hold this discretized model. 
c--they are reused for a different purpose below
      open(lun,file=modfile,form='formatted',status='old',iostat=iochec)
      if(iochec.ne.0) then
            print *,'fatal error(tabcalc)   cannot open file->',modfile 
            print *,'system returned iostat error code=',iochec 
            stop
      endif 
      call vmodin(vel,dep,nvelmax,nvel,header,lun)
      close(lun)
c--open file table is to be written to
      open(lun,file=tabfile,form='unformatted',status='unknown',
     $          iostat=iochec)
      if(iochec.ne.0) then
            print *,'fatal error(tabcalc)   cannot open file->',tabfile 
            print *,'system returned iostat error code=',iochec 
            stop
      endif 
c--now calculate travel times, distances, and ray parameters for
c--surface focus sources
      call surftt(vel,dep,nvel,step)
c--write headers on output files
      write(lun) header 
      write(lun) nz,dz,dx 
c--calculate small ray parameter needed for upward rays 
      call shootup
c--produce the travel time table for buried sources 
      call ttable 
      stop  
      end 

c $Id$ 
