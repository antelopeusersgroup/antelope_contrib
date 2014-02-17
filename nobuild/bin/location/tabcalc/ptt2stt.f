      program ptt2stt
c----------------------------------------------------------------------
c  simple program to take a p travel time table written by ttcalc
c  and make a p and s table based on a constant vp/vs ratio.  this
c  program does little more than read the input table, calculate s
c  times, and write the new table.
c
c  author:  gary l. pavlis
c  written:  june 1988
c--------------------------------------------------------------------
      character*80 header 
      integer maxztab,maxxtab,maxvel
      parameter (maxztab=30,maxxtab=50,maxvel=200)
      real dx,dz,vreduce
      integer nxtab,nztab 
      real tp(maxztab,maxxtab),pp(maxztab,maxxtab)
      character pbran(maxztab,maxxtab)
      real ts(maxztab,maxxtab),ps(maxztab,maxxtab)
c     character sbran(maxztab,maxxtab)
      real dep(maxvel),velp(maxvel),vels(maxvel)
      character*70 infile,outfile
      real vpvs
      integer lun,lunout
      parameter(lun=7,lunout=8)
      write(*,*) 'enter p travel time table file'
      read(*,1000) infile
 1000 format(a)
      open(lun,file=infile,form='unformatted',access='sequential',
     $       status ='old')
      write(*,*) 'enter output file name to hold p and s table'
      read(*,1000) outfile
      open(lunout,file=outfile,form='unformatted',access='sequential',
     $         status='new')
      write(*,*) 'enter vp/vs ratio to convert p times to s times'
      read(*,*) vpvs  
      read(lun) header
      read(lun) nvel,nztab,nxtab
      if((nztab.gt.maxztab).or.(nxtab.gt.maxxtab)) then
            print *,'fatal error(ttabin))   not enough space for ',
     $                  'travel time tables.'
            print *,'table is ',nztab,' by ',nxtab  
            print *,'space available is ',maxztab,' by ',maxxtab
      elseif(nvel.gt.maxvel) then
            print *,'fatal error(ttabin))   not enough space for ',
     $                  'velocity model.'
            print *,'velocity model has ',nvel,' points.'
            print *,'space available is ',maxvel,' points.'
      endif
      read(lun) dx,dz,vreduce
      read(lun) (dep(i),velp(i),i=1,nvel)
      read(lun) ((tp(i,j),i=1,nztab),j=1,nxtab)
      read(lun) ((pp(i,j),i=1,nztab),j=1,nxtab)
      read(lun) ((pbran(i,j),i=1,nztab),j=1,nxtab)
      do 100 i=1,nvel
            vels(i) = velp(i)/vpvs
  100 continue
      do 120 j=1,nxtab
            do 110 i=1,nztab
                  r = sqrt((float(i-1)*dz)**2+(float(j-1)*dx)**2)
                  ptime = tp(i,j) + r/vreduce
                  ts(i,j) = ptime*vpvs - r/vreduce  
                  ps(i,j) = pp(i,j)*vpvs
  110       continue  
  120 continue
      write(lunout) header
      write(lunout) nvel,nztab,nxtab
      write(lunout) dx,dz,vreduce
      write(lunout) (dep(i),velp(i),vels(i),i=1,nvel)
      write(lunout) ((tp(i,j),i=1,nztab),j=1,nxtab)
      write(lunout) ((pp(i,j),i=1,nztab),j=1,nxtab)
      write(lunout) ((pbran(i,j),i=1,nztab),j=1,nxtab)
      write(lunout) ((ts(i,j),i=1,nztab),j=1,nxtab)
      write(lunout) ((ps(i,j),i=1,nztab),j=1,nxtab)
      write(lunout) ((pbran(i,j),i=1,nztab),j=1,nxtab)
      end 

c $Id$ 
