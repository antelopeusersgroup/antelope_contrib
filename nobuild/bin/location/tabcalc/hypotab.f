      program hypotab
c-----------------------------------------------------------------------
c       this program takes the travel time table from the output file 
c  written by program tabcalc and replaces it by a  
c  table of strictly first arrivals.  such a table is needed for
c  hypocenter location programs.  this version writes a table in a
c  format readable by program lquake.  important global variables are-  
c 
c     ntab-number of elements of the travel time curve for this depth 
c        from input file. 
c     ptab-ray parameter(input) 
c     xtab-distance(input)
c     ttab-travel time(input) 
c     tone-first arrival travel times (output)
c     nx-number of first arrival points returned by arriv1
c     nxtab-required number of first arrival points in table. 
c 
c  files assignments- 
c     vmodel-input file containing velocity model in glp format.
c            (see comments of tabcalc for description of structure) 
c     txpxall - travel time table written by program tabcalc  
c               (see comments of tabcalc for description of structure)
c     txpxone-output file containing first arrival table. 
c 
c  language-1977 ansi standard fortran
c  written - june 1979
c  modifications- 
c     august 1980 
c     converted to run 1977 ansi standard fortran.  arriv1p was not 
c     converted but must be compiled under mnf. 
c     march 1982
c     ansi conversion completed.  while-endwhile structure of arriv1p 
c     converted to an obnoxious sequence of goto statements.
c     i/o changes made for compatibility of new version of travel 
c     time calculator that feeds this one that is now called tabcalc. 
c  February 1997
c  Added interface to genloc -- writes parameter file version of table 
c  It does this by calling a different toneout function than the 
c  old code.  The main program is unchanged.  Note that the user will
c  normally need to rename the txpxone file to something more 
c  appropriate (i.e. P.pf, S.pf, etc. )
c  June 1998
c  Removed stupid option to control print on or off.  It always
c  write stuff top output now.  Also changed dimensions to allow
c  more finely spaced tables.  Older sizes were a relic of the 
c  64 K days.
c 
c-----------------------------------------------------------------------
	include 'table.common' 
	include 'model.common' 
	include 'first.common' 
	include 'title.common' 
c--holds header from travel time table for comparison with header 
c--on velocity model  
      character*80 headtab
      parameter(maxwork=2*nxtab)
      real twork(maxwork),pwork(maxwork)
      character brwork(maxwork) 
      character refr  
c--refr flags "end refractions" that are used to fill the table if
c--necessary.  physically they are like refractions off the bottom of 
c--the velocity model.
      parameter(refr='e') 
      character*(*) modfile,intab,outtab
      parameter(modfile='vmodel',intab='txpxall',outtab='txpxone')
c--these are fortran "logical unit numbers" associated with each file 
c--relevance of these numbers is system dependent 
      parameter(modlun=11,lunin=13,lunout=2)
c--switch to control printing of table
      character list  
c--string comparison routine
      logical strcmp  
      external strcmp 
      print '(1h1)' 
      print '(50(1h+))'
      print *,'                program hypotab' 
      print *,'                first arrival table calculator'
      print '(50(1h+))'
c      read '(a)',list 
c      if(list.eq.'y') then
c            print *,'table will be printed' 
c      else  
c            print *,'table will not be printed' 
c      endif 
      list = 'y'
c--open and read input files.  first get the velocity model 
      open(modlun,file=modfile,form='formatted',status='old', 
     $       access='sequential',iostat=iochec) 
      if(iochec.ne.0) then
            print *,'//////fatal error(hypotab)   cannot open file',
     $                modfile,'/////' 
            print *,'      system returned error code iostat=',iochec 
            stop
      endif 
      call vmodin(vel,dep,nvelmax,nvel,header,modlun) 
      print *,'header read from velocity model file'
      print *,header  
      close(modlun) 
c--open file containing full travel time, ray parameter table and 
c--file that the first arrival table is to be written to. 
      open(lunin,file=intab,form='unformatted',status='old',
     $       access='sequential',iostat=iochec) 
      if(iochec.ne.0) then
            print *,'//////fatal error(hypotab)   cannot open file',
     $                intab,'/////' 
            print *,'      system returned error code iostat=',iochec 
            stop
      endif 
c
c  This file used to be unformatted 
c
      open(lunout,file=outtab,form='formatted', 
     $       access='sequential',iostat=iochec) 
      if(iochec.ne.0) then
            print *,'//////fatal error(hypotab)   cannot open file',
     $                outtab,'/////'
            print *,'      system returned error code iostat=',iochec 
            stop
      endif 
c--note velocity model header is overwritten and this one is kept.
      read(lunin) headtab 
      read(lunin) nz,dz,dx
      print *,'header read from full travel time table file'  
      print *,headtab 
      print *,'epicentral distance grid spacing = ',dx
      print *,'source depth grid spacing = ',dz 
c--check for error
      if(.not.strcmp(header,headtab)) then
           print *,'fatal error(hypotab)   inconsistent headers'
           stop 
      endif 
      if(nz.lt.nztab) then
          print 1015, nz,nztab  
 1015     format('0///fatal error, number of depth points on input ', 
     $        'file is too small///',/, 
     $  'number of point read =',i5,',',5x,i5,' points are required') 
          stop
      endif 
	print *,"Number of depth points in table = ",nz
	print *,"Enter number of distance points for first arrival table"
	print *,"Maximum number allowed = ",nxtab
	read (*,*) nxout
	if (nxout .gt. nxtab) then
		write(0,*) "Number too large, reset to ", nxtab
		nxout = nxtab
	endif
c--loop on depths 
      do 100 iz=1,nztab 
          read(lunin) ntab
          read(lunin) (ptab(i),xtab(i),ttab(i),branch(i),i=1,ntab)
          call arriv1p(twork,pwork,brwork,maxwork,dx,nx)
          istop=min0(nxout,nx)
          do 50 i=1,istop 
               tone(iz,i)=twork(i)
               pone(iz,i)=pwork(i)
               brone(iz,i)=brwork(i)
   50     continue
c--if nx is not large enough fill the table with refractions
c--with slowness of the last point in the known table.
c--give these points a special name.
          if(nx.lt.nxout) then  
               pref = pone(iz,nx) 
               do 75 i=nx+1,nxout 
                    tone(iz,i) = tone(iz,i-1) + dx*pref 
                    pone(iz,i)=pref 
                    brone(iz,i)=refr
   75          continue 
          endif 
  100 continue
      call toneout(dx,dz,nxout,lunout)
      if(list.ne.'y') stop
      print 1050
 1050 format('0reduced travel time table')
      print *,'reducing velocity = ',vel(nvel)
      print 1060
 1060 format('0output gives ordered triples=(reduced travel time,', 
     $       ' ray parameter, branch name)')
      print *,'branch name code'
      print *,'  d = refracted arrival (takeoff angle downward)'
      print *,'  u = direct arrival (takeoff angle upward)' 
      print *,'  c = crossover point (a crossover occurs between ', 
     $                ' here and the next grid point)'
      print *,'  l = arrival is a refraction off the lid of a lvz'
      print *,'  e = end refraction.  pseudo arrival used to fill', 
     $               ' the table when necessary'
      do 200 i=1,nztab,4
           zone=dz*float(i-1) 
           ztwo = dz*float(i) 
           zthree = dz*float(i+1) 
           zfour  = dz*float(i+2) 
           nspace = nztab - i + 1 
           print 1070, zone,ztwo,zthree,zfour 
           do 150 j=1,nxout
                 ncolum = min(4,nspace) 
                 print 1080, (tone(ii,j),pone(ii,j),brone(ii,j),
     $                         ii=i,i+ncolum-1) 
  150      continue 
 1070 format('0',4('source depth = ',f10.5,5x)) 
 1080 format(1x,4(f10.5,f10.7,2x,a6,2x))
  200 continue
      stop  
      end 

c $Id$ 
