

c  @(#)dbresp1f.f	1.1 7/1/99 
c
c    Program to test FORTRAN interface
c
      parameter (NPMAX  = 100)
      parameter (NZMAX  = 100)
      parameter (NNMAX  = 500)
      parameter (NDMAX  = 500)
      parameter (MXFREQ = 1000)
      complex pole(NPMAX)
      complex polerr(NPMAX)
      complex zero(NPMAX)
      complex zererr(NPMAX)
      real*4 coefn(NNMAX), coeferrn(NNMAX)
      real*4 coefd(NDMAX), coeferrd(NDMAX)
c      real*4 freq(MXFREQ), amp(MXFREQ), phase(MXFREQ)
      character*1 chr
      character*3 cps
      character*80 str
      character*60 database, fname, title,psfile
      character*10 sta, chan, yearstr, daystr
      character*10 stas, chans
      character*20 sratestr, freqstr, mnfreqstr, mxfreqstr
      character*4 type
      logical postscript
      real*8 time, times, endtime, h2e
      integer idb, db(4), dummy
      external h2e
#include "dbl2.i"
c
c    Process command line
c
      narg = iargc()
c
c  do we want a postscript output
c
      call getarg(narg-1,cps)
      if (cps .eq.'-ps') then
        postscript = .TRUE.
        call getarg(narg,psfile)
        narg = narg - 2
      end if 
      call getarg(narg,cps)
      if (cps .eq.'-ps') then
        postscript = .TRUE.
        psfile = 'dbresp.ps'
        narg = narg - 1
      end if
      if (.NOT. postscript) psfile = 'none' 
c   
c  process rest of command line
c     
      if ((narg .ne. 6) .and. (narg .ne. 2)) then
	write (6, '(a)') 
     +  'usage: dbresp database sta chan year day freq'
	write (6, '(a)') 
     +  '   or: dbresp response_file_name freq'
	stop
      end if
      if (narg .eq. 5) then
        call getarg (1, database)
        call getarg (2, sta)
        call getarg (3, chan)
        call getarg (4, yearstr)
        read (yearstr, *) iyear
        call getarg (5, daystr)
        read (daystr, *) iday
        call getarg (5, freqstr)
        read (daystr, *) freq1

c
c  make title
c
        islash = 0
        do 9 i=1,len(database)
          if (database(i:i) .eq.'/') islash = i
    9   continue
        title = database(islash+1:lenb(database))
        title(lenb(title)+2:len(title)) = sta
        title(lenb(title)+2:len(title)) = chan
        title(lenb(title)+2:len(title)) = yearstr
        title(lenb(title)+2:len(title)) = daystr
c
c    Open database
c
        call db30create (database, idb)
        time = h2e (iyear, iday, 0, 0, 0.0d0)
        jdate = iyear*1000 + iday
        write (6, '(a,a,i5,i4)') sta, chan, iyear, iday
c
c    Look through sensor table
c
        call dbgetntuples (idb, RELID_SENSOR, ntuples)
        if (ntuples .lt. 1) then
	  write (6, '(a)') 'dbresp: No sensor rows'
	  stop
        end if
        do 10  i = 1, ntuples
	  call dbgetattrs (idb, RELID_SENSOR, i,
     +                     ATTRID_STA, stas,
     +                     ATTRID_CHAN, chans,
     +                     ATTRID_JDATE, jdates,
     +                     ATTRID_TIME, times,
     +                     ATTRID_ENDTIME, endtime,
     +                     ATTRID_INID, inid,
     +                     0)
	  if (jdate .ne. jdates .and. 
     +       (time .lt. times .or. time .gt. endtime)) go to 10
	  if (sta .ne. stas) go to 10
	  if (chan .ne. chans) go to 10
	  go to 20
   10   continue
        write (6, '(a,a,a,a,a)') sta,chan,yearstr,daystr,' not found'
        stop
   20   continue
c
c    Look through instrument table
c
        call dbgetntuples (idb, RELID_INSTRUMENT, ntuples)
        if (ntuples .lt. 1) then
	  write (6, '(a)') 'dbresp: No instrument rows'
	  stop
        end if
        do 30  i = 1, ntuples
	  call dbgetattrs (idb, RELID_INSTRUMENT, i,
     +                     ATTRID_INID, inidi,
     +                     ATTRID_NCALIB, calibn,
     +                     ATTRID_NCALPER, calpern,
     +                     ATTRID_SAMPRATE, samprate,
     +                     0)
	  if (inidi .ne. inid) go to 30
	  go to 40
   30   continue
        write (6, '(a,a,a,a,a)') sta,chan,yearstr,daystr,
     +                                 ' not found (ins)'
        stop
   40   continue
	call DBLink2Dbptr ( idb, db ) 
	dummy = dblookup ( db, '', 'instrument', '', '' ) 
	db(4) = i 
        dummy = dbfilename (db, fname)
      else if (narg .eq. 2) then
        call getarg (1, fname)
        call getarg (2, freqstr)
        read (freqstr,*) freq1
      end if
c
c      write (6, '(a,a)') 'Reading file ',fname
c
c    Read filter file
c
      call readresp (fname, iresp)
      if (iresp .eq. 0) then
	write (6, '(a,a)') 'dbresp: readresp() error on ',fname
        stop
      end if
c
c    Get the number of stages
c
      call getrespnstages (iresp, nstages)
c      write (6, '(a,i2)') '# stages = ',nstages
c
c    Loop through stages
c
      do 100  i = 1, nstages
c
c         Get stage type
c
	call getresptype (iresp, i, type)
c	write (6, '(a,i2,a,a)') 'Stage # ',i,', type = ',type
c
c         PAZ stage
c
	if (type .eq. 'paz') then
	  npoles = NPMAX
	  nzeros = NZMAX
	  call getresppaz (iresp, i, xnorm,
     +                  npoles, pole, polerr,
     +                  nzeros, zero, zererr)
c	  write (6, '(a,e15.7)') 'Normalization = ',xnorm
c	  write (6, '(a,i3)') '# Poles = ',npoles
c	  write (6, '(a,i3)') '# Zeros = ',nzeros
c
c         FIR stage
c
	else if (type .eq. 'fir') then
	  nden = NDMAX
	  nnum = NNMAX
	  call getrespfir (iresp, i, srate,
     +                  nnum, coefn, coeferrn,
     +                  nden, coefd, coeferrd)
c	  write (6, '(a,f15.5)') 'Input sample rate = ',srate
c	  write (6, '(a,i3)') '# Numerator coefficients = ',nnum
c	  write (6, '(a,i3)') '# Denominator coefficients = ',nden
	else if (type .eq. 'fap') then
	  write (6, '(a)') 'dbresp: No fortran i/f for fap type.'
	else
	  write (6, '(a,a)') 'dbresp: Unknown type ',type
	end if
  100 continue
c
c    Evaluate response
c
c      write (6, '(a)') 'Evaluating response:'
      omega = freq1*2*3.141592654
      call evlresp (omega, iresp, xreal, ximag)
      amp = sqrt(xreal*xreal+ximag*ximag)
      if (amp .gt. 0.0) then
         phase = atan2 (ximag, xreal) *180.0/3.141592654
      else
         phase = 0.0
      end if

      write (6, '(a)') '  frequency        amplitude        phase'
      write (6, '(3g15.5)') freq1,amp,phase
      end

c****************************************************************
      integer function lenb (string)

c finds index of last nonblank character in string

      character*(*) string
      n=len(string)
      do 10 i=n,1,-1
         if (string(i:i).ne.' ') then
            lenb=i
            return
         end if
10    continue
      lenb=0
      return
      end

c $Id$ 
