      program dblocgrad
c  
c  program to locate local earthquakes using  a 1-d linear gradient
c  model with station corrections. 
c  includes error analysis using jackknife and bootstrap (3/26/90)
c  primary code written by Jennifer Scott for her Ph.D. thesis 1992
c  incorporated into datascope by Frank Vernon July, 1996
c
c---------------------------------------------------------------------
c  Things to do
c  
c  clean up code.
c
# include "db.i"

      implicit real*8 (a-h,o-z)

c     mdim must be 10+maximum # of stations

      parameter   (mnod=25,mdim=200)
      parameter   (herra=1.0,herrb=5.0)
      parameter   (ndim=4,ldim=1,ma=2*mdim,nreal=40,nelp=51)

      common /mod/g(mnod),b(mnod),z(mnod),u(mnod),nodes

      common /psmod/psg(mnod,2),psb(mnod,2),psz(mnod,2),psu(mnod,2),nops       

      common /travtable/ptab(200),xtab(200),ttab(200),maxpindex

      dimension    stnloc(4,mdim),  levs(mdim)
      dimension    psig(mdim),      ssig(mdim)
      dimension    psc(mdim),       ssc(mdim)
      dimension    ev(6),           rwt(3,2)
      dimension    part(mdim),      sart(mdim)
      dimension    pwt(mdim),       swt(mdim)
      dimension    delta(mdim),     seaz(mdim),     esaz(mdim) 
      dimension    tres(ma)
      dimension    psave(5,mdim),   ssave(5,mdim),  qsvd(ndim)
      dimension    a(ma,ndim),      usvd(ma,ndim),  vsvd(ndim,ndim)


      character*6  stacode(mdim),   dummy

      integer      dbin(4),	 dbout(4)
      integer      parid(mdim),  sarid(mdim)
      integer      orid

      character*40 dbinput,	dboutput
      character*20 vmodel,	pfname
      character*15 author,	algorithm

      character *55 errmsg(6)
      data errmsg / "ierr=1 event depth perturbed above ground",
     *	      "ierr=2 model not deep enough",
     *	      "ierr=3 location didn't converge in maxiter iterations",
     *	      "ierr=4 moho pick in this event",
     *	      "ierr=5 less than 4 arrivals",
     *	      "ierr=6 less than 4 arrivals above model limit"/

      author(1:15)    = '  '
      algorithm(1:15) = 'dblocgrad'
      pfname(1:20)    = 'dblocgrad'
      vmodel(1:20)    = '  '
      dbinput(1:40)   = '  '
      dboutput(1:40)  = '  '

      if ((iargc().lt.1).or.(iargc().gt.2)) then
         write(*,*) 'usage: dblocgrad dbin [dbout]'
         call exit(1)
      endif

      call getarg(1,dbinput)
      if (dbopen(dbinput,"r+",dbin) .lt. 0) 
     *    call die (0,"Can't open input database")

      if (iargc().eq.2) then
         call getarg(2,dboutput)
         if (dbopen(dboutput,"r+",dbout) .lt. 0) 
     *       call die (0,"Can't open output database")
      endif

   10 continue
c      write(*,*) "Enter: "
      read(*,'(a)',end=20) dummy

      call set_def(mdim,    ev,    rwt,    psig, ssig,  
     +             psc,     ssc,   stnloc, part, sart,   
     +             pwt,     swt,   delta,  seaz, esaz,   
     +             parid,   sarid, stacode)


      call readpf1(pfname,author,vmodel)
      
      call readmod(dbin,vmodel)

      call readstn(dbin, mdim, vmodel, nsta, stnloc, levs,
     +             psig, ssig, psc, ssc, reflat, reflon, stacode)

      call readpf2(pfname, reflat, reflon, mdim, stacode, ev, rwt,
     +             parid, sarid, part, sart, pwt, swt, nparr, nsarr)

c      call check_common      

c      do 1000 ista=1,nsta
c         write(*,999) stacode(ista),(stnloc(j,ista),j=1,4),levs(ista),
c     +                psig(ista),ssig(ista),psc(ista),ssc(ista),ista
c  999    format(a6,4(f8.3),i4,2(f4.1),2(f9.3),i4)
c 1000    continue

c      do 2000 ista = 1,mdim
c         if ((part(ista).ne.0.0).or.(sart(ista).ne.0.0)) then
c            write(*,2001) stacode(ista),part(ista),sart(ista),
c     +                    pwt(ista),swt(ista),parid(ista),
c     +                    sarid(ista),ista
c         endif
c 2000 continue
c 2001 format(a,2x,f17.5,2x,f17.5,2x,f5.3,2x,f5.3,3i6)

c      write(*,'(a,a)') 'vmodel ',vmodel
c      write(*,'(a,a)') 'author ',author
c      write(*,'(5f10.5,f17.5)') (ev(j),j=1,6)
c
c      write(*,'(a)') 'residual cutoffs '
c      write(*,'(2f10.5)') (rwt(1,j),j=1,2)
c      write(*,'(2f10.5)') (rwt(2,j),j=1,2)
c      write(*,'(2f10.5)') (rwt(3,j),j=1,2)

c
c  locate event
c
      if(nparr+nsarr.gt.3) then

        call leqs(ev, part, sart, stnloc, levs, psc, ssc, psig, ssig,
     +            pwt, swt, rwt, mdim, psave, ssave, a, usvd, vsvd,
     +            qsvd, tres, ma, chip, chis, nparr, nsarr, ierr)

c       returns location in ev, residuals azimuth epdis takeoff
c       angle and backazimuth in psave and ssave
c       usvd and vsvd are used to calculate errors

        narr   = nparr + nsarr
        chi    = chip  + chis
        chitot = chitot + chi
        if ((nparr + nsarr) .lt. 4) then
           ierr = 5
        endif
      else
         ierr = 5
      endif

      if ( ierr .ne. 0 ) then
	write (6, *) "dblocsat2: no solution : ", errmsg(ierr)

      else

	  call add_origin  ( dbout,  narr,   narr, ev, reflat, reflon,
     +                   algorithm, author, orid)
	  call add_origerr ( dbout, orid)
	  call add_assocs  ( dbout, orid, mdim, stacode, parid, sarid,  
     +                   pwt, swt, delta, seaz, esaz, vmodel )
	  write (6,*) "dblocsat2: new origin ", orid

      endif

      call flush(6)
      go to 10
   20 continue
      stop
      end

c****************************************************************************

      subroutine readpf1(pfname,author,vmodel)
c
c  subroutine to extract author and velocity model names from 
c       file "pfname"
c
c  inputs:
c	pfname	parameter file name
c  returns:
c	author  user who located event
c	vmodel	velocity model name
c
      implicit real*8 (a-h,o-z)
      character*(*)  vmodel,	pfname,		author
      integer        pf,           retcode

      call pfread ( pfname, pf, retcode)
      
      if (retcode .ne. 0) 
     +    call die (0,"No parameter file dblocgrad.pf")

      call pfget_string ( pf, 'travel_time_model', vmodel )
 
      call pfget_string ( pf, 'author',            author )

c      write(*,'(a,a,i5)') 'vmodel ',vmodel, len(vmodel)
c      write(*,'(a,a,i5)') 'author ',author, len(vmodel)

      return
      end

c********1*********1*********1*********1*********1*********1*********1**

      subroutine readpf2(pfname, reflat, reflon, mdim, sta, ev, rwt,
     +                   parid, sarid, part, sart, pwt, swt, 
     +                   nparr, nsarr)
c
c  subroutine to extract initial origin and arrival parameters from 
c       file "pfname"
c
c  inputs:
c	pfname	parameter file name
c	reflon	reference longitude
c       reflat  reference latitude
c	mdim	maximum number of stations
c	sta	array of station codes
c  returns:
c	ev	initial event coordinates
c	rwt	residual standard deviation cutoffs
c	parid	p arrival id
c	sarid	s arrival id
c	part	p arrival times
c	sart	s arrival times
c	pwt	p pick limits
c	swt	s pick limits
c	narr	number of arrivals
c
      implicit real*8 (a-h,o-z)

      dimension      ev(6),          rwt(3,2)
      dimension      part(mdim),     sart(mdim)
      dimension      pwt(mdim),      swt(mdim)

      integer        parid(mdim),    sarid(mdim)
      integer        pf,             retcode
      integer        maxtbl

      character*6    sta(mdim)
      character*(*)  pfname
      character*80   line
      character*6    arid,stacode,phase,wgt,fm
      character*10   rwt1,rwt2
      character*18   time

      ptime = 1.d15
      nparr = 0
      nsarr = 0

      call pfread ( pfname, pf, retcode)

      call pfget_double ( pf, 'initial_latitude',   olat   )
      call pfget_double ( pf, 'initial_longitude',  olon   )
      call pfget_double ( pf, 'initial_depth',      depth  )
c      write(*,*) olat,olon,depth

      call pfget_tbl ( pf, 'arrival_table', itbl)

c      write(*,*) 'maxtbl',itbl
      narr = maxtbl(itbl)
c      write(*,*) 'narrivals', narr

      do 100 irow = 1, narr

         call strgettbl(itbl,irow-1,line)

         call gettoken ( line, 1, arid    )
         call gettoken ( line, 2, stacode )
         call gettoken ( line, 3, phase   )
         call gettoken ( line, 4, time    )
         call gettoken ( line, 5, wgt     )
         call gettoken ( line, 6, fm      )

         do 90 ista = 1,mdim

            if ( sta(ista) .eq. stacode ) then

               if ( phase(1:1) .eq. 'P' ) then
                  read( arid, '(i8)' )    parid(ista)
                  read( time, '(f17.5)')  part(ista)
                  read( wgt,  '(f5.3)')   pwt(ista)
                  ptime = min( ptime, part(ista) )
                  nparr = nparr + 1
                  go to 100
               endif

               if ( phase(1:1) .eq. 'S' ) then
                  read( arid, '(i8)' )    sarid(ista)
                  read( time, '(f17.5)')  sart(ista)
                  read( wgt,  '(f5.3)')   swt(ista)
                  nsarr = nsarr + 1
                  go to 100
               endif

            endif

   90       continue

  100    continue

      call km ( reflon, reflat, olon, olat, ev(1), ev(2) )
c
c  move the starting coordinates by 1 meter so svd routine
c  will not blow up.  
c
      ev(1) = ev(1) + 0.001 
      ev(2) = ev(2) + 0.001 
      ev(3) = depth 
c
c  renormalize all times so first P arrival is at 10.0 seconds
c  and origin time is at 5 seconds.
c
      ev(4) = 5.d0
      ev(6) = ptime - 10.d0

      do 150 ista = 1,mdim
         if (part(ista).ne.0.0) part(ista) = part(ista) - ev(6)
         if (sart(ista).ne.0.0) sart(ista) = sart(ista) - ev(6)
  150 continue
 
      call pfget_tbl ( pf, 'residual_cutoffs', itbl)

c      write(*,*) 'maxtbl',itbl
      if (itbl .ne. 0) then 
         nrows = maxtbl(itbl)
c         write(*,*) 'residual_cutoffs nrows', nrows

         line(1:80)    = ' '
         do 200 irow = 1, nrows

           call strgettbl(itbl,irow-1,line)
           call gettoken ( line, 1, rwt1 )
           call gettoken ( line, 2, rwt2 )
           read( rwt1,  '(f10.3)')   rwt(irow,1)
           read( rwt2,  '(f10.3)')   rwt(irow,2)

  200    continue
      endif
      

      return
      end


c****************************************************************************

      subroutine readmod(db, vmodel)
c
c  reads model from database db into common
c  calculates gradients,slownesses
c  loads P wave model into common mod 
c  assumption that the layer depths are the same can be relaxed if necessary
c  assumption that the number of layers are required to be the same may
c     not be necessary
c  these assumptions were in the original ANZA models
c  database schema will need to be modified to remove these assumptions
c
# include "db.i"
      implicit real*8 (a-h,o-z)
      parameter (mnod=25)
      common /mod/g(mnod),b(mnod),z(mnod),u(mnod),nodes
      common /psmod/psg(mnod,2),psb(mnod,2),psz(mnod,2),psu(mnod,2),nops
 
      integer        db(4)

      character*(*)  vmodel
      character*40   subset

      integer        P, S
      parameter      (P=1, S=2)

      write(subset(1:40),'(a9,a,a1)') 
     +                   'vmodel=="',vmodel(1:lenb(vmodel)),'"'
c      write(*,*) 'subset = ',subset

      call dblookup(db,db,"","velocity","","")
      call dbsubset(db,db,subset,"")
      call dbquery(db, dbRECORD_COUNT, nvel)

      if (nvel .lt. 1) then
          call register_error (0,"Subroutine readmod")
          call die      (0,"No velocity structure after dbsubset")
      endif

      nops = nvel

c
c  load velocities and gradients
c
      do 100 ivel = 1, nvel
         db(4) = ivel-1
         ierr  = dbgetv( db, "", 
     +             "depth",     psz(ivel,P),
     +             "vp",        psb(ivel,P),
     +             "vs",        psb(ivel,S),
     +             "gradvp",    psg(ivel,P),
     +             "gradvs",    psg(ivel,S),
     +             0)
         psz(ivel,S) = psz(ivel,P)
  100    continue
c
c  calculate gradients
c
c      do 200 ivel = 2, nvel
c         psg(ivel-1,P) = (psb(ivel,P) - psb(ivel-1,P)) / 
c     +                   (psz(ivel,P) - psz(ivel-1,P))
c         psg(ivel-1,S) = (psb(ivel,S) - psb(ivel-1,S)) / 
c     +                   (psz(ivel,S) - psz(ivel-1,S))
c  200    continue
c      psg(nvel,P) = 0.000001
c      psg(nvel,S) = 0.000001
c
c  calculate slownesses at each interface
c

      do 300 ivel = 1, nvel
         psu(ivel,P)=1.d0/psb(ivel,P)
         psu(ivel,S)=1.d0/psb(ivel,S)
  300    continue
c
c  initialize common mod with P wave model
c
      call switchv(P)

      return
      end

c***************************************************************************

      subroutine readstn(db, mdim, vmodel, nsta, stn, levs,
     +                   psig, ssig, psc, ssc, reflat, reflon, sta)
c
c  reads database, calculates slownesses, finds layer containing
c  stations, loads station corrections and weights
c
c  input arguments
c     db      - database pointer
c     mdim    - maximum number of stations
c     vmodel  - velocity model
c
c  output arguments:
c     nsta    - number of stations
c     stn     - location of stations in x,y,z coordinates and u slowness
c     levs    - model layer station is in
c     psig    - station p weight
c     ssig    - station s weight
c     psc     - station p correction
c     ssc     - station s correction
c     reflat  - reference latitude
c     reflon  - reference longitude
c     sta     - station codes
c
# include "db.i"

      implicit real *8 (a-h,o-z)
      parameter (mnod=25)
      common /mod/       g(mnod), b(mnod), z(mnod), u(mnod), nodes
      common /travtable/ ptab(200), xtab(200), ttab(200), maxpindex

c
c     stn(1,i)=x,(2,i)=y,(3,i)=z,(4,i)=u
c
      dimension      stn(4,mdim),  levs(mdim)
      dimension      psig(mdim),   ssig(mdim)
      dimension      psc(mdim),    ssc(mdim)
      character*6    sta(mdim)
      character*(*)  vmodel
      character*40   subset


      integer     db(4), dbs(4), dbsc(4), dbvrf(4)
      integer     tbl

c
c  open and join site and sitecor tables
c

      call dblookup(dbs,  db, "", "site",    "", "")
      call dblookup(dbsc, db, "", "sitecor", "", "")
      call dbquery (dbs,  dbRECORD_COUNT, nsta)
c      write(*,*) 'nsites',nsta

c
c  uniquely sort site view by station name
c
      call strtbl(tbl, "sta", 0)
      call dbsort(dbs, dbs, tbl, 2, " ")
      call dbquery (dbs,  dbRECORD_COUNT, nsta)
c      write(*,*) 'unique nsites',nsta
  

      call dbjoin (dbs, dbs, dbsc, 0, 0, 1, 0, "")
      call dbquery(dbs, dbRECORD_COUNT, nsta)
c      write(*,*) 'nsites joined',nsta
c
c  subset using correct velocity model
c
      write(subset(1:40),'(a9,a,a1)') 
     +                   'vmodel=="',vmodel(1:lenb(vmodel)),'"'
c      write(*,*) 'subset = ',subset

      call dbsubset(dbs, dbs, subset, "")

      call dbquery(dbs, dbRECORD_COUNT, nsta)
c      write(*,*) 'nsites subsetted',nsta

      if (nsta .lt. 1) then
          call register_error (0,"Subroutine readstn")
          call die (0,"No stations after dbsubset site-sitecor join")
      endif


c
c  open velref table, subset to correct model, get top of model
c

      call dblookup(dbvrf, db,  "",     "velref", "", "")
      call dbsubset(dbvrf, dbvrf, subset, "" )
      call dbquery (dbvrf, dbRECORD_COUNT, nmod)
c      write(*,*) 'models subsetted',nmod

      if (nmod .ne. 1) then
          call register_error (0,"Subroutine readstn")
          call die (0,"No model after dbsubset")
      endif

      dbvrf(4) = 0
      ierr  = dbgetv( dbvrf, "", 
     +               "lat",   reflat,
     +               "lon",   reflon,
     +               "refelev",  top,
     +                0)

c      write(*,*) 'top',top
c      write(*,*) 'reflat',reflat
c      write(*,*) 'reflon',reflon

      if (ierr .ne. 0) 
     +    call complain( 0, 'Error writing S wave to assoc table')

      do 100 ista = 1, nsta

         dbs(4) = ista-1

         ierr  = dbgetv( dbs, "", 
     +             "sta",   sta(ista),
     +             "lat",   dlat,
     +             "lon",   dlon,
     +             "elev",  stn(3,ista),
     +             "pcorr", psc(ista),
     +             "scorr", ssc(ista),
     +             "pwgt",  psig(ista),
     +             "swgt",  ssig(ista),
     +             0)
         if (psc(ista).eq.-999.)  psc(ista)  = 0.
         if (ssc(ista).eq.-999.)  ssc(ista)  = 0.
         if (psig(ista).eq.-1.)   psig(ista) = 1.
         if (ssig(ista).eq.-1.)   ssig(ista) = 1.

c  convert to kilometers

         call km( reflon, reflat, dlon, dlat, stn(1,ista), stn(2,ista))
         stn(3,ista) = top - stn(3,ista)

c  find layer receiver is in

         call finlay(stn(3,ista),levs(ista))
         stn(4,ista) = 1.d0 / (b(levs(ista)) + g(levs(ista))
     +                        * (stn(3,ista) - z(levs(ista))))
  100 continue
      return
      end
         

c****************************************************************************

      subroutine finlay(d,layd)
c     finds layer in model that depth d is in.
c     input: d=depth
c     output: layd=model layer

      implicit real*8 (a-h,o-z)
      parameter(mnod=25)
      common /mod/g(mnod),b(mnod),z(mnod),u(mnod),nodes
      
      j=nodes+1
 20   continue
        j=j-1
      if (d.lt.z(j)) goto 20
      layd=j

      return
      end

c****************************************************************************

      subroutine km ( reflon, reflat, dlon, dlat, xkm, ykm)
c
c     calculate (x,y) coordinates given decimal lat and lon
c     origin is reflon, reflat
c
c     input:  
c	reflon	reference longitude
c       reflat  reference latitude
c	dlon    decimal   longitude 
c	dlat    decimal   latitude
c
c     output: 
c	xkm	km in x direction from xo, yo
c       ykm	km in y direction from xo, yo

      implicit  real*8 (a-h,o-z)
      parameter (pion180=3.14159265358979d0/180.d0)

      xkm= 6371.d0 * dcos(dlat * pion180) * (dlon - reflon) * pion180
      ykm= 6371.d0 * (dlat - reflat) * pion180
      return
      end

c***************************************************************************


      subroutine gettoken( string, itoken, token )
c
c   purpose:
c      To take a typed in string of characters and break it
c      up into smaller strings called tokens. Tokens are
c      fields delimited by blanks. Multiple or
c      redundant blanks are treated as one blank. 
c
c   args:
c      string      buffer containing the string to be parsed
c      itoken      token number
c      token       token found ( or eoi for end of input )
c
c
      character*(*) string,token
c
c   see that buffer pointer is in bounds
c
      l  =  len( string )
      if ( itoken .lt. 1 ) then
        token  =  ' '
        return
      end if
c
c   skip white space
c
      ibp     = 0
      istoken = 0
      jtoken  = 0
100   ibp     = ibp + 1
        if (string(ibp:ibp) .ne. ' ' )  then
            if (istoken .eq. 0) then
               jtoken  = jtoken + 1
               istoken = 1
            endif
        else
            istoken = 0
        endif
        if (jtoken .eq. itoken) go to 200
        if( ibp .lt. l ) go to 100
c
c   end of buffer - token = 'eoi'
c
150   token  =  'eoi'
      return
c
c   copy token to output
c
200   ist  =  ibp
400   ibp  =  ibp + 1
      if( string(ibp:ibp) .eq. ' ' ) go to 500
      if( ibp .lt. l ) go to 400
      token  =  string( ist:l )
      return
500   token  =  string( ist:ibp-1 )
      return
      end

c****************************************************************************

      subroutine add_origin ( db, nass, ndef, ev, reflat, reflon,
     +                        algorithm, author, orid)
c
c  inputs
c	db		database pointer
c	nass		number of associated arrivals
c	ndef		number of defining arrivals
c	ev		event coordinates in km
c       reflat  	reference latitude
c	reflon		reference longitude
c	algorithm	algorithm
c	author		author
c
c  output
c	orid		origin id
c
# include "db.i"

      implicit real*8 (a-h,o-z)

      dimension  ev(6)

      integer    db(4)
      integer    orid, grn, srn
      integer    grnumber, srnumber, yearday
     
      character*(*) author, algorithm
      character*7   etype

      etype(1:7) = "l"

      orid = dbnextid(db,"orid")

c      write(*,*) 'orid',orid

c 
c  converts origin from kilometers to latitude and longitude
c

      call dll( reflon, reflat, dlon, dlat, ev(1), ev(2))

c      write(*,*) dlat, dlon, ev(3)

      otime = ev(4) + ev(6)
      grn = grnumber( dlat, dlon )
      srn = srnumber( grn )
      jdate = yearday( otime )

c      write(*,*) grn, srn 
  
      call dblookup ( db, db, "", "origin", "", "" ) 
      db(4) = dbaddnull(db) 
      ierr = dbputv( db, "origin",
     +               "lat",        dlat,
     +               "lon",        dlon,
     +               "depth",      ev(3),
     +               "time",       otime,
     +               "orid",       orid,
     +               "jdate",      jdate,
     +               "nass",       nass,
     +               "ndef",       ndef,
     +               "grn",        grn,
     +               "srn",        srn,
     +               "etype",      etype,
     +               "algorithm",  algorithm,
     +               "auth",       author,
     +               0)
      
      if (ierr .lt. 0) call complain( 0, 'Error writing to origin table')
      return
      end
      


c****************************************************************************

      subroutine add_origerr ( db, orid)
c
c
c
# include "db.i"

      implicit real*8 (a-h,o-z)

      integer    db(4)
      integer    orid
     
      call dblookup ( db, db, "", "origerr", "", "" ) 
      db(4) = dbaddnull(db)
      ierr = dbputv( db, "origerr",
     +               "orid",  orid,
     +               0)
      
      if (ierr .lt. 0) call complain( 0, 'Error writing to origerr table')
      return
      end
     

c****************************************************************************

      subroutine add_assocs  ( db, orid, mdim, sta, parid, sarid,  
     +                         pwt, swt, delta, seaz, esaz, vmodel )
c
c
c
# include "db.i"

      implicit real*8 (a-h,o-z)

      integer      db(4)
      integer      orid,	arid

      integer      parid(mdim), sarid(mdim)

      dimension    pwt(mdim),    swt(mdim)

      character*6   sta(mdim)
      character*(*) vmodel
      character*6   sta1
      character*8   phase
      
      ista = 1
      arid = parid(ista)
      sta1 = sta(ista)
      phase(1:8) = 'P'
      wgt  = pwt(ista)

      do 100 ista = 1, mdim

        if (parid(ista) .ne. -1) then 

            ierr = dbaddv( db, "assoc",
     +                     "arid",    parid(ista),
     +                     "orid",    orid,
     +                     "sta",     sta(ista),
     +                     "phase",   "P",
     +                     "wgt",     pwt(ista),
     +                     "vmodel",  vmodel,
     +                     0)
      
             if (ierr .lt. 0) 
     +          call complain( 0, 'Error writing P wave to assoc table')

             endif

        if (sarid(ista) .ne. -1) then 

            ierr = dbaddv( db, "assoc",
     +                     "arid",    sarid(ista),
     +                     "orid",    orid,
     +                     "sta",     sta(ista),
     +                     "phase",   "S",
     +                     "wgt",     swt(ista),
     +                     "vmodel",  vmodel,
     +                     0)
      
             if (ierr .lt. 0) 
     +          call complain( 0, 'Error writing S wave to assoc table')

             endif

  100   continue
         
      return
      end
      


      
c****************************************************************************

      subroutine dll ( reflon, reflat, dlon, dlat, xkm, ykm)
c
c     calculate  decimal lat and lon given (x,y) coordinates
c     origin is reflon, reflat
c
c     input: 
c	reflon	reference longitude
c       reflat  reference latitude
c	xkm	km in x direction from xo, yo
c       ykm	km in y direction from xo, yo

c     output: 
c	dlon    decimal  longitude 
c	dlat    decimal  latitude
c

      implicit  real*8 (a-h,o-z)
      parameter (pion180=3.14159265358979d0/180.d0)


c      write(*,*) 'dll reflon = ',reflon
c      write(*,*) 'dll reflat = ',reflat

      dlat = reflat + (ykm / pion180 / 6371.d0)
      dlon = reflon + (xkm / dcos(dlat * pion180) / 6371.d0 / pion180)
      return
      end

c****************************************************************************

      subroutine set_def(mdim,    ev,    rwt,    psig, ssig,  
     +                   psc,     ssc,   stnloc, part, sart,   
     +                   pwt,     swt,   delta,  seaz, esaz,   
     +                   parid,   sarid, stacode)
      implicit  real*8 (a-h,o-z)

      dimension    ev(6),           rwt(3,2)
      dimension    psig(mdim),      ssig(mdim)
      dimension    psc(mdim),       ssc(mdim)
      dimension    stnloc(4,mdim)
      dimension    part(mdim),      sart(mdim)
      dimension    pwt(mdim),       swt(mdim)
      dimension    delta(mdim),     seaz(mdim),   esaz(mdim) 

      character*6  stacode(mdim)

      integer      parid(mdim),  sarid(mdim)

c
c  ev(1) - x coordinate of event location in km
c  ev(2) - y coordinate of event location in km
c  ev(3) - z coordinate of event location in km
c  ev(4) - event_time - ev(6)
c  ev(5) - not used 
c  ev(6) - normalizing time factor currently set to 0.0
c          originally set to event_time - 10.d0 to make all times small
c

      ev(1) = 0.d0
      ev(2) = 0.d0
      ev(3) = 0.d0
      ev(4) = 0.d0
      ev(5) = 0.d0
      ev(6) = 0.d0

      do 100 ista = 1, mdim

         psig(ista)    = 1.d0
         ssig(ista)    = 1.d0
         psc(ista)     = 0.d0
         ssc(ista)     = 0.d0
         part(ista)    = 0.d0
         sart(ista)    = 0.d0
         pwt(ista)     = 0.d0
         swt(ista)     = 0.d0
         delta(ista)   = -1.d0
         seaz(ista)    = -999.d0
         esaz(ista)    = -999.d0
         parid(ista)   = -1
         sarid(ista)   = -1
         stacode(ista) = ' '
        

         do 99 ic = 1, 4
            stnloc(ic,ista) = 0.d0
  99        continue
 100     continue

c
c  residual standard deviation cutoffs for removal of large arrival outliers
c  in the subroutine kickout
c  

      rwt(1,1) = 0.1
      rwt(1,2) = 1.5
      rwt(2,1) = 10.5
      rwt(2,2) = 7.5
      rwt(3,1) = 10.5
      rwt(3,2) = 7.5


      return
      end

c****************************************************************************

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

c****************************************************************************

      subroutine check_common
      implicit real*8 (a-h,o-z)

c     mdim must be 10+maximum # of stations

      parameter   (mnod=25,mdim=200)
      parameter   (herra=1.0,herrb=5.0)
      parameter   (ndim=4,ldim=1,ma=2*mdim,nreal=40,nelp=51)

      common /mod/g(mnod),b(mnod),z(mnod),u(mnod),nodes

      common /psmod/psg(mnod,2),psb(mnod,2),psz(mnod,2),psu(mnod,2),nops       

      common /travtable/ptab(200),xtab(200),ttab(200),maxpindex

      write(*,*) "nops  ",nops
      write(*,*) "nodes ",nodes


c***    output initial model to model file
      write(*,*) "pwave model"
      write (*,280) (psb(j,1),j=1,nops)
      write (*,285) (psu(j,1),j=1,nops)
      write (*,290) (psg(j,1),j=1,nops)
      write (*,295) (psz(j,1),j=1,nops)

      write(*,*) "swave model"
      write (*,280) (psb(j,2),j=1,nops)
      write (*,285) (psu(j,2),j=1,nops)
      write (*,290) (psg(j,2),j=1,nops)
      write (*,295) (psz(j,2),j=1,nops)

      write(*,*) "current model"
      write (*,280) (b(j),j=1,nodes)
      write (*,285) (u(j),j=1,nodes)
      write (*,290) (g(j),j=1,nodes)
      write (*,295) (z(j),j=1,nodes)

 280  format('initial velocity model:',/,4(f19.16,1x),/,4(f19.16,1x),/,
     +   4(f19.16,1x),/,4(f19.16,1x),/,4(f19.16,1x),/,4(f19.16,1x))
 285  format('initial slownesses:',/,4(f19.17,1x),/,4(f19.17,1x),/,
     +   4(f19.17,1x),/,4(f19.17,1x),/,4(f19.17,1x),/,4(f19.17,1x))
 290  format('initial gradients:',/,4(f19.17,1x),/,4(f19.17,1x),/,
     +   4(f19.17,1x),/,4(f19.17,1x),/,4(f19.17,1x),/,4(f19.17,1x))
 295  format('depths:',/,4(f19.16,1x),/,4(f19.16,1x),/,
     +   4(f19.16,1x),/,4(f19.16,1x),/,4(f19.16,1x),/,4(f19.16,1x))

      return
      end

c $Id$ 
