!       pgs 11/1/88
!       pgs 7/7/89 modified to change statistics
!       pgs 1/12/90
!       George Helffrich Aug./Oct. 1992
!       Cecily Wolfe Apr 1995 modified for deriving best set of
!       splitting parameters for a multitude of events by
!       computing a stacked, weighted error surface
!       use with SAC macro split_mult
!          Read event information and data from SAC file.
!          Use SAC exclusively for plotting information
!       this version modified allow either minimization of the transverse 
!       component or smaller eigenvalue of the covariance matrix
!       program to compute shear splitting parameters
!       It reads in 3-component data in the order radial, transverse, vertical
!       and expects the specification of two basis vectors in terms of colat,
!       lon. colat in degrees measured from vertical up.  Azimuthal angle
!       measured counterclockwise (right handed) from the
!       radial direction(x coordinate).
!       The program solves for the angle theta which minimizes the smallest
!       eigenvalue of the 2-d covariance matrix of horizontal particle motion.
!       (appropriate for studying S phase with unknow polarity) 
!       for several events at the same station. The individual variance
!       arrays are summed and can be used for a joint estimate for a particular
!       station.  The
!       two parameters are the azimuth of the fast polarization direction
!       and the timelag tlag.
!       The two basis vectors are assumed to be orthogonal
!       The conventions are the following:
!       if deltat is found to be negative, it means that u1 is the fast
!       direction and u2 is the slow direction.  
!       V1 is north and V2 is east.  The data are 'derotated
!       to be in this frame.  The angle found is with
!       respect to the original v1.  THis is converted to clockwise azimuth
!       of fast direction with respect to north. 
!       note: error bars correspond to 1sd.
!
!***    INPUT PARAMETERS
!       Program is run by giving the command dbshearsplit_compute
!       Four arguments are required that must be put on command line
!       1) SAC file names (i of them,i/3 *( E, N, Z) containing data and 
!          event parameters.
!       2) Inversion modes: 't' for minimizing transverse component (appropriate
!       for SKS or 'e' for minimizing smaller eigenvalue of covariance matrix.
!       This is appropriate for other S phases but may also be used for
!       SKS.
!       3) hatndf.  CJW this is not an option now!
!       This is the number of degrees freedom per data point.
!       if this parameter is greater than zero, it will be used to compute
!       the ndf.  Typical values are .3 for RSTN ip channel, .1 for DWWSSN
!       ip channel, and .05 to .1 for GSN 20s/s data.  If hatndf is less
!       than or equal to zero, then it is estimated from the data. 
!       4) fourth argument is the name of the measurement file where the
!       resulting measurments will be stored.  If this parameter isn't given,
!       a measurement file name is constructed from the name of the SAC file.
!______________________________________________________________________________
!
!       Jennifer Bowen 8/19/03
!       Modified to work with the Antelope Real-Time System.
!       Last modified: 10 Sept 2003 5:38pm PST
!______________________________________________________________________________

      program dbshearsplit_compute

#include "db.i"

      ! integer dbSORT_REVERSE
      ! parameter (dbSORT_REVERSE = 1)
      ! integer dbSORT_UNIQUE
      ! parameter (dbSORT_UNIQUE = 2)
      !integer dbOUTER_JOIN
      !parameter (dbOUTER_JOIN = 4)
 
      character*255 database_name, database_name2
      character*256 filterstr, filterstr2, line
      character*6 sta
      character*12 modestr
      character*12 groupsplitstr
      character*12 newsplitstr
      character*1 inversion_mode,imode
      character*11 oridstr
      character*256 splitschema, schemaname, defaultschema,goodschema
      integer pf,retcode,orid,newsplit,groupsplit
      integer db(4),arrivaldb(4),assocdb(4),origindb(4)
      integer sitedb(4),joindb(4),wfdiscdb(4),dbs(4),keys
      integer splitdb(4),dbp(4),composite
      integer strstr,lenstr
      integer sstart,send
      logical verbose, overwrite_schema

      !dbSORT_REVERSE = 1
      !dbSORT_UNIQUE = 2
      !dbOUTER_JOIN = 4

      ! Read database, logfile, number of events and filter string
      ! from parameter file.
      verbose = .false.
      call pfread( "dbshearsplit", pf, retcode )
      if ( retcode .ne. 0 ) call die( 0, "Error in parameter file:" )
      call pfkeys( pf, keys );

      if ( intbl( keys, 'dbname', 6 ) .eq. 1 ) 
     &     call pfget_string( pf, "dbname", database_name )

      if ( intbl( keys, 'inversion_mode', 14 ) .eq. 1)
     &    call pfget_string( pf, "inversion_mode", inversion_mode )

      if ( intbl( keys, 'filter_string', 13 ) .eq. 1) 
     &     call pfget_string( pf, 'filter_string', filterstr )

      if ( intbl( keys, 'split_schema', 12 ) .eq. 1) then
           call pfget_string( pf, 'split_schema', splitschema )
      else
           splitschema = 'ssplit1.0'
      endif
      if ( intbl( keys, 'default_schema', 14 ) .eq. 1) then
           call pfget_string( pf, 'default_schema', defaultschema )
      else
           defaultschema = 'css3.0'
      endif


      call process_args( orid, sta, imode, database_name2, filterstr2,
     &                   verbose, overwrite_schema )

      if ( ( imode .eq. 't' ) .or. ( imode .eq. 'e' ) ) then
          inversion_mode =  imode 
      endif

      if ( lenstr(database_name2) > 0 ) then
          database_name =  database_name2
      endif

      if ( lenstr(filterstr2) > 0 ) then
          if ( filterstr2 .eq. 'none' ) then
              filterstr =  ''
          else
              filterstr =  filterstr2
          endif
      endif

      call int2str(orid,oridstr)

      if ( inversion_mode .eq. 'e' .or. inversion_mode .eq. 'E' ) 
     &    modestr = 'eigenvalue'

      if ( inversion_mode .eq. 't' .or. inversion_mode .eq. 'T' )
     &    modestr = 'transverse'

      if ( verbose ) then
          write (*,*) ''
          write (*,'(a)') 'Computing shear-wave splitting parameters'
          write (*,'(a)') '-----------------------------------------'
          write (*,'(a,a)')'          orid: ',oridstr(1:lenstr(oridstr))
          write (*,'(a,a)') '       station: ', sta(1:lenstr(sta))
          write (*,'(a,a)') '      database: ',
     &                      database_name(1:lenstr(database_name))
          write (*,'(a,a)') '        filter: ', 
     &                      filterstr(1:lenstr(filterstr))
          write (*,'(a,a)')'inversion mode: ',modestr(1:lenstr(modestr))
          write (*,*) ''
      endif

      ! As a means of ensuring the split table exists, open and then
      ! close it. If it doesn't exist, this should create it.
      open (33,file=database_name(1:lenstr(database_name))//'.split')
      close(33)

      ! Determine the format of the database.
      open (33,file=database_name(1:lenstr(database_name)))
      line = '#'
      do while (((line(1:1) .eq. '#' ) .or. (line(1:1) .eq. ' ' )))
          read(33,'(a$)', end=246) line
      end do
246   close(33)

      ! Fail if db is bad.
      if ( line .eq. '#' ) then
          write(*,*) 'Database '//database_name(1:lenstr(database_name))
     &               //' does not exist. Check descriptor file format.'
         call exit(-1)
      end if

      sstart = strstr(line,'schema') + 7
      send = sstart + lenstr(line(sstart:len(line)));
      schemaname = line(sstart:send)
      
      ! Check if the database is of the correct base format.
      if ( strstr(schemaname,defaultschema).eq. 0) then
          write(*,*)'Database must be in css3.0 format; found format '//
     &               schemaname(1:lenstr(schemaname))//'.'
          call exit(-1)
      endif

      goodschema = defaultschema(1:lenstr(defaultschema))//
     &                     ':'//splitschema(1:lenstr(splitschema))

      ! Check if the schema includes the split extension.
      if ( strstr(schemaname,goodschema) .eq. 0 ) then
          ! Quit if we cannot overwrite. 
          if ( .not. overwrite_schema ) then
              write(*,'(a)') 'Database schema for '//
     &                  database_name(1:lenstr(database_name))//
     &                  ' is incorrect.'
              write(*,'(a)') 'Expected '//
     &                  goodschema(1:lenstr(goodschema))//
     &                  ' and found '//
     &                  schemaname(strstr(schemaname,defaultschema):
     &                  lenstr(schemaname))//'.'
              write(*,'(a)') 'Please correct this '//
     &                  'or run with the -x option to overwrite.'
              call exit(-1)
          endif

          ! Overwrite the schema.
          open (33,file=database_name(1:lenstr(database_name)))
          write (33,'(a)' ) 'schema '//goodschema(1:lenstr(goodschema))
          close(33)
      endif 

      ! Open the database.
      if ( dbopen( database_name, 'r+', db ) .lt. 0 ) 
     &    call die( 0, "Can't open database" )

      call dblookup( arrivaldb, db, '', 'arrival', '', '' )
      call dblookup( assocdb, db, '', 'assoc', '', '' )
      call dblookup( origindb, db, '', 'origin', '', '' )
      call dblookup( wfdiscdb, db, '', 'wfdisc', '', '' )
      call dblookup( sitedb, db, '', 'site', '', '' )
      call dblookup( splitdb, db, '', 'split', '', '' )

      call dbsubset(wfdiscdb,wfdiscdb,'sta=~/'//sta(1:lenstr(sta))//'/',
     &              '')

      call dbjoin( joindb, wfdiscdb, arrivaldb, 0, 0, 0, 0, '' )
      call dbjoin( joindb, joindb, assocdb, 0, 0, 0, 0, '' )
      call dbjoin( joindb, joindb, origindb, 0, 0, 0, 0, '' )
      call dbjoin( joindb, joindb, sitedb, 0, 0, 0, 0, '' )

      !call dbjoin( dbs, joindb, splitdb, 0, 0, 0, 0, '' )
      !call dbquery( dbs, dbRECORD_COUNT, ievtot )
      !if ( ievtot .gt. 0 ) joindb = dbs

      !call strtbl( keys, 'splitid', 0 )
      !call dbsort( dbp, joindb, keys, dbSORT_UNIQUE, '' )
      !call strfreetbl( keys )

      call strtbl( keys, 'samprate',  0 )
      ! dbsort options hardcoded to correct bad header file
      call dbsort( dbp, joindb, keys, 1 , '' )
      call strfreetbl( keys )

      call dbquery( dbp, dbRECORD_COUNT, ievtot )

      if ( ievtot .lt. 1 )
     &    call die( 0, "No events found in database." )

      ! Compute parameters for the specified event
      call dbsubset( dbs, dbp, 'orid=='//oridstr(1:lenstr(oridstr)),'');
      call shear_compute( dbs, orid, newsplit,filterstr,inversion_mode,
     &                    verbose )

      if ( newsplit .ne. 0 ) then
          ! Recompute parameters for all events at the station
          write (*,'(a)' ) 'Computing group parameters ...'
          write (*,*) ''

          call dbjoin( dbp, splitdb, dbp, 0, 0, 0, 0, '' )
          call dbsubset( dbs, dbp,'sta=~/'//sta(1:lenstr(sta))//'/','');
          write (filterstr,*) ' '
          call shear_compute(dbs,-1,groupsplit,filterstr,inversion_mode,
     &                       verbose )

          call int2str(newsplit,newsplitstr)
          call int2str(groupsplit,groupsplitstr)

          write (*,'(a)') 'Details saved in row with splitid '//
     &                newsplitstr(1:lenstr(newsplitstr))//'.'
          write (*,'(a)')'Group parameters saved in row with splitid '//
     &                 groupsplitstr(1:lenstr(groupsplitstr))//'.'
          write (*,*)
      endif

      call dbclose( dbp )

      end 

      subroutine shear_compute( db, orid, splitid, filterstr, 
     &                          inversion_mode, verbose )
      integer db(4), orid, splitid
      character*256 filterstr
      character*1 inversion_mode
      logical verbose

#include "db.i"

      parameter (ndim=250000,nadim=200,nshift=401,tlaglim=4.,
     &  nangle=181)
      parameter (aztol = 0.001,numevt=10,numpol=10)
      common/acfcom/acfs1,acfs2,norm
      common/indxcm/lagmin,ianglemin
      logical lev,lndf
      dimension se(ndim), sn(ndim),s(ndim,(3*numevt)),syn(ndim,4)
      dimension ss_ee(1+ndim*2),ss_nn(1+ndim*2),ss_en(1+ndim*2)
      dimension s1(ndim),s2(ndim),s1_int(ndim),s2_int(ndim),
     &    s_work(ndim)
      dimension var_array(nshift,nadim),pol_array(nshift,nadim)
      dimension pol_save(numpol,nshift,nadim)
      dimension tlag_bars(2),angle_bars(2)
      dimension varvec(nshift*nadim)
      dimension var_sum(nshift,nadim)
      dimension loc1(numevt),loc2(numevt)
      dimension ebaz(numevt)
      real var_sum_int(nshift,nadim),var_array_int(nshift,nadim),
     &  pol_array_int(nshift)
      equivalence (varvec,var_array_int)
!     for testing only. remove when done
      dimension s_tst(ndim,2)
      dimension polarloc(numevt)
      integer*4 npts(numevt)
      character*4 scomp(3*numevt)
      logical ltst,seen
      data lev/.true./
      data pi/3.141592654/
      data dt_int/.05/,dt_long/.25/,dt_short/.10/
      data tolerance/0.000001/
      character*255 expression
      character*255 outstr
      character*6 sta
      character*11 oridstr
      double precision slat,slon,elat,elon
      double precision beg,endtime,tloc1,tloc2,startt,endt
      double precision dt1,dt2,dt3,dt,dtmin,dt_max
      double precision eaz,ebaz(numevt)
      real anglemin,polmin,tlag_bar,angle_bar,fac
      integer dbp(4),dbs(4),currentdb(4)
      integer oridloc,splitid,strlen,loc1_old,loc2_old,keys

      ltst= .false.
      seen = .false.
      iseed= 8432
      dtmin = -1.0;

      ! initialize our working arrays.
      do i=1,nshift
          do j=1,nangle
              var_sum(i,j)=0.
              var_sum_int(i,j)=0.
          enddo
      enddo

      call int2str(orid, oridstr)

      ! Do a unique sort on our view so each row corresponds
      ! to a single event.
      call strtbl( keys, 'orid', 0 )
      call dbsort( dbp, db, keys, 2, '' )
      call strfreetbl( keys )
      call dbquery( dbp, dbRECORD_COUNT, ievtot )

      if ( ievtot .lt. 1 )
     &    call die( 0, "No events found in database." )

      pcount = 0
      t = 0

      do ievt=1,ievtot
          ! write(*,*) 'Event # ', ievt
          dbp(4) = ievt - 1

          if ( dbgetv( dbp, '',
     &                      'lat', elat,
     &                      'lon', elon,
     &                      'site.lat', slat,
     &                      'site.lon', slon,
     &                      'time', beg,
     &                      'endtime', endtime,
     &                      'samprate', dt,
     &                      'orid', oridloc,
     &                      'sta', sta,
     &                      0 ) .eq. dbINVALID ) then
              call complain( 0, "Could not retrieve database row" )
              cycle
          endif
          
          ! If we are computing group parameters, the split table
          ! should be part of the current view.  Get the filter for
          ! the given event.
          if ( orid .eq. -1 ) then
              ! If we are computing the group parameters,
              ! we need to skip the group parameter row.
              ! Cheaper to skip than to subset, and we need
              ! this row later ...
              if ( oridloc .eq. -1 ) cycle

              ! Get the filter used for this row
              if ( dbgetv( dbp, '', 'filter', filterstr, 0 )
     &             .eq. dbINVALID ) then
                   call int2str(oridloc,oridstr)
                   outstr = 'Could not retrieve filter '//
     &                     'for event '//oridstr(1:lenstr(oridstr))
                   call complain( 0, outstr )
                   call int2str(orid,oridstr)
              endif
          endif
          
          ! Get the P pick time.
          write (expression,*) "iphase==\'A\' && orid==",oridloc

          call dbsubset( currentdb, db, expression, '' )
          call dbquery( currentdb, dbRECORD_COUNT, numrecs )
          if ( numrecs .le. 0 ) then
              if ( orid .ne. -1 )
     &               write(*,*) 'Skipping event with orid '//
     &                 oridstr(1:lenstr(oridstr))//
     &                 ' (no start window found)'
              cycle
          endif

          currentdb(4) = 0
          if ( dbgetv( currentdb,'', 'arrival.time', tloc1,
     &                 0 ) .eq. dbINVALID ) then
              if ( orid .ne. -1 )
     &               write(*,*) 'Skipping event with orid '//
     &                   oridstr(1:lenstr(oridstr))//
     &                   ' (database is broken, run dbverify)'
              cycle
          endif


          ! Get the S pick time.
          write (expression,*) "iphase==\'F\' && orid==",oridloc

          call dbsubset( currentdb, db, expression, '' )
          call dbquery( currentdb, dbRECORD_COUNT, numrecs )
          if ( numrecs .le. 0 ) then
              if ( orid .ne. -1 )
     &               write(*,*) 'Skipping event with orid '//
     &                 oridstr(1:lenstr(oridstr))//
     &                 ' (no end window found)'
              cycle
          endif

          currentdb(4) = 0
          if ( dbgetv( currentdb,'', 'arrival.time', tloc2,
     &                 0 ) .eq. dbINVALID ) then
              if ( orid .ne. -1 )
     &               write(*,*) 'Skipping event with orid '//
     &                   oridstr(1:lenstr(oridstr))//
     &                   ' (database is broken, run dbverify)'
              cycle
          endif

          seen = .true.

          istart=(ievt-1)*3+1
          iend=istart+2

          if ( dbgetv( currentdb, '', 'esaz', eaz,
     &                 'seaz', ebaz(ievt), 0 ) .eq. dbINVALID )
     &        call disaz(elat,elon,slat,slon,eaz,ebaz(ievt),ekm,edist)

          ! Set the inversion mode.
          if (inversion_mode.eq.'T' .or. inversion_mode.eq.'t') then
              lev=.false.

          elseif (inversion_mode.eq.'E' .or. inversion_mode.eq.'e') then
              lev=.true.

              if (pcount .gt. numpol) then
                   write(*,*) 'not yet set-up for more than ', numpol,
     &     't solutions'
                  stop
              endif
              
              pcount=pcount+1
              polarloc(ievt)=pcount
          elseif ( inversion_mode .eq. ' ' ) then
              write(*,*)'inversion option not designated, abort'
              stop
          else
              write(*,*)'invalid option ',inversion_mode,' abort'
              stop
          endif
          
!cc       ndf will always be computed from the data for now.
          hatndf_sum=0.
          lndf=.false.
          rad=180./pi
          dt_old = 0.0

!cc       jshiftmax is the max number of points we could possibly lag by
!cc       this is determined from the size of the shift window
          jshiftmax=(nshift-1)/2

!         zero var_array,var_array_int 
          do i=1,nshift
              do j=1,nangle
                  var_array(i,j)=0.
                  var_array_int(i,j)=0.
              enddo
          enddo

          ! Get the component data for this event.
          call dbsubset( currentdb,db,expression,'' )
          call strtbl( keys, 'chan', 0 )
          call dbsort( currentdb, currentdb, keys, 2, '' )
          call strfreetbl( keys )
          call dbquery( currentdb, dbRECORD_COUNT, numrecs )

          ! E component
          currentdb(4) = 0
          if ( dbgetv( currentdb, '', 'samprate', dt1, 0 )  .eq.
     &         dbINVALID ) then
              call complain( 0, 
     &              'Could not retrieve east component data.')
              cycle
          endif
          dt1 = 1 / dt1
          call trgetwf( currentdb, 0, s(1,istart), ndim, beg,
     &                  0, startt, endt, npts1, 0, 0 )

          ! N component
          currentdb(4) = 1
          if ( dbgetv( currentdb, '', 'samprate', dt2, 0 )  .eq.
     &         dbINVALID ) then
              call complain( 0, 
     &              'Could not retrieve north component data.')
              cycle
          endif
          dt2 = 1 / dt2
          call trgetwf( currentdb, 0, s(1,istart+1), ndim, beg, 
     &                  0, startt, endt, npts2, 0, 0 )

          ! Z component
          currentdb(4) = 2
          if ( dbgetv( currentdb, '', 'samprate', dt3, 0 )  .eq.
     &         dbINVALID ) then
              call complain( 0, 
     &              'Could not retrieve verticle component data.')
              cycle
          endif
          dt3 = 1 / dt3
          call trgetwf( currentdb, 0, s(1,iend), ndim, beg, 
     &                  0, startt, endt, npts3, 0, 0 )

          ! if any of the components was recorded at a different
          ! sampling interval, re-interpolate over the smallest interval
          npts(ievt) = npts1

          if ( dtmin .lt. 0 ) dtmin = dt1
          if ( dt2 .lt. dtmin ) dtmin = dt2
          if ( dt3 .lt. dtmin ) dtmin = dt3
          if ( dtmin .lt. dt_short) dtmin = dt_short
          if ( dtmin .gt. dt_long ) dtmin = dt_long
          dt_max = 0

          if ( abs(dt1 - dtmin) > tolerance ) then
              call zeroarray( s_work, 1, ndim )
              fac = dt1/dtmin
              call spline_int( s(1,istart),s_work,fac,npts1,npts_save)
              call tscopy(s_work,s(1,istart),npts_save)
              dt_old = dt1
              npts(ievt) = npts_save
          endif

          if ( abs(dt2 - dtmin) > tolerance ) then
              call zeroarray( s_work, 1, ndim )
              fac = dt2/dtmin
              call spline_int(s(1,istart+1),s_work,fac,npts2,npts_save)
              call tscopy(s_work,s(1,istart+1),npts_save)
              dt_old = dt2
              npts(ievt) = npts_save
          endif

          if ( abs(dt3 - dtmin) > tolerance ) then
              call zeroarray( s_work, 1, ndim )
              fac = dt3/dtmin
              call spline_int( s(1,iend),s_work,fac,npts3,npts_save)
              call tscopy(s_work,s(1,iend),npts_save)
              dt_old = dt3
              npts(ievt) = npts_save
          endif

          dt = dtmin
          if ( dt_max .gt. dt_long ) dt_max = dt_long

          ! Compute the number of data points in the waveform before the
          ! P arrival and before the S arrival.  If the sample rates are
          ! different and we interpolated, we have to do extra work.
          if ( abs(dt_old) > tolerance ) then
              loc1_old = nint((tloc1-beg)/dt_old)
              loc1(ievt) = float(loc1_old-1)*dt_old/dt + 1.

              loc2_old = nint((tloc2-beg)/dt_old)
              loc2(ievt) = float(loc2_old-1)*dt_old/dt + 1.
          else
              loc1(ievt) = nint((tloc1-beg)/dt)
              loc2(ievt) = nint((tloc2-beg)/dt)
          endif

!         demean data , detrend data  
          call dtr(s(1,istart),npts(ievt),2)      
          call dtr(s(1,(istart+1)),npts(ievt),2)  
          call dtr(s(1,iend),npts(ievt),2)

!*******check for long period************************************************
          if (dt.gt.dt_long) then
!             long period

              do icomp=istart,iend
                  call zeroarray(s_work,1,ndim)
                  call spline_int(s(1,icomp),s_work,real(dt/dt_long),
     &                            npts(ievt),npts_long)
                  call tscopy(s_work,s(1,icomp),npts_long)
              enddo

!             redefine loc1,loc2,npts(ievt)
              loc1(ievt)=float(loc1(ievt)-1)*dt/dt_long + 1.
              loc2(ievt)=float(loc2(ievt)-1)*dt/dt_long + 1.
              npts(ievt)=npts_long
              dt=dt_long
          endif 
!*******end of long period check

          length=npts(ievt)

          call tscopy(s(1,istart),se,npts(ievt))
          call tscopy(s(1,(istart+1)),sn,npts(ievt))

!cc       We now have se and sn available as working arrays for the segments
!cc       given.  sn starts one sample later.

!****************************************************************************
!*******Begin of test block**************************************************
!*****************************************************************************
          if (ltst.eq..true.) then
              write(*,*)'caution, this is a test run for SKS phase!!!'
              tlag_tst= 1.0                      
              azimuth_tst=-20.0                

!             Rotate to change e-n to r-t time series. 
!cc           This should cause the contents of s to be rotated.
              call rotcomp(se,sn,s(1,istart),s(1,(istart+1)),
     &                     npts(ievt),(90-ebaz(ievt))/rad)

!             zero out t 
              call zeroarray(s(1,istart+1),1,ndim)  

!             rotate back to e & n
!cc           puts in anistropy into s_tst with azimuth_tst and timeshift 
!cc           ebaz(ievt)
              call aniput(s(1,istart),s(1,(istart+1)),s_tst(1,1),
     &                    s_tst(1,2),npts(ievt),dt,tlag_tst,
     &                    azimuth_tst, ebaz(ievt))

!             add noise to both components. 
!             get average rms signal and set noise to .1 of this      

              rms=0.
              do kk=1,npts(ievt)
                  rms=rms+(s_tst(kk,1)**2+s_tst(kk,1)**2)
              enddo
              rms=sqrt(rms/(2.0*float(npts(ievt))))

!             initiallize
!cc           zero the s arrays and init them to the rms signal strength times
!cc           a gaussian distributed random number
              iseed=iseed+1
              jstt=RAND(iseed)
              call zeroarray(s(1,istart+1),1,ndim)  
              call zeroarray(s(1,istart),1,ndim)  
              do kk=1,npts(ievt)
                  s(kk,istart)=.8*rms*GAUSS(iseed)
                  s(kk,istart+1)=.8*rms*GAUSS(iseed)
              enddo

!             filter noise with SAC 2-pole, 2-pass, bp bu filter, 0.01 to 0.1
!cc           xapiir must be the noise fitering function
!              call xapiir(s(1,istart),npts(ievt),'BU',0.3,30.0,2,'BP',
!     &       0.01,0.1,dt,2)
!              call xapiir(s(1,(istart+1)),npts(ievt),'BU',0.3,30.0,2,
!     &         'BP',0.01,0.1,dt,2)

              do kk=1,npts(ievt)
                  s_tst(kk,1)=s_tst(kk,1)+s(kk,istart)
                  s_tst(kk,2)=s_tst(kk,2)+s(kk,istart+1)
              enddo      
              call rotcomp(s_tst(1,1),s_tst(1,2),se,sn,
     &                     npts(ievt),(-(90-ebaz(ievt)))/rad)

!             copy back to s array to save for later
!cc           s(1, istart) and s(s, istart+1) will be kept
              call tscopy(se,s(1,istart),npts(ievt))
              call tscopy(sn,s(1,(istart+1)),npts(ievt))

!       aniput test ended                           


!***************************************************************************
!*******End of test block*************************************************
!***************************************************************************
          endif

          if ( filterstr .ne. '' ) then
              call trfilter_seg( npts(ievt), dt, se,filterstr )
              call trfilter_seg( npts(ievt), dt, sn,filterstr )
          endif

          npts1=loc2(ievt)-loc1(ievt)+1

!cc       tlaglim is our upper limit on lag ( about 4 secs )
!cc       so nlag is the number of samples in tlaglim
!cc       and nshift1 is twice this
          nlag = int(tlaglim/dt+.5)
          nshift1 = 2*nlag + 1

!         check if nshift1 is larger than nshift
          if (nshift1.gt.nshift) then
              write(*,*)'nshift1 greater than nshift, pgm abort'
              stop
          endif

!         form cross correlations on subinterval
!         zero out ss, cross cor arrays
          do kk=1,ndim*2
              ss_ee(kk)=0.
              ss_nn(kk)=0.
              ss_en(kk)=0.
          enddo

!         don't normalize
          norm=0
          call crosscorr2(se(loc1(ievt)), se(loc1(ievt)),npts1,ss_ee)
          call crosscorr2(sn(loc1(ievt)), sn(loc1(ievt)),npts1,ss_nn)
          call crosscorr2(se(loc1(ievt)), sn(loc1(ievt)),npts1,ss_en)
          npts2=npts1*2+1   

          ! length of the cross correlation time series
          ! zero lag is in npts1+1 position
          if (lev.eq..true.) then  
           !calculate covariance matrix
!           calculate smaller (of the two) eigenvalues for all angles,tlag
!cc           pol_array, var_array, ratio are calculated
              call e_ev_mult(ss_ee,ss_nn,ss_en,npts1+1,
     &            var_array,pol_array,nangle,nshift1,nadim,nshift,ratio)

!             save pol_array for later
              do i=1,nangle 
                  do k=1,nshift
                      pol_save(int(polarloc(ievt)),k,i)=pol_array(k,i)
                  enddo
              enddo
          else 

!             !transverse component
!             calculate energy on transverse component for all angles,tlag
              call e_t_mult(ss_ee,ss_nn,ss_en,npts1+1,var_array,nangle,
     &                      nshift1,nadim,nshift,ebaz(ievt))  
          endif

!         interpolate var_array in the tlag direction. check if necessary
          if ( dt .lt. dt_int ) then
              dt_int=dt
          endif

          if ( abs(dt - dt_int) .lt. tolerance ) then
              nshift_int=nshift1
              do i=1,nangle
                  call tscopy(var_array(1,i),var_array_int(1,i),
     &             nshift_int)
              enddo
          else
              fac=dt/dt_int
              do i=1,nangle 
                  call spline_int(var_array(1,i),var_array_int(1,i),fac,
     &      nshift1,nshift_int) 
                  if (nshift_int.gt.nshift.and.i.eq.1) then
                      write(*,*)'nshift_int gt nshift ',nshift_int,
     &                 nshift,'abort'
                      stop
                  endif
              enddo
          endif

!         now add to var_sum
          do i=1,nshift_int
              do j=1,nangle
                  var_sum_int(i,j)=var_sum_int(i,j)+var_array_int(i,j)
                  var_sum(i,j)=var_sum(i,j)+var_array(i,j)
              enddo
          enddo
      enddo  ! END loop on all events

!     zero lag is in the (nshift_int-1)/2+1 position
!     find minimum value and confidence interval      
!     find minimum value of angle tlag. we need this now to get reconstructed
!     seismogram to get number of degrees of freedom.  Will be computed
!     again in shear_bars_all

      if ( .not. seen ) then
          if ( orid .ne. -1 )
     &        write (*,*) 'No events processed.'
          return;
      endif 

      call shear_min(var_sum_int,nangle,nshift_int,nadim,nshift,
     &    dt_int,anglemin,tlagmin)   
!     convert anglemin and tlag to the azimuth (clockwise from
!     north and a positive tlag.  Before conversion, anglemin is the
!     counterclockwise angle from east of the fast direction 
!     direction if tlag is negative, and the slow direction tlagmin
!     is positive. thus...
      if (tlagmin.le.0.) then
          sign=-1.
          anglemin=90.-anglemin
      else
          sign=1.
          anglemin=-anglemin
          if (anglemin.lt.-90.)anglemin=anglemin+180.
      endif
      tlagmin=abs(tlagmin)
      jshift0=tlagmin/dt_int          

      ptsum=0
      ndfsum=0

      do ievt=1,ievtot

!         Need to reread in header into
          if (inversion_mode.eq.'T' .or. inversion_mode.eq.'t') then
              lev=.false.
          elseif (inversion_mode.eq.'E' .or. inversion_mode.eq.'e') then
              lev=.true.

              if (pcount .gt. numpol) then
                  write(*,*) 'not yet set-up for more than ', numpol,
     &     't solutions'
                  stop
              endif
          endif
 
          istart=(ievt-1)*3+1
          iend=istart+2

!         reconstruct (radial and transverse) or (pol and pol-perp) 
!         seismograms with lag 
!         first, rotate into fast, slow coordinate system.  Since anglemin
!         is clockwise wrt north, and rotcomp need counterclockwise wrt E
!         we have to send 90-anglemin if x=east and y=north
          call tscopy(s(1,istart),se,npts(ievt))
          call tscopy(s(1,(istart+1)),sn,npts(ievt))
          call rotcomp(s(1,istart),s(1,(istart+1)),s1,s2,npts(ievt),
     &                 (90.-anglemin)/rad)

!         now s1 and s2 are in the appropriate frame
!         interpolate data to dt_int sec. If dt=dt_int, then just copy arrays
          if ( abs(dt - dt_int) .lt. tolerance ) then
              call tscopy(s1,s1_int,npts(ievt))
              call tscopy(s2,s2_int,npts(ievt))
              npts_int=npts(ievt)
          else
              if (npts(ievt)*dt/dt_int .gt. ndim)
     &            stop '**Resampling generates too many data points.'
              call spline_int(s1,s1_int,real(dt/dt_int),npts(ievt),
     &                        npts_int)
              call spline_int(s2,s2_int,real(dt/dt_int),npts(ievt),
     &                        npts_int)
          endif

!         zero out syn 
          do k=1,4
              call zeroarray(syn(1,k),1,npts_int) 
          enddo

!         shift seismograms by tlag to get rid of anisotropy and rotate into
!         e-w n-s coord   
!         note, at this stage tlagmin is always positive
          if (jshift0.le.0) then
              write(*,*)'jshift0 is less than 0 ',jshift,' abort'
              stop
          endif

          call rotcomp(s1_int,s2_int(1+jshift0),syn(1,1),syn(1,2),
     &                  npts_int,(anglemin-90.)/rad)

          if (dt.ne.dt_int) then
              loc1_int=float(loc1(ievt)-1)*dt/dt_int + 1.
              loc2_int=float(loc2(ievt)-1)*dt/dt_int + 1.
          else
              loc1_int=loc1(ievt)   
              loc2_int=loc2 (ievt)
          endif

          npts1_int=loc2_int-loc1_int+1
          npts1=loc2(ievt)-loc1(ievt)+1

!*******  find polarization of the minumum variance array elt
          if (lev.eq..true.) then
              do i=1,nangle 
                  do k=1,nshift
                      pol_array(k,i)=pol_save(int(polarloc(ievt)),k,i)
                  enddo
              enddo

              if (abs(dt - dt_int) .lt. tolerance) then
                  polmin=pol_array(lagmin,ianglemin)
              else
                  call spline_int(pol_array(1,ianglemin),pol_array_int,
     &                            fac,nshift1,nshift_int)
                  polmin=pol_array_int(lagmin)
              endif

              call rotsub(syn(1,2),syn(1,1),syn(1,1),syn(1,2),
     &                    npts_int,(180.+polmin)/rad)
          else
              call rotsub(syn(1,2),syn(1,1),syn(1,1),syn(1,2),
     &                    npts_int,ebaz(ievt)/rad)
          endif

!*************************************************************************
!c        compute error bars for angle,tlag
          ndf=ndf_fun2(syn(loc1_int,2),npts1_int,npts1) 
          ndfsum=ndfsum+ndf
          ptsum=ptsum+npts1

!         put polmin on interval -90 90
801       if (polmin.lt.-90..or.polmin.gt. 90.) then
              if (polmin.lt.-90.)polmin=polmin+180.
              if (polmin.gt. 90.)polmin=polmin-180.
              go to 801
          endif

          if (lev.eq..false.) then  
              polmin=ebaz(ievt)
          endif

      enddo

      hatndf=float(ndfsum)/float(ptsum)

!     ftest_mse_2 is specialized to two parameters and 95% conf level.
      varmax=ftest_mse_2(ndfsum)
      !varmin = 1.0
      call shear_bars_ev(var_sum_int,varmax,
     &    nangle,nshift_int,nadim,nshift,
     &    dt_int,anglemin,tlagmin,angle_bars,tlag_bars,varmin)   

!     get average bars
      angle_bar=amax1(abs(angle_bars(1)),abs(angle_bars(2)))
      tlag_bar=amax1(abs(tlag_bars(1)),abs(tlag_bars(2)))

      close(34)
!*******the following has to be done again to make sure nothing is screwed***
!*******up since tlagmin and anglemin have been changed above***************
!     convert anglemin and tlag to the azimuth (clockwise from
!     north and a positive tlag.  Before conversion, anglemin is the
!     clockwise angle from east of the fast direction of the fast
!     direction if tlag is negative, and the slow direction tlagmin
!     is positive. thus...
      if (tlagmin.le.0.) then
          sign=-1.
          anglemin=90.-anglemin
      else
          sign=1.
          anglemin=-anglemin
          if (anglemin.lt.-90.)anglemin=anglemin+180.
      endif

      tlagmin=abs(tlagmin)
!*******end of repeated section*********************************************** 
      ! dbopen expects azimuth and polmin on the interval of 0-360, so
      ! make sure that's right.
      if ( anglemin .lt. 0.0 ) 
     &    anglemin = 360 - amod(abs(anglemin), 360.0)

      if ( anglemin .ge. 360 )
     &    anglemin = amod(anglemin,360.0)

      if ( polmin .lt. 0.0 ) 
     &    polmin = 360 - amod(abs(polmin), 360.0)

      if ( polmin .ge. 360 )
     &    polmin = amod(polmin,360.0)

      ! Blank out filterstr if we are computing group parameters.
      ! Filter should only apply to individual events.
      if ( orid .eq. -1 )  then
          filterstr = ''
          composite = 1
      else
          composite = 0
          if ( verbose ) then
              write(*,102) '      Estimated time lag: ', tlagmin, 
     &                     ' (+/- ', tlag_bar, ') seconds'
              write(*,102) 'Fast direction of motion: ', anglemin,
     &                     ' (+/- ', angle_bar, ') degrees'
              write(*,*), ''
          endif
      endif

      ! Check if there is a composite value in the split table,
      ! indicated by a row containing a null orid.
      call dblookup( dbp, db, '', 'split', '', '' )
      call dbsubset(dbs,dbp,'orid=='//oridstr(1:lenstr(oridstr))//
     &                      '&&sta=~/'//sta(1:lenstr(sta))//'/','')

      call dbquery( dbs, dbRECORD_COUNT, nrecs )

      ! If a row exists, overwrite it.
      if ( nrecs .ge. 1 ) then
          dbs(4) = 0;
          if ( dbgetv( dbs, '', 'splitid', splitid, 0) .lt. 0 ) 
     &        call die( 0, "Can't read old splitting parameters.")
          if ( dbputv( dbs,'','azmin',dble(anglemin),'azminerr',
     &             dble(angle_bar),'tlag',dble(tlagmin),'tlagerr',
     &             dble(tlag_bar),'polmin',dble(polmin),'ndf',ndf,
     &             'varmin',dble(varmin),'varmax',dble(varmax),
     &             'sta',sta(1:lenstr(sta)),'splitid',splitid, 
     &             'filter',filterstr,'orid',orid,'composite',composite,
     &             'invmode', inversion_mode(1:1), 0)  .lt. 0 ) 
     &        call die( 0, "Can't write splitting parameters." )

      ! Otherwise, add a new row to the split table.
      else
          splitid = dbnextid( dbp, 'splitid' )
          if ( dbaddv( dbp,'split','azmin',dble(anglemin),'azminerr',
     &             dble(angle_bar),'tlag',dble(tlagmin),'tlagerr',
     &             dble(tlag_bar),'polmin',dble(polmin),'ndf',ndf,
     &             'varmin',dble(varmin),'varmax',dble(varmax),
     &             'sta',sta(1:lenstr(sta)),'splitid',splitid,
     &             'filter',filterstr,'orid',orid,'composite',composite,
     &             'invmode', inversion_mode(1:1), 0)  .lt. 0 ) 
     &        call die( 0, "Can't write splitting parameters." )
      endif

100   format(1x,a4,1x,f4.0,1x,f4.0,3x,f5.2,5x,f4.2,2x,f6.2,i8,f6.3,
     &       e10.2)
101   format(1x,f7.3,1x,f7.3,1x,f7.3,1x,f7.3,1x,f7.3,1x,f7.3,1x,a1,
     &       1x,f7.3,1x,f7.3)
102   format(a26,f6.2,a6,f5.2,a9)
      end

      subroutine process_args(orid, sta, inversion_mode, dbname, filter,
     &                        verbose, overwrite_schema )
      integer orid
      character*1 inversion_mode
      character sta *(*)
      character dbname *(*)
      character filter *(*) 
      character*255 arg
      logical verbose, skipnext, overwrite_schema
      integer numargs
      integer i

      skipnext = .false.
      numargs = iargc()
      if ( numargs .lt. 1 ) call usage()
      do i=1,numargs,1
         if ( skipnext ) then
             skipnext = .false.
             cycle
         endif 

         skipnext = .true.
         call getarg( i, arg )

         if ( arg .eq. "-d" ) then
             call getarg( i+1, dbname )

         elseif ( arg .eq. "-f" ) then
             if ( i == numargs )  call usage()
             call getarg( i+1, filter )

         elseif ( arg .eq. "-x" ) then
             overwrite_schema = .true.
             skipnext = .false.

         elseif ( arg .eq. "-v" ) then
             verbose = .true.
             skipnext = .false.

         elseif ( arg .eq. "-s" ) then
             if ( i == numargs )  call usage()
             call getarg( i+1, sta )

         elseif ( arg .eq. "-i" ) then
             if ( i == numargs )  call usage()
             call getarg( i+1, inversion_mode )

         elseif ( arg .eq. "-o" ) then
             if ( i == numargs )  call usage()
             call getarg( i+1, arg )
             read(arg,*) orid 
         end if
      end do

      if ( lenstr(sta) .eq. 0 ) call usage()
      if ( orid .eq. 0 ) call usage()

      end


      subroutine usage( )
          write (*,'(a$)') 'Usage: dbshearsplit_compute '
          write (*,'(a$)') '[-d database_path] '
          write (*,'(a$)') '[-f filter] '
          write (*,'(a$)') '[-v] '
          write (*,'(a$)') '[-x] '
          write (*,'(a$)') '[-i inversion_mode] '
          write (*,'(a$)') '[-s station]'
          write (*,'(a$)') ' [-o orid]'
          write (*,*) ''
          stop
      end

