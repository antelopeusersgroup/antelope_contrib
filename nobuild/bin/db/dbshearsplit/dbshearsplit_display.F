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
!       Program is run by giving the command dbshearsplit_display
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

      program dbshearsplit_display
#include "db.i"
      parameter (ndim=250000,nadim=200,nshift=2001,tlaglim=4.,
     &  nangle=181)
      parameter (aztol = 0.001,numevt=100,numpol=10)
      common/acfcom/acfs1,acfs2,norm
      common/indxcm/lagmin,ianglemin
      logical lev,lndf,ex
      dimension se(ndim), sn(ndim),s(ndim,(3*numevt)),syn(ndim,4)
      dimension ss_ee(1+ndim*2),ss_nn(1+ndim*2),ss_en(1+ndim*2)
      dimension s1(ndim),s2(ndim),s1_int(ndim),s2_int(ndim),
     &    s_work(ndim)
      dimension var_array(nshift,nadim),pol_array(nshift,nadim)
      dimension pol_save(numevt)
      dimension tlag_bars(2),angle_bars(2)
      dimension varvec(nshift*nadim)
      dimension var_sum(nshift,nadim)
      dimension loc1(numevt),loc2(numevt)
      dimension ebaz(numevt),lsacpf(numevt)
      real var_sum_int(nshift,nadim),var_array_int(nshift,nadim),
     &  pol_array_int(nshift)
      equivalence (varvec,var_array_int)
!     for testing only. remove when done
      dimension s_tst(ndim,2)
      dimension polarloc(numevt)
      real caz(3*numevt),cin(3*numevt)
      integer*4       iyr,iday,ihr,imin,npts(numevt)
      character*4     dnet,stn,chan,file(3),form
      character charndf*10
      character*1 charev(numevt)
      character*255 line(numevt)
      character*12 basename
      character labr*16,labt*16
      character*80 buffer,linesave*255,flname,
     &  logfl,channel*1,comp*4,dot*1
      character*2 ipic
      character*4 scomp(3*numevt)
      character*7 sphase
      character*128 sacpfx(numevt)
      character*256 filterstr
      logical ltst
      data lev/.true./
      data pi/3.141592654/
      data iout/12/,ioutin/10/
      data dt_int/.05/,dt_long/.25/,dt_short/0.10/
      data comp/'prtz'/,dot/'.'/
      integer db(4),affildb(4),arrivaldb(4),assocdb(4),origindb(4)
      integer sitedb(4),joindb(4),wfdiscdb(4),dbp(4),keys,currentdb(4)
      integer splitdb(4), splitt(4)
      integer pf,retcode, count
      character*1 inversion_mode
      character*255 database_name,database_name2
      character*255 expression
      character*255 scr, errmesg
      character*6 sta
      character*1 inversion_mode
      character*11 splitidstr, oridstr
      double precision slat,slon,elat,elon,dt,dtmin,dtmax
      double precision beg,endtime,tloc1,tloc2,startt,endt,dt1,dt2,dt3
      double precision eaz,ebaz(numevt)
      double precision d_anglemin,d_tlagmin,d_angle_bars,d_tlag_bars
      double precision d_polmin,d_varmin,d_varmax
      integer orid,splitid,npts,strlen,loc1_old,loc2_old,so,eo
      real tlagmin, anglemin,fac
      logical isgroup

      ltst= .false.
      isgroup = .false.
      iseed= 8432
      basename = 'matlab_data/'

      ! initialize our working arrays.
      do i=1,nshift
          do j=1,nangle
              var_sum(i,j)=0.
              var_sum_int(i,j)=0.
          enddo
      enddo

      ! Read database, logfile, number of events and filter string
      ! from parameter file.
      call pfread( "dbshearsplit", pf, retcode )
      if ( retcode .ne. 0 ) call die( 0, "Error in parameter file:" )
      call pfkeys( pf, keys );

      if ( intbl( keys, 'dbname', 6 ) .eq. 1 ) 
     &     call pfget_string( pf, "dbname", database_name )

      if ( intbl( keys, 'logfile', 7 ) .eq. 1 )
     &    call pfget_string( pf, "logfile", logfl )

      call process_args( splitid, database_name2 )

      if ( lenstr(database_name2) > 0 ) then
          write(database_name,'(a$)') database_name2
      endif

      call int2str( splitid, splitidstr )
      eo = lenstr(splitidstr)

      pcount=0
      t = 0

      ! Open the database.
      if ( dbopen( database_name, 'r', db ) .lt. 0 ) 
     &    call die( 0, "Can't open database" )

      call dblookup( affildb, db, '', 'affiliation', '', '' )
      call dblookup( arrivaldb, db, '', 'arrival', '', '' )
      call dblookup( assocdb, db, '', 'assoc', '', '' )
      call dblookup( origindb, db, '', 'origin', '', '' )
      call dblookup( wfdiscdb, db, '', 'wfdisc', '', '' )
      call dblookup( sitedb, db, '', 'site', '', '' )
      call dblookup( splitdb, db, '', 'split', '', '' )

      ! Figure out what orid we will use by subsetting the split table.
      call dbsubset( splitt,splitdb,'splitid=~/'//splitidstr(1:eo)//'/',
     &               '' )
      call dbquery( splitt, dbRECORD_COUNT, ievtot )

      if ( ievtot .ne. 1 ) then
          write ( errmesg, * ) 'splitid ', splitid, ' was not found ',
     &            'in database ',database_name(1:lenstr(database_name)),
     &            '.'
          call die( 0, errmesg )
      endif

      splitt(4) = 0;
      if ( dbgetv(splitt, '', 'orid', orid, 
     &                        'filter', filterstr, 
     &                        'sta', sta, 
     &                        'tlag', d_tlagmin,
     &                        'azmin', d_anglemin,
     &                        'tlagerr', d_tlag_bars,
     &                        'azminerr', d_angle_bars,
     &                        'varmin', d_varmin,
     &                        'varmax', d_varmax,
     &                        'ndf', ndf,
     &                        0 ) .eq. dbINVALID )
     &     call die( 0, "could not retrieve orid from split table." )
      tlagmin = real(d_tlagmin)
      anglemin = real(d_anglemin)
      angle_bars = real(d_angle_bars)
      tlag_bars = real(d_tlag_bars)
      varmin = real(d_varmin)
      varmax = real(d_varmax)

      call int2str( orid, oridstr )

      write (errmesg, '(a,a,a)') "sta=~/", sta(1:lenstr(sta)), "/"
      call dbsubset(wfdiscdb,wfdiscdb,errmesg(1:lenstr(errmesg)),'')
      call dbquery( wfdiscdb, dbRECORD_COUNT, ievtot )
 
      ! If we are not computing the group id, we can make things go
      ! a little faster by subsetting the assocdb table.
      if ( orid .ne. -1 ) then
          write (errmesg,'(a,a)') "orid==", oridstr(1:lenstr(oridstr))
          call dbsubset(assocdb,assocdb,errmesg,'')
          call dbquery( assocdb, dbRECORD_COUNT, ievtot )
      endif

      call dbjoin( joindb, wfdiscdb, arrivaldb, 0, 0, 0, 0, '' )
      call dbjoin( joindb, joindb, sitedb, 0, 0, 0, 0, '' )
      call dbjoin( joindb, joindb, assocdb, 0, 0, 0, 0, '' )
      call dbjoin( joindb, joindb, origindb, 0, 0, 0, 0, '' )
      call dbjoin( joindb, joindb, splitdb, 0, 0, 0, 0, '' )

      ! If we are displaying multiple event paramters, we need
      ! to loop for each row in the split table.  Otherwise,
      ! we only do it for our particular row.
      if ( orid .eq. -1 ) then
          isgroup = .true.
          write (errmesg,'(a)') "orid != -1"
      else
          write (errmesg,'(a,a,a)') "splitid=~/",
     &                              splitidstr(1:lenstr(splitidstr)),"/"
      endif

      call dbsubset(joindb,joindb,errmesg,'')

      call strtbl( keys, 'splitid', 'sta', 0 )
      call dbsort( dbp, joindb, keys, 2, '' )
      call dbquery( dbp, dbRECORD_COUNT, ievtot )

      if ( ievtot .lt. 1 ) then
          write ( errmesg, * ) 'No events were specified. ',
     &            'Expected at least 1 event. Check logic.'
          call die( 0, errmesg )
      endif

      do ievt=1,ievtot
          dbp(4) = ievt - 1

          if ( dbgetv( dbp, '',
     &                      'origin.lat', elat,
     &                      'origin.lon', elon,
     &                      'lat', slat,
     &                      'lon', slon,
     &                      'time', beg,
     &                      'endtime', endtime,
     &                      'samprate', dt,
     &                      'orid', orid,
     &                      'sta', sta,
     &                      'polmin', d_polmin,
     &                      'invmode', inversion_mode,
     &                      0
     &                ) .eq. dbINVALID ) then
              call complain( 0, "Could not retrieve database row" )
              cycle
          endif

          pol_save(ievt) = real(d_polmin)

!          if ( polmin .ge. 180 )
!     &       polmin = 180 - amod( abs(polmin),360.0 )

          call epoch2str( line(ievt), beg, '%y%j' )
          
          ! Get the P pick time.
          write (expression,*) "iphase==\'A\' && sta==\'", 
     &                         sta(1:lenstr(sta)),"\' && orid==",orid

          call dbsubset( currentdb, joindb, expression, '' )
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
              call complain(1, 'Could not retrieve a database row' )
              cycle
          endif

          ! Get the S pick time.
          write (expression,*) "iphase==\'F\' && sta==\'", 
     &                         sta(1:lenstr(sta)),"\' && orid==",orid

          call dbsubset( currentdb, joindb, expression, '' )
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
              call complain(1, 'Could not retrieve a database row' )
              cycle
          endif


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
          
!         cjw commented out
!         call getarg(5,charndf)
!         if (charndf(1:1).ne.' ') then
!             read(charndf,*,iostat=ios)hatndf
!             if (ios.eq.0 .and. hatndf.gt.0.) then
!                 write(*,*)'ndf computed from input value of hatndf=',hatndf
!                 lndf=.true. 
!             endif
!         endif   

          !write(*,*) 'ndf computed from the data'
          charndf(1:1)=' '
          ndf_indx=0
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
          call dbsubset( currentdb,joindb,expression,'' )
          call strfreetbl( keys )
          call strtbl( keys, 'chan', 0 )
          call dbsort( currentdb, currentdb, keys, 2, '' )
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
     &                  beg, startt, endt, npts1, 0, 0 )

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
     &                  beg, startt, endt, npts2, 0, 0 )

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
     &                  beg, startt, endt, npts3, 0, 0 )

          ! if any of the components was recorded at a different
          ! sampling interval, re-interpolate over the smallest interval
          npts(ievt) = npts1
          dtmax = 0

          dtmin = dt1
          if ( dt2 < dtmin ) dtmin = dt2
          if ( dt3 < dtmin ) dtmin = dt3
          if ( dtmin .lt. dt_short) dtmin = dt_short
          if ( dtmin .gt. dt_long ) dtmin = dt_long

          if ( dt1 .ne. dtmin ) then
              call zeroarray( s_work, 1, ndim )
              fac=dt1/dtmin
              call spline_int( s(1,1),s_work,fac,npts1,npts_save)
              call tscopy(s_work,s(1,1),npts_save)
              dt_old = dt1
              npts(ievt) = npts_save
          endif

          if ( dt2 .ne. dtmin ) then
              call zeroarray( s_work, 1, ndim )
              fac=dt2/dtmin
              call spline_int( s(1,2),s_work,fac,npts2,npts_save)
              call tscopy(s_work,s(1,2),npts_save)
              dt_old = dt2
              npts(ievt) = npts_save
          endif

          if ( dt3 .ne. dtmin ) then
              call zeroarray( s_work, 1, ndim )
              fac=dt3/dtmin
              call spline_int( s(1,3),s_work,fac,npts3,npts_save)
              call tscopy(s_work,s(1,3),npts_save)
              dt_old = dt3
              npts(ievt) = npts_save
          endif

          dt = dtmin
          if ( dtmax .gt. dt_lon ) dtmax = dt_long

          ! Compute the number of data points in the waveform before the
          ! P arrival and before the S arrival.  If the sample rates are
          ! different and we interpolated, we have to do extra work.
          if ( abs(dt_old) > tolerance ) then
              loc1_old = nint((tloc1-beg)/dtmin)
              loc1(ievt) = float(loc1_old-1)*dtmin/dt + 1.

              loc2_old = nint((tloc2-beg)/dtmin)
              loc2(ievt) = float(loc2_old-1)*dtmin/dt + 1.
          else
              loc1(ievt) = nint((tloc1-beg)/dt)
              loc2(ievt) = nint((tloc2-beg)/dt)
          endif

          !,sstn,iiyr,iiday

!         demean data , detrend data  
          call dtr(s(1,istart),npts(ievt),2)      
          call dtr(s(1,(istart+1)),npts(ievt),2)  
          call dtr(s(1,iend),npts(ievt),2)

!*******check for long period************************************************
          if (dt.gt.dt_long) then
!             long period

              do icomp=istart,iend
                  call zeroarray(s_work,1,ndim)
                  call spline_int(s(1,icomp),s_work,dt/dt_long,
     &                            npts(ievt),npts_long)
                  call tscopy(s_work,s(1,icomp),npts_long)
              enddo

!             redefine loc1,loc2,npts(ievt)
              loc1(ievt)=float(loc1(ievt)-1)*dt/dt_long + 1.
              loc2(ievt)=float(loc2(ievt)-1)*dt/dt_long + 1.
              npts(ievt)=npts_long
              dt=dt_long
              write(*,*)' npts(ievt), npts_long, dt, dt_long',
     &                  npts(ievt),npts_long,dt,dt_long
          endif 
!*******end of long period check

          length=npts(ievt)

          call tscopy(s(1,istart),se,npts(ievt))
          call tscopy(s(1,(istart+1)),sn,npts(ievt))

!cc       We now have se and sn available as working arrays for the segments
!cc       given.  sn starts one sample later.
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
!cc       nshift1 cannot be greater that nshift.  Why?
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
!              do i=1,nangle 
!                  do k=1,nshift
!                      ! polmin is read in from file
!                      ! pol_save(ievt)=polmin
!                  enddo
!              enddo
          else 

!             !transverse component
!             calculate energy on transverse component for all angles,tlag
              call e_t_mult(ss_ee,ss_nn,ss_en,npts1+1,var_array,nangle,
     &                      nshift1,nadim,nshift,ebaz(ievt))  
          endif

!         interpolate var_array in the tlag direction. check if necessary
          if (dt.lt.dt_int) then
              dt_int=dt
          endif

          if (dt.eq.dt_int) then
              nshift_int=nshift1
              do i=1,nangle
                  call tscopy(var_array(1,i),var_array_int(1,i),
     &                        nshift_int)
              enddo
          else
              fac=dt/dt_int
              do i=1,nangle 
                  call spline_int(var_array(1,i),var_array_int(1,i),fac,
     &                            nshift1,nshift_int) 
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

      ! read in anglemin
      ! read in tlagmin

      jshift0=tlagmin/dt_int          

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
          call rotcomp(se,sn,s1,s2,npts(ievt),(90.-anglemin)/rad)

!         now s1 and s2 are in the appropriate frame
!         interpolate data to dt_int sec. If dt=dt_int, then just copy arrays

          ! Stupid hack to get around rounding errors when comparing
          ! floats
          if ( abs(dt-dt_int) .lt. 0.001 ) then
              call tscopy(s1,s1_int,npts(ievt))
              call tscopy(s2,s2_int,npts(ievt))
              npts_int=npts(ievt)
          else
              if (npts(ievt)*dt/dt_int .gt. ndim)
     &            stop '**Resampling generates too many data points.'
              call spline_int(s1,s1_int,dt/dt_int,npts(ievt),npts_int)
              call spline_int(s2,s2_int,dt/dt_int,npts(ievt),npts_int)
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
          call rotcomp(s1_int,s2_int,syn(1,3),syn(1,4),npts_int,
     &                  (anglemin-90.)/rad)

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
              polmin=pol_save(ievt)
              call rotsub(syn(1,2),syn(1,1),syn(1,1),syn(1,2),
     &                    npts_int,(180.+polmin)/rad)
              call rotsub(syn(1,4),syn(1,3),syn(1,3),syn(1,4),
     &                    npts_int,(180.+polmin)/rad)
          else
              write(*,*)'rotating into radial transverse'
              call rotsub(syn(1,2),syn(1,1),syn(1,1),syn(1,2),
     &                    npts_int,ebaz(ievt)/rad)
              call rotsub(syn(1,4),syn(1,3),syn(1,3),syn(1,4),
     &                    npts_int,ebaz(ievt)/rad)
          endif

!         Instead, we're just going to dump points to a file and read
!         them into matlab.

          ! .ro
          open (33,file= basename//splitidstr(1:eo)//'.ro')
          do kk=(loc1_int-(100/dt)),(loc2_int+(100/dt))
              write(33,'(f15.2,f15.2)') real(kk*dt_int), syn(kk,3)
          enddo
          close(33)

          ! .r1
          open (33,file= basename//splitidstr(1:eo)//'.rl')
          do kk=(loc1_int-(100/dt)),(loc2_int+(100/dt))
              write(33,'(f15.2,f15.2)') real(kk*dt_int), syn(kk,1)
          enddo
          close(33)

          ! .to
          open (33,file= basename//splitidstr(1:eo)//'.to')
          do kk=(loc1_int-(100/dt)),(loc2_int+(100/dt))
              write(33,'(f15.2,f15.2)') real(kk*dt_int), syn(kk,4)
          enddo
          close(33)

          ! .t1
          open (33,file= basename//splitidstr(1:eo)//'.tl')
          do kk=(loc1_int-(100/dt)),(loc2_int+(100/dt))
              write(33,'(f15.2,f15.2)') real(kk*dt_int), syn(kk,2)
          enddo
          close(33)

          ! picks
          open (33,file= basename//splitidstr(1:eo)//'.picks')
          write(33,'(f15.2,f15.2)') real(loc1_int*dt_int),
     &                              real(loc2_int*dt_int)
          close(33)

!         If making the final plot, show time series and 
!         horizontal particle motion
!         in fast, slow frame, both with and without time shift delta t.  
!         resample vertical to match horizontals if necessary
          if (dt.ne.dt_int) then
              call spline_int(s(1,3),syn(1,3),dt/dt_int,npts,npts_int)
          else
              call tscopy(s(1,3),syn(1,3),npts)
          endif
!         De-mean the shifted and rotated signal.  Copy the shifted data into
!         another work area so that detrending doesn't introduce a discontinuity
!         in the unshifted signal.
          call dtr(s1_int(loc1_int),npts1_int,1)
          call tscopy(s2_int(loc1_int+jshift0),s_work,npts1_int)
          call dtr(s_work,npts1_int,1)

!         Check whether signal needs inversion by checking sign of 
!         cross-correlation.  If negative, invert.
          s12=0.
          do kk=1,npts1_int
              s12=s12+s1_int(loc1_int+kk)*s_work(kk)
          enddo

          if (s12.lt.0.) then
              do kk=1,ndim
                  s2_int(kk)=-s2_int(kk)
                  s_work(kk)=-s_work(kk)
              enddo
          endif
      enddo

      if ( .not. isgroup ) then
          ! .scr
          open (33,file= basename//splitidstr(1:eo)//'.scr')
          do kk=1,npts1_int
              write(33,'(f15.2,f15.2)') real(kk*dt_int),
     &                 s1_int(kk+loc1_int)
          enddo
          close(33)

          ! .sct
          open (33,file= basename//splitidstr(1:eo)//'.sct')
          do kk=1,npts1_int
              write(33,'(f15.2,f15.2)') real(kk*dt_int),
     &                 s2_int(kk+loc1_int)
          enddo
          close(33)

          ! .scl
          open (33,file= basename//splitidstr(1:eo)//'.scl')
          do kk=1,npts1_int
              write(33,'(f15.2,f15.2)') real(kk*dt_int), s_work(kk)
          enddo
          close(33)

          ! .xy1
          open (33,file= basename//splitidstr(1:eo)//'.xy1')

          do kk=1,npts1_int
              write(33,'(f15.2,f15.2)') s1_int(kk+loc1_int),
     &                 s2_int(kk+loc1_int)
          enddo
          close(33)

!          ! .xy2
          open (33,file= basename//splitidstr(1:eo)//'.xy2')
          do kk=1,npts1_int
              write(33,'(f15.2,f15.2)') s1_int(kk+loc1_int), s_work(kk)

          enddo
          close(33)

      endif
      if ( isgroup ) orid = -1

      open (33,file=basename//splitidstr(1:eo)//'.specs')
      write(33,*) beg
      write(33,*) orid
      write(33,*) sta
      write(33,*) filterstr
      close(33)


      ! varmin and varmax from first run
      ! read in varmin
      ! read in varmax

!*******end of repeated section***********************************************

!     Try writing out the variance file as a SAC 3-D file for contouring.
!     Place in proper format for contour plot.  Write out only positive
!     lag values.  vmult is the variance conversion to units of 95% conf.
!     level.
      vmult = varmin*varmax
      ix = 0
!     if (sign .gt. 0) then
!     Code commented out (GRH) -- as of Oct. 1992 empirically there is 
!     no difference between the structures of the variance arrays
!     returned in the two cases where tlagmin > 0 and tlagmin < 0.

      if (sign .eq. 0) then
!         First angle corresponds to fast direction of east.  Load
!         contouring array in reverse order.
          do j=nangle,1,-1
              do i=1+nlag,nshift1
                  ix = ix + 1
                  varvec(ix) = var_sum(i,j)/vmult
              enddo
          enddo
      else
!         First angle corresponds to fast direction of north.  Load
!         contouring array from center (west) in reverse down to
!         the beginning (north), then from the end (south) down to
!         the center (east).
           do j=(nangle-1)/2,1,-1
              do i=1+nlag,nshift1
                  ix = ix + 1
                  varvec(ix) = var_sum(i,j)/vmult
              enddo
          enddo
          do j=nangle,(nangle+1)/2,-1
              do i=1+nlag,nshift1
                  ix = ix + 1
                  varvec(ix) = var_sum(i,j)/vmult
              enddo
          enddo
      endif

!     Let's plot anyway
      open (33,file= basename//'contour.var')
      do j=1,(nangle*(nlag+1))
          write(33,'(f8.2)') varvec(j)
      enddo
      close(33)

      open (33,file=basename//'contour.specs')
      write(33,'(f8.4)') tlagmin, anglemin, dt, real(ndf)
      close(33)

!      do j=1,nangle
!          do i=1,nlag+1
!              write(33,'(f8.2,$)') varvec( (j*nlag) + i )
!          enddo
!          write(33,*) 
 
      end

      subroutine process_args( splitid, dbname )
      integer splitid
      character dbname *(*)
      character*255 arg
      integer numargs
      integer i

      numargs = iargc()

      if ( numargs .lt. 1 ) call usage()
      do i=1,numargs,2
         call getarg( i, arg )

         if ( arg .eq. "-d" ) then
             if ( i == numargs )  call usage()
             call getarg( i+1, dbname )
         else
             if ( i .ne. numargs )  call usage()
             read ( arg, * ) splitid

         end if
      end do

      end


      subroutine usage( )
          write (*,*) 'Usage: dbshearsplit_display [-d database_path] ',
     &                'splitid'
          stop
      end

