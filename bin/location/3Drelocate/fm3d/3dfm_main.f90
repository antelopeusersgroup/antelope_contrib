!************************************************************************************************
! This is the main program unit for the 3d-multistage FMM code
! The purpose of this unit is to 
! - read the input files,
! - perform a range of initializations (including the wave front propagation
!   on the refined source grid) by calling routines located in in the file 3dfmlib.f90
! - go through the multistage FMM sequences on the main propagation grid as 
!   defined in the input files by initializing the appropriate intersections 
!   and calling the fast marching routines located in the file propagate.f90
! - call the ray tracing routines for all paths by calling routines from the file rays.f90
! - finish by calling the Frechet derivatives routines in frechet.f90
! - along the way, create the OpenDx visualisation files by calling routines from visual.f90

program fm3d
  use mod_3dfm
  implicit none

  integer                            :: n,i,j,k,m,i1,i2,ns,path_id,nsec,top_id,bot_id,vtype
  integer                            :: ctype,step,prev_tf,n_tf,recpath_id,direction
  integer                            :: t1,t2,t3,t4,t5,count_rate,count_max,io
  integer                            :: is_teleseismic,n_teleseismic,nfile,nsave,save_grid_counter
!  integer                            :: i3,n_tot_ref
  type(Tintersection),pointer        :: istart,inext
  type(Tregion),pointer              :: reg
  type(Tsource),pointer              :: s,ss
  type(Treceiver),pointer            :: rec
  type(Tray),pointer                 :: ray
!  type(Ttime_field),pointer          :: tf1,tf2,tf3
  real(kind=dp)                      :: xr(3),xs(3),xcp(3)
  real(kind=dp)                      :: deg_to_rad,r
!  real(kind=dp)                      :: dist,hb,h,p_arrival,mean_error
  real(kind=dp)                      :: interpolate_interface
  real(kind=dp)                      :: t_arrival
  integer,dimension(:),allocatable   :: npp_receiver,ipp_receiver,iarr,jarr
  logical                            :: do_frechet_derivatives
  real                               :: evlat,evlon,ndlat,ndlon,deltas,cazim,bazim,azima
  integer                            :: myid,nproc


!  logical,dimension(:),allocatable   :: logarr

! read the modes under which the program is to run from file

  open(1,file='mode_set.in')

  read (1,*) file_mode
  read (1,*) no_pp_mode
  read (1,*) parallel_mode
  read (1,*) display_mode
  read (1,*) save_rays_mode
  read (1,*) save_timefields_mode

  close(1)


  if (no_pp_mode .and. file_mode) &
       stop 'if no_pp_mode enabled file_mode must be disabled'

  if (.not.no_pp_mode .and. parallel_mode) &
       stop 'currently parrallel_mode is only possible in no_pp_mode'

  if (parallel_mode) then
     open(1,file='myid')
     read(1,*) nproc,myid
     close(1)
  endif

  call system_clock(t1,count_rate,count_max)

  deg_to_rad=acos(-1.0_dp)/180._dp

!-------------------
! initialize objects that are defined in input files

  write(unit=*,fmt='(a30)',advance='no') 'initializing propagation grid'
  call initialize_propagation_grid
  print *,'......finished'

  write(unit=*,fmt='(a30)',advance='no')' initializing velocity grids '
  call initialize_velocity_grids
  print *,'......finished'

  write(unit=*,fmt='(a30)',advance='no')' initializing interfaces'
  call initialize_interfaces
  print *,'......finished'


!---------------------------------------------------------------------------------------------------
! initialize intersections (interface grid + navigation pointers) and regions (collection of grid
! points between two interfaces, including the bounding intersections ). The fast marching
! solution is evaluated region by region.  

!---------
!  calculate the intersection points, i.e. where the surfaces cut through the regular propagation grid

  n_intersections=n_interfaces
  allocate(intersection(n_intersections))

  do n=1,n_intersections ; call intersection_defaults(intersection(n)) ; intersection(n)%id=n ; end do

  do n=1, n_intersections
     call find_intersection(intersection(n),intrface(n),pgrid)
  end do

  print *,'main grid intersections initialized'


!----------
! set up the 3-D regions that lie between the interfaces 

  n_regions=n_interfaces-1
  allocate(region(n_regions))

  do n=1,n_regions

     call region_defaults(region(n))
     region(n)%id=n

! pointers to the intersections that are the boundaries of the region
     region(n)%itop => intersection(n)
     region(n)%ibot => intersection(n+1)

     if (region(n)%itop%nnode == 0) then
        print *,'region',n,'does not exist'
        region(n)%nnode = 0
        cycle
     endif

     call define_region(region(n),region(n)%itop,region(n)%ibot,pgrid)

  end do

  print *,'main grid regions initialized'


!---------

! set a flag at each node of the main grid that has only regular connected cells
! This is used later to speed up calculations. Intersections have to be initialized to do this

  call tag_regular_nodes(pgrid)


!---------
! calculate the velocities on the regular grid and intersection grid points
! and transfer them to the corresponding regional nodes

  call velocities_to_grid(pgrid)

  do n=1,n_intersections
     call velocities_to_intersection(intersection(n))
  end do

  do n=1,n_regions
     call velocities_to_region(region(n),pgrid)   
  end do

  print *,'velocities on main grid, its intersections and regions evaluated'


!------------------
! read source location from file, and determine some of its properties

  open(1,file='sources.in')

  read (1,*) n_sources             ! the number of sources

!  print *,'nsources is', n_sources

 ! we already need to know the number of receivers since these can potentially become sources
 ! if a phase containing late reflections (pp type) is requested

  open(2,file='receivers.in')

  read(2,*) n_receivers            ! the number of receivers

!  print *,'nreceivers is', n_receivers

  allocate(receiver(n_receivers))
  do n=1,n_receivers ; call receiver_defaults(receiver(n)) ; receiver(n)%id=n ; end do

  allocate(source(n_sources+n_receivers))

  do n=1,n_sources+n_receivers
     call source_defaults(source(n)) 
     source(n)%id=n 
  end do

  n_sources_ppinc=n_sources   ! will contain the number of sources including receivers of pp phases

  n_teleseismic = 0           ! will contain the number of teleseismic sources

  do ns=1,n_sources

     print *,'reading source',ns

     s => source(ns) 

     read (1,*) is_teleseismic

     if (is_teleseismic == 1) then
        
        s%is_teleseismic = .true.
        n_teleseismic = n_teleseismic+1

        read (1,*) s%teleseismic_phase
        read (1,*) s%r,s%lat,s%long

        s%r = earth_radius - s%r
        s%lat=deg_to_rad*s%lat
        s%long=deg_to_rad*s%long
        s%coslat=cos(s%lat)

        s%teleseismic_id = n_teleseismic

     else
        print *,'Local source' !AAA
        s%is_local = .true.

        read (1,*) s%r,s%lat,s%long
        print *,'Source location: ',s%r,s%lat,s%long

        s%r = earth_radius - s%r
        s%lat=deg_to_rad*s%lat
        s%long=deg_to_rad*s%long
        s%coslat=cos(s%lat)

        call initialize_source(s,pgrid)! determines where the source is located wrt grid/intersections


        if (.not. s%on_interface) print *,'source',ns,' is located in region ',s%region_id
        if (s%on_interface)  print *,'source',ns,' is located on interfaces ',s%topint_id,s%botint_id

     endif


!---------------------------------------------------------
! read in the required paths (reflections/transmissions)

     read(1,*) s%n_paths    ! the number of paths to this source

!     print *,s%n_paths,'is npath'

 ! allocate the array containing path information for this source

     allocate(s%path(s%n_paths))
     do n=1,s%n_paths ; call path_defaults(s%path(n)) ; end do

  ! read the paths

     do n=1,s%n_paths

!        print *,'reading path',n

 ! register the path id

        s%path(n)%id  = n 


 ! read the number of timefields on this path (= number of steps in the sequence)
        read(1,*) n_tf

        s%path(n)%n_tf = n_tf

!        print *,'ntf',s%path(n)%n_tf

        allocate(s%path(n)%sequence(2*n_tf))  ! contains the sequence definition
        allocate(s%path(n)%tf_sequence(n_tf)) ! indices of the time fields corresponding to each step
        allocate(s%path(n)%vtype_sequence(n_tf)) ! velocity type of each step

        read (1,*) s%path(n)%sequence(1:2*n_tf)
        read (1,*) s%path(n)%vtype_sequence(1:n_tf)

        if (count(s%path(n)%vtype_sequence(1:n_tf) == 2) > 0 .and. n_vtypes == 1) then
           print *
           print *,'***** ERROR  ***********************************'
           print *, 'two velocity types specified in path but only one defined'
           stop
        endif

!        print *,s%path(n)%sequence(1:2*s%path(n)%n_tf)

     ! check whether the path contains a reflection fit step. Store the number of the step if so

        do step=1,n_tf

           if (s%path(n)%sequence(2*step-1) == s%path(n)%sequence(2*step)) then
              s%path(n)%refstep=step
              s%path(n)%fitting_interface=s%path(n)%sequence(2*step)
              print *,'path',n,'step',step,' is a reflection step'
              if (no_pp_mode) then
                 print *,'when no_pp_mode is enabled late reflections are not allowed'
                 stop 'illegal request for late reflection fit while no_pp_mode enabled'
              endif
           endif

        end do

     end do

!     print *,s%n_paths,' paths read from file'


! first test paths for consistency with source position 

     if (s%is_local) then

        do n=1,s%n_paths
           if (s%path(n)%sequence(1) /= 0) then
              print *
              print *,'***** ERROR  ***********************************'
              print *,'first item in path definition must be 0 for local sources'
              print *,'source',s%id, ' path',n
              stop
           endif
        end do

        if (s%on_interface) then

           do n=1,s%n_paths

              if (.not.((s%path(n)%sequence(2) == s%botint_id+1  &
                   .or. s%path(n)%sequence(2) == s%topint_id-1))) then
                 print *
                 print *,'***** ERROR  ***********************************'
                 print *,'path',s%path(n)%id,' of source',s%id,' inconsistent with source position'
                 stop 
              endif

           end do

        else

           do n=1,s%n_paths

              if (.not.(s%path(n)%sequence(2) == region(s%region_id)%itop%id .or. &
                   s%path(n)%sequence(2) == region(s%region_id)%ibot%id)) then
                 print *
                 print *,'***** ERROR  ***********************************'
                 print *,n,s%path(n)%sequence(2),region(s%region_id)%itop%id, &
                      region(s%region_id)%ibot%id
                 print *,'path',s%path(n)%id,' of source',s%id,' inconsistent with source position'
                 stop 
              endif
           
           end do

        endif

     endif

     if (s%is_teleseismic) then

        do n=1,s%n_paths

           if (.not.(s%path(n)%sequence(2) == n_interfaces-1  &
                .and. s%path(n)%sequence(1) == n_interfaces)) then
              print *
              print *,'***** ERROR  ***********************************'
              print *,'path',s%path(n)%id,' path from teleseismic source does not start at bottom'
              stop 
           endif

        end do

     endif


! then test paths for allowable sequence


     do n=1,s%n_paths

        do m=3,2*s%path(n)%n_tf,2

           if (abs(s%path(n)%sequence(m+1)-s%path(n)%sequence(m))/= 1.and.s%path(n)%sequence(m) /= 0 &
                .and. m /= 2*s%path(n)%refstep-1) then
              print *
              print *,'***** ERROR  ***********************************'
              print *, 'illegal path sequence for source',s%id,' path',n
              stop
           endif

           if ((s%path(n)%sequence(m+1) > n_interfaces .or. s%path(n)%sequence(m+1) < 1) .and. &
                m /= 2*s%path(n)%refstep-1) then
              print *
              print *,'***** ERROR  ***********************************'
              print *, 'non-existing interface in path sequence',m,s%path(n)%refstep
              stop
           endif

           if ((s%path(n)%sequence(m) /= s%path(n)%sequence(m-1)) .and. &
                (s%path(n)%sequence(m) /= s%path(n)%sequence(m-2))) then
              print *
              print *,'***** ERROR  ***********************************'
              print *, 'illegal path sequence for source',s%id,' path',n
              stop
           endif              

        end do
     end do


  end do  ! loop over input sources

close(1)

print *,'finished reading sources'


!---------------------------------------------------------
! read in the receiver properties


do n=1,n_receivers

   rec => receiver(n)

   read(2,*) rec%r,rec%lat,rec%long    ! the position of the receiver

   rec%r = earth_radius - rec%r
   rec%lat = rec%lat*deg_to_rad
   rec%long = rec%long*deg_to_rad

   if ( (rec%lat < pgrid%lat(1) .or. rec%lat > pgrid%lat(pgrid%nlat)) .or. &
        (rec%long < pgrid%long(1) .or. rec%long > pgrid%long(pgrid%nlong))) then
      print *
      print *,'error: receiver',n,' lies outside propagation grid in lat or long'
      print *, 'Receiver Coordinates:'
      print *, rec%r,rec%lat*180/3.14,rec%long*180/3.14
      stop

   else

      if ((rec%r < interpolate_interface(rec%lat,rec%long,intrface(n_interfaces))-0.1_dp*pgrid%tolerance) .or. &
        (rec%r >  interpolate_interface(rec%lat,rec%long,intrface(1))+0.1_dp*pgrid%tolerance) ) then 
         print *,'error: receiver',n,' lies above or below the propagation grid'
         print *,'rec%r=',rec%r
         print *,'lower surface=',interpolate_interface(rec%lat,rec%long,intrface(n_interfaces))
         print *,'upper surface=',interpolate_interface(rec%lat,rec%long,intrface(1))
         !AAA If the receiver is above or below the grid, just set it on the boundary
         if (rec%r>interpolate_interface(rec%lat,rec%long,intrface(1))+0.1_dp*pgrid%tolerance) then
             rec%r=interpolate_interface(rec%lat,rec%long,intrface(1))
         else
             rec%r=interpolate_interface(rec%lat,rec%long,intrface(n_interfaces))
         endif
!AAA         stop
      endif

   endif

   read(2,*) rec%n_rays                               ! the number of paths to this receiver

   allocate(receiver(n)%ray(receiver(n)%n_rays))
   do i=1,receiver(n)%n_rays ; call ray_defaults(receiver(n)%ray(i)) ; end do

   read(2,*) rec%ray(1:rec%n_rays)%source_id  ! the index of the source of the rays

! verify that sources are valid

   if (count(rec%ray(1:rec%n_rays)%source_id > n_sources) > 0) then
      print *
      print *,'****** ERROR: INCONSISTENT INPUT ********'
      print *,'receiver',n,' is requesting paths from a non-existent source'
      stop
   endif

   do i=1,rec%n_rays ; rec%ray(i)%source => source(rec%ray(i)%source_id) ; end do

   read(2,*) rec%ray(1:rec%n_rays)%raypath_id  ! the index in the source path list of the rays

! verify that path references are valid

   do i=1,rec%n_rays 
      if (rec%ray(i)%raypath_id > rec%ray(i)%source%n_paths) then
         print *
         print *,'***** ERROR: INCONSISTENT INPUT *******************'
         print *,'the list of rays to receiver ',rec%id,' contains a path that is not defined'
         print *,'the number of the offending ray is ',i
         print *,'it refers to path',rec%ray(i)%raypath_id,' from source ',rec%ray(i)%source%id
         stop
      endif
   end do

end do

close(2)


! test for consistency between receiver positions and requested paths

do n=1,n_receivers

   rec => receiver(n)

   do m=1,rec%n_rays

      s => rec%ray(m)%source
      if (s%is_teleseismic) cycle
      path_id = rec%ray(m)%raypath_id
      nsec=s%path(path_id)%n_tf

      if (nsec > 1) then

         i1 = s%path(path_id)%sequence(2*nsec)
         i2 = s%path(path_id)%sequence(2*nsec-1)
         top_id = min(i1,i2)
         bot_id = max(i1,i2)
         if ( (rec%r > interpolate_interface(rec%lat,rec%long,intrface(top_id))+pgrid%tolerance) .or. &
              (rec%r < interpolate_interface(rec%lat,rec%long,intrface(bot_id))-pgrid%tolerance)) then
            print *,'receiver',n,' does not lie in final time field of path',m
            stop
         endif

      else

         if (s%on_interface) then

            top_id=max(1,s%topint_id-1)
            bot_id=min(n_interfaces,s%botint_id+1)
            if ( (rec%r > interpolate_interface(rec%lat,rec%long,intrface(top_id))+pgrid%tolerance) .or. &
                 (rec%r < interpolate_interface(rec%lat,rec%long,intrface(bot_id))-pgrid%tolerance)) then
               print *,'receiver',n,' does not lie in final time field of path',m
               stop
            endif

         else

            top_id=s%region_id
            bot_id=s%region_id+1
            if ( (rec%r > interpolate_interface(rec%lat,rec%long,intrface(top_id))+pgrid%tolerance) .or. &
                 (rec%r < interpolate_interface(rec%lat,rec%long,intrface(bot_id))-pgrid%tolerance)) then
               print *,'receiver',n,' does not lie in final time field of path',m
               stop
            endif

         endif

      endif

   end do

end do

print *,'finished reading receivers'



! test which paths are actually used, and pre-count the number of pp-phases arriving at this receiver

allocate(npp_receiver(n_receivers),ipp_receiver(n_receivers))

npp_receiver = 0    ! will contain the number of pp phases arriving at this receiver
ipp_receiver = 1    ! a counter for pp-phases

do n=1,n_receivers
   do m=1,receiver(n)%n_rays

      ss => receiver(n)%ray(m)%source
      path_id = receiver(n)%ray(m)%raypath_id

      ss%path(path_id)%used=.true.

      if (ss%path(path_id)%refstep /= 0) then  ! we have a pp type phase
         npp_receiver(n)=npp_receiver(n)+1
      endif

   end do
end do


! add receivers of pp-type phases to the source list, and construct their
! path sequences by inverting the tail end of the original pp-type sequence  

do n=1,n_receivers

   if (npp_receiver(n) > 0) allocate(receiver(n)%path_equivalent(receiver(n)%n_rays))

   do m=1,receiver(n)%n_rays

      ss => receiver(n)%ray(m)%source
      path_id = receiver(n)%ray(m)%raypath_id


      if (ss%path(path_id)%refstep /= 0) then  ! we have a pp type phase


      ! if the receiver does not yet have a source equivalent,create one and point to it

         if (receiver(n)%source_equivalent == 0) then

            n_sources_ppinc = n_sources_ppinc + 1
            receiver(n)%source_equivalent = n_sources_ppinc
            s => source(receiver(n)%source_equivalent)


            s%r=receiver(n)%r
            s%lat=receiver(n)%lat
            s%long=receiver(n)%long
            s%coslat=cos(s%lat)

            s%is_local = .true.

            call initialize_source(s,pgrid)

            if (.not. s%on_interface) print *,'virtual receiver source', &
                 n_sources_ppinc,' is located in region ',s%region_id

            if (s%on_interface) print *,'virtual receiver source', &
                 n_sources_ppinc,' is located on interfaces ',&
                 s%topint_id,s%botint_id

            receiver(n)%path_equivalent(m)= 1


         ! allocate storage for path info

            s%n_paths = npp_receiver(n)
            allocate(s%path(s%n_paths))
            do i=1,s%n_paths ; call path_defaults(s%path(i)) ; end do

         else   ! point to the already existing source equivalent

            s => source(receiver(n)%source_equivalent) 
            receiver(n)%path_equivalent(m) = ipp_receiver(n)

         endif


       ! set some attributes of the path

         recpath_id=ipp_receiver(n)

         s%path(recpath_id)%n_tf = ss%path(path_id)%n_tf - ss%path(path_id)%refstep 
         s%path(recpath_id)%id   = ipp_receiver(n)
         s%path(recpath_id)%valid   = .true.
         s%path(recpath_id)%used    = .true.
         s%path(recpath_id)%refstep = s%path(recpath_id)%n_tf + 1
         s%path(recpath_id)%fitting_interface = ss%path(path_id)%fitting_interface

         allocate(s%path(recpath_id)%sequence(2*s%path(recpath_id)%n_tf))
         allocate(s%path(recpath_id)%tf_sequence(s%path(recpath_id)%n_tf))
         allocate(s%path(recpath_id)%vtype_sequence(s%path(recpath_id)%n_tf))


       ! construct the path for the receiver equivalent source by inverting the orginal source path

       ! go step by step

         do i=1,2*s%path(recpath_id)%n_tf,2

            s%path(recpath_id)%sequence(i)   =  ss%path(path_id)%sequence(2*ss%path(path_id)%n_tf-i+1 )
            s%path(recpath_id)%sequence(i+1) =  ss%path(path_id)%sequence(2*ss%path(path_id)%n_tf-i )
            j=(i+1)/2
            s%path(recpath_id)%vtype_sequence(j) =   &
                 ss%path(path_id)%vtype_sequence(ss%path(path_id)%n_tf-j+1 )

          ! if it is a turning step exchange the interfaces in this step 

            if ( i > 1 ) then

               if ( s%path(recpath_id)%sequence(i+1) == s%path(recpath_id)%sequence(i-1)) then
                  j= s%path(recpath_id)%sequence(i+1)
                  s%path(recpath_id)%sequence(i+1) = s%path(recpath_id)%sequence(i)
                  s%path(recpath_id)%sequence(i) = j

               endif

            endif

         end do 

         s%path(recpath_id)%sequence(1)=0

         ipp_receiver(n)=ipp_receiver(n)+1

      endif

   end do

end do

deallocate(npp_receiver,ipp_receiver)


! now remove the unwanted sections from the original pp-type paths from the original sources

do n=1,n_sources
   s=> source(n)
   do m=1,s%n_paths
      if (s%path(m)%refstep /= 0) s%path(m)%n_tf=s%path(m)%refstep - 1
   end do
end do

! finished dealing with late reflections


! allocate the required timefields for all sources

do n=1,n_sources_ppinc

   i=n_vtypes*n_regions        ! max time fields from initial propagation through 
                               ! regions overlapping source grid
   do m=1,source(n)%n_paths
      i=i+source(n)%path(m)%n_tf
   end do
   allocate(source(n)%time_field(i))
   do j=1,i ; call time_field_defaults(source(n)%time_field(j)); source(n)%time_field(j)%id = j ; end do

end do

! if it is requested that some grids of arrival times be saved,
! read the information about which paths are to be svaed from the
! file gridsave.in

if (save_timefields_mode) then

   open(19,file='gridsave.in',iostat=io)
   if (io /= 0) then
      print *, 'save time fields mode specified, but there is a problem with the input file'
      print *, 'check that the file gridsave.in is present and not empty'
      stop 'the file gridsave.in does not appear to be present'
   endif

   save_grid_counter=0

   do
      read(19,*,iostat=io) n,nsave
      if (io<0) exit
      if (nsave>0) then
         allocate(iarr(nsave),jarr(nsave))
         read(19,*) iarr(1:nsave)    
         do m=1,nsave
            source(n)%path(iarr(m))%gridsave=.true.
            save_grid_counter=save_grid_counter+1
         end do
         read(19,*) jarr(1:nsave)    
         do m=1,nsave
            source(n)%path(iarr(m))%first_tf_to_save=jarr(m)
         end do
         deallocate(iarr,jarr)
      endif
   end do

   close(19)

   open(19,file='arrtimes.dat')
   write(19,*) pgrid%nr,pgrid%nlat,pgrid%nlong
   write(19,*) pgrid%dr0,pgrid%dlat0/deg_to_rad,pgrid%dlong0/deg_to_rad
   write(19,*) pgrid%r0,pgrid%lat0/deg_to_rad,pgrid%long0/deg_to_rad
   write(19,*) save_grid_counter
       
endif


! check if info for all sources is correct

! print *,'n_sources_ppinc',n_sources_ppinc

!do n=1,n_sources_ppinc
!   do m=1,source(n)%n_paths
!
!      print *
!      print *,'source',n,'  path', m, 'id', source(n)%path(m)%id
!      print *,'ntfinit',source(n)%n_tf_init
!      print *,'ntf =',source(n)%path(m)%n_tf,'refstep=',source(n)%path(m)%refstep
!      print '(a10,20i5)','path',source(n)%path(m)%sequence(1:2*source(n)%path(m)%n_tf)
!      print *,'fitting interface',source(n)%path(m)%fitting_interface
!
!   end do
! end do

! stop 'pptest stop'

! finished initializing!!!!!!!!!!!!!!!!!!!!
print *,'finsihed initializing sources and receivers'
print *

call system_clock(t2,count_rate,count_max)


if (display_mode) then

! create OpenDx input files showing the geometry of the problem

   do i=1,n_interfaces
      call display_interface(intersection(i))
   end do

   call display_sources
   call display_receivers

endif

call system_clock(t3,count_rate,count_max)
!--------------------------------------------------
! now calculate the required time fields for each source

global_source_counter = 0

! in no_pp_mode is true, the ray tracing will be done immediately after the
! fast marching for each source. It is called no_pp_mode because in this mode
! it is not possible to do late reflection, since this requires the time 
! fields from different sources at the same time

if (no_pp_mode) then

   call initialize_inversion(do_frechet_derivatives)

   open(11,file='arrivals.dat')
   open(21,file='frechet.dat')
   if (save_rays_mode) open(31,file='rays.dat')
   if (display_mode) open(41,file='raypos')
   if (display_mode) open(42,file='raytype')
   raypoint_counter=0

endif



do ns=1,n_sources_ppinc

   s => source(ns)

   if (parallel_mode) then
      if (mod(s%id-1,nproc)/=myid) cycle
   endif

! first evaluate the initial time fields of the sequences starting at the source

   if (s%is_local) then

      print *,'starting to initialize local source',s%id

! do the first sweep from the source through the regions overlapping source grid
! this is a big subroutine that does the entire grid refinement procedure around the source
! the call to this subroutine takes a significant part of the computation time

      call initialize_source_regions(s)

   endif


   if (s%is_teleseismic) then

!      print *,'starting to initialize teleseismic source',s%id,s%teleseismic_id
         
     call initialize_teleseismic_source(s)

   endif

   print *,'# time fields from initialization',s%n_time_fields


! we now have the first time fields for every possible sequence


! start the sequence of sweeps required for the paths

pathloop: do n=1,s%n_paths

   vtype = s%path(n)%vtype_sequence(1)     ! get the velocity type (P or S)
   print *

 ! identify the first time field of the path, created during initialisation above

   if (s%is_local) then

      if (s%on_interface) then

         if (s%path(n)%sequence(2) == s%topint_id - 1) prev_tf = s%first_tf_up(vtype)
         if (s%path(n)%sequence(2) == s%botint_id + 1) prev_tf = s%first_tf_down(vtype)

      else

         prev_tf = s%first_tf_up(vtype)

      endif

   endif

   if (s%is_teleseismic) prev_tf = s%first_tf_up(vtype)


   s%path(n)%tf_sequence(1) = prev_tf
   print *
   print '(a5,i4,a35,i4)', &
        'path',s%path(n)%id,' leg  1  using source time field',prev_tf  



! now go through the specified sequence of the path

   do m=3,2*s%path(n)%n_tf,2

      step=(m+1)/2
      vtype = s%path(n)%vtype_sequence(step)

!      print *
!      print *,'path=',n,'step=',step,'prev_tf=',prev_tf,'s%nt=',s%n_time_fields
!      print *,'children of prev_tf',s%time_field(prev_tf)%next_tf(1:4)

      istart => intersection(s%path(n)%sequence(m))      ! intersection at which the sweep starts
      inext  => intersection(s%path(n)%sequence(m+1))    ! intersection at which the sweep ends
      if (istart%id > inext%id) then
         reg    => istart%regabo
      else
         reg    => istart%regbel
      endif

      print '(a5,i4,a5,i4,a16,i4,a5,i4,a16,i4)', 'path',s%path(n)%id, &
           ' leg',step,' from interface',istart%id,'to',inext%id,'through region',reg%id


      ! set the child type of the next time field (up,down,turning,non-turning)

      if (step > 2) then   ! if the previous time field is not a source time field

         if ( istart%id == s%time_field(prev_tf)%inonstart%id ) then

            ! if the next required timefield is not derived from a turning ray
         
            if (istart%id > inext%id) ctype=1+(vtype-1)*4              ! propagate upwards
            if (istart%id < inext%id) ctype=2+(vtype-1)*4              ! propagate downwards

         else
         
            ! if the next required timefield is derived from a turning ray
       
            ! check if turning rays are indeed present
            if (.not.s%time_field(prev_tf)%turning_rays_present) then
               print '(a5,i4,a5,i4,a44)', &
                    'path',n,' leg',step,'no turning rays present, non-existing path'
               s%path(n)%valid=.false.
               print *,'step=',step,s%path(n)%sequence(1),prev_tf,s%time_field(prev_tf)%inonstart%id
               cycle pathloop
            endif

            if (istart%id > inext%id) ctype=3+(vtype-1)*4              ! propagate upwards
            if (istart%id < inext%id) ctype=4+(vtype-1)*4              ! propagate downwards

         endif

      else   ! only if the previous step is a source time field

         print *,'previous step is a source time field'

         
          if (istart%id == s%time_field(prev_tf)%reg%itop%id ) then

            if (istart%id > inext%id) ctype=1+(vtype-1)*4              ! propagate upwards
            if (istart%id < inext%id) ctype=2+(vtype-1)*4              ! propagate downwards

         else

            if (istart%id > inext%id) ctype=3+(vtype-1)*4              ! propagate upwards
            if (istart%id < inext%id) ctype=4+(vtype-1)*4              ! propagate downwards
               
         endif

      endif

!      print *,'ctype =',ctype,vtype

      if (s%time_field(prev_tf)%next_tf(ctype) == 0) then  ! if the required timefield does not exist

!         print *,'the requested field does not exist'

         ! transfer the starting times to the starting intersection
         ! here we use the fact that regional (and thus timefield) nodes are always in the order
         ! regular grid nodes, top intersection nodes, bottom intersection nodes

         if (istart%id == s%time_field(prev_tf)%reg%ibot%id) then
            i=s%time_field(prev_tf)%reg%nnode-istart%nnode+1
            j=s%time_field(prev_tf)%reg%nnode
            istart%arrivaltime=s%time_field(prev_tf)%arrivaltime(i:j)
            istart%time_gradient(1,:)=s%time_field(prev_tf)%time_gradient(1,i:j)
            istart%time_gradient(2,:)=s%time_field(prev_tf)%time_gradient(2,i:j)
            istart%time_gradient(3,:)=s%time_field(prev_tf)%time_gradient(3,i:j)
         else
            i=s%time_field(prev_tf)%reg%ngnode+1
            j=s%time_field(prev_tf)%reg%ngnode+istart%nnode
            istart%arrivaltime=s%time_field(prev_tf)%arrivaltime(i:j)
            istart%time_gradient(1,:)=s%time_field(prev_tf)%time_gradient(1,i:j)
            istart%time_gradient(2,:)=s%time_field(prev_tf)%time_gradient(2,i:j)
            istart%time_gradient(3,:)=s%time_field(prev_tf)%time_gradient(3,i:j)
         endif



         ! modify the time gradients at the interface for the next leg of the path

         if (reg%id == s%time_field(prev_tf)%reg%id) then

            ! if we go back into the same region as the previous time field, it is a reflection
            ! convert the direction of the gradient, and set the arrival time at the 
            ! intersection points where reflection is impossible (due to wave type conversion) 
            ! to huge_time so that they do not act as a source

 !           print *,'calling reflect-gradient at interface',istart%id

            call reflect_gradient(istart,s%time_field(prev_tf),vtype)

         else

            ! if the next region is another region, it is a refraction
            ! convert the direction of the gradient, and set the arrival time at the 
            ! intersection points where total reflection occurs to huge_time 
            ! so that they do not act as a source

            direction = s%time_field(prev_tf)%reg%id - reg%id

!            print *,'calling refract_gradient with direction',direction

            call refract_gradient(istart,s%time_field(prev_tf)%reg,vtype,direction)   

            
         endif


     ! we are finally set up, now do the actual fast marching across the region 
     ! generating a new time field
     !-------------------------------------------------------------------------

         call sweep_region_from_interface(reg,istart,vtype,s)


     ! attach info to the generated time field

         ! time field preceding the current one
         s%time_field(s%n_time_fields)%prev_tf = prev_tf

         ! identify the current time field as the child of the previous time field
         s%time_field(prev_tf)%next_tf(ctype) = s%n_time_fields

         ! store the index of the time field in the sequence of timefields for this path
         s%path(n)%tf_sequence(step) = s%n_time_fields

         ! pointers to start and non-start interfaces
         s%time_field(s%n_time_fields)%istart => istart
         s%time_field(s%n_time_fields)%inonstart =>  inext  
         s%time_field(s%n_time_fields)%reg =>  reg  
         s%time_field(s%n_time_fields)%vtype = vtype

         prev_tf = s%n_time_fields
         print *,'created new time field',s%n_time_fields

      else  ! the required timefield already exists

         ! step to the next (existing) time field
         prev_tf = s%time_field(prev_tf)%next_tf(ctype)
         print *,'used existing time field',prev_tf

         ! store the index of the time field in the sequence of timefields for this path
         s%path(n)%tf_sequence(step) = prev_tf
         
      endif

   end do   ! crossings in path

end do pathloop  ! path loop


do n=1,s%n_paths
   print '(a5,i5,a12,10i5)','path',s%path(n)%id,' timefields',s%path(n)%tf_sequence(1:s%path(n)%n_tf)
end do

! save the arrival times on the main grid for this path if requested

if (save_timefields_mode) then
   do n=1,s%n_paths
       if (s%path(n)%gridsave.and.s%path(n)%valid) then
         call write_arrivaltime_grid(s,s%path(n))
      endif
   end do
endif


if (file_mode) then

! store the timefields of this source on file
   global_source_counter =  global_source_counter + 1
   nfile =  global_source_counter + 1000
   s%nfile=nfile
   open(nfile,form='unformatted')

   do n=1,s%n_time_fields
      write(nfile) s%time_field(n)%arrivaltime
      write(nfile) s%time_field(n)%time_gradient
      deallocate(s%time_field(n)%arrivaltime,s%time_field(n)%time_gradient)
   end do

   close(nfile)

endif


if (no_pp_mode) then

   print *,'starting the ray tracing for source',s%id

   do n=1,n_receivers

      do m=1,receiver(n)%n_rays

         ray => receiver(n)%ray(m)
         if (ray%source%id /= s%id) cycle 
         path_id = ray%raypath_id

         if (s%path(path_id)%valid) then ! the original path was recognised as valid 
                                         ! during the timefield calculations

            if (s%path(path_id)%refstep == 0) then    !  the standard case of no reflection fitting
               call trace_ray_from_receiver(receiver(n),s,ray)
               if (ray%valid) then   ! valid ray path found

                  print '(a12,i4,a10,i4,a15,i4,a4,f10.4,2l5)','traced ray',m,'to source',s%id,&
                       ' from receiver',n,'  t=', ray%receiver_time,ray%diffracted,ray%headwave

                  k=0
                  write(11,'(4i6,f15.6,2l5)') n,ray%source%id,m,k,ray%receiver_time, &
                       ray%diffracted,ray%headwave

                  if (display_mode) call store_ray(ray)

               else   ! ray tracing found that this ray path does not exist

                  print '(a12,i4,a10,i4,a15,i4,a13)','ray',m,'to source',ray%source_id,&
                       ' from receiver',n,' is invalid'

                  k=0
                  t_arrival=-1.0_dp               
                  write(11,'(4i6,f15.6,2l5)') n,ray%source%id,m,k,t_arrival,ray%diffracted,ray%headwave

               endif

            endif
         
            if (s%path(path_id)%refstep /= 0) then  !  if reflection fitting is required..

               stop 'trying to preform reflection fit while no_pp_mode enabled'

               call trace_reflectionfit(n,m)

            endif


   ! do the frechet derivatives for this ray if required

            if (do_frechet_derivatives) then

               if (receiver(n)%ray(m)%is_multiray) then

                  stop 'inconsistency: multiray in no_pp_mode'

               else

                  ray => receiver(n)%ray(m)
                  if (ray%valid) then
                     print *, 'getting partials for rec',n,'ray',m
                     call ray_partials(ray)
                  else
                     print *,'no valid ray path for rec',n,'ray',m
                  endif

               endif

               call write_frechet_derivatives(n,m)

            endif


         else           ! the original path was recognised as invalid during the timefield calculations

            print *,'ray',m,' to receiver ',n, &
                 ' : requested path was recognised as invalid during the timefield calculations'

            ray%valid = .false.
            k=0
            t_arrival=-1.0_dp               
            write(11,'(4i6,f15.6,2l5)') n,ray%source_id,m,k,t_arrival,ray%diffracted,ray%headwave

         endif

         if (save_rays_mode) call write_valid_rays(n,m)

         call clean_ray(n,m)

      end do    ! loop over rays/paths

   end do    !loop over receivers


endif

! deallocate source specific stuff that is not needed any more

do n=1,s%n_time_fields
   if (associated(s%time_field(n)%received_turning_ray)) &
        deallocate(s%time_field(n)%received_turning_ray)
end do

if (s%is_teleseismic) then
   do n=1,n_regions
      reg=>region(n)
      if (reg%n_init > 0) deallocate(reg%init_id,reg%init_type,reg%init_arrivaltime, &
           reg%init_time_gradient)
   end do
   reg%n_init=0
endif

if (no_pp_mode) then
   do n=1,s%n_time_fields
      deallocate(s%time_field(n)%arrivaltime,s%time_field(n)%time_gradient)
   end do
endif

if (no_pp_mode) then
   print *,'finished  fast marching and ray tracing for source',s%id   
else
   print *,'finished the fast marching for source',s%id
endif

print *,'******************************************************************************'
print *

end do ! loop over sources


if (no_pp_mode) then

   close(11)
   close(21)
   if (save_rays_mode) close(31)
   if (display_mode) close(41)
   if (display_mode) close(42)

   if (display_mode) call display_stored_rays

endif

!! finished the time fields
!******************************************************************************************************

  call system_clock(t4,count_rate,count_max)


! -------------- now do the ray tracing if not in no_pp_mode  --------------------------------------------------------------

if (n_receivers > 0 .and. (.not.no_pp_mode)) then

   call initialize_inversion(do_frechet_derivatives)

   open(11,file='arrivals.dat')
   open(21,file='frechet.dat')
   if (save_rays_mode) open(31,file='rays.dat')
   if (display_mode) open(41,file='raypos')
   if (display_mode) open(42,file='raytype')
   raypoint_counter=0

   print *,'starting the ray tracing'

   do n=1,n_receivers

      do m=1,receiver(n)%n_rays

         ray => receiver(n)%ray(m)
         s   => ray%source
         path_id = ray%raypath_id

         if (s%path(path_id)%valid) then ! the original path was recognised as valid 
                                         ! during the timefield calculations

            if (s%path(path_id)%refstep == 0) then    !  the standard case of no reflection fitting

               if (file_mode) call load_source_timefields(s)
               call trace_ray_from_receiver(receiver(n),s,ray)

               if (ray%valid) then   ! valid ray path found

                  print '(a12,i4,a10,i4,a15,i4,a4,f10.4,2l5)','traced ray',m,'to source',ray%source_id,&
                       ' from receiver',n,'  t=', ray%receiver_time,ray%diffracted,ray%headwave

                  k=0
                  write(11,'(4i6,f15.6,2l5)') n,ray%source_id,m,k,ray%receiver_time,ray%diffracted,ray%headwave

                  if (display_mode) call store_ray(ray)

               else   ! ray tracing found that this ray path does not exist

                  print '(a12,i4,a10,i4,a15,i4,a13)','ray',m,'to source',ray%source_id,&
                       ' from receiver',n,' is invalid'

                  k=0
                  t_arrival=-1.0_dp               
                  write(11,'(4i6,f15.6,2l5)') n,ray%source_id,m,k,t_arrival,ray%diffracted,ray%headwave

               endif

            endif
         
            if (s%path(path_id)%refstep /= 0) then  !  if reflection fitting is required..

               if (file_mode) call load_source_timefields(receiver(n)%ray(m)%source)
               if (file_mode) call load_source_timefields(source(receiver(n)%source_equivalent))

               call trace_reflectionfit(n,m)

            endif


   ! do the frechet derivatives for this ray if required

            if (do_frechet_derivatives) then

               if (receiver(n)%ray(m)%is_multiray) then

                  do k=1,receiver(n)%ray(m)%n_subrays
                     ray => receiver(n)%ray(m)%subray(k)
                     if (ray%valid) then
                        print *, 'getting partials for rec',n,'ray',m,'subray',k
                        call ray_partials(ray)
                     else
                        print *, 'no valid ray path for rec',n,'ray',m,'subray',k
                     endif
                  end do

               else

                  ray => receiver(n)%ray(m)
                  if (ray%valid) then
                     print *, 'getting partials for rec',n,'ray',m
                     call ray_partials(ray)
                  else
                     print *,'no valid ray path for rec',n,'ray',m
                  endif

               endif

               call write_frechet_derivatives(n,m)

            endif


            if (file_mode) call clean_source_timefields(receiver(n)%ray(m)%source)
            if (file_mode .and. s%path(path_id)%refstep /= 0)  &
                 call clean_source_timefields(source(receiver(n)%source_equivalent))


         else           ! the original path was recognised as invalid during the timefield calculations

            print *,'ray',m,' to receiver ',n, &
                 ' : requested path was recognised as invalid during the timefield calculations'

            ray%valid = .false.
            k=0
            t_arrival=-1.0_dp               
            write(11,'(4i6,f15.6,2l5)') n,ray%source_id,m,k,t_arrival,ray%diffracted,ray%headwave

         endif

         if (save_rays_mode) call write_valid_rays(n,m)

         call clean_ray(n,m)

      end do    ! loop over rays/paths

   end do    !loop over receivers

   close(11)
   close(21)
   if (save_rays_mode) close(31)
   if (display_mode) close(41)
   if (display_mode) close(42)

   call system_clock(t5,count_rate,count_max)

   print *,'init time       :',dble(t2-t1)/dble(count_rate),' sec'

   print *,'propagation time:',dble(t4-t3)/dble(count_rate),' sec'

   print *,'ray tracing time:',dble(t5-t4)/dble(count_rate),' sec'

   if (display_mode) call display_stored_rays


endif  ! n_receivers > 0 and not in no_pp_mode


!-----------------------------------------------------------------------------------------------------------

end program fm3d

