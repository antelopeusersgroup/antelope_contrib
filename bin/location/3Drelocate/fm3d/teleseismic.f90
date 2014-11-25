!*****************************************************************************************************
!***********************************************************************************************
! This subroutine takes a teleseismic source as input,  initializes the bottom
! intersection of the grid and the sides facing the source, and does a sweep through all regions 
! in the upward direction to the surface.

subroutine initialize_teleseismic_source(s)
use mod_3dfm
implicit none

type(Tsource)       :: s  ! the source with its properties on the main grid

type(Tintersection),pointer       :: itopc,ibotc ! top and bottom intersections of main grid 
type(Tregion),pointer             :: reg          ! the source region in the main grid
logical                           :: do_northside,do_southside,do_westside,do_eastside
integer,dimension(:),allocatable  :: init_type

! local stuff
integer :: n,m,i,j,i1,i2,i3,vtype,n_init,n_tot_ref
real(kind=4)  :: latsw,latnw,latne,latse
real(kind=4)  :: lonsw,lonnw,lonne,lonse
real(kind=4)  :: slat,slon,deltas,cazim,bazim,azima
real(kind=8)  :: deg_to_rad 

deg_to_rad = acos(-1.0_dp)/180.0_dp

print *,'initializing teleseismic source',s%id,s%teleseismic_id

! allocate the arrays that will contain the indices of the source time fields in the time field list
allocate(s%first_tf_up(n_vtypes),s%first_tf_down(n_vtypes))


! determine which sides of the grid face the source

do_northside = .false. ; do_southside = .false.
do_westside = .false.  ; do_eastside = .false.

! corners of the gird on the surface and source coordinates in real*4

latsw=(pgrid%lat0)/deg_to_rad
latnw=(pgrid%lat0 + (pgrid%nlat-1)*pgrid%dlat0)/deg_to_rad
latne=(pgrid%lat0 + (pgrid%nlat-1)*pgrid%dlat0)/deg_to_rad
latse=(pgrid%lat0)/deg_to_rad

lonsw=(pgrid%long0)/deg_to_rad
lonnw=(pgrid%long0)/deg_to_rad
lonne=(pgrid%long0 + (pgrid%nlong-1)*pgrid%dlong0)/deg_to_rad
lonse=(pgrid%long0 + (pgrid%nlong-1)*pgrid%dlong0)/deg_to_rad

slat=(s%lat)/deg_to_rad
slon=(s%long)/deg_to_rad

call ydist(slat,slon,latsw,lonsw,deltas,cazim,bazim,azima)
do_westside = (sin(cazim*deg_to_rad)> 0.01)  
call ydist(slat,slon,latnw,lonnw,deltas,cazim,bazim,azima)
do_westside = (sin(cazim*deg_to_rad)> 0.01).and.do_westside

call ydist(slat,slon,latnw,lonnw,deltas,cazim,bazim,azima)
do_northside = (cos(cazim*deg_to_rad) < -0.01)  
call ydist(slat,slon,latne,lonne,deltas,cazim,bazim,azima)
do_northside = (cos(cazim*deg_to_rad) < -0.01).and.do_northside

call ydist(slat,slon,latne,lonne,deltas,cazim,bazim,azima)
do_eastside = (sin(cazim*deg_to_rad) < -0.01)  
call ydist(slat,slon,latse,lonse,deltas,cazim,bazim,azima)
do_eastside = (sin(cazim*deg_to_rad) < -0.01).and.do_eastside

call ydist(slat,slon,latse,lonse,deltas,cazim,bazim,azima)
do_southside = (cos(cazim*deg_to_rad) > 0.01)  
call ydist(slat,slon,latsw,lonsw,deltas,cazim,bazim,azima)
do_southside = (cos(cazim*deg_to_rad) > 0.01).and.do_southside

 print *,'exposed sides: N',do_northside,' S ',do_southside,' W ', &
      do_westside,' E ',do_eastside


! make a list of nodes to be initialized, region by region

do m=n_regions,1,-1

   reg => region(m)
   allocate(init_type(reg%nnode))
   init_type = 0
   n_init = 0

!   print *,'region',reg%id,reg%nnode

   do n=1,reg%nnode

      if (reg%node(n)%i1==0 .and. reg%node(n)%i2==n_intersections) then
         n_init=n_init+1
         init_type(n)=1
         cycle
      endif
         
      if (do_northside .and. reg%r(n)*abs(reg%lat(n)-pgrid%latmax)<pgrid%tolerance) then
         n_init=n_init+1
         init_type(n)=2
         cycle
      endif

      if (do_southside .and. reg%r(n)*abs(reg%lat(n)-pgrid%lat0)<pgrid%tolerance) then
         n_init=n_init+1
         init_type(n)=3
         cycle
      endif

      if (do_eastside .and. reg%r(n)*abs(reg%long(n)-pgrid%longmax)<pgrid%tolerance) then
         n_init=n_init+1
         init_type(n)=4
         cycle
      endif

      if (do_westside .and. reg%r(n)*abs(reg%long(n)-pgrid%long0)<pgrid%tolerance) then
         n_init=n_init+1
         init_type(n)=5
         cycle
      endif

   end do

   reg%n_init = n_init

!   print *,n_init,'nodes in list'
!   print *,count(init_type == 1),count(init_type == 2),count(init_type == 3),count(init_type == 4),count(init_type == 5)


   if (n_init > 0) then
      allocate(reg%init_id(reg%n_init),reg%init_type(reg%n_init),reg%init_arrivaltime(reg%n_init), &
           reg%init_time_gradient(3,reg%n_init))
      i=0
      do n=1,reg%nnode
         if (init_type(n) /= 0) then
            i=i+1
            reg%init_id(i)=n
            reg%init_type(i)=init_type(n)
         endif
      end do
   endif

   deallocate(init_type)

end do  ! loop making init node list for each region

print *,'init nodes identified'

! for all regions, get the starting times and time gradients on the bottom interface
! and relevant sides from the ttimes program

do m=1,n_regions
   if (region(m)%n_init>0) call teleseismic_initialization(region(m),s)
end do

print *

! all boundary nodes hit by the incoming front are now identified and have arrival times
! incoming time gradients  if a solution for that node exists

!----------------------------------------------------------------------------------------

! a teleseismic source is intialized by doing a path sequence going from bottom to top
! for 1 or 2 velocity types as required

s%n_tf_init = n_regions

! sweep through all regions towards the surface

vloop: do vtype=1,n_vtypes

   do n=1,s%n_tf_init

      reg => region(n_regions - n + 1)

      itopc => intersection(reg%id)      ! the top intersection of the region
      ibotc => intersection(reg%id+1)    ! the bottom intersection of the region
      allocate(reg%arrivaltime(reg%nnode),reg%time_gradient(3,reg%nnode),reg%node_status(reg%nnode))
      reg%node_status=-1
      reg%arrivaltime = huge_time
      reg%time_gradient=0.0_dp

      if (reg%id < n_regions) then  ! if the region is not the bottom region

   ! create a narrow band on the starting intersection from previous result

         call refract_gradient(ibotc,region(reg%id+1),vtype,1) 

         i=reg%nnode-ibotc%nnode+1
         j=reg%nnode
         reg%arrivaltime(i:j)=ibotc%arrivaltime
         reg%time_gradient(1:3,i:j)=ibotc%time_gradient(1:3,1:ibotc%nnode)
         reg%node_status(i:j) = 1


      endif


   ! create a narrow band from the incoming teleseismic wave
   ! ( copy arrival times from init, refract gradients, set node status)

      if (reg%n_init>0) call refract_teleseismic_front(reg,vtype,n_tot_ref)

      if (n==1 .and. n_tot_ref == reg%n_init) then

         print *,'teleseismic front incompatible with start from  bottom region'
         print *,'all boundary nodes experienced total reflection or inside hits'
         print *,'for vtype =',vtype,' No paths starting with this type are possible'
         print *
         cycle vloop

      endif


! do the fast marching sweep across the main grid region containing the source

      call propagate(reg,vtype)

      print *,'propagation through region',reg%id,' finished'

! transfer regional travel times to interfaces 

      if (itopc%nnode > 0 .and. .not. associated(itopc%arrivaltime)) then
         allocate(itopc%arrivaltime(itopc%nnode))
         allocate(itopc%time_gradient(3,itopc%nnode))
      endif
      if (ibotc%nnode > 0 .and. .not. associated(ibotc%arrivaltime)) then
         allocate(ibotc%arrivaltime(ibotc%nnode))
         allocate(ibotc%time_gradient(3,ibotc%nnode))
      endif

      do i=1,reg%nnode

         i1 = reg%node(i)%i1 ; i2 = reg%node(i)%i2 ; i3 = reg%node(i)%i3

         if (i1 == 0) then

            intersection(i2)%arrivaltime(i3) = reg%arrivaltime(i)
            intersection(i2)%time_gradient(1:3,i3) = reg%time_gradient(1:3,i)

         endif

      end do
      
!   print *,'min time on top isec',minval(itopc%arrivaltime)

  ! transfer time field to the array of saved time fields

      s%n_time_fields=s%n_time_fields+1
      allocate(s%time_field(s%n_time_fields)%arrivaltime(reg%nnode))
      s%time_field(s%n_time_fields)%arrivaltime=reg%arrivaltime
      allocate(s%time_field(s%n_time_fields)%time_gradient(3,reg%nnode))
      s%time_field(s%n_time_fields)%time_gradient=reg%time_gradient

      print *,'results written to timefield',s%n_time_fields

  
! save pointer to region,start and non-start interfaces and tree structure pointers

      s%time_field(s%n_time_fields)%reg =>  reg 

      s%time_field(s%n_time_fields)%istart => ibotc
      s%time_field(s%n_time_fields)%inonstart =>  itopc
      s%time_field(s%n_time_fields)%vtype = vtype

      if (n==1) s%first_tf_up(vtype)=s%n_time_fields

      if (n>1) then
         s%time_field(s%n_time_fields)%prev_tf = n-1 + (vtype-1)*s%n_tf_init
         s%time_field(s%n_time_fields-1)%next_tf(1+(vtype-1)*4)= s%n_time_fields
         print *,'field is ctype',1+(vtype-1)*4,'of timefield',s%n_time_fields-1
      endif

! deallocate  everything that is no longer required

!      print *,'timefield attributes saved'

      deallocate(reg%arrivaltime,reg%time_gradient,reg%node_status)

   end do

end do vloop ! vtypes


print *,'initial timefields for teleseismic source established'


! stop ' temp stop in init teleseismic source'

return

end subroutine initialize_teleseismic_source




!*****************************************************************************************************
!
subroutine  teleseismic_initialization(reg,s)

  use mod_3dfm
  implicit none
!
!     Determines ak135 traveltimes for a specified phase to
!     the boundary nodes of a region . It first solves iteratively for the   
!     angular separation at which the teleseismic ray and the ray from the
!     boundary node hit the surface with the same angle (ray parameter)
!     The time at the node is then the difference between the travel times 
!     of the two phases to this surface location 
!
!     Marthijn de Kool
!     Australian National University
!
!    
!     NOTE: Makes use of subroutines from the freeware
!           program ttimes, and uses some code from Nick Rawlinson's aktsurf.
!
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

  type(Tregion) :: reg
  type(Tsource)       :: s

  integer             :: i,j,k,n,m,phid,iter,n_suspect_init,vtype
  integer,parameter   :: maxnp=60
  real                :: tt(maxnp),dtdd(maxnp),dtdh(maxnp),dddp(maxnp)
  real                :: evlat,evlon,evdep,ndlat,ndlon,rslat,usrc(2),nddep
  real                :: deltas1,deltas2,deltas3,delta
  real                :: deltan1,deltan2,deltan3,gt_norm,f1,f2,f3,delta_min,f1_old,f2_old
  real                :: stt1,stt2,stt3,sdtdd1,sdtdh1,sdtdd2,sdtdh2,sdtdd3,sdtdh3
  real                :: ntt1,ntt2,ntt3,ndtdd1,ndtdh1,ndtdd2,ndtdh2,ndtdd3,ndtdh3
  real                :: dt1,dt2,dt3
  real                :: deltas,cazim,bazim,edist,bazr,etcor,azima
  real                :: delta_pos,delta_neg,time_error,accuracy
  real(kind=dp)       :: deg_to_rad,deg_per_km,surf_vel,det

  real,parameter      :: pi=3.1415926535
  character(len=8)    :: phcd(maxnp),phlst(maxnp),tele_phase,loc_phase,s_id,n_id
  character(len=25)   :: modnam
  logical             :: prnt(2),s1_invalid,s2_invalid

!c     tt = traveltime of phase
!c     dtdd, dtdh,dddp = partial derivatives
!c     phcd = Phase id
!c     phlst = Phase list
!c     prnt = debugging print flag
!c     modnam = Name of velocity model
!c     evlat = event latitude
!c     evlon = event longitude
!c     evdep = event depth
!c     ndlat = node latitude
!c     ndlon = node longitude
!c     rslat = station co-latitude
!c     deltas = event-station angular distance
!c     cazim,bazim,azima = azimuth information
!c     m = number of phases found
!c     edist = event-station distance
!c     bazr = adjusted bazim
!c     etcor = elliptical correction for travel time
!c     phid = Phase id of specific phase
!c     tph = traveltime of specific phase
!c     maxnp = maximum number of ak135 phases
!c     tele_phase = specific phase name for given event
!c     pi = pi
!c     er = Earth radius
!c     kmpd = Number of km per great circle degree

 
! some validation of the input

  if (.not.s%is_teleseismic) stop 'teleseismic_initialization called with local source'

  if ( count(reg%r<(earth_radius-800.0_dp)) > 0 )  then
     print *, ' !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
     print *, 'sorry, teleseismic initialization not possible if nodes on the'
     print *,' bottom interface have a depth > 800 km'
     stop 'bottom interface node(s) with depth > 800 km in tele_init'
  endif

  deg_to_rad = acos(-1.0_dp)/180.0_dp
  deg_per_km = 1.d0/(earth_radius*deg_to_rad)
  gt_norm = 0.20 / deg_per_km    ! establish order of magnitude of d(Tarrival)/d(Delta) at the surface
                                   ! assuming velocity is 5 km/sec

!      Specify model type

  modnam='ak135'

!     Open travel time tables

  prnt(1)=.false.
  prnt(2)=.false.
  phlst(2)='  '
  call tabin(10,modnam)

!     get source location and phase type

  evlat = s%lat/deg_to_rad
  evlon = s%long/deg_to_rad
  evdep = earth_radius - s%r
  tele_phase = s%teleseismic_phase


! determine whether the last leg of the teleseismic phase is P or S

  i = index(tele_phase,'P',back=.true.)
  j = index(tele_phase,'S',back=.true.)
  if (i>j) then
     loc_phase = 'P'
     vtype=1
     surf_vel=5.8_dp
  else
     loc_phase = 'S'
     vtype=n_vtypes
     surf_vel=3.46_dp
  endif


!  Now loop through all the boundary nodes

  n_suspect_init = 0
  print *,'starting initialization of',reg%n_init,' boundary nodes of region',reg%id

  nodeloop: do m=1,reg%n_init

     n=reg%init_id(m)

 ! convert position of the node to input taken by ttimes
     
     ndlat = reg%lat(n)/deg_to_rad
     ndlon = reg%long(n)/deg_to_rad
     nddep = earth_radius - reg%r(n)
     if (nddep < 0.0) then
        reg%init_arrivaltime(m)=huge_time
        reg%init_time_gradient(1:3,m) = 0.d0
        cycle nodeloop
     endif


!  This is a bit of a hack fix for the case when event and grid node longitude are equal. 
!  For some reason the ellipticity corrections are wrong for this case

     if(ndlon.eq.evlon)then
        ndlon=ndlon+pgrid%dlong0/(50.0*deg_to_rad)
     endif


! get angular distance between source and node
! note that this is the SPHEROIDAL distance, not the SPHERICAL
 
     call ydist(evlat,evlon,ndlat,ndlon,deltas,cazim,bazim,azima)

! if the node lies on the surface, it is a special case. No iteration necessary

     if (abs(nddep) < pgrid%tolerance) then

        call phase_time(deltas,evdep,tele_phase,stt1,sdtdd1,sdtdh1)

        if (phid == 0) then  ! if phase does not exist, try next node
           reg%init_arrivaltime(m)=huge_time
           reg%init_time_gradient(1:3,m) = 0.d0
           cycle nodeloop
        endif

        phlst(1)=tele_phase
        call brnset(1,phlst,prnt)
        call depset(evdep,usrc)
        rslat = (90.-evlat)*0.017453292
        call ellref(rslat)
        edist=deltas*0.017453292
        bazr=bazim*0.017453292
        call ellcor(edist, bazr, evdep, tele_phase, etcor)
        stt1=stt1+etcor

        reg%init_arrivaltime(m) = dble(stt1)

        reg%init_time_gradient(2,m) = sdtdd1*deg_per_km*cos(cazim*deg_to_rad)
        reg%init_time_gradient(3,m) = sdtdd1*deg_per_km*sin(cazim*deg_to_rad)
        det = 1.0_dp/surf_vel**2 - (reg%init_time_gradient(3,m)**2 + reg%init_time_gradient(2,m)**2)
        if (det >= 0.0_dp) then
           reg%init_time_gradient(1,m) = sqrt(det)
        else
           stop 'det < 0 at surface node during teleseismic initialization'
        endif

        cycle nodeloop

     endif


 ! for all nodes not on the surface:
 ! we will use the secant method to find the angular distance of the point on the surface
 ! where the ray parameters (dtdd) of the ray from the boundary node and
 ! the teleseismic source are equal.

 ! if we can find two points bracketing the solution (function positive and negative)
 ! a solution is sure to exist, if these can not be found we assume the phase does not exist at the node

! the travel time and arrival time gradient from the source to the two initial guess points

     delta  = nddep*deg_per_km  ! the first guess at the size of the interval is the depth of the node
     deltas1 = deltas            ! point 1 lies above the node
     deltas2 = deltas + delta    
     s1_invalid = .false.
     s2_invalid = .false.

     call phase_time(deltas1,evdep,tele_phase,stt1,sdtdd1,sdtdh1)
     if (phid == 0) s1_invalid = .true.
     call phase_time(deltas2,evdep,tele_phase,stt2,sdtdd2,sdtdh2)
     if (phid == 0) s2_invalid = .true.

! if both points are invalid, a valid arrival time can not be found

     if (s1_invalid .and. s2_invalid) then
        reg%init_arrivaltime(m)=huge_time
        reg%init_time_gradient(1:3,m) = 0.d0
 !       print *,'failed because 2 invalid times'
        cycle nodeloop
     endif


 ! the travel time and arrival time gradient from the node to the two initial guess points
 ! these should always exist, so if they don't its a serious error stop
 ! note that we use a special approximation for the ray parameter if the node lies
 ! very close to the surface since for this case ttimes returns a wrong value

     deltan1 = 0.0
     deltan2=delta

     call phase_time(deltan1,nddep,loc_phase,ntt1,ndtdd1,ndtdh1)
     if (nddep <= 5.0 ) ndtdd1 = deltan1/(sqrt(deltan1**2+(nddep*deg_per_km)**2)*surf_vel*deg_per_km)
     if (phid == 0) stop 'tele_init: phase does not exist n1'

     call phase_time(deltan2,nddep,loc_phase,ntt2,ndtdd2,ndtdh2)
     if (nddep <= 5.0 ) ndtdd2 = deltan2/(sqrt(deltan2**2+(nddep*deg_per_km)**2)*surf_vel*deg_per_km)
     if (phid == 0) stop 'tele_init: phase does not exist n2'


! if only one of the two paths from the source is invalid, try if we can find better initial guesses

     if (s1_invalid .or. s2_invalid) then

    ! if more distant point p2 is valid, but not the one above the node (p1)

        if (s1_invalid) then

      ! try bringing point 1 closer to point 2
           
           deltas1 = deltas1+delta/2.0
           iter =0
           do while (iter < 6 .and. s1_invalid)

              iter = iter + 1

              s1_invalid = .false.
              call phase_time(deltas1,evdep,tele_phase,stt1,sdtdd1,sdtdh1)
              if (phid == 0) s1_invalid = .true.

         ! if the new point is valid
              if (.not.s1_invalid) then

                 f1 = sdtdd1 - ndtdd1

                 if (f1 >= 0.0) then

              ! bracketing points found, finished
                       
                    cycle

                 else
                
              ! overshot, move closer to p1

                    deltas1=deltas1-delta/2**(iter+1)
                    s1_invalid = .true.

                 endif

              else
  
              ! still not valid, move closer to p2

                 deltas1 = deltas1+delta/2**(iter+1)

              endif

           end do

        ! if iteration didn't help, write off node as invalid
           if (s1_invalid) then
              reg%init_arrivaltime(m)=huge_time
              reg%init_time_gradient(1:3,m) = 0.d0
 !             print *,'failed because node p1 invalid and search failed'
              cycle nodeloop
           endif

        endif



    ! if more distant point p2 is invalid, but not the one above the node (p1)

        if (s2_invalid) then

      ! try bringing point 2 closer to point 1
           
           deltas2 = deltas2-delta/2
           iter =0
           do while (iter < 6 .and. s2_invalid)

              iter = iter + 1

              s2_invalid = .false.
              call phase_time(deltas2,evdep,tele_phase,stt2,sdtdd2,sdtdh2)
              if (phid == 0) s2_invalid = .true.

         ! if the new point is valid
              if (.not.s2_invalid) then

                 f2 = sdtdd2 - ndtdd2

                 if (f2 <= 0.0) then

              ! bracketing points found, finished
                       
                    cycle

                 else
                
              ! overshot, move closer to p2 again

                    deltas2=deltas2+delta/2**(iter+1)
                    s2_invalid = .true.

                 endif

              else
  
              ! still not valid, move closer to p1

                 deltas2 = deltas2-delta/2**(iter+1)

              endif

           end do

        ! if iteration didn't help, write off node as invalid
           if (s2_invalid) then
              reg%init_arrivaltime(m)=huge_time
              reg%init_time_gradient(1:3,m) = 0.d0
 !             print *,'failed because node p2 invalid and search failed'
              cycle nodeloop
           endif

        endif

     endif



  ! we have two valid points

     f1 = sdtdd1 - ndtdd1
     f2 = sdtdd2 - ndtdd2

     if (f1 < 0.0) then
        
        print *,'Warning 1 in tele_init'
        print '(a5,7f12.5)','p1',deltas1,stt1,deltan1,ntt1,f1,sdtdd1,ndtdd1
        print '(a5,7f12.5)','p2',deltas2,stt2,deltan2,ntt2,f2,sdtdd2,ndtdd2
        reg%init_arrivaltime(m)=huge_time
        reg%init_time_gradient(1:3,m) = 0.d0
        cycle nodeloop
     endif

!     print *,'first two fs',f1,f2,deltan2


 ! if the root is not bracketed in the first interval, try extending it

     iter = 1
     delta_pos=deltan1
     delta_neg=deltan2

     do while (f1*f2 > 0.0)

        iter = iter + 1

        if (iter > 3) then
           reg%init_arrivaltime(m)=huge_time
           reg%init_time_gradient(1:3,m) = 0.d0
!           print *,'failed because root could not be bracketed'
           cycle nodeloop
        endif

        deltas2 = deltas2 + delta  
        deltan2 = deltan2 + delta

        call phase_time(deltas2,evdep,tele_phase,stt2,sdtdd2,sdtdh2)
        if (phid == 0) stop 'phase does not exist s2_rep'
        call phase_time(deltan2,nddep,loc_phase,ntt2,ndtdd2,ndtdh2)
        if (nddep <= 5.0 ) ndtdd2 = deltan2/(sqrt(deltan2**2+(nddep*deg_per_km)**2)*surf_vel*deg_per_km)
!        if (nddep <= 5.0 ) ndtdd2 = sin(deltan2*deg_to_rad)/(surf_vel*deg_per_km)
        if (phid == 0) stop 'phase does not exist n2_rep'

        f2 = sdtdd2 - ndtdd2

!        print *,'trying to bracket root,next f2',f2,deltan2

     end do


!  start the root finding iteration 
     deltan1=deltas1-deltas
     deltan2=deltas2-deltas
     iter = 0
     delta_min=pgrid%dlong0/(50.0*deg_to_rad)
     accuracy = pgrid%dr0/(50.0*reg%velocity(1,1))
     f3 = gt_norm ; f1_old=2.*f3 ; f2_old=2.*f3
     delta_pos=deltan1
     delta_neg=deltan2


itloop: do while (abs(f3) > 1.e-7*gt_norm  .and. abs(deltan2-deltan1) > 1.e-6*deltas) 

        iter = iter + 1

        deltan3 = deltan1 +((f1/(f1-f2))*(deltan2-deltan1))

      ! if solution deteriorates rather than improves, switch to bisection

        if ((deltan3-delta_pos)*(deltan3-delta_neg)>0.0 .or. &
             (abs(f3)>=abs(f1_old).and.abs(f3)>=abs(f2_old))) then
            deltan3=(delta_pos+delta_neg)/2.
        endif


        deltas3 = deltas + deltan3  

        call phase_time(deltas3,evdep,tele_phase,stt3,sdtdd3,sdtdh3)
        if (phid == 0) stop 'phase does not exist s3'
        s_id=phcd(phid)
        call phase_time(deltan3,nddep,loc_phase,ntt3,ndtdd3,ndtdh3)
        if (nddep <= 5.0 ) ndtdd3 = deltan3/(sqrt(deltan3**2+(nddep*deg_per_km)**2)*surf_vel*deg_per_km)
        if (phid == 0) stop 'phase does not exist n3'
        n_id=phcd(phid)

        f3 = sdtdd3 - ndtdd3

        if (abs(f3) <= 1.e-7*gt_norm ) exit itloop  ! exit if converged

        if (iter > 20) then

           dt1=stt1-ntt1 ; dt2=stt2-ntt2 ; dt3=stt3-ntt3 ;
           time_error=max(abs(dt1-dt2),abs(dt1-dt3),abs(dt2-dt3))

        ! if the solution has not fully converged but the maximum time error is acceptable
        ! still allow the solution to be used, otherwise discard and treat node as not initialized

           if (time_error < accuracy) then
              n_suspect_init = n_suspect_init+1
              exit itloop
           else
              reg%init_arrivaltime(m)=huge_time
              reg%init_time_gradient(1:3,m) = 0.d0
              cycle nodeloop
           endif

        endif

 
        f1_old=f1 ; f2_old=f2

        if (abs(f1)>abs(f2)) then
           f1 = f3 ; deltan1=deltan3 ; deltas1=deltas3 ; ntt1 = ntt3 ; stt1 = stt3 ; sdtdd1=sdtdd3 ; ndtdd1=ndtdd3
        else
           f2 = f3 ; deltan2=deltan3 ; deltas2=deltas3 ; ntt2 = ntt3 ; stt2 = stt3 ; sdtdd2=sdtdd3 ; ndtdd2=ndtdd3
        endif

        if (f1 > 0.0) then
           delta_pos = max(delta_pos,deltan1)
        else
           delta_neg = min(delta_neg,deltan1)
        endif
        if (f2 > 0.0) then
           delta_pos = max(delta_pos,deltan2)
        else
           delta_neg = min(delta_neg,deltan2)
        endif


        delta =abs(deltan2-deltan1)


     end do itloop


!    Apply elliptical corrections
!

     bazr=bazim*0.017453292

     phlst(1)=tele_phase
     call brnset(1,phlst,prnt)
     call depset(evdep,usrc)
     rslat = (90.-evlat)*0.017453292
     call ellref(rslat)
     edist=deltas3*0.017453292
     call ellcor(edist, bazr, evdep, tele_phase, etcor)
     stt3=stt3+etcor

     phlst(1)=loc_phase
     call brnset(1,phlst,prnt)
     call depset(nddep,usrc)
     rslat = (90.-ndlat)*0.017453292
     call ellref(rslat)
     edist=deltan3*0.017453292
     call ellcor(edist, bazr, nddep, loc_phase, etcor)
     ntt3=ntt3+etcor


! arrival time at the bottom interface node is the difference in arrival times 

     reg%init_arrivaltime(m) = stt3 - ntt3


 ! store the incoming (before refraction at bottom interface) time gradient in the ak135 model

     reg%init_time_gradient(1,m) = ndtdh3
     reg%init_time_gradient(2,m) = ndtdd3*deg_per_km*cos(cazim*deg_to_rad)* &
          earth_radius/reg%r(n)
     reg%init_time_gradient(3,m) = ndtdd3*deg_per_km*sin(cazim*deg_to_rad)* &
          earth_radius/reg%r(n)

!     if(reg%init_time_gradient(2,m)>0.0) then
!        print *,m,cazim,azima
!        stop 'gothcha'
!     endif

  enddo nodeloop


! print some diagnostics on how the initialization went
  
  if (n_suspect_init > 0) print *,'warning: teleseismic initialization problematic at',n_suspect_init,' nodes'
  i=count(reg%init_arrivaltime == huge_time)
  if (i>0) print *,'warning: teleseismic initialization failed at     ',i,' nodes'
  if (i==0 .and. n_suspect_init==0) print *,'teleseismic initialization successful at all nodes'

  return

  contains

    subroutine phase_time(del,dep,phase,time,raypar,tg_radial)

! this is a short intrinsic subroutine that simplifies calls to the ttimes routines
! it takes the angular separation (del), depth of the source(dep) and the phase of
! interest (phase) as input and returns the arrival time (time) and dtdd, the ray parameter
! (raypar) and tg_radial ( = dtdh, the arrival time derivative wrt source depth) as output


      real :: del,dep,time,raypar,tg_radial
      character(len=8) :: phase,n_phase,b_phase,g_phase
      integer  :: mm


! if nodes of bottom interface lie close to the surface, the local ray to the surface will be named
! Pn,Pb,Pg or Sn,Sb,Sg by the ttimes routines, check for this

      n_phase = phase ;b_phase = phase ;g_phase = phase  

      if (phase == 'P') then
         n_phase = 'Pn'
         b_phase = 'Pb'
         g_phase = 'Pg'
      endif

      if (phase == 'S') then
         n_phase = 'Sn'
         b_phase = 'Sb'
         g_phase = 'Sg'
      endif

        phlst(1)=phase
        call brnset(1,phlst,prnt)
        call depset(dep,usrc)
        call trtm(del,maxnp,mm,tt,dtdd,dtdh,dddp,phcd)


        k=1
        phid=0
        do while(k.le.mm.and.phid.eq.0)
           if( phcd(k).eq.phase .or. phcd(k).eq.n_phase &
                .or. phcd(k).eq.b_phase.or. phcd(k).eq.g_phase )then
              phid=k
           else
              k=k+1
           endif
        enddo

        if (phid > 0) then 
           time=tt(phid) 
           raypar=dtdd(phid) 
           tg_radial=dtdh(phid)
        endif

    end subroutine phase_time

end subroutine teleseismic_initialization

!*********************************
subroutine refract_teleseismic_front(reg,vtype,n_tot_ref)

! this subroutine takes the incoming teleseismic wave front normal (time gradient)
! and refracts it through the appropriate side of the grid to initialize the time
! gradient on the regional boundary. Nodes at which the incoming wave front is
! totally reflected are also removed from the initial narrow band.

  use mod_3dfm

  type(Tregion)          :: reg
  integer                :: vtype,n_tot_ref,n,m,inside_hits,late_hits
  real(kind=dp)          :: grad_perp,grad_perp_refracted,grad_par(3),det,normal(3)

  n_tot_ref = 0
  inside_hits=0
  late_hits=0

  do m=1,reg%n_init

     n=reg%init_id(m)     ! take m'th entry from list of nodes to be initialized

  ! do not use teleseismic arrival time if an internal wave arrives at the node earlier
     if (reg%init_arrivaltime(m) > reg%arrivaltime(n)) then
        late_hits=late_hits+1
        cycle
     endif

     select case (reg%init_type(m))   ! get the appropriate inward-pointing normal

        case(1)   ! bottom interface
           normal = intersection(n_intersections)%normal(1:3,reg%node(n)%i3)

        case(2)   ! north side
           normal =(/0.0_dp,-1.0_dp,0.0_dp/)

        case(3)   ! south side
           normal =(/0.0_dp,1.0_dp,0.0_dp/)

        case(4)   ! east side
           normal =(/0.0_dp,0.0_dp,-1.0_dp/)

        case(5)   ! west side
           normal =(/0.0_dp,0.0_dp,1.0_dp/)

        case default
           print *,'illegal node type in refract_teleseismic_front'

     end select

 
! decompose into parallel and perpendicular components

        grad_perp=dot_product(normal,reg%init_time_gradient(1:3,m))

     ! reject if the teleseismic wave hits the boundary from the inside
        if (grad_perp < 0.0_dp) then
           inside_hits=inside_hits+1
           write(15,'(4i5,6f12.3)') reg%init_type(m),reg%node(n)%i1, &
                reg%node(n)%i2,reg%node(n)%i3,reg%init_time_gradient(1:3,m),normal
           cycle
        endif

        grad_par=reg%init_time_gradient(1:3,m)-grad_perp*normal

 ! calculate perpendicular component on the inside of the boundary

        det=1.d0/(reg%velocity(n,vtype)**2) - sum(grad_par**2)

        if (det >= 0.0_dp) then
             
           ! the refracted ray exists

           grad_perp_refracted=sqrt(det)
           reg%time_gradient(1:3,n)=grad_par + grad_perp_refracted*normal
           reg%arrivaltime(n)=reg%init_arrivaltime(m)
           reg%node_status(n)=1

        else

           ! total reflection

           reg%arrivaltime(n)=huge_time
           reg%time_gradient(1:3,n) = 0.0_dp
           n_tot_ref = n_tot_ref + 1
           reg%node_status(n)=-1

        endif

  end do


  if (late_hits>0) print *,'region',reg%id,' :',late_hits,' nodes reached from inside before teleseismic'
  if (inside_hits>0) then
     print *,'region',reg%id,' :',inside_hits,' nodes hit by outgoing teleseismic wave '
     stop
  endif
  if (n_tot_ref > 0) then
     print *,'total reflection of the teleseismic wavefront in region',reg%id,' velocity type',vtype
     print *,'This occurred at',n_tot_ref,' out of',reg%n_init,' nodes'
  endif

  n_tot_ref = n_tot_ref + inside_hits

end subroutine refract_teleseismic_front
