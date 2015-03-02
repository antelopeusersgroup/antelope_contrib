!*************************************************************
! This subroutine reads the initial parametrization of the velocity
! fields from which the values on the propagation grid are to be interpolated
! from file into the appropriate structures

subroutine initialize_velocity_grids
use mod_3dfm
implicit none

integer :: n,m,i,j,k
real(kind=dp) :: deg_to_rad
deg_to_rad=acos(-1.0_dp)/180.0_dp

open(10,file='vgrids.in')

! read the velocity mode from the file

! read the number of regions in the input velocity structure
read (10,*) n_vgrids,n_vtypes

! allocate space for these regions
allocate(vgrid(n_vgrids,n_vtypes))
do m=1,n_vtypes
   do n=1,n_vgrids 
      call vgrid_defaults(vgrid(n,m)) 
   end do
end do
! read the grid properties and velocity values to be interpolated for each region

do m=1,n_vtypes

   do n=1,n_vgrids

      ! grid parameters
      read(10,*) vgrid(n,m)%nr,vgrid(n,m)%nlat,vgrid(n,m)%nlong
      read(10,*) vgrid(n,m)%dr0,vgrid(n,m)%dlat0,vgrid(n,m)%dlong0
      read(10,*) vgrid(n,m)%r0,vgrid(n,m)%lat0,vgrid(n,m)%long0
      print *,'nr       nlat        nlon'
      print *,vgrid(n,m)%nr,vgrid(n,m)%nlat,vgrid(n,m)%nlong
      print *,'dr       dlat        dlon'
      print *,vgrid(n,m)%dr0,vgrid(n,m)%dlat0/deg_to_rad,vgrid(n,m)%dlong0/deg_to_rad
      print *,'or       olat        olon'
      print *,vgrid(n,m)%r0,vgrid(n,m)%lat0/deg_to_rad,vgrid(n,m)%long0/deg_to_rad
      print *,'maxr     maxlat      maxlon'
      print *,vgrid(n,m)%r0 + (vgrid(n,m)%nr-1)*vgrid(n,m)%dr0, &
              (vgrid(n,m)%lat0 + (vgrid(n,m)%nlat-1)*vgrid(n,m)%dlat0)/deg_to_rad, &
              (vgrid(n,m)%long0 + (vgrid(n,m)%nlong-1)*vgrid(n,m)%dlong0)/deg_to_rad
!      pgrid%r0 + (pgrid%nr-1)*pgrid%dr0
!      pgrid%lat0 + (pgrid%nlat-1)*pgrid%dlat0
!      pgrid%long0 + (pgrid%nlong-1)*pgrid%dlong0

! initialize the grid

      allocate(vgrid(n,m)%r(vgrid(n,m)%nr),vgrid(n,m)%lat(vgrid(n,m)%nlat), &
           vgrid(n,m)%long(vgrid(n,m)%nlong))
!      print *, 'Minimum/Maximum Coordinates of Velocity Grid' !AAA

      do i=1,vgrid(n,m)%nr
         vgrid(n,m)%r(i)=vgrid(n,m)%r0 + (i-1)*vgrid(n,m)%dr0
!         print*, vgrid(n,m)%r(i)
      end do
      print *, '----r'
      do i=1,vgrid(n,m)%nlat
         vgrid(n,m)%lat(i)=vgrid(n,m)%lat0 + (i-1)*vgrid(n,m)%dlat0
!         print*, vgrid(n,m)%lat(i)
      end do
      print *, '----lat'

      do i=1,vgrid(n,m)%nlong
         vgrid(n,m)%long(i)=vgrid(n,m)%long0 + (i-1)*vgrid(n,m)%dlong0
!         print*, vgrid(n,m)%long(i)
      end do

! read in the velocity values on the interpolation grid

      allocate(vgrid(n,m)%velocity(vgrid(n,m)%nr,vgrid(n,m)%nlat,vgrid(n,m)%nlong))

      do i=1,vgrid(n,m)%nr
         do j=1,vgrid(n,m)%nlat
            do k=1,vgrid(n,m)%nlong
               read (10,*) vgrid(n,m)%velocity(i,j,k)
            end do
         end do
      end do

      if (count(vgrid(n,m)%velocity > 20.0_dp) > 0 ) &
           print *,'*** WARNING *** : velocity grid contains values larger than 20 km/sec'
      if (count(vgrid(n,m)%velocity < 1.0_dp) > 0 ) &
           print *,'*** WARNING *** : velocity grid contains values less than 1 km/sec'

      vgrid(n,m)%nnode=vgrid(n,m)%nr*vgrid(n,m)%nlat*vgrid(n,m)%nlong

 ! allocate and initialize the activity flag

      allocate(vgrid(n,m)%active(vgrid(n,m)%nr,vgrid(n,m)%nlat,vgrid(n,m)%nlong))
      vgrid(n,m)%active = .false.


   end do  ! loop over interpolation regions

end do  ! vtypes

close(10)

end subroutine initialize_velocity_grids


!***********************************************************************************************
! reads the parameters of the propagation grid from file and initializes the grid
subroutine initialize_propagation_grid
use mod_3dfm
implicit none

integer :: i,j,k
real(kind=dp) :: deg_to_rad

open(10,file='propgrid.in')

allocate(pgrid)
call pgrid_defaults(pgrid)

! grid parameters
read(10,*) pgrid%nr,pgrid%nlat,pgrid%nlong
read(10,*) pgrid%dr0,pgrid%dlat0,pgrid%dlong0
read(10,*) pgrid%r0,pgrid%lat0,pgrid%long0
print *, pgrid%nr,pgrid%nlat,pgrid%nlong
print *, pgrid%dr0,pgrid%dlat0,pgrid%dlong0
print *, pgrid%r0,pgrid%lat0,pgrid%long0

deg_to_rad=acos(-1.0_dp)/180.0_dp

pgrid%dlat0=pgrid%dlat0*deg_to_rad
pgrid%dlong0=pgrid%dlong0*deg_to_rad
pgrid%lat0=pgrid%lat0*deg_to_rad
pgrid%long0=pgrid%long0*deg_to_rad

pgrid%r0 =  earth_radius + pgrid%r0 - dble(pgrid%nr-1)*pgrid%dr0

pgrid%tolerance=interface_tolerance*pgrid%dr0*.001 !EXTREMELY AD HOC AAA

pgrid%rmax = pgrid%r0 + (pgrid%nr-1)*pgrid%dr0
pgrid%latmax = pgrid%lat0 + (pgrid%nlat-1)*pgrid%dlat0
pgrid%longmax = pgrid%long0 + (pgrid%nlong-1)*pgrid%dlong0
print *,'Radmax/min     Latmax/min     Lonmax/min'
print *,pgrid%rmax, pgrid%latmax/deg_to_rad, pgrid%longmax/deg_to_rad
print *, pgrid%r0,pgrid%lat0/deg_to_rad,pgrid%long0/deg_to_rad

read(10,*) refinement_factor,ncell_to_be_refined

! initialize the grid

allocate(pgrid%r(pgrid%nr),pgrid%lat(pgrid%nlat),pgrid%coslat(pgrid%nlat),pgrid%long(pgrid%nlong))

do i=1,pgrid%nr
   pgrid%r(i)=pgrid%r0 + (i-1)*pgrid%dr0
!   print *,pgrid%r(i),pgrid%r0, pgrid%dr0,i
end do

do i=1,pgrid%nlat
   pgrid%lat(i)=pgrid%lat0 + (i-1)*pgrid%dlat0
end do

do i=1,pgrid%nlat
   pgrid%coslat(i)=cos(pgrid%lat(i))
end do


do i=1,pgrid%nlong
   pgrid%long(i)=pgrid%long0 + (i-1)*pgrid%dlong0
end do


close(10)


allocate(pgrid%rnode_id(pgrid%nr,pgrid%nlat,pgrid%nlong))
allocate(pgrid%node_region(pgrid%nr,pgrid%nlat,pgrid%nlong))
pgrid%node_region = 0
allocate(pgrid%ccind_from_3dc(pgrid%nr,pgrid%nlat,pgrid%nlong))
do k=1,pgrid%nlong
   do j=1,pgrid%nlat
      do i=1,pgrid%nr
         nullify(pgrid%ccind_from_3dc(i,j,k)%p)
      end do
   end do
end do

pgrid%is_main_grid = .true.

end subroutine initialize_propagation_grid



!*************************************************************
! This subroutine reads the initial parametrization of the interface positions
! from which the values on the propagation grid are to be interpolated
! from file into the appropriate structures

subroutine initialize_interfaces
use mod_3dfm
implicit none

integer :: n,i,j
integer :: nlat,nlong
real(kind=dp) :: dlat0,dlong0,lat0,long0,h,hb

open(10,file='interfaces.in')


! read the number of interfaces
read (10,*) n_interfaces

! allocate space for these interfaces (and the associated intersections and regions for future use)
allocate(intrface(n_interfaces))
do n=1,n_interfaces ; call interface_defaults(intrface(n)) ; intrface(n)%id=n ; end do


! read the grid properties and radius values to be interpolated for the internal interfaces

! grid parameters
   read(10,*) nlat,nlong
   read(10,*) dlat0,dlong0
   read(10,*) lat0,long0


do n=1,n_interfaces

   intrface(n)%nlat = nlat
   intrface(n)%nlong = nlong
   intrface(n)%dlat0 = dlat0
   intrface(n)%dlong0 = dlong0
   intrface(n)%lat0 = lat0
   intrface(n)%long0 = long0


! initialize the grid

   allocate(intrface(n)%lat(intrface(n)%nlat),intrface(n)%long(intrface(n)%nlong))

   do i=1,intrface(n)%nlat
      intrface(n)%lat(i)=intrface(n)%lat0 + (i-1)*intrface(n)%dlat0
   end do

   do i=1,intrface(n)%nlong
      intrface(n)%long(i)=intrface(n)%long0 + (i-1)*intrface(n)%dlong0
   end do


! read in the radius values on the interpolation grid

   allocate(intrface(n)%r(intrface(n)%nlat,intrface(n)%nlong))

   do i=1,intrface(n)%nlat
      do j=1,intrface(n)%nlong
         read (10,*) intrface(n)%r(i,j)
      end do
   end do

   intrface(n)%nnode=intrface(n)%nlat*intrface(n)%nlong

end do  ! loop over interfaces

close(10)

! check top and bottom interfaces are not outside the propagation grid
     print *,'Minimum interface height = ',minval(intrface(n_interfaces)%r) 
     print *,'Maximum interface height = ',maxval(intrface(1)%r) !AAA
     if (count(intrface(1)%r > pgrid%r(pgrid%nr)) > 0) stop ' ERROR: surface above propagation grid'
     if (count(intrface(n_interfaces)%r < pgrid%r(1)) >= 1) stop ' ERROR: bottom below propagation grid'


! correct for intersecting interfaces, higher takes priority

  do n=2,n_interfaces

     do j=1,intrface(n)%nlong
        do i=1,intrface(n)%nlat

    ! higher has priority, EXCEPT bottom interface

           if (n < n_interfaces) then

              hb=intrface(n_interfaces)%r(i,j) 

              if (intrface(n)%r(i,j) < hb) then
                 intrface(n)%r(i,j) = hb
                 intrface(n)%pinched = .true.
                 intrface(n_interfaces)%pinched = .true.
              endif

           endif

    ! check if interface above is crossed

           h=intrface(n-1)%r(i,j)

           if (intrface(n)%r(i,j) > h) then
              intrface(n)%r(i,j) = h
              intrface(n)%pinched = .true.
              intrface(n-1)%pinched = .true.
           endif

        end do
     end do

  end do


end subroutine initialize_interfaces


!*****************************************************
! this function returns the position (radius) of an interface at a horizontal position
! using 2D bicubic spline interpolation

function interpolate_interface(lat,long,iface)
use mod_3dfm
implicit none

real(kind=dp) :: interpolate_interface
real(kind=dp) :: lat,long
type(Tinterface)  :: iface

integer        :: i,j,ilat,ilong

real(kind=dp) :: u,v,bu(4),bv(4),value

ilat=floor((lat-iface%lat0)/iface%dlat0)+1
ilong=floor((long-iface%long0)/iface%dlong0)+1

!print *, 'long,iface%long0,iface%dlong0=',long,iface%long0,iface%dlong0

if (ilong < 2 .or. ilong > (iface%nlong-2)) then 
   print *,'interpolate_interface : interpolation outside range :ilong'
   print *,'iface%id,ilong,ilat=',iface%id,ilong,ilat
   print *,'long*180/3.14,lat*180/3.14,iface%nlong-2=',long*180/3.14,lat*180/3.14,iface%nlong-2
   print *,'if this happens during initialization your interface parameter '
   print *,'grid may not cover the entire propagation grid'
   stop
endif

if (ilat < 2 .or. ilat > (iface%nlat-2)) then
   print *,'interpolate_interface : interpolation outside range:ilat',iface%id
   print *,ilat,iface%nlat-2, lat,iface%lat0+(ilat-1)*iface%dlat0,iface%lat0+(ilat)*iface%dlat0
   print *,'if this happens during initialization your interface parameter '
   print *,'grid may not cover the entire propagation grid'
   stop
endif

u=(lat-iface%lat(ilat))/iface%dlat0
v=(long-iface%long(ilong))/iface%dlong0

bu(1)=(1.0_dp-u)**3/6.0_dp
bu(2)=(4.0_dp-6.0_dp*u**2+3.0_dp*u**3)/6.0_dp
bu(3)=(1.0_dp+3.0*u+3.0_dp*u**2-3.0_dp*u**3)/6.0_dp
bu(4)=u**3/6.0_dp
bv(1)=(1.0_dp-v)**3/6.0_dp
bv(2)=(4.0_dp-6.0_dp*v**2+3.0_dp*v**3)/6.0_dp
bv(3)=(1.0_dp+3.0*v+3.0_dp*v**2-3.0_dp*v**3)/6.0_dp
bv(4)=v**3/6.0_dp


value=0.0_dp
do j=1,4
   do i=1,4
      value=value+bu(i)*bv(j)*iface%r(ilat+i-2,ilong+j-2)
   end do
end do

interpolate_interface=value

end function interpolate_interface
!*****************************************************

! this subroutine returns the upward normal of an interface at a horizontal position
! using 2D bicubic spline interpolation

subroutine interface_normal(lat,long,iface,norm_r,norm_lat,norm_long,h)
use mod_3dfm
implicit none

real(kind=dp) :: lat,long,norm_r,norm_lat,norm_long
type(Tinterface)  :: iface

integer        :: i,j,ilat,ilong

real(kind=dp) :: u,v,bu(4),bv(4),h,dhdu,dhdv,bpu(4),bpv(4),dhdlat,dhdlong,norm

ilat=floor((lat-iface%lat0)/iface%dlat0)+1
ilong=floor((long-iface%long0)/iface%dlong0)+1

if (ilong < 2 .or. ilong > (iface%nlong-2)) then 
   print *,'interface_normal : interpolation outside range :ilong'
   print *,iface%id,ilong,ilat
   print *,long,lat
   stop
endif

if (ilat < 2 .or. ilat > (iface%nlat-2)) then
   print *,'interface_normal : interpolation outside range:ilat',iface%id
   print *,ilat,iface%nlat-2, lat,iface%lat0+(ilat-1)*iface%dlat0,iface%lat0+(ilat)*iface%dlat0
   stop
endif

u=(lat-iface%lat(ilat))/iface%dlat0
v=(long-iface%long(ilong))/iface%dlong0

bu(1)=(1.0_dp-u)**3/6.0_dp
bu(2)=(4.0_dp-6.0_dp*u**2+3.0_dp*u**3)/6.0_dp
bu(3)=(1.0_dp+3.0*u+3.0_dp*u**2-3.0_dp*u**3)/6.0_dp
bu(4)=u**3/6.0_dp
bv(1)=(1.0_dp-v)**3/6.0_dp
bv(2)=(4.0_dp-6.0_dp*v**2+3.0_dp*v**3)/6.0_dp
bv(3)=(1.0_dp+3.0*v+3.0_dp*v**2-3.0_dp*v**3)/6.0_dp
bv(4)=v**3/6.0_dp

bpu(1)=-0.5_dp*(1.0_dp-u)**2
bpu(2)=-2.0_dp*u+1.5_dp*u**2
bpu(3)=0.5_dp+u-1.5_dp*u**2
bpu(4)=0.5_dp*u**2
bpv(1)=-0.5_dp*(1.0_dp-v)**2
bpv(2)=-2.0_dp*v+1.5_dp*v**2
bpv(3)=0.5_dp+v-1.5_dp*v**2
bpv(4)=0.5_dp*v**2


h=0.0_dp
dhdu=0.0_dp
dhdv=0.0_dp
do j=1,4
   do i=1,4
      h   =h   +bu(i)*bv(j)*iface%r(ilat+i-2,ilong+j-2)
      dhdu=dhdu+bpu(i)*bv(j)*iface%r(ilat+i-2,ilong+j-2)
      dhdv=dhdv+bu(i)*bpv(j)*iface%r(ilat+i-2,ilong+j-2)
   end do
end do

dhdlat=dhdu/(h*iface%dlat0)
dhdlong=dhdv/(h*cos(lat)*iface%dlong0)

norm=sqrt(1.0_dp+dhdlat**2+dhdlong**2)

norm_r=1.0_dp/norm
norm_lat=-dhdlat/norm
norm_long=-dhdlong/norm

end subroutine interface_normal


!*****************************************************
! this function returns the velocity (propagation speed) at a 3D position
! using 3D bicubic spline interpolation

function interpolate_velocity(r,lat,long,gridv)
use mod_3dfm
implicit none


real(kind=dp) :: r,lat,long,interpolate_velocity
type(Tvelocity_grid)        :: gridv

integer        :: i,j,k,ir,ilat,ilong

real(kind=dp) :: u,v,w,bu(4),bv(4),bw(4),value

ir=floor((r-gridv%r0)/gridv%dr0)+1
ilat=floor((lat-gridv%lat0)/gridv%dlat0)+1
ilong=floor((long-gridv%long0)/gridv%dlong0)+1

!print *,ir,r,gridv%r0,gridv%dr0

if (ir < 2 .or. ir > (gridv%nr-2)) then
   print *,gridv%r0,gridv%dr0
   print *,r,ir,gridv%nr
   print *,gridv%r(gridv%nr),gridv%r(gridv%nr-1),gridv%r(1),gridv%r(2)
   print *, 'interpolate_velocity : interpolation outside range ir'
   print *,'if this happens during initialization your velocity parameter '
   print *,'grid may not cover the entire propagation grid'
   stop
endif

if (ilong < 2 .or. ilong > (gridv%nlong-2))  then
   print*,gridv%long0*180/3.14,(gridv%nlong*gridv%dlong0+gridv%long0)*180/3.14,gridv%dlong0*180/3.14
   print *,long*180/3.14,ilong,gridv%nlong,(gridv%nlong*gridv%dlong0+gridv%long0)*180/3.14
   print *, 'gridv%long0,gridv%dlong0',gridv%long0*180/3.14,gridv%dlong0
   print *, 'interpolate_velocity : interpolation outside range ilong'
   print *,'if this happens during initialization your velocity parameter '
   print *,'grid may not cover the entire propagation grid'
   stop
endif

if (ilat < 2 .or. ilat > (gridv%nlat-2))  then
   print *, 'interpolate_velocity : interpolation outside range ilat'
   print *,'if this happens during initialization your velocity parameter '
   print *,'grid may not cover the entire propagation grid'
   stop
endif

u=(r-gridv%r(ir))/gridv%dr0
v=(lat-gridv%lat(ilat))/gridv%dlat0
w=(long-gridv%long(ilong))/gridv%dlong0

bu(1)=(1.0_dp-u)**3/6.0_dp
bu(2)=(4.0_dp-6.0_dp*u**2+3.0_dp*u**3)/6.0_dp
bu(3)=(1.0_dp+3.0*u+3.0_dp*u**2-3.0_dp*u**3)/6.0_dp
bu(4)=u**3/6.0_dp
bv(1)=(1.0_dp-v)**3/6.0_dp
bv(2)=(4.0_dp-6.0_dp*v**2+3.0_dp*v**3)/6.0_dp
bv(3)=(1.0_dp+3.0*v+3.0_dp*v**2-3.0_dp*v**3)/6.0_dp
bv(4)=v**3/6.0_dp
bw(1)=(1.0_dp-w)**3/6.0_dp
bw(2)=(4.0_dp-6.0_dp*w**2+3.0_dp*w**3)/6.0_dp
bw(3)=(1.0_dp+3.0*w+3.0_dp*w**2-3.0_dp*w**3)/6.0_dp
bw(4)=w**3/6.0_dp


value=0.0_dp
do k=1,4
   do j=1,4
      do i=1,4
         value=value+bu(i)*bv(j)*bw(k)*gridv%velocity(ir+i-2,ilat+j-2,ilong+k-2)
      end do
   end do
end do


interpolate_velocity=value   !(5.0_dp*6271.0_dp)/r 


end function interpolate_velocity
!*****************************************************

! this subroutine returns the gradient of the propagation speed at a 3d position
! using 3D bicubic spline interpolation

subroutine velocity_gradient(r,lat,long,gridv,dvdr,dvdlat,dvdlong,vel)
use mod_3dfm
implicit none

real(kind=dp) :: r,lat,long,dvdr,dvdlat,dvdlong,vel
type(Tvelocity_grid)  :: gridv

integer        :: i,j,k,ir,ilat,ilong

real(kind=dp) :: u,v,w,bu(4),bv(4),bw(4),bpu(4),bpv(4),bpw(4),dvdu,dvdv,dvdw,vv

ir=floor((r-gridv%r0)/gridv%dr0)+1
ilat=floor((lat-gridv%lat0)/gridv%dlat0)+1
ilong=floor((long-gridv%long0)/gridv%dlong0)+1

if (ir < 2 .or. ir > (gridv%nr-2)) then
   print *,r,ir
   print *,gridv%r(gridv%nr),gridv%r(gridv%nr-1),gridv%r(1),gridv%r(2)
   stop ' velocity_gradient: interpolation outside range ir'
endif
if (ilong < 2 .or. ilong > (gridv%nlong-2)) then 
   print *,'velocity_gradient : interpolation outside range :ilong'
   print *,ilong,ilat
   print *,long,lat
   stop
endif

if (ilat < 2 .or. ilat > (gridv%nlat-2)) then
   print *,' velocity_gradient: interpolation outside range'
   print *,ilat
   stop
endif

u=(r-gridv%r(ir))/gridv%dr0
v=(lat-gridv%lat(ilat))/gridv%dlat0
w=(long-gridv%long(ilong))/gridv%dlong0

bu(1)=(1.0_dp-u)**3/6.0_dp
bu(2)=(4.0_dp-6.0_dp*u**2+3.0_dp*u**3)/6.0_dp
bu(3)=(1.0_dp+3.0*u+3.0_dp*u**2-3.0_dp*u**3)/6.0_dp
bu(4)=u**3/6.0_dp
bv(1)=(1.0_dp-v)**3/6.0_dp
bv(2)=(4.0_dp-6.0_dp*v**2+3.0_dp*v**3)/6.0_dp
bv(3)=(1.0_dp+3.0*v+3.0_dp*v**2-3.0_dp*v**3)/6.0_dp
bv(4)=v**3/6.0_dp
bw(1)=(1.0_dp-w)**3/6.0_dp
bw(2)=(4.0_dp-6.0_dp*w**2+3.0_dp*w**3)/6.0_dp
bw(3)=(1.0_dp+3.0*w+3.0_dp*w**2-3.0_dp*w**3)/6.0_dp
bw(4)=w**3/6.0_dp


bpu(1)=-0.5_dp*(1.0_dp-u)**2
bpu(2)=-2.0_dp*u+1.5_dp*u**2
bpu(3)=0.5_dp+u-1.5_dp*u**2
bpu(4)=0.5_dp*u**2
bpv(1)=-0.5_dp*(1.0_dp-v)**2
bpv(2)=-2.0_dp*v+1.5_dp*v**2
bpv(3)=0.5_dp+v-1.5_dp*v**2
bpv(4)=0.5_dp*v**2
bpw(1)=-0.5_dp*(1.0_dp-w)**2
bpw(2)=-2.0_dp*w+1.5_dp*w**2
bpw(3)=0.5_dp+w-1.5_dp*w**2
bpw(4)=0.5_dp*w**2

vv=0.0_dp
dvdu=0.0_dp
dvdv=0.0_dp
dvdw=0.0_dp
do j=1,4
   do i=1,4
      do k=1,4
         vv  = vv   + bu(i)*bv(j)*bw(k)*gridv%velocity(ir+i-2,ilat+j-2,ilong+k-2)
         dvdu= dvdu + bpu(i)*bv(j)*bw(k)*gridv%velocity(ir+i-2,ilat+j-2,ilong+k-2)
         dvdv= dvdv + bu(i)*bpv(j)*bw(k)*gridv%velocity(ir+i-2,ilat+j-2,ilong+k-2)
         dvdw= dvdw + bu(i)*bv(j)*bpw(k)*gridv%velocity(ir+i-2,ilat+j-2,ilong+k-2)
      end do
   end do
end do

vel    = vv
dvdr   = dvdu
dvdlat = dvdv/r
dvdlong= dvdw/(r*cos(lat))

return

end subroutine velocity_gradient


!*****************************************************
!****************************************************************************************************


! ****************************************************************************************
! this subroutine takes a grid and an interface as input, and produces
! the intersection between the two as output
! the intersection is a collection of nodes that lie on the intersections
! between the grid connections and the interface
! in addition, each intersection contains pointers to the grid cells cut by the interface
! and arrays containing pointers from interface nodes to the cut grid cells they are
! associated with, and the inverse, pointers from the cut grid cells to the interface nodes
! associated with them

subroutine find_intersection(isec,iface,grid)
use mod_3dfm_nointerfaces
implicit none

! subroutine arguments
type(Tintersection)              :: isec
type(Tinterface)                 :: iface
type(Tpropagation_grid),target   :: grid

! local arrays
real(kind=dp), dimension(:,:,:),allocatable            :: rdiff
real(kind=dp), dimension(:,:)  ,allocatable            :: r_interface
type(Tinteger_coordinates), dimension(:)  ,allocatable :: ijk_isec
integer, dimension(:)  ,allocatable                    :: type_isec 

integer,dimension(:,:,:), allocatable                  :: nextranodes 
integer,dimension(:,:,:,:),allocatable                 :: extranodes
integer,dimension(:),allocatable                       :: counter

! local variables
integer                                                :: i,j,k,n,m,nodecount
real(kind=dp)                                          :: rint
logical                                                :: diag = .false.

! external functions
real(kind=dp)                                          :: interpolate_interface


! id of interface on which the intersection is based

isec%iface_id = iface%id

! pointer to grid on which this intersection is defined

isec%grid => grid

! set flag indicating pinched intersection

isec%pinched=iface%pinched

! first allocate local arrays

allocate(rdiff(grid%nr,grid%nlat,grid%nlong),r_interface(grid%nlat,grid%nlong)) 
nodecount=grid%nr*grid%nlat*grid%nlong
allocate(ijk_isec(nodecount),type_isec(nodecount))
allocate(nextranodes(grid%nr+1,grid%nlat+1,grid%nlong+1))
allocate(extranodes(10,grid%nr+1,grid%nlat+1,grid%nlong+1))


! evaluate height above the interface of the propagation grid nodes
 
do k=1,grid%nlong
   do j=1,grid%nlat
      r_interface(j,k)=interpolate_interface(grid%lat(j),grid%long(k),iface)
      do i=1,grid%nr
         rdiff(i,j,k)=grid%r(i)-r_interface(j,k)
      end do

   end do
end do


!--------------------------
! find the intersection nodes , the cut cells and the region borders
! note that we have allocated an extra layer of cells around the real grid
! so that we don't have to test whether the cut cell is in the grid all the time

! irg_abo(j,k) and irg_bel(j,k) contain the r-index of the first regular grid node above 
! and below the interface

allocate(isec%irg_abo(grid%nlat,grid%nlong),isec%irg_bel(grid%nlat,grid%nlong))


! initialize irg_abo and irg_bel with defaults

do k=1,grid%nlong
   do j=1,grid%nlat
      if (r_interface(j,k) > grid%r(grid%nr)) then
         isec%irg_bel(j,k) = grid%nr + 1
         isec%irg_abo(j,k) = grid%nr + 1
      endif
      if (r_interface(j,k) < grid%r(1)) then
         isec%irg_bel(j,k) = 0
         isec%irg_abo(j,k) = 0
      endif
   end do
end do




! go over all regular grid nodes and test if there is an intersection between the node and
! the next in r , lat or long

nodecount=0
nextranodes=0

do k=1,grid%nlong
   do j=1,grid%nlat
      do i=1,grid%nr

         if (abs(rdiff(i,j,k)) <= grid%tolerance)  then ! node coincides with a grid node

            ! we have a new interface node           
            nodecount=nodecount+1 

            ! store the base integer coordinates and the offset type (in r, lat or long)
            ijk_isec(nodecount)%ir=i ;ijk_isec(nodecount)%ilat=j 
            ijk_isec(nodecount)%ilong=k ;type_isec(nodecount)=0

            ! in this case the node is part of 8 adjacent cells and we want to build up a list of
            ! nodes belonging to each cell

            ! first add one to the intersection node count of the affected grid cells
            nextranodes(i+1,j+1,k+1)=nextranodes(i+1,j+1,k+1)+1
            nextranodes(i+1,j,k+1)  =nextranodes(i+1,j,k+1)+1
            nextranodes(i+1,j+1,k)  =nextranodes(i+1,j+1,k)+1
            nextranodes(i+1,j,k)    =nextranodes(i+1,j,k)+1

            nextranodes(i,j+1,k+1)=nextranodes(i,j+1,k+1)+1
            nextranodes(i,j,k+1)  =nextranodes(i,j,k+1)+1
            nextranodes(i,j+1,k)  =nextranodes(i,j+1,k)+1
            nextranodes(i,j,k)    =nextranodes(i,j,k)+1
            
      ! then store the number of the new intersection node in the list of the affected grid cells
            extranodes(nextranodes(i+1,j+1,k+1),i+1,j+1,k+1) =nodecount
            extranodes(nextranodes(i+1,j,k+1)  ,i+1,j,k+1)   =nodecount
            extranodes(nextranodes(i+1,j+1,k)  ,i+1,j+1,k)   =nodecount
            extranodes(nextranodes(i+1,j,k)    ,i+1,j,k)     =nodecount

            extranodes(nextranodes(i,j+1,k+1),i,j+1,k+1) =nodecount
            extranodes(nextranodes(i,j,k+1)  ,i,j,k+1)   =nodecount
            extranodes(nextranodes(i,j+1,k)  ,i,j+1,k)   =nodecount
            extranodes(nextranodes(i,j,k)    ,i,j,k)     =nodecount



            ! store region boundary
            isec%irg_abo(j,k)=i+1
            isec%irg_bel(j,k)=i-1


            if(diag) write(13,'(a8,4i5)') 'it=0',nodecount,ijk_isec(nodecount)%ir, &
                 ijk_isec(nodecount)%ilat,ijk_isec(nodecount)%ilong
            

         else

            if (i<grid%nr)    then   ! if next in r is not outside the grid

               if (sign(1.0_dp,rdiff(i,j,k))*rdiff(i+1,j,k) < -grid%tolerance) then   

                ! interface intersects grid between grid node and the next in r

                  ! we have a new interface node
                  nodecount=nodecount+1 

                  ! store the base integer coordinates and the offset type (in r, lat or long)

                  ijk_isec(nodecount)%ir=i ;ijk_isec(nodecount)%ilat=j 
                  ijk_isec(nodecount)%ilong=k ;type_isec(nodecount)=1

                  ! the new interface node is part of 4 grid cells,and we want to build up a list of
                  ! nodes belonging to each cell

                  ! first add one to the intersection node count of the affected grid cells
                  nextranodes(i+1,j+1,k+1)=nextranodes(i+1,j+1,k+1)+1
                  nextranodes(i+1,j,k+1)  =nextranodes(i+1,j,k+1)+1
                  nextranodes(i+1,j+1,k)  =nextranodes(i+1,j+1,k)+1
                  nextranodes(i+1,j,k)    =nextranodes(i+1,j,k)+1
                  
         ! then store the number of the new intersection node in the list of the affected grid cells
                  extranodes(nextranodes(i+1,j+1,k+1),i+1,j+1,k+1) =nodecount
                  extranodes(nextranodes(i+1,j,k+1)  ,i+1,j,k+1)   =nodecount
                  extranodes(nextranodes(i+1,j+1,k)  ,i+1,j+1,k)   =nodecount
                  extranodes(nextranodes(i+1,j,k)    ,i+1,j,k)     =nodecount

                  ! store region boundary
                  isec%irg_abo(j,k)=i+1
                  isec%irg_bel(j,k)=i


                  if(diag) write(13,'(a8,4i5)') 'it=1',nodecount,ijk_isec(nodecount)%ir, &
                       ijk_isec(nodecount)%ilat,ijk_isec(nodecount)%ilong

               endif

            endif

            if (j<grid%nlat)  then     ! if next in lat is not outside the grid

               if (sign(1.0_dp,rdiff(i,j,k))*rdiff(i,j+1,k) < -grid%tolerance) then  

             ! node between grid node and the next in lat
             
                  nodecount=nodecount+1 
                  ijk_isec(nodecount)%ir=i ;ijk_isec(nodecount)%ilat=j 
                  ijk_isec(nodecount)%ilong=k ;type_isec(nodecount)=2

                  nextranodes(i+1,j+1,k+1)=nextranodes(i+1,j+1,k+1)+1
                  nextranodes(i,j+1,k+1)=nextranodes(i,j+1,k+1)+1
                  nextranodes(i+1,j+1,k)=nextranodes(i+1,j+1,k)+1
                  nextranodes(i,j+1,k)=nextranodes(i,j+1,k)+1
                  extranodes(nextranodes(i+1,j+1,k+1),i+1,j+1,k+1) =nodecount
                  extranodes(nextranodes(i,j+1,k+1)  ,i,j+1,k+1)   =nodecount
                  extranodes(nextranodes(i+1,j+1,k)  ,i+1,j+1,k)   =nodecount
                  extranodes(nextranodes(i,j+1,k)    ,i,j+1,k)     =nodecount

                  if(diag) write(13,'(a8,4i5)') 'it=2',nodecount,ijk_isec(nodecount)%ir, &
                       ijk_isec(nodecount)%ilat,ijk_isec(nodecount)%ilong

               endif

            endif

            if (k<grid%nlong) then    ! if next in long is not outside the grid

               if (sign(1.0_dp,rdiff(i,j,k))*rdiff(i,j,k+1) < -grid%tolerance) then  

                  ! node between grid node and the next in long
                  
                  nodecount=nodecount+1 
                  ijk_isec(nodecount)%ir=i ;ijk_isec(nodecount)%ilat=j 
                  ijk_isec(nodecount)%ilong=k ;type_isec(nodecount)=3

                  nextranodes(i+1,j+1,k+1)=nextranodes(i+1,j+1,k+1)+1
                  nextranodes(i+1,j,k+1)=nextranodes(i+1,j,k+1)+1
                  nextranodes(i,j+1,k+1)=nextranodes(i,j+1,k+1)+1
                  nextranodes(i,j,k+1)=nextranodes(i,j,k+1)+1
                  extranodes(nextranodes(i+1,j+1,k+1),i+1,j+1,k+1) =nodecount
                  extranodes(nextranodes(i+1,j,k+1)  ,i+1,j,k+1)   =nodecount
                  extranodes(nextranodes(i,j+1,k+1)  ,i,j+1,k+1)   =nodecount
                  extranodes(nextranodes(i,j,k+1)    ,i,j,k+1)     =nodecount

                  if(diag) write(13,'(a8,4i5)') 'it=3',nodecount,ijk_isec(nodecount)%ir, &
                       ijk_isec(nodecount)%ilat,ijk_isec(nodecount)%ilong

               endif

            endif

         endif
      end do
   end do
end do

print *,'intersection ',isec%id, ':',nodecount,' nodes found'

if (nodecount == 0) then
   isec%nnode=nodecount
!   print *,'no intersection points found,intersection',isec%id,' not created'
   deallocate(rdiff,r_interface,ijk_isec,type_isec,extranodes,nextranodes)
   deallocate(isec%irg_abo,isec%irg_bel)
   return
endif

!print *,isec%id,'irgbel,irgabo',isec%irg_bel(11,71),isec%irg_abo(11,71)


! allocate the arrays required to store the intersection

isec%nnode=nodecount      ! the number of nodes in the intersection

! the # of cells cut by the intersection
isec%n_ccells=count(nextranodes(2:grid%nr,2:grid%nlat,2:grid%nlong)>0) 

allocate(isec%r(nodecount),isec%lat(nodecount),isec%long(nodecount),isec%coslat(nodecount))
allocate(isec%intype(nodecount))
allocate(isec%ccell_from_inode(8,isec%nnode))
allocate(isec%ccells(isec%n_ccells))
allocate(isec%n_inodes(isec%n_ccells))
allocate(isec%inodes(maxval(nextranodes),isec%n_ccells))
allocate(isec%time_gradient(3,nodecount))
allocate(isec%normal(3,nodecount))


! store the 3d-indices of the cut cells and all interface nodes that belong to them

isec%n_inodes=0
n=0
do k=2,grid%nlong
   do j=2,grid%nlat
      do i=2,grid%nr

         if (nextranodes(i,j,k)>0) then  ! this cell is a cut cell

            n=n+1

            isec%ccells(n)%ir=i-1        ! integer coordinates of this cut cell
            isec%ccells(n)%ilat=j-1
            isec%ccells(n)%ilong=k-1

            ! store intersection nodes of this cut cell
            isec%n_inodes(n)=nextranodes(i,j,k)  ! store intersection nodes of this cut cell
            isec%inodes(1:isec%n_inodes(n),n)=extranodes(1:isec%n_inodes(n),i,j,k)

            ! construct the pointer from the corresponding main grid cell to this cut cell
            if (.not.associated(grid%ccind_from_3dc(i-1,j-1,k-1)%p)) then
               allocate(grid%ccind_from_3dc(i-1,j-1,k-1)%p(n_interfaces))
               grid%ccind_from_3dc(i-1,j-1,k-1)%p = 0
            endif
            grid%ccind_from_3dc(i-1,j-1,k-1)%p(isec%iface_id)=n

         endif

      end do
   end do
end do


! construct the inverse pointers from inodes to the cut cell they are in

allocate(counter(nodecount))
counter=0
isec%ccell_from_inode=0

do n=1,isec%n_ccells
   do m=1,isec%n_inodes(n)
      counter(isec%inodes(m,n))=counter(isec%inodes(m,n))+1
      if(counter(isec%inodes(m,n))>8) stop 'find_intersection:  counter > 8'
      isec%ccell_from_inode(counter(isec%inodes(m,n)),isec%inodes(m,n))=n
   end do
end do

deallocate(counter)


! find the coordinates of the intersection nodes

do n=1,isec%nnode

   select case (type_isec(n))

   case (0)          ! node coincides with a grid node

      isec%r(n)    = grid%r(ijk_isec(n)%ir)
      isec%lat(n)  = grid%lat(ijk_isec(n)%ilat)
      isec%coslat(n)  = grid%coslat(ijk_isec(n)%ilat)
      isec%long(n) = grid%long(ijk_isec(n)%ilong)
      isec%intype(n)= type_isec(n)

   case (1)          ! node between grid node and the next in r

      isec%r(n)    = r_interface(ijk_isec(n)%ilat,ijk_isec(n)%ilong)
      isec%lat(n)  = grid%lat(ijk_isec(n)%ilat)
      isec%coslat(n)  = grid%coslat(ijk_isec(n)%ilat)
      isec%long(n) = grid%long(ijk_isec(n)%ilong)
      isec%intype(n)= type_isec(n)

   case (2)          ! node between grid node and the next in lat

      isec%r(n)    = grid%r(ijk_isec(n)%ir)
      isec%lat(n)  = find_zero(ijk_isec(n),type_isec(n))
      isec%coslat(n)=cos(isec%lat(n))
      isec%long(n) = grid%long(ijk_isec(n)%ilong)
      isec%intype(n)= type_isec(n)

   case (3)          ! node between grid node and the next in long

      isec%r(n)    = grid%r(ijk_isec(n)%ir)
      isec%lat(n)  = grid%lat(ijk_isec(n)%ilat)
      isec%coslat(n)  = grid%coslat(ijk_isec(n)%ilat)
      isec%long(n) = find_zero(ijk_isec(n),type_isec(n))
      isec%intype(n)= type_isec(n)


   case default 
      stop 'illegal intersection type in subroutine find_intersection'

   end select

end do

! deallocate all the local temporary arrays

deallocate(rdiff,r_interface,ijk_isec,type_isec,extranodes,nextranodes)


! calculate the surface normals

do n=1,isec%nnode
   call interface_normal(isec%lat(n),isec%long(n),iface,isec%normal(1,n), &
        isec%normal(2,n),isec%normal(3,n),rint)
end do


contains

  function find_zero(ijk,itype)
  ! bisection root finding subroutine from NR

    use mod_3dfm
    implicit none

    real(kind=dp) :: find_zero

    ! function arguments
    type(Tinteger_coordinates)     :: ijk     ! base integer coordinates of the root
    integer     :: itype   ! root is offset from base coordinates in lat (itype=2) or long(itype=3)

    real(kind=dp) :: f,fmid,x1,x2,xmid,rtbis,dx 
    integer :: nn



    select case (itype)

        case(2)    ! intersection node lies in positive lat direction
           
           x1=grid%lat(ijk%ilat)
           x2=grid%lat(ijk%ilat+1)
           f=rdiff(ijk%ir,ijk%ilat,ijk%ilong)
           fmid=rdiff(ijk%ir,ijk%ilat+1,ijk%ilong)
           if (f*fmid >= 0.0_dp) stop 'root not bracketed in find_intersection'
           if (f < 0.0_dp) then
              rtbis=x1
              dx=x2-x1
           else
              rtbis=x2
              dx=x1-x2
           endif
           do nn=1,10
              dx=dx*0.5_dp
              xmid=rtbis+dx
              fmid=grid%r(ijk%ir)-interpolate_interface(xmid,grid%long(ijk%ilong),iface)
              if (fmid <= 0.0_dp) rtbis=xmid
           end do


        case(3)    ! intersection node lies in positive long direction

           x1=grid%long(ijk%ilong)
           x2=grid%long(ijk%ilong+1)
           f=rdiff(ijk%ir,ijk%ilat,ijk%ilong)
           fmid=rdiff(ijk%ir,ijk%ilat,ijk%ilong+1)
           if (f*fmid >= 0.0_dp) stop 'root not bracketed in find_intersection'
           if (f < 0.0_dp) then
              rtbis=x1
              dx=x2-x1
           else
              rtbis=x2
              dx=x1-x2
           endif
           do nn=1,10
              dx=dx*0.5_dp
              xmid=rtbis+dx
              fmid=grid%r(ijk%ir)-interpolate_interface(grid%lat(ijk%ilat),xmid,iface)
              if (fmid <= 0.0_dp) rtbis=xmid
           end do


        case default
           stop 'illegal intersection type in function find_zero'

      end select

      find_zero=rtbis

   end function find_zero

end subroutine find_intersection

!***********************************************************************************************
! This subroutine constructs the list of nodes (regular, top/bottom intersection) 
! belonging to a region

subroutine define_region(reg,itop,ibot,grid)
use mod_3dfm_nointerfaces
implicit none

type(Tregion),target     :: reg
type(Tintersection)      :: itop,ibot
type(Tpropagation_grid),target :: grid

integer :: m,i,j,k,istart,iend


if (.not.associated(itop%r) .and. itop%nnode /= 0) stop 'define regions: intersections not allocated'

! set the velocity grid to be used for this region

reg%ivgrid=itop%iface_id

! pointer to the grid on which this region is defined

reg%grid => grid


! assign the proper region # to the nodes of the main propagation grid  
! itop or ibot may be above or below the grid and have no intersection nodes

if (itop%nnode > 0 .and. ibot%nnode > 0) then      
   do k=1,grid%nlong
      do j=1,grid%nlat
         istart=max(1,ibot%irg_abo(j,k))
         iend=min(grid%nr,itop%irg_bel(j,k))
         if (iend >= istart) then
            do i=istart,iend
               grid%node_region(i,j,k) = reg%id
            end do
         endif
      end do
   end do
endif
if (itop%nnode > 0 .and. ibot%nnode == 0) then      
   do k=1,grid%nlong
      do j=1,grid%nlat
         do i=1,min(grid%nr,itop%irg_bel(j,k))
            grid%node_region(i,j,k) = reg%id
         end do
      end do
   end do
endif
if (itop%nnode == 0 .and. ibot%nnode > 0) then      
   do k=1,grid%nlong
      do j=1,grid%nlat
         do i=max(1,ibot%irg_abo(j,k)),grid%nr
            grid%node_region(i,j,k) = reg%id
         end do
      end do
   end do
endif
if (itop%nnode == 0 .and. ibot%nnode == 0) then      
   do k=1,grid%nlong
      do j=1,grid%nlat
         do i=1,grid%nr
            grid%node_region(i,j,k) = reg%id
         end do
      end do
   end do
endif


! print *,'def reg: grid node region id assigned'

! derive and store useful information about the regions

! register the region with bounding intersections
   itop%regbel => reg 
   ibot%regabo => reg 


! # of grid nodes in this region
   reg%ngnode=count(grid%node_region == reg%id)  ! regular grid nodes only
   reg%nnode=reg%ngnode + itop%nnode + ibot%nnode ! total

!   print *,'reg def: nnode = ',reg%nnode


! make a 1-D list of all nodes (grid + intersection) nodes in this region

!  array reg%node contains pointers from the list to the grid/interface nodes

   allocate(reg%node(reg%nnode))


! these arrays contain pointers from intersection arrays to the regional node lists above and below

   if (itop%nnode > 0) allocate(itop%rbel_node_id(itop%nnode))
   if (ibot%nnode > 0) allocate(ibot%rabo_node_id(ibot%nnode))



! storage for regional node coordinates
   allocate(reg%r(reg%nnode))
   allocate(reg%lat(reg%nnode))
   allocate(reg%coslat(reg%nnode))
   allocate(reg%long(reg%nnode))

 !  print *,'reg def : pos arrrays allocated'

! start constructing the 1-D pointer arrays
   m=0

! add the regular grid points
   do k=1,grid%nlong
      do j=1,grid%nlat
         do i=1,grid%nr
            if (grid%node_region(i,j,k) == reg%id) then
               m=m+1
               reg%node(m)%i1=i 
               reg%node(m)%i2=j 
               reg%node(m)%i3=k 
         ! grid%rnode_id contains the index of the node in its regional node list
               grid%rnode_id(i,j,k)=m  
               reg%r(m) = grid%r(i)
               reg%lat(m) = grid%lat(j)
               reg%coslat(m) = grid%coslat(j)
               reg%long(m) = grid%long(k)
            endif
         end do
      end do
   end do

! print *,'reg def regular nodes initialized'

! add the top bounding intersection nodes if they exist 
   if (itop%nnode > 0) then
      do i=1,itop%nnode
         m=m+1
         reg%node(m)%i1=0 
         reg%node(m)%i2=itop%id 
         reg%node(m)%i3=i
         itop%rbel_node_id(i) = m
         reg%r(m) = itop%r(i)
         reg%lat(m) = itop%lat(i)
         reg%coslat(m) = itop%coslat(i)
         reg%long(m) = itop%long(i)
      end do
   endif

! print *,'reg def top nodes initialized'

! add the bottom bounding intersection nodes if they exist
   if (ibot%nnode > 0) then
      do i=1,ibot%nnode
         m=m+1
         reg%node(m)%i1=0 
         reg%node(m)%i2=ibot%id 
         reg%node(m)%i3=i
         ibot%rabo_node_id(i) = m
         reg%r(m) = ibot%r(i)
         reg%lat(m) = ibot%lat(i)
         reg%coslat(m) = ibot%coslat(i)
         reg%long(m) = ibot%long(i)
      end do
   endif

   if (m /= reg%nnode) stop 'define region: node count mismatch'

!   print *,'reg def bot nodes initialized'

return

end subroutine define_region

!*************************************************************************************************
! calculates the velocities on the nodes of a propagation grid with cubic spline interpolation
subroutine velocities_to_grid(grid)
use mod_3dfm
implicit none

type(Tpropagation_grid)  :: grid

integer  :: i,j,k,ivg,m

real(kind=dp)  :: interpolate_velocity


allocate(grid%velocity(grid%nr,grid%nlat,grid%nlong,n_vtypes))

! velocities on regular grid

if (grid%is_main_grid) then

do m=1,n_vtypes
   do k=1,grid%nlong
      do j=1,grid%nlat
         do i=1,grid%nr
            if (grid%node_region(i,j,k)>0) then
               ivg = region(grid%node_region(i,j,k))%ivgrid
               grid%velocity(i,j,k,m) = interpolate_velocity(grid%r(i),grid%lat(j), &
                    grid%long(k),vgrid(ivg,m))
            endif
         end do
      end do
   end do
end do

endif

if (grid%is_source_grid) then

do m=1,n_vtypes
   do k=1,grid%nlong
      do j=1,grid%nlat
         do i=1,grid%nr
            if (grid%node_region(i,j,k)>0) then
               ivg = sregion(grid%node_region(i,j,k))%ivgrid
               grid%velocity(i,j,k,m) = interpolate_velocity(grid%r(i),grid%lat(j), &
                    grid%long(k),vgrid(ivg,m))
            endif
         end do
      end do
   end do
end do

endif

return

end subroutine velocities_to_grid

!*************************************************************************************************
! calculates the velocities on the nodes of an intersection with cubic spline interpolation
subroutine velocities_to_intersection(isec)
use mod_3dfm
implicit none

type(Tintersection)  :: isec
integer  :: i,j,n,m,vg_abo,vg_bel,isig_abo,isig_bel,iface_id

real(kind=dp)  :: interpolate_velocity,interpolate_interface,h,vel


! velocities on intersection nodes

   if (associated(isec%regabo)) then   

! if there is a defined region above the intersection,get velocities on the top

      allocate(isec%vel_top(isec%nnode,n_vtypes)) 

      vg_abo = isec%regabo%ivgrid

      do m=1,n_vtypes
         do i=1,isec%nnode
            isec%vel_top(i,m)=interpolate_velocity(isec%r(i),isec%lat(i),isec%long(i),vgrid(vg_abo,m))
         end do
      end do

   endif

   if (associated(isec%regbel)) then   

! if there is a defined region below the intersection,get velocities on the bottom

      allocate(isec%vel_bot(isec%nnode,n_vtypes))

      vg_bel = isec%regbel%ivgrid

      do m=1,n_vtypes
         do i=1,isec%nnode
            isec%vel_bot(i,m)=interpolate_velocity(isec%r(i),isec%lat(i),isec%long(i),vgrid(vg_bel,m))
         end do
      end do
   endif



! adjust velocities at the intersection nodes where the interfaces are pinched


   if (isec%pinched) then

      m=0
      iface_id = isec%iface_id


  ! find the interfaces significantly above or below the current one at the node positions

nodeloop:      do i=1,isec%nnode

      ! above
         isig_abo = 0         
         if (iface_id > 1) then
            do j=iface_id-1,1,-1
               h = interpolate_interface(isec%lat(i),isec%long(i),intrface(j))
               if (h-isec%r(i) > pgrid%tolerance) then
                  isig_abo = j
                  exit
               end if
            end do
         end if

      ! below
         isig_bel = n_interfaces+1         
         if (iface_id < n_interfaces) then
            do j=iface_id+1,n_interfaces
               h = interpolate_interface(isec%lat(i),isec%long(i),intrface(j))
               if (isec%r(i)-h > pgrid%tolerance) then
                  isig_bel = j
                  exit
               end if
            end do
         end if


      ! go to next node if interfaces not pinched at this position

         if (isig_abo == iface_id-1 .and. isig_bel == iface_id+1) cycle nodeloop



      ! now adjust the velocity values at the node 


         if (isig_abo == 0) then      ! the special case that the interface coincides with the surface

            ! set top and bottom velocity to that from the first significant region below
            ! this ensures the wave passes without modification

            vg_bel=region(isig_bel-1)%ivgrid

            do n=1,n_vtypes
               vel = interpolate_velocity(isec%r(i),isec%lat(i),isec%long(i),vgrid(vg_bel,n))

               if (iface_id > 1) then    ! vel_top of intersection(1) does not exist
                  isec%vel_top(i,n)=vel
               endif
               isec%vel_bot(i,n)=vel

            end do

            m=m+1
            cycle nodeloop

         endif


         if (isig_bel == n_interfaces+1) then      

       ! the special case that the interface coincides with the bottom

           ! set top and bottom velocity to that from the first significant region above
           ! this ensures the wave passes without modification

            vg_abo=region(isig_abo)%ivgrid

            do n=1,n_vtypes
               vel = interpolate_velocity(isec%r(i),isec%lat(i),isec%long(i),vgrid(vg_abo,n))

               if (iface_id < n_interfaces) then 

           ! vel_bot of intersection(n_interfaces) does not exist

                  isec%vel_bot(i,n)=vel
               endif
               isec%vel_top(i,n)=vel

           end do

            m=m+1
            cycle nodeloop

         endif

       ! the normal case: make only the lowest of the locally pinched interfaces significant

         vg_abo=region(isig_abo)%ivgrid
         do n=1,n_vtypes
            vel = interpolate_velocity(isec%r(i),isec%lat(i),isec%long(i),vgrid(vg_abo,n))
            isec%vel_top(i,n) = vel 
            if (isig_bel /= iface_id+1) isec%vel_bot(i,n) = vel
         end do
         m=m+1

      end do nodeloop ! loop over intersection nodes

      print '(a50,2i7)','corrected velocities at pinched nodes of iface ',isec%iface_id,m

   endif  ! if interface is pinched


   return

end subroutine velocities_to_intersection

!*************************************************************************************************
! copies the velocities stored in grid and intersections to the corresponding nodes of a region
subroutine velocities_to_region(reg,grid)
use mod_3dfm
implicit none

type(Tpropagation_grid)  :: grid
type(Tregion)  :: reg
integer  :: n,m

! assign velocities in regions

   allocate(reg%velocity(reg%nnode,n_vtypes))

   do n=1,n_vtypes
      do m=1,reg%nnode

         if (reg%node(m)%i1 /= 0) then

            reg%velocity(m,n)= grid%velocity(reg%node(m)%i1,reg%node(m)%i2,reg%node(m)%i3,n)

         else

            if (reg%node(m)%i2 == reg%itop%id) then
               reg%velocity(m,n)= reg%itop%vel_bot(reg%node(m)%i3,n)
            else
               if (reg%node(m)%i2 /= reg%ibot%id)  stop 'transfer velocities: ordering problem'
               reg%velocity(m,n)= reg%ibot%vel_top(reg%node(m)%i3,n) 
            endif

         endif
      end do
   end do


! tag the nodes on the velocity grid that actually influence the velocity field in this region
! since the velocity grids are square and in the simplest case cover the entire propagation grid
! this can be a small subset only

   call tag_active_vgrid_nodes(reg)


return

end subroutine velocities_to_region



!*****************************************************************************************************
!----------------------------------------------------------------------------------------------
! finds out where a source lies relative to propagation grid and intersections
! and stores this information inside the derived type Tsource associated with it

subroutine initialize_source(s,grid)
  use mod_3dfm
  implicit none

  integer       :: n,i,j,k
  type(Tsource) :: s 
  type(Tpropagation_grid) :: grid
  real(kind=dp) :: dist,dist_below,interpolate_interface,h
  logical,dimension(:),allocatable       :: source_on_interface


  print *,'entering initialize_source subroutine' !AAA

! test for source in grid
!  print *, s%r
!  print *, interpolate_interface(s%lat,s%long,intrface(1))
  if (s%lat > pgrid%lat(pgrid%nlat)+pgrid%tolerance/s%r ) stop 'ERROR: source position beyond maximum lat'
  if (s%lat < pgrid%lat(1) -pgrid%tolerance/s%r) stop 'ERROR:source position beyond minimum lat'
  if (s%long > pgrid%long(pgrid%nlong)+pgrid%tolerance/s%r ) stop 'ERROR:source position beyond maximum long'
  if (s%long < pgrid%long(1)-pgrid%tolerance/s%r ) stop 'ERROR:source position beyond minimum long'
  !AAA If source is above the furface, set it on the surface!
  if (s%r > interpolate_interface(s%lat,s%long,intrface(1))+pgrid%tolerance) then
       print *, 'Source is above the surface; placing source on the surface'
       print *, 'Old source depth= ',s%r
       s%r=interpolate_interface(s%lat,s%long,intrface(1))
       print *, 'New source depth= ',s%r
  endif
  if (s%r < interpolate_interface(s%lat,s%long,intrface(n_interfaces))-pgrid%tolerance ) &
       stop 'ERROR:source below lowest interface'

!  print*, 'after test',s%r,interpolate_interface(s%lat,s%long,intrface(n_interfaces))-pgrid%tolerance

! determine grid cell in which the source is located

  s%ir    =  floor((s%r - grid%r0)/grid%dr0 + 1)
  s%ilat  =  floor((s%lat - grid%lat0)/grid%dlat0 + 1)
  s%ilong =  floor((s%long - grid%long0)/grid%dlong0 + 1)

! correct if source lies exactly on grid boundary 
 
  s%ir = min(s%ir,grid%nr-1)
  s%ilat = min(s%ilat,grid%nlat-1)
  s%ilong = min(s%ilong,grid%nlong-1)

  s%on_grid=.false.
  s%on_interface=.false.
  s%n_cnode = 0

! allocate arrays that will contain the indices of the source time fields in the time field array
  allocate(s%first_tf_up(n_vtypes),s%first_tf_down(n_vtypes))


! test where the source lies


! first test if the source lies exactly on any interface

  allocate(source_on_interface(n_interfaces))
  source_on_interface=.false.

  dist_below=1.0_dp
  do n=n_interfaces,1,-1

     dist = s%r-interpolate_interface(s%lat,s%long,intrface(n))

     source_on_interface(n) = abs(dist) < 2.0_dp*grid%tolerance 
     if (source_on_interface(n))   print *,'source lies exactly on interface',n

     if (dist < 0.0_dp .and. dist_below > 0.0_dp) s%region_id = n

     dist_below=dist

  enddo

  s%on_interface = count(source_on_interface(1:n_interfaces)) > 0
  s%on_pinched_interface = count(source_on_interface(1:n_interfaces)) > 1


  if (s%on_interface) then

     s%topint_id = 0 
     s%botint_id = n_interfaces+1

     do i=1,n_interfaces !This loop or the next one breaks sometimes! Probably because of tolerance

        h = interpolate_interface(s%lat,s%long,intrface(i))
        if (h-s%r > pgrid%tolerance) s%topint_id = i
        print *,'h-s%r,pgrid%tolerance= ',h-s%r,pgrid%tolerance
        print *,h-s%r>pgrid%tolerance
     end do
     print *,'s%topint_id= ',s%topint_id !AAA
     do i=n_interfaces,1,-1

        h = interpolate_interface(s%lat,s%long,intrface(i))
        if (s%r-h > pgrid%tolerance) s%botint_id = i
        print *,'s%r-h= ',s%r-h
     end do
     print *,'s%botint_id= ',s%botint_id
     s%topint_id=s%topint_id+1
     s%botint_id=s%botint_id-1

     s%topreg_id=s%topint_id-1
     s%botreg_id=s%botint_id
     print *,'s%topint_id,s%botint_id= ',s%topint_id,s%botint_id
     if (s%topint_id == 1 .or. s%botint_id == n_interfaces) then
        s%n_tf_init = 1
     else
        s%n_tf_init = 2
     endif

     print *,'istest',s%topint_id,s%botint_id,s%topreg_id,s%botreg_id
!     stop
  endif

  deallocate(source_on_interface)


! test if the source lies exactly on a regular grid node or not

  if (.not. s%on_interface) then

     do i=0,1
        do j=0,1
           do k=0,1

              dist=sqrt((s%r-grid%r(s%ir+i))**2 + (s%lat-grid%lat(s%ilat+j))**2 +  &
                   (s%long-grid%long(s%ilong+k))**2 ) 
              if (dist < grid%tolerance) s%on_grid=.true.

           end do
        end do
     end do
 
     s%n_tf_init = 1
             
  endif  ! if not on interface



  return

end subroutine initialize_source

!----------------------------------------------------------------------------------------
! this is the basic unit used to find a fast marching solution in a region
! arrival times and gradients are copied from the starting intersection
! to the region, node status is set appropriately and then the region is
! passed on to the fast marching routine propagate
! also tests for and flags turning rays, i.e. nodes on the starting interface 
! that receive a new time during fast marching


subroutine sweep_region_from_interface(reg,istart_in,vtype,s)
  use mod_3dfm_nointerfaces
  implicit none

  integer                                :: vtype
  integer                                :: i,j,i1,i2,i3,n_turning
  type(Tintersection),pointer            :: itop,ibot,istart
  type(Tintersection),target             :: istart_in
  type(Tregion)                          :: reg
  type(Tsource)                          :: s
  logical,dimension(:),allocatable       :: received_turning_ray

!----------------------------------------------------------
! prepare for propagation in a region


  if (reg%id > n_regions .or. reg%id < 1) stop 'invalid region requested'
  if (istart_in%iface_id > n_interfaces .or. istart_in%iface_id < 1) &
       stop ' invalid starting intersection'
  if (.not.(istart_in%id == reg%itop%id .or. istart_in%id == reg%ibot%id ))  &
       stop ' requested starting intersection and region incompatible'


! local names for  starting, top and bottom intersections

  istart => istart_in
  itop => reg%itop
  ibot => reg%ibot

  allocate(reg%arrivaltime(reg%nnode),reg%time_gradient(3,reg%nnode))
  allocate(reg%node_status(reg%nnode))
  allocate(received_turning_ray(istart%nnode))
  received_turning_ray=.false.

  reg%node_status=-1

  if (.not. associated(istart%arrivaltime))  &
       stop 'requested starting interface does not have arrival times'

  allocate(istart%starttime(istart%nnode))


! set start time, time gradient and node status narrow band in regional 
! nodes that belong to the starting interface

  do i=1,istart%nnode
     istart%starttime(i)=istart%arrivaltime(i)
     if (istart%id == ibot%id) then
        j=istart%rabo_node_id(i)
     else
        j=istart%rbel_node_id(i)
     endif
     reg%arrivaltime(j)= istart%arrivaltime(i)
     reg%time_gradient(1:3,j)=istart%time_gradient(1:3,i)
     reg%node_status(j)=1
  end do

 ! print *,'region',reg%id,' starting times set'


!-------------------------------------------------------------------------------------
! do the fast marching sweep

  call propagate(reg,vtype)

  print *,'propagation through region',reg%id,' finished'


! transfer regional travel times to interfaces and regular grid
! and check for turning rays

  n_turning=0
  if (itop%nnode > 0 .and. .not. associated(itop%arrivaltime)) allocate(itop%arrivaltime(itop%nnode))
  if (ibot%nnode > 0 .and. .not. associated(ibot%arrivaltime)) allocate(ibot%arrivaltime(ibot%nnode))

  do i=1,reg%nnode

     i1 = reg%node(i)%i1 ; i2 = reg%node(i)%i2 ; i3 = reg%node(i)%i3

     if (i1 /= 0) then

!        grid%arrivaltime(i1,i2,i3)=reg%arrivaltime(i)

     else

        if (i2 /= itop%id .and. i2 /= ibot%id)  &
             stop 'intersection id mismatch in sweep_region_from_interface' 

        if (i2 == itop%id) then
           itop%arrivaltime(i3) = reg%arrivaltime(i)
           itop%time_gradient(1:3,i3) = reg%time_gradient(1:3,i)        
        else
           ibot%arrivaltime(i3) = reg%arrivaltime(i)
           ibot%time_gradient(1:3,i3) = reg%time_gradient(1:3,i)
        endif

        if (i2 == istart%id) then
           if (istart%arrivaltime(i3) < istart%starttime(i3)) then 
              n_turning=n_turning+1
              received_turning_ray(i3) = .true.
           endif
        endif

     endif

  end do
      
  if (n_turning > 0) print *,n_turning,' starting intersection nodes received a turning ray'


  ! transfer time field to the array of saved time fields

  s%n_time_fields=s%n_time_fields+1
  allocate(s%time_field(s%n_time_fields)%arrivaltime(reg%nnode))
  s%time_field(s%n_time_fields)%arrivaltime=reg%arrivaltime
  allocate(s%time_field(s%n_time_fields)%time_gradient(3,reg%nnode))
  s%time_field(s%n_time_fields)%time_gradient=reg%time_gradient


  ! if  turning rays were present, identify the intersection nodes that received them

  if (n_turning > 0) then
     s%time_field(s%n_time_fields)%turning_rays_present = .true.
     allocate(s%time_field(s%n_time_fields)%received_turning_ray(istart%nnode))
     s%time_field(s%n_time_fields)%received_turning_ray = received_turning_ray
  endif

  deallocate(reg%arrivaltime,reg%time_gradient,reg%node_status,istart%starttime)
  deallocate(received_turning_ray)

end subroutine sweep_region_from_interface

!----------------------------------------------------------------------------------------
! A simple version of the routine above used only for regions on the refined source grid
subroutine sweep_sregion_from_interface(reg,istart_in,vtype)
  use mod_3dfm_nointerfaces
  implicit none

  integer                                :: vtype
  integer                                :: i,j
  type(Tintersection),pointer            :: itop,ibot,istart
  type(Tintersection),target             :: istart_in
  type(Tregion)                          :: reg

!----------------------------------------------------------
! prepare for propagation in a region


  if (reg%id > n_regions .or. reg%id < 1) stop 'invalid region requested'
  if (istart_in%iface_id > n_interfaces .or. istart_in%iface_id < 1) &
       stop ' invalid starting intersection'
  if (.not.(istart_in%id == reg%itop%id .or. istart_in%id == reg%ibot%id ))  &
       stop ' requested starting intersection and region incompatible'


! local names for  starting, top and bottom intersections

  istart => istart_in
  itop => reg%itop
  ibot => reg%ibot

  allocate(reg%arrivaltime(reg%nnode),reg%time_gradient(3,reg%nnode))
  allocate(reg%node_status(reg%nnode))
  reg%node_status=-1

  if (.not. associated(istart%arrivaltime))  &
       stop 'requested starting interface does not have arrival times'

  allocate(istart%starttime(istart%nnode))


! set start time, time gradient and node status narrow band in regional 
! nodes that belong to the starting interface
  do i=1,istart%nnode
     istart%starttime(i)=istart%arrivaltime(i)
     if (istart%id == ibot%id) then
        j=istart%rabo_node_id(i)
     else
        j=istart%rbel_node_id(i)
     endif
     reg%arrivaltime(j)= istart%arrivaltime(i)
     reg%time_gradient(1:3,j)=istart%time_gradient(1:3,i)
     reg%node_status(j)=1
  end do

 ! print *,'sregion',reg%id,' starting times set'


!-------------------------------------------------------------------------------------
! do the fast marching sweep

  call propagate(reg,vtype)

  print *,'propagation through sregion',reg%id,' finished'

end subroutine sweep_sregion_from_interface


!----------------------------------------------------------------------------------------

subroutine sweep_region_from_source(reg,s,vtype)
  use mod_3dfm
  implicit none

  type(Tsource)   :: s
  type(Tpropagation_grid),pointer :: grid
  integer         :: vtype
  integer         :: m,j,i1,i2,i3
  type(Tintersection),pointer :: itop,ibot
  type(Tregion)   :: reg

  real(kind=dp)   :: dist,vel_av,vel_source,interpolate_velocity,dtr


  grid => reg%grid

! validate the arguments

  if (.not.s%on_interface) then
     if (reg%id /= s%region_id) stop 'region and source inconsistent'
  endif

  dtr=acos(-1.0_dp)/180.0_dp

! some local variable names to make expressions shorter

  itop => reg%itop
  ibot => reg%ibot

  vel_source=interpolate_velocity(s%r,s%lat,s%long,vgrid(reg%ivgrid,vtype))

! allocate space for regional arrays that do not need to be saved for later

  allocate(reg%arrivaltime(reg%nnode),reg%time_gradient(3,reg%nnode),reg%node_status(reg%nnode))

! set all regional points to FAR
  reg%node_status=-1
  reg%arrivaltime=huge_time


! set start time and node status NARROW BAND at the source

  if (s%n_cnode == 1) then   ! source lies exactly on a node, only 1 starting point with time 0

     i1=s%cnode(1)%i1  ;i2=s%cnode(1)%i2  ; i3=s%cnode(1)%i3  

     if (i1 /= 0) then  ! node is a regular grid point

        j= grid%rnode_id(i1,i2,i3)
        reg%arrivaltime(j)= 0.0_dp
        reg%time_gradient(1:3,j)= 0.0_dp
        reg%node_status(j)=1 
       
!        print *,'time 0 assigned to',j,i1,i2,i3
!        print *,reg%r(j),reg%lat(j)/dtr,reg%long(j)/dtr


     else                          ! node is an interface node

        if (i2 == ibot%id) then
           j=ibot%rabo_node_id(i3)
        else
           j=itop%rbel_node_id(i3)
        endif
        reg%arrivaltime(j)= 0.0_dp
        reg%time_gradient(1:3,j)= 0.0_dp
        reg%node_status(j)= 1

        print *,'source node',j,'set to zero time'
!        print *,reg%r(j),reg%lat(j)/dtr,reg%long(j)/dtr

     endif

  else                     ! source does not lie on a node, several starting points

     do m=1,s%n_cnode

     i1=s%cnode(m)%i1  ;i2=s%cnode(m)%i2  ; i3=s%cnode(m)%i3  

     if (i1 /= 0) then  ! node is a regular grid point

        j= grid%rnode_id(i1,i2,i3)
        vel_av=0.5_dp*(grid%velocity(i1,i2,i3,vtype)+vel_source)
        dist=sqrt((s%r-grid%r(i1))**2 + (s%r*(s%lat-grid%lat(i2)))**2 + &
             (s%r*s%coslat*(s%long-grid%long(i3)))**2 ) 
        reg%arrivaltime(j)= dist/vel_av
        reg%time_gradient(1,j)= (grid%r(i1)-s%r)/(dist*grid%velocity(i1,i2,i3,vtype))
        reg%time_gradient(2,j)= s%r*(grid%lat(i2)-s%lat)/(dist*grid%velocity(i1,i2,i3,vtype))
        reg%time_gradient(3,j)= s%r*s%coslat*(grid%long(i3)-s%long)/ &
             (dist*grid%velocity(i1,i2,i3,vtype))

        reg%node_status(j)=1   
     
     else                          ! node is an interface node

        if (i2 == ibot%id) then   ! node lies on the bottom interface of the region

           j=ibot%rabo_node_id(i3)
           dist=sqrt((s%r-ibot%r(i3))**2 + (s%r*(s%lat-ibot%lat(i3)))**2 + &
                (s%r*s%coslat*(s%long-ibot%long(i3)))**2 )
           reg%arrivaltime(j)= dist/ibot%vel_top(i3,vtype)
           reg%time_gradient(1,j)= (ibot%r(i3)-s%r)/(dist*ibot%vel_top(i3,vtype))
           reg%time_gradient(2,j)= s%r*(ibot%lat(i3)-s%lat)/(dist*ibot%vel_top(i3,vtype))
           reg%time_gradient(3,j)= s%r*s%coslat*(ibot%long(i3)-s%long)/(dist*ibot%vel_top(i3,vtype))

        else                             ! node lies on the top interface of the region

           j=itop%rbel_node_id(i3)
           dist=sqrt((s%r-itop%r(i3))**2 + (s%r*(s%lat-itop%lat(i3)))**2 + &
                (s%r*s%coslat*(s%long-itop%long(i3)))**2 )
           reg%arrivaltime(j)= dist/itop%vel_bot(i3,vtype)
           reg%time_gradient(1,j)= (itop%r(i3)-s%r)/(dist*itop%vel_bot(i3,vtype))
           reg%time_gradient(2,j)= s%r*(itop%lat(i3)-s%lat)/(dist*itop%vel_bot(i3,vtype))
           reg%time_gradient(3,j)= s%r*s%coslat*(itop%long(i3)-s%long)/(dist*itop%vel_bot(i3,vtype))

        endif

        reg%node_status(j)=1

     endif

     end do

  endif

!  print *,'region',reg%id,' starting times set'

!  print *,'calling propagate in sweep region from source'


!-------------------------------------------------------------------------------------
! do the fast marching sweep

  call propagate(reg,vtype)

  print *,'propagation through region',reg%id,' finished'

  return

end subroutine sweep_region_from_source


!*****************************************************************************************************
!----------------------------------------------------------------------------------------------------

! This subroutine initializes the source in the refined source grid and its intersections

subroutine initialize_refined_source(s,sc,grid,reg,itop,ibot)

  use mod_3dfm_nointerfaces
  implicit none

  integer       :: n,m,i,j,k,is,js,ks,iface
  type(Tsource) :: s,sc
  type(Tpropagation_grid) :: grid
  type(Tintersection),target   :: itop,ibot
  type(Tregion)         :: reg
  type(Tintersection),pointer   :: isec
  real(kind=dp) :: dist



! copy position from coarse source
  s%r=sc%r
  s%lat=sc%lat
  s%long=sc%long
  s%coslat=sc%coslat
  s%region_id=reg%id

! determine grid cell in which the source is located

  s%ir    =  floor((s%r - grid%r0)/grid%dr0 + 1)
  s%ilat  =  floor((s%lat - grid%lat0)/grid%dlat0 + 1)
  s%ilong =  floor((s%long - grid%long0)/grid%dlong0 + 1)

  print *,'s%ir,s%ilat,s%ilong= ',s%ir,s%ilat,s%ilong
  print *,'sc%on_interface= ',sc%on_interface
  print *,'itop%id,ibot%id= ',itop%id,ibot%id
  print *,'sc%topint_id,sc%botint_id= ',sc%topint_id,sc%botint_id

! correct if source lies exactly on grid boundary 
 
  s%ir = min(s%ir,grid%nr-1)
  s%ilat = min(s%ilat,grid%nlat-1)
  s%ilong = min(s%ilong,grid%nlong-1)

  s%on_grid=.false.
  s%on_interface=sc%on_interface
  s%n_cnode = 0

  allocate(s%first_tf_up(n_vtypes),s%first_tf_down(n_vtypes))

!-----------------------------------------


! test where the source lies

  if (s%on_interface) then

     if (sc%topint_id == ibot%id ) then
        s%interface_id = ibot%id
        isec => ibot
     else if (sc%botint_id == itop%id) then
        s%interface_id = itop%id
        isec => itop
     else
        stop 'illegal source interface in initialize refined source'
     endif


 !     print *,'isec%id',n,isec%id,isec%iface_id

     iface=isec%iface_id


        ! test if the source lies exactly on an intersection node

     do m=1,isec%nnode

        dist =sqrt((s%r-isec%r(m))**2 +(s%r*(s%lat-isec%lat(m)))**2 + &
             (s%r*s%coslat*(s%long-isec%long(m)))**2 )

        if (abs(dist) < grid%tolerance) then  ! source lies exactly on an interface node

           ! assign time value 0 to the source node

           s%n_cnode = s%n_cnode + 1
           s%cnode(s%n_cnode)%i1=0
           s%cnode(s%n_cnode)%i2=isec%id
           s%cnode(s%n_cnode)%i3=m
           s%on_grid=.true.
           print *,'source lies on node ',m,' of intersection ',isec%id

           exit

        endif
     end do


     if (.not. s%on_grid) then   ! source lies on interface but not on an interface node

        print *,'source lies exactly on interface',iface,'but not on a node'

        !  find the nodes of intersection that are part of the cell containing the source

        m= grid%ccind_from_3dc(s%ir,s%ilat,s%ilong)%p(iface)
        print *,m,isec%n_inodes(m)
        do i=1,isec%n_inodes(m)
              
           k=isec%inodes(i,m)
           s%n_cnode = s%n_cnode + 1
           s%cnode(s%n_cnode)%i1=0
           s%cnode(s%n_cnode)%i2=isec%id
           s%cnode(s%n_cnode)%i3=k

        end do
        print *, 'alpha'
        ! the regular nodes of the cut cell containing the source


        do i=0,1
           is=s%ir+i
           do j=0,1
              js=s%ilat+j
              do k=0,1
                 ks=s%ilong+k
                 if (is > 0 .and. is <= grid%nr .and. &
                      js > 0 .and. js <= grid%nlat .and. &
                      ks > 0 .and. ks <= grid%nlong .and. &
                      grid%node_region(is,js,ks) == s%region_id) then
                    s%n_cnode = s%n_cnode + 1
                    s%cnode(s%n_cnode)%i1=is
                    s%cnode(s%n_cnode)%i2=js
                    s%cnode(s%n_cnode)%i3=ks
                 endif
              end do
           end do
        end do


     end if

  endif

! test if the source lies exactly on a regular grid node or not

  if (.not. s%on_interface) then

     do i=0,1
        do j=0,1
           do k=0,1

!              print *,s%r,s%lat,s%long
!              print *,s%ir+i,s%ilat+j,s%ilong+k
!              print *,grid%r(s%ir+i),grid%lat(s%ilat+j),grid%long(s%ilong+k)
!              print *

              dist=sqrt((s%r-grid%r(s%ir+i))**2 + (s%r*(s%lat-grid%lat(s%ilat+j)))**2 &
                   + (s%r*s%coslat*(s%long-grid%long(s%ilong+k)))**2 ) 
!              print *,i,j,k,dist
              if (dist < grid%tolerance) then



                 s%on_grid=.true.
                 s%n_cnode = s%n_cnode + 1
                 s%cnode(s%n_cnode)%i1=s%ir+i
                 s%cnode(s%n_cnode)%i2=s%ilat+j
                 s%cnode(s%n_cnode)%i3=s%ilong+k
                 print *,'source on grid but not interface'

              endif

           end do
        end do
     end do
 
             
     if (.not. s%on_grid) then

        print *,'source does not lie on grid'

! test if the cell in which the source resides is cut

        if (associated(grid%ccind_from_3dc(s%ir,s%ilat,s%ilong)%p)) then

           print *,'source lies in a cut cell'

        ! if so, make a list of the nodes in the cut cell

        ! first the intersection nodes

           do n=1,2

              if (n == 1) isec => itop
              if (n == 2) isec => ibot

              if (grid%ccind_from_3dc(s%ir,s%ilat,s%ilong)%p(isec%iface_id) /= 0) then

                 m= grid%ccind_from_3dc(s%ir,s%ilat,s%ilong)%p(isec%iface_id)
                 do i=1,isec%n_inodes(m)
              
                    k=isec%inodes(i,m)
                    s%n_cnode = s%n_cnode + 1
                    s%cnode(s%n_cnode)%i1=0
                    s%cnode(s%n_cnode)%i2=isec%id
                    s%cnode(s%n_cnode)%i3=k
                 
                 end do
              endif
           end do

        !then the regular nodes

           do i=-1,2
              is=s%ir+i
              do j=-1,2
                 js=s%ilat+j
                 do k=-1,2
                    ks=s%ilong+k
                    if (is > 0 .and. is <= grid%nr .and. &
                         js > 0 .and. js <= grid%nlat .and. &
                         ks > 0 .and. ks <= grid%nlong .and. &
                         grid%node_region(is,js,ks) == s%region_id) then
                       s%n_cnode = s%n_cnode + 1
                       s%cnode(s%n_cnode)%i1=is
                       s%cnode(s%n_cnode)%i2=js
                       s%cnode(s%n_cnode)%i3=ks
!                       print '(6i5,i8)',i,j,k,s%cnode(s%n_cnode)%i1,is,js,ks,grid%rnode_id(is,js,ks)
                    endif
                 end do
              end do
           end do

!           do i=0,1
!              do j=0,1
!                 do k=0,1
!                    if (grid%node_region(s%ir+i,s%ilat+j,s%ilong+k) == s%region_id) then
!                       s%n_cnode = s%n_cnode + 1
!                       s%cnode%i1=s%ir+i
!                       s%cnode%i2=s%ilat+j
!                       s%cnode%i3=s%ilong+k
!                    endif
!                 end do
!              end do
!           end do

        else   ! the cell in which the source lies is not cut

        ! make a list of the nodes in the regular cell

           print *,'source lies in an uncut cell'

           do i=-1,2
              is=s%ir+i
              do j=-1,2
                 js=s%ilat+j
                 do k=-1,2
                    ks=s%ilong+k
                    if (is > 0 .and. is <= grid%nr .and. &
                         js > 0 .and. js <= grid%nlat .and. &
                         ks > 0 .and. ks <= grid%nlong .and. &
                         grid%node_region(is,js,ks) == s%region_id) then
                       s%n_cnode = s%n_cnode + 1
                       s%cnode(s%n_cnode)%i1=is
                       s%cnode(s%n_cnode)%i2=js
                       s%cnode(s%n_cnode)%i3=ks
!                       print '(6i5,i8)',i,j,k,s%cnode(s%n_cnode)%i1,is,js,ks,grid%rnode_id(is,js,ks)
                    endif
                 end do
              end do
           end do
        endif    ! cut cell or else

     endif  ! if not on grid

  endif  ! if not on interface

!  print *,'source nodes'
!  do n=1,s%n_cnode
!     print '(4i5)',n,s%cnode(n)%i1,s%cnode(n)%i2,s%cnode(n)%i3
!  end do

  return

end subroutine initialize_refined_source



!-----------------------------------------------------------------------------------------------------

! This is an alternative refined source initialization (currently not used)
! Initializes a larger region region around the source with simple analytical estimates

subroutine initialize_refined_source2(s,sc,grid,reg,itop,ibot)

  use mod_3dfm_nointerfaces
  implicit none

  integer       :: n,m,i,j,k,is,js,ks,source_node
  type(Tsource) :: s,sc
  type(Tpropagation_grid) :: grid
  type(Tintersection),target   :: itop,ibot
  type(Tregion)         :: reg
  type(Tintersection),pointer   :: isec
  real(kind=dp) :: dist,interpolate_interface



! copy position from coarse source
  s%r=sc%r
  s%lat=sc%lat
  s%long=sc%long
  s%region_id=reg%id

! determine grid cell in which the source is located

  s%ir    =  floor((s%r - grid%r0)/grid%dr0 + 1)
  s%ilat  =  floor((s%lat - grid%lat0)/grid%dlat0 + 1)
  s%ilong =  floor((s%long - grid%long0)/grid%dlong0 + 1)

!  if (s%ir < 1 .or. s%ir>grid%nr-1) stop 'source outside propagation grid'
!  if (s%ilat < 1 .or. s%ilat>grid%nlat-1) stop 'source outside propagation grid'
!  if (s%ilong < 1 .or. s%ilong>grid%nlong-1) stop 'source outside propagation grid'


  s%on_grid=.false.
  s%on_interface=.false.
  s%n_cnode = 0


!-----------------------------------------


! test where the source lies

  do n=1,2

     if (n == 1) isec => itop
     if (n == 2) isec => ibot

!     print *,'isec%id',n,isec%id,isec%iface_id


     if (isec%nnode /= 0) then  ! intersection corresponds to a real interface 

!     print *,n,s%r,interpolate_interface(s%lat,s%long,intrface(iface))

        dist = s%r-interpolate_interface(s%lat,s%long,intrface(isec%iface_id))


! first test if the source lies exactly on the interface

        if (abs(dist) < grid%tolerance) then  ! source lies on interface

           s%interface_id = isec%iface_id
           s%on_interface=.true.
           print *,'source lies exactly on interface',isec%id

        ! test if the source lies exactly on an intersection node

           do m=1,isec%nnode

              dist =sqrt((s%r-isec%r(m))**2 +(s%r*(s%lat-isec%lat(m)))**2 + &
                   (s%r*s%coslat*(s%long-isec%long(m)))**2 )

              if (dist < grid%tolerance) then  ! source lies exactly on an interface node

              ! assign time value 0 to the source node

                 s%n_cnode = s%n_cnode + 1
                 s%cnode(s%n_cnode)%i1=0
                 s%cnode(s%n_cnode)%i2=isec%id
                 s%cnode(s%n_cnode)%i3=m
                 s%on_grid=.true.

                 if (n == 1) source_node=isec%rbel_node_id(m)
                 if (n == 2) source_node=isec%rabo_node_id(m)

                 call get_source_neighbours(source_node,s,reg,grid)                 

                 print *,'source lies on node ',m,' of intersection ',isec%id
                 exit

              endif
           end do

           if (.not. s%on_grid) then   ! source lies on interface but not on an interface node

           !  find the nodes of intersection that are part of the cell containing the source

              m= grid%ccind_from_3dc(s%ir,s%ilat,s%ilong)%p(n)
              do i=1,isec%n_inodes(m)
              
                 k=isec%inodes(i,m)
                 s%n_cnode = s%n_cnode + 1
                 s%cnode(s%n_cnode)%i1=0
                 s%cnode(s%n_cnode)%i2=isec%id
                 s%cnode(s%n_cnode)%i3=k

              end do
           end if

           exit ! we don't need to test other interfaces if the source lies exactly on the present one

        endif    ! test for source on interface

     endif  ! test for real intersection

  end do



! test if the source lies exactly on a regular grid node or not

  if (.not. s%on_interface) then

     do i=0,1
        do j=0,1
           do k=0,1

!              print *,s%r,s%lat,s%long
!              print *,grid%r(s%ir+i),grid%r(s%ilat++j),grid%long(s%ilong+k)
!              print *

              dist=sqrt((s%r-grid%r(s%ir+i))**2 + (s%r*(s%lat-grid%lat(s%ilat+j)))**2 &
                   + (s%r*s%coslat*(s%long-grid%long(s%ilong+k)))**2 ) 
!              print *,i,j,k,dist
              if (dist < grid%tolerance) then

                 s%on_grid=.true.
                 is=s%ir+i ; js=s%ilat+j ; ks=s%ilong+k

                 source_node= grid%rnode_id(is,js,ks)
                 write(22,*) 'snode =',is,js,ks
                 call get_source_neighbours(source_node,s,reg,grid)
                 exit

              endif

           end do
        end do
     end do
 
             
     if (.not. s%on_grid) then

! test if the cell in which the source resides is cut

        if (associated(grid%ccind_from_3dc(s%ir,s%ilat,s%ilong)%p)) then

        ! if so, make a list of the nodes in the cut cell

        ! first the intersection nodes

           do n=1,2

              if (n == 1) isec => itop
              if (n == 2) isec => ibot

              if (grid%ccind_from_3dc(s%ir,s%ilat,s%ilong)%p(isec%iface_id) /= 0) then

                 m= grid%ccind_from_3dc(s%ir,s%ilat,s%ilong)%p(isec%iface_id)
                 do i=1,isec%n_inodes(m)
              
                    k=isec%inodes(i,m)
                    s%n_cnode = s%n_cnode + 1
                    s%cnode(s%n_cnode)%i1=0
                    s%cnode(s%n_cnode)%i2=isec%id
                    s%cnode(s%n_cnode)%i3=k
                 
                 end do
              endif
           end do

        !then the regular nodes

           do i=0,1
              do j=0,1
                 do k=0,1
                    if (grid%node_region(s%ir+i,s%ilat+j,s%ilong+k) == s%region_id) then
                       s%n_cnode = s%n_cnode + 1
                       s%cnode%i1=s%ir+i
                       s%cnode%i2=s%ilat+j
                       s%cnode%i3=s%ilong+k
                    endif
                 end do
              end do
           end do

        else   ! the cell in which the source lies is not cut

        ! make a list of the nodes in the regular cell

           do i=0,1
              do j=0,1
                 do k=0,1

                    s%n_cnode = s%n_cnode + 1
                    s%cnode(s%n_cnode)%i1=s%ir+i
                    s%cnode(s%n_cnode)%i2=s%ilat+j
                    s%cnode(s%n_cnode)%i3=s%ilong+k

                 end do
              end do
           end do
        endif    ! cut cell or else

     endif  ! if not on grid

  endif  ! if not on interface


  do n=1,s%n_cnode
     write(22,*) n,s%cnode(n)%i1,s%cnode(n)%i2,s%cnode(n)%i3
  end do

  return

end subroutine initialize_refined_source2

!***********************************************************************************************




!***********************************************************************************************

!***********************************************************************************************
! This subroutine takes a source as input, constructs a refined grid around the source 
! sweeps through the refined grid, transfers the
! arrivaltimes from the refined grid regions to the regions in the main grid and does a sweep 
! through these regions in the upward and downward directions.

subroutine initialize_source_regions(s)
use mod_3dfm
implicit none

type(Tsource)       :: s  ! the source with its properties on the main grid
type(Tsource)       :: ss ! the source with its properties on the refined grid

type(Tintersection),pointer :: itop,ibot    ! top and bottom intersections of refined grid
type(Tintersection),pointer :: itopc,ibotc  ! top and bottom intersections of main grid
type(Tregion),pointer       :: reg          ! the source region in the main grid
type(Tregion),pointer       :: sreg         ! the refined source region 
type(Tregion)               :: tst !AAA

! local stuff
integer :: n,m,i,j,k,i1,i2,i3,prev_tf,nstart
integer :: nrmax,nrmin,nlatmax,nlatmin,nlongmax,nlongmin,vtype
real(kind=dp)  :: rmin,rmax,latmin,latmax,longmin,longmax,t_short
integer        ::t1,t2

call system_clock(t1)

! first construct the refined grid around the source

allocate(sgrid)
call pgrid_defaults(sgrid)

sgrid%is_source_grid = .true.

! limits of volume of main grid to be refined, taking main grid boundaries into account

i =  nint((s%r - pgrid%r0)/pgrid%dr0 + 1)
j =  nint((s%lat - pgrid%lat0)/pgrid%dlat0 + 1)
k =  nint((s%long - pgrid%long0)/pgrid%dlong0 + 1)

nrmax = min(pgrid%nr , i + ncell_to_be_refined)
nrmin = max( 1 , i - ncell_to_be_refined)
nlatmax = min(pgrid%nlat , j + ncell_to_be_refined)
nlatmin = max( 1 , j - ncell_to_be_refined)
nlongmax = min(pgrid%nlong , k + ncell_to_be_refined)
nlongmin = max( 1 , k - ncell_to_be_refined)

! origin of the refined source grid in propagation grid index coordinates

sgrid%index_r0 = nrmin
sgrid%index_lat0 = nlatmin
sgrid%index_long0 = nlongmin

! calculate the refined source grid parameters

sgrid%nr      = (nrmax - nrmin)*refinement_factor + 1
sgrid%nlat    = (nlatmax - nlatmin)*refinement_factor + 1
sgrid%nlong   = (nlongmax - nlongmin)*refinement_factor + 1
sgrid%dr0     = pgrid%dr0/refinement_factor
sgrid%dlat0   = pgrid%dlat0/refinement_factor
sgrid%dlong0  = pgrid%dlong0/refinement_factor
sgrid%r0      = pgrid%r(s%ir) - (s%ir - nrmin)*pgrid%dr0
sgrid%lat0    = pgrid%lat(s%ilat) - (s%ilat - nlatmin)*pgrid%dlat0
sgrid%long0   = pgrid%long(s%ilong) - (s%ilong - nlongmin)*pgrid%dlong0


! initialize the grid coordinates

allocate(sgrid%r(sgrid%nr),sgrid%lat(sgrid%nlat),sgrid%long(sgrid%nlong),sgrid%coslat(sgrid%nlat))

do i=1,sgrid%nr
   sgrid%r(i)=sgrid%r0 + (i-1)*sgrid%dr0
end do

do i=1,sgrid%nlat
   sgrid%lat(i)=sgrid%lat0 + (i-1)*sgrid%dlat0
end do

do i=1,sgrid%nlat
   sgrid%coslat(i)=cos(sgrid%lat(i))
end do


do i=1,sgrid%nlong
   sgrid%long(i)=sgrid%long0 + (i-1)*sgrid%dlong0
end do

sgrid%tolerance = refinement_factor*interface_tolerance*sgrid%dr0


! allocate storage for the refined source grid, intersections and regions

allocate(sgrid%rnode_id(sgrid%nr,sgrid%nlat,sgrid%nlong))
sgrid%rnode_id=0
allocate(sgrid%node_region(sgrid%nr,sgrid%nlat,sgrid%nlong))
sgrid%node_region=0
allocate(sgrid%ccind_from_3dc(sgrid%nr,sgrid%nlat,sgrid%nlong))
do k=1,sgrid%nlong
   do j=1,sgrid%nlat
      do i=1,sgrid%nr
         nullify(sgrid%ccind_from_3dc(i,j,k)%p)
      end do
   end do
end do

allocate(sgrid%arrivaltime(sgrid%nr,sgrid%nlat,sgrid%nlong))
sgrid%arrivaltime = huge_time

n_sintersections=n_intersections
allocate(sintersection(n_sintersections))
do i=1,n_sintersections ; call intersection_defaults(sintersection(i)) ; sintersection(i)%id=i ; end do
n_sregions=n_regions
allocate(sregion(n_sregions))
do i=1,n_sregions ; call region_defaults(sregion(i)) ; sregion(i)%id=i ; end do

do n=1,n_sintersections
   call find_intersection(sintersection(n),intrface(n),sgrid)
   if (sintersection(n)%nnode > 0) then
      allocate(sintersection(n)%arrivaltime(sintersection(n)%nnode))
      allocate(sintersection(n)%time_gradient(3,sintersection(n)%nnode))
   endif
end do
print *,'intersections found'


! set a flag at nodes on the grid that are completely regular, i.e. none of the connected cells
! has irregular nodes

call tag_regular_nodes(sgrid)
print *,'nodes of refined source grid tagged'



do n=1,n_sregions

     sregion(n)%id=n

! pointers to the intersections that are the boundaries of the region
     sregion(n)%itop => sintersection(n)
     sregion(n)%ibot => sintersection(n+1)
     sregion(n)%ivgrid=region(n)%ivgrid

     if (sregion(n)%itop%nnode == 0 .and.sregion(n)%ibot%nnode == 0 .and. &
          sregion(n)%id /= s%region_id ) then

        print *,'sregion',n,'does not exist in refined source grid'
        sregion(n)%nnode = 0
        cycle

     endif

   call define_region(sregion(n),sintersection(n),sintersection(n+1),sgrid)

   print *,'refined source region',n,' defined, nnode =',sregion(n)%nnode

end do


! transfer the velocity values to all refined grid nodes

call velocities_to_grid(sgrid)

do n=1,n_sintersections
   if (sintersection(n)%nnode /= 0) call velocities_to_intersection(sintersection(n))
end do
do n=1,n_sregions
  if (sregion(n)%nnode > 0)  call velocities_to_region(sregion(n),sgrid)
end do

print *,'refined velocities transferred'



!!------------------------------------------------------------------------------------------------
! determine the paths up and down from the source region covering the main regions overlapping 
! with the refined source region
!------------------------------------------------------------------------------------------------

! set default values for the intialization paths

do i=1,2
   do j=1,2
      call path_defaults(s%init_path(i,j))
      s%init_path(i,j)%used = .false.
   end do
end do

! first construct the path up if it exists

do vtype=1,n_vtypes

! ! first construct the path up if it exists ( init_path(1,vtype) )

if (.not.(s%on_interface .and. s%topint_id == 1)) then

!   print *,'constructing source sequence up'

   if (s%on_interface) then
      nstart = s%topreg_id - 1
   else
      nstart = s%region_id - 1
   endif

   s%init_path(1,vtype)%n_tf = 1
   do n=nstart,1,-1     ! count the number of timefields up
      if (sregion(n)%nnode > 0) then
         s%init_path(1,vtype)%n_tf = s%init_path(1,vtype)%n_tf + 1
      endif
   end do

   allocate(s%init_path(1,vtype)%sequence(2*s%init_path(1,vtype)%n_tf))
   allocate(s%init_path(1,vtype)%tf_sequence(s%init_path(1,vtype)%n_tf))

   s%init_path(1,vtype)%id  = 0
   s%init_path(1,vtype)%valid= .true.
   s%init_path(1,vtype)%used = .true.
   s%init_path(1,vtype)%refstep = 0
   s%init_path(1,vtype)%fitting_interface = 0

   s%init_path(1,vtype)%sequence(1) = 0
   if (s%on_interface) then
      s%init_path(1,vtype)%sequence(2) = region(s%topreg_id)%itop%iface_id
   else
      s%init_path(1,vtype)%sequence(2) = region(s%region_id)%itop%iface_id
   endif

! construct the sequence of the first path
   if (s%init_path(1,vtype)%n_tf > 1) then   
      do i=2,s%init_path(1,vtype)%n_tf
         s%init_path(1,vtype)%sequence(2*i-1) = s%init_path(1,vtype)%sequence(2*i-2)
         s%init_path(1,vtype)%sequence(2*i) = s%init_path(1,vtype)%sequence(2*i-1) - 1
      end do
   endif

endif


! ! then construct the path down if it exists ( init_path(2,vtype) )

if (.not.(s%on_interface .and. s%botint_id == n_interfaces)) then

!   print *,'constructing source sequence down'

   if (s%on_interface) then
      nstart = s%botreg_id + 1
   else
      nstart = s%region_id + 1
   endif

   s%init_path(2,vtype)%n_tf = 1
   do n=nstart,n_sregions     ! count the number of timefields down
      if (sregion(n)%nnode > 0) then
         s%init_path(2,vtype)%n_tf = s%init_path(2,vtype)%n_tf + 1
      endif
   end do

   allocate(s%init_path(2,vtype)%sequence(2*s%init_path(2,vtype)%n_tf))
   allocate(s%init_path(2,vtype)%tf_sequence(s%init_path(2,vtype)%n_tf))

   s%init_path(2,vtype)%id  = 0
   s%init_path(2,vtype)%valid= .true.
   s%init_path(2,vtype)%used = .true.
   s%init_path(2,vtype)%refstep = 0
   s%init_path(2,vtype)%fitting_interface = 0

   s%init_path(2,vtype)%sequence(1) = 0
   if (s%on_interface) then
      s%init_path(2,vtype)%sequence(2) = region(s%botreg_id)%ibot%iface_id
   else
      s%init_path(2,vtype)%sequence(2) = region(s%region_id)%ibot%iface_id
   endif

! construct the sequence of the second path
   if (s%init_path(2,vtype)%n_tf > 1) then   
      do i=2,s%init_path(2,vtype)%n_tf
         s%init_path(2,vtype)%sequence(2*i-1) = s%init_path(2,vtype)%sequence(2*i-2)
         s%init_path(2,vtype)%sequence(2*i) = s%init_path(2,vtype)%sequence(2*i-1) + 1
      end do
   endif

endif

end do   ! vtypes

! paths through the regions overlapping with the source region have been defined


! do the initialization for the number of velocity types in the problem

do vtype=1,n_vtypes

print *,'--------------------------------------------------'
print *,'starting source initialization for vtype =',vtype

!--------------------------------------------------------
! Do the propagation through the source regions first

! if the source lies on an interface initialize both regions above and below, else only source region

do n=1,s%n_tf_init
   print *, 'source number ',n
   if (s%on_interface) then   ! initialize the regions above and below the interface
      print *, 'Is on interface'
      if (s%n_tf_init == 2) then
         print *, 's%n_tf_init= 2'
         if (n==1) sreg => sregion(s%topreg_id)
         if (n==2) sreg => sregion(s%botreg_id)
      else
!         print *, 's%n_tf_init= ',s%n_tf_init
!         print *, 's%topreg_id,s%botreg_id = ',s%topreg_id,s%botreg_id
         !print *, 'sregion(s%topreg_id),sregion(s%botreg_id)=',sregion(s%topreg_id),sregion(s%botreg_id)
         if (s%topreg_id > 0) sreg => sregion(s%topreg_id)
         if (s%botreg_id <= n_regions) sreg => sregion(s%botreg_id) !Made this a <= instead of < otherwise you may get sreg that is never assigned 
      endif
!      print *,'Region IDs assigned to source.' !AAA
   else                       ! only he one region in which the source lies
      sreg => sregion(s%region_id)
   endif
!   print *,associated(sreg)
   print *,'sreg%id= ',sreg%id
!   print *,'alpha' !AAA
   itop => sintersection(sreg%id) ! top intersection of the refined source region
   ibot => sintersection(sreg%id+1)   ! bottom intersection of the refined source region

   ! initialize the source in the refined source grid.
   print *,'Calling initialize_refined_source subroutine'
   call initialize_refined_source(ss,s,sgrid,sreg,itop,ibot)

   print *,'refined source initialized in sregion',sreg%id


   ! sweep the refined source grid

   call sweep_region_from_source(sreg,ss,vtype)

   print *,'refined source region',sreg%id,' swept'


   ! transfer regional travel times to interfaces and regular grid

!  if (itop%nnode > 0 .and. .not. associated(itop%arrivaltime)) allocate(itop%arrivaltime(itop%nnode))
!  if (ibot%nnode > 0 .and. .not. associated(ibot%arrivaltime)) allocate(ibot%arrivaltime(ibot%nnode))


   do i=1,sreg%nnode

      i1 = sreg%node(i)%i1 ; i2 = sreg%node(i)%i2 ; i3 = sreg%node(i)%i3

      if (i1 == 0) then

         sintersection(i2)%arrivaltime(i3) = sreg%arrivaltime(i)
         sintersection(i2)%time_gradient(1:3,i3) = sreg%time_gradient(1:3,i)

      else

         sgrid%arrivaltime(i1,i2,i3)= sreg%arrivaltime(i)

      endif

   end do

enddo



!--------------------------------------------------------------------------------
! now sweep up and down through the remaining regions of the refined source grid

! sweep up

if (.not.s%on_interface .or. (s%on_interface .and. s%topreg_id > 1)) then

   if (s%on_interface) then
      nstart = s%topreg_id - 1
   else
      nstart = s%region_id - 1
   endif

   do n=nstart,1,-1

      sreg => sregion(n)

      if (sreg%nnode > 0) then

         call refract_gradient(sintersection(n+1),sregion(sreg%id+1),vtype,1)

         call sweep_sregion_from_interface(sreg,sintersection(n+1),vtype)

         print *,'refined region',n,' swept'

! transfer regional travel times to interfaces 

      
         do i=1,sreg%nnode

            i1 = sreg%node(i)%i1 ; i2 = sreg%node(i)%i2 ; i3 = sreg%node(i)%i3

            if (i1 == 0) then

               sintersection(i2)%arrivaltime(i3) = sreg%arrivaltime(i)
               sintersection(i2)%time_gradient(1:3,i3) = sreg%time_gradient(1:3,i)
            else

               sgrid%arrivaltime(i1,i2,i3)= sreg%arrivaltime(i)

            endif

         end do

      endif

   end do

endif


! sweep down

if (.not.s%on_interface .or. (s%on_interface .and. s%botreg_id < n_regions)) then

   if (s%on_interface) then
      nstart = s%botreg_id + 1
   else
      nstart = s%region_id + 1
   endif

   do n=nstart,n_sregions

      sreg => sregion(n)

      if (sreg%nnode > 0) then

         print *,'refracting at interface',n
         call refract_gradient(sintersection(n),sregion(sreg%id-1),vtype,-1)

         print *,'starting sweep from interface',n,'into region',sreg%id
         call sweep_sregion_from_interface(sreg,sintersection(n),vtype)

         print *,'refined region',n,' swept'

         do i=1,sreg%nnode

            i1 = sreg%node(i)%i1 ; i2 = sreg%node(i)%i2 ; i3 = sreg%node(i)%i3

            if (i1 == 0) then

!               write(1,*) i,i1,i2,i3 

               sintersection(i2)%arrivaltime(i3) = sreg%arrivaltime(i)
               sintersection(i2)%time_gradient(1:3,i3) = sreg%time_gradient(1:3,i)


            else

!               write(1,*) i,i1,i2,i3

               sgrid%arrivaltime(i1,i2,i3)= sreg%arrivaltime(i)

            endif

         end do
         
      endif

   end do

endif

call system_clock(t2)
print *,'refined grid for source',s%id,'took', real(t2-t1)/10000.,' sec'
print *

!-------------------------------------------------------------------------------------------------
! transfer the results to the region(s) in which the source resides on the main propagation grid

! first find shortest traveltime on refined region boundary

t_short=huge_time

rmin=pgrid%r(1)+pgrid%tolerance
rmax=pgrid%r(pgrid%nr)-pgrid%tolerance
latmin=pgrid%lat(1)+pgrid%tolerance/pgrid%dr0
latmax=pgrid%lat(pgrid%nlat)-pgrid%tolerance/pgrid%dr0
longmin=pgrid%long(1)+pgrid%tolerance/pgrid%dr0
longmax=pgrid%long(pgrid%nlong)-pgrid%tolerance/pgrid%dr0

if (sgrid%r(1) > rmin)  &
     t_short = min(t_short,minval(sgrid%arrivaltime(1,1:sgrid%nlat,1:sgrid%nlong)))
if (sgrid%r(sgrid%nr) < rmax) &
     t_short = min(t_short,minval(sgrid%arrivaltime(sgrid%nr,1:sgrid%nlat,1:sgrid%nlong)))
if (sgrid%lat(1) > latmin) &
     t_short = min(t_short,minval(sgrid%arrivaltime(1:sgrid%nr,1,1:sgrid%nlong)))
if (sgrid%lat(sgrid%nlat) < latmax) &
     t_short = min(t_short,minval(sgrid%arrivaltime(1:sgrid%nr,sgrid%nlat,1:sgrid%nlong)))
if (sgrid%long(1) > longmin) &
     t_short = min(t_short,minval(sgrid%arrivaltime(1:sgrid%nr,1:sgrid%nlat,1)))
if (sgrid%long(sgrid%nlong) < longmax) &
     t_short = min(t_short,minval(sgrid%arrivaltime(1:sgrid%nr,1:sgrid%nlat,sgrid%nlong)))

print *,'shortest time on refined grid boundary is',t_short



! transfer the refined source region(s) to the main propagation grid source region(s)
! if the source lies on an interface initialize both regions above and below, else only source region

do n=1,2

   if (s%init_path(n,vtype)%used) then

      if (s%on_interface) then   ! initialize the regions above and below the interface

         if (n==1) then ; sreg => sregion(s%topreg_id) ; reg => region(s%topreg_id) ; endif
         if (n==2) then ; sreg => sregion(s%botreg_id) ; reg => region(s%botreg_id) ; endif

      else                       ! only the one region in which the source lies

         sreg => sregion(s%region_id) ; reg => region(s%region_id)

      endif

      itopc => intersection(reg%id)      ! the top intersection of the normal source region
      ibotc => intersection(reg%id+1)    ! the bottom intersection of the normal source region
      allocate(reg%arrivaltime(reg%nnode),reg%time_gradient(3,reg%nnode),reg%node_status(reg%nnode))
      reg%node_status=-1
      reg%arrivaltime = huge_time


      print *,'before transefr refined region'

      call transfer_refined_region(sreg,reg,t_short)

      print *,'transferred times from fine grid to coarse for region',reg%id


   ! create a narrow band around the nodes transferred from the fine grid

      call create_narrow_band(reg,vtype)

      print *,'narrow band created in region',reg%id,'alive/nb',count(reg%node_status == 0),&
           count(reg%node_status == 1)

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
      
  ! transfer time field to the array of saved time fields

      s%n_time_fields=s%n_time_fields+1
      allocate(s%time_field(s%n_time_fields)%arrivaltime(reg%nnode))
      s%time_field(s%n_time_fields)%arrivaltime=reg%arrivaltime
      allocate(s%time_field(s%n_time_fields)%time_gradient(3,reg%nnode))
      s%time_field(s%n_time_fields)%time_gradient=reg%time_gradient

      print *,'results written to timefield',s%n_time_fields

  ! pointer to source region

      s%time_field(s%n_time_fields)%reg =>  reg 

      s%time_field(s%n_time_fields)%vtype =  vtype

      s%init_path(n,vtype)%tf_sequence(1) = s%n_time_fields

      if (s%on_interface) then
         if (reg%id == s%topreg_id) then
            s%time_field(s%n_time_fields)%istart => intersection(s%topint_id)
            s%time_field(s%n_time_fields)%inonstart =>  intersection(s%topint_id - 1)
         else
            s%time_field(s%n_time_fields)%istart => intersection(s%botint_id)
            s%time_field(s%n_time_fields)%inonstart =>  intersection(s%botint_id + 1)
         endif
      else
         s%time_field(s%n_time_fields)%istart => ibotc
         s%time_field(s%n_time_fields)%inonstart =>  itopc
      endif

  ! register the first (source) time fields with the associated source

      if (n == 1) s%first_tf_up(vtype) = s%n_time_fields
      if (n == 2) s%first_tf_down(vtype) = s%n_time_fields


! deallocate  everything that is no longer required

      deallocate(reg%arrivaltime,reg%time_gradient,reg%node_status)


  ! if the source does not lie on an interface, the first timefields up and down are the same
  ! we do not need to do the direction down (loop index n = 2)
      if (.not.s%on_interface) then
         if (n==2) stop 'error n==2 in source region init'
         s%init_path(2,vtype)%tf_sequence(1) = s%n_time_fields         
         s%first_tf_down(vtype) = s%n_time_fields
         exit
      endif


   endif

end do

! timefields 1(/2) describing the source region(s) on the main grid ha(s)(ve) been established
print *,'timefields in main grid regions connected to the source established, vtype=',vtype



!--------------------------------------------------------------------------------------------
! now construct the actual time field sequences on the main grid up (path 1) or down (path 2)
! through the regions overlapping with the refined source grid


! the upward path 1

! only if there are regions above the source region

if (s%init_path(1,vtype)%used .and. s%init_path(1,vtype)%n_tf > 1) then 

   prev_tf = s%first_tf_up(vtype)

   print *,'starting upward sweep in main grid regions overlapping source region'

   do n=2,s%init_path(1,vtype)%n_tf


! transfer the refined source region to the main propagation grid region

      if (s%on_interface) then
         reg => region(s%topreg_id-n+1)
      else
         reg =>  region(s%region_id-n+1)         ! the region in the main propagation grid
      endif

      itopc => reg%itop                       ! the top intersection of the region
      ibotc => reg%ibot                       ! the bottom intersection of the region
      allocate(reg%arrivaltime(reg%nnode),reg%time_gradient(3,reg%nnode),reg%node_status(reg%nnode))
      reg%node_status=-1
      reg%arrivaltime = huge_time

      call transfer_refined_region(sregion(reg%id),reg,t_short)

      print *,'transferred times from fine grid to coarse for region',reg%id


 ! create narrow band around transferred nodes

      call create_narrow_band(reg,vtype)
      print *,'narrow band created in region',reg%id,'alive/nb',count(reg%node_status == 0),&
           count(reg%node_status == 1)


 ! add the intersection nodes to the narrow band

      if (.not. associated(ibotc%arrivaltime)) &
           stop 'requested starting interface does not have arrival times'

      allocate(ibotc%starttime(ibotc%nnode)) 
! start time will be compared to arrival time after propagation to find turning rays


! set start time, time gradient and node status narrow band in regional 
! nodes that belong to the starting interface

      call refract_gradient(ibotc,region(ibotc%iface_id),vtype,1)

      do i=1,ibotc%nnode

         j=ibotc%rabo_node_id(i)
         if (reg%node_status(j) /= 0) then     

    ! only for nodes that did not receive a value from the refined grid

            reg%arrivaltime(j)= ibotc%arrivaltime(i)
            reg%time_gradient(1:3,j)=ibotc%time_gradient(1:3,i)
            reg%node_status(j)=1

         endif
         ibotc%starttime(i)=reg%arrivaltime(j)
      end do

!      print *,'region',reg%id,' starting times set'


! do the fast marching sweep across the main grid region 

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
      
  ! transfer time field to the array of saved time fields

      s%n_time_fields=s%n_time_fields+1
      allocate(s%time_field(s%n_time_fields)%arrivaltime(reg%nnode))
      s%time_field(s%n_time_fields)%arrivaltime=reg%arrivaltime
      allocate(s%time_field(s%n_time_fields)%time_gradient(3,reg%nnode))
      s%time_field(s%n_time_fields)%time_gradient=reg%time_gradient

      print *,'results written to timefield',s%n_time_fields

     ! attach info to the generated time field

      ! time field preceding the current one
      s%time_field(s%n_time_fields)%prev_tf = prev_tf

      ! identify the current time field as the child of the previous time field
      if (prev_tf <= s%n_tf_init) then
         s%time_field(prev_tf)%next_tf(1+(vtype-1)*4) = s%n_time_fields
      else
         s%time_field(prev_tf)%next_tf(1+(vtype-1)*4) = s%n_time_fields
      endif

      ! store the index of the time field in the sequence of timefields for this path
      s%init_path(1,vtype)%tf_sequence(n) = s%n_time_fields

      ! pointers to start and non-start interfaces
      s%time_field(s%n_time_fields)%istart => ibotc
      s%time_field(s%n_time_fields)%inonstart =>  itopc  
      s%time_field(s%n_time_fields)%reg =>  reg
  
      s%time_field(s%n_time_fields)%vtype =  vtype

      prev_tf = s%n_time_fields

     ! check for turning rays
      if (count(ibotc%arrivaltime /= ibotc%starttime) > 0) then
         print *,'turning rays were present on interface',ibotc%iface_id
         s%time_field(s%n_time_fields)%turning_rays_present=.true.
         allocate(s%time_field(s%n_time_fields)%received_turning_ray(ibotc%nnode)) 
         s%time_field(s%n_time_fields)%received_turning_ray = ibotc%arrivaltime /= ibotc%starttime
      endif

! deallocate  everything that is no longer required

      deallocate(reg%arrivaltime,reg%time_gradient,reg%node_status,ibotc%starttime)

   end do  ! end of loop over steps of upward path


endif   ! upward path exist



! the downward path 2

! only if the region below the source region exists

if (s%init_path(2,vtype)%used .and. s%init_path(2,vtype)%n_tf > 1) then

   prev_tf =s%first_tf_down(vtype)

   print *,'starting downward sweep in main grid regions overlapping source region'

   do n=2,s%init_path(2,vtype)%n_tf

      print *,'timefield',n,' of the downward sequence started'

! transfer the refined source region to the main propagation grid region

      if (s%on_interface) then
         reg => region(s%botreg_id+n-1)
      else
         reg =>  region(s%region_id+n-1)         ! the region in the main propagation grid
      endif

      itopc => reg%itop                       ! the top intersection of the region
      ibotc => reg%ibot                       ! the bottom intersection of the region
      allocate(reg%arrivaltime(reg%nnode),reg%time_gradient(3,reg%nnode),reg%node_status(reg%nnode))
      reg%node_status=-1
      reg%arrivaltime = huge_time

      call transfer_refined_region(sregion(reg%id),reg,t_short)

      print *,'transferred times from fine grid to coarse for region',reg%id


 ! create narrow band around transferred nodes

      call create_narrow_band(reg,vtype)
      print *,'narrow band created in region',reg%id,'alive/nb',count(reg%node_status == 0),&
           count(reg%node_status == 1)


 ! add the intersection nodes to the narrow band

      if (.not. associated(itopc%arrivaltime)) &
           stop 'requested starting interface does not have arrival times'

      allocate(itopc%starttime(itopc%nnode))


! set start time, time gradient and node status narrow band in regional 
! nodes that belong to the starting interface

      call refract_gradient(itopc,region(itopc%iface_id-1),vtype,-1)

      do i=1,itopc%nnode

         j=itopc%rbel_node_id(i)
         if (reg%node_status(j) /= 0) then     

       ! only for nodes that did not receive a value from the refined grid

            reg%arrivaltime(j)= itopc%arrivaltime(i)
            reg%time_gradient(1:3,j)=itopc%time_gradient(1:3,i)
            reg%node_status(j)=1

         endif
         itopc%starttime(i)=reg%arrivaltime(j)
      end do


! do the fast marching sweep across the main grid region 

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
      
  ! transfer time field to the array of saved time fields

      s%n_time_fields=s%n_time_fields+1
      allocate(s%time_field(s%n_time_fields)%arrivaltime(reg%nnode))
      s%time_field(s%n_time_fields)%arrivaltime=reg%arrivaltime
      allocate(s%time_field(s%n_time_fields)%time_gradient(3,reg%nnode))
      s%time_field(s%n_time_fields)%time_gradient=reg%time_gradient

      print *,'results written to timefield',s%n_time_fields

     ! attach info to the generated time field

      ! time field preceding the current one
      s%time_field(s%n_time_fields)%prev_tf = prev_tf

      ! identify the current time field as the child of the previous time field
      if (prev_tf <= s%n_tf_init) then
         s%time_field(prev_tf)%next_tf(4+(vtype-1)*4) = s%n_time_fields
!         print *,'timefield',s%n_time_fields,'is ctype 4 of time field',prev_tf
      else
         s%time_field(prev_tf)%next_tf(2+(vtype-1)*4) = s%n_time_fields
!         print *,'timefield',s%n_time_fields,'is ctype 2 of time field',prev_tf
      endif

      ! store the index of the time field in the sequence of timefields for this path
      s%init_path(2,vtype)%tf_sequence(n) = s%n_time_fields

      ! pointers to start and non-start interfaces
      s%time_field(s%n_time_fields)%istart => itopc
      s%time_field(s%n_time_fields)%inonstart =>  ibotc  
      s%time_field(s%n_time_fields)%reg =>  reg  

      s%time_field(s%n_time_fields)%vtype =  vtype

      prev_tf = s%n_time_fields

     ! check for turning rays
      if (count(itopc%arrivaltime /= itopc%starttime) > 0) then
         print *,'turning rays were present on interface',itopc%iface_id
         s%time_field(s%n_time_fields)%turning_rays_present=.true.
         allocate(s%time_field(s%n_time_fields)%received_turning_ray(itopc%nnode)) 
         s%time_field(s%n_time_fields)%received_turning_ray = itopc%arrivaltime /= itopc%starttime
      endif


! deallocate  everything that is no longer required

      deallocate(reg%arrivaltime,reg%time_gradient,reg%node_status,itopc%starttime)

!      print *,'timefield',n,' of the downward sequence finished'

   end do  ! end of loop over steps of downward path

endif   ! downward path exist

end do  ! vtypes

! print *,'finished inside source_reg_init'

do m=1,n_vtypes
do n=1,2
   if (s%init_path(n,m)%used) then
      if (n==1) print '(a45,i3,a1,11i5)','timefields on upward init path for vtype',m,':', &
           s%init_path(n,m)%tf_sequence(1:s%init_path(n,m)%n_tf)
      if (n==2) print '(a45,i3,a1,11i5)','timefields on downward init path for vtype',m,':', &
           s%init_path(n,m)%tf_sequence(1:s%init_path(n,m)%n_tf)
   else
      if (n==1) print *,'there was no upward initialization path for vtype',m 
      if (n==2) print *,'there was no downward initialization path for vtype',m
   endif
end do
end do


! deallocate all storage related to the refined source grid

call clean_grid(sgrid)
deallocate(sgrid)
do n=1,n_sintersections
   call clean_intersection(sintersection(n))
end do
deallocate(sintersection)
do n=1,n_sregions
   call clean_region(sregion(n))
end do
deallocate(sregion)

! stop ' temp stop in initsource'

return

end subroutine initialize_source_regions

!***************************************************************

subroutine clean_grid(grid)

use mod_3dfm
implicit none

integer :: i,j,k

type(Tpropagation_grid) :: grid

if (associated(grid%r)) deallocate(sgrid%r)
if (associated(grid%lat)) deallocate(grid%lat)
if (associated(grid%long)) deallocate(grid%long)
if (associated(grid%arrivaltime)) deallocate(grid%arrivaltime)
if (associated(grid%time_gradient)) deallocate(grid%time_gradient)
if (associated(grid%rnode_id)) deallocate(grid%rnode_id)
if (associated(grid%node_region)) deallocate(grid%node_region)
if (associated(grid%velocity)) deallocate(grid%velocity)
if (associated(grid%coslat)) deallocate(grid%coslat)
if (associated(grid%fully_regular)) deallocate(grid%fully_regular)

if (associated(grid%ccind_from_3dc)) then
   do i=1,grid%nr
      do j=1,grid%nlat
         do k=1,grid%nlong
            if (associated(grid%ccind_from_3dc(i,j,k)%p)) deallocate(grid%ccind_from_3dc(i,j,k)%p)
         end do
      end do
   end do
   deallocate(grid%ccind_from_3dc)
endif

return

end subroutine clean_grid

!*******************************************************************

subroutine clean_intersection(isec)

use mod_3dfm
implicit none

type(Tintersection) :: isec

if (associated(isec%r)) deallocate(isec%r) 
if (associated(isec%lat)) deallocate(isec%lat)
if (associated(isec%coslat)) deallocate(isec%coslat)
if (associated(isec%long)) deallocate(isec%long)
if (associated(isec%normal)) deallocate(isec%normal)
if (associated(isec%vel_top)) deallocate(isec%vel_top)
if (associated(isec%vel_bot)) deallocate(isec%vel_bot)
if (associated(isec%arrivaltime)) deallocate(isec%arrivaltime)
if (associated(isec%starttime)) deallocate(isec%starttime)
if (associated(isec%time_gradient)) deallocate(isec%time_gradient)
if (associated(isec%intype)) deallocate(isec%intype)
if (associated(isec%ccells)) deallocate(isec%ccells)
if (associated(isec%n_inodes)) deallocate(isec%n_inodes)
if (associated(isec%inodes)) deallocate(isec%inodes)
if (associated(isec%ccell_from_inode)) deallocate(isec%ccell_from_inode)
if (associated(isec%rabo_node_id)) deallocate(isec%rabo_node_id)
if (associated(isec%rbel_node_id)) deallocate(isec%rbel_node_id)
if (associated(isec%irg_abo)) deallocate(isec%irg_abo)
if (associated(isec%irg_bel)) deallocate(isec%irg_bel)


end subroutine clean_intersection

!*******************************************************************

subroutine clean_region(reg)

use mod_3dfm
implicit none
type(Tregion)  :: reg


if (associated(reg%node)) deallocate(reg%node) 
if (associated(reg%node_status)) deallocate(reg%node_status)
if (associated(reg%arrivaltime)) deallocate(reg%arrivaltime)
if (associated(reg%time_gradient)) deallocate(reg%time_gradient)
if (associated(reg%velocity)) deallocate(reg%velocity)
if (associated(reg%r)) deallocate(reg%r)
if (associated(reg%lat)) deallocate(reg%lat)
if (associated(reg%coslat)) deallocate(reg%coslat)
if (associated(reg%long)) deallocate(reg%long)
if (associated(reg%init_id)) deallocate(reg%init_id)
if (associated(reg%init_arrivaltime)) deallocate(reg%init_arrivaltime)
if (associated(reg%init_time_gradient)) deallocate(reg%init_time_gradient)

end subroutine clean_region


!-----------------------------------------------------------------------------------------
! this logical function returns true if centernode has neighbours that are not alive
! used to evaluate status of nodes that received an arrival time from the refined source grid

  function non_alive_neighbours(centernode,reg,grid)
    use mod_3dfm
    implicit none

    logical        :: non_alive_neighbours
    type(Tregion)  :: reg
    type(Tpropagation_grid) :: grid
    integer        :: i1,i2,i3           ! identify a node (i,j,k or 0,interface,inode)
    integer        :: n,m,i,j,k,ii,jj,kk,icell ! local variables
    integer        :: centernode,n_concell
    type(Tinteger_coordinates)  :: concell(8)
    type(Tintersection),pointer  :: isec


    non_alive_neighbours = .false.

! store the identifiers of the centernode node in local variables. Reminder:
! for regular grid nodes i1,i2,i3  correspond to ir,ilat,ilong of the node
! for intersection nodes i1,i2,i3 are 0, intersection #, node # in intersection 

   i1 = reg%node(centernode)%i1 ; i2 = reg%node(centernode)%i2 ; i3 = reg%node(centernode)%i3


! make a list of grid cells of which the new alive node is part

   if (i1 /= 0) then   ! centernode is a regular grid node. Use spatial correlation of grid

      n_concell = 0
      do i=0,1
         if ((i1-i>0) .and. (i1-i<grid%nr)) then
            do j=0,1
               if ((i2-j>0) .and. (i2-j<grid%nlat)) then
                  do k=0,1
                     if ((i3-k>0) .and. (i3-k<grid%nlong)) then
                        n_concell=n_concell+1
                        concell(n_concell)%ir=i1-i  
                        concell(n_concell)%ilat=i2-j 
                        concell(n_concell)%ilong=i3-k
                     endif
                  end do
               end if
            end do
         end if
      end do

   else          ! centernode is an intersection node. Use the connections that have been found before

      if (i2 == reg%itop%id) then ; isec => reg%itop ; else ; isec => reg%ibot ; endif

      n_concell = 0
      do n=1,8
         icell = isec%ccell_from_inode(n,i3)
         if (icell > 0) then
            n_concell=n_concell+1
            concell(n_concell)%ir= isec%ccells(icell)%ir
            concell(n_concell)%ilat=isec%ccells(icell)%ilat 
            concell(n_concell)%ilong=isec%ccells(icell)%ilong
         endif
      enddo

   endif

   do n=1,n_concell

! find the intersection nodes of connected cell n

! explanation: each intersection has a 1-D list of cells cut by the interface, 
! and a list of intersection nodes that are part of each cut cell. Each regular 
! grid cell has a pointer ccind_from_3dc(i,j,k)%p (Cut Cell INDex FROM 3D Coordinates)
! associated with it, where p is a pointer to an integer array with as many elements 
! as there are intersections. If a cell is cut by interface n, the pointer 
! ccind_from_3dc(i,j,k)%p is allocated, and the variable ccind_from_3dc(i,j,k)%p(n) 
! contains the array index of cell (i,j,k) in the 1D cut cell list of intersection n 

! test whether the cell is cut by any interface, in that case the pointer to the local list of
! interfaces cutting the cell has been allocated

      if (associated(grid%ccind_from_3dc(concell(n)%ir,concell(n)%ilat,concell(n)%ilong)%p)) then

! if so, check if the cell is cut by the top intersection

    ! icell is the index of the current connected cell in the list of cells cut by interface reg%itop.
                  
        icell=grid%ccind_from_3dc(concell(n)%ir,concell(n)%ilat,concell(n)%ilong)%p(reg%itop%iface_id)

     ! if icell == 0 the cell is not cut be the top interface
    
         if(icell /= 0) then
 
            isec => reg%itop
            do jj=1,isec%n_inodes(icell)

!   m is the node number in the regional node list of node  jj in the list of inteface 
!   nodes that are part of cut cell icell
                m=isec%rbel_node_id(isec%inodes(jj,icell))

               if ( reg%node_status(m) < 0 ) then  
                  non_alive_neighbours = .true.
                  return
               endif

            end do

         end if


! then check the bottom intersection

        icell=grid%ccind_from_3dc(concell(n)%ir,concell(n)%ilat,concell(n)%ilong)%p(reg%ibot%iface_id)
         if (icell /= 0) then

            isec => reg%ibot
            do jj=1,isec%n_inodes(icell)

               m=isec%rabo_node_id(isec%inodes(jj,icell))
!                  print *,'its regional node number is ',m,node_is_counted(m),centernode

               if ( reg%node_status(m) < 0 ) then  
                  non_alive_neighbours = .true.
                  return
               endif

            end do

         endif

      endif


! then find the regular grid nodes of connected cell n


      do i=0,1
         ii=concell(n)%ir+i
         do j=0,1
            jj=concell(n)%ilat+j
            do k=0,1
               kk=concell(n)%ilong+k

               ! reduced connectivity for regular nodes if cell is completely regular

               if ((i1 == 0 .or. (i1 /= 0 .and. abs(i1-ii)+abs(i2-jj)+abs(i3-kk) == 1))) then 
                  
                 if (grid%node_region(ii,jj,kk) == reg%id) then  
               ! node has to belong to the current region

                     m=grid%rnode_id(ii,jj,kk)

                     if ( reg%node_status(m) < 0 ) then  
                        non_alive_neighbours = .true.
                        return
                     endif

                  endif

               endif

            end do
         end do
      end do


   end do  ! loop over connected cells

   return

  end function non_alive_neighbours
!-----------------------------------------------------------------------------------------
! creates the list of neighbours (connected nodes) of a source

  subroutine get_source_neighbours(centernode,s,reg,grid)
    use mod_3dfm
    implicit none

    type(Tsource)  :: s
    type(Tregion)  :: reg
    type(Tpropagation_grid) :: grid
    integer        :: i1,i2,i3           ! identify a node (i,j,k or 0,interface,inode)
    integer        :: n,m,i,j,k,ii,jj,kk,icell ! local variables
    integer        :: centernode,n_concell,mstore(30)
    type(Tinteger_coordinates)  :: concell(8)
    type(Tintersection),pointer  :: isec

    

! store the identifiers of the centernode node in local variables. Reminder:
! for regular grid nodes i1,i2,i3  correspond to ir,ilat,ilong of the node
! for intersection nodes i1,i2,i3 are 0, intersection #, node # in intersection 

   i1 = reg%node(centernode)%i1 ; i2 = reg%node(centernode)%i2 ; i3 = reg%node(centernode)%i3


! make a list of grid cells of which the new alive node is part

   if (i1 /= 0) then   ! centernode is a regular grid node. Use spatial correlation of grid

      n_concell = 0
      do i=0,1
         if ((i1-i>0) .and. (i1-i<grid%nr)) then
            do j=0,1
               if ((i2-j>0) .and. (i2-j<grid%nlat)) then
                  do k=0,1
                     if ((i3-k>0) .and. (i3-k<grid%nlong)) then
                        n_concell=n_concell+1
                        concell(n_concell)%ir=i1-i  
                        concell(n_concell)%ilat=i2-j 
                        concell(n_concell)%ilong=i3-k
                     endif
                  end do
               end if
            end do
         end if
      end do

   else          ! centernode is an intersection node. Use the connections that have been found before

      if (i2 == reg%itop%id) then ; isec => reg%itop ; else ; isec => reg%ibot ; endif

      n_concell = 0
      do n=1,8
         icell = isec%ccell_from_inode(n,i3)
         if (icell > 0) then
            n_concell=n_concell+1
            concell(n_concell)%ir= isec%ccells(icell)%ir
            concell(n_concell)%ilat=isec%ccells(icell)%ilat 
            concell(n_concell)%ilong=isec%ccells(icell)%ilong
         endif
      enddo

   endif

   do n=1,n_concell

! find the intersection nodes of connected cell n


! test whether the cell is cut by any interface, in that case the pointer to the local list of
! interfaces cutting the cell has been allocated

      if (associated(grid%ccind_from_3dc(concell(n)%ir,concell(n)%ilat,concell(n)%ilong)%p)) then

! if so, check if the cell is cut by the top intersection

  ! icell is the index of the current connected cell in the list of cells cut by interface reg%itop.
                  
        icell=grid%ccind_from_3dc(concell(n)%ir,concell(n)%ilat,concell(n)%ilong)%p(reg%itop%iface_id)

     ! if icell == 0 the cell is not cut be the top interface
    
         if(icell /= 0) then
 
            isec => reg%itop
            do jj=1,isec%n_inodes(icell)

!   m is the node number in the regional node list of node  jj in the list of 
!  inteface nodes that are part of cut cell icell
                m=isec%rbel_node_id(isec%inodes(jj,icell))

               if ( s%n_cnode == 0 ) then  
                  s%n_cnode=1
                  mstore(s%n_cnode) = m
                  s%cnode(s%n_cnode)%i1=reg%node(m)%i1
                  s%cnode(s%n_cnode)%i2=reg%node(m)%i2
                  s%cnode(s%n_cnode)%i3=reg%node(m)%i3
               else
                  if ( count(mstore(1:s%n_cnode) == m) == 0 ) then
                     s%n_cnode=s%n_cnode+1
                     mstore(s%n_cnode) = m
                     s%cnode(s%n_cnode)%i1=reg%node(m)%i1
                     s%cnode(s%n_cnode)%i2=reg%node(m)%i2
                     s%cnode(s%n_cnode)%i3=reg%node(m)%i3
                  endif
               endif

            end do

         end if


! then check the bottom intersection

        icell=grid%ccind_from_3dc(concell(n)%ir,concell(n)%ilat,concell(n)%ilong)%p(reg%ibot%iface_id)

         if (icell /= 0) then

            isec => reg%ibot
            do jj=1,isec%n_inodes(icell)

               m=isec%rabo_node_id(isec%inodes(jj,icell))

               if ( s%n_cnode == 0 ) then  
                  s%n_cnode=1
                  mstore(s%n_cnode) = m
                  s%cnode(s%n_cnode)%i1=reg%node(m)%i1
                  s%cnode(s%n_cnode)%i2=reg%node(m)%i2
                  s%cnode(s%n_cnode)%i3=reg%node(m)%i3
               else
                  if ( count(mstore(1:s%n_cnode) == m) == 0 ) then
                     s%n_cnode=s%n_cnode+1
                     mstore(s%n_cnode) = m
                     s%cnode(s%n_cnode)%i1=reg%node(m)%i1
                     s%cnode(s%n_cnode)%i2=reg%node(m)%i2
                     s%cnode(s%n_cnode)%i3=reg%node(m)%i3
                  endif
               endif

            end do

         endif

      endif


! then find the regular grid nodes of connected cell n

!      write(22,*) 'regular nodes of cell',n

      do i=0,1
         ii=concell(n)%ir+i
         do j=0,1
            jj=concell(n)%ilat+j
            do k=0,1
               kk=concell(n)%ilong+k

                 if (grid%node_region(ii,jj,kk) == reg%id) then  
                 ! node has to belong to the current region

                     m=grid%rnode_id(ii,jj,kk)

                     if ( s%n_cnode == 0 ) then  
                        s%n_cnode=1
                        mstore(s%n_cnode) = m
                        s%cnode(s%n_cnode)%i1=reg%node(m)%i1
                        s%cnode(s%n_cnode)%i2=reg%node(m)%i2
                        s%cnode(s%n_cnode)%i3=reg%node(m)%i3
                     else
                        if ( count(mstore(1:s%n_cnode) == m) == 0 ) then
                           s%n_cnode=s%n_cnode+1
                           mstore(s%n_cnode) = m
                           s%cnode(s%n_cnode)%i1=reg%node(m)%i1
                           s%cnode(s%n_cnode)%i2=reg%node(m)%i2
                           s%cnode(s%n_cnode)%i3=reg%node(m)%i3
                        endif
                     endif


                  endif


            end do
         end do
      end do


   end do  ! loop over connected cells

   return

  end subroutine get_source_neighbours

!****************************************************************************************************************
! this subroutine modifies the time gradient at an intersection to represent the reflected wave
! tgrad(reflected)= tgrad - 2*(tgrad.n)*n, where n is the normal to the interface (no conversion)
! or in the same way as a refraction if a wave type conversion takes place

  subroutine reflect_gradient(isec,tf_prev,vtype)
    use mod_3dfm
    implicit none

    type(Tintersection)  :: isec
    type(Ttime_field)    :: tf_prev
    integer  :: n,vtype,n_no_ref
    real(kind=dp),dimension(:),pointer :: vel
    real(kind=dp)        :: direction,det,grad_perp,grad_perp_reflected,grad_par(3)

    if (tf_prev%vtype == vtype) then  ! simple reflection ( no wave type conversion)

       do n=1,isec%nnode

          isec%time_gradient(1:3,n)=isec%time_gradient(1:3,n)- &
               2.0_dp*dot_product(isec%normal(1:3,n),isec%time_gradient(1:3,n))*isec%normal(1:3,n)

       end do

    else          ! reflection with wave type conversion

       if (isec%id == tf_prev%reg%itop%id) then   ! reflection off the top interface
          vel => isec%vel_bot(:,vtype)
          direction = -1.0_dp
       else                                       ! reflection off the bottom interface
          vel => isec%vel_top(:,vtype)
          direction = 1.0_dp
       endif
       n_no_ref=0


       do n=1,isec%nnode

          grad_perp=dot_product(isec%normal(1:3,n),isec%time_gradient(1:3,n))
          grad_par=isec%time_gradient(1:3,n)-grad_perp*isec%normal(1:3,n)

          det=1.d0/(vel(n)**2) - sum(grad_par**2)

          if (det > 0.0_dp) then
             
             ! the refracted ray exists

             grad_perp_reflected=sqrt(det)
             isec%time_gradient(1:3,n)=grad_par + &
                  sign(grad_perp_reflected,direction)*isec%normal(1:3,n)

          else

             ! no converted reflection possible

             isec%arrivaltime(n)=huge_time
             isec%time_gradient(1:3,n)=0.0_dp
             n_no_ref= n_no_ref+1

          endif

       end do

       if (n_no_ref > 0) print *,n_no_ref,' intersection points could not reflect the converted wave'
    
    endif




    return

  end subroutine reflect_gradient


!****************************************************************************************************
! this subroutine modifies the time gradient at an intersection to represent the refracted wave
! tgrad(parallel) =  tgrad - (tgrad.n)*n, where n is the normal to the interface 
! then solve |tgrad(perpendicular)|^2 + |tgrad(parallel)|^2 = s^2 where s is the slowness 
! in the region refracted into.
! also checks for total reflection and supresses such points as a source of refracted waves

  subroutine refract_gradient(isec,reg,vtype,direction)
    use mod_3dfm
    implicit none

    type(Tintersection)  :: isec
    type(Tregion)        ::reg

    integer  :: n,n_tot_ref,direction,vtype
    real(kind=dp)         :: grad_par(3),grad_perp,grad_perp_refracted,det,direction_real
    real(kind=dp),dimension(:),pointer  :: vel


    if (isec%id == reg%itop%id) vel => isec%vel_top(:,vtype)
    if (isec%id == reg%ibot%id) vel => isec%vel_bot(:,vtype)
    n_tot_ref=0
    direction_real = dble(direction) 

    do n=1,isec%nnode

       grad_perp=dot_product(isec%normal(1:3,n),isec%time_gradient(1:3,n))
       grad_par=isec%time_gradient(1:3,n)-grad_perp*isec%normal(1:3,n)

       det=1.d0/(vel(n)**2) - sum(grad_par**2)

       if (det > 0.0_dp) then
             
          ! the refracted ray exists

          grad_perp_refracted=sqrt(det)
          isec%time_gradient(1:3,n)=grad_par + &
               sign(grad_perp_refracted,direction_real)*isec%normal(1:3,n)

       else

          ! total reflection

          isec%arrivaltime(n)=huge_time
          isec%time_gradient(1:3,n)=0.0_dp
          n_tot_ref= n_tot_ref+1

       endif

    end do

    if (n_tot_ref > 0) print *,'total reflection occurred at ',n_tot_ref,' intersection nodes'

    return

  end subroutine refract_gradient

!*****************************************************************************************************
! this subroutine applies a correction to the time gradient at a node as derived from
! the direction of the wavefront used for the final update (for irregular updates only).
! we assume that the change in velocity between the location where the wavefront  used for 
! the update is evaluated and the velocity
! at the update node occurs as a step at a plane with a normal given by the norm of the
! velocity gradient. The corrected time gradient is the incoming time gradient refracted at this
! "interface". Experiments show that this significantly improves the constancy of the ray parameter
! along a ray in 1-D velocity distribution tests


  subroutine refract_locally(r,lat,long,gridv,tgrad)
    use mod_3dfm
    implicit none

    real(kind=dp)         :: grad_par(3),grad_perp,grad_perp_refracted,det,r,lat,long,vel
    real(kind=dp),dimension(3)  :: tgrad,vgrad,normal
    type(Tvelocity_grid)  :: gridv


    call velocity_gradient(r,lat,long,gridv,vgrad(1),vgrad(2),vgrad(3),vel)

    if (sum(vgrad**2) < 1.0e-30_dp) return  ! avoid division by zero if gradient = 0

    normal=vgrad/sqrt(sum(vgrad**2))

    grad_perp=dot_product(normal,tgrad)

    grad_par=tgrad-grad_perp*normal
    
    det=1.d0/(vel**2) - sum(grad_par**2)

    if (det > 0.0_dp) then
             
       ! the refracted ray exists

       grad_perp_refracted=sqrt(det)
       tgrad=grad_par + sign(grad_perp_refracted,grad_perp)*normal

    endif


    return

  end subroutine refract_locally
!*********************************************************************************
!--------------------------------------------------------------------------------\
!*******************************************************************************************
! This subroutine creates a triangualtion of a 2-D set of points
! It calls malcolm Sambridge's triangualtion routines that are a part
! of the natural neighbours package

      subroutine triangulate(t)

      use mod_3dfm

! all default floats are set to double

      implicit double precision (a-h,o-z)

! argument definition

      type(Ttriangulation) :: t

! local array definition

      double precision,dimension(:,:),allocatable::points,centres
      integer,dimension(:,:),allocatable::neighbour,SPfromTR
      integer,dimension(:),allocatable::vis_tlist,vis_elist,add_tlist,hulltriangles
      integer,dimension(:),allocatable::nnn,nnlist,ntrilist
      logical,dimension(:),allocatable::lt_work,ln_work
      double precision,dimension(:,:),allocatable::work_d1,work_d2,work_d3,work_d4
      double precision,dimension(:,:),allocatable::work_d5,work_d6,work_d7
      real,dimension(:,:),allocatable::work_r1
      real,dimension(:),allocatable::work_r2
      integer,dimension(:),allocatable::work_i1,work_i3
      integer,dimension(:,:),allocatable::work_i2


! other variables

      double precision eps
      integer dmode
      logical clockwise

!---------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------
! start the triangulation 
!------------------------------------------------------------------------------------


! nl is the initial number of points

      nl=t%npoints

! nlmax is the maximum number of points

      nlmax=nl

! ntmax is the maximum number of delaunay triangles built on the points

      ntmax=nlmax*3

! nhmax is the maximum number of points on the convex hull formed
! by the lagrangian cloud

      nhmax=nlmax

! npmax is nlmax

      npmax=nlmax

! nnpnmax is the maximum number of neighbours per point

      nnpnmax=50

! nvmax and nmax are the size of working arrays

      nvmax=ntmax
      nmax=3*ntmax+npmax

! eps is a small number

      eps=tiny(eps)

! allocates memory for the delaunay triangulation

     allocate (points(2,nlmax),centres(3,ntmax))
     allocate (neighbour(3,ntmax),SPfromTR(3,ntmax))
     allocate (vis_tlist(nvmax),vis_elist(nvmax),add_tlist(nvmax))
     allocate (hulltriangles(nhmax),nnn(npmax+1),nnlist(nmax),ntrilist(nmax))
     allocate (lt_work(ntmax),ln_work(nlmax))
     allocate (work_d1(2,nnpnmax),work_d2(2,nnpnmax),work_d3(2,nnpnmax))
     allocate (work_d4(2,nnpnmax),work_d5(2,nnpnmax),work_d6(2,nnpnmax))
     allocate (work_d7(2,nnpnmax),work_r1(nnpnmax,2),work_r2(nnpnmax))
     allocate (work_i1(nnpnmax),work_i2(2,nnpnmax),work_i3(nnpnmax))





!     print *,'before triangulation'
!--------------------------------------------------------------------------------------


! initialise variables for nn_setup

         dmode=-2
         nmode=0
         clockwise=.true.
         nohalt_hull=0
         loc=1
         SPfromTR=0
         neighbour=0
         points=0.d0
         field=0.d0
         centres=0.d0
         hulltriangles=0
         nnn=0
         nnlist=0
         ntrilist=0
         vis_tlist=0
         vis_elist=0
         add_tlist=0
         lt_work=.false.
         ln_work=.false.

         do i=1,nl
            points(1,i)=t%points(1,i)
            points(2,i)=t%points(2,i)
         enddo

! nn_setup (calculates delaunay triangles and other info)

         call nn2d_setup &
              (nl,ntmax,nhmax,npmax,nnpnmax,nmax, &
              points,dmode,nmode,clockwise,t%points(1,:),nt,SPfromTR, &
              centres,neighbour,nh,hulltriangles,nohalt_hull, &
              loc,nnn,nnlist,ntrilist, &
              eps,nvmax,vis_tlist,vis_elist,add_tlist, &
              lt_work,ln_work)



!  
         t%ntriangles = nt
         allocate(t%points_from_triangle(3,nt),t%triangle_neighbours(3,nt))
         do i=1,nt
            t%points_from_triangle(1:3,i)=SPfromTR(1:3,i)
            t%triangle_neighbours(1:3,i)=neighbour(1:3,i)
         end do


       ! the inverse connectivity

         allocate(t%n_triangles_from_point(t%npoints))
         t%n_triangles_from_point = 0
         do i=1,nt
            do j=1,3
               t%n_triangles_from_point(SPfromTR(j,i)) = t%n_triangles_from_point(SPfromTR(j,i)) + 1
            end do
         end do

         nmax = maxval(t%n_triangles_from_point)
         allocate(t%triangles_from_point(nmax,t%npoints))
         t%n_triangles_from_point = 0
         do i=1,nt
            do j=1,3
               ipoint = SPfromTR(j,i)
               t%n_triangles_from_point(ipoint) = t%n_triangles_from_point(ipoint) + 1
               t%triangles_from_point(t%n_triangles_from_point(ipoint),ipoint) = i
            end do
         end do




! deallocate everything


      deallocate (points,centres)
      deallocate (neighbour,SPfromTR)
      deallocate (vis_tlist,vis_elist,add_tlist)
      deallocate (hulltriangles,nnn,nnlist,ntrilist)
      deallocate (lt_work,ln_work)
      deallocate (work_d1,work_d2,work_d3,work_d4,work_d5,work_d6,work_d7)
      deallocate (work_r1,work_r2,work_i1,work_i2,work_i3)


         return

       end subroutine triangulate

!****************************************************************************

subroutine clean_triangulation(tri)
  use mod_3dfm

  type(Ttriangulation) ::tri

  if (associated(tri%points_from_triangle)) deallocate(tri%points_from_triangle)
  if (associated(tri%triangle_neighbours)) deallocate(tri%triangle_neighbours)
  if (associated(tri%points)) deallocate(tri%points)
  if (associated(tri%triangles_from_point)) deallocate(tri%triangles_from_point)
  if (associated(tri%n_triangles_from_point)) deallocate(tri%n_triangles_from_point)

  return
end subroutine clean_triangulation

!**************************************************************************************
! This subroutine sets a flag at all nodes of the propagation grid that are not
! connected to any cut cell. This is used to speed up the fast marching calculations
! 
subroutine tag_regular_nodes(grid)
  use mod_3dfm

  type(Tpropagation_grid)  :: grid

  integer  :: i,j,k
  logical  :: not_regular

  if (.not. associated(grid%ccind_from_3dc)) &
       stop 'nodes cannot be tagged when ccind_from_3dc not initialized'

  allocate(grid%fully_regular(grid%nr,grid%nlat,grid%nlong))

  grid%fully_regular = .false.

  do k=2, grid%nlong-1
     do j=2,grid%nlat-1
        do i=2,grid%nr-1

           not_regular = associated(grid%ccind_from_3dc(i-1,j-1,k-1)%p ) .or. &
                associated(grid%ccind_from_3dc(i  ,j-1,k-1)%p ) .or. &
                associated(grid%ccind_from_3dc(i-1,j  ,k-1)%p ) .or. &
                associated(grid%ccind_from_3dc(i  ,j  ,k-1)%p ) .or. &
                associated(grid%ccind_from_3dc(i-1,j-1,k  )%p ) .or. &
                associated(grid%ccind_from_3dc(i  ,j-1,k  )%p ) .or. &
                associated(grid%ccind_from_3dc(i-1,j  ,k  )%p ) .or. &
                associated(grid%ccind_from_3dc(i  ,j  ,k  )%p )

           grid%fully_regular(i,j,k) = .not. not_regular

        end do
     end do
  end do

end subroutine tag_regular_nodes

!********************************************************************************
! This subroutine transfers the nodes from a region in the refined source grid to 
! the corresponding region on the main propagation grid and sets the node status of the
! nodes in the propagation grid region that received a value to 0 (final value reached)

subroutine transfer_refined_region(sreg,reg,t_short)

use mod_3dfm
implicit none

type(Tregion) :: sreg,reg
real(kind=dp) :: t_short

type(Tintersection),pointer :: isec

integer :: k,m,n,i1,i2,i3,j1,j2,j3,icell,kk,intype
real(kind=dp)  :: xir,xilat,xilong,dxir,dxilat,dxilong,dist
integer  :: nrmin,nlatmin,nlongmin
!logical  :: pdiag = .false.


nrmin = sgrid%index_r0
nlatmin = sgrid%index_lat0
nlongmin = sgrid%index_long0

tloop : do n=1,sreg%nnode

!   pdiag = (reg%id == 3) .and. (n == 359764)

!   if (pdiag) print *,'trying to transfer xnode'

   if (sreg%arrivaltime(n) <= t_short) then  ! transfer only times less than the first boundary hit

!      if (pdiag) print *,'passed t-short'

   i1=sreg%node(n)%i1  ; i2=sreg%node(n)%i2   ; i3= sreg%node(n)%i3
   

   if (i1 /= 0) then           ! the regional node is a regular node

!      if (pdiag) print *,'xnode is regular'

      if (mod(i1-1,refinement_factor) == 0 .and. mod(i2-1,refinement_factor) == 0 &
           .and. mod(i3-1,refinement_factor) == 0) then

         j1 = nrmin    +   (i1-1)/refinement_factor
         j2 = nlatmin  +   (i2-1)/refinement_factor
         j3 = nlongmin +   (i3-1)/refinement_factor

!         print *,n,'regular node',j1,j2,j3
!         print *,pgrid%rnode_id(j1,j2,j3), reg%nnode

         reg%arrivaltime(pgrid%rnode_id(j1,j2,j3)) = sreg%arrivaltime(n)
         reg%time_gradient(1:3,pgrid%rnode_id(j1,j2,j3)) = sreg%time_gradient(1:3,n)
         reg%node_status(pgrid%rnode_id(j1,j2,j3)) = 0

!         print *,n,'regular node'

      endif

   else   ! the regional node is an intersection node

!      if (pdiag) print *,'irregular sreg node',i1,i2,i3,sintersection(i2)%intype(i3)

! choose the corresponding intersection on the main grid

      if (i2 == sreg%itop%id) then
         isec => reg%itop
      else
         isec => reg%ibot
      endif

! coarse grid index coordinates

      xir      = (sintersection(i2)%r(i3)-pgrid%r0)/pgrid%dr0
      xilat    = (sintersection(i2)%lat(i3)-pgrid%lat0)/pgrid%dlat0
      xilong   = (sintersection(i2)%long(i3)-pgrid%long0)/pgrid%dlong0

!      if (pdiag) print '(a10,3f15.6)','gi cs',xir,xilat,xilong

! distance from nearest coarse grid coordinate plane

      dxir    = abs(xir - anint(xir))
      dxilat  = abs(xilat - anint(xilat))
      dxilong = abs(xilong - anint(xilong))

!      if (pdiag) print '(a10,3f15.6)','npdist',dxir,dxilat,dxilong


! coordinates of the coarse grid cell containing the refined region node

      j1= min(int(xir)+1,pgrid%nr-1)
      j2= min(int(xilat)+1,pgrid%nlat-1)
      j3= min(int(xilong)+1,pgrid%nlong-1)

!      if (pdiag) print *,'cgcell',j1,j2,j3


! convert type of inode on refined grid to type of inode on coarse grid

      if (sintersection(i2)%intype(i3) /= 0) then

         intype = sintersection(i2)%intype(i3)

      else

         if (dxir<interface_tolerance.and.dxilat<interface_tolerance.and.&
              dxilong<interface_tolerance) then
            intype = 0
         else if(dxilat < interface_tolerance .and. dxilong < interface_tolerance) then
            intype = 1
         else if(dxir < interface_tolerance .and. dxilong < interface_tolerance) then
            intype = 2
         else if(dxir < interface_tolerance .and. dxilat < interface_tolerance) then
            intype = 3
         else
            cycle tloop
         endif

      endif

! test whether the refined regional node lies on r,lat or long connection

      select case (intype)

         case(0)

            ! if all coordinates coincide exactly with the coarse grid lines, there is 
            ! a corresponding coarse grid intersection node

            
            if (dxir < interface_tolerance .and. dxilat < interface_tolerance &
                 .and. dxilong < interface_tolerance ) then 

!               if (pdiag) print *,n,'type 0'

               ! get the cut cell index of the coarse grid cell
               if (associated(pgrid%ccind_from_3dc(j1,j2,j3)%p)) then

                  icell = pgrid%ccind_from_3dc(j1,j2,j3)%p(isec%iface_id)

       ! test the nodes of this cut cell for coincidence with the refined inode under consideration
                  do m=1,isec%n_inodes(icell)
                  
                     k=isec%inodes(m,icell)  ! the intersection node #

                     if (i2 == sreg%itop%id) then 
                        kk=isec%rbel_node_id(k)
                     else
                        kk=isec%rabo_node_id(k)
                     endif


                     if (isec%intype(k) == 0) then  
              ! only consider nodes of the same type as the refined inode

                        dist =sqrt((sintersection(i2)%r(i3)-isec%r(k))**2 &
                             + (isec%r(k)*(sintersection(i2)%lat(i3)-isec%lat(k)))**2 &
                             + (isec%r(k)*isec%coslat(k)*(sintersection(i2)%long(i3) &
                             -isec%long(k)))**2)

                        if (dist < pgrid%tolerance) then
                           reg%arrivaltime(kk) = sreg%arrivaltime(n)
                           reg%time_gradient(1:3,kk) = sreg%time_gradient(1:3,n)
                           reg%node_status(kk) = 0
                           cycle tloop
                        endif

                     endif

                  end do

               endif

            endif


         case(1)    ! inode lies on an r-connection

            ! if other coordinates coincide exactly with the coarse grid lines, there is 
            ! a corresponding coarse grid intersection node

            
            if (dxilat < interface_tolerance .and. dxilong < interface_tolerance) then 

!               if (pdiag) print *,n,'type 1'

               ! get the cut cell index of the coarse grid cell
               if (associated(pgrid%ccind_from_3dc(j1,j2,j3)%p)) then

                  icell = pgrid%ccind_from_3dc(j1,j2,j3)%p(isec%iface_id)

       ! test the nodes of this cut cell for coincidence with the refined inode under consideration
                  do m=1,isec%n_inodes(icell)
                  
                     k=isec%inodes(m,icell)  ! the intersection node #

                     if (i2 == sreg%itop%id) then 
                        kk=isec%rbel_node_id(k)
                     else
                        kk=isec%rabo_node_id(k)
                     endif


                     if (isec%intype(k) == 1) then  
                ! only consider nodes of the same type as the refined inode

                        dist =sqrt((sintersection(i2)%r(i3)-isec%r(k))**2 &
                             +(isec%r(k)*(sintersection(i2)%lat(i3)-isec%lat(k)))**2 &
                             + (isec%r(k)*isec%coslat(k)*(sintersection(i2)%long(i3) &
                             -isec%long(k)))**2)


                        if (dist < pgrid%tolerance) then
                           reg%arrivaltime(kk) = sreg%arrivaltime(n)
                           reg%time_gradient(1:3,kk) = sreg%time_gradient(1:3,n)
                           reg%node_status(kk) = 0
                           cycle tloop
                        endif

                     endif

                  end do

               endif

            endif


         case(2) ! inode lies on a lat-connection

            if (dxir < interface_tolerance .and. dxilong < interface_tolerance) then

!               if (pdiag) print *,n,'type 2'
               if (associated(pgrid%ccind_from_3dc(j1,j2,j3)%p)) then

                  icell = pgrid%ccind_from_3dc(j1,j2,j3)%p(isec%iface_id)

                  do m=1,isec%n_inodes(icell)
                  
                     k=isec%inodes(m,icell)

                     if (i2 == sreg%itop%id) then 
                        kk=isec%rbel_node_id(k)
                     else
                        kk=isec%rabo_node_id(k)
                     endif

                     if (isec%intype(k) == 2) then

                        dist =sqrt((sintersection(i2)%r(i3)-isec%r(k))**2 &
                             +(isec%r(k)*(sintersection(i2)%lat(i3)-isec%lat(k)))**2 &
                             + (isec%r(k)*isec%coslat(k)*(sintersection(i2)%long(i3) &
                             -isec%long(k)))**2)

                        if (dist < pgrid%tolerance) then
                           reg%arrivaltime(kk) = sreg%arrivaltime(n)
                           reg%time_gradient(1:3,kk) = sreg%time_gradient(1:3,n)
                           reg%node_status(kk) = 0
                           cycle tloop
                        endif

                     endif

                  end do

               endif

            endif


         case(3)    ! inode lies on a long-connection

            if (dxilat < interface_tolerance .and. dxir < interface_tolerance) then

!               if (pdiag) print *,n,'type 3'

               if (associated(pgrid%ccind_from_3dc(j1,j2,j3)%p)) then

                  icell = pgrid%ccind_from_3dc(j1,j2,j3)%p(isec%iface_id)

                  do m=1,isec%n_inodes(icell)
                  
                     k=isec%inodes(m,icell)


                     if (i2 == sreg%itop%id) then 
                        kk=isec%rbel_node_id(k)
                     else
                        kk=isec%rabo_node_id(k)
                     endif

                     if (isec%intype(k) == 3) then

                        dist =sqrt((sintersection(i2)%r(i3)-isec%r(k))**2 &
                             +(isec%r(k)*(sintersection(i2)%lat(i3)-isec%lat(k)))**2 &
                             + (isec%r(k)*isec%coslat(k)*(sintersection(i2)%long(i3) &
                             -isec%long(k)))**2)

                        if (dist < pgrid%tolerance) then
                           reg%arrivaltime(kk) = sreg%arrivaltime(n)
                           reg%time_gradient(1:3,kk) = sreg%time_gradient(1:3,n)
                           reg%node_status(kk) = 0
                           cycle tloop
                        endif

                     endif

                  end do

               endif

            endif

      end select


   endif   ! refined node is an inode

   endif  ! arrivaltime < t_short

end do tloop

end subroutine transfer_refined_region

!*****************************************************************************************************
!*****************************************************************************************************
subroutine write_valid_rays(n,m)

  use mod_3dfm
  type(Tray),pointer                   :: ray
  integer                              :: i,j,k,m,n,zero

  zero=0

  if (receiver(n)%ray(m)%is_multiray) then

     do k=1,receiver(n)%ray(m)%n_subrays

        ray => receiver(n)%ray(m)%subray(k)

        if (ray%valid) then

           write(31,'(5i6)') n,ray%source_id,m,k,ray%nsections

           do i=1,ray%nsections

              write(31,'(2i6,2l5)') ray%section(i)%npoints,ray%section(i)%reg%id, &
                   ray%section(i)%diffracted,ray%section(i)%headwave

              do j=ray%section(i)%npoints,1,-1
                 write(31,'(3f17.8)') ray%section(i)%point(1:3,j)
              end do

           end do

        else

           write(31,'(5i6)') n,ray%source_id,m,k,zero

        endif

     end do

  else

     k = 0

     ray => receiver(n)%ray(m)

     if (ray%valid) then

        write(31,'(5i6)') n,ray%source_id,m,zero,ray%nsections

        do i=1,ray%nsections

           write(31,'(2i6,2l5)') ray%section(i)%npoints,ray%section(i)%reg%id, &
                ray%section(i)%diffracted,ray%section(i)%headwave

           do j=ray%section(i)%npoints,1,-1
              write(31,'(3f17.8)') ray%section(i)%point(1:3,j)
           end do

        end do

     else

        write(31,'(5i6)') n,ray%source_id,m,zero,zero

     endif

  endif


end subroutine write_valid_rays


!*****************************************************************************************************
!*****************************************************************************************************
subroutine clean_ray(n,m)

  use mod_3dfm

  integer                              :: i,k,m,n
  type(Tray),pointer                   :: ray

  if (receiver(n)%ray(m)%is_multiray) then

     do k=1,receiver(n)%ray(m)%n_subrays

        ray => receiver(n)%ray(m)%subray(k)

        if (associated(ray%pdev)) deallocate(ray%pdev)
        if (associated(ray%pdev_indx)) deallocate(ray%pdev_indx)

        do i=1,ray%nsections
           if (associated(ray%section(i)%point)) deallocate(ray%section(i)%point)           
        end do

     end do

  else

     ray => receiver(n)%ray(m)

     if (associated(ray%pdev)) deallocate(ray%pdev)
     if (associated(ray%pdev_indx)) deallocate(ray%pdev_indx)

     do i=1,ray%nsections
        if (associated(ray%section(i)%point)) deallocate(ray%section(i)%point)
     end do

  endif

end subroutine clean_ray

!*****************************************************************************************************
subroutine write_frechet_derivatives(n,m)

  use mod_3dfm

  integer                              :: i,k,m,n,zero
  type(Tray),pointer                   :: ray

  zero=0

  if (receiver(n)%ray(m)%is_multiray) then

     do k=1,receiver(n)%ray(m)%n_subrays

        ray => receiver(n)%ray(m)%subray(k)

        if (ray%valid) then

           write(21,'(5i6)') n,ray%source_id,m,k,ray%n_pdev

           do i=1,ray%n_pdev
              write(21,'(i10,f17.8)') ray%pdev_indx(i),ray%pdev(i)
           end do

        else

           write(21,'(5i6)') n,ray%source_id,m,k,zero

        endif

     end do

  else

     k = 0

     ray => receiver(n)%ray(m)

     if (ray%valid) then

        write(21,'(5i6)') n,ray%source_id,m,k,ray%n_pdev

        do i=1,ray%n_pdev
           write(21,'(i10,e17.8)') ray%pdev_indx(i),ray%pdev(i)
        end do

     else

        write(21,'(5i6)') n,ray%source_id,m,k,zero

     endif

  endif

end subroutine write_frechet_derivatives
!
!**************************************************************
!

subroutine load_source_timefields(s)

  use mod_3dfm
  implicit none

  type(Tsource)  :: s
  integer   :: n,nfile

  nfile=s%nfile
  open(nfile,form='unformatted')

   do n=1,s%n_time_fields

      allocate(s%time_field(n)%arrivaltime(s%time_field(n)%reg%nnode))
      allocate(s%time_field(n)%time_gradient(3,s%time_field(n)%reg%nnode))
      read(nfile) s%time_field(n)%arrivaltime
      read(nfile) s%time_field(n)%time_gradient

   end do

   close(nfile)

end subroutine load_source_timefields

subroutine clean_source_timefields(s)

  use mod_3dfm
  implicit none

  type(Tsource)  :: s
  integer  :: n

  do n=1,s%n_time_fields

     deallocate(s%time_field(n)%arrivaltime,s%time_field(n)%time_gradient)

  end do

end subroutine clean_source_timefields

!
!*******************************************************************************
!
 subroutine write_arrivaltime_grid(src,path)

  use mod_3dfm
  implicit none

  type(Tpath) :: path
  type(Tsource) :: src
  integer       :: n,i,i1,i2,i3,ir,ilat,ilong,ntype
  type(Ttime_field),pointer :: tf
  type(Tregion),pointer :: reg
  real(kind=dp) :: r,lat,long,dxr,dxlat,dxlong


  if (associated(pgrid%arrivaltime)) deallocate(pgrid%arrivaltime)

  allocate(pgrid%arrivaltime(pgrid%nr,pgrid%nlat,pgrid%nlong))
  pgrid%arrivaltime=huge_time

  do n=path%first_tf_to_save, path%n_tf

     tf=> src%time_field(path%tf_sequence(n))
     reg => tf%reg

     do i=1, reg%nnode

        i1=reg%node(i)%i1 ; i2=reg%node(i)%i2 ; i3=reg%node(i)%i3 

        if (i1 /= 0) then
           pgrid%arrivaltime(i1,i2,i3)=min(tf%arrivaltime(i),pgrid%arrivaltime(i1,i2,i3))
        endif
        
        if (i1==0) then

           r=reg%r(i)-pgrid%r0 ; lat=reg%lat(i)-pgrid%lat0 ; long=reg%long(i)-pgrid%long0

           ir=nint(r/pgrid%dr0)+1
           ilat=nint(lat/pgrid%dlat0)+1
           ilong=nint(long/pgrid%dlong0)+1

           dxr=abs(r-pgrid%dr0*(ir-1))
           dxlat=reg%r(i)*abs(lat-pgrid%dlat0*(ilat-1))          
           dxlong=reg%r(i)*cos(reg%lat(i))*abs(long-pgrid%dlong0*(ilong-1)) 

           ntype=intersection(i2)%intype(i3)
           select case(ntype)
              case(0)
                 pgrid%arrivaltime(ir,ilat,ilong)= &
                      min(tf%arrivaltime(i),pgrid%arrivaltime(ir,ilat,ilong))

              case(1)
                 if (dxr<=pgrid%tolerance) pgrid%arrivaltime(ir,ilat,ilong)= &
                      min(tf%arrivaltime(i),pgrid%arrivaltime(ir,ilat,ilong))

              case(2)
                 if (dxlat<=pgrid%tolerance) pgrid%arrivaltime(ir,ilat,ilong)= &
                      min(tf%arrivaltime(i),pgrid%arrivaltime(ir,ilat,ilong))

              case(3)
                 if (dxlong<=pgrid%tolerance) pgrid%arrivaltime(ir,ilat,ilong)= &
                      min(tf%arrivaltime(i),pgrid%arrivaltime(ir,ilat,ilong))

           end select

        endif

     end do

  end do

  where (pgrid%arrivaltime>1.e10) pgrid%arrivaltime=-1.0


  write(19,*) src%id,path%id,path%first_tf_to_save
  do i3=1,pgrid%nlong
     do i2=1,pgrid%nlat
        do i1=1,pgrid%nr
           write(19,'(f12.5)') pgrid%arrivaltime(i1,i2,i3)
        end do
     end do
  end do


  deallocate(pgrid%arrivaltime)


end subroutine write_arrivaltime_grid
!
