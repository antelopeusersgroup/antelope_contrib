!************************************************************************************************
! this subroutine reads from file for which variables the inverson is to be done
! and establishes a global index for each variable
!
! the order is first the velocity grids and then interface grids, with the order inside
! each velocity or interface grid varying in the order r fastest, lat medium, long slowest
! e.g. n_loc = ir + (ilat-1)*nlat + (ilong-1)*nlat*nlong for velocities
! source position is at the end

subroutine initialize_inversion(do_frechet_derivatives)
  use mod_3dfm
  implicit none

  integer     :: ngrids,nifaces,nsources,i,j,igrid,iiface,isource,group,last_index
  logical     :: do_frechet_derivatives

  integer     :: input_list(1000),type_list(100),vtype

  n_inv_parms = 0
  n_inv_active= 0
  group = 0

  open(1,file='frechet.in')

  read(1,*) i
  if (i == 0) then
     do_frechet_derivatives = .false.
     close(1)
     return
  else
     do_frechet_derivatives = .true.
  endif



  read (1,*) ngrids
  n_inv_vgrid=ngrids

  if (n_inv_vgrid > 0) then

     read(1,*) input_list(1:ngrids)
     read(1,*) type_list(1:ngrids)

     allocate(vgrids_to_be_inv(ngrids))

     do i=1,ngrids
        igrid = input_list(i)
        vtype = type_list(i)
        vgrids_to_be_inv(i)%igrid=igrid
        vgrids_to_be_inv(i)%vtype=vtype
        vgrid(igrid,vtype)%to_be_inverted = .true.
        group=group+1
        if (group == 1 ) then
           vgrid(igrid,vtype)%start_index=1
        else
           vgrid(igrid,vtype)%start_index=last_index  
        endif
        last_index = vgrid(igrid,vtype)%start_index + vgrid(igrid,vtype)%nnode
        n_inv_parms = n_inv_parms+vgrid(igrid,vtype)%nnode
        n_inv_active= n_inv_active+count(vgrid(igrid,vtype)%active)
     end do

  endif

  read (1,*) nifaces
  n_inv_iface=nifaces

  if (n_inv_iface > 0) then

     read(1,*) input_list(1:nifaces)

     allocate(ifaces_to_be_inv(nifaces))

     do i=1,nifaces
!        read(1,*) iiface
        iiface=input_list(i)
        ifaces_to_be_inv(i)=iiface
        intrface(iiface)%to_be_inverted = .true.
        group=group+1
        if (group == 1 ) then
           intrface(iiface)%start_index=1
        else
           intrface(iiface)%start_index=last_index
        endif
        last_index = intrface(iiface)%start_index + intrface(iiface)%nnode
        n_inv_parms = n_inv_parms + intrface(iiface)%nnode
        n_inv_active= n_inv_active + intrface(iiface)%nnode
     end do

  endif

  read(1,*) nsources
  if (nsources > 0) then ; locate_source=.true.; else ; locate_source=.false. ; endif

  if (locate_source) then

     n_inv_source=nsources

     read(1,*) input_list(1:nsources)

     allocate(sources_to_be_inv(nsources))
     do i=1,nsources
        isource = input_list(i)
        sources_to_be_inv(i)=isource
        source(isource)%to_be_inverted = .true.
        group=group+1
        if (group == 1) then
           source(isource)%start_index=1
        else
           source(isource)%start_index=last_index 
        endif
        last_index = source(isource)%start_index + 4
        n_inv_parms =n_inv_parms+4
        n_inv_active=n_inv_active+4
        j=4
     end do
  end if

  close(1)

  return


end subroutine initialize_inversion


!*****************************************************

! this subroutine takes a ray as input and produces the partial derivatives of the
! arrival time of this ray with respect to the velocity grids it encounters
! partial derivatives are stored in compact row storage as part of the ray derived type

subroutine ray_partials(ray)
  use mod_3dfm
  implicit none

  real(kind=dp)                                     :: r,lat,long
  type(Tray)                                        :: ray
  type(Tray_section),pointer                        :: raysec,raysec_prev
  type(Tvelocity_grid), pointer                     :: gridv
  type(Tintersection),pointer                       :: isec
  type(Tinterface),pointer                          :: iface
  integer, dimension(:),pointer                     :: regid

  real(kind=dp),dimension(:),allocatable             :: dtdpar,r_interface
  logical,dimension(:),allocatable                   :: iface_pinched

  integer                            :: start_index,glob_inv_index

  integer        :: i,j,k,ii,jj,kk,ir,ilat,ilong,n,m,nr,nlat,nlatnr,icell,ninodes,n_pinched_interfaces
  integer        :: iftop,ifbot,vtype
  real(kind=dp) :: u,v,w,bu(4),bv(4),bw(4),vel,weight,dl,pinch_modifier
  real(kind=dp) :: dpos(3),gradt_in(3),normal(3),h,gw(20),norm,dist,distmin
  real(kind=dp) :: gradt_in_perp,gradt_out_perp,gradt_in_par,geo_factor,vel_out,det,vel_in
  real(kind=dp) :: interpolate_velocity,interpolate_interface
  logical       :: rec_region_done,do_interface_partials

  ! allocate a temporary array to store the row in the inversion matrix corresponding
  ! to this ray. At the end the zeros will be removed by conversion to CRS

  allocate(dtdpar(n_inv_parms))
  dtdpar=0.0_dp

 ! some local arrays to assist with the identification of pinched interfaces
  allocate(r_interface(n_interfaces),iface_pinched(n_interfaces))

 ! raysec_prev is a pointer to the previous ray section in the sense of ray path integration

  nullify(raysec_prev)

 ! these are used to suppress interface derivative calculation in the receiver section
  rec_region_done = .false.
  do_interface_partials = .false.

  ! loop over the sections of this ray starting at receiver

  do m=ray%nsections,1,-1

!     print *,'ray_partial: section',m

     raysec => ray%section(m)

   ! if the ray section contains only two points (start and end) it is considered to lie
   ! between pinched interfaces, and does not influence the arrival time

     if (raysec%npoints <= 2) then

        cycle    ! go to the next section in the sense of path integration, the previous in the time field sequence

     else

   ! for the section starting at the receiver, there are no partial derivatives at the starting interface
   ! also set the previous section to the first significant (not pinched) section on the ray

        if (.not. rec_region_done) then
           raysec_prev => raysec
           rec_region_done = .true.
        else
           do_interface_partials = .true.
        endif

!        print *,'more than 2 points'

     ! first the partial derivatives with respect to the velocity grid

        if (raysec%tf%vtype == 0) then
           print *,raysec%tf%vtype
           print *,raysec%tf%id,raysec%tf%reg%id,raysec%tf%istart%id
           stop 'illegal vtype in ray_partials'
        endif

        vtype = raysec%tf%vtype
        gridv => vgrid(raysec%reg%ivgrid,vtype)   ! the velocity grid that applies to this section


        if (gridv%to_be_inverted) then

!           print *,'vgrid partials for section',m,'velocity grid',raysec%reg%ivgrid

           start_index = gridv%start_index     ! the starting index of the velocity grid parameters in the global list

           nr= gridv%nr
           nlat = gridv%nlat
           nlatnr=nlat*nr


           do n=2,raysec%npoints

              r    =  (raysec%point(1,n)+raysec%point(1,n-1))*0.5_dp
              lat  =  (raysec%point(2,n)+raysec%point(2,n-1))*0.5_dp
              long =  (raysec%point(3,n)+raysec%point(3,n-1))*0.5_dp


              dl=sqrt(  (raysec%point(1,n)-raysec%point(1,n-1))**2 + &
                   (r*(raysec%point(2,n)-raysec%point(2,n-1)))**2 + &
                   (r*cos(lat)*(raysec%point(3,n)-raysec%point(3,n-1)))**2 )



              ir=floor((r-gridv%r0)/gridv%dr0)+1
              ilat=floor((lat-gridv%lat0)/gridv%dlat0)+1
              ilong=floor((long-gridv%long0)/gridv%dlong0)+1

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


              vel=0.0_dp

              do k=1,4
                 kk=ilong+k-2
                 do j=1,4
                    jj=ilat+j-2
                    do i=1,4
                       ii=ir+i-2

                       weight=bu(i)*bv(j)*bw(k)
                       vel = vel + weight*gridv%velocity(ii,jj,kk)

                    end do
                 end do
              end do

              do k=1,4
                 kk=ilong+k-2
                 do j=1,4
                    jj=ilat+j-2
                    do i=1,4
                       ii=ir+i-2

                       weight=bu(i)*bv(j)*bw(k)

                       glob_inv_index=start_index + nlatnr*(kk-1) + nr*(jj-1) + (ii-1)

                       dtdpar(glob_inv_index) = dtdpar(glob_inv_index) - dl*weight/vel**2

                    end do
                 end do
              end do


           end do  ! ray section points loop

!        print *,'vgrid partials for section',m,'finished'

        endif    ! vgrid is to be inverted


     ! now the partial derivatives with respect to interface position

        if (do_interface_partials) then   

!           print *,'considering interface partials for section',m

           isec => raysec%istart
           iface => intrface(isec%iface_id)

           if (iface%to_be_inverted) then

!           print *,'interface partials for iface',iface%id,raysec_prev%reg%id,raysec%reg%id


        ! the first point of the ray section is the intersection point

           r    =  raysec%point(1,1)
           lat  =  raysec%point(2,1)
           long =  raysec%point(3,1)



       ! test if interfaces are pinched at the position the ray hits 

           iface_pinched = .false.  ! elements of this array will be set to .true. if pinched at this position

           if (raysec%reg%id /= raysec_prev%reg%id) then

           ! if it is a refraction, we can use the current and previous region to identify
           ! pinched interfaces

              n_pinched_interfaces = abs(raysec_prev%reg%id - raysec%reg%id)
              iftop = min(raysec_prev%reg%id,raysec%reg%id) + 1
              ifbot = max(raysec_prev%reg%id,raysec%reg%id)
              iface_pinched(iftop:ifbot) = .true. 

           else

            ! if it is a reflection, can't use path to identify pinched interface
            ! must do it directly

              do i=1,n_interfaces
                 r_interface(i)=interpolate_interface(lat,long,intrface(i))
              end do

              n_pinched_interfaces = 0

              do i=1,n_interfaces

                 if (abs(r_interface(i)-r_interface(iface%id)) < pgrid%tolerance) then

                    n_pinched_interfaces=n_pinched_interfaces+1
                    iface_pinched(i) = .true.

                 endif

              end do             

           endif


       ! the frechet derivatives will be divided by this modifier to account for pinched interfaces

           pinch_modifier = dble(count(iface_pinched(1:n_interfaces) .and. intrface(1:n_interfaces)%to_be_inverted))

!           print *,'pinch modifier is',pinch_modifier


              start_index = iface%start_index ! starting index of the intrface parameters in the global list
              nlat=iface%nlat

           ! first interpolate the value of the incoming time gradient at the interface

           ! determine if interface is at the top or bottom of the region

              if (isec%id == raysec%reg%itop%id) then
                 regid => isec%rbel_node_id
              else
                 if (isec%id == raysec%reg%ibot%id) then
                    regid => isec%rabo_node_id
                 else
                    stop 'error a in ray partials'
                 endif
              endif


           ! get the cut cell

              ir=min(pgrid%nr-1,floor((r-pgrid%r0)/pgrid%dr0)+1)
              ilat=min(pgrid%nlat-1,floor((lat-pgrid%lat0)/pgrid%dlat0)+1)
              ilong=min(pgrid%nlong-1,floor((long-pgrid%long0)/pgrid%dlong0)+1)


              if (associated(pgrid%ccind_from_3dc(ir,ilat,ilong)%p)) then

            ! if the first point lies in a cut cell (always except problematic cases)

                 icell = pgrid%ccind_from_3dc(ir,ilat,ilong)%p(isec%iface_id)

                 if (icell == 0)  stop 'error 2 in ray partials'


           ! interpolate based on inodes of the cut cell

                 ninodes=isec%n_inodes(icell)
                 do j=1,ninodes
                    gw(j)=1.d0/(sqrt((r-isec%r(isec%inodes(j,icell)))**2 + &
                         (r*(lat-isec%lat(isec%inodes(j,icell))))**2 &
                         +(r*cos(lat)*(long-isec%long(isec%inodes(j,icell)))**2)) + 0.05_dp*pgrid%dr0)
                 end do

                 gw(1:ninodes) = gw(1:ninodes)/sum(gw(1:ninodes))

                 gradt_in=0.0_dp
                 do j=1,ninodes
                    gradt_in = gradt_in + gw(j)*raysec%tf%time_gradient(1:3,regid(isec%inodes(j,icell)))
                 end do

              else

            ! if the first point does not lie in a cut cell (interfaces crossing back and
            ! forth along a coordinate plane on a scale less than the grid spacing)
            ! take the gradient at the closest point

                 distmin=10.0*earth_radius**2
                 do j=1,raysec%reg%nnode
                    dist=(r-raysec%reg%r(j))**2 + (r*(lat-raysec%reg%lat(j)))**2 + &
                         (r*cos(lat)*(long-raysec%reg%long(j)))**2
                    if (dist < distmin) then
                       distmin=dist
                       jj=j
                    endif
                 end do

                 gradt_in=raysec%tf%time_gradient(1:3,jj)

              endif

!         ensure the interpolated time gradient has the correct norm

              vel_in = interpolate_velocity(r,lat,long,vgrid(raysec%reg%ivgrid,vtype))
              norm = sqrt(sum(gradt_in**2))
              gradt_in = gradt_in/(vel_in*norm)

!           write(24,*) 'gin',gradt_in

           ! we have the incoming time gradient, now the surface normal

              call interface_normal(lat,long,intrface(isec%iface_id),normal(1),normal(2),normal(3),h)

!           write(24,*) 'nor',normal

           ! the component of the incoming time gradient normal to the surface 

              gradt_in_perp = dot_product(gradt_in,normal)


           ! the component of the outgoing time gradient normal to the surface 
        
              if (raysec%reg%id == raysec_prev%reg%id)  then   ! reflection

                 gradt_out_perp = -gradt_in_perp

              else                                                  ! refraction

                 gradt_in_par=sqrt(sum((gradt_in-gradt_in_perp*normal)**2))
              
                 vel_out=interpolate_velocity(r,lat,long,vgrid(raysec_prev%reg%ivgrid,raysec_prev%tf%vtype))

                 det = 1.0_dp/vel_out**2 - gradt_in_par**2
                 if (det < 0.0_dp) then

                    print *, 'warning 3 in ray partials : violating total reflection'
                    print *, 'section',m
                    print *, 'region_in',raysec%tf%reg%id,'region_out',raysec_prev%tf%reg%id
                    print *, 'vel_in',vel_in,'vel_out',vel_out
                    print *, det,1.0_dp/vel_out**2,gradt_in_par**2
                    print *, gradt_in
                    print *, gradt_in_perp*normal
                    print *, 1.0/sqrt(sum(gradt_in**2)),1.0/gradt_in_perp,1.0/sqrt(gradt_in_perp**2+gradt_in_par**2)
                    print *, 1.0/gradt_in_par,vel_out
!                    stop 'error 3 in ray partials : violating total reflection'
                    det = 0.0_dp
                 endif

                 gradt_out_perp = sign(sqrt(det),gradt_in_perp)

              endif


           ! finally we can calculate the geometrical factor

              geo_factor=(gradt_in_perp-gradt_out_perp)*normal(1)


           ! now calculate the partial derivatives

              ilat=floor((lat-iface%lat0)/iface%dlat0)+1
              ilong=floor((long-iface%long0)/iface%dlong0)+1

              v=(lat-iface%lat(ilat))/iface%dlat0
              w=(long-iface%long(ilong))/iface%dlong0

              bv(1)=(1.0_dp-v)**3/6.0_dp
              bv(2)=(4.0_dp-6.0_dp*v**2+3.0_dp*v**3)/6.0_dp
              bv(3)=(1.0_dp+3.0*v+3.0_dp*v**2-3.0_dp*v**3)/6.0_dp
              bv(4)=v**3/6.0_dp
              bw(1)=(1.0_dp-w)**3/6.0_dp
              bw(2)=(4.0_dp-6.0_dp*w**2+3.0_dp*w**3)/6.0_dp
              bw(3)=(1.0_dp+3.0*w+3.0_dp*w**2-3.0_dp*w**3)/6.0_dp
              bw(4)=w**3/6.0_dp


              do k=1,4
                 kk=ilong+k-2
                 do j=1,4
                    jj=ilat+j-2

                    weight=bv(j)*bw(k)

                    do n=1,n_interfaces

                       if (iface_pinched(n) .and. intrface(n)%to_be_inverted) then

                          glob_inv_index=intrface(n)%start_index + nlat*(kk-1) + (jj-1)

                          dtdpar(glob_inv_index) = dtdpar(glob_inv_index) + weight*geo_factor/pinch_modifier

                       endif

                    end do

                 end do
              end do
   
           endif   ! iface to be inverted

        endif  ! section is not the first significant (first section considered starts at receiver, not an interface)

        raysec_prev => raysec  ! points to the last significant (non-pinched) section along the ray

     endif   ! interfaces not pinched at position ray passes through

  end do ! ray sections loop

!  print *,'finished ray section loop'

  ! the partial derivatives with respect to source position and time (source is last point on the ray)

  if (locate_source) then

     if (ray%source%to_be_inverted) then

        n=ray%section(1)%npoints

        dpos(1)= ray%section(1)%point(1,n)-raysec%point(1,n-1)
        dpos(2)= ray%section(1)%point(1,n)*(ray%section(1)%point(2,n)-raysec%point(2,n-1))
        dpos(3)= ray%section(1)%point(1,n)*cos(ray%section(1)%point(2,n))*(ray%section(1)%point(3,n)-raysec%point(3,n-1))

        dpos=dpos/sqrt(sum(dpos**2))

        vel=interpolate_velocity(ray%section(1)%point(1,n),ray%section(1)%point(2,n), &
             ray%section(1)%point(3,n),vgrid(ray%section(1)%reg%ivgrid,ray%section(1)%tf%vtype))


        glob_inv_index=ray%source%start_index

        dtdpar(glob_inv_index:glob_inv_index+2)=dpos/vel

        dtdpar(glob_inv_index+3) = -1.0_dp

     endif

  endif


! store the partial derivatives for this ray (row in inversion matrix)) in CRS

  ray%n_pdev = count(dtdpar /= 0.0_dp)
  allocate(ray%pdev(ray%n_pdev),ray%pdev_indx(ray%n_pdev))
  m=0
  do n=1,n_inv_parms
     if (dtdpar(n) /= 0.0_dp) then
        m=m+1
        ray%pdev(m) = dtdpar(n)
        ray%pdev_indx(m)=n
     endif
  end do

  deallocate(dtdpar)

  return

end subroutine ray_partials

!*************************************************************************************************

subroutine decode_global_index(glob_index,i1,i2,i3,p_type,p_index,p_subindex)
  use mod_3dfm
  implicit none

  integer                       ::  glob_index,i1,i2,i3,p_type,p_index,p_subindex,n_groups
  integer                       ::  start(31)
  integer                       ::  i,n,m,group,nr,nlat,loc_index

  p_subindex = 0

  n_groups=n_inv_vgrid+n_inv_iface

  if (locate_source) n_groups=n_groups+n_inv_source

  if (n_groups > 30) stop 'too many groups in decode_global_index'

  m=0
  do i=1,n_inv_vgrid
     m=m+1
     start(m)=vgrid(vgrids_to_be_inv(i)%igrid,vgrids_to_be_inv(i)%vtype)%start_index
  end do

  do i=1,n_inv_iface
     m=m+1
     start(m)=intrface(ifaces_to_be_inv(i))%start_index
  end do

  if (locate_source) then
     do i=1,n_inv_source
        m=m+1
        start(m)=source(sources_to_be_inv(i))%start_index
     end do
  endif

  start(n_groups+1)=n_inv_parms+1

  do n=1,n_groups
     if (glob_index < start(n+1)) then
        group=n
        exit
     endif 
  end do

  if (group <= n_inv_vgrid) then

!     write(24,*) 'group',group,start(1:n_groups)

     p_type=1
     p_index=vgrids_to_be_inv(group)%igrid
     p_subindex=vgrids_to_be_inv(group)%vtype
     
     nr=vgrid(p_index,p_subindex)%nr
     nlat=vgrid(p_index,p_subindex)%nlat

     loc_index=glob_index-start(group)
     i3= loc_index/(nlat*nr) +1                      
     i2=(loc_index-(i3-1)*nlat*nr)/nr + 1            
     i1= loc_index-(i3-1)*nlat*nr-(i2-1)*nr + 1    


     return

  endif

  if (group <= (n_inv_vgrid+n_inv_iface) ) then

!     write(24,*) 'group',group,start(1:n_groups)

     p_type=2
     p_index=ifaces_to_be_inv(group-n_inv_vgrid)
     
     nlat=intrface(p_index)%nlat

     loc_index=glob_index-start(group)
     i3= loc_index/nlat + 1                          
     i2= loc_index-(i3-1)*nlat + 1                   
     i1=0

     return

  endif

  if (group <= n_groups ) then

     p_type=3
     p_index=sources_to_be_inv(group-n_inv_vgrid-n_inv_iface)

     loc_index=glob_index-start(group)
     i1=loc_index+1 ; i2=0 ; i3=0

     return

  endif

  stop 'something wrong in decode_global_index'

end subroutine decode_global_index

!*********************************************************************************************
! this subroutine sets a flag at the nodes of the velocity grids belonging to a region
! to true if the velocity grid node actually influences the velocity field in the region

subroutine tag_active_vgrid_nodes(reg)

  use mod_3dfm
  implicit none

  type(Tregion)  :: reg
  type(Tvelocity_grid),pointer :: gridv

  integer :: ir,ilat,ilong,n,vtype


  do vtype=1,n_vtypes

     gridv => vgrid(reg%ivgrid,vtype) 

     do n=1,reg%nnode

        ir=floor((reg%r(n)-gridv%r0)/gridv%dr0)+1
        ilat=floor((reg%lat(n)-gridv%lat0)/gridv%dlat0)+1
        ilong=floor((reg%long(n)-gridv%long0)/gridv%dlong0)+1

        gridv%active(ir-1:ir+2,ilat-1:ilat+2,ilong-1:ilong+2) = .true.

     end do  ! regional node loop

  end do  ! vtypes

end subroutine tag_active_vgrid_nodes
