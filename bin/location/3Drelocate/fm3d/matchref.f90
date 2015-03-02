subroutine match_reflections(interface_id,tf1,tf2,extrema,n_extrema,s,sr)

! this subroutine finds stationary points of a travel time field on an intersection

  use mod_3dfm
  implicit none

  integer                                    :: interface_id
  type(Ttime_field)                          :: tf1,tf2
  real(kind=dp)                              :: extrema(3,*)
  integer                                    :: n_extrema
  type(Tintersection),pointer                :: isec    
  type(Tsource)                              :: s,sr
  real(kind=dp),dimension(:,:),allocatable   :: tgsum,tg1,tg2
  real(kind=dp),dimension(:),allocatable     :: tsum,tgnorm
  logical,dimension(:),allocatable           :: node_invalid,node_is_locmin,node_is_tmin
  integer                                    :: n,i,nnode,ii
  integer                                    :: ind_start,ind_end


  type(Ttriangulation)                       :: tri
  real(kind=dp)                              :: norm(3),vec(3)
  real(kind=dp)                              :: dotp
  real(kind=dp)                              :: deg_to_rad
  real(kind=dp)                              :: locmin_ratio 
  integer                                    :: iv,ivv,ip,ip1,ipp1,it,it1,itt,itt1
  

  locmin_ratio = 0.05_dp


! some validation of the input parameters

  if (tf2%reg%id /= tf1%reg%id ) stop 'regions of specified time fields unmatched in match_reflection'
  if (interface_id /= tf1%reg%itop%iface_id .and. interface_id /= tf1%reg%ibot%iface_id)  &
       stop 'illegal interface in match_reflection'


  deg_to_rad = acos(-1.0_dp)/180.0_dp


! 
! get the gradients for the fitting interface from the regional time gradient array

  isec => intersection(interface_id)
  nnode = isec%nnode
  allocate(tgsum(3,nnode),tg1(3,nnode),tg2(3,nnode),node_invalid(nnode),tsum(nnode),tgnorm(nnode),&
       node_is_locmin(nnode),node_is_tmin(nnode))

  if (interface_id == tf1%reg%itop%iface_id) then

     ind_start = tf1%reg%ngnode + 1
     ind_end = ind_start + nnode - 1

  else

     ind_end = tf1%reg%nnode
     ind_start = ind_end - nnode + 1

  endif

  tg1=tf1%time_gradient(1:3,ind_start:ind_end)
  tg2=tf2%time_gradient(1:3,ind_start:ind_end)
  tgsum=tg1+tg2
  tsum = tf1%arrivaltime(ind_start:ind_end) + tf2%arrivaltime(ind_start:ind_end)

! construct the norm of the parallel time gradient

  do i=1,nnode

!     call interface_normal(isec%lat(i),isec%long(i),intrface(interface_id),norm(1),norm(2),norm(3),h)
     norm = isec%normal(1:3,i)
     dotp=dot_product(norm,tgsum(1:3,i))
     vec(1:3) = tgsum(1:3,i) - dotp*norm(1:3)
     tgnorm(i)=sqrt(sum(vec**2))

  end do


! set nodes too close to the source or receiver as invalid since no valid time gradient may be available

  do n=1,nnode

     node_invalid(n) = (sqrt( (s%r-isec%r(n))**2 + (isec%r(n)*(s%lat-isec%lat(n)))**2 + &
          (isec%r(n)*(s%long-isec%long(n)))**2) < isec%r(n)*pgrid%dlat0) .or. &
          (sqrt( (sr%r-isec%r(n))**2 + (isec%r(n)*(sr%lat-isec%lat(n)))**2 + &
          (isec%r(n)*(sr%long-isec%long(n)))**2) < isec%r(n)*pgrid%dlat0) 

  end do


! now do a Delaunay triangulation of the intersection nodes

  call triangulation_defaults(tri)
  tri%npoints=nnode
  allocate(tri%points(2,nnode))
  tri%points(1,:) = isec%lat
  tri%points(2,:) = isec%long

  call triangulate(tri)


! find the modes that are local minima of tgnorm

  node_is_locmin=.true.

tgminloop:  do ip=1,nnode    ! all nodes

     do it=1,tri%n_triangles_from_point(ip)   ! triangles connected to zero level node

        it1=tri%triangles_from_point(it,ip)

     ! don't consider the node of a conncted triangle if on the edge (may be artifcial minimum)

        if (count(tri%triangle_neighbours(1:3,it1) == 0) /= 0 ) then
           node_is_locmin(ip) = .false.
           cycle tgminloop
        endif

        do iv=1,3    ! first level nodes of triangles connected to zero level node

           ip1 = tri%points_from_triangle(iv,it1)

           if (tgnorm(ip1) < tgnorm(ip)) then
              node_is_locmin(ip) = .false.
              cycle tgminloop
           endif

           do itt=1,tri%n_triangles_from_point(ip1) ! triangles connected to first level nodes

              itt1=tri%triangles_from_point(itt,ip1)

              do ivv=1,3    ! second level nodes of triangles connected to first level nodes

                 ipp1 = tri%points_from_triangle(ivv,itt1)

                 if (tgnorm(ipp1) < tgnorm(ip)) then
                    node_is_locmin(ip) = .false.
                    cycle tgminloop
                 endif

              end do

           end do

        end do

     end do

  end do  tgminloop



! find the modes that are local minima of tsum

  node_is_tmin=.true.

minloop:  do ip=1,nnode    ! all nodes

     do it=1,tri%n_triangles_from_point(ip)   ! triangles connected to zero level node

        it1=tri%triangles_from_point(it,ip)

     ! don't consider the node of a conncted triangle if on the edge (may be artifcial minimum)

        if (count(tri%triangle_neighbours(1:3,it1) == 0) /= 0 ) then
           node_is_locmin(ip) = .false.
           cycle minloop
        endif

        do iv=1,3    ! first level nodes of triangles connected to zero level node

           ip1 = tri%points_from_triangle(iv,it1)

           if (tsum(ip1) < tsum(ip)) then
              node_is_tmin(ip) = .false.
              cycle minloop
           endif

           do itt=1,tri%n_triangles_from_point(ip1) ! triangles connected to first level nodes

              itt1=tri%triangles_from_point(itt,ip1)

              do ivv=1,3    ! second level nodes of triangles connected to first level nodes

                 ipp1 = tri%points_from_triangle(ivv,itt1)

                 if (tsum(ipp1) < tsum(ip)) then
                    node_is_tmin(ip) = .false.
                    cycle minloop
                 endif

              end do

           end do

        end do

     end do

  end do  minloop

! select further to take only nodes with a sufficiently small value at the minimum and that are not
! right next to the source or receiver

do n=1,nnode
  node_is_locmin(n) = node_is_locmin(n) .and. (tgnorm(n) < locmin_ratio*(2.0_dp*sqrt(sum(tg1(1:3,n)**2)))) &
       .and. .not.node_invalid(n)
end do

do n=1,nnode
  node_is_tmin(n) = node_is_tmin(n) .and. .not.node_invalid(n)
end do

! copy the reflection points to the output array

   n_extrema=count(node_is_locmin)

   if (n_extrema > 0) then

      ii=0
      do n=1,nnode
         if(node_is_locmin(n)) then
            ii=ii+1
            extrema(1,ii)= isec%r(n)
            extrema(2,ii)= isec%lat(n)
            extrema(3,ii)= isec%long(n)

         endif
      end do

   else

      return

   endif

  deallocate(tgsum,tg1,tg2,node_invalid,tsum,tgnorm,node_is_locmin,node_is_tmin)
  call clean_triangulation(tri)

  return

end subroutine match_reflections

!***************************************************************************************************
subroutine trace_reflectionfit(n,m)

! this subroutine generates the ray paths for a phase that includes a late reflection search

  use mod_3dfm

  integer   :: i,k,m,n,ifit,nsec,npsec,iref,recpath_id,path_id,nsec_comp

  type(Tsource),pointer                  :: s,ss
  type(Treceiver)                        :: receiver_at_reflection
  type(Tray),pointer                     :: ray,subray
  type(Tray)                             :: ray_to_src,ray_to_rec
  type(Tray_section),pointer             :: raysec,subsec
  type(Ttime_field),pointer              :: tf1,tf2
  real(kind=dp)                          :: reflections(3,10),deg_to_rad,t_arrival
  integer                                :: n_reflections

  deg_to_rad = acos(-1.0_dp)/180.0_dp

  ray => receiver(n)%ray(m)
  s   => ray%source
  path_id = ray%raypath_id

  ss   => source(receiver(n)%source_equivalent)
  recpath_id = receiver(n)%path_equivalent(m)
  tf1  => s%time_field(s%path(path_id)%tf_sequence(s%path(path_id)%n_tf))
  tf2  => ss%time_field(ss%path(recpath_id)%tf_sequence(ss%path(recpath_id)%n_tf))
  ifit =  s%path(path_id)%fitting_interface

!  print *,'calling match-reflections with'
!  print *,'source 1',s%id
!  print *,'path 1',path_id
!  print *,'tf 1',tf1%id
!  print *,'source 2',ss%id
!  print *,'path 2',recpath_id
!  print *,'tf 2',tf2%id

  call ray_defaults(ray_to_rec)
  call ray_defaults(ray_to_src)

  call match_reflections(ifit,tf1,tf2,reflections,n_reflections,s,ss)

  if (n_reflections > 0) print '(a5,i3,a12,i3,a3,i5,a20)', &
       'ray',m,'from source',s%id, ':',n_reflections,'reflection points'

  if (n_reflections == 0) then
     print '(a12,i4,a10,i4,a15,i4,a23)','ray',m,'to source',ray%source_id,&
          ' from receiver',n,' : no reflections found' 
     ray%valid = .false.

     k=0
     t_arrival=-1.0_dp               
     write(11,'(4i6,f15.6,2l5)') n,ray%source_id,m,k,t_arrival,ray%diffracted,ray%headwave

     return   ! go to next ray
  endif


! if there are reflections, the array of subrays within the ray associated with the path
! is allocated. The subrays will contain raypaths from all the reflection points found.

  if (n_reflections > 0) then
     ray%is_multiray = .true.
     ray%n_subrays = n_reflections
     allocate(ray%subray(n_reflections))
     do i=1,n_reflections ; call ray_defaults(ray%subray(i)) ; end do
  endif

!  define the path of the two rays from reflection point to source and receiver

  ray_to_src%source     => s
  ray_to_src%raypath_id = path_id
  ray_to_rec%source     => ss
  ray_to_rec%raypath_id = recpath_id


  do iref=1,n_reflections

     subray => ray%subray(iref)

!  create a receiver at the reflection point

     receiver_at_reflection%r    = reflections(1,iref)
     receiver_at_reflection%lat  = reflections(2,iref)
     receiver_at_reflection%long = reflections(3,iref)

     ray_to_src%valid = .true.
     ray_to_rec%valid = .true.

!  trace ray from reflection point to source


     call trace_ray_from_receiver(receiver_at_reflection,s,ray_to_src)
     if (.not.ray_to_src%valid) then   ! no valid ray path found
        print *,'subray from reflection point',iref,' to source was not valid'
        ray%subray(iref)%valid = .false.

        k=0
        t_arrival=-1.0_dp               
        write(11,'(4i6,f15.6,2l5)') n,ray%source_id,m,iref,t_arrival,subray%diffracted,subray%headwave

        cycle
     endif
   
!  trace ray from reflection point to receiver

     call trace_ray_from_receiver(receiver_at_reflection,ss,ray_to_rec)
     if (.not.ray_to_rec%valid) then   ! no valid ray path found
        print *,'subray from reflection point',iref,' to receiver was not valid'
        ray%subray(iref)%valid = .false.

        k=0
        t_arrival=-1.0_dp               
        write(11,'(4i6,f15.6,2l5)') n,ray%source_id,m,iref,t_arrival,subray%diffracted,subray%headwave

        cycle
     endif

  
! assemble total ray from the two pieces

  ! first assign the attributes of the composite ray

     subray%valid = .true.
     subray%source => s
     subray%raypath_id = path_id
     subray%nsections=ray_to_src%nsections+ray_to_rec%nsections
     subray%source_id = s%id
     subray%receiver_time = ray_to_src%receiver_time + ray_to_rec%receiver_time 
     subray%receiver_time_gradient(1:3) = 0.0_dp
     subray%diffracted = ray_to_src%diffracted .or. ray_to_rec%diffracted 
     subray%headwave = ray_to_src%headwave .or. ray_to_rec%headwave

     allocate(subray%section(subray%nsections))
     do i=1,subray%nsections ; call ray_section_defaults(subray%section(i)) ; end do

  ! transfer the ray sections from the reflection point to receiver part
  ! here the direction of travel has to be inverted

     do nsec=ray_to_rec%nsections,1,-1

        nsec_comp = ray_to_src%nsections + ray_to_rec%nsections - nsec + 1
        npsec = ray_to_rec%section(nsec)%npoints

        allocate(subray%section(nsec_comp)%point(3,npsec))

        raysec => ray_to_rec%section(nsec)
        subsec => subray%section(nsec_comp)

        subsec%ray => subray
        subsec%reg => raysec%reg 
        subsec%istart => raysec%iend
        subsec%iend => raysec%istart
        subsec%tf => raysec%tf
        subsec%source => raysec%source
        subsec%npoints  =  raysec%npoints      
        subsec%place_in_sequence  = nsec_comp                   
        subsec%diffracted = raysec%diffracted
        subsec%headwave = raysec%headwave

        do i=1,subsec%npoints
           subsec%point(1:3,i) = raysec%point( 1:3 , raysec%npoints - i + 1)
        end do

        deallocate(raysec%point)

     end do

  ! transfer the ray sections  from the reflection point to source part
  ! here the direction of travel is the same 

     do nsec=ray_to_src%nsections,1,-1

        nsec_comp = nsec 
        npsec = ray_to_src%section(nsec)%npoints

        allocate(subray%section(nsec_comp)%point(3,npsec))

        raysec => ray_to_src%section(nsec)
        subsec => subray%section(nsec_comp)

        subsec%ray => subray
        subsec%reg => raysec%reg 
        subsec%istart => raysec%istart
        subsec%iend => raysec%iend
        subsec%tf => raysec%tf
        subsec%source => raysec%source
        subsec%npoints  =  raysec%npoints      
        subsec%place_in_sequence  = nsec_comp                 
        subsec%diffracted = raysec%diffracted
        subsec%headwave = raysec%headwave

        do i=1,subsec%npoints
           subsec%point(1:3,i) = raysec%point(1:3,i)
        end do

        deallocate(raysec%point)

     end do

   ! have to set the starting interface of the ray section from the reflection point to the source
   ! to the reflection interface 

     subray%section(ray_to_src%nsections)%istart => intersection(ifit)


     deallocate(ray_to_src%section,ray_to_rec%section)

     print '(a12,i4,a10,i4,a15,i4,a8,i3,a4,f10.5,2l3,3f10.4)','traced ray',m,'to source',ray%source_id, &
          ' from receiver',n,'subray ',iref,'  t=', subray%receiver_time, subray%diffracted,subray%headwave, &
          reflections(1,iref), reflections(2:3,iref)/deg_to_rad

     write(11,'(4i6,f15.6,2l5)') n,ray%source_id,m,iref,subray%receiver_time,subray%diffracted,subray%headwave

     if (display_mode) call store_ray(subray)

  end do  ! loop over reflection points

  return

end subroutine trace_reflectionfit

!***************************************************************************************************
function interpolate_triangle(pos,field,vpos)

! linear interpolation in a triangle, the weight of each vertex is proportional to
! the distance from the point along the normal of the opposite side

  use mod_3dfm

  real(kind=dp)  :: pos(2),field(3),vpos(2,3),w(3),x(2),e(2),d(2),h
  real(kind=dp)  :: interpolate_triangle


  x = pos - vpos(1:2,2)
  e = vpos(1:2,3) - vpos(1:2,2)
  e = e/sqrt(sum(e**2))
  d = vpos(1:2,1) - vpos(1:2,2)
  h = sqrt(sum((d - dot_product(d,e)*e)**2))
  w(1)= sqrt(sum((x - dot_product(x,e)*e)**2))/h

  x = pos - vpos(1:2,3)
  e = vpos(1:2,1) - vpos(1:2,3)
  e = e/sqrt(sum(e**2))
  d = vpos(1:2,2) - vpos(1:2,3)
  h = sqrt(sum((d - dot_product(d,e)*e)**2))
  w(2)= sqrt(sum((x - dot_product(x,e)*e)**2))/h


  x = pos - vpos(1:2,1)
  e = vpos(1:2,2) - vpos(1:2,1)
  e = e/sqrt(sum(e**2))
  d = vpos(1:2,3) - vpos(1:2,1)
  h = sqrt(sum((d - dot_product(d,e)*e)**2))
  w(3)= sqrt(sum((x - dot_product(x,e)*e)**2))/h

  w=w/sum(w(1:3))

  h = dot_product(w,field)

  interpolate_triangle=dot_product(w,field)

  return

end function interpolate_triangle
