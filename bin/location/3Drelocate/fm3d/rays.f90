subroutine trace_ray_from_receiver(rec,s,ray)
  use mod_3dfm_nointerfaces
  implicit none

  type(Tsource),target         :: s                    ! the source of the ray
  type(Treceiver)              :: rec                  ! the receiver at which it arrives
  type(Tray),target            :: ray                  ! the ray to be traced

  real(kind=dp)                :: r_in,lat_in,long_in       ! starting points for each ray section
  real(kind=dp)                :: r_out,lat_out,long_out    ! end points of each ray section
  integer                      :: n,m,nsections,ntf,n_shift               ! counter, number of sections on the rays

  logical                      :: ldum=.false.
  logical                      :: outside
  real(kind=dp)                :: interpolate_interface

  nsections=s%path(ray%raypath_id)%n_tf
  ray%nsections=nsections
  allocate(ray%section(nsections))
  do n=1,nsections ; call ray_section_defaults(ray%section(n)) ; end do

  r_in = rec%r ; lat_in=rec%lat ; long_in=rec%long

! first fill in some info on all the ray sections

  do n=nsections,1,-1

     ray%section(n)%ray => ray
     ray%section(n)%source => s
     ray%section(n)%place_in_sequence = n

     ntf = s%path(ray%raypath_id)%tf_sequence(n)
     if (ntf > 0) then
        ray%section(n)%tf => s%time_field(ntf)
     else
        print *,'tracing ray with 1 timefield'
        if (nsections /= 1 ) stop 'error 1 in trace ray from receiver'
        if (rec%r >= interpolate_interface(rec%lat,rec%long,intrface(s%topint_id))) ntf=1
        if (rec%r <  interpolate_interface(rec%lat,rec%long,intrface(s%botint_id))) ntf=2
        ray%section(n)%tf => s%time_field(ntf)
     endif

     ray%section(n)%reg => ray%section(n)%tf%reg
     if (n < nsections) ray%section(n)%istart => intersection(s%path(ray%raypath_id)%sequence(2*n+1))
     if (n > 1) ray%section(n)%iend => intersection(s%path(ray%raypath_id)%sequence(2*n-1))
     if (n == 1 .and. s%is_teleseismic) ray%section(n)%iend => intersection(n_intersections)
     if (n < nsections) ray%section(n)%istart => ray%section(n+1)%iend

  end do
! now do the actual integration section by section

  do n=nsections,1,-1

     if (n == nsections) then  ! start by storing the time and direction of arrival of this ray at the receiver

        call interpolate_arrivaltime(ray%section(n)%tf,rec%r,rec%lat,rec%long,ray%receiver_time)
        if (ray%receiver_time<0) return !AAA

        call interpolate_time_gradient(ray%section(n)%tf,rec%r,rec%lat,rec%long,&
             ray%receiver_time_gradient(1),ray%receiver_time_gradient(2),ray%receiver_time_gradient(3), &
             ldum,outside,ray%section(n))
     endif

     call trace_ray_section(ray%section(n),r_in,lat_in,long_in,r_out,lat_out,long_out,outside)

     if (outside) exit  ! exit if the ray path has gone through the sides of the box

     if (.not.ray%valid) exit

     ! make end points the starting points of the next section unless it was the last section
     if (n > 1) then
        r_in = r_out ; lat_in=lat_out ; long_in=long_out
     endif

  end do

  if (outside.and.s%is_local) stop 'error in ray tracing:ray outside grid for local source'

  if (outside.and.n>1) then
     
   ! if the ray from a teleseismic source has exited the box through the sides
   ! the info on the ray has to be modified, unless it was the last section....  

     print *,'ray exited grid before bottom region was reached'
     do m=n,nsections
        n_shift=m-n+1
        ray%section(n_shift)=ray%section(m)
        ray%section(n_shift)%place_in_sequence = n_shift
     end do
     ray%nsections=nsections-n+1

  endif


  return

end subroutine trace_ray_from_receiver

!***************************************************************************************************************

subroutine trace_ray_section(raysec,r_in,lat_in,long_in,r_out,lat_out,long_out,outside)
  use mod_3dfm
  implicit none

  type(Tray_section)                          :: raysec          ! the ray section
  type(Ttime_field),pointer                   :: tf              ! the time field to be used
  type(Ttime_field),pointer                   :: tf_next         ! the next time field as viewed 
                                                                 ! from the direction of tracing
  type(Tpropagation_grid),pointer            :: grid            ! the grid on which the time field is based
  type(Tinterface),pointer                    :: iftop,ifbot     ! the top, bottom interfaces of the time field 
  type(Tinterface),pointer                    :: ifend   ! the interface at which the ray section ends
  integer                                     :: ifstart_id,ifend_id
  type(Tsource),pointer                       :: s               ! the source of the ray
  real(kind=dp)                               :: r_in,lat_in,long_in   ! ray section start
  real(kind=dp)                               :: r_out,lat_out,long_out,lat,long   ! ray section end
  real(kind=dp)                               :: interpolate_interface ! external function

  real(kind=dp),allocatable,dimension(:,:)    :: pt     ! temporary storage for ray section
  real(kind=dp)                               :: pp(3),grad0(3),grad1(3),r_interface
  real(kind=dp)                               :: grad2(3),gtp_in,norm(3),h
  real(kind=dp)                               :: dlmax,dl ! max, actual step size of ray integration
  real(kind=dp)                               :: v_average = 5.0_dp   ! used to estimate max step size
  real(kind=dp)                               :: p_tolerance          ! stopping criterion for distance to interface
  real(kind=dp)                               :: s_tolerance          ! stopping criterion for distance to source
  real(kind=dp)                               :: x1,x2,f,fmid,xmid,dx,rtbis
  integer                                     :: maxpoints = 10000
  integer                                     :: n,nn,iter,seqnum,start_direction,i
  logical                                     :: section_is_refraction,outside

  logical  :: verbose

!  print *,'entering trace ray section'

  ! define some local pointers

  tf => raysec%tf
  iftop => intrface(tf%reg%itop%iface_id)
  ifbot => intrface(tf%reg%ibot%iface_id)
  grid => pgrid
  s => raysec%source

  seqnum=raysec%place_in_sequence

! a ray section with others before and after
  if (seqnum < raysec%ray%nsections .and.seqnum > 1 ) then 
     ifstart_id = raysec%istart%id
     ifend_id = raysec%iend%id
     ifend => intrface(ifend_id)
     tf_next => raysec%ray%section(seqnum-1)%tf
     start_direction = raysec%ray%section(seqnum+1)%reg%id - tf%reg%id
     section_is_refraction = raysec%ray%section(seqnum-1)%reg%id /= raysec%ray%section(seqnum)%reg%id
  endif

! a ray section that starts at the receiver with others after
  if (seqnum == raysec%ray%nsections .and. seqnum > 1) then 
     ifstart_id = 0
     ifend_id = raysec%iend%id
     ifend => intrface(ifend_id)
     tf_next => raysec%ray%section(seqnum-1)%tf
     start_direction = 0
     section_is_refraction = raysec%ray%section(seqnum-1)%reg%id /= raysec%ray%section(seqnum)%reg%id
  endif

! the last ray section to the source
  if (raysec%place_in_sequence == 1 ) then 

     if (s%is_local) then
        ifstart_id = 0
        ifend_id = 0
        nullify(ifend)
        nullify(tf_next)
        if (seqnum+1 <= raysec%ray%nsections) then
           start_direction = raysec%ray%section(seqnum+1)%reg%id - tf%reg%id
        else
           start_direction=0
        endif
     endif

     if (s%is_teleseismic) then
        ifstart_id = 0 ! raysec%istart%id
        ifend_id = raysec%iend%id
        ifend => intrface(ifend_id)
        nullify(tf_next) 
        if (seqnum+1 <= raysec%ray%nsections) then
           start_direction = raysec%ray%section(seqnum+1)%reg%id - tf%reg%id
        else
           start_direction=0
        endif
        section_is_refraction=.false.  ! must be set so that the next time field is not tested
                                       ! for total reflection
     endif

  endif

  ! test whether time field exists
  if (.not.associated(tf%time_gradient)) stop 'trace_ray_section: no timefield gradient'


  ! allocate local array for temporary storage of this ray section
  allocate(pt(3,maxpoints))

  dlmax=0.1_dp*grid%dr0*v_average   ! the maximum step size for the integration
  p_tolerance =5.e-4_dp*grid%dr0    ! maximum allowed difference between predictor and 
                                    ! predictor+corrector step in integration 
  s_tolerance = 1.0_dp*grid%dr0     ! distance to the source at which the integration stops

!  print *
!  print *
!  print '(a25,i5,3f15.7)','starting ray section',raysec%place_in_sequence,r_in,lat_in,long_in
!  print *,'into region',tf%reg%id, 'with top/bot iface',iftop%id,ifbot%id  !tf%reg%itop%id,tf%reg%ibot%id
!  print *,'using time field',tf%id
!  print *,'start/end ifaces',ifstart_id,ifend_id
!  print *,'s_tolerance',s_tolerance


! set starting point
  n=1
  pt(1,1)=r_in ; pt(2,1)=lat_in ; pt(3,1)=long_in 



ploop:  do

      ! find the next point 

           n=n+1

           if (n > maxpoints) then
              print *,'more than maxpoints in raysection'
              print '(i6,3f12.5)',n-1,pt(1:3,n-1)
              print '(a6,3f12.5)','source',s%r,s%lat,s%long
              stop
           endif

           dl=dlmax

           if (n ==0) then ; verbose =.true. ; else; verbose=.false. ; endif
            call interpolate_time_gradient(tf,pt(1,n-1),pt(2,n-1),pt(3,n-1),grad0(1),grad0(2),grad0(3) &
                ,verbose,outside,raysec)
            if (outside) then ; n=n-1 ; exit ploop ; endif
           if (.not.raysec%ray%valid) then; deallocate(pt) ; return ; endif
           verbose=.false.

           ! first guess at the position of the next point
           ! note: time gradients have units time/length, convert to angle for angular coordinates

           pp(1)=pt(1,n-1)-dl*grad0(1)
           pp(2)=pt(2,n-1)-dl*grad0(2)/pp(1)          
           pp(3)=pt(3,n-1)-dl*grad0(3)/(pp(1)*cos(pp(2)))


           ! now test if we can get into the next requested region at all

           if (n == 2 .and. start_direction /= 0 ) then ! on the first step and when the starting direction is defined

              call interface_normal(lat_in,long_in,intrface(raysec%istart%id),norm(1),norm(2),norm(3),h) 

              ! if -grad(T) points straight back through the interface at which we start, quit this section

              if (dot_product(grad0,start_direction*norm) > 0.0_dp) then

                 print *,'can not get into region',tf%reg%id,' from region',raysec%ray%section(seqnum+1)%reg%id
!                 print '(i5,6f12.5)',raysec%place_in_sequence,grad0,start_direction*norm

                 raysec%ray%valid=.false.
                 deallocate(pt)
                 return

              endif

           endif


           ! test if an interface is crossed

           r_interface=interpolate_interface(pp(2),pp(3),iftop)
           if (pp(1) >= r_interface) then
              
              if (iftop%id == ifend_id) then

                 if (section_is_refraction) then  

                    ! when the ray is refracted into the region check for total reflection (headwave),
                    ! if so keep going along the interface until the ray can get through

                    call interface_normal(pp(2),pp(3),intrface(ifend_id),norm(1),norm(2),norm(3),h)
                    call interpolate_time_gradient(tf,r_interface,pp(2),pp(3),grad1(1),grad1(2),grad1(3) &
                         ,verbose,outside,raysec)   
                    call interpolate_time_gradient(tf_next,r_interface,pp(2),pp(3),grad2(1),grad2(2),grad2(3) &
                         ,verbose,outside,raysec)

                    if (outside) then ; n=n-1 ; exit ploop ; endif
                    if (.not.raysec%ray%valid) then; deallocate(pt) ; return ; endif

                    gtp_in =sqrt(sum((grad2 - dot_product(norm,grad2)*norm)**2))


                    if (gtp_in <= sqrt(sum(grad1**2))) then  ! the ray can pass through

                       exit ploop

                    else   ! total reflection at this point, keep going along the interface

                       pt(2,n)=pp(2)          
                       pt(3,n)=pp(3)
                       pt(1,n)=r_interface
                       raysec%headwave =.true.
                       raysec%ray%headwave =.true.     

                       ! special return condition if source is reached
                       if(s%is_local .and. raysec%place_in_sequence == 1) then
                          dx= sqrt((pt(1,n)-s%r)**2+(s%r*(pt(2,n)-s%lat))**2+(s%r*s%coslat*(pt(3,n)-s%long))**2)
                          if (dx < s_tolerance) then
                             pt(1,n)=s%r ;pt(2,n)=s%lat ;pt(3,n)=s%long 
                             raysec%npoints=n
                             allocate(raysec%point(3,n))
                             raysec%point(1:3,1:n)=pt(1:3,1:n)
                             deallocate(pt)
                             return
                          endif
                       endif

                       cycle ploop

                    endif

                 else   ! section is a reflection, we can stop this section here

                    exit ploop

                 endif

              else  ! if the ray hits the non-end interface, the requested phase is a diffraction
                    ! just keep going along the interface and hopefully at some point the ray will detach again
                 
                 pt(2,n)=pp(2)          
                 pt(3,n)=pp(3)
                 pt(1,n)=r_interface
                 raysec%diffracted =.true.
                 raysec%ray%diffracted =.true.  

                 ! special return condition if source is reached
                 if(s%is_local .and. raysec%place_in_sequence == 1) then
                    dx= sqrt((pt(1,n)-s%r)**2+(s%r*(pt(2,n)-s%lat))**2+(s%r*s%coslat*(pt(3,n)-s%long))**2)
                    if (dx < s_tolerance) then
                       pt(1,n)=s%r ;pt(2,n)=s%lat ;pt(3,n)=s%long 
                       raysec%npoints=n
                       allocate(raysec%point(3,n))
                       raysec%point(1:3,1:n)=pt(1:3,1:n)
                       deallocate(pt)
                       return
                    endif
                 endif

                 cycle ploop
                 
              endif

           endif  ! if top interface crossed


           r_interface=interpolate_interface(pp(2),pp(3),ifbot)
           if (pp(1) <= r_interface) then

              if (ifbot%id == ifend_id) then

                 if (section_is_refraction) then  

                    ! when the ray is refracted into the region check for total reflection (headwave),
                    ! if so keep going along the interface until the ray can get through

                    call interface_normal(pp(2),pp(3),intrface(ifend_id),norm(1),norm(2),norm(3),h)
                    call interpolate_time_gradient(tf,r_interface,pp(2),pp(3),grad1(1),grad1(2),grad1(3) &
                         ,verbose,outside,raysec)   
                    call interpolate_time_gradient(tf_next,r_interface,pp(2),pp(3),grad2(1),grad2(2),grad2(3) &
                         ,verbose,outside,raysec) 

                    if (outside) then ; n=n-1 ; exit ploop ; endif
                    if (.not.raysec%ray%valid) then; deallocate(pt) ; return ; endif

                    gtp_in =sqrt(sum((grad2 - dot_product(norm,grad2)*norm)**2))

                    if (gtp_in <= sqrt(sum(grad1**2))) then  ! the ray can pass through

!                       print '(a30,4f12.5)','refracting through top 1  ',r_interface,pp(1:3)

                       exit ploop

                    else   ! total reflection at this point, keep going along the interface

                       pt(2,n)=pp(2)          
                       pt(3,n)=pp(3)
                       pt(1,n)=r_interface
                       raysec%headwave =.true.
                       raysec%ray%headwave =.true. 
    
                       ! special return condition if source is reached
                       if(s%is_local .and. raysec%place_in_sequence == 1) then
                          dx= sqrt((pt(1,n)-s%r)**2+(s%r*(pt(2,n)-s%lat))**2+(s%r*s%coslat*(pt(3,n)-s%long))**2)
                          if (dx < s_tolerance) then
                             pt(1,n)=s%r ;pt(2,n)=s%lat ;pt(3,n)=s%long 
                             raysec%npoints=n
                             allocate(raysec%point(3,n))
                             raysec%point(1:3,1:n)=pt(1:3,1:n)
                             deallocate(pt)
                             return
                          endif
                       endif

                       cycle ploop

                    endif

                 else   ! section is a reflection, we can stop this section here

                    exit ploop

                 endif

              else  ! if the ray hits the non-end interface, the requested phase is a diffraction
                    ! just keep going along the interface and hopefully at some point the ray will detach again

                 pt(2,n)=pp(2)          
                 pt(3,n)=pp(3)
                 pt(1,n)=r_interface
                 raysec%diffracted =.true.
                 raysec%ray%diffracted =.true.

                 ! special return condition if source is reached
                 if(s%is_local .and. raysec%place_in_sequence == 1) then
                    dx= sqrt((pt(1,n)-s%r)**2+(s%r*(pt(2,n)-s%lat))**2+(s%r*s%coslat*(pt(3,n)-s%long))**2)
                    if (dx < s_tolerance) then
                       pt(1,n)=s%r ;pt(2,n)=s%lat ;pt(3,n)=s%long 
                       raysec%npoints=n
                       allocate(raysec%point(3,n))
                       raysec%point(1:3,1:n)=pt(1:3,1:n)
                       deallocate(pt)
                       return
                    endif
                 endif

                 cycle ploop

              endif
           endif

           ! iterate the step until result is sufficiently accurate
           iter = 1
itloop   : do 

              pp(1)=pt(1,n-1)-dl*grad0(1)
              pp(2)=pt(2,n-1)-dl*grad0(2)/pp(1)          
              pp(3)=pt(3,n-1)-dl*grad0(3)/(pp(1)*cos(pp(2)))


              ! calculate gradient at provisional next position

              call interpolate_time_gradient(tf,pp(1),pp(2),pp(3),grad1(1),grad1(2),grad1(3),verbose,outside,raysec)
              if (outside) then ; n=n-1 ; exit ploop ; endif
              if (.not.raysec%ray%valid) then; deallocate(pt) ; return ; endif

              ! repeat the step with the average of the gradients at the aoriginal and provisional points

              pt(1,n)=pt(1,n-1)-dl*(grad0(1)+grad1(1))*0.5_dp
              pt(2,n)=pt(2,n-1)-dl*(grad0(2)+grad1(2))/(pt(1,n)+pt(1,n-1))          
              pt(3,n)=pt(3,n-1)-dl*(grad0(3)+grad1(3))/((pt(1,n)+pt(1,n-1))*cos(pt(2,n)))

              ! test again if an interface is crossed during the iteration, if so exit

              r_interface=interpolate_interface(pt(2,n),pt(3,n),iftop)
              if (pt(1,n) >= r_interface) then

                 if (iftop%id == ifend_id) then

                    if (section_is_refraction) then  

                       ! when the ray is refracted into the region check for total reflection (headwave),
                       ! if so keep going along the interface until the ray can get through

                       call interface_normal(pt(2,n),pt(3,n),intrface(ifend_id),norm(1),norm(2),norm(3),h)
                       call interpolate_time_gradient(tf,r_interface,pt(2,n),pt(3,n),grad1(1),grad1(2),grad1(3) &
                            ,verbose,outside,raysec)   
                       call interpolate_time_gradient(tf_next,r_interface,pt(2,n),pt(3,n),grad2(1),grad2(2),grad2(3) &
                            ,verbose,outside,raysec) 

                       if (outside) then ; n=n-1 ; exit ploop ; endif
                       if (.not.raysec%ray%valid) then; deallocate(pt) ; return ; endif

                       gtp_in =sqrt(sum((grad2 - dot_product(norm,grad2)*norm)**2))

                       if (gtp_in <= sqrt(sum(grad1**2))) then ! the ray can get through here

!                          print '(a30,4f12.5)','refracting through top 2  ',r_interface,pt(1:3,n)
                          pp=pt(1:3,n)
                          exit ploop

                       else  ! total reflection, keep going along the interface

                          pt(1,n)=r_interface
                          raysec%headwave =.true.
                          raysec%ray%headwave =.true.   
                          cycle ploop

                       endif

                    else  ! in case the section is a reflection

                       pp=pt(1:3,n)
                       exit ploop

                    endif

                 else  ! it is the non-end interface, keep going along it

                    pt(1,n)=r_interface
                    raysec%diffracted =.true.
                    raysec%ray%diffracted =.true.
                    cycle ploop
                 
                 endif

              endif

              r_interface=interpolate_interface(pt(2,n),pt(3,n),ifbot)
              if (pt(1,n) <= r_interface) then

                 if (ifbot%id == ifend_id) then

                    if (section_is_refraction) then  

                       ! when the ray is refracted into the region check for total reflection (headwave),
                       ! if so keep going along the interface until the ray can get through

                       call interface_normal(pt(2,n),pt(3,n),intrface(ifend_id),norm(1),norm(2),norm(3),h)
                       call interpolate_time_gradient(tf,r_interface,pt(2,n),pt(3,n),grad1(1),grad1(2),grad1(3) &
                            ,verbose,outside,raysec)   
                       call interpolate_time_gradient(tf_next,r_interface,pt(2,n),pt(3,n),grad2(1),grad2(2),grad2(3) &
                            ,verbose,outside,raysec) 

                       if (outside) then ; n=n-1 ; exit ploop ; endif
                       if (.not.raysec%ray%valid) then; deallocate(pt) ; return ; endif

                       gtp_in =sqrt(sum((grad2 - dot_product(norm,grad2)*norm)**2))

                       if (gtp_in <= sqrt(sum(grad1**2))) then ! the ray can get through here

                          pp=pt(1:3,n)
                          exit ploop

                       else  ! total reflection, keep going along the interface

                          pt(1,n)=r_interface
                          raysec%headwave =.true.
                          raysec%ray%headwave =.true.   
                          cycle ploop

                       endif

                    else  ! in case the section is a reflection

                       pp=pt(1:3,n)
                       exit ploop

                    endif

                 else

                    pt(1,n)=r_interface
                    raysec%diffracted =.true.
                    raysec%ray%diffracted =.true.
                    cycle ploop
                 
                 endif

              endif
       
              ! if no interface is crossed, test for convergence, if sufficient exit
              if (sqrt(sum((pt(1:3,n)-pp(1:3))**2)) < p_tolerance) exit itloop

              ! safety stop for no convergence
              iter = iter+1
              if (iter > 10) stop 'trace_ray_section: no convergence after max iterations'

              ! reduce step size if initial size not accurate enough
              dl=0.5_dp*dl

           end do itloop


    ! special return condition if source is reached
           if(s%is_local .and. raysec%place_in_sequence == 1) then
              dx= sqrt((pt(1,n)-s%r)**2+(s%r*(pt(2,n)-s%lat))**2+(s%r*s%coslat*(pt(3,n)-s%long))**2)
              if (dx < s_tolerance) then
                 pt(1,n)=s%r ;pt(2,n)=s%lat ;pt(3,n)=s%long 
                 raysec%npoints=n
                 allocate(raysec%point(3,n))
                 raysec%point(1:3,1:n)=pt(1:3,1:n)
                 deallocate(pt)
                 return
              endif
           endif

        end do ploop

        if(s%is_local .and. raysec%place_in_sequence == 1)  stop ' unable to find local source in ray section 1'   

        if (outside) then

           raysec%npoints=n
           allocate(raysec%point(3,n))
           raysec%point(1:3,1:n)=pt(1:3,1:n)
           deallocate(pt)
           return

        endif

   ! find the intersection of the ray with the end interface

   x1=0.0_dp
   x2=dl
   f=pt(1,n-1) - interpolate_interface(pt(2,n-1),pt(3,n-1),ifend)
   fmid=pp(1)-r_interface


! check that interface is actually crossed, if not error stop

   if (f*fmid > 0.01_dp*pgrid%tolerance) then

      print *,raysec%place_in_sequence
!      print '(a10,4f14.7)','parms',dx,p_tolerance,s_tolerance
      print '(a10,4f14.7)','rt1',pt(1:3,n-1),f
      print '(a10,4f14.7)','rt2',pp(1:3),fmid
      print '(a10,4f14.7)','grad0',grad0(1:3),dl
      print *,'top if',interpolate_interface(pt(2,n-1),pt(3,n-1),intrface(tf%reg%itop%id))
      print *,'bot if',interpolate_interface(pt(2,n-1),pt(3,n-1),intrface(tf%reg%ibot%id))
      print *,'004 if',interpolate_interface(pt(2,n-1),pt(3,n-1),intrface(4))
!      do i=1,tf%reg%nnode
!         write(29,'(i5,4f12.6)') i,tf%arrivaltime(i),tf%time_gradient(1:3,i)
!         if (sqrt(sum(tf%time_gradient(1:3,i)**2)) < 0.01_dp) &
!              print'(i5,4f12.6,3i5)', i,tf%arrivaltime(i),tf%time_gradient(1:3,i),&
!              tf%reg%node(i)%i1,tf%reg%node(i)%i3,tf%reg%node(i)%i3
!      end do
      stop 'root not bracketed in trace_ray_section'
   endif


! catch pinched interfaces (end point = starting point)

   if (abs(f*fmid) <= 0.01_dp*pgrid%tolerance) then

      if ( ((raysec%iend%id == ifbot%id .and. fmid<0.0_dp) .or. &
         (raysec%iend%id == iftop%id .and. fmid>0.0_dp))) then


         pt(1:3,n) = pt(1:3,n-1)

         pt(1,n)=interpolate_interface(pt(2,n),pt(3,n),ifend)
!         print *,'no end point iteration necessary'
!         print '(a20,3f15.7)','last section point',pt(1:3,n)

         r_out=pt(1,n) ;lat_out=pt(2,n) ;long_out=pt(3,n) 

!         write(1,'(i5,6f12.5)') n,pt(1:3,n),grad0

         raysec%npoints=n
         allocate(raysec%point(3,n))
         raysec%point(1:3,1:n)=pt(1:3,1:n)

         deallocate(pt)

         return


      else

         print *,raysec%place_in_sequence
!         print '(a10,4f14.7)','parms',dx,p_tolerance,s_tolerance
         print '(a10,4f14.7)','rt1',pt(1:3,n-1),f
         print '(a10,4f14.7)','rt2',pp(1:3),fmid
         stop 'error traversing pinched interface'

      endif
   endif


! normal case of root bracketed

   if (f*fmid < -0.01_dp*pgrid%tolerance) then

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
         lat = pt(2,n-1)-xmid*grad0(2)/pt(1,n-1)
         long= pt(3,n-1)-xmid*grad0(3)/(pt(1,n-1)*cos(pt(2,n-1)))
         fmid=(pt(1,n-1)-xmid*grad0(1))-interpolate_interface(lat,long,ifend)
         if (fmid <= 0.0_dp) rtbis=xmid
      end do

      pt(1,n)=pt(1,n-1)-rtbis*grad0(1)
      pt(2,n)=pt(2,n-1)-rtbis*grad0(2)/pt(1,n-1)          
      pt(3,n)=pt(3,n-1)-rtbis*grad0(3)/(pt(1,n-1)*cos(pt(2,n-1)))

      pt(1,n)=interpolate_interface(pt(2,n),pt(3,n),ifend)

!      print '(a20,3f15.7)','last section point',pt(1:3,n)

   endif

   r_out=pt(1,n) ;lat_out=pt(2,n) ;long_out=pt(3,n) 

!   write(1,'(i5,6f12.5)') n,pt(1:3,n),grad0


   ! finally copy the ray section points from temporary array pt into the ray section
  raysec%npoints=n
  allocate(raysec%point(3,n))
  raysec%point(1:3,1:n)=pt(1:3,1:n)

  deallocate(pt)

  return

end subroutine trace_ray_section
!*************************************************************************************************************
!**********************************************************************************************************
! This routine returns the time gradient at an arbitray position within a time field
! In a regular cell it uses 
! In a cut cell this modified, could probably be improved for cut cells
subroutine interpolate_time_gradient(tf,r,lat,long,dtdr,dtdlat,dtdlong,verbose,outside,raysec)
  use mod_3dfm
  implicit none

  real(kind=dp)                              :: r,lat,long,dtdr,dtdlat,dtdlong
  type(Ttime_field)                          :: tf
  type(Tregion),pointer                      :: reg
  type(Tpropagation_grid),pointer            :: grid  
  type(Tray_section)                         :: raysec
  real(kind=dp)                              :: w(30),w1,w2,w3,tgrad(3,30)
  real(kind=dp)                              :: dxplus,dxmin,xbase,dx,avnorm(3),dplane,d(3)
  integer                                    :: nnode,i,j,k,m,ii,jj,kk,icell,ir,ilat,ilong,inode,n_inode,inode1
  logical    :: verbose,outside
  integer    :: locnode(20)
  real(kind=dp)                               :: norm_r,norm_lat,norm_long,h,nor,nolat,nolong
  real(kind=dp)                               :: interpolate_interface

  
  reg=> tf%reg
  grid => reg%grid

  ! find cell in which position resides

  ir    =  floor((r - grid%r0)/grid%dr0 + 1)
  ilat  =  floor((lat - grid%lat0)/grid%dlat0 + 1)
  ilong =  floor((long - grid%long0)/grid%dlong0 + 1)

  if (ir == grid%nr) ir=ir-1
  if (ir == 0) ir=ir+1

  outside = .false.
  if ((ir <1 .or. ir > grid%nr-1).or.(ilat <1 .or. ilat > grid%nlat-1).or.(ilong <1 .or. ilong > grid%nlong-1)) then
     print *,' outside grid in raytracing'
!     print *,ir,ilat,ilong
!     print *,grid%nr-1,grid%nlat-1,grid%nlong-1
!     print *,r,lat,long
     outside = .true.
     return
  endif



  nnode=0
  n_inode=0
  avnorm=0.0_dp
  locnode=0

  ! find the nodes that belong to this cell


  ! first check if the cell is cut by an interface

  if (associated(pgrid%ccind_from_3dc(ir,ilat,ilong)%p)) then

     ! check if the cell is cut by the top intersection

     ! icell is the index of the current connected cell in the list of cells cut by interface reg%itop.
                  
     icell = pgrid%ccind_from_3dc(ir,ilat,ilong)%p(reg%itop%iface_id)

     ! if icell == 0 the cell is not cut by the top interface
    
     if(icell /= 0) then
 
        ii=reg%itop%id
        do jj=1,intersection(ii)%n_inodes(icell)

!   m is the node number in the regional node list of node  jj in the list of inteface nodes that are part of cut cell icell

           inode =(intersection(ii)%inodes(jj,icell))
           m=intersection(ii)%rbel_node_id(inode)

           n_inode=n_inode+1
           if (n_inode == 1) inode1=m
           nnode=nnode+1
           locnode(nnode)=m
           tgrad(1:3,nnode)= tf%time_gradient(1:3,m) 


           select case (intersection(ii)%intype(inode))

              case(0)
                 w1=1.0_dp - abs(r-reg%r(m))/grid%dr0
                 w2=1.0_dp - abs(lat-reg%lat(m))/grid%dlat0
                 w3=1.0_dp - abs(long-reg%long(m))/grid%dlong0

              case(1)
                 xbase=grid%r0+grid%dr0*floor((reg%r(m)-grid%r0)/grid%dr0)
                 dxplus=(xbase+grid%dr0)-reg%r(m)
                 dxmin=reg%r(m)-xbase
                 dx=r-reg%r(m)
                 if (dx >= 0.0_dp)  w1=1.0_dp - abs(dx)/dxplus
                 if (dx < 0.0_dp)   w1=1.0_dp - abs(dx)/dxmin
                 w2=1.0_dp - abs(lat-reg%lat(m))/grid%dlat0
                 w3=1.0_dp - abs(long-reg%long(m))/grid%dlong0

              case(2)
                 xbase=grid%lat0+grid%dlat0*floor((reg%lat(m)-grid%lat0)/grid%dlat0)
                 dxplus=(xbase+grid%dlat0)-reg%lat(m)
                 dxmin=reg%lat(m)-xbase
                 dx=lat-reg%lat(m)
                 w1=1.0_dp - abs(r-reg%r(m))/grid%dr0
                 if (dx >= 0.0_dp)  w2=1.0_dp - abs(dx)/dxplus
                 if (dx < 0.0_dp)   w2=1.0_dp - abs(dx)/dxmin
                 w3=1.0_dp - abs(long-reg%long(m))/grid%dlong0

              case(3)
                 xbase=grid%long0+grid%dlong0*floor((reg%long(m)-grid%long0)/grid%dlong0)
                 dxplus=(xbase+grid%dlong0)-reg%long(m)
                 dxmin=reg%long(m)-xbase
                 dx=long-reg%long(m)
                 w1=1.0_dp - abs(r-reg%r(m))/grid%dr0
                 w2=1.0_dp - abs(lat-reg%lat(m))/grid%dlat0
                 if (dx >= 0.0_dp)  w3=1.0_dp - abs(dx)/dxplus
                 if (dx < 0.0_dp)   w3=1.0_dp - abs(dx)/dxmin

           end select


           avnorm=avnorm+intersection(ii)%normal(1:3,inode)
           w(nnode)=w1*w2*w3


!           if (intersection(ii)%intype(inode) /= 0) print *,'xbase',xbase,dxplus,dxmin

!  write(33,'(3i5,10f12.5)') ii,inode,intersection(ii)%intype(inode),reg%r(m),reg%lat(m),&
!           reg%long(m),xbase,dx,dxplus,dxmin,w1,w2,w3

        end do

     end if


! then check the bottom intersection

     icell = pgrid%ccind_from_3dc(ir,ilat,ilong)%p(reg%ibot%iface_id)
     if (icell /= 0) then

        ii=reg%ibot%id
        do jj=1,intersection(ii)%n_inodes(icell)
!   m is the node number in the regional node list of node  jj in the list of inteface nodes that are part of cut cell icell

           inode =(intersection(ii)%inodes(jj,icell))
           m=intersection(ii)%rabo_node_id(inode)

           n_inode=n_inode+1
           if (n_inode == 1) inode1=m
           nnode=nnode+1
           locnode(nnode)=m
           tgrad(1:3,nnode)= tf%time_gradient(1:3,m) 


           select case (intersection(ii)%intype(inode))

              case(0)
                 w1=1.0_dp - abs(r-reg%r(m))/grid%dr0
                 w2=1.0_dp - abs(lat-reg%lat(m))/grid%dlat0
                 w3=1.0_dp - abs(long-reg%long(m))/grid%dlong0

              case(1)
                 xbase=grid%r0+grid%dr0*floor((reg%r(m)-grid%r0)/grid%dr0)
                 dxplus=(xbase+grid%dr0)-reg%r(m)
                 dxmin=reg%r(m)-xbase
                 dx=r-reg%r(m)
                 if (dx >= 0.0_dp)  w1=1.0_dp - abs(dx)/dxplus
                 if (dx < 0.0_dp)   w1=1.0_dp - abs(dx)/dxmin
                 w2=1.0_dp - abs(lat-reg%lat(m))/grid%dlat0
                 w3=1.0_dp - abs(long-reg%long(m))/grid%dlong0

              case(2)
                 xbase=grid%lat0+grid%dlat0*floor((reg%lat(m)-grid%lat0)/grid%dlat0)
                 dxplus=(xbase+grid%dlat0)-reg%lat(m)
                 dxmin=reg%lat(m)-xbase
                 dx=lat-reg%lat(m)
                 w1=1.0_dp - abs(r-reg%r(m))/grid%dr0
                 if (dx >= 0.0_dp)  w2=1.0_dp - abs(dx)/dxplus
                 if (dx < 0.0_dp)   w2=1.0_dp - abs(dx)/dxmin
                 w3=1.0_dp - abs(long-reg%long(m))/grid%dlong0

              case(3)
                 xbase=grid%long0+grid%dlong0*floor((reg%long(m)-grid%long0)/grid%dlong0)
                 dxplus=(xbase+grid%dlong0)-reg%long(m)
                 dxmin=reg%long(m)-xbase
                 dx=long-reg%long(m)
                 w1=1.0_dp - abs(r-reg%r(m))/grid%dr0
                 w2=1.0_dp - abs(lat-reg%lat(m))/grid%dlat0
                 if (dx >= 0.0_dp)  w3=1.0_dp - abs(dx)/dxplus
                 if (dx < 0.0_dp)   w3=1.0_dp - abs(dx)/dxmin

           end select

           avnorm=avnorm+intersection(ii)%normal(1:3,inode)
           w(nnode)=w1*w2*w3

!  write(33,'(3i5,10f12.5)') ii,inode,intersection(ii)%intype(inode),reg%r(m),reg%lat(m),&
!           reg%long(m),xbase,dx,dxplus,dxmin,w1,w2,w3
        end do

     endif

  endif  ! if cell is cut


! then find the regular grid nodes of connected cell n

  do i=0,1
     ii=ir+i
     do j=0,1
        jj=ilat+j
        do k=0,1
           kk=ilong+k

           if (pgrid%node_region(ii,jj,kk) == reg%id) then  ! node has to belong to the current region

              m=pgrid%rnode_id(ii,jj,kk)

              nnode=nnode+1
              locnode(nnode)=m
              tgrad(1:3,nnode)= tf%time_gradient(1:3,m)  
              w1=1.0_dp - abs(r-reg%r(m))/grid%dr0
              w2=1.0_dp - abs(lat-reg%lat(m))/grid%dlat0
              w3=1.0_dp - abs(long-reg%long(m))/grid%dlong0
              w(nnode)=w1*w2*w3 

           endif


        end do
     end do
  end do

  if (n_inode > 500) then
  avnorm=avnorm/n_inode
     do i=1,n_inode
        if (intersection(reg%node(inode1)%i2)%intype(reg%node(inode1)%i3) /= 0) then
           d(1) = r-reg%r(inode1)
           d(2) = lat-reg%lat(inode1)*r
           d(3) = long-reg%long(inode1)*r*cos(lat)
           dplane= abs(dot_product(d,avnorm))/grid%dr0
           w(i)=w(i)/(dplane+0.5_dp)
        endif
     end do
  end if

  w(1:nnode)=w(1:nnode)/sum(w(1:nnode))

!  write(1,*) 'itgrad:'
!  do n=1,nnode
!     write(1,'(2i5,4f12.5,3i5)') n,n_inode,w(n),tgrad(1:3,n),ir,ilat,ilong
!  end do


  dtdr    = sum(tgrad(1,1:nnode)*w(1:nnode))  
  dtdlat  = sum(tgrad(2,1:nnode)*w(1:nnode))
  dtdlong = sum(tgrad(3,1:nnode)*w(1:nnode))

!  if (tf%id == 2) write(15,'(3i5,6f12.6)') ir,ilat,ilong,r,lat,long,dtdr,dtdlat,dtdlong

  if (sqrt(dtdr**2+dtdlat**2+dtdlong**2) < 0.01_dp) then
     print *
     print *,'----- WARNING -------'
     print *,'an interpolated time gradient was unphysically small '
     print *,'this occurred for ray',raysec%ray%raypath_id,' from source',raysec%source%id
     print *,'this sometimes happens when the specified source/path/receiver combination'
     print *,'is not physically consistent. The ray will be declared invalid'
     print *,'If you think the ray should exist please report this message as a bug'
     print *
     raysec%ray%valid = .false.

     return

     print *,'nodes:',nnode
     print *,'cell:',ir,ilat,ilong
     print *,'itop,ibot',tf%reg%itop%id,tf%reg%ibot%id
     if (.not.associated(pgrid%ccind_from_3dc(ir,ilat,ilong)%p)) then
        print *,'gradient error in uncut cell'
        print '(4f15.7)',r,lat,long,interpolate_interface(lat,long,intrface(4))
        do i=0,1
           ii=ir+i
           do j=0,1
              jj=ilat+j
              do k=0,1
                 kk=ilong+k

                 m=pgrid%rnode_id(ii,jj,kk)
                 print '(5i5,3f15.7)',m,ii,jj,kk,pgrid%node_region(ii,jj,kk),pgrid%r(ii),&
                      pgrid%lat(jj),pgrid%long(kk)

              end do
           end do
        end do
        stop
     else
        print *,'ccind',pgrid%ccind_from_3dc(ir,ilat,ilong)%p(1:n_interfaces)
     endif


     call interface_normal(lat,long,intrface(tf%inonstart%id),norm_r,norm_lat,norm_long,h)

     do i=1,nnode  
        m=locnode(i)
        call interface_normal(tf%reg%lat(m),tf%reg%long(m),intrface(tf%inonstart%iface_id),nor,nolat,nolong,h)
 

        print '(i5,4f12.6,3i5,2f12.6)',i,w(i),tgrad(1:3,i),tf%reg%node(m)%i1, &
             tf%reg%node(m)%i2,tf%reg%node(m)%i3, &
             (norm_r*tgrad(1,i)+norm_lat*tgrad(2,i)+norm_long*tgrad(3,i))/sqrt(sum(tgrad(1:3,i)**2)), &
             (nor*tgrad(1,i)+nolat*tgrad(2,i)+nolong*tgrad(3,i))/sqrt(sum(tgrad(1:3,i)**2))
!        print '(3f12.6)',nor,nolat,nolong
!        print *
     end do

     stop 'interpolate gradient error in cut cell'

  endif

  return

  end subroutine interpolate_time_gradient

!*********************************************************************************
!**********************************************************************************************************
! This subroutine estimates the arrival time at an arbitrary location within a time field
! It look for the closest regional node, and estimates the time to be
! t = t(node) + (x_node-x).grad(T)_node

subroutine interpolate_arrivaltime(tf,r,lat,long,atime)
  use mod_3dfm
  implicit none

  real(kind=dp)                              :: r,lat,long
  type(Ttime_field)                          :: tf
  type(Tregion),pointer                      :: reg
  type(Tintersection),pointer                :: isec
  type(Tpropagation_grid),pointer            :: grid  
  real(kind=dp)                              :: dist,dr(3),distmin,atime

  integer                                    :: node,i,j,k,m,ii,jj,kk,icell,ir,ilat,ilong,inode

  reg=> tf%reg
  grid => pgrid
  ! find cell in which position resides

  ir    =  floor((r - grid%r0)/grid%dr0 + 1)
  ilat  =  floor((lat - grid%lat0)/grid%dlat0 + 1)
  ilong =  floor((long - grid%long0)/grid%dlong0 + 1)
  if (ir == grid%nr) ir=ir-1
  if (ir == 0) ir=ir+1


  if ((ir <1 .or. ir > grid%nr-1).or.(ilat <1 .or. ilat > grid%nlat-1).or.(ilong <1 .or. ilong > grid%nlong-1)) then
     print *,' outside grid in raytracing'
     print *,ir,ilat,ilong
     print *,grid%nr-1,grid%nlat-1,grid%nlong-1
     print *,r,lat,long
  endif



  distmin=1.e100_dp
  node=0

  ! find the nodes that belong to this cell


  ! first check if the cell is cut by an interface
  if (associated(pgrid%ccind_from_3dc(ir,ilat,ilong)%p)) then

     ! check if the cell is cut by the top intersection

     ! icell is the index of the current connected cell in the list of cells cut by interface reg%itop.
     icell = pgrid%ccind_from_3dc(ir,ilat,ilong)%p(reg%itop%iface_id)

     ! if icell == 0 the cell is not cut be the top interface
     if(icell /= 0) then
        isec => reg%itop
        do jj=1,isec%n_inodes(icell)

!   m is the node number in the regional node list of node  jj in the list of inteface nodes that are part of cut cell icell

           inode =(isec%inodes(jj,icell))
           m=isec%rbel_node_id(inode)

           dr(1)= (r-reg%r(m))
           dr(2)= (lat-reg%lat(m))*r
           dr(3)= (long-reg%long(m))*r*cos(lat)

           dist=sum(dr**2)
           if (dist < distmin) then
              node=m
              distmin=dist
           endif

        end do

     end if


! then check the bottom intersection

     icell = pgrid%ccind_from_3dc(ir,ilat,ilong)%p(reg%ibot%iface_id)
     if (icell /= 0) then

        isec => reg%ibot
        do jj=1,isec%n_inodes(icell)
!   m is the node number in the regional node list of node  jj in the list of inteface nodes that are part of cut cell icell

           inode =(isec%inodes(jj,icell))
           m=isec%rabo_node_id(inode)

           dr(1)= (r-reg%r(m))
           dr(2)= (lat-reg%lat(m))*r
           dr(3)= (long-reg%long(m))*r*cos(lat)

           dist=sum(dr**2)
           if (dist < distmin) then
              node=m
              distmin=dist
           endif

        end do

     endif

  endif  ! if cell is cut


! then find the regular grid nodes of connected cell n

  do i=0,1
     ii=ir+i
     do j=0,1
        jj=ilat+j
        do k=0,1
           kk=ilong+k
           if (pgrid%node_region(ii,jj,kk) == reg%id) then  ! node has to belong to the current region

              m=pgrid%rnode_id(ii,jj,kk)

              dr(1)= (r-reg%r(m))
              dr(2)= (lat-reg%lat(m))*r
              dr(3)= (long-reg%long(m))*r*cos(lat)

              dist=sum(dr**2)
              if (dist < distmin) then
                 node=m
                 distmin=dist
              endif


           endif

        end do
     end do
  end do
  !AAA Some idiotproofing to avoid segmentation faults when no nodes were found
  if (distmin==1.e100_dp) then
      atime=-9999.99
      return
  endif
!  print *,'closest node is',reg%node(node)%i1,reg%node(node)%i2,reg%node(node)%i3
!  print *, 'distance', distmin, tf%arrivaltime(node)

  dr(1)= (r-reg%r(node))
  dr(2)= (lat-reg%lat(node))*r
  dr(3)= (long-reg%long(node))*r*cos(lat)
  atime=tf%arrivaltime(node)+dot_product(tf%time_gradient(1:3,node),dr)

  return

  end subroutine interpolate_arrivaltime

!*********************************************************************************
