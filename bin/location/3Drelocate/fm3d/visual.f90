!*********************************************************************************
!--------------------------------------------------------------------------------\
!*******************************************************************************************


      subroutine display_interface(isec)

      use mod_3dfm

! all default floats are set to double

      implicit double precision (a-h,o-z)

! argument definition

      type(Tintersection) :: isec

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

      real(kind=dp) :: x,y,z
      integer dmode
      logical clockwise


!---------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------
! start the triangulation 
!------------------------------------------------------------------------------------


! nl is the initial number of lagrangian points

      nl=isec%nnode

! nlmax is the maximum number of lagrangian points

      nlmax=isec%nnode*2

! ntmax is the maximum number of delaunay triangles built on the lagrangian points

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
            points(1,i)=isec%lat(i)
            points(2,i)=isec%long(i)
         enddo

! nn_setup (calculates delaunay triangles IN SURFACE COORDINATES RSPOS and other info)

         call nn2d_setup &
              (nl,ntmax,nhmax,npmax,nnpnmax,nmax, &
              points,dmode,nmode,clockwise,isec%lat(:),nt,SPfromTR, &
              centres,neighbour,nh,hulltriangles,nohalt_hull, &
              loc,nnn,nnlist,ntrilist, &
              eps,nvmax,vis_tlist,vis_elist,add_tlist, &
              lt_work,ln_work)



!  

         SPfromTR=SPfromTR-1   ! counting in DX starts at zero
         value=0.00

         open(1,file='iface'//char(48+isec%id)//'.dx')

         write (1,*) 'object 1 class array type float rank 1 shape 3 items',nl, 'data follows'
         do n=1,nl
            x=isec%r(n)*cos(isec%lat(n))*cos(isec%long(n))
            y=isec%r(n)*cos(isec%lat(n))*sin(isec%long(n))
            z=isec%r(n)*sin(isec%lat(n))
            write(1,'(3f15.6)') y,z,x
         end do
         write (1,*)

         write (1,*) 'object 2 class array type int rank 1 shape 3 items',nt, 'data follows'

         do n=1,nt
            write(1,'(3i6)') (SPfromTR(i,n),i=1,3)
         end do

         write (1,*) 'attribute "element type" string "triangles"'
         write (1,*) 'attribute "ref" string "positions"'
         write (1,*)

         write (1,*) 'object 3 class array type float rank 0 items',nt, 'data follows'
         do n=1,nt
            write(1,'(f15.6)') value
         end do
         write (1,*) 'attribute "dep" string "connections"'
         write (1,*)

         write (1,*) 'object "irregular positions irregular connections" class field'
         write (1,*) 'component "positions" value 1'
         write (1,*) 'component "connections" value 2'
         write (1,*) 'component "data" value 3'
         write (1,*) 'end'

         close(1)

!         print*,'display_interface:',nt,' triangles witten to file iface.dx'

! deallocate everything


      deallocate (points,centres)
      deallocate (neighbour,SPfromTR)
      deallocate (vis_tlist,vis_elist,add_tlist)
      deallocate (hulltriangles,nnn,nnlist,ntrilist)
      deallocate (lt_work,ln_work)
      deallocate (work_d1,work_d2,work_d3,work_d4,work_d5,work_d6,work_d7)
      deallocate (work_r1,work_r2,work_i1,work_i2,work_i3)

      return

      end subroutine display_interface

!*********************************************************************************************************
subroutine display_valid_rays

use mod_3dfm

type(Tray),dimension(:),allocatable  :: disp_ray
integer                              :: j,k,m,n

  j=0
  do n=1,n_receivers
     do m=1,receiver(n)%n_rays
        if (receiver(n)%ray(m)%valid) then
           if (receiver(n)%ray(m)%is_multiray) then
              do k=1,receiver(n)%ray(m)%n_subrays
                 if (receiver(n)%ray(m)%subray(k)%valid) then
                    j=j+1
                 endif
              end do
           else
              j=j+1
           endif
        endif
     end do
  end do


  allocate(disp_ray(j))
  j=0
  do n=1,n_receivers
     do m=1,receiver(n)%n_rays
        if (receiver(n)%ray(m)%valid) then
           if (receiver(n)%ray(m)%is_multiray) then
              do k=1,receiver(n)%ray(m)%n_subrays
                 if (receiver(n)%ray(m)%subray(k)%valid) then
                    j=j+1
                    disp_ray(j)=receiver(n)%ray(m)%subray(k)
                 endif
              end do
           else
              j=j+1
              disp_ray(j)=receiver(n)%ray(m)
           endif
        endif
     end do
  end do

  call display_ray(disp_ray,j)


  deallocate(disp_ray)

  print *,j,'rays displayed'


end subroutine display_valid_rays


!*********************************************************************************************************
subroutine display_ray(ray,nray)

use mod_3dfm

type(Tray) :: ray(nray)
integer,dimension(:),pointer :: vtype_seq
integer i,k,n,m,nsec,nray
real(kind=dp) ::c1,c2,c,x,y,z,r,lat,long


            open(1,file='ray.dx')

            c1=1.0_dp
            c2=1.1_dp
            k=0
            do m=1,nray
               nsec= ray(m)%nsections
               do i=1,nsec ; k=k+ ray(m)%section(i)%npoints ; end do 
            end do

            write (1,*) 'object 1 class array type float rank 1 shape 3 items',k, 'data follows'


            do m=1,nray
               do i=1,ray(m)%nsections
                  do n=1,ray(m)%section(i)%npoints
                     lat=ray(m)%section(i)%point(2,n)
                     long=ray(m)%section(i)%point(3,n)
                     r=ray(m)%section(i)%point(1,n)
                     x=r*cos(lat)*cos(long)
                     y=r*cos(lat)*sin(long)
                     z=r*sin(lat)
                     write(1,'(3f15.6)') y,z,x
                  end do
               end do
            end do

            write (1,*)


            write (1,*) 'object 2 class array type float rank 0 items',k, 'data follows'

            do m=1,nray
               vtype_seq => ray(m)%source%path(ray(m)%raypath_id)%vtype_sequence
               do i=1,ray(m)%nsections
                  if (vtype_seq(i) == 1) c=c1
                  if (vtype_seq(i) == 2) c=c2
                  do n=1,ray(m)%section(i)%npoints
                     write(1,'(3f15.6)') c
                  end do
               end do
            end do

            write (1,*) 'attribute "dep" string "positions"'
            write (1,*)

            write (1,*) 'object "irregular positions irregular connections" class field'
            write (1,*) 'component "positions" value 1'
            write (1,*) 'component "data" value 2'
            write (1,*) 'end'
            
            close(1)

end subroutine display_ray
!*********************************************************************************************************
subroutine display_stored_rays

  use mod_3dfm

  integer n
  real(kind=dp) ::c,x,y,z

  open(1,file='ray.dx')  
  open(41,file='raypos')
  open(42,file='raytype')

  write (1,*) 'object 1 class array type float rank 1 shape 3 items', &
       raypoint_counter, 'data follows'

  do n=1,raypoint_counter
     read(41,*) y,z,x
     write(1,'(3f15.6)') y,z,x
  end do

  write (1,*)


  write (1,*) 'object 2 class array type float rank 0 items', &
       raypoint_counter, 'data follows'

  do n=1,raypoint_counter
     read(42,*) c
     write(1,'(3f15.6)') c
  end do


  write (1,*) 'attribute "dep" string "positions"'
  write (1,*)

  write (1,*) 'object "irregular positions irregular connections" class field'
  write (1,*) 'component "positions" value 1'
  write (1,*) 'component "data" value 2'
  write (1,*) 'end'
            
  close(1)

end subroutine display_stored_rays


!*********************************************************************************************************
subroutine store_ray(ray)

use mod_3dfm

type(Tray) :: ray
integer,dimension(:),pointer :: vtype_seq
integer i,k,n,m,nsec,nray
real(kind=dp) ::c1,c2,c,x,y,z,r,lat,long


            c1=1.0_dp
            c2=1.1_dp
            k=0
            nsec= ray%nsections
            do i=1,nsec 
               raypoint_counter=raypoint_counter+ ray%section(i)%npoints 
            end do


            do i=1,ray%nsections
               do n=1,ray%section(i)%npoints
                  lat=ray%section(i)%point(2,n)
                  long=ray%section(i)%point(3,n)
                  r=ray%section(i)%point(1,n)
                  x=r*cos(lat)*cos(long)
                  y=r*cos(lat)*sin(long)
                  z=r*sin(lat)
                  write(41,'(3f15.6)') y,z,x
               end do
            end do

            vtype_seq => ray%source%path(ray%raypath_id)%vtype_sequence
            do i=1,ray%nsections
               if (vtype_seq(i) == 1) c=c1
               if (vtype_seq(i) == 2) c=c2
               do n=1,ray%section(i)%npoints
                  write(42,'(3f15.6)') c
               end do
            end do

end subroutine store_ray

!
!************************************************************************************
!

subroutine display_sources

use mod_3dfm
implicit none

integer n,nsloc
real(kind=dp) ::ttrue,x,y,z

            open(1,file='sources.dx')


            nsloc=count(source(1:n_sources)%is_local)

            write (1,*) 'object 1 class array type float rank 1 shape 3 items',nsloc, 'data follows'


            do n=1,n_sources
               if (source(n)%is_local) then
                  x=source(n)%r*cos(source(n)%lat)*cos(source(n)%long)
                  y=source(n)%r*cos(source(n)%lat)*sin(source(n)%long)
                  z=source(n)%r*sin(source(n)%lat)
                  write(1,'(3f15.6)') y,z,x
               endif
            end do

            write (1,*)


            write (1,*) 'object 2 class array type float rank 0 items',nsloc, 'data follows'

            ttrue=1.0_dp
            do n=1,nsloc
               write(1,'(3f15.6)') ttrue
            end do

            write (1,*) 'attribute "dep" string "positions"'
            write (1,*)

            write (1,*) 'object "irregular positions irregular connections" class field'
            write (1,*) 'component "positions" value 1'
            write (1,*) 'component "data" value 2'
            write (1,*) 'end'
            
            close(1)

end subroutine display_sources
!*********************************************************************************************************
subroutine display_receivers

use mod_3dfm


integer n
real(kind=dp) ::ttrue,x,y,z

            open(1,file='receivers.dx')

 
            write (1,*) 'object 1 class array type float rank 1 shape 3 items',n_receivers+1, 'data follows'

               x=receiver(1)%r*cos(receiver(1)%lat)*cos(receiver(1)%long)
               y=receiver(1)%r*cos(receiver(1)%lat)*sin(receiver(1)%long)
               z=receiver(1)%r*sin(receiver(1)%lat)
               write(1,'(3f15.6)') y,z,x

            do n=1,n_receivers
               x=receiver(n)%r*cos(receiver(n)%lat)*cos(receiver(n)%long)
               y=receiver(n)%r*cos(receiver(n)%lat)*sin(receiver(n)%long)
               z=receiver(n)%r*sin(receiver(n)%lat)
               write(1,'(3f15.6)') y,z,x
            end do

            write (1,*)


            write (1,*) 'object 2 class array type float rank 0 items',n_receivers+1, 'data follows'

            ttrue=1.0_dp
            do n=1,n_receivers+1
               write(1,'(3f15.6)') ttrue
            end do

            write (1,*) 'attribute "dep" string "positions"'
            write (1,*)

            write (1,*) 'object "irregular positions irregular connections" class field'
            write (1,*) 'component "positions" value 1'
            write (1,*) 'component "data" value 2'
            write (1,*) 'end'
            
            close(1)

end subroutine display_receivers

!*********************************************************************************************************
subroutine display_nodes(reg,display)

use mod_3dfm

type(Tregion) :: reg
integer n
real(kind=dp) ::ttrue,x,y,z
logical ::display(reg%nnode)

            open(1,file='nodes.dx')

            nnode=count(display)
 
            write (1,*) 'object 1 class array type float rank 1 shape 3 items',nnode, 'data follows'

            do n=1,reg%nnode
               if(display(n)) then
                  x=reg%r(n)*cos(reg%lat(n))*cos(reg%long(n))
                  y=reg%r(n)*cos(reg%lat(n))*sin(reg%long(n))
                  z=reg%r(n)*sin(reg%lat(n))
                  write(1,'(3f15.6)') y,z,x
               endif
            end do

            write (1,*)


            write (1,*) 'object 2 class array type float rank 0 items',nnode, 'data follows'

            ttrue=1.0_dp
            do n=1,nnode
               write(1,'(3f15.6)') ttrue
            end do

            write (1,*) 'attribute "dep" string "positions"'
            write (1,*)

            write (1,*) 'object "irregular positions irregular connections" class field'
            write (1,*) 'component "positions" value 1'
            write (1,*) 'component "data" value 2'
            write (1,*) 'end'
            
            close(1)

end subroutine display_nodes
!*********************************************************************************************************
subroutine display_vectors(reg,vectors,display)

use mod_3dfm

type(Tregion) :: reg
integer n
real(kind=dp) ::x,y,z,vectors(3,reg%nnode),xyz(3),a(3,3)
logical ::display(reg%nnode)

            open(1,file='vectors.dx')

            nnode=count(display)
 
            write (1,*) 'object 1 class array type float rank 1 shape 3 items',nnode, 'data follows'

            do n=1,reg%nnode
               if(display(n)) then
                  x=reg%r(n)*cos(reg%lat(n))*cos(reg%long(n))
                  y=reg%r(n)*cos(reg%lat(n))*sin(reg%long(n))
                  z=reg%r(n)*sin(reg%lat(n))
                  write(1,'(3f15.6)') y,z,x
               endif
            end do

            write (1,*)


            write (1,*) 'object 2 class array type float rank 1 shape 3 items',nnode, 'data follows'


            do n=1,reg%nnode
               if(display(n)) then

     a(1,1)=cos(reg%lat(n))*cos(reg%long(n)) ; a(1,2)=-sin(reg%lat(n))*cos(reg%long(n)) ; a(1,3)=-sin(reg%long(n))
     a(2,1)=cos(reg%lat(n))*sin(reg%long(n)) ; a(2,2)=-sin(reg%lat(n))*sin(reg%long(n)) ; a(2,3)= cos(reg%long(n))
     a(3,1)=sin(reg%lat(n))                  ; a(3,2)= cos(reg%lat(n))                  ; a(3,3)= 0.0_dp

                  xyz = matmul(a,vectors(1:3,n))

                  write(1,'(3f15.6)') xyz(2),xyz(3),xyz(1)
               endif

            end do

            write (1,*) 'attribute "dep" string "positions"'
            write (1,*)

            write (1,*) 'object "irregular positions irregular connections" class field'
            write (1,*) 'component "positions" value 1'
            write (1,*) 'component "data" value 2'
            write (1,*) 'end'
            
            close(1)

end subroutine display_vectors
