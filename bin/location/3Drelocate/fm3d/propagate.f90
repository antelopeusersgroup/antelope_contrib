! module mod_prop contains variables that relate to the fast marching through a
! region (grid between two interfaces including points on the interface)
!
module mod_prop
use mod_3dfm_nointerfaces

type(Tregion),pointer                              :: reg     ! the argument of the subroutine
type(Tpropagation_grid),pointer                    :: grid    ! grid on which the region is defined
real(kind=dp),dimension(:),pointer                 :: velocity
type(Tvelocity_grid),pointer                       :: velgrid

integer, dimension(:),pointer              :: node_from_tree_ind ! pointers from tree position to the associated node
integer, dimension(:),pointer              :: node_is_counted    ! array to keep track of whether a node is already
                                                                 ! counted when constructing a unique neighbour list
logical,dimension(:),pointer               :: suspect_time_gradient ! flag indicating that time gradient should be re-evaluated


real(kind=dp),dimension(:),pointer         :: cosla,sinla,coslo,sinlo ! precomputed sines and cosines of the regional nodes

type(Tinteger_coordinates),dimension(8)    :: concell            ! a list of cells connected to a given node
integer,dimension(200)                     :: connode            ! a list of nodes connected to a given node
integer,dimension(8)                       :: concell_nnode      ! the # of connected nodes in a concell
integer,dimension(8)                       :: concell_nbase      ! start of nodes from a cell in the list
logical,dimension(8)                       :: concell_is_regular ! true if cell contains no intersection nodes
integer,dimension(200)                     :: trialnode          ! the unique list of nodes connected to a given node


integer                                    :: ntree              ! # of nodes in the tree
integer                                    :: newalive           ! index of node most recently turned alive
integer                                    :: n_connode,n_concell! # of connected nodes or cells
integer                                    :: n_trial            ! number of neighbours to a node

integer                                    :: regular_order = 2
integer                                    :: n_det_negative


real(kind=dp)                              :: oldtime
integer,dimension(:),pointer               :: in
logical                                    :: diag = .false.
logical                                    :: diat = .false.
logical                                    :: fullcon = .false.
integer                                    :: testnode = 66566

end module mod_prop


!***********************************************************************************************
! the following subroutine does a fast marching sweep across the region that is
! its argument. Before calling this routine the regional nodes that have a starting time
! must have their values, and their node_status must be 0 for nodes whose values are already fixed,
! or 1 for nodes in the narrow band, the rest of the nodes must have node_status -1.
!
subroutine propagate(regin,vtype)

use mod_prop
implicit none

type(Tregion),target                       :: regin
integer                                    :: vtype
integer                                    :: n,nt ! local variables
logical                                    :: first_step

reg => regin
grid => reg%grid
velocity => reg%velocity(:,vtype)
velgrid => vgrid(reg%ivgrid,vtype)

n_det_negative = 0

!print *,'entering propagate'
!print *,'region',reg%id,reg%ivgrid
!print *,'top', reg%itop%id,reg%itop%iface_id
!print *,'bot', reg%ibot%id,reg%ibot%iface_id

if (reg%id == 99) then
   diat=.true.
   testnode=pgrid%rnode_id(37,9,5)
   write (24,*) 'testnode set to',testnode
   write (24,'(a20,3f12.5)') 'test coordinates',reg%r(testnode),reg%lat(testnode),reg%long(testnode)
   write (24,'(3i8)') reg%node(testnode)%i1,reg%node(testnode)%i2,reg%node(testnode)%i3
else
   diat=.false.
endif

! the time gradient derived during regular updates when fewer than 3 nodes are used can be very inaccurate
! set this flag in that case and correct afterwards

allocate(suspect_time_gradient(reg%nnode))
suspect_time_gradient = .false.

! precalculate required sines and cosines

allocate(cosla(reg%nnode),sinla(reg%nnode),coslo(reg%nnode),sinlo(reg%nnode))

cosla=cos(reg%lat)
sinla=sin(reg%lat)
coslo=cos(reg%long)
sinlo=sin(reg%long)


! set up the initial tree

allocate(node_from_tree_ind(reg%nnode),node_is_counted(reg%nnode))
node_is_counted=0
ntree = 0

do n=1,reg%nnode
   if (reg%node_status(n) == 1) call add_node_to_tree(n) 
end do


!start main propagation loop

first_step=.true.
do while (ntree > 0)

! take the node with the smallest time from the narrowband, set it to alive and adjust the tree

   newalive=node_from_tree_ind(1)
   reg%node_status(newalive) = 0
   call remove_root_from_tree

   if (ntree == 0 .and. .not.first_step) exit  ! stop when the tree structure is empty, but not in the first step since there may
                                               ! be only a single starting point (the source)

   first_step=.false.

   if (diag) print *,'root removed',newalive
   if (diag) print '(i5,3f12.5,3i5)',newalive,reg%r(newalive),reg%lat(newalive),reg%long(newalive),reg%node(newalive)

! find the neighbours of newalive, a list of node numbers is returned in the array connode 

   call find_connected_nodes(newalive)

   if (diag) print *,'neighbour list found',n_connode

   if (diag) then
      do n=1,n_connode
         print '(2i5,3f12.5,3i5)',n,connode(n),reg%r(connode(n)),reg%lat(connode(n)),reg%long(connode(n)),&
              reg%node(connode(n))
      end do

     print *
     print *,'arrivaltime at newalive is',reg%arrivaltime(newalive)
     print *

     stop
   endif

! save the list of neighbours, because the array connode will be used again when finding neighbours of the
! neighbours of newalive that have to be updated 

   n_trial=n_connode
   trialnode(1:n_trial)=connode(1:n_trial)


!  loop over neighbours and take appropopriate action

   do nt=1,n_trial

! if the neighbour is already in the narrow band, update it

      if (reg%node_status(trialnode(nt)) > 0) then

         oldtime=reg%arrivaltime(trialnode(nt))
         call update_time(trialnode(nt))

         if (diag) print '(i5,a6,f12.5,a6,f12.5)',nt,'nb ',reg%arrivaltime(trialnode(nt)),'was',oldtime

         if ( reg%arrivaltime(trialnode(nt)) < oldtime ) call update_tree(trialnode(nt))

      endif

! if the neighbour is far, add it to the narrow band

      if (reg%node_status(trialnode(nt)) == -1) then

         reg%arrivaltime(trialnode(nt)) = huge_time
         oldtime=reg%arrivaltime(trialnode(nt))
         call update_time(trialnode(nt))

         if (diag) print '(i5,a6,f12.5)',nt,'far',reg%arrivaltime(trialnode(nt))

         if ( reg%arrivaltime(trialnode(nt)) < oldtime ) call add_node_to_tree(trialnode(nt))

      endif
   end do   ! loop over neighbours of newalive

   if (diag) print *


end do  ! main propagation loop


deallocate(cosla,sinla,coslo,sinlo)
deallocate(node_from_tree_ind,node_is_counted)


if (n_det_negative > 0) print *,'warning!!!! determinant in regular update was negative ',n_det_negative,' times'


! correct suspect time gradients

   do n=1,reg%nnode
      if (suspect_time_gradient(n)) then
         call fit_gradient_at_node(reg,n,reg%time_gradient(1,n),reg%time_gradient(2,n),reg%time_gradient(3,n))
      endif
   end do

deallocate(suspect_time_gradient)

!-----------------------------------------------------------------

end subroutine propagate


!-----------------------------------------------------------------------------------------
! when returning,this intrinsic subroutine has filled the arrays giving the connected cells
! and the list of unique neighbours (connected nodes) of the node centernode

  subroutine find_connected_nodes(centernode)
    use mod_prop
    implicit none
    integer                                    :: i1,i2,i3           ! identify a node (i,j,k or 0,interface,inode)
    integer                                    :: n,m,i,j,k,ii,jj,kk,icell ! local variables
    integer   :: centernode
    type(Tintersection),pointer                :: isec
    

! store the identifiers of the centernode node in local variables. Reminder:
! for regular grid nodes i1,i2,i3  correspond to ir,ilat,ilong of the node
! for intersection nodes i1,i2,i3 are 0, intersection #, node # in intersection 

   i1 = reg%node(centernode)%i1 ; i2 = reg%node(centernode)%i2 ; i3 = reg%node(centernode)%i3

!   print *,'finding neighbours of',i1,i2,i3

   if (i1 /= 0) then
      if (grid%fully_regular(i1,i2,i3)) then

         n_connode = 6
         connode(1)=grid%rnode_id(i1-1,i2,i3)
         connode(2)=grid%rnode_id(i1+1,i2,i3)
         connode(3)=grid%rnode_id(i1,i2-1,i3)
         connode(4)=grid%rnode_id(i1,i2+1,i3)
         connode(5)=grid%rnode_id(i1,i2,i3-1)
         connode(6)=grid%rnode_id(i1,i2,i3+1)

         return

      endif
   endif

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

!print *,'connected cells'
!do n=1,n_concell
!   print *,n,concell(n)
!end do


! make a list of nodes in the connected cells

! make sure the centernode itself is not counted as one of the neighbours

   node_is_counted(centernode)=centernode

   n_connode=0

   do n=1,n_concell

      ! if centernode is a regular node (i1 /= 0) , the cell is potentially regular

      concell_is_regular(n) = (i1 /= 0)

! find the intersection nodes of connected cell n

!   explanation: each intersection has a 1-D list of cells cut by the interface, and a list of intersection
!   nodes that are part of each cut cell. Each regular grid cell has a pointer ccind_from_3dc(i,j,k)%p
!   (Cut Cell INDex FROM 3D Coordinates)
!   associated with it, where p is a pointer to an integer array with as many elements as there are intersections.
!   If a cell is cut by interface n, the pointer ccind_from_3dc(i,j,k)%p is allocated, and the variable
!   ccind_from_3dc(i,j,k)%p(n) contains the array index of cell (i,j,k) in the 1D cut cell list of intersection n 

! test whether the cell is cut by any interface, in that case the pointer to the local list of
! interfaces cutting the cell has been allocated

      if (associated(grid%ccind_from_3dc(concell(n)%ir,concell(n)%ilat,concell(n)%ilong)%p)) then

! if so, check if the cell is cut by the top intersection

     ! icell is the index of the current connected cell in the list of cells cut by interface reg%itop.
                  
         icell = grid%ccind_from_3dc(concell(n)%ir,concell(n)%ilat,concell(n)%ilong)%p(reg%itop%iface_id)

     ! if icell == 0 the cell is not cut be the top interface
    
         if(icell /= 0) then
 
            concell_is_regular(n) = .false.

            isec => reg%itop
            do jj=1,isec%n_inodes(icell)

!   m is the node number in the regional node list of node  jj in the list of inteface nodes that are part of cut cell icell

                m=isec%rbel_node_id(isec%inodes(jj,icell))

               if ( node_is_counted(m) /= centernode ) then   ! if node is not yet counted as a neighbour of centernode

                  n_connode=n_connode+1
                  connode(n_connode)=m
                  node_is_counted(m) = centernode

               endif

            end do

         end if


! then check the bottom intersection

         icell = grid%ccind_from_3dc(concell(n)%ir,concell(n)%ilat,concell(n)%ilong)%p(reg%ibot%iface_id)
         if (icell /= 0) then

            concell_is_regular(n) = .false.

            isec => reg%ibot
            do jj=1,isec%n_inodes(icell)

               m=isec%rabo_node_id(isec%inodes(jj,icell))

               if ( node_is_counted(m) /= centernode ) then 
                  n_connode=n_connode+1
                  connode(n_connode)=m
                  node_is_counted(m) = centernode

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
               if (((i1 /= 0 .and. abs(i1-ii)+abs(i2-jj)+abs(i3-kk) == 1)).or.(.not.concell_is_regular(n))) then

                 if (grid%node_region(ii,jj,kk) == reg%id) then  ! node has to belong to the current region

                     m=grid%rnode_id(ii,jj,kk)
                     if ( node_is_counted(m) /= centernode ) then 
                        n_connode=n_connode+1
                        connode(n_connode)=m
                        node_is_counted(m) = centernode
                     endif

                  endif

               endif

            end do
         end do
      end do

   end do  ! loop over connected cells


   return

  end subroutine find_connected_nodes


!-----------------------------------------------------------------------------------------
! when returning,this intrinsic subroutine has filled the arrays giving the connected cells
! of the node centernode and the ALIVE nodes that are part of these connected cells (non-unique,
! in the sense that nodes that are part of more than one connected cell appear more than one 
! time

  subroutine find_connected_cells(centernode)
    use mod_prop
    implicit none
    integer                                    :: i1,i2,i3           ! identify a node (i,j,k or 0,interface,inode)
    integer                                    :: n,m,i,j,k,ii,jj,kk,icell ! local variables
    integer  :: centernode
    type(Tintersection),pointer                :: isec

! store the identifiers of the centernode node in local variables. Reminder:
! for regular grid nodes i1,i2,i3  correspond to ir,ilat,ilong of the node
! for intersection nodes i1,i2,i3 are 0, intersection #, node # in intersection 

   i1 = reg%node(centernode)%i1 ; i2 = reg%node(centernode)%i2 ; i3 = reg%node(centernode)%i3


! make a list of grid cells of which the new node is part

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

! make a list of nodes in the connected cells

   n_connode=0

   do n=1,n_concell

     ! if centernode is a regular node (i1 /= 0) , the cell is potentially regular

     concell_is_regular(n) = (i1 /= 0)

! the intersection nodes of these cells

! test whether the cell is cut by any interface, in that case the pointer to the local list of
! interfaces cutting the cell has been allocated

      if (associated(grid%ccind_from_3dc(concell(n)%ir,concell(n)%ilat,concell(n)%ilong)%p)) then

! if so, check the top intersection

     ! icell is the index of the current connected cell in the list of cells cut by interface reg%itop
     ! if the current connected cell is not cut by this interface icell == 0

         icell = grid%ccind_from_3dc(concell(n)%ir,concell(n)%ilat,concell(n)%ilong)%p(reg%itop%iface_id)
         if(icell /= 0) then

            concell_is_regular(n)=.false.

            isec => reg%itop
            do jj=1,isec%n_inodes(icell)

               m=isec%rbel_node_id(isec%inodes(jj,icell))

               ! take only alive nodes (note that this excludes centernode itself)
               if ( reg%node_status(m) == 0 ) then
                  n_connode=n_connode+1
                  connode(n_connode)=m
               endif

            end do

         end if


! then do the same for the bottom intersection

         icell = grid%ccind_from_3dc(concell(n)%ir,concell(n)%ilat,concell(n)%ilong)%p(reg%ibot%iface_id)
         if (icell /= 0) then

            concell_is_regular(n)=.false.


            isec => reg%ibot

            if (icell < 1) print *, 'icell < 1',icell
            if (icell > isec%n_ccells) then
               print *,'icell > n_ccells',icell,isec%n_ccells
               print *,'cell',concell(n)%ir,concell(n)%ilat,concell(n)%ilong
               print *,'interface',ii
               print *,grid%ccind_from_3dc(concell(n)%ir,concell(n)%ilat,concell(n)%ilong)%p(1:4)
            endif

            do jj=1,isec%n_inodes(icell)

               m=isec%rabo_node_id(isec%inodes(jj,icell))
               if ( reg%node_status(m) == 0 ) then
                  n_connode=n_connode+1
                  connode(n_connode)=m
               endif

            end do

         endif

      endif


! the regular grid nodes of these cells

      do i=0,1
         ii=concell(n)%ir+i
         do j=0,1
            jj=concell(n)%ilat+j
            do k=0,1
               kk=concell(n)%ilong+k

               ! reduced connectivity for regular nodes if cell is completely regular
               if ((i1 /= 0 .and. abs(i1-ii)+abs(i2-jj)+abs(i3-kk) == 1).or.(.not.concell_is_regular(n))) then

                  if (grid%node_region(ii,jj,kk) == reg%id) then  ! node has to belong to the current region

                     m=grid%rnode_id(ii,jj,kk)
                     if ( reg%node_status(m) == 0 ) then  ! node has to be alive(note that this excludes centernode itself)
                        n_connode=n_connode+1
                        connode(n_connode)=m
                     endif

                  endif

               endif

            end do
         end do
      end do

      concell_nnode(n)=n_connode  

   end do  ! loop over connected cells


! store the starting index and length of the list of alive nodes for each cell, since updating is
! done by cell
 
   concell_nbase(1)=1
   do n=1,n_concell

      concell_nnode(n)=concell_nnode(n)-concell_nbase(n)+1
      if (n < n_concell) concell_nbase(n+1)=concell_nbase(n)+concell_nnode(n)

   end do

   return

  end subroutine find_connected_cells

!-------------------------------------------------------------------------
!
!
  subroutine update_time(update_node)
    use mod_prop
    implicit none

    integer       :: update_node
    integer       :: n,i,j,k            ! local variables
    integer       :: jl,kl, i1,i2,i3,id
    integer       :: anode(100),nanode  ! list of nodes used for the update


   i1 = reg%node(update_node)%i1 ; i2 = reg%node(update_node)%i2 ; i3 = reg%node(update_node)%i3

   if (i1 /= 0) then

      if (grid%fully_regular(i1,i2,i3)) then

!         print *,'node is fully regular'     ! deldiag

         do k=-1,1,2
            do j=-1,1,2
               do i=-1,1,2

                  nanode=0

                  id = grid%rnode_id(i1+i,i2,i3)
                  if (reg%node_status(id) == 0) then
                     nanode=nanode+1
                     anode(nanode)= id
                  endif
                  id = grid%rnode_id(i1,i2+j,i3)
                  if (reg%node_status(id) == 0) then
                     nanode=nanode+1
                     anode(nanode)= id
                  endif
                  id = grid%rnode_id(i1,i2,i3+k)
                  if (reg%node_status(id) == 0) then
                     nanode=nanode+1
                     anode(nanode)= id
                  endif

                  if (nanode > 0)  then

                  ! put newalive at position 1 if it is there
                     do n=1,nanode
                        if (anode(n) == newalive) then
                           anode(n)=anode(1)
                           anode(1)=newalive
                        endif
                     end do

                     if (anode(1) == newalive) call regular_update2(update_node,anode(1:nanode),nanode)

                  endif

               end do
            end do
         end do

         return

      endif

   endif

!
! first find the cells to which the node to be updated is connected, and the list of
! ALIVE nodes in these cells

    call find_connected_cells(update_node)

! now loop over the connected cells, and if possible use the alive nodes
! in each cell to find a new arrival time at the node to be updated
 
    do n=1,n_concell


       if (concell_nnode(n) > 0) then   ! if there are any alive nodes in this cell


! copy alive nodes in this connected cell to local array
       nanode=concell_nnode(n)

       if (nanode > 30) then
          print *,'nanode =',nanode

          do i=1,nanode

             print'(4i5,4f12.5)',i,reg%node(anode(i))%i1,reg%node(anode(i))%i2,reg%node(anode(i))%i3, &
                  reg%r(anode(i)),reg%lat(anode(i)),reg%long(anode(i))

          enddo
          stop 'subroutine update_time: nanode >30'
       endif

       anode(1:nanode)=connode(concell_nbase(n):concell_nbase(n)+nanode-1)


! put newalive at position 1 if it is there
       do i=1,nanode
          if (anode(i) == newalive) then
             j=anode(1)
             anode(1)=anode(i)
             anode(i)=j
          endif
       end do

       if (diat) then
          if (update_node == testnode) then
             write(24,*) '************************'
             write(24,'(2i8,a15,3i5)') update_node,newalive,'connected cell',concell(n)
             write(24,'(10i12)')anode(1:nanode)
             write(24,'(10i12)')reg%node_status(anode(1:nanode))
             write(24,'(10i12)')reg%node(anode(1:nanode))%i1
             write(24,'(10i12)')reg%node(anode(1:nanode))%i2
             write(24,'(10i12)')reg%node(anode(1:nanode))%i3
             write(24,'(10f12.5)')reg%r(anode(1:nanode))
             write(24,'(10f12.5)')reg%lat(anode(1:nanode))
             write(24,'(10f12.5)')reg%long(anode(1:nanode))
             write(24,'(10f12.5)')reg%arrivaltime(anode(1:nanode))
          endif
       endif


    ! test if newalive is among the nodes that will be used for the update
    ! otherwise the update has already been done and does not have to be repeated

          if (anode(1) == newalive) then 

             if (concell_is_regular(n)) then

                ! do a regular fast marching update, second order if possible, in this octant

                call regular_update2(update_node,anode(1:nanode),nanode)

             else

             ! do an irregular update in this octant

                select case (nanode)
          
                case(1)     ! there is 1 alive node in the cell

                   call time_from_1_node(update_node,anode(1))

                case(2)     ! there are 2 alive nodes in the cell

                   call time_from_1_node(update_node,anode(1))
                   call time_from_2_nodes(update_node,anode(1),anode(2))

                case(3)     ! there are 3 alive nodes in the cell

                   call time_from_1_node(update_node,anode(1))
                   call time_from_2_nodes(update_node,anode(1),anode(2))
                   call time_from_2_nodes(update_node,anode(1),anode(3))
                   call time_from_3_nodes(update_node,anode(1),anode(2),anode(3))

                case(4:)     ! there are more than 3 alive nodes in the cell

                   call time_from_1_node(update_node,anode(1))

                   do jl=2,nanode
                      call time_from_2_nodes(update_node,anode(1),anode(jl))
                   end do

                   do jl=2,nanode-1
                      do kl=jl+1,nanode
                         call time_from_3_nodes(update_node,anode(1),anode(jl),anode(kl))
                      end do
                   end do


                end select

             endif   ! regular or irregular

          endif ! if newalive is among the nodes used for the update

       endif   ! if there are any alive nodes in this cell


    end do ! loop over connected cells


  end subroutine update_time

!***********************************************************************************************
subroutine time_from_1_node(unode,node1)
  use mod_prop
  implicit none

  integer           :: unode,node1
  real(kind=dp)     :: atime,ttime,dist,xu(3),x1(3) 
  real(kind=dp)     :: a(3,3),b(3)

!  print *,'updating from 1 node'     ! deldiag


! for all irregular updates the coordinates of the nodes are first converted to Cartesian
! afterwards the time gradient( wave vector of updating wave) is converted back to spherical

  xu(1)=reg%r(unode)*cosla(unode)*coslo(unode)
  xu(2)=reg%r(unode)*cosla(unode)*sinlo(unode)
  xu(3)=reg%r(unode)*sinla(unode)

  x1(1)=reg%r(node1)*cosla(node1)*coslo(node1)
  x1(2)=reg%r(node1)*cosla(node1)*sinlo(node1)
  x1(3)=reg%r(node1)*sinla(node1)

  dist=sqrt(sum((xu-x1)**2))

  if (dist < 0.01_dp*pgrid%tolerance) then
     reg%arrivaltime(unode) = reg%arrivaltime(node1)
     reg%time_gradient(1:3,unode) =  reg%time_gradient(1:3,node1) 
     return
   endif


  ttime= dist/(0.5*(velocity(unode)+velocity(node1)))
  atime= reg%arrivaltime(node1) + ttime

  if ( atime < reg%arrivaltime(unode)) then

     reg%arrivaltime(unode) = atime

! convert time gradient back to spherical

     b=(xu-x1)/(dist*velocity(unode))
     a(1,1)=cosla(unode)*coslo(unode) ; a(2,1)=-sinla(unode)*coslo(unode) ; a(3,1)=-sinlo(unode)
     a(1,2)=cosla(unode)*sinlo(unode) ; a(2,2)=-sinla(unode)*sinlo(unode) ; a(3,2)= coslo(unode)
     a(1,3)=sinla(unode)              ; a(2,3)= cosla(unode)              ; a(3,3)= 0.0_dp
     reg%time_gradient(1:3,unode) = matmul(a,b)

     call refract_locally(reg%r(unode),reg%lat(unode),reg%long(unode),velgrid, &
          reg%time_gradient(1:3,unode))

     if (diat.and.unode == testnode) write (24,*) 'arrival adjusted'

  endif

  if (diat.and.unode == testnode) write(24,'(a20,2i8,2f12.5)') 'updating from 1 node',newalive,node1,&
       reg%arrivaltime(unode),reg%arrivaltime(node1)

  return

end subroutine time_from_1_node


!**********************************************************************************************
subroutine time_from_2_nodes(unode,anode1,anode2)
  use mod_prop
  implicit none

  integer           :: unode,node1,node2,anode1,anode2
  real(kind=dp)     :: atime  

  real(kind=dp)     :: wn1(3),wn2(3)
  real(kind=dp)     :: a(3,3),b(3),p(3),q(3),u1,u2,det,ae,be,ce
  real(kind=dp)     :: n1(3),n2(3),r1(3),r2(3),n0(3),xu(3),x1(3),x2(3)

  integer           :: i,vscount,indx(3),status


! we take a geometric approach to solving for the wavefront normal n. The 3 equations
! to be fulfilled are:
! t2 = t1+n.(x2-x1)/v12        --> a(1,1)*n1 + a(1,2)*n2 + a(1,3)*n3  = b(1)
!  0 = n . [(x2-x) x (x1-x)]   --> a(2,1)*n1 + a(2,2)*n2 + a(2,3)*n3  = b(2)
! |n| = 0                      --> n1**2 +   n2**2 +  n3**2   = 1
!
! The second equation comes from the condition that the wavefront normal n lies in the plane
! containing the 2 known points and the point at which the arrivaltime is evaluated.
! The method of solution first calculates the line defining the intersection of
! the two planes defined by the two linear equations, then the 2 intersections of this line
! with  the sphere defined by the normalization constraint


  if (diat.and.unode == testnode) write(24,*) 'updating from 2 nodes'

  node1=anode1 ; node2=anode2 

! order from latest to earliest arrival time

  if (reg%arrivaltime(node1) < reg%arrivaltime(node2)) then ; i=node1 ; node1=node2 ; node2=i ; endif


! convert nodes to Cartesian

  xu(1)=reg%r(unode)*cosla(unode)*coslo(unode)
  xu(2)=reg%r(unode)*cosla(unode)*sinlo(unode)
  xu(3)=reg%r(unode)*sinla(unode)

  x1(1)=reg%r(node1)*cosla(node1)*coslo(node1)
  x1(2)=reg%r(node1)*cosla(node1)*sinlo(node1)
  x1(3)=reg%r(node1)*sinla(node1)

  x2(1)=reg%r(node2)*cosla(node2)*coslo(node2)
  x2(2)=reg%r(node2)*cosla(node2)*sinlo(node2)
  x2(3)=reg%r(node2)*sinla(node2)


! first define some local variables (r1,r2 difference vectors between node1,node2 and unode)

  r1=x1-xu
  r2=x2-xu


! first check if nodes coincide, if so copy times and gradients and return

  if (sqrt(sum(r1**2)) < 0.01_dp*pgrid%tolerance) then
     reg%arrivaltime(unode) = reg%arrivaltime(node1)
     reg%time_gradient(1:3,unode) =  reg%time_gradient(1:3,node1) 
     return
  endif
  if (sqrt(sum(r2**2)) < 0.01_dp*pgrid%tolerance) then
     reg%arrivaltime(unode) = reg%arrivaltime(node2)
     reg%time_gradient(1:3,unode) =  reg%time_gradient(1:3,node2) 
     return
  endif
  if (sqrt(sum((x2-x1)**2)) < 0.01_dp*pgrid%tolerance) then
     call time_from_1_node(unode,node1)
     return
  endif

 ! construct the two linear equations
  b(1)=reg%arrivaltime(node1)-reg%arrivaltime(node2)
  b(2)=0.0_dp

!  a(1,1:3)=(x1-x2)/velocity(node1)
  a(1,1:3)=(x1-x2)/(0.5_dp*(velocity(node1)+velocity(node2)))


  a(2,1)=r1(2)*r2(3)-r1(3)*r2(2)
  a(2,2)=r1(3)*r2(1)-r1(1)*r2(3)
  a(2,3)=r1(1)*r2(2)-r1(2)*r2(1)

  n0=a(2,:)    ! save the normal to the plane containing the 3 points (2 alive+update) for future use


! the direction of the line that is the intersection between the two planes defined by the
! two linear equations is the cross product  q = a(1,1:3) x a(2,1:3)

  q(1)=a(1,2)*a(2,3)-a(1,3)*a(2,2)
  q(2)=a(1,3)*a(2,1)-a(1,1)*a(2,3)
  q(3)=a(1,1)*a(2,2)-a(1,2)*a(2,1)


! a third equation is added that defines the plane that is normal to the direction of the 
! intersection line and goes through (0,0,0)

  b(3)=0.0_dp
  a(3,1)=q(1) ; a(3,2)=q(2) ; a(3,3)=q(3)


! now find the intersection point of the intersection line with the plane that is normal to it
! and goes through (0,0,0). This gives the minimum length support vector

  call ludcmp(a,indx,status)
  if (status == 1) then

     print *,a(1,1:3),b(1)
     print *,a(2,1:3),b(2)
     print *,a(3,1:3),b(3)

     stop 'singular matrix in time_from_2_nodes'
  endif
  call lubksb(a,indx,b)
  p=b

! we now have the intersection line defined as p+uq , where u is a scalar variable
! solve the quadratic normalization constraint |n| = 1 for u

   ae=q(1)*q(1)+q(2)*q(2)+q(3)*q(3)
   be=2.0_dp*(p(1)*q(1)+p(2)*q(2)+p(3)*q(3))
   ce=p(1)*p(1)+p(2)*p(2)+p(3)*p(3)-1.0_dp

   det=be*be-4.0_dp*ae*ce

   if (det < 0.0_dp) return   ! there is no valid solution if the line does not intersect the sphere


   u1=0.5_dp*(-be+sqrt(det))/ae
   u2=0.5_dp*(-be-sqrt(det))/ae

! the two solutions for the wave vector

   wn1=p+u1*q
   wn2=p+u2*q

  if (diat.and.unode == testnode) write(24,*)' wn1',wn1
  if (diat.and.unode == testnode) write(24,*)' wn2',wn2

! now test if either of these two solutions is valid (obeys the causality constraint)
! -wn  must lie within the wedge defined by the planes containing x1-x0 or x2-x0
! and the normal to the plane of the three points x0,x1,x2, which is just the vector a(2,:)
! calculated above (saved in n0 before calling ludcmp because a is modified there)


! calculate the inward pointing normals of the two planes defining the wedge

   n1(1)=n0(2)*r1(3)-n0(3)*r1(2)
   n1(2)=n0(3)*r1(1)-n0(1)*r1(3)
   n1(3)=n0(1)*r1(2)-n0(2)*r1(1)
   n1=n1/sqrt(sum(n1**2))
   if (n1(1)*r2(1)+n1(2)*r2(2)+n1(3)*r2(3) < 0.0_dp) n1=-n1   ! define the sign of the normal so the other alive point is
                                                              ! on the positive side 
   n2(1)=n0(2)*r2(3)-n0(3)*r2(2)
   n2(2)=n0(3)*r2(1)-n0(1)*r2(3)
   n2(3)=n0(1)*r2(2)-n0(2)*r2(1)
   n2=n2/sqrt(sum(n2**2))
   if (n2(1)*r1(1)+n2(2)*r1(2)+n2(3)*r1(3) < 0.0_dp) n2=-n2   ! define the sign of the normal so the other alive point is
                                                              ! on the positive side 


  if (diat.and.unode == testnode) write(24,*)' n1',n1
  if (diat.and.unode == testnode) write(24,*)' n2',n2

  if (diat.and.unode == testnode) write(24,*)'dp1 ',dot_product(wn1,n1) , dot_product(wn1,n2)
  if (diat.and.unode == testnode) write(24,*)'dp2 ',dot_product(wn2,n1) , dot_product(wn2,n2)

   vscount=0
   if (dot_product(wn1,n1) <= 0.0_dp .and. dot_product(wn1,n2) <= 0.0_dp ) then

      atime= reg%arrivaltime(node1)-dot_product(wn1,r1)/(0.5_dp*(velocity(unode)+velocity(node1)))
      if (atime >= reg%arrivaltime(node1)) then
         if ( atime < reg%arrivaltime(unode)) then
            reg%arrivaltime(unode) = atime

            b=wn1/(sqrt(sum(wn1**2))*velocity(node1))
            a(1,1)=cosla(unode)*coslo(unode) ; a(2,1)=-sinla(unode)*coslo(unode) ; a(3,1)=-sinlo(unode)
            a(1,2)=cosla(unode)*sinlo(unode) ; a(2,2)=-sinla(unode)*sinlo(unode) ; a(3,2)= coslo(unode)
            a(1,3)=sinla(unode)              ; a(2,3)= cosla(unode)              ; a(3,3)= 0.0_dp
            reg%time_gradient(1:3,unode) = matmul(a,b)

            x1=reg%time_gradient(1:3,unode)

            call refract_locally(reg%r(unode),reg%lat(unode),reg%long(unode),velgrid, &
                 reg%time_gradient(1:3,unode))

            if (diat.and.unode == testnode) then
               write (24,*) 'arrival adjusted'
               write(24,'(a5,3f12.6)') 'wn1',wn1
               write(24,'(a5,3f12.6)') 'b  ',b
               write(24,'(a5,3f12.6)') 'tg ',x1
               write(24,'(a5,3f12.6)') 'tgr',reg%time_gradient(1:3,unode)

            endif
         endif
         vscount=vscount+1
   if (diat.and.unode == testnode) write (24,'(a20,3i8,2f12.5)') 'update from 2 node',newalive,node1,node2,&
        reg%arrivaltime(unode),reg%arrivaltime(node1)
      endif

   end if

   if (dot_product(wn2,n1) <= 0.0_dp .and. dot_product(wn2,n2) <= 0.0_dp ) then

      atime= reg%arrivaltime(node1)-dot_product(wn2,r1)/(0.5_dp*(velocity(unode)+velocity(node1)))
      if (atime >= reg%arrivaltime(node1)) then
         if ( atime < reg%arrivaltime(unode)) then
            reg%arrivaltime(unode) = atime

            b=wn2/(sqrt(sum(wn2**2))*velocity(node1))
            a(1,1)=cosla(unode)*coslo(unode) ; a(2,1)=-sinla(unode)*coslo(unode) ; a(3,1)=-sinlo(unode)
            a(1,2)=cosla(unode)*sinlo(unode) ; a(2,2)=-sinla(unode)*sinlo(unode) ; a(3,2)= coslo(unode)
            a(1,3)=sinla(unode)              ; a(2,3)= cosla(unode)              ; a(3,3)= 0.0_dp
            reg%time_gradient(1:3,unode) = matmul(a,b)

            call refract_locally(reg%r(unode),reg%lat(unode),reg%long(unode),velgrid, &
                 reg%time_gradient(1:3,unode))

            if (diat.and.unode == testnode) write (24,*) 'arrival adjusted'
         endif
         vscount=vscount+1
   if (diat.and.unode == testnode) write (24,'(a20,3i8,2f12.5)') 'update from 2 node',newalive,node1,node2, &
        reg%arrivaltime(unode),reg%arrivaltime(node1)
      endif
   end if

   if (vscount == 2 .and. u1 /= u2 ) then
      if (dot_product(wn1,wn2) < 0.99999_dp) then
      print *,' r1',r1
      print *,' wn1',wn1
      print *,' wn2',wn2
      print *,'dp1 ',dot_product(wn1,n1) , dot_product(wn1,n2)
      print *,'dp2 ',dot_product(wn2,n1) , dot_product(wn2,n2)
      stop 'time_from_2_nodes: two different valid solutions found'
      endif
   endif
   return

end subroutine time_from_2_nodes

!**********************************************************************************************
subroutine time_from_3_nodes(unode,anode1,anode2,anode3)
  use mod_prop
  implicit none

  integer           :: unode,node1,node2,node3,anode1,anode2,anode3
  real(kind=dp)     :: atime  


  real(kind=dp)     :: wn1(3),wn2(3)
  real(kind=dp)     :: a(3,3),b(3),p(3),q(3),u1,u2,det,ae,be,ce,dist_from_plane,cangle(3)
  real(kind=dp)     :: n1(3),n2(3),n3(3),r1(3),r2(3),r3(3),xu(3),x1(3),x2(3),x3(3)

  integer           :: i,vscount,indx(3),status,min_loc(1)

  logical           :: wn1_inplane,wn2_inplane


! we take a geometric approach to solving for the wavefront normal n . The 3 equations
! to be fulfilled are:
! t2=t1+n.(x2-x1)/v12   --> a(1,1)*n1 + a(1,2)*n2 + a(1,3)*n3  = b(1)
! t3=t1+n.(x3-x1)/v13   --> a(2,1)*n1 + a(2,2)*n2 + a(2,3)*n3  = b(2)
! |n| = 0               --> n1**2 +   n2**2 +  n3**2   = 1
! the method of solution first calculates the line defining the intersection of
! the two planes defined by the two linear equations, then the intersections of this line
! with  the sphere defined by the normalization constraint


  if (diat.and.unode == testnode) write(24,*) 'updating from 3 nodes',node1,node2,node3

  node1=anode1 ; node2=anode2 ; node3=anode3

! order from latest to earliest arrival time

  if (reg%arrivaltime(node1) < reg%arrivaltime(node2)) then ; i=node1 ; node1=node2 ; node2=i ; endif
  if (reg%arrivaltime(node2) < reg%arrivaltime(node3)) then ; i=node2 ; node2=node3 ; node3=i ; endif
  if (reg%arrivaltime(node1) < reg%arrivaltime(node2)) then ; i=node1 ; node1=node2 ; node2=i ; endif


! construct the two linear equations
  b(1)=reg%arrivaltime(node1)-reg%arrivaltime(node2)
  b(2)=reg%arrivaltime(node1)-reg%arrivaltime(node3)

! convert to Cartesian

  xu(1)=reg%r(unode)*cosla(unode)*coslo(unode)
  xu(2)=reg%r(unode)*cosla(unode)*sinlo(unode)
  xu(3)=reg%r(unode)*sinla(unode)

  x1(1)=reg%r(node1)*cosla(node1)*coslo(node1)
  x1(2)=reg%r(node1)*cosla(node1)*sinlo(node1)
  x1(3)=reg%r(node1)*sinla(node1)

  x2(1)=reg%r(node2)*cosla(node2)*coslo(node2)
  x2(2)=reg%r(node2)*cosla(node2)*sinlo(node2)
  x2(3)=reg%r(node2)*sinla(node2)

  x3(1)=reg%r(node3)*cosla(node3)*coslo(node3)
  x3(2)=reg%r(node3)*cosla(node3)*sinlo(node3)
  x3(3)=reg%r(node3)*sinla(node3)


   r1=x1-xu
   r2=x2-xu
   r3=x3-xu


! first check if nodes coincide, if so copy times and gradients and return

  if (sqrt(sum(r1**2)) < 0.01_dp*pgrid%tolerance) then
      atime= reg%arrivaltime(node1)
      reg%arrivaltime(unode) = reg%arrivaltime(node1)
      reg%time_gradient(1:3,unode) =  reg%time_gradient(1:3,node1) 
      return
   endif
  if (sqrt(sum(r2**2)) < 0.01_dp*pgrid%tolerance) then
     reg%arrivaltime(unode) = reg%arrivaltime(node2)
     reg%time_gradient(1:3,unode) =  reg%time_gradient(1:3,node2) 
     return
   endif     
  if (sqrt(sum(r3**2)) < 0.01_dp*pgrid%tolerance) then
     reg%arrivaltime(unode) = reg%arrivaltime(node3)
     reg%time_gradient(1:3,unode) =  reg%time_gradient(1:3,node3) 
     return
   endif    
  if (sqrt(sum((x2-x1)**2)) < 0.1_dp*pgrid%tolerance) then
     call time_from_2_nodes(unode,node1,node3)
     return
  endif
  if (sqrt(sum((x2-x3)**2)) < 0.1_dp*pgrid%tolerance) then
     call time_from_2_nodes(unode,node1,node2)
     return
  endif
  if (sqrt(sum((x3-x1)**2)) < 0.1_dp*pgrid%tolerance) then
     call time_from_2_nodes(unode,node1,node2)
     return
  endif

!  a(1,1:3)=(x1-x2)/velocity(node1)
!  a(2,1:3)=(x1-x3)/velocity(node1)
  a(1,1:3)=(x1-x2)/(0.5_dp*(velocity(node1)+velocity(node2)))
  a(2,1:3)=(x1-x3)/(0.5_dp*(velocity(node1)+velocity(node3)))

! the direction of the line that is the intersection between the two planes defined by the
! two linear equations is the cross product  q = a(1:3) x b(1:3)

  q(1)=a(1,2)*a(2,3)-a(1,3)*a(2,2)
  q(2)=a(1,3)*a(2,1)-a(1,1)*a(2,3)
  q(3)=a(1,1)*a(2,2)-a(1,2)*a(2,1)



! a third equation is added that defines he plane that is normal to the direction of the 
! intersection line and goes through (0,0,0)

  b(3)=0.0_dp
  a(3,1)=q(1) ; a(3,2)=q(2) ; a(3,3)=q(3)


! now find the intersection point of the intersection line with the plane

  call ludcmp(a,indx,status)
  if (status == 1) stop 'singular matrix in time_from_3_nodes'
  call lubksb(a,indx,b)
  p=b


! we now have the intersection line defined as p+uq , where u is a scalar variable
! solve the quadratic normalization constraint |n| = 1 for u

   ae=q(1)*q(1)+q(2)*q(2)+q(3)*q(3)
   be=2.0_dp*(p(1)*q(1)+p(2)*q(2)+p(3)*q(3))
   ce=p(1)*p(1)+p(2)*p(2)+p(3)*p(3)-1.0_dp

   det=be*be-4.0_dp*ae*ce

   if (det < 0.0_dp) return  ! if the line does not intersect the sphere there is no valid solution

   u1=0.5_dp*(-be+sqrt(det))/ae
   u2=0.5_dp*(-be-sqrt(det))/ae


! the two solutions for the wavefront normal

   wn1=p+u1*q
   wn2=p+u2*q

   if (diat.and.unode == testnode) write(24,*) ' wn1',wn1
   if (diat.and.unode == testnode) write(24,*) ' wn2',wn2

! now test if either of these two solutions is valid (obeys the causality constraint)
! -wn  must lie within the sector defined by the vectors x1-x0,x2-x0,x3-x0



   if (diat.and.unode == testnode) write(24,*) 'r1 ',r1
   if (diat.and.unode == testnode) write(24,*) 'r2 ',r2
   if (diat.and.unode == testnode) write(24,*) 'r3 ',r3

! calculate the inward pointing normals to the three planes defining the sector

   n1(1)=r1(2)*r2(3)-r1(3)*r2(2)
   n1(2)=r1(3)*r2(1)-r1(1)*r2(3)
   n1(3)=r1(1)*r2(2)-r1(2)*r2(1)
   n1=n1/sqrt(sum(n1**2))
 

! check for coplanarity of update and alive points, since then the causality criterion 
! becomes degenerate and breaks down
    
   dist_from_plane= (n1(1)*r3(1)+n1(2)*r3(2)+n1(3)*r3(3))/sqrt(sum(r3**2))

   if (abs(dist_from_plane) < 0.001_dp) then ! update node coplanar with alive points used for update

!      if (reg%node_status(unode) == 1) return


      wn1_inplane=abs(dot_product(wn1,n1)) < 0.001_dp
      wn2_inplane=abs(dot_product(wn2,n1)) < 0.001_dp

   ! quick test whether any of the two wave front normals lies in the nodal plane
   ! this will reject most cases with little computation

      if (.not.(wn1_inplane.or.wn2_inplane)) then

         if (diat.and.unode == testnode) write(24,*) 'coplanar reject1 ',unode,node1,node2,node3
         return

      else

         ! find the two nodes with the largest angle between them, and apply 2 point update with them

         cangle(1) = dot_product(r2,r3)/sqrt(sum(r2**2)*sum(r3**2))
         cangle(2) = dot_product(r3,r1)/sqrt(sum(r3**2)*sum(r1**2))
         cangle(3) = dot_product(r1,r2)/sqrt(sum(r1**2)*sum(r2**2))

         min_loc=minloc(cangle)

         select case(min_loc(1))

            case(1)
               n2(1)=n1(2)*r3(3)-n1(3)*r3(2)
               n2(2)=n1(3)*r3(1)-n1(1)*r3(3)
               n2(3)=n1(1)*r3(2)-n1(2)*r3(1)
               n2=n2/sqrt(sum(n2**2))
               if (n2(1)*r1(1)+n2(2)*r1(2)+n2(3)*r1(3) < 0.0_dp) n2=-n2

               n3(1)=n1(2)*r2(3)-n1(3)*r2(2)
               n3(2)=n1(3)*r2(1)-n1(1)*r2(3)
               n3(3)=n1(1)*r2(2)-n1(2)*r2(1)
               n3=n3/sqrt(sum(n3**2))
               if (n3(1)*r1(1)+n3(2)*r1(2)+n3(3)*r1(3) < 0.0_dp) n3=-n3

            case(2)
               n2(1)=n1(2)*r3(3)-n1(3)*r3(2)
               n2(2)=n1(3)*r3(1)-n1(1)*r3(3)
               n2(3)=n1(1)*r3(2)-n1(2)*r3(1)
               n2=n2/sqrt(sum(n2**2))
               if (n2(1)*r2(1)+n2(2)*r2(2)+n2(3)*r2(3) < 0.0_dp) n2=-n2

               n3(1)=n1(2)*r1(3)-n1(3)*r1(2)
               n3(2)=n1(3)*r1(1)-n1(1)*r1(3)
               n3(3)=n1(1)*r1(2)-n1(2)*r1(1)
               n3=n3/sqrt(sum(n3**2))
               if (n3(1)*r2(1)+n3(2)*r2(2)+n3(3)*r2(3) < 0.0_dp) n3=-n3

            case(3)
               n2(1)=n1(2)*r1(3)-n1(3)*r1(2)
               n2(2)=n1(3)*r1(1)-n1(1)*r1(3)
               n2(3)=n1(1)*r1(2)-n1(2)*r1(1)
               n2=n2/sqrt(sum(n2**2))
               if (n2(1)*r3(1)+n2(2)*r3(2)+n2(3)*r3(3) < 0.0_dp) n2=-n2

               n3(1)=n1(2)*r2(3)-n1(3)*r2(2)
               n3(2)=n1(3)*r2(1)-n1(1)*r2(3)
               n3(3)=n1(1)*r2(2)-n1(2)*r2(1)
               n3=n3/sqrt(sum(n3**2))
               if (n3(1)*r3(1)+n3(2)*r3(2)+n3(3)*r3(3) < 0.0_dp) n3=-n3

         end select

         vscount=0
         if (dot_product(wn1,n2) <= 0.0_dp .and. dot_product(wn1,n3) <= 0.0_dp .and. wn1_inplane) then
            
            atime= reg%arrivaltime(node1)-dot_product(wn1,r1)/(0.5_dp*(velocity(unode)+velocity(node1)))
            if (atime >= reg%arrivaltime(node1)) then
               if ( atime < reg%arrivaltime(unode)) then

                  reg%arrivaltime(unode) = atime

                  b=wn1/(sqrt(sum(wn1**2))*velocity(node1))
                  a(1,1)=cosla(unode)*coslo(unode) ; a(2,1)=-sinla(unode)*coslo(unode) ; a(3,1)=-sinlo(unode)
                  a(1,2)=cosla(unode)*sinlo(unode) ; a(2,2)=-sinla(unode)*sinlo(unode) ; a(3,2)= coslo(unode)
                  a(1,3)=sinla(unode)              ; a(2,3)= cosla(unode)              ; a(3,3)= 0.0_dp
                  reg%time_gradient(1:3,unode) = matmul(a,b)

                  call refract_locally(reg%r(unode),reg%lat(unode),reg%long(unode),velgrid, &
                       reg%time_gradient(1:3,unode))

                  if (diat.and.unode == testnode) write (24,*) 'arrival adjusted'
               endif

               vscount=vscount+1
               if(diat.and.unode == testnode) write(24,'(a15,4i8,2f12.5)') '3 node updatec',newalive,node1,node2,node3,&
                    reg%arrivaltime(unode),reg%arrivaltime(node1)
            endif

         end if

         if (dot_product(wn2,n2) <= 0.0_dp .and. dot_product(wn2,n3) <= 0.0_dp .and. wn2_inplane) then

            atime= reg%arrivaltime(node1)-dot_product(wn2,r1)/(0.5_dp*(velocity(unode)+velocity(node1)))
            if (atime >= reg%arrivaltime(node1)) then
               if ( atime < reg%arrivaltime(unode)) then

                  reg%arrivaltime(unode) = atime

                  b=wn2/(sqrt(sum(wn2**2))*velocity(node1))
                  a(1,1)=cosla(unode)*coslo(unode) ; a(2,1)=-sinla(unode)*coslo(unode) ; a(3,1)=-sinlo(unode)
                  a(1,2)=cosla(unode)*sinlo(unode) ; a(2,2)=-sinla(unode)*sinlo(unode) ; a(3,2)= coslo(unode)
                  a(1,3)=sinla(unode)              ; a(2,3)= cosla(unode)              ; a(3,3)= 0.0_dp
                  reg%time_gradient(1:3,unode) = matmul(a,b)

                  call refract_locally(reg%r(unode),reg%lat(unode),reg%long(unode),velgrid, &
                       reg%time_gradient(1:3,unode))

                  if (diat.and.unode == testnode) write (24,*) 'arrival adjusted'
               endif
               vscount=vscount+1
               if(diat.and.unode == testnode) write(24,'(a15,4i8,2f12.5)') '3 node updatec',newalive,node1,node2,node3,&
                    reg%arrivaltime(unode),reg%arrivaltime(node1)
            endif
         end if

         if (vscount == 2 .and. u1 /= u2 ) then
            if (dot_product(wn1,wn2) < 0.999_dp) then
               print *,'minloc(1)',min_loc(1)
               print *,' r1',r1
               print *,' r2',r2
               print *,' r3',r3
               print *,' wn1',wn1
               print *,' wn2',wn2
               print *,'dpt ',dot_product(wn1,n1),dot_product(wn2,n1)
               print *,'dp1 ',dot_product(wn1,n2) , dot_product(wn1,n3)
               print *,'dp2 ',dot_product(wn2,n2) , dot_product(wn2,n3)
               stop 'time_from_3_nodes: two different valid solutions found in coplanar case'
            endif
         endif

         return

      endif  ! coplanar case passing first rejection test

   endif  ! coplanar case

   if (dist_from_plane < 0.0_dp) n1=-n1                       ! define the sign of the normal so the other alive point is
                                                              ! on the positive side 

   n2(1)=r1(2)*r3(3)-r1(3)*r3(2)
   n2(2)=r1(3)*r3(1)-r1(1)*r3(3)
   n2(3)=r1(1)*r3(2)-r1(2)*r3(1)
   n2=n2/sqrt(sum(n2**2))
   if (n2(1)*r2(1)+n2(2)*r2(2)+n2(3)*r2(3) < 0.0_dp) n2=-n2   ! define the sign of the normal so the other alive point is
                                                              ! on the positive side 

   n3(1)=r2(2)*r3(3)-r2(3)*r3(2)
   n3(2)=r2(3)*r3(1)-r2(1)*r3(3)
   n3(3)=r2(1)*r3(2)-r2(2)*r3(1)
   n3=n3/sqrt(sum(n3**2))
   if (n3(1)*r1(1)+n3(2)*r1(2)+n3(3)*r1(3) < 0.0_dp) n3=-n3   ! define the sign of the normal so the other alive point is
                                                              ! on the positive side 

   if (diat.and.unode == testnode) write(24,*) 'n1 ',n1
   if (diat.and.unode == testnode) write(24,*) 'n2 ',n2
   if (diat.and.unode == testnode) write(24,*) 'n3 ',n3
   if (diat.and.unode == testnode) write(24,*) 'dp1 ',dot_product(wn1,n1) , dot_product(wn1,n2), dot_product(wn1,n3)
   if (diat.and.unode == testnode) write(24,*) 'dp2 ',dot_product(wn2,n1) , dot_product(wn2,n2), dot_product(wn2,n3)
   if (diat.and.unode == testnode) write(24,*) reg%arrivaltime(node1),dot_product(wn1,r1) , dot_product(wn2,r1)
   if (diat.and.unode == testnode) write(24,*) reg%arrivaltime(unode)
   if (diat.and.unode == testnode) write(24,*) 


! the solution is only acceptable if it lies on the correct side of all 3 planes

   vscount=0
   if (dot_product(wn1,n1) <= 0.0_dp .and. dot_product(wn1,n2) <= 0.0_dp .and. dot_product(wn1,n3) <= 0.0_dp) then

      atime= reg%arrivaltime(node1)-dot_product(wn1,r1)/(0.5_dp*(velocity(unode)+velocity(node1)))
      if (atime >= reg%arrivaltime(node1)) then
         if ( atime < reg%arrivaltime(unode)) then

            reg%arrivaltime(unode) = atime

            b=wn1/(sqrt(sum(wn1**2))*velocity(node1))
            a(1,1)=cosla(unode)*coslo(unode) ; a(2,1)=-sinla(unode)*coslo(unode) ; a(3,1)=-sinlo(unode)
            a(1,2)=cosla(unode)*sinlo(unode) ; a(2,2)=-sinla(unode)*sinlo(unode) ; a(3,2)= coslo(unode)
            a(1,3)=sinla(unode)              ; a(2,3)= cosla(unode)              ; a(3,3)= 0.0_dp
            reg%time_gradient(1:3,unode) = matmul(a,b)

            call refract_locally(reg%r(unode),reg%lat(unode),reg%long(unode),velgrid, &
                 reg%time_gradient(1:3,unode))

            if (diat.and.unode == testnode) write (24,'(a20,4f14.6)') 'arrival adjusted',atime,reg%time_gradient(1:3,unode)
         endif
         vscount=vscount+1
         if(diat.and.unode == testnode) write(24,'(a15,4i8,2f12.5)') '3 node update',newalive,node1,node2,node3, &
              reg%arrivaltime(unode),reg%arrivaltime(node1)

         if (diat.and.unode == testnode)  then
            write(24,*) 'updated from wn1'
            write(24,*) node1,node2,node3,reg%arrivaltime(unode),atime
            write(24,*) reg%node(unode)
            write(24,*) reg%node(node1)
            write(24,*) reg%node(node2)
            write(24,*) reg%node(node3)
            write(24,*)
         endif
      endif
 
   end if

   if (dot_product(wn2,n1) <= 0.0_dp .and. dot_product(wn2,n2) <= 0.0_dp .and. dot_product(wn2,n3) <= 0.0_dp) then

      atime= reg%arrivaltime(node1)-dot_product(wn2,r1)/(0.5_dp*(velocity(unode)+velocity(node1)))
      if (atime >= reg%arrivaltime(node1)) then
         if ( atime < reg%arrivaltime(unode)) then

            reg%arrivaltime(unode) = atime

            b=wn2/(sqrt(sum(wn2**2))*velocity(node1))
            a(1,1)=cosla(unode)*coslo(unode) ; a(2,1)=-sinla(unode)*coslo(unode) ; a(3,1)=-sinlo(unode)
            a(1,2)=cosla(unode)*sinlo(unode) ; a(2,2)=-sinla(unode)*sinlo(unode) ; a(3,2)= coslo(unode)
            a(1,3)=sinla(unode)              ; a(2,3)= cosla(unode)              ; a(3,3)= 0.0_dp
            reg%time_gradient(1:3,unode) = matmul(a,b)

            call refract_locally(reg%r(unode),reg%lat(unode),reg%long(unode),velgrid, &
                 reg%time_gradient(1:3,unode))

            if (diat.and.unode == testnode) write (24,'(a20,4f14.6)') 'arrival adjusted',atime,reg%time_gradient(1:3,unode)
         endif
         vscount=vscount+1
         if(diat.and.unode == testnode) write(24,'(a15,4i8,2f12.5)') '3 node update',newalive,node1,node2,node3,&
              reg%arrivaltime(unode),reg%arrivaltime(node1)

         if (diat.and.unode == testnode) then
            write(24,*) 'updated from wn2'
            write(24,*) node1,node2,node3,reg%arrivaltime(unode),atime
            write(24,*) reg%node(unode)
            write(24,*) reg%node(node1)
            write(24,*) reg%node(node2)
            write(24,*) reg%node(node3)
           write(24,*)
         endif
      endif

   end if

   if (vscount == 2 .and. u1 /= u2 ) then
 
      if(dot_product(wn1,wn2) < 0.99999_dp) then
      print '(a10,5f12.5,3i5)','unode :',reg%r(unode),reg%lat(unode),reg%long(unode),&
           velocity(unode),reg%arrivaltime(unode),reg%node(unode)
      print '(a10,5f12.5,3i5)','node1 :',reg%r(node1),reg%lat(node1),reg%long(node1),&
           velocity(node1),reg%arrivaltime(node1),reg%node(node1)
      print '(a10,5f12.5,3i5)','node2 :',reg%r(node2),reg%lat(node2),reg%long(node2),&
           velocity(node2),reg%arrivaltime(node2),reg%node(node2)
      print '(a10,5f12.5,3i5)','node3 :',reg%r(node3),reg%lat(node3),reg%long(node3),&
           velocity(node3),reg%arrivaltime(node3),reg%node(node3)

      print *,u1,u2,det

      print *,' r1',r1
      print *,' r2',r2
      print *,' r3',r3

      print *,' n1',n1
      print *,' n2',n2
      print *,' n3',n3

      print *,' wn1',wn1
      print *,' wn2',wn2
      print *,'dp1 ',dot_product(wn1,n1) , dot_product(wn1,n2), dot_product(wn1,n3)
      print *,'dp2 ',dot_product(wn2,n1) , dot_product(wn2,n2), dot_product(wn2,n3)

      stop 'time_from_3_nodes: two different valid solutions found'
      endif
   endif

   return

end subroutine time_from_3_nodes

!**********************************************************************************************************
subroutine regular_update2(unode,anode,n_anode)
  use mod_prop
  implicit none

  integer           :: unode,n_anode,anode(n_anode)
  integer           :: iu,ju,ku,ia,ja,ka,ias,jas,kas,di,dj,dk,secnode,ioff,joff,koff
  real(kind=dp)     :: a,b,c,t1,t2,d2,det,t1r,t2r,t1lat,t2lat,t1long,t2long,velr
  integer           :: n,i,j,k
  logical           :: r_info,lat_info,long_info
  real(kind=dp)     :: deg_to_rad,tmax

  deg_to_rad=acos(-1.0_dp)/180.0_dp

  if (n_anode > 3) stop 'too many nodes for regular update'

  if (diat.and.unode == testnode) then
     write(24,*) 'regular update with',n_anode,'connected nodes'
     write(24,'(a8,3i6,f12.6)') 'node :',reg%node(unode)%i1,reg%node(unode)%i2,reg%node(unode)%i3,reg%arrivaltime(unode)
     do n=1,n_anode
     write(24,'(a8,3i6,f12.6)') 'anode :',reg%node(anode(n))%i1,reg%node(anode(n))%i2,reg%node(anode(n))%i3,&
          reg%arrivaltime(anode(n))
     end do
  endif



! integer coordinates of the node to be updated

  iu=reg%node(unode)%i1 ; ju=reg%node(unode)%i2 ; ku=reg%node(unode)%i3 


! loop over the available nodes to accumulate the coefficients of the quadratic equation for the new time 

  a=0.0_dp ; b=0.0_dp ; c=0.0_dp

  t2r=0.0_dp ; t2lat=0.0_dp ; t2long=0.0_dp

  r_info=.false. ;lat_info=.false. ;long_info=.false.  

  tmax=0.0

  do n=1,n_anode

     tmax=max(tmax,reg%arrivaltime(anode(n)))

    ! integer coordinates of the alive node used in the update
 
     ia=reg%node(anode(n))%i1 ; ja=reg%node(anode(n))%i2 ; ka=reg%node(anode(n))%i3 


     if (ia == 0) stop 'interface node in regular update'

!!!
     if (ia /= iu) then       ! the alive node is offset in r

        r_info=.true.
        ioff=ia
        di = ia - iu
        ias= iu + 2*di

        velr = 0.5_dp*(velocity(unode)+velocity(anode(n)))

        ! test if the next node in the r direction is available for a second order update

        if (ias >= 1 .and. ias <= grid%nr) then         ! if it lies inside the grid 

           if (grid%node_region(ias,ja,ka) == reg%id) then        ! if it lies inside the region

              secnode=grid%rnode_id(ias,ja,ka)             ! get the # of the next node in the regional node list

                 ! if the next node is alive and causal

                 if (reg%node_status(secnode) == 0 .and. reg%arrivaltime(anode(n)) >= reg%arrivaltime(secnode)) then

                 ! second order unsafe at high wavefront curvature

                 if (dot_product(reg%time_gradient(1:3,anode(n)),reg%time_gradient(1:3,secnode)) >= & 
                      0.71_dp/(velocity(anode(n))*velocity(secnode)) ) then

                   ! .and. &
                   ! abs(velocity(anode(n))-velocity(secnode)) < 10.2_dp .and. &
                   ! abs(velocity(unode)-velocity(anode(n))) < 10.2_dp ) then

                 ! use second order stencil

                 t1r = reg%arrivaltime(anode(n))
                 t2r = reg%arrivaltime(secnode)
!                 d2 = 1.0_dp/grid%dr0**2
!                 d2 = 0.25_dp*((velocity(unode)+velocity(anode(n)))/grid%dr0)**2
                 d2 = (velr/grid%dr0)**2
                 a = a + 2.25_dp*d2
                 b = b + (-6.0_dp*t1r + 1.5_dp*t2r)*d2
                 c = c + (4.0_dp*t1r*t1r - 2.0_dp*t1r*t2r + 0.25_dp*t2r*t2r)*d2 

                 if (diat.and.unode == testnode) write(24,*) 'anode',n,'offset in r, second order'
                 if (diat.and.unode == testnode) write(24,*) a,b,c,t1,t2

                 cycle   ! done for this neighbouring alive point

                 endif

              endif   ! alive test

           endif    ! region test

        endif    ! grid test

       ! next node is not available, use first order stencil  

        t1r = reg%arrivaltime(anode(n))
!        d2 = 1.0_dp/grid%dr0**2
!        d2 = 0.25_dp*((velocity(unode)+velocity(anode(n)))/grid%dr0)**2
        d2 = (velr/grid%dr0)**2
        a = a + d2
        b = b + (-2.0_dp*t1r)*d2
        c = c + (t1r*t1r)*d2 

        if (diat.and.unode == testnode) write(24,*) 'anode',n,'offset in r, first order'
        if (diat.and.unode == testnode) write(24,*) a,b,c,t1
      
     else

        if (ja /= ju) then    ! the alive node is offset in lat

           lat_info=.true.
           joff=ja
           dj = ja - ju
           jas= ju + 2*dj


           ! test if the next node in the lat direction is available for a second order update

           if (jas >= 1 .and. jas <= grid%nlat) then             ! if it lies inside the grid 

              if (grid%node_region(ia,jas,ka) == reg%id) then            ! if it lies inside the region

                 secnode=grid%rnode_id(ia,jas,ka)             ! get the # of the next node in the regional node list

                 ! if the next node is alive and causal

                 if (reg%node_status(secnode) == 0 .and. reg%arrivaltime(anode(n)) >= reg%arrivaltime(secnode)) then

                 ! second order unsafe at high wavefront curvature

                    if (dot_product(reg%time_gradient(1:3,anode(n)),reg%time_gradient(1:3,secnode)) >= &
                      0.71_dp/(velocity(anode(n))*velocity(secnode))) then
!                         0.71_dp/velocity(anode(n))**2) then

                    ! use second order stencil

                    t1lat = reg%arrivaltime(anode(n))
                    t2lat = reg%arrivaltime(secnode)
!                    d2 = 1.0_dp/(grid%r(iu)*grid%dlat0)**2
                    d2 = 0.25_dp*((velocity(unode)+velocity(anode(n)))/(grid%r(iu)*grid%dlat0))**2
                    a = a + 2.25_dp*d2
                    b = b + (-6.0_dp*t1lat + 1.5_dp*t2lat)*d2
                    c = c + (4.0_dp*t1lat*t1lat - 2.0_dp*t1lat*t2lat + 0.25_dp*t2lat*t2lat)*d2 

                    if (diat.and.unode == testnode) write(24,*) 'anode',n,'offset in lat, second order'
                    if (diat.and.unode == testnode) write(24,*) a,b,c,t1,t2

                    cycle   ! done for this neighbouring alive point

                    endif

                 endif   ! alive test

              endif    ! region test

           endif    ! grid test

           ! next node is not available, use first order stencil  

           t1lat = reg%arrivaltime(anode(n))
!           d2 = 1.0_dp/(grid%r(iu)*grid%dlat0)**2
           d2 = 0.25_dp*((velocity(unode)+velocity(anode(n)))/(grid%r(iu)*grid%dlat0))**2
           a = a + d2
           b = b + (-2.0_dp*t1lat)*d2
           c = c + (t1lat*t1lat)*d2    
  
           if (diat.and.unode == testnode) write(24,*) 'anode',n,'offset in lat, first order'
           if (diat.and.unode == testnode) write(24,*) a,b,c,t1

        else                  ! the alive node is offset in long

           long_info=.true.
           koff=ka
           dk = ka - ku
           kas= ku + 2*dk


           ! test if the next node in the long direction is available for a second order update

           if (kas >= 1 .and. kas <= grid%nlong) then            ! if it lies inside the grid 

              if (grid%node_region(ia,ja,kas) == reg%id) then            ! if it lies inside the region

                 secnode=grid%rnode_id(ia,ja,kas)             ! get the # of the next node in the regional node list

                 ! if the next node is alive and causal

                 if (reg%node_status(secnode) == 0 .and. reg%arrivaltime(anode(n)) >= reg%arrivaltime(secnode)) then


                 ! second order unsafe at high wavefront curvature

                    if (dot_product(reg%time_gradient(1:3,anode(n)),reg%time_gradient(1:3,secnode)) >= &
                      0.71_dp/(velocity(anode(n))*velocity(secnode))) then
!                         0.71_dp/velocity(anode(n))**2) then

                    ! use second order stencil

                    t1long = reg%arrivaltime(anode(n))
                    t2long = reg%arrivaltime(secnode)
!                    d2 = 1.0_dp/(grid%r(iu)*grid%coslat(ju)*grid%dlong0)**2
                    d2 = 0.25_dp*((velocity(unode)+velocity(anode(n)))/(grid%coslat(ju)*grid%r(iu)*grid%dlong0))**2
                    a = a + 2.25_dp*d2
                    b = b + (-6.0_dp*t1long + 1.5_dp*t2long)*d2
                    c = c + (4.0_dp*t1long*t1long - 2.0_dp*t1long*t2long + 0.25_dp*t2long*t2long)*d2 

                    if (diat.and.unode == testnode) write(24,*) 'anode',n,'offset in long, second order'
                    if (diat.and.unode == testnode) write(24,*) a,b,c,t1,t2

                    cycle   ! done for this neighbouring alive point

                    endif

                 endif   ! alive test

              endif    ! region test

           endif    ! grid test

           ! next node is not available, use first order stencil  

           t1long = reg%arrivaltime(anode(n))
!           d2 = 1.0_dp/(grid%r(iu)*grid%coslat(ju)*grid%dlong0)**2
           d2 = 0.25_dp*((velocity(unode)+velocity(anode(n)))/(grid%coslat(ju)*grid%r(iu)*grid%dlong0))**2
           a = a + d2
           b = b + (-2.0_dp*t1long)*d2
           c = c + (t1long*t1long)*d2      

           if (diat.and.unode == testnode) write(24,*) 'anode',n,'offset in long, first order'
           if (diat.and.unode == testnode) write(24,*) a,b,c,t1

        endif

     endif

  end do  ! end loop over alive points in octant

! move the RHS of the Eikonal equation (s**2) to the LHS

  c = c - 1.0_dp      !/velocity(unode)**2 !- 1.0_dp

  if (diat.and.unode == testnode) write(24,*) 'final abc'
  if (diat.and.unode == testnode) write(24,*) a,b,c

  det= b*b - 4.0_dp*a*c

  if (det < 0.0_dp) then

!     if ( 0.5_dp*sqrt(abs(det))/abs(a) < 0.05_dp) then 
     if ( 0.5_dp*sqrt(abs(det))/abs(a) < 1.0e20_dp) then 

        det=0.0_dp
        n_det_negative = n_det_negative + 1

     else

        print *,grid%dr0,grid%r(iu)*grid%dlat0,grid%r(iu)*grid%coslat(ju)*grid%dlong0
        i=0 ; j=0 ; k=0
        if (r_info) i=i+1
        if (t2r /= 0.0_dp) i=i+1
        if (lat_info) j=j+1
        if (t2lat /= 0.0_dp) j=j+1
        if (long_info) k=k+1
        if (t2long /= 0.0_dp) k=k+1
        print '(i8,6i5)',unode,reg%node(unode),i,j,k
        do n=1,n_anode
           print '(i8,3i5,4f9.3)',anode(n),reg%node(anode(n)),reg%arrivaltime(anode(n)),reg%time_gradient(1:3,anode(n))
           print *,pgrid%r(reg%node(anode(n))%i1), &
                pgrid%lat(reg%node(anode(n))%i2)/deg_to_rad, &
                pgrid%long(reg%node(anode(n))%i3)/deg_to_rad
           print *
        enddo
        print *,a,b,c
        print *,'det time=',0.5_dp*sqrt(abs(det))/abs(a)
        print *,'time with det=0',0.5_dp*(-b)/a
        stop ' negative determinant too large'

     endif

  endif

  t1 = 0.5_dp*(-b + sqrt(det))/a

! hard enforcement of causality
 
  t1=max(t1,tmax)

  if (t1 < reg%arrivaltime(unode)) then  ! node will be updated

     reg%arrivaltime(unode) = t1  ! the arrival time

     reg%time_gradient(1:3,unode)=0.0_dp   ! and the time gradient that led to this update

     if (r_info) then
        if (t2r == 0.0_dp) then
           reg%time_gradient(1,unode)=(t1-t1r)/(grid%r(iu)-grid%r(ioff))
        else
           reg%time_gradient(1,unode)=0.5_dp*(3.0_dp*t1-4.0_dp*t1r+t2r)/(grid%r(iu)-grid%r(ioff))
        endif
     endif

     if (lat_info) then
        if (t2lat == 0.0_dp) then
           reg%time_gradient(2,unode)=(t1-t1lat)/(reg%r(unode)*(grid%lat(ju)-grid%lat(joff)))
        else
           reg%time_gradient(2,unode)=0.5_dp*(3.0_dp*t1-4.0_dp*t1lat+t2lat)/(reg%r(unode)*(grid%lat(ju)-grid%lat(joff)))
        endif
     endif

     if (long_info) then
        if (t2long == 0.0_dp) then
           reg%time_gradient(3,unode)=(t1-t1long)/(reg%r(unode)*cosla(unode)*(grid%long(ku)-grid%long(koff)))
        else
           reg%time_gradient(3,unode)=0.5_dp*(3.0_dp*t1-4.0_dp*t1long+t2long)/(reg%r(unode)*cosla(unode)*&
                (grid%long(ku)-grid%long(koff)))
        endif
     endif

     ! flag the time gradient as suspect if it does not use information in all three dimensions
     suspect_time_gradient(unode)= .not.(r_info .and. lat_info .and. long_info)

  endif

  if (diat.and.unode == testnode) write(24,*) 'newt, current estimate',reg%arrivaltime(unode),t1

  return

end subroutine regular_update2

!**********************************************************************************************************
! This subroutine creates a narrow band around the set of alive points transferred from the
! refined source grid to the main source region

 
subroutine create_narrow_band(regin,vtype)
  use mod_prop
  implicit none

  type(Tregion),target                       :: regin
  integer                                    :: vtype
  integer                                    :: n,m,nt ! local variables


!  print *,'entering create narrow band'

  reg =>regin
  grid => reg%grid
  velocity => reg%velocity(:,vtype)
  velgrid => vgrid(reg%ivgrid,vtype)


  allocate(node_is_counted(reg%nnode))
! precalculate required sines and cosines

  allocate(cosla(reg%nnode),sinla(reg%nnode),coslo(reg%nnode),sinlo(reg%nnode),suspect_time_gradient(reg%nnode))

  cosla=cos(reg%lat)
  sinla=sin(reg%lat)
  coslo=cos(reg%long)
  sinlo=sin(reg%long)

  do n=1,reg%nnode

     if (reg%node_status(n) == 0) then

        call find_connected_nodes(n)

! save the list of neighbours, because the array connode will be used again when finding neighbours of the
! neighbours that have to be updated 

        n_trial=n_connode
        trialnode(1:n_trial)=connode(1:n_trial)


!  loop over neighbours and take appropopriate action


        do m=1,n_trial

           nt=trialnode(m)
           if (reg%node_status(trialnode(m)) == -1) then

              reg%node_status(trialnode(m)) = 1
              
              reg%arrivaltime(trialnode(m))= huge_time
              
              call update_nb_time(trialnode(m))

           endif

        end do

     endif

  end do

  deallocate(cosla,sinla,coslo,sinlo,node_is_counted,suspect_time_gradient)

!  print *,'leaving create_narrowband'

  return

end subroutine create_narrow_band
!-------------------------------------------------------------------------
!
! This subroutine is used only by the subroutine create_narrow_band 
! It updates the time at a point, and is only different from the normal 
! routine update_time in not using the special properties of the most recent 
! alive point (newalive)

  subroutine update_nb_time(update_node)
    use mod_prop
    implicit none

    integer       :: update_node
    integer       :: n            ! local variables
    integer       :: il,jl,kl
    integer       :: anode(20),nanode
!
! first find the cells to which the node to be updated is connected, and the list of
! ALIVE nodes in these cells

    call find_connected_cells(update_node)

! now loop over the connected cells, and if possible use the alive nodes
! in each cell to find a new arrival time at the node to be updated
 
    do n=1,n_concell


       if (concell_nnode(n) > 0) then   ! if there are any alive nodes in this cell


! copy alive nodes in this connected cell to local array
       nanode=concell_nnode(n)
       if (nanode > 20) stop 'subroutine update_time: nanode >20'
       anode(1:nanode)=connode(concell_nbase(n):concell_nbase(n)+nanode-1)


             if (concell_is_regular(n)) then

                ! do a regular fast marching update, second order if possible, in this octant

                call regular_update2(update_node,anode(1:nanode),nanode)

             else

             ! do an irregular update in this octant

                select case (nanode)
          
                case(1)     ! there is 1 alive node in the cell

                   call time_from_1_node(update_node,anode(1))

                case(2)     ! there are 2 alive nodes in the cell

                   do jl=1,2
                      call time_from_1_node(update_node,anode(jl))
                   end do

                   call time_from_2_nodes(update_node,anode(1),anode(2))

                case(3)     ! there are 3 alive nodes in the cell

                   do jl=1,3
                      call time_from_1_node(update_node,anode(jl))
                   end do

                   do jl=1,2
                      do kl=jl+1,3
                         call time_from_2_nodes(update_node,anode(jl),anode(kl))
                      end do
                   end do

                   call time_from_3_nodes(update_node,anode(1),anode(2),anode(3))

                case(4:)     ! there are more than 3 alive nodes in the cell

                   do jl=1,nanode
                      call time_from_1_node(update_node,anode(jl))
                   end do 

                   do jl=2,nanode-1
                      do kl=jl+1,nanode
                      call time_from_2_nodes(update_node,anode(jl),anode(kl))
                      end do
                   end do

                   do il=2,nanode-2
                      do jl=il+1,nanode-1
                         do kl=jl+1,nanode
                         
                            call time_from_3_nodes(update_node,anode(il),anode(jl),anode(kl))
                         end do
                      end do
                   end do


                end select

             endif   ! regular or irregular

       endif   ! if there are any alive nodes in this cell


    end do ! loop over connected cells

    return

  end subroutine update_nb_time


!*********************************************************************************************************
!-------------------------------------------------------------------------
! below are the subroutines used to manipulate the binary tree
! NOTE!!!!!! the naming of the directions in the binary tree used for sorting 
! the narrow band has been reversed from the definition used by Sethian. This means:
! the root (smallest value) is at the BOTTOM, not the top
! UP is in the direction of increasing values, and increasing index in the tree array
! DOWN is in the direction of decreasing values, and decreasing index in the tree array
!
!-----------------------------------------------------------------
! adds a node to the binary tree at its correct position

  subroutine add_node_to_tree(nn)
    use mod_prop
    implicit none

    integer :: nn, parent_index, child_index, temp
!
! First, increase the size of the tree by one.
!
    ntree=ntree+1
!
! Put new value at top of tree
!
    reg%node_status(nn)=ntree
    node_from_tree_ind(ntree)=nn
!
! Now filter the new value down to its correct position
!
    child_index = ntree
    parent_index = ntree/2

    do while (parent_index > 0)

       if (reg%arrivaltime(nn) < reg%arrivaltime(node_from_tree_ind(parent_index))) then

          reg%node_status(nn)=parent_index
          reg%node_status(node_from_tree_ind(parent_index)) = child_index
          temp=node_from_tree_ind(child_index)
          node_from_tree_ind(child_index) = node_from_tree_ind(parent_index)
          node_from_tree_ind(parent_index) = temp
          child_index=parent_index
          parent_index=child_index/2

       else

          parent_index = 0

       endif

    end do
    return

  end subroutine add_node_to_tree

!-----------------------------------------------------------------
! removes the root from the tree and adjusts the tree structure above

  subroutine remove_root_from_tree
    use mod_prop
    implicit none

    integer  :: parent_index,child_index,temp


! if only one node left in the tree, nothing is left to be done

    if(ntree == 1) then ; ntree=0 ; return ; endif


! put the node with largest travel time at the root position

    reg%node_status(node_from_tree_ind(ntree))=1
    node_from_tree_ind(1)=node_from_tree_ind(ntree)


! Reduce size of tree by one

    ntree=ntree-1


! Now move the new root up to its correct position

    parent_index=1
    child_index=2*parent_index

    do while (child_index < ntree)

! Check which of the two children is smallest - use the smallest

       if(reg%arrivaltime(node_from_tree_ind(child_index)) > reg%arrivaltime(node_from_tree_ind(child_index+1))) &
            child_index=child_index+1


!  Check whether the child is smaller than the parent; if so, then swap,
!  if not, then we are done

       if(reg%arrivaltime(node_from_tree_ind(child_index)) < reg%arrivaltime(node_from_tree_ind(parent_index))) then

          reg%node_status(node_from_tree_ind(parent_index)) = child_index
          reg%node_status(node_from_tree_ind(child_index)) = parent_index
          temp = node_from_tree_ind(child_index)
          node_from_tree_ind(child_index) = node_from_tree_ind(parent_index)
          node_from_tree_ind(parent_index) = temp
          parent_index=child_index
          child_index=2*parent_index

       else

          child_index=ntree+1

       endif

    end do

    if (child_index == ntree) then

      if(reg%arrivaltime(node_from_tree_ind(child_index)) < reg%arrivaltime(node_from_tree_ind(parent_index))) then

          reg%node_status(node_from_tree_ind(parent_index)) = child_index
          reg%node_status(node_from_tree_ind(child_index)) = parent_index
          temp = node_from_tree_ind(child_index)
          node_from_tree_ind(child_index) = node_from_tree_ind(parent_index)
          node_from_tree_ind(parent_index) = temp

       endif
    endif

    return

  end subroutine remove_root_from_tree


!-----------------------------------------------------------------
! updates the tree structure after the value of the traveltime at a node has been
! updated (which should always be a decrease)

  subroutine update_tree(nn)
    use mod_prop
    implicit none

    integer  :: nn,temp,parent_index,child_index

!
! Filter the updated value down to its correct position
!
    child_index=reg%node_status(nn)
    parent_index=child_index/2

    do while (parent_index > 0)
       if (reg%arrivaltime(nn) < reg%arrivaltime(node_from_tree_ind(parent_index))) then

          reg%node_status(nn)=parent_index
          reg%node_status(node_from_tree_ind(parent_index)) = child_index
          temp=node_from_tree_ind(child_index)
          node_from_tree_ind(child_index) = node_from_tree_ind(parent_index)
          node_from_tree_ind(parent_index) = temp
          child_index=parent_index
          parent_index=child_index/2

       else

          parent_index = 0

       endif

    end do
    return

  end subroutine update_tree

!----------------------------------------------------------------------------------------
!  ***************************************************************
!  * Given an N x N matrix A, this routine replaces it by the LU *
!  * decomposition of a rowwise permutation of itself. A and N   *
!  * are input. INDX is an output vector which records the row   *
!  * permutation effected by the partial pivoting; D is output   *
!  * as -1 or 1, depending on whether the number of row inter-   *
!  * changes was even or odd, respectively. This routine is used *
!  * in combination with LUBKSB to solve linear equations or to  *
!  * invert a matrix. Return code is 1, if matrix is singular.   *
!  ***************************************************************
 Subroutine LUDCMP(A,INDX,status)
   use mod_3dfm
 implicit none
 real(kind=dp), PARAMETER              :: TINY=1.5D-16
 REAL(kind=dp)                         :: AMAX,DUM, SUM, A(3,3),VV(3)
 INTEGER                               :: status, INDX(3),i,j,k,imax

 status=0

 DO I=1,3
   AMAX=0._dp
   DO J=1,3
     IF (ABS(A(I,J)).GT.AMAX) AMAX=ABS(A(I,J))
   END DO ! j loop
   IF(AMAX.LT.TINY) THEN
     status = 1
     RETURN
   END IF
   VV(I) = 1.0_dp / AMAX
 END DO ! i loop

 DO J=1,3
   DO I=1,J-1
     SUM = A(I,J)
     DO K=1,I-1
       SUM = SUM - A(I,K)*A(K,J) 
     END DO ! k loop
     A(I,J) = SUM
   END DO ! i loop
   AMAX = 0.0_dp
   DO I=J,3
     SUM = A(I,J)
     DO K=1,J-1
       SUM = SUM - A(I,K)*A(K,J) 
     END DO ! k loop
     A(I,J) = SUM
     DUM = VV(I)*ABS(SUM)
     IF(DUM.GE.AMAX) THEN
       IMAX = I
       AMAX = DUM
     END IF
   END DO ! i loop  
   
   IF(J.NE.IMAX) THEN
     DO K=1,3
       DUM = A(IMAX,K)
       A(IMAX,K) = A(J,K)
       A(J,K) = DUM
     END DO ! k loop
     VV(IMAX) = VV(J)
   END IF

   INDX(J) = IMAX
   IF(DABS(A(J,J)) < TINY) A(J,J) = TINY

   IF(J.NE.3) THEN
     DUM = 1.d0 / A(J,J)
     DO I=J+1,3
       A(I,J) = A(I,J)*DUM
     END DO ! i loop
   END IF 
 END DO ! j loop

 RETURN
 END subroutine LUDCMP


!  ******************************************************************
!  * Solves the set of N linear equations A . X = B.  Here A is     *
!  * input, not as the matrix A but rather as its LU decomposition, *
!  * determined by the routine LUDCMP. INDX is input as the permuta-*
!  * tion vector returned by LUDCMP. B is input as the right-hand   *
!  * side vector B, and returns with the solution vector X. A, N and*
!  * INDX are not modified by this routine and can be used for suc- *
!  * cessive calls with different right-hand sides. This routine is *
!  * also efficient for plain matrix inversion.                     *
!  ******************************************************************
 Subroutine LUBKSB(A,INDX,B)
   use mod_3dfm
 real(kind=dp)         ::SUM, A(3,3),B(3)
 INTEGER              ::INDX(3),ii,ll,i,j

 II = 0

 DO I=1,3
   LL = INDX(I)
   SUM = B(LL)
   B(LL) = B(I)
   IF(II.NE.0) THEN
     DO J=II,I-1
       SUM = SUM - A(I,J)*B(J)
     END DO ! j loop
   ELSE IF(SUM.NE.0.0_dp) THEN
     II = I
   END IF
   B(I) = SUM
 END DO ! i loop

 DO I=3,1,-1
   SUM = B(I)
   IF(I < 3) THEN
     DO J=I+1,3
       SUM = SUM - A(I,J)*B(J)
     END DO ! j loop
   END IF
   B(I) = SUM / A(I,I)
 END DO ! i loop

 RETURN
 END subroutine LUBKSB

!*************************************************************************************************************

! this subroutine is used to find the gradient of the arrival time at a node
! if the gradient stored during the fast marching is suspect, which is always 
! when it was derived from a regular update from less than 3 nodes
! If all cells connected to the node are regular, just use finite differences on the grid.
! Otherwise, the local gradient is derived from a least squares fit of a constant gradient
! field to the values of the arrival times at the nodes connected to the one at which
! the gradient is evaluated

subroutine fit_gradient_at_node(reg,centernode,dtdr,dtdlat,dtdlong)
use mod_3dfm
implicit none

type(Tregion)                :: reg

integer,parameter            :: maxfitnodes = 50
integer                      :: n,m,i,j,k,i1,i2,i3,n_cnode,nminus,nplus
integer                      :: ii,jj,kk,nfit,icell
integer                      :: centernode,n_concell,mstore(maxfitnodes)
type(Tinteger_coordinates)   :: concell(8)
logical                      :: concell_irregular(8)
real(kind=dp)                :: r(maxfitnodes),lat(maxfitnodes),long(maxfitnodes),atime(maxfitnodes)
real(kind=dp)                :: dtdr,dtdlat,dtdlong,wmax,wmin,dx,dy,dz,dw

real(kind=dp),allocatable ::v(:,:),w(:),sol(:)
real(kind=dp),allocatable ::a(:,:),b(:)

type(Tintersection),pointer      :: isec
type(Tpropagation_grid),pointer :: grid


grid => reg%grid


! first make list of connecetd cells


! store the identifiers of the centernode node in local variables. Reminder:
! for regular grid nodes i1,i2,i3  correspond to ir,ilat,ilong of the node
! for intersection nodes i1,i2,i3 are 0, intersection #, node # in intersection 

   i1 = reg%node(centernode)%i1 ; i2 = reg%node(centernode)%i2 ; i3 = reg%node(centernode)%i3


! make a list of grid cells of which the node is part

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

   ! test whether any connected cell is cut by an interface, in that case the pointer to the local list of
   ! interfaces cutting the cell has been allocated

   do n=1,n_concell
      concell_irregular(n) = associated(grid%ccind_from_3dc(concell(n)%ir,concell(n)%ilat,concell(n)%ilong)%p)
   end do


   ! if all connected cells are regular, gradient is easy

   if (count(concell_irregular(1:n_concell)) == 0) then

      if (i1 == 0) stop 'fit_gradient_at_node : interface node in regular gradient evaluation'

      k = max(1,i1-1)
      if ( k<1 .or. k>grid%nr .or. i2<1 .or. i2>grid%nlat .or. i3<1 .or. i3>grid%nlong ) then

         print *,k,i1,i2,i3
         stop

      endif

      nminus = grid%rnode_id(max(1,i1-1),i2,i3)
      nplus  = grid%rnode_id(min(grid%nr,i1+1),i2,i3)
      dtdr = (reg%arrivaltime(nplus)-reg%arrivaltime(nminus))/(reg%r(nplus)-reg%r(nminus))

      nminus = grid%rnode_id(i1,max(1,i2-1),i3)
      nplus  = grid%rnode_id(i1,min(grid%nlat,i2+1),i3)
      dtdlat = (reg%arrivaltime(nplus)-reg%arrivaltime(nminus))/(reg%r(centernode)*(reg%lat(nplus)-reg%lat(nminus)))

      nminus = grid%rnode_id(i1,i2,max(1,i3-1))
      nplus  = grid%rnode_id(i1,i2,min(grid%nlong,i3+1))
      dtdlong = (reg%arrivaltime(nplus)-reg%arrivaltime(nminus))/&
           (reg%r(centernode)*reg%coslat(centernode)*(reg%long(nplus)-reg%long(nminus)))

      return

   endif



! some connected cells are irregular, use fitting mode for gradient

   n_cnode=0

   do n=1,n_concell

! find the intersection nodes of connected cell n

!   explanation: each intersection has a 1-D list of cells cut by the interface, and a list of intersection
!   nodes that are part of each cut cell. Each regular grid cell has a pointer ccind_from_3dc(i,j,k)%p
!   (Cut Cell INDex FROM 3D Coordinates)
!   associated with it, where p is a pointer to an integer array with as many elements as there are intersections.
!   If a cell is cut by interface n, the pointer ccind_from_3dc(i,j,k)%p is allocated, and the variable
!   ccind_from_3dc(i,j,k)%p(n) contains the array index of cell (i,j,k) in the 1D cut cell list of intersection n 

      if (concell_irregular(n)) then

! if so, check if the cell is cut by the top intersection

     ! icell is the index of the current connected cell in the list of cells cut by interface reg%itop.
                  
         icell = grid%ccind_from_3dc(concell(n)%ir,concell(n)%ilat,concell(n)%ilong)%p(reg%itop%iface_id)

     ! if icell == 0 the cell is not cut be the top interface
    
         if(icell /= 0) then
 
            isec => reg%itop
            do jj=1,isec%n_inodes(icell)

!   m is the node number in the regional node list of node  jj in the list of 
!   interface nodes that are part of cut cell icell

               m=isec%rbel_node_id(isec%inodes(jj,icell))
               if (m == centernode) cycle

               if ( n_cnode == 0 ) then  
                  n_cnode=1
                  mstore(n_cnode) = m
                  r(n_cnode)     = reg%r(m)
                  lat(n_cnode)   = reg%lat(m)
                  long(n_cnode)  = reg%long(m)
                  atime(n_cnode) = reg%arrivaltime(m)
               else
                  if ( count(mstore(1:n_cnode) == m) == 0 ) then
                     n_cnode=n_cnode+1
                     mstore(n_cnode) = m
                     r(n_cnode)     = reg%r(m)
                     lat(n_cnode)   = reg%lat(m)
                     long(n_cnode)  = reg%long(m)
                     atime(n_cnode) = reg%arrivaltime(m)
                  endif
               endif

            end do

         end if


! then check the bottom intersection

         icell = grid%ccind_from_3dc(concell(n)%ir,concell(n)%ilat,concell(n)%ilong)%p(reg%ibot%iface_id)
         if (icell /= 0) then

            isec => reg%ibot
            do jj=1,isec%n_inodes(icell)

               m=isec%rabo_node_id(isec%inodes(jj,icell))
               if (m == centernode) cycle

               if ( n_cnode == 0 ) then  
                  n_cnode=1
                  mstore(n_cnode) = m
                  r(n_cnode)     = reg%r(m)
                  lat(n_cnode)   = reg%lat(m)
                  long(n_cnode)  = reg%long(m)
                  atime(n_cnode) = reg%arrivaltime(m)
               else
                  if ( count(mstore(1:n_cnode) == m) == 0 ) then
                     n_cnode=n_cnode+1
                     mstore(n_cnode) = m
                     r(n_cnode)     = reg%r(m)
                     lat(n_cnode)   = reg%lat(m)
                     long(n_cnode)  = reg%long(m)
                     atime(n_cnode) = reg%arrivaltime(m)
                  endif
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

                 if (grid%node_region(ii,jj,kk) == reg%id) then  ! node has to belong to the current region

                     m=grid%rnode_id(ii,jj,kk)
                     if (m == centernode) cycle

                     if ( n_cnode == 0 ) then  
                        n_cnode=1
                        mstore(n_cnode) = m
                        r(n_cnode)     = reg%r(m)
                        lat(n_cnode)   = reg%lat(m)
                        long(n_cnode)  = reg%long(m)
                        atime(n_cnode) = reg%arrivaltime(m)
                     else
                        if ( count(mstore(1:n_cnode) == m) == 0 ) then
                           n_cnode=n_cnode+1
                           mstore(n_cnode) = m
                           r(n_cnode)     = reg%r(m)
                           lat(n_cnode)   = reg%lat(m)
                           long(n_cnode)  = reg%long(m)
                           atime(n_cnode) = reg%arrivaltime(m)
                        endif
                     endif


                  endif


            end do
         end do
      end do

   end do  ! loop over connected cells


! now do the actual least squares fit using singular value decomposition

      nfit=3

      allocate(a(n_cnode,nfit),b(n_cnode),v(nfit,nfit),w(nfit),sol(nfit))

      do i=1,n_cnode

         dx=r(i)-reg%r(centernode)
         dy=reg%r(centernode)*(lat(i)-reg%lat(centernode))
         dz=reg%r(centernode)*reg%coslat(centernode)*(long(i)-reg%long(centernode))
         dw=1.0_dp/(sqrt(dx*dx+dy*dy+dz*dz) + grid%tolerance)

         a(i,1)=dx*dw
         a(i,2)=dy*dw
         a(i,3)=dz*dw
         b(i)  =(atime(i)- reg%arrivaltime(centernode))*dw

      end do

      call svdcmp(a,n_cnode,nfit,n_cnode,nfit,w,v)

!	find maximum singular value

      wmax=maxval(abs(w(1:nfit)))

!	define "small"

      wmin=wmax*1.e-6_dp
 
!	zero the "small" singular values
      where (abs(w(1:nfit)) < wmin) w(1:nfit)=0.0d0

      call svbksb(a,w,v,n_cnode,nfit,n_cnode,nfit,b,sol)
   
      dtdr=sol(1)
      dtdlat=sol(2)
      dtdlong=sol(3)

      deallocate(a,b,v,w,sol)

      return


 end subroutine fit_gradient_at_node

!**********************************************************************************************************






