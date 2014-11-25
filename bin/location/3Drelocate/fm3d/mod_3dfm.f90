module type_definitions

  INTEGER, PARAMETER     :: sp = SELECTED_REAL_KIND(6,37)
  INTEGER, PARAMETER     :: dp = SELECTED_REAL_KIND(15,307)

! declarations of derived types
!------------------------------------------------------

type Tpointer_to_integer_array

   integer,dimension(:),pointer   :: p 
   
end type Tpointer_to_integer_array


type Tpropagation_grid    ! a regular grid used for wave front propagation with fast marching

    integer       :: nr,nlong,nlat     ! # of grid cells in each direction
    REAL(KIND=dp) :: dr0,dlat0,dlong0  ! grid step sizes
    REAL(KIND=dp) :: r0,lat0,long0     ! position of grid origin
    REAL(KIND=dp) :: rmax,latmax,longmax     ! position of grid corner opposite origin
    REAL(KIND=dp) :: tolerance         ! tolerance for deciding if a position coincides exactly with the grid or not
    logical       :: is_main_grid 
    logical       :: is_source_grid 

    integer       :: nnode             ! total # of nodes

    integer       :: index_r0,index_lat0,index_long0  ! indices of the origin of a refined source grid in the main grid



    REAL(KIND=dp), DIMENSION (:), pointer :: r 
    REAL(KIND=dp), DIMENSION (:), pointer :: lat 
    REAL(KIND=dp), DIMENSION (:), pointer :: long 
    REAL(KIND=dp), DIMENSION (:), pointer :: coslat 


    REAL(KIND=dp), DIMENSION (:,:,:,:), pointer     :: velocity 
    REAL(KIND=dp), DIMENSION (:,:,:), pointer     :: arrivaltime 
    REAL(KIND=dp), DIMENSION (:,:,:,:), pointer   :: time_gradient 

    integer, DIMENSION (:,:,:), pointer :: node_region           ! identifies region of which this node is part
    integer, DIMENSION (:,:,:), pointer :: rnode_id              ! index of this node in the regional node list of
                                                                          ! the region to which it belongs

    logical, dimension(:,:,:),  pointer :: fully_regular       ! is node surrounded by regular cells only


    ! Here it gets complicated: ccind_from_3dc stands for cut cell index from 3D coordinates.
    ! If a cell i,j,k of the grid is cut by an interface, and therefore has intersection nodes,
    ! the pointer ccind_from_3dc(i,j,k) is allocated, and points to an integer array ccind_from_3dc(i,j,k)%p
    ! of length n_intersections. If the cell is cut by intersection n, ccind_from_3dc(i,j,k)%p(n) contains
    ! the index of the cut cell i,j,k in the cut cell list of intersection n. For an intersection m that does not
    ! cut the cell  ccind_from_3dc(i,j,k)%p(m) is zero. In this way we can identify the intersection nodes that
    ! are part of a cell from its position in the grid.
    ! 

    type(Tpointer_to_integer_array), DIMENSION (:,:,:), pointer :: ccind_from_3dc 

end type Tpropagation_grid

!-----------------------------------------------------------------------------------

type Tvelocity_grid   ! regular grid defining the velocity as a function of position
                      ! Note: usually each velocity grid is defined over the entire propagation
                      ! grid, but only some nodes actually influence the corresponding region  

    integer       :: nr,nlong,nlat     ! # of grid cells in each direction
    REAL(KIND=dp) :: dr0,dlat0,dlong0  ! grid step sizes
    REAL(KIND=dp) :: r0,lat0,long0     ! position of grid origin


    integer       :: nnode             ! total # of nodes

    integer       :: start_index       ! for use in inversion if the grid is a velocity grid
    logical       :: to_be_inverted ! for use in inversion if the grid is a velocity grid


    REAL(KIND=dp), DIMENSION (:), pointer :: r 
    REAL(KIND=dp), DIMENSION (:), pointer :: lat 
    REAL(KIND=dp), DIMENSION (:), pointer :: long 


    REAL(KIND=dp), DIMENSION (:,:,:), pointer     :: velocity 
    logical,dimension(:,:,:),pointer              :: active   ! set to true for the nodes that actually
                                                              ! influence the region to which the grid belongs 


end type Tvelocity_grid


!-------------------------------

type Tpath   ! contains information about the sequence of interface interactions defining a path in
             ! multi-stage fast marching

    integer                            :: id                ! index of path in user-defined list
    integer                            :: n_tf              ! # of time fields on each path
    integer,dimension(:) ,pointer      :: sequence    ! the sequence of interfaces on each path
    integer,dimension(:) ,pointer      :: tf_sequence ! the sequence of time fields on each path
                                                               ! number refers to index in time_field array in source
    integer,dimension(:) ,pointer      :: vtype_sequence    ! the sequence of velocity types on each path

    logical                            :: valid       ! flag for valid path, required sequence may not exist
    logical                            :: used         ! flag set if path is actually used
    logical                            :: gridsave     ! flag set if a grid of arrival times is to be saved
                                                       ! for this path
    integer                            :: first_tf_to_save


    integer                            :: refstep          ! =0 if no late reflections, step indicating refl otherwise
    integer                            :: fitting_interface  ! interface at which fitting is performed

end type Tpath

!-------------------------------------------------------------------------------------------------------


type Tbackpointer    ! three integer numbers that uniquely identify each node
                     ! for regular grid nodes these are the grid coordinates in r,lat and long
                     ! for interface nodes i1=0, i2= id of interface, i3 = number of node in list of interface
                     ! nodes

    integer :: i1
    integer :: i2
    integer :: i3

end type Tbackpointer

!-------------------------------
type Tsource

    integer                  :: id                  ! id # of source
    REAL(KIND=dp)            :: r,lat,long,coslat   ! position
    integer                  :: ir,ilat,ilong       ! main grid cell containing source
    type(Tbackpointer)       :: cnode(100)          ! nodes connected to the source
    integer                  :: n_cnode             ! # of nodes connected to the source

    logical                  :: on_grid,on_interface,on_pinched_interface
    integer                  :: region_id,interface_id
    integer                  :: topreg_id,botreg_id,topint_id,botint_id

    logical                  :: is_local,is_teleseismic
    integer                  :: teleseismic_id
    character(len=8)         :: teleseismic_phase

    integer                  :: nfile

    integer                  :: n_tf_init        ! # of timefields in which source lies (1/2)
                                                    ! 1 if in a region, 2 if on an interface that is not top or bottom

    integer,dimension(:),pointer   :: first_tf_up   ! indices of source time fields
    integer,dimension(:),pointer   :: first_tf_down ! indices of source time fields


! these are the paths (sequence of reflections/refractions) originating from this source that have to be computed
    integer                              :: n_paths       ! # of paths for this source
    type(Tpath),dimension(:),pointer     :: path     ! the actual paths
    type(Tpath),dimension(2,2)           :: init_path

! these are the time fields containing regional traveltimes for all computed paths
    integer                                 :: n_time_fields      ! # of time fields for this source
    type(Ttime_field),dimension(:),pointer  :: time_field    ! list of time fields


! parameters having to do with the calculation of Frechet derivatives
    integer                  :: start_index         ! location of source parametrs in inversion parameter array
    logical                  :: to_be_inverted      ! flag showing whether the the source parameters are fixed or free

     
end type Tsource


!-------------------------------!

type Treceiver

    integer                                    :: id                    ! identifies the receiver
    REAL(KIND=dp)                              :: r,lat,long,coslat     ! position
    REAL(KIND=dp), dimension(:),pointer        :: arrivaltime           ! the arrival time at the receiver (not used)
    integer                                    :: n_rays                ! number of paths ending at this receiver
    type(Tray),dimension(:),pointer            :: ray                   ! rays of the paths ending at this receiver


  ! the variables below are only used if the path contains a reflection fit
  ! they identify the sequence of time fields from the receiver to the intermediate fitting interface

    integer                                    :: source_equivalent  ! index in source list of this receiver
    integer,dimension(:),pointer               :: path_equivalent       ! corresponding path in the source_equivalent
                                                                        ! path list

    
end type Treceiver

!-------------------------------!

type Tinterface
! this type defines the position of an interface
!  note: type Tinterface defines of the position of the interface (used as input by the cubic spline interpolation)
!        type Tintersection contains the actual nodes of the interface and everything related to them

! grid definitions
    integer :: nlat,nlong,id                        ! # of points in lat,long
    REAL(KIND=dp) :: dlat0,dlong0                   ! size of intervals
    REAL(KIND=dp) :: lat0,long0                     ! position of grid origin
    logical       :: pinched                        ! true if the interface touches another


! parametrs of the inversion
    integer       :: nnode                          ! # of parameters describing the interface position    
    integer       :: start_index                    ! start index of the interface parameters in the global parametr list
    logical       :: to_be_inverted        ! is the position of this interface is to be inverted for?


    REAL(KIND=dp), DIMENSION (:), pointer :: lat      
    REAL(KIND=dp), DIMENSION (:), pointer :: long 

    REAL(KIND=dp), DIMENSION (:,:), pointer :: r    ! the actual radius values for the interface at the nodes

end type Tinterface

!-------------------------------!

type Tinteger_coordinates

    integer :: ir,ilat,ilong

end type Tinteger_coordinates

!-------------------------------

type Tintersection
! this type contains information about where an interface intersects with a grid
!  note: type Tinterface defines of the position of the interface (used as input by the cubic spline interpolation)
!        type Tintersection contains the actual nodes of the interface and everything related to them

   integer :: nnode,n_ccells                         ! # of intersection nodes, # of grid cells cut by the interface
   integer :: id                                 ! intersection id
   integer :: iface_id                            ! corresponding interface id
   type(Tpropagation_grid),pointer :: grid  ! pointer to the grid on which the intersection is defined        

! flags
   logical :: pinched           ! true if the the intersection touches another

   REAL(KIND=dp), DIMENSION (:), pointer              :: r             ! positions of the intersection nodes
   REAL(KIND=dp), DIMENSION (:), pointer              :: lat 
   REAL(KIND=dp), DIMENSION (:), pointer              :: long 
   REAL(KIND=dp), DIMENSION (:), pointer              :: coslat 
   REAL(KIND=dp), DIMENSION (:,:), pointer            :: normal        ! vector giving the normal of the interface
                                                                                ! at every intersection node

   REAL(KIND=dp), DIMENSION (:,:), pointer              :: vel_top       ! velocities on both side of the interface
   REAL(KIND=dp), DIMENSION (:,:), pointer              :: vel_bot 

   REAL(KIND=dp), DIMENSION (:), pointer              :: arrivaltime   ! time a front arrives at an intersection node
   REAL(KIND=dp), DIMENSION (:), pointer              :: starttime     ! time a front starts from an intersection node
   REAL(KIND=dp), DIMENSION (:,:), pointer            :: time_gradient ! time gradient at an interface node
                                                 ! the time gradient is normal to the wave front and has size 1/velocity

   integer,dimension(:),pointer           :: intype      ! type of inode (0:on grid,1,2,3 on r,lat long connection) 


! the variables below allow us to identify the grid cells that an intersection node is part of
! and the inverse, the intersection nodes that are part of a given grid cell

   type(Tinteger_coordinates), DIMENSION (:), pointer :: ccells           ! list of cells cut by this interface
   integer,dimension(:), pointer                      :: n_inodes         ! number of inodes in cut cell
   integer,dimension(:,:),pointer                     :: inodes           ! actual inodes in cut cell
   integer, DIMENSION (:,:), pointer                  :: ccell_from_inode ! pointer from inode to cut cells it is in


! these variables connect the intersection with the regions (abov and below) that is is part of

   type(Tregion),pointer                                  :: regabo       ! the region above the intersection
   type(Tregion),pointer                                  :: regbel      ! the region above the intersection

   integer, DIMENSION (:), pointer          :: rabo_node_id   !index of each intersection node in the node 
                                                                       ! list of the region above the intersection

   integer, DIMENSION (:), pointer          :: rbel_node_id   !index of each intersection node in the node 
                                                                       ! list of the region below the intersection

   integer, DIMENSION (:,:), pointer          :: irg_abo   !first regular grid node above the interface at (j,k)
   integer, DIMENSION (:,:), pointer          :: irg_bel  !first regular grid node below the interface at (j,k)


end type Tintersection

!-------------------------------

type Tregion
! this type contains a region of the propagation grid between interfaces
! the region is a volume of space between two interfaces, and contains nodes of the regular grid 
! and both bounding intersections. Fast marching proceeds on a region by region basis

   integer      :: id                           ! region identification
   integer      :: ivgrid                       ! velocity definition grid associated with this region
   type(Tintersection),pointer :: itop,ibot        ! intersections at the top and bottom of this region
   type(Tpropagation_grid),pointer :: grid  ! pointer to the grid on which the region is defined   
   integer      :: ngnode                         ! number of propagation grid nodes in this region


! arrays below define a 1-D list of regular + intersection nodes constituting this region
! this 1-D array is used for the fast marching 

   integer                                     :: nnode   ! total number of nodes in this region including boundary nodes
   type(Tbackpointer),dimension(:),pointer     :: node    ! points back from 1-D list of nodes in the 
                                                                   ! region to gridnode or intnode it corresponds with
   integer, dimension(:), pointer              :: node_status  ! fast marching status
   real(kind=dp),dimension(:),pointer          :: arrivaltime  ! 
   real(kind=dp),dimension(:,:),pointer        :: time_gradient  !
   real(kind=dp),dimension(:,:),pointer        :: velocity     !
   real(kind=dp),dimension(:),pointer          :: r    !
   real(kind=dp),dimension(:),pointer          :: lat   !
   real(kind=dp),dimension(:),pointer          :: long     !
   REAL(KIND=dp),DIMENSION(:),pointer          :: coslat 

! nodes that have been initialized from a teleseismic source
   integer                                     :: n_init       ! number of initialized nodes
   integer,dimension(:),pointer                :: init_id      ! the list of initialized nodes
   integer,dimension(:),pointer                :: init_type      ! the list of initialized nodes
   real(kind=dp),dimension(:),pointer          :: init_arrivaltime  ! arrival time of the init nodes
   real(kind=dp),dimension(:,:),pointer        :: init_time_gradient ! incoming gradient at the init node

end type Tregion

!-------------------------------

type Ttime_field
! the type Ttimefield contains a fast marching solution for the arrival times
! and the time gradients in a region


   integer                                    :: id                             ! identifies the time field
   integer                                    :: vtype                 ! the type of velocity used for this tf
   integer                                    :: nfile          ! number of file in which data will be stored   
   type(Tregion), pointer                     :: reg                   ! the region this time field refers to
   type(Tintersection),pointer                :: istart                ! the intersection from which the FMM started 
   type(Tintersection),pointer                :: inonstart            ! the other intersection of the region
   real(kind=dp),dimension(:),pointer         :: arrivaltime  !  
   real(kind=dp),dimension(:,:),pointer       :: time_gradient  !  

   logical                                    :: turning_rays_present  ! indicates if turning rays hit the starting
                                                                                ! intersection during FMM
   logical,dimension(:),pointer               :: received_turning_ray  ! if turning rays were present, this array
                                                       ! indicates which nodes of the starting intersection received them


 ! these pointers allow the time fields to be organised in a tree structure that allows for easy re-use
 ! of timefields already calculated by other path sequences

   integer                                    :: prev_tf      ! pointer to the previous time field on the path/ray
   integer                                    :: next_tf(8)   ! pointer to the next time fields, possibilities:
                                                              ! (1) up from inonstart (vtype = 1)
                                                              ! (2) down from inonstart 1
                                                              ! (3) up from istart (only possible if turning rays exist) 1
                                                              ! (4) down from istart (only possible if turning rays exist) 1
                                                              ! (1) up from inonstart (vtype = 2)
                                                              ! (2) down from inonstart 2
                                                              ! (3) up from istart (only possible if turning rays exist) 2
                                                              ! (4) down from istart (only possible if turning rays exist) 2
end type Ttime_field

!-------------------------------

type Tray_section
! a ray section is the part of a ray between two interfaces, except the first one which starts at source, and the
! last one which ends at a receiver

   type(Tray),pointer                         :: ray      ! the ray that this section is part of
   type(Tregion), pointer                     :: reg      ! the region in which the ray section lies 
   type(Tintersection),pointer                :: istart   ! the intersection at which the integration to find
                                                          ! the ray starts. Note that the rays are found integrating
                                                          ! backward in time!
   type(Tintersection),pointer                :: iend     ! the intersection at which the ray integration ends
   type(Ttime_field),pointer                  :: tf       ! the time field used for finding the ray
   type(Tsource),pointer                      :: source   ! the source of the ray

   integer                                    :: npoints           ! # of points on this ray section
   real(kind=dp),dimension(:,:),pointer       :: point  ! ! the actual positions of the points on the ray section

   integer                                    :: place_in_sequence ! the position of the section in collection of ray 
                                                                   ! sections defining the ray
   logical                                    :: diffracted 
   logical                                    :: headwave 

end type Tray_section

!-------------------------------

type Tray
! a ray is a collection of ray sections that define a given ray/path/phase

   integer                                      :: nsections      ! # of sections on the ray
   type(Tray_section),dimension(:),pointer      :: section   ! the actual ray sections
   type(Tsource),pointer                        :: source    ! the source of this ray
   integer                                      :: raypath_id      ! number of ray in list of rays from this source
   integer                                      :: source_id 
   real(kind=dp)                                :: receiver_time      ! time the ray arrives at the receiver
   real(kind=dp),dimension(3)                   :: receiver_time_gradient ! time gradient (direction) of ray at the receiver
   logical                                      :: diffracted 
   logical                                      :: headwave 
   logical                                      :: is_multiray  ! true if reflection fit is required
   integer                                      :: n_subrays          ! # of reflections found in fit
   type(Tray),dimension(:),pointer              :: subray              ! contains the rays in case of a the reflection fit 

! variables relating to the inversion process
   integer                                      :: n_pdev             ! # of non-zero partial derivatives based on this ray
   integer,dimension(:),pointer                 :: pdev_indx          ! list of inversion parameters for which the 
                                                                      ! partial derivative of the arrival time is non-zero
   real(kind=dp),dimension(:),pointer           :: pdev               ! partial derivative of the arrival time with respect
                                                                      ! to the corresponding inversion parameter in the 
                                                                      ! array pdev_indx

   logical                                      :: valid     ! set to false if this ray does not exist             

end type Tray


type Ttriangulation
! this type contains a 2-dimensional triangulation of a point set

   integer                                         :: npoints    ! number of points
   real(kind=dp),dimension(:,:),pointer            :: points  ! the coordinates of the points

   integer                                         :: ntriangles  ! # of triangles
   integer,dimension(:,:),pointer                  :: points_from_triangle  ! the points that are part of a given
                                                                                     ! triangle
   integer,dimension(:,:),pointer                  :: triangle_neighbours   ! the triangles that neighbour a
                                                                                     ! triangle

   integer,dimension(:),pointer                    :: n_triangles_from_point  ! # of triangles connected to a
                                                                                       ! given point
   integer,dimension(:,:),pointer                  :: triangles_from_point    ! indices of triangles connected to a
                                                                                       ! given point

end type Ttriangulation


type Tgrid_identifier

   integer   :: igrid,vtype

end type Tgrid_identifier


!**************************************************************************************************************************

contains

! the subroutines below can be called to give variables inside instances of the derived types defined
! above default values when allocated. Initialization inside the derived type definition
! is a Fortran 95 feature, and we had to remove it to ensure fortran 90 compatibility.


  subroutine pgrid_defaults(grid)

    type(Tpropagation_grid)  :: grid

    grid%is_main_grid = .false.
    grid%is_source_grid  = .false.
    grid%nnode = 0      

    nullify(grid%r) 
    nullify(grid%lat) 
    nullify(grid%long) 
    nullify(grid%coslat) 
    nullify(grid%velocity) 
    nullify(grid%arrivaltime) 
    nullify(grid%time_gradient) 
    nullify(grid%node_region)   
    nullify(grid%rnode_id)      
    nullify(grid%fully_regular) 
    nullify(grid%ccind_from_3dc) 

  end subroutine pgrid_defaults


  subroutine vgrid_defaults(grid)

    type(Tvelocity_grid)  :: grid

    grid%to_be_inverted  = .false.
    grid%nnode = 0      
    nullify(grid%r) 
    nullify(grid%lat) 
    nullify(grid%long) 
    nullify(grid%velocity)
    nullify(grid%active)

  end subroutine vgrid_defaults


  subroutine source_defaults(source)
    type(Tsource)  :: source

    source%on_grid = .false.
    source%on_interface = .false.
    source%on_pinched_interface = .false.
    source%region_id = 0
    source%interface_id = 0
    source%topreg_id = 0
    source%botreg_id = 0
    source%topint_id = 0
    source%botint_id = 0
    source%is_local = .false.
    source%is_teleseismic = .false.
    source%teleseismic_id = 0
    source%teleseismic_phase = ' '
    source%n_tf_init = 1 
    nullify(source%first_tf_up)
    nullify(source%first_tf_down)
    source%n_paths = 0 
    source%to_be_inverted  = .false.
    nullify(source%path)  
    source%n_time_fields = 0   
    nullify(source%time_field)

  end subroutine source_defaults


  subroutine path_defaults(path)
    type(Tpath)  :: path

    path%id = 0               
    path%n_tf = 0             
    nullify(path%sequence)
    nullify(path%tf_sequence)
    nullify(path%vtype_sequence)
    path%valid = .true.  
    path%used  = .false. 
    path%gridsave  = .false.       
    path%refstep = 0    
    path%fitting_interface = 0

  end subroutine path_defaults


  subroutine receiver_defaults(rec)
    type(Treceiver) :: rec

    rec%id  = 0              
    nullify(rec%arrivaltime)   
    rec%n_rays  = 0      
    nullify(rec%ray)                 
    rec%source_equivalent = 0
    nullify(rec%path_equivalent)
                                
  end subroutine receiver_defaults


  subroutine intersection_defaults(isec)
    type(Tintersection)  :: isec
    isec%nnode = 0
    isec%n_ccells = 0 
    isec%id = 0       
    isec%iface_id = 0 
    nullify(isec%grid)
    isec%pinched = .false.  
    nullify(isec%r)
    nullify(isec%lat)
    nullify(isec%long)
    nullify(isec%coslat)
    nullify(isec%normal)
    nullify(isec%vel_top)
    nullify(isec%vel_bot)
    nullify(isec%arrivaltime)
    nullify(isec%starttime)
    nullify(isec%time_gradient)
    nullify(isec%intype)
    nullify(isec%ccells)
    nullify(isec%n_inodes)
    nullify(isec%inodes)
    nullify(isec%ccell_from_inode)
    nullify(isec%regabo)
    nullify(isec%regbel)
    nullify(isec%rabo_node_id)
    nullify(isec%rbel_node_id)
    nullify(isec%irg_abo)
    nullify(isec%irg_bel)

  end subroutine intersection_defaults


  subroutine interface_defaults(iface)
    type(Tinterface)  :: iface

    iface%pinched = .false.       
    iface%nnode = 0                 
    iface%to_be_inverted = .false. 
    nullify(iface%lat)
    nullify(iface%long)
    nullify(iface%r)

  end subroutine interface_defaults


  subroutine region_defaults(reg)
    type(Tregion)  :: reg

    reg%id = 0          
    reg%ivgrid = 0      
    nullify(reg%grid)
    reg%ngnode   = 0 
    reg%nnode  = 0 
    reg%n_init = 0
    nullify(reg%node)
    nullify(reg%node_status)
    nullify(reg%arrivaltime)
    nullify(reg%time_gradient)
    nullify(reg%velocity)
    nullify(reg%r) 
    nullify(reg%lat)
    nullify(reg%long)
    nullify(reg%coslat)
    nullify(reg%init_id)
    nullify(reg%init_arrivaltime)
    nullify(reg%init_time_gradient)

  end subroutine region_defaults


  subroutine time_field_defaults(tf)
    type(Ttime_field)  :: tf

    tf%id = 0  
    tf%vtype = 0
    nullify(tf%reg)
    nullify(tf%istart)
    nullify(tf%inonstart)
    nullify(tf%arrivaltime)
    nullify(tf%time_gradient)
    tf%turning_rays_present = .false.
    nullify(tf%received_turning_ray)
    tf%prev_tf = 0                   
    tf%next_tf(1:8) = 0              

  end subroutine time_field_defaults


  subroutine ray_defaults(ray)
    type(Tray)  :: ray

    ray%nsections = 0  
    nullify(ray%section)
    nullify(ray%source)
    ray%raypath_id = 0 
    ray%source_id = 0
    ray%diffracted = .false.
    ray%headwave = .false.
    ray%is_multiray = .false.
    ray%n_subrays = 0        
    nullify(ray%subray)
    ray%n_pdev = 0           
    nullify(ray%pdev_indx)
    nullify(ray%pdev)
    ray%valid = .true.    

  end subroutine ray_defaults


  subroutine ray_section_defaults(raysec)
    type(Tray_section) :: raysec

    nullify(raysec%ray)
    nullify(raysec%reg)
    nullify(raysec%istart)
    nullify(raysec%iend)
    nullify(raysec%tf)
    nullify(raysec%source)
    raysec%npoints = 0       
    nullify(raysec%point)
    raysec%place_in_sequence = 0
    raysec%diffracted = .false.
    raysec%headwave = .false.

  end subroutine ray_section_defaults


  subroutine triangulation_defaults(tri)
    type(Ttriangulation) :: tri

    tri%npoints = 0  
    nullify(tri%points)
    tri%ntriangles = 0
    nullify(tri%points_from_triangle)
    nullify(tri%triangle_neighbours)
    nullify(tri%n_triangles_from_point)
    nullify(tri%triangles_from_point)

  end subroutine triangulation_defaults

end module type_definitions

!*************************************************************************************************************************


module global_variables

  use type_definitions

! global parameters

  REAL(KIND=dp), PARAMETER   :: interface_tolerance = 0.005_dp !intersection nodes snap to regular grid if closer than this
                                                    ! fraction of radial grid cell size (to avoid tiny triangles)

  REAL(KIND=dp), PARAMETER   :: huge_time = 1.0e20_dp  ! default value for a time that is larger than any realistic value

  REAL(KIND=dp), PARAMETER   :: earth_radius = 6371.0_dp

  integer   :: refinement_factor          ! reduction in size for refined source grid
  integer   :: ncell_to_be_refined        ! extent of refined grid around the source in main grid cells 
                                           ! refinement parameters are overwritten by values in propgrid.in 
  integer   :: global_source_counter
  integer   :: raypoint_counter

  logical   :: file_mode
  logical   :: no_pp_mode
  logical   :: parallel_mode
  logical   :: display_mode
  logical   :: save_rays_mode
  logical   :: save_timefields_mode

! below the definition of variables that will be accessible to every subprogram

!.........................................................................
! this array of type Tinterface defines  the location of the interfaces on the propagation grids
! by B-spline interpolation 
  integer                                                  :: n_interfaces
  type(Tinterface),dimension(:),pointer                    :: intrface 


!.........................................................................
! these are the velocity grids used to define the velocity on the propagation grids
! by B-spline interpolation
  integer                                                  :: n_vgrids
  integer                                                  :: n_vtypes
  type(Tvelocity_grid),dimension(:,:),pointer          :: vgrid 



!.........................................................................
! the main propagation grid
  type(Tpropagation_grid),pointer                         :: pgrid 

! these are the intersections associated with the main propgation grid
  integer                                                  :: n_intersections
  type(Tintersection),dimension(:),pointer                 :: intersection 

! these are the regions of the main propagation grid
  integer                                                  :: n_regions
  type(Tregion),dimension(:),pointer                       :: region 


!.........................................................................
! the fine grid around the source
  type(Tpropagation_grid),pointer                         :: sgrid 

! these are the intersections associated with the refined source grid
  integer                                                  :: n_sintersections
  type(Tintersection),dimension(:),pointer                 :: sintersection 

! these are the regions of the refined source grid
  integer                                                  :: n_sregions
  type(Tregion),dimension(:),pointer                       :: sregion 

!.........................................................................
! the receivers
  integer                                                  :: n_receivers
  type(Treceiver),dimension(:),pointer                     :: receiver 

! the sources
  integer                                                  :: n_sources   ! the number of sources defined in the input
  integer                                                  :: n_sources_ppinc ! n_sources + # of virtual sources at receiver
                                                                            ! positions required for reflection matching
  type(Tsource),dimension(:),pointer                       :: source

!.........................................................................
! parameters of the inversion 
  integer                                                  :: n_inv_parms        ! total # of inversion parameters
  integer                                                  :: n_inv_active       ! total # of active inversion parameters
  integer                                                  :: n_inv_vgrid        ! # of velocity grids to be solved for
  type(Tgrid_identifier) ,dimension(:),pointer             :: vgrids_to_be_inv   ! list of velocity grids to be solved for
  integer                                                  :: n_inv_iface        ! # of interfaces to be solved for
  integer,dimension(:),pointer                             :: ifaces_to_be_inv   ! list of interfaces to be solved for

  logical                                                  :: locate_source      ! solve for source position and time or not
  integer                                                  :: n_inv_source       ! # of sources to be solved for
  integer,dimension(:),pointer                             :: sources_to_be_inv  ! list of sources to be solved for

end module global_variables

!*************************************************************************************************************


module interface_definitions
! explicit interfaces for subroutines that have pointer/target arguments

interface
   subroutine propagate(regin,vtype)
     use type_definitions
     type(Tregion),target    :: regin
     integer                 :: vtype
   end subroutine
end interface

interface
   subroutine trace_ray_from_receiver(rec,s,ray)
     use type_definitions
     type(Tsource),target          :: s         
     type(Treceiver)               :: rec       
     type(Tray),target             :: ray       
   end subroutine
end interface

interface
   subroutine find_intersection(isec,iface,grid)
     use type_definitions
     type(Tintersection)               :: isec
     type(Tinterface)                  :: iface
     type(Tpropagation_grid),target    :: grid
   end subroutine
end interface

interface
   subroutine define_region(reg,itop,ibot,grid)
     use type_definitions
     type(Tregion),target     :: reg
     type(Tintersection)      :: itop,ibot
     type(Tpropagation_grid),target :: grid
   end subroutine
end interface

interface
   subroutine sweep_region_from_interface(reg,istart_in,vtype,s)
     use type_definitions
     type(Tregion)                          :: reg
     type(Tintersection),target             :: istart_in
     integer                                :: vtype
     type(Tsource)                          :: s
   end subroutine
end interface

interface
   subroutine sweep_sregion_from_interface(reg,istart_in,vtype)
     use type_definitions
     type(Tregion)                          :: reg
     type(Tintersection),target             :: istart_in
     integer                                :: vtype
   end subroutine
end interface

interface
   subroutine initialize_refined_source(s,sc,grid,reg,itop,ibot)
     use type_definitions
     type(Tsource) :: s
     type(Tsource) :: sc
     type(Tpropagation_grid) :: grid
     type(Tregion)         :: reg
     type(Tintersection),target   :: itop,ibot
   end subroutine
end interface


interface
   subroutine initialize_refined_source2(s,sc,grid,reg,itop,ibot)
     use type_definitions
     type(Tsource) :: s
     type(Tsource) :: sc
     type(Tpropagation_grid) :: grid
     type(Tregion)         :: reg
     type(Tintersection),target   :: itop,ibot
   end subroutine
end interface

end module interface_definitions

!**********************************************************************************************************

module mod_3dfm

  use type_definitions
  use global_variables
  use interface_definitions

end module mod_3dfm

module mod_3dfm_nointerfaces

  use type_definitions
  use global_variables

end module mod_3dfm_nointerfaces
