* INTRODUCTION
**************

This is a brief description of how to run the multi-stage 3D fast marching
code. A description of the methods used is given in the accompanying paper,
currently submitted to GJI. We advise users to have a look through this paper
first to gain an understanding of the functionality of the code, i.e. what it
can do and what it can't, and to learn the meaning of some of the terms used
below.

The code distributed here contains several additions/improvements over what is
described in the paper: 

1 - the refined source grid is no longer restricted to the region in which the
source lies. For problems in which the source is located in a very thin layer
(where "thin" means poorly resolved in the main fast marching propagation
grid), this restriction caused the extent of the refined source grid to be
very small in some directions. In these directions, the curvature of the wave
front is not small compared to the grid spacing on the main propagation grid
when it hits the boundary of the refined source grid, leading to decreased
accuracy. The current implementation solves most of these problems, but some
decrease in accuracy still occurs if the wave front is reflected (or converted
to another phase) at an interface overlapping with the refined source grid.

2 - the code can also return the Frechet derivatives, that is the partial
derivatives of arrival times with respect to the parameters of the model.

3 - either 1 or 2 types of velocity (P / S) can be defined for each region,
and conversion of phase at interfaces can be modelled

4 - It is now possible to specify a source that lies outside the propagation
grid: instead of starting the wave front at the source, the boundary nodes
of the grid are initialized with the arrival time of a wavefront from a
distant source as calculated in the ak135 reference model.

NOTES regarding teleseismic sources: 

There are still some limitations on the use of sources outside the grid. 

If the source is sufficiently close to the modelling region, and we are
interested in say direct P or S waves, the wave front may hit only the sides
of the grid, and while still moving downward. This can lead to inconsistencies
with the basic mode of operation of the multi-stage fast marching method, as
the proper order in which to evaluate the wave front propagation through the
regions is not clear. The program is currently set up in such a way that it
assumes that the bottom region is hit first by the teleseismic wave front, and
that the sequence of propagation from teleseismic source to a receiver on the
surface when no reflections occur follows the order of regions from bottom to
top. This means that teleseismic wave fronts that hit the side of the grid
with a downward motion can generally only be modelled for the case of A SINGLE
REGION, since in that case no inconsistencies can arise.

Another important limitation is that teleseimic sources can not be used for
grids extending deeper than 800 km. This because the initialization method as
currently implemented requires the travel times from the grid boundary node to
the surface in the ak135 model, and the ttimes routines used for these travel
times do not allow sources deeper than this limit.

Also note that if the velocity model in the region considered does not match
smoothly with the ak135 model used to represent the space outside, some
unusual effects like refractions, total reflections etc. can occur on the
sides of the grid.


5 - An option has been added to save (regular) grids of wave front arrival times 
for specific source/path combinations. This is a useful option if the code 
is to be used for source location in complex 3D media. By treating each receiver
in the network as a source and propagating the wavefront outward from them, 
grids of travel
times from each node to each receiver are obtained. The location algorithm can 
interpolate in this 3D grid to obtain travel times to the receiver from an 
arbitrary location in the modelled region. This mode is still experimental.


******************************************************************************
This version is a first (beta) release. The code was written and tested by a
single person only, so it is likely to contain bugs.  You can send bug reports
to marthijn?dekool%anu?edu?au (replace % by @ and ?  by .) . At least
initially, we will only look at problems that arise after compilation with the
Intel Fortran compiler and on a Linux platform. We guarantee no help or
support of any kind, but will let you know if we find a solution to your
problem.

We would appreciate feedback. Let us know if you can run the code succesfully
with other compilers/platforms, if you think of some changes/additions that
would be useful to you, or just if you find a good use for it. 

If you are able to use the code for your work, please include a reference to
the paper mentioned above.
******************************************************************************




* COMPILATION / RUNNING
***********************

This code was written in Fortran 90. We have tried to ensure that no use is
made of features that are part of later versions such as Fortran 95 so that
older compilers can also be used.

To compile, edit the makefile to refer to the compiler you want to use and
type 'make fm3d' to create the executable fm3d. 

We have managed to compile the code and run it successfully with the Intel
Fortran compiler version 8.1 or higher on 32 bit Intel and 64 bit AMD machines
running SuSE Linux, an AMD based Suse system using the Pathscale compiler, and
on an AMD based Sun Ultra 20 using the SunWSpro f90 compiler. Intel Fortran
version 8.0 runs into internal compiler errors during compilation. g95 still
appears to be in flux, we experienced problems ranging from segmentation
faults during compilation to run time segmentation faults depending on the
version. The code is ca. 1.5 times slower on the Sun Ultra 20 than it is with
Intel Fortran under Suse on a 2.8 GHz Pentium 4. The main code runs at similar
speed to the Intel system on the AMD/Pathscale/Suse system, but on this
platform we experienced difficulties with the auxiliary programs generating
travel time tables in the ak135 model based on the older ttimes program.
We have succesfully compiled and run the code on SPARC based SUNs, but the 
execution times on this platform appear to be prohibitively slow (i.e. a factor 
of ~30 slower than on INTEL/AMD systems) 

Once you have the executable, ensure that you have all the required input
files described below, then run fm3d. The program is fairly verbose, writing
all the steps it is going through to standard output (the screen). Redirect
the output to a log file if you are not interested in seeing the progress.




* INPUT FILES
*************

The code requires several input files. Examples are provided with the
distribution, except for ak135.hed and ak135.tbl which are only required if
you want to use teleseismic sources. These are binary formatted and platform
dependent so you have generate them yourself as described below. 

- mode_set.in
-------------
edited by hand. The program has several modes in which it can run, which
govern things like memory usage and what kind of output is produced. The
modes are explained in the example mode_set.in file.  


- propgrid.in
-------------

edited by hand. Contains the properties of the main FMM propagation grid like
grid step size, nr of grid points in each direction, origin coordinates, and
the size of the refined source grid


- sources.in   
------------

edited by hand. Contains information about the sources,like position and info
about the paths through the sequence of interfaces that have to be calculated
by the fast marching. The structure of the input is as follows ( The meaning
of the input lines is also described as comments in the file 'sources.in' in
the distribution):

line 1 : number of sources

Then for each source the following lines:

- flag indicating whether the source is local (inside the grid, flag=0) or
  telesesimic (outside the grid,flag=1) 

- if the source is teleseismic, this line contains the teleseismic phase to be
  used, e.g. P, S, PcP etc. For local sources this line does not occur

- the position of the source in depth(km) ,lat, long (degrees) 

- the number of paths ( interface interaction sequences) from this source that
  have to be computed

  Then the specifications of each path that has to be computed for this source
  follow, where each path is defined in 3 lines:

  - the number of steps (interface interactions) defining the path. This
    should be equal to exactly half the number of integers on the next line,
    i.e. a step indicating a reflection fit rather than a propagation step
    (Example 3 below) must also be counted.

  - the actual sequence of interface interactions (2 integers per step), see
    explanation and examples below

  - the velocity type (1/2) to be used during each fast marching step between
    two interfaces. Each pair of entries on the previous line must have a
    corresponding velocity type. If the step indicates a reflection fit, the
    value of the corresponding entry is ignored but must still be given.


A path or sequence of interface interactions is specified as follows: Each FM
step between two interfaces is specified by two numbers. For the first step
starting at the source the first number is 0, the second either the number of
the top or bottom interface of the source region, whichever is the start of
the next step.  For subsequent steps the first number is the starting
interface and the second number is the other interface of the
region. Interfaces are numbered starting at 1 for the surface and increase
downward.

Example 1 : 0 2  2 3  3 2  2 1 represents the path for a source located in the
first region below the surface, refracted through interface 2, reflected from
interface 3, and refracted through interface 2 towards the surface.  

Example 2 : 0 2  2 3  2 1 represents the path for a source located in the first
region below the surface, refracted through interface 2, turning in the region
between interface 2 and interface 3, and refracted through interface 2 towards
the surface.

If two equal numbers are given the code assumes that a reflection match of
timefields originating from source and receiver is to be performed at the
interface whose number is repeated.  

Example 3 : 0 1  1 1  1 2 is used to represent PP type phases in a medium
consisting of a single region between two interfaces.


- receivers.in 
--------------
 
edited by hand. Contains the positions of the receivers and a
reference to which path from which source is to be calculated for this
receiver.


- interfaces.in 
---------------

This file contains information that is used to define the position of the
interfaces. This is done by cubic B-spline interpolation on a grid in latitude
and longitude, and the file contains information about the grid and values of
the RADIUS of the interface at each grid node. Note that the position of the
interface grid nodes is the same for every interface! The format is as follows:

line 1  : number of interfaces  (n_interfaces)

line 2  : number of interface grid nodes in latitude, number of interface grid
          nodes in latitude 
line 3  : grid node spacing in lat and in long (in radians)
line 4  : origin of interface grid in lat and long (in radians) 

next    : values of the radius at each interface grid node for grid 1, in km,
          one per line with longitude index varying the fastest

next    : values of the radius at each interface grid node for grid 2, in km,
          one per line with longitude index varying the fastest
	   
  .........and this is continued for all interfaces. 

Three example programs are provided that create the file 'interfaces.in',
that can be used to recreate the three examples described below. You can edit these
programs or create your own 'interfaces.in' file. The example programs read
the file 'propgrid.in' to create interface grids matching the propagation
grid. The interface grid generation programs consist of the a main executable in
the file 'make_interfaces.f90' and 3 sets of interface grid definitions (idefs_simple.f90,
idefs_ak135.f90 and idefs_cmplx.f90). To generate grids for a specific example 
compile the main program together with the set of definitions you want, e.g.
'f90 -o mif make_interfaces.f90 idefs_ak135.f90' and then run 'mif'. The example 
interface definition files can be easily edited, they simply contain a set of functions
that return the depth (in km) of each interface for a given position lat(deg), long(deg) 
(function iface1 has to return the surface, iface2 the next interface down etc.)

If you generate your own file, you must be aware that cubic spline
interpolation requires that all the nodes of the FM propagation grid must be
INSIDE the SECOND layer of interface grid nodes as counted inward from the
interface grid boundaries. In other words, if lat and long are the
coordinates of a propgation grid node and ilat and ilong the coordinates
of the interface grid it is required that ilat(2) < lat < ilat(nlat-1) and
ilong(2) < long < ilong(nlong-1) for all nodes in the propagation grid (nlat
and nlong are the dimensions of the interface grid).

Note that the program requires each region of the computational domain to be
bounded at the top and the bottom by interfaces. Even for the simplest problem of
one region without internal discontinuities, 2 interfaces representing the surface and
the bottom of the grid  have to be defined


- vgrids.in 
-----------

This file contains information that is used to define the velocities
throughout the computational domain. This is done by cubic B-spline
interpolation on a grid in radius, latitude and longitude, and the file
contains information about the grid and values of the velocity of the
medium at each grid node. Note that every region of the computational
domain (that is, the part between two interfaces) has its own velocity
grid. Therefore by definition there are always n_interfaces-1 velocity
grids. The format is as follows:

line 1  : number of velocity grids , number of velocity types (1 or 2)

next    : number of velocity grid nodes in radius,latitude, and longitude for grid 1
next    : grid node spacing in radius (km) and in lat and in long (in radians) for grid 1
next    : origin of velocity grid in radius (km) and in lat and long (in radians) for grid 1
next    : values of the velocity at each velocity grid node for grid 1, in km/sec,
          one per line with longitude index varying the fastest, latitude
	  index the second fastest and radius index  the slowest

next    :: number of velocity grid nodes in radius, latitude, and longitude for grid 2

  .........and this is continued for all velocity grids. 

If 2 velocity types have been specified in the first line, a second set of
velocity grid definitions in exactly the same format as above but defining the
second type of velocity follows.

Example programs are provided that create the file 'vgrids.in' that can be used
to recreate the three examples described below. You can edit these programs or
create your own 'vgrids.in' file. The example programs read the file
'propgrid.in' to create velocity grids matching the propagation grid and the
file 'interfaces.in' to ensure exactly n_interfaces-1 velocity grids will be
defined. The velocity grid generation programs consist of the a main executable in
the file 'make_vgrids.f90' and 3 sets of velocity grid definitions (vdefs_simple.f90,
vdefs_ak135.f90 and vdefs_cmplx.f90). To generate grids for a specific example 
compile the main program together with the set of definitions you want, e.g.
'f90 -o mvg make_vgrids.f90 vdefs_ak135.f90' and then run 'mvg'. The example 
velocity definition files can be easily edited, they simply contain a set of functions
that return the velocity for a given depth (km), lat(deg), long(deg) and velocity type
(1/2 generally corresponding to P/S) for each region (function vel1 has to return
velocities for region 1 etc.)

Note that in these examples we have chosen to generate velocity grids
for each region that cover the entire propgation grid, even though the region 
itself may only extend over a small fraction of the propagation grid. 
This was done because in the case that the program is used for inversion, interfaces 
may move about changing the extent of each region. To avoid problems when 
this happens, the velocity inside each region must be defined everywhere.

If you generate your own file, you must be aware that cubic spline
interpolation requires that all the nodes of the FM propagation grid must be
INSIDE the SECOND layer of velocity grid nodes as counted from the grid
boundaries, i.e. if r, lat and long are the coordinates of a propagation grid
node and vr, vlat and vlong the coordinates of the velocity grid it is
required that vr(2) < r < vr(nr-1) , vlat(2) < lat < vlat(nlat-1) and vlong(2)
< long < vlong(nlong-1) for all nodes in the propagation grid (nr, nlat and
nlong are the dimensions of the velocity grid).
Also be aware that our radial coordinate is radius, not depth. Thus the origin
of all grids in radius is at the deepest point of the grid, not at the surface. 


- frechet.in
------------

Edited by hand. This file contains information on whether to compute the
Frechet derivatives (i.e the partial derivatives of the arrival times with
respect to the model parameters such as nodal values on the velocity and
interface grids and source positions), and if so, with respect to what
parameters. You may not always want to invert for all parameters of the
problem, and you can choose for example to calculate derivatives only for the
velocity in a certain region, or for interface positions only.

line 1  : 1 if Frechet derivatives are required, 0 otherwise
line 2  : the number of velocity grids for which derivatives have to be
          calculated 
next    : list of id #s of velocity grids for which derivatives have to be calculated 

next    : the number of interface grids for which derivatives have to be
          calculated 
next    : list of id #s of interface grids for which derivatives have to be calculated 

next    : the number of source positions for which derivatives have to be
          calculated 
next    : list of id #s of sources for which derivatives have to be calculated 




gridsave.in:
------------

Edited by hand. There is a mode in the file mode_set.in (save_timefields_mode) which should be 
set to true if you want to save grids of arrival times. If it is set to true, 
the program expects to find this input file (gridsave.in, 
example included) that specifies what exactly is to be saved. 
gridsave.in is now set up to find times for arrivals that are reflections from the 
bottom interfaces for the first path in the default problem in the main distribution. 
For a simple case of direct arrivals in one region only, all numbers should be set to 1,  
i.e. the 4 on the last line should also be a one. The format of gridsave.in is 

line 1 : the id of the source for which arrival time grids are to be saved 
         and the number of paths from this source which are to be saved

line 2 : the ids of the paths to be saved 

line 3 : the first step on the path sequence for which saving is to start. 
         Currently the default behaviour of the code is to assign the smallest arrival 
         time associated with the path to a grid node if the wavefront passes over it 
         multiple times. By setting the first step to be saved to a value larger than 1 
         you can control this behaviour to some extent. For instance, for a simple 
         one-region model in which you are interested in a reflection from the bottom, 
         set this value to 2 and the first wavefront crossing over the node from the 
         source on the way down will be ignored.

These groups of 3 lines can be repeated for all sources you want to save arrival time fields for.



- ak135.dat , ak135.tvel
-------------------------

These are tables describing the ak135 reference model, leave them as they are
in the distribution


- ak135.tbl ,ak135.hed
-----------------------

These are PLATFORM AND COMPILER DEPENDENT binary formatted tables of travel
times in the ak135 model that are required if you want to use the capability
of the code to use teleseismic sources (i.e. sources that lie outside the
propagation grid). The initialization of the arrival time of a teleseismic wave
front on the boundaries of the propagation grid is based on the program ttimes
(Buland, Kennett) which contains these platform dependencies. You must
therefore generate these binary formatted tables yourself on your platform (if
you want to use teleseismic sources).  The main directory of the distribution
contains a subdirectory called 'aktimes'. Go into this directory, edit the
Makefile in there to refer to your own fortran 90 compiler (i.e. the same one
used to compile the main fast marching code), type 'make', execute
the two generated executables "remodl" and "setbrn" in that order, and copy
the resulting binary formatted files ak135.tbl and ak135.hed into the
directory in which you execute the fast marching code.




* OUTPUT FILES
**************

- arrivals.dat
--------------

Arrival times are saved in the file arrivals.dat. Each line contains an
arrival time, or an arrival time value of -1 if the ray path requested is
invalid for some reason. Each line contains the following information:

   the identification of the receiver, which is its position in the
   receivers.in file 

   the identification of the source, which is its position in the sources.in
   file  

   the identification of the ray path, which is its position in the list of paths
   from each source in the sources.in file

   the next entry is zero if the path is a normal path, and equal to the
   number of the ray in the list of rays found from a reflection match if this
   is part of the specified path. In this case the number of rays returned will
   not be known in advance.

   the actual arrival time in seconds

   a logical variable indicating whether the ray contained sections that were
   diffracted along an interface

   a logical variable indicating whether the ray contained sections that were
   head waves



- rays.dat
----------

The ray paths are saved in the file rays.dat. Each path, even one that
represents an invalid ray, has an entry. Each ray starts with a line
containing the following info, similar to that in the file arrivals.dat:

   the identifaction of the receiver, which is its position in the
   receivers.in file 

   the identifaction of the source, which is its position in the sources.in
   file  

   the identifaction of the ray path, which is its position in the list of paths
   from each source in the sources.in file

   the next entry is zero if the path is a normal path, and equal to the
   number of the ray in the list of rays found from a reflection match if this
   is part of the specified path. In this case the number of rays returned will
   not be known in advance.

   the number of ray sections of this ray. A ray section is a part of a ray
   that lies in one region, or equivalently between two interfaces. *** The
   number is zero if the ray is invalid, and in that case the following ray
   section information is skipped and the next line is the first info line for
   the next ray ***


The next line contains information about the first ray section

    the number of points on this ray section  (npoints)

    the identification of the region that contains this ray section

   a logical variable indicating whether the ray section contained a diffraction 

   a logical variable indicating whether the ray section contained a headwave
  

This followed by a list of npoints positions that define the ray section.

This pattern of ray section information is repeated for all the ray sections
that define this ray.


The next line again contains information about the next ray, just like the
starting line, and the entire pattern described above for the first ray is
repeated for all requested rays.


arrtimes.dat
------------

The output file arrtimes.dat is generated if you request that some grids
of arrival times be saved by setting save_timefields_mode in the file 
mode_set.in to true (T). The specific paths for which arrival time grids 
are saved are specified in the input file gridsave.in.

The format of arrtimes.dat is as follows:

First line : nr of nodes in radial, latitude and longitude direction

Second line : grid spacing in r, lat and long (km and degrees respectively)

Third line : origin of the grid in radius(km) lat and long (degrees)

Fourth line : the number of sets of arrival time values grids that follow


For each set of arrival times a first line is written that specifies the 
source  and the path from this source from which these arrival times on 
the grid are calculated. After this the arrival times on the grid nodes 
follow, one per line, with the r-index varying fastest, then the 
latitude-index, and the longitude-index varying slowest.  Missing values 
are possible in general, and are indicated by a value of -1.0.


- frechet.dat
-------------

The Frechet derivatives are saved in the file frechet.dat. To know which
derivative is with respect to what parameter, a global numbering system for
parameters such as velocity/interface grid nodal values and source parameters is
set up first. First come the nodal values of the velocity grids that have been
specified in the file frechet.in, with the radius index varying fastest, then
the latitude index and then the longitude index, for each velocity grid
specified. This is repeated for the nodal values of the interface
grids specified in frechet.in, with latitude index varying fastest. Finally
the derivatives with respect to source position and time (4 values) follow for
each source specified in frechet.in.

For each ray, only the non-zero partial derivatives are given in the file. The
following format is repeated for every ray path (arrival time) requested.

The first line contains

   the identification of the receiver, which is its position in the
   receivers.in file 

   the identification of the source, which is its position in the sources.in
   file  

   the identification of the ray path, which is its position in the list of paths
   from each source in the sources.in file

   the next item is zero if the path is a normal path, and equal to the
   number of the ray in the list of rays found from a reflection match if this
   is part of the specified path. 

   NPDEV, the number of non-zero partial derivatives with respect to model parameters
   for this arrival time. If the ray path is invalid, NPDEV is zero.

The following NPDEV lines contain two numbers, an integer and a real:

    the index of the model parameter according to the global parameter
    numbering system to which the partial derivative refers

    the partial derivative itself. Units are (sec)/(km/sec) for the
    velocities, (sec)/(km) for interfaces, (sec)/(km) for source positions and
    (sec)/(sec) for source time

NOTE: There is some ambiguity as to how to deal with partial derivatives with
respect to interface position when interfaces are pinched together at the
point a ray passes through. The method currently implemented first
calculates a partial derivative treating the pinched interfaces as one
interface, then divides this partial derivative by the number of interfaces
pinched at the point the ray passes through, and assigns the result to every
interface pinched at this position. How this behaves in an inversion has not
been tested.



- visualization
---------------

The results of the calculations can be visualized with the free OpenDX
visualization package. The FM3D code automatically produces several output
files in OpenDX format that contain the interfaces, sources, receivers and
raypaths. A number of simple OpenDX programs are provided in the distribution
to look at these. These are called iface*.net, where * stands for the number
of interfaces in the problem.
All subroutines creating the OpenDx files are in file visual.f90. You can
modify these if you want to generate files for another visualization
package. Most subroutines are straightforward, the only one that is slightly
more involved is 'display_interface', which creates a triangulation of the
interface before writing this triangulation to file for display. 




********************
* EXAMPLES ********* 
********************

NOTE: you have to re-generate the input files interfaces.in and vgrids.in
if you make changes to the propagation grid in propgrid.in.


- Example 1

The input files provided in the distribution are ready to run this example, it
should execute as soon as you run the executable fm3d after compilation .  It
is a very simple example which takes only about 35 seconds (on our
platform). If you have overwritten the input files by trying another example,
you can re-generate them by running the programs make_interfaces.f90 and
make_vgrids.f90 provided in the distribution, in that order, using the set
of velocity/interface definitions vdefs_simple.f90 and idefs_simple.f90. 
The example has 4 interfaces and illustrates P/S phase conversions. If at 
all possible, you should inspect the results with the OpenDx program 
(iface4.net) provided, which displays P rays as green and S rays as blue.
For this example only, an image if what this should look like is provided in 
phase_conversion.tiff

- Example 2

This example requires that you have generated the ak135.hed and ak135.tbl
input files as described above because it includes a teleseismic source.

The example programs make_cmplayers / make_cmpvelocities create input files
for a complex geometry with a subducting plate similar to the one from the
paper describing our method.  If you want to run this example, first copy the
appropriate lines in the provided propgrid.in, sources.in ,receivers.in and
frechet.in files to the top of the respective files. Then compile and run the
programs make_interfaces.f90 and make_vgrids.f90 (with definition files
idefs_cmplx.f90 and vdefs_cmplx.f90), in that order, to
generate new interfaces.in and vgrids.in files. Running the example takes
about 4 minutes on our platform.  The example contains 3 local sources and one
teleseismic source which is a PcP arrival from a source located 30 degrees
away from the grid. 50 rays are computed, so the image displaying them can be
confusing. Note the rays due to the teleseismic wave front that propgate as an
S wave through the grid: some of these come through the sides of the grid
relatively high up, which is a result of the inconsistency between the local
model and the ak135 model assumed outside the modelling region.


- Example 3

The programs make_interfaces.f90 and make_vgrids.f90 (with definition files
idefs_ak135.f90 and vdefs_ak135.f90) generate input files for the 
ak135 reference model.  As you will be able to deduce from the the lines in the 
sources.in and receiver.in file that define the phases for this example, 
it calculates three direct P arrivals and a PP arrival (which at the 
source/receiver separation of 22.416 degrees is actually a PnPn). 

Some of the problems associated with finding late reflections such as PP
phases are seen when the same problem is run at double the resolution.  In
that case the reflection matching algorithm finds four PnPn rays, with a
spread in travel times of about 60 msec. This is due to the fact that the
saddle point defining the reflection point on the surface is extremely broad,
and small errors create local minima during the reflection point search. This
is explained in more detail in the paper. Note that this result has a physical
basis, and that although the arrival time for this phase can be fairly
accurately determined, the ray path is poorly defined in reality.  






