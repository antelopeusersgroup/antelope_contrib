GENERAL OVERVIEW
The files in this directory represent the adaptation of Cecily Wolfe's
shear-wave splitting code for use with the antelope database systems and
Matlab (currently under Solaris SPARC only, other environments at your own
risk) here at IGPP.  

Unlike the original code, this new code is broken down into a computational
portion and a display portion.  The computational side computes the shear-wave
splitting paramters and stores them in the antelope database using the schema
ssplit1.0, an extension to the css3.0 format. The display side uses these
parameters to modify waveforms and display partical motion, shifted time
windows, and a contour plot of the variance using Matlab.

Shear wave splitting parameters can be computed for individual events or for
groups of events recoreded at the same station. The parameters recorded
include the estimated fast polarization direction and estimated delay time,
computed as per Silver and Chan [1991].


USAGE

1) Pick time window for splitting.

This can be done with a program like dbpick.  Windows should be picked on either
the north or east component of a seismogram; the start of the window should be
indicated with an 'A' phase pick and the end of the window should be indicated with
a 'F' phase pick.

2) Populate the split table with dbshearsplit_compute.

dbshearsplit_compute prompts the user for origin id, station, database, and filter,
using the defaults provided in the parameter file dbshearsplit.pf where appropriate.

3) View the results in matlab by running dbshearsplit_display.m.

dbshearsplit_display requires two arguments (the splitid and the database name) and 
takes an optional third argument to print the results to a postscript file. Type
'help dbshearsplit_display' in matlab for further details.


ADAPTING THIS CODE
The original shear-wave splitting code written by George Helffrich and modified
by Cecily Wolfe had been divided into versions for Solaris and non-Solaris
operating systems respectively.  This version developed for the Antelope 
Real-Time System is built from the Solaris-specific codebase.  It is unknown
at this time what modifications would need to be made to cleanly compile this 
code under other operating systems; however, I will document additions
made to the code since that will likely cause problems for anyone making such
an attempt.

