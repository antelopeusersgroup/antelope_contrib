#!/bin/csh

# Remove previous results.
if (-e TEST_backazimuths.dat) rm TEST_backazimuths.dat
if (-e TEST_distances.dat) rm TEST_distances.dat

../bin/distaz TEST korea

exit
