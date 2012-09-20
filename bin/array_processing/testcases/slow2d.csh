#!/bin/csh

# Note that slow2d will append to output data file if it exists.
# This script uses the data recorded for the Korean nuke.

# Clear out previous results.
if (-e korea.fkgrid) rm korea.fkgrid
if (-e kmaps) then
  rm kmaps/*
else
  mkdir kmaps
endif

../bin/slow2d 1160358465 20 5 "BW 0.6 4 1.2 4" 20.0 0.20 101 1 1 COH YMSZ RPY korea kmaps kmap_coh_0.6-1.2.dat
../bin/slow2d 1160358465 20 5 "BW 1.2 4 1.8 4" 20.0 0.20 101 1 1 COH YMSZ RPY korea kmaps kmap_coh_1.2-1.8.dat
../bin/slow2d 1160358465 20 5 "BW 1.8 4 2.4 4" 20.0 0.20 101 1 1 COH YMSZ RPY korea kmaps kmap_coh_1.8-2.4.dat
../bin/slow2d 1160358465 20 5 "BW 2.4 4 3.0 4" 20.0 0.20 101 1 1 COH YMSZ RPY korea kmaps kmap_coh_2.4-3.0.dat
../bin/slow2d 1160358465 20 5 "BW 0.8 4 3.0 4" 20.0 0.20 101 1 2 RT2 YMSZ RPY korea kmaps kmap_rt2_0.8-3.0.dat
../bin/slow2d 1160358465 20 5 "BW 0.8 4 3.0 4" 20.0 0.20 101 1 3 RT3 YMSZ RPY korea kmaps kmap_rt3_0.8-3.0.dat
../bin/slow2d 1160358465 20 5 "BW 0.8 4 3.0 4" 20.0 0.20 101 1 4 RT4 YMSZ RPY korea kmaps kmap_rt4_0.8-3.0.dat
../bin/slow2d 1160358465 20 5 "BW 0.8 4 3.0 4" 20.0 0.20 101 1 5 INC YMSZ RPY korea kmaps kmap_inc_0.8-3.0.dat

exit
