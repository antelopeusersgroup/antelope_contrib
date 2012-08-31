#!/bin/csh

# Remove previous results.
if (-e sigcor.dat) rm sigcor.dat

../bin/sigcor 1160358470.0 1.0 0.5 "" 20.0 0.0493 315 YMSZ9 RPY korea sigcor.dat

exit
