#!/bin/csh

# Remove previous results.
if (-e arfmap.fkgrid) rm arfmap.fkgrid
if (-e arfmaps) then
  rm arfmaps/*
else
  mkdir arfmaps
endif

../bin/arf 1.0 0.20 101 YMSZ RPY arfmap arfmaps arfmap_1.0.dat
../bin/arf 2.0 0.20 101 YMSZ RPY arfmap arfmaps arfmap_2.0.dat
../bin/arf 1.0 0.20 101 LINE RPY arfmap arfmaps arfmap_line_1.0.dat
../bin/arf 2.0 0.20 101 LINE RPY arfmap arfmaps arfmap_line_2.0.dat

exit
