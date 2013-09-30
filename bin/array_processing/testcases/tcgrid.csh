#!/bin/csh

# Remove previous results.
if (-e tcgrid.fkgrid) rm tcgrid.fkgrid
if (-e tcgridmaps) then
  rm tcgridmaps/*
else
  mkdir tcgridmaps
endif

../bin/tcgrid 0.0 99.95 5.0 50.0  51 4.0  51 SHZ SYN 0 tcgrid tcgridmaps tcgrid_51x51
../bin/tcgrid 0.0 99.95 5.0 50.0 101 4.0  51 SHZ SYN 0 tcgrid tcgridmaps tcgrid_101x51
../bin/tcgrid 0.0 99.95 5.0 50.0  51 4.0 101 SHZ SYN 0 tcgrid tcgridmaps tcgrid_51x101

exit
