#!/bin/csh

# Remove previous results.
if (-e kfrespmap.fkgrid) rm kfrespmap.fkgrid
if (-e kfrespmaps) then
  rm kfrespmaps/*
else
  mkdir kfrespmaps
endif

../bin/kfresp 10.0 0.20 101 2.0 51 1 6 10.0 0.0 10.0 SHZ dummy dummy kfrespmap kfrespmaps resp_linear_101x51 

exit
