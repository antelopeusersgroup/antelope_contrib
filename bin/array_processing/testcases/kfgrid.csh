#!/bin/csh

if (-e synkf.fkgrid) rm synkf.fkgrid
if (-e synkfgrids) then
  rm synkfgrids/*
else
  mkdir synkfgrids
endif

../bin/kfgrid 0.0 99.95 5.0 2.0  51 2.0 51 0 SHZ SYN 0 synkf synkfgrids grid_51x51
../bin/kfgrid 0.0 99.95 5.0 2.0 101 2.0 51 1 SHZ SYN 0 synkf synkfgrids grid_101x51
../bin/kfgrid 0.0 99.95 5.0 2.0  51 2.0 51 0 SHZ SYN 1 synkf synkfgrids grid_51x51_whiten
../bin/kfgrid 0.0 99.95 5.0 2.0 101 2.0 51 1 SHZ SYN 1 synkf synkfgrids grid_101x51_whiten

exit
