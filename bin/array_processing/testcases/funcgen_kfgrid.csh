#!/bin/csh

# This script tests most options of funcgen.  Results can be viewed with
# % dbpick synkf

if (-e synkf.wfdisc) rm synkf.wfdisc

if (-e wfsynkf) then
  rm wfsynkf/*
else
  mkdir wfsynkf
endif

../bin/funcgen triangle   0 10 0.0 UUU SHZ 0 20 2000 synkf wfsynkf triang_00_10
../bin/funcgen triangle  10 10 0.0 VVV SHZ 0 20 2000 synkf wfsynkf triang_10_10
../bin/funcgen triangle  20 10 0.0 WWW SHZ 0 20 2000 synkf wfsynkf triang_20_10
../bin/funcgen triangle  30 10 0.0 XXX SHZ 0 20 2000 synkf wfsynkf triang_30_10
../bin/funcgen triangle  40 10 0.0 YYY SHZ 0 20 2000 synkf wfsynkf triang_40_10
../bin/funcgen triangle  50 10 0.0 ZZZ SHZ 0 20 2000 synkf wfsynkf triang_50_10

exit
