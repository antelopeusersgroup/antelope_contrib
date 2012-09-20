#!/bin/csh

# This script tests most options of funcgen.  Results can be viewed with
# % dbpick syn

if (-e syn.wfdisc) rm syn.wfdisc
if (-e wfsyn) then
  rm wfsyn/*
else
  mkdir wfsyn
endif

../bin/funcgen zeros     0  0 0.0 AAA SHZ 0 20 2000 syn wfsyn zeros
../bin/funcgen delta    10  0 0.0 BBB SHZ 0 20 2000 syn wfsyn delta
../bin/funcgen boxcar    0 10 0.0 CCC SHZ 0 20 2000 syn wfsyn boxcar_00_10
../bin/funcgen boxcar    0 20 0.0 DDD SHZ 0 20 2000 syn wfsyn boxcar_00_20
../bin/funcgen boxcar   10 20 0.0 EEE SHZ 0 20 2000 syn wfsyn boxcar_10_20
../bin/funcgen triangle  0 10 0.0 FFF SHZ 0 20 2000 syn wfsyn triang_00_10
../bin/funcgen triangle  0 20 0.0 GGG SHZ 0 20 2000 syn wfsyn triang_00_20
../bin/funcgen triangle 10 20 0.0 HHH SHZ 0 20 2000 syn wfsyn triang_10_20
../bin/funcgen sine      0 10 0.0 III SHZ 0 20 2000 syn wfsyn sine_10
../bin/funcgen sine      0  2 0.0 JJJ SHZ 0 20 2000 syn wfsyn sine_02
../bin/funcgen impulse   0 10 0.0 KKK SHZ 0 20 2000 syn wfsyn impulse_00_10
../bin/funcgen impulse   0  2 0.0 LLL SHZ 0 20 2000 syn wfsyn impulse_00_02
../bin/funcgen impulse  10  2 0.0 MMM SHZ 0 20 2000 syn wfsyn impulse_10_02
../bin/funcgen impulse  10  2 0.1 NNN SHZ 0 20 2000 syn wfsyn noisy_impulse_10_02
../bin/funcgen noise     0  2 0.0 OOO SHZ 0 20 2000 syn wfsyn noise

exit
