#!/bin/csh

# Remove previous results.
rm beams.*
if (-e wfbeams) then 
  rm wfbeams/*
else
 mkdir wfbeams
endif

../bin/beam 1160358455 30 "BW 0.8 4 3.0 4" 20.0 0.0493 315 1 1 0 COH  YMSZ  RPY korea beams wfbeams YM_SHZ_COH_beam
../bin/beam 1160358455 30 "BW 0.8 4 3.0 4" 20.0 0.0493 315 1 2 0 RT2  YMSZ  RPY korea beams wfbeams YM_SHZ_2RT_beam
../bin/beam 1160358455 30 "BW 0.8 4 3.0 4" 20.0 0.0493 315 1 3 0 RT3  YMSZ  RPY korea beams wfbeams YM_SHZ_3RT_beam
../bin/beam 1160358455 30 "BW 0.8 4 3.0 4" 20.0 0.0493 315 1 4 0 RT4  YMSZ  RPY korea beams wfbeams YM_SHZ_4RT_beam
../bin/beam 1160358455 30 "BW 0.8 4 3.0 4" 20.0 0.0493 315 1 5 0 INC  YMSZ  RPY korea beams wfbeams YM_SHZ_INC_beam
../bin/beam 1160358455 30 "BW 0.8 4 8.0 4" 20.0 0.0493 315 1 1 0 COH2 YMSZ  RPY korea beams wfbeams YM_SHZ_COH2_beam
../bin/beam 1160358455 30 "BW 0.8 4 3.0 4" 20.0 0.0493 315 1 1 1 COH1 YMSZ1 RPY korea beams ./wfbeams YM_SHZ_COH1_beam
