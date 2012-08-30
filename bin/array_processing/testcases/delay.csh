#/bin/csh

# Remove previous results.
if (-e delay_*.dat) rm delay_*.dat

../bin/delay 0.00 315 0 YMSZ RPY korea delay_0.00_0.dat
../bin/delay 0.00 315 1 YMSZ RPY korea delay_0.00_1.dat
../bin/delay 0.05 315 0 YMSZ RPY korea delay_0.05_0.dat
../bin/delay 0.05 315 1 YMSZ RPY korea delay_0.05_1.dat

exit
