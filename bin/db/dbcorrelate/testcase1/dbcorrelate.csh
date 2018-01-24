# This runs the correlation program on the synthetic data.
# Compare output file to the original correl_output.dat -- should be identical
# to within machine roundoff.
# For some unknown reason, correl_output1.dat may have lines in different order
# than correl_output.dat.  After fixing with vi, the diff of the two files 
# should show no significant differences in numerical dt and ccc values.
../bin/dbcorrelate db . 0 1.0 0.25 0.0 event_pairs.dat correl_output1.dat
# The following makes sure all P and S correlations are used.
../bin/create_ccfile correl_output1.dat dt.cc 0.0 0 0.0 0.2 1
echo "output in dt.cc"
exit
