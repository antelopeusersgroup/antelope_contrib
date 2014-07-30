display('Running dbexample_tr_samp2time')

echo on

time =    7.061397152e+08;
samprate = 20;

% Find the time of sample number 1000
tr_samp2time( time,samprate,1000 )

echo off
