display('Running dbexample_trinsert_data')

echo on

tr=trnew;

tr=dblookup_table( tr,'trace' );

% Construct a fake waveform:

data=sin( [0:999] ); 

nsamp=1000;

sta='SINE';

chan='BHZ';

time=str2epoch( '9/30/02 11:15 AM' );

samprate=20;      

endtime=tr_endtime( time,samprate,nsamp );

% Put the waveform into the trace-object:

tr.record=dbaddnull( tr );

dbputv( tr,'time',time,'samprate',samprate,'endtime',endtime,'sta',sta,'chan',chan );

trinsert_data( tr,data )

trdestroy( tr );

echo off
