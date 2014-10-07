display('Running dbexample_trload_cssgrp')

dbexample_get_demodb_path;

echo on

db = dbopen( demodb_path,'r' );

db=dblookup_table( db,'wfdisc' );

% CRITICAL NOTE: This sort by sta, chan, and time MUST precede the 
% trload_cssgrp command:
db= dbsort( db, 'sta', 'chan', 'time' );

format long

[time,endtime,nsamp,samprate]=dbgetv( db,'time','endtime','nsamp','samprate' );

tr = trload_cssgrp( db,time(1),endtime(1) )

tr.record=0;
data=trextract_data( tr );
subplot( 1, 1, 1 );
plot(data)

% (Allow time for figure to come up when running in batch mode)
pause(0.5);

dbclose( db );

trdestroy( tr );

echo off
