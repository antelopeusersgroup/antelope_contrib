dbexample_get_demodb_path;

echo on

db = dbopen( demodb_path,'r' );

db=dblookup_table( db,'wfdisc' );

db= dbsort( db, 'sta', 'chan', 'time' );

format long

[time,endtime,nsamp,samprate]=dbgetv( db,'time','endtime','nsamp','samprate' );

tr = trload_cssgrp( db,time(1),endtime(1) )

tr.record=0;
data=trextract_data( tr );
plot(data)

% (Allow time for figure to come up when running in batch mode)
pause(0.5);

dbclose( db );

echo off
