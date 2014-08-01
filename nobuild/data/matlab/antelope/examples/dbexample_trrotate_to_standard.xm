display('Running dbexample_trrotate_to_standard')

dbexample_get_demodb_path;

echo on

db = dbopen( demodb_path,'r' );

db=dblookup_table( db,'wfdisc' );

db.record=0;

format long

[time,endtime,nsamp,samprate]=dbgetv( db,'time','endtime','nsamp','samprate' )

tr = trload_css( db,time,endtime )
trapply_calib( tr );

nrecs_before_rotate = dbnrecs(tr);

% Rotate all data to standard coordinates. This command actually 
% does nothing in this case since the sample database is already 
% aligned with E,N,Z...

trrotate_to_standard( tr, {'BHE','BHN','BHZ'} )

nrecs_after_rotate = dbnrecs( tr );

% Since we probably only care about the newly rotated components, 
% subset to ignore the rest:
tr = dblist2subset( tr, nrecs_before_rotate:nrecs_after_rotate-1 );

trdestroy( tr );

dbclose( db );

echo off
