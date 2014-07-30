display('Running dbexample_trextract_data')

dbexample_get_demodb_path;

echo on

db = dbopen( demodb_path,'r' );

db=dblookup_table( db,'wfdisc' );

db.record=0;

[time,endtime,nsamp,samprate]=dbgetv( db,'time','endtime','nsamp','samprate' );

tr = trload_css( db,time,endtime );

tr.record=0;

data=trextract_data( tr );

whos data

subplot( 1, 1, 1 );
plot( data )

% (Allow time for figure to come up when running in batch mode)
pause(0.5);

dbclose( db );

trdestroy( tr );

echo off
