display('Running dbexample_trapply_calib')

echo on

db = dbopen( demodb_path,'r' );

db=dblookup_table( db,'wfdisc' );

db.record=0;

[time,endtime,nsamp,samprate]=dbgetv( db,'time','endtime','nsamp','samprate' );

tr = trload_css( db,time,endtime );

trapply_calib( tr )       

dbclose( db );

trdestroy( tr );

echo off
