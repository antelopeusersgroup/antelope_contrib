display('Running dbexample_trload_css')

dbexample_get_demodb_path;

echo on

db = dbopen( demodb_path,'r' );

db=dblookup_table( db,'wfdisc' );

db.record=0;

format long

[time,endtime,nsamp,samprate]=dbgetv( db,'time','endtime','nsamp','samprate' )

tr = trload_css( db,time,endtime )

dbclose( db );

trdestroy( tr );

echo off
