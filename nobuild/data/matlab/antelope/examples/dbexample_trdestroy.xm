display('Running dbexample_trdestroy')

echo on

db = dbopen( demodb_path,'r' );

db=dblookup_table( db,'wfdisc' );

db.record=0;

[time,endtime,nsamp,samprate]=dbgetv( db,'time','endtime','nsamp','samprate' );

tr = trload_css( db,time,endtime );

trdestroy( tr )

dbclose( db );

echo off
