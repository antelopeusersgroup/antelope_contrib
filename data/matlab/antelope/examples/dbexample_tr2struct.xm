dbexample_get_demodb_path;

echo on

filename = '/tmp/dbexample_samplemat.mat';

db = dbopen( demodb_path,'r' );

db=dblookup_table( db,'wfdisc' );

db.record=0;

[time,endtime,nsamp,samprate]=dbgetv( db,'time','endtime','nsamp','samprate' );

tr = trload_css( db, time, endtime );

s = tr2struct( tr );

save '/tmp/dbexample_data.mat' s

dbclose( db );

echo off
