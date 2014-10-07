display('Running dbexample_dbjoin_keys')

dbexample_get_demodb_path;

echo on

db = dbopen( demodb_path,'r' );

dbarrival=dblookup_table( db,'arrival' );

dbwfdisc=dblookup_table( db,'wfdisc' );

% Example 1:

dbjoin_keys( dbarrival,dbwfdisc )       

% Example 2:

dbjoin_keys( db,'origin','assoc' )

dbclose( db );

echo off
