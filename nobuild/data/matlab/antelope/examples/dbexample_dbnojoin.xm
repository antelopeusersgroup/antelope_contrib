display('Running dbexample_dbnojoin')

dbexample_get_demodb_path;

echo on

db = dbopen( demodb_path,'r' );

dbarrival=dblookup_table( db,'arrival' );

dbwfdisc=dblookup_table( db,'wfdisc' );

db=dbnojoin( dbarrival,dbwfdisc )

dbclose( db );

echo off
