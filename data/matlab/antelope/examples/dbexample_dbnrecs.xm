echo on

db = dbopen( demodb_path,'r' );

db = dblookup_table( db,'origin' );

nrecs = dbnrecs( db )

dbclose( db );

echo off
