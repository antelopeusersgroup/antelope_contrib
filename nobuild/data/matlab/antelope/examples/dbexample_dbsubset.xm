display('Running dbexample_dbsubset')

dbexample_get_demodb_path;

echo on

db = dbopen( demodb_path,'r' );

db=dblookup_table( db,'origin' );

db=dbsubset( db,'mb>6.3' );

dbgetv( db,'mb' )

dbclose( db );

echo off
