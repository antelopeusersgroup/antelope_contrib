display('Running dbexample_dbsort')

dbexample_get_demodb_path;

echo on

db = dbopen( demodb_path,'r' );

db=dblookup_table( db,'origin' );

db=dbsubset( db,'mb>6.3' );

db=dbsort( db,'mb' )

dbgetv( db,'mb' )   

dbclose( db );

echo off
