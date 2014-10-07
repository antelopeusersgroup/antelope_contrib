display('Running dbexample_dbclose')

dbexample_get_demodb_path;

echo on

db = dbopen( demodb_path,'r' );

dbclose( db )

echo off
