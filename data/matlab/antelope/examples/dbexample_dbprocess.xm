display('Running dbexample_dbprocess')

dbexample_get_demodb_path;

echo on

db = dbopen( demodb_path,'r' );

db = dbprocess( db, { 'dbopen arrival'; 'dbsubset sta == "AAK"'; 'dbjoin assoc' } );

[iphase, delta] = dbgetv( db,'iphase', 'timeres' )

dbclose( db );

echo off
