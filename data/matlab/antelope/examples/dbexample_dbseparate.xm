display('Running dbexample_dbseparate')

dbexample_get_demodb_path;

echo on

db = dbopen( demodb_path,'r' );
db = dbprocess( db, { 'dbopen wfdisc'; ...
		      'dbjoin arrival'; ...
		      'dbjoin assoc'; ...
		      'dbjoin origin'; ...
		      'dbsubset orid == 645' } );

db = dbseparate( db, 'wfdisc' );

db.record=0;
dbextfile( db, 'wfdisc' )

dbclose( db );

echo off
