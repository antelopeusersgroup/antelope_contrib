display('Running dbexample_dbfind')

dbexample_get_demodb_path;

echo on

db = dbopen( demodb_path,'r' );

db = dblookup_table( db, 'origin' );

% Find the first listed record with body-wave magnitude above 6:
db.record = dbfind( db,'mb>6',0 )

dbgetv( db,'mb' ) 

% Find the third listed record with body-wave magnitude above 6:
db.record = dbfind( db,'mb>6',0,3 )

dbgetv( db,'mb' ) 

% Find the last listed record with body-wave magnitude above 6:
db.record = dbfind( db,'mb>6','backwards' )

dbgetv( db,'mb' ) 

dbclose( db );

echo off
