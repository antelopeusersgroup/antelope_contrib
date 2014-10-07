display('Running dbexample_dbgetv')

dbexample_get_demodb_path;

echo on

db = dbopen( demodb_path,'r' );

db = dblookup_table( db, 'origin' );

% First example: get the lat, lon, and author of the first hypocenter in 
% the database:

db.record=0;

[lat,lon,auth] = dbgetv( db,'lat','lon','auth' )

% Second example: get the magnitudes of all earthquakes greater than 
% mb 6.0

db = dbsubset( db,'mb>6' );

dbgetv( db,'mb' )  

dbclose( db );

echo off
