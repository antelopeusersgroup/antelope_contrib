display('Running dbexample_get_hypocenter_vitals')

dbexample_get_demodb_path;

echo on

db = dbopen( demodb_path, 'r' );

db = dblookup_table( db, 'origin' );

nrows = dbquery( db, 'dbRECORD_COUNT' )

% That's too many rows to display for an example. Limit to Mb >6:
db = dbsubset( db, 'mb >= 6.0' );

% Now sort them by magnitude:
db = dbsort( db, 'mb' );

[lat, lon, depth, epoch, mb] = dbgetv( db,'lat','lon','depth','time','mb' );
[lat, lon, depth, mb]

dbclose( db )

echo off
