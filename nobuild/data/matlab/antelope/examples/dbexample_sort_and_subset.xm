display('Running dbexample_sort_and_subset')

dbexample_get_demodb_path;

echo on

db = dbopen( demodb_path, 'r' );

db = dblookup_table( db, 'origin' );

db = dbsubset( db, 'mb > 6' );
db = dbsort( db, 'mb' );

[lat, lon, depth, epoch, mb] = dbgetv( db,'lat','lon','depth','time','mb' )

dbclose( db )

echo off
