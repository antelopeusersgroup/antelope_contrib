display('Running dbexample_dbungroup')

dbexample_get_demodb_path;

echo on

db = dbopen( demodb_path,'r' );

db = dblookup_table( db, 'arrival' );

db = dbsort( db, 'sta' );

db = dbgroup( db, { 'sta' } );

% Subset for one station:
db = dbsubset( db, 'sta == "AAK"' );

db = dbungroup( db );

% Get the arriving phases detected at this station:
db = dblookup( db, '', '', '', 'dbALL' );
dbgetv( db, 'iphase' )

dbclose( db );

echo off
