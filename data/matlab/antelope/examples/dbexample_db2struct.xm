display('Running dbexample_db2struct')

dbexample_get_demodb_path;

echo on

db = dbopen( demodb_path,'r' );

db = dblookup_table( db, 'origin' );

db.record=0;

% Example 1:

db2struct( db )

% Example 2:

db=dblookup( db,'','','','dbALL' );

db2struct( db )

% Example 3:

db.record=0;

db2struct( db,'lat','lon','depth','mb' )

dbclose( db );

echo off
