display('Running dbexample_dbget')

dbexample_get_demodb_path;

echo on

db = dbopen( demodb_path,'r' );

db = dblookup_table( db, 'origin' );

% Extract the entirety of the first row of the origin table. 
% NOTE: before trying to parse this row afresh, consider using 
% the dbgetv command which will parse it for you!

db.record=0;

dbget( db )

dbclose( db );

echo off
