display('Running dbexample_dblookup_table')

dbexample_get_demodb_path;

echo on

db = dbopen( demodb_path,'r' );

% Set the database pointer to point to the origin table:
db = dblookup_table( db,'origin' )

dbclose( db );

echo off
