display('Running dbexample_dblookup')

dbexample_get_demodb_path;

echo on

db = dbopen( demodb_path,'r' );

% Set the database pointer to point to all records of the origin table:
dblookup( db,'','origin','','dbALL' )      

dbclose( db );

echo off
