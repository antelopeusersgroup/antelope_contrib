display('Running dbexample_dbfilename')

dbexample_get_demodb_path;

echo on

db = dbopen( demodb_path,'r' );

db = dblookup_table( db,'instrument' );

% Find the external-file pathname of the response file for the 
% first record in the demo-database instrument table:

db.record=0;

dbfilename( db )                    

dbclose( db );

echo off
