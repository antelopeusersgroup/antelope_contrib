display('Running dbexample_dbresponse')

dbexample_get_demodb_path;

echo on

db = dbopen( demodb_path,'r' );

db=dblookup_table( db,'instrument' );

db.record=0;

file=dbfilename( db ) 

resp = dbresponse( file )

dbclose( db );

echo off
