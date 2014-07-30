display('Running dbexample_free_response')

echo on

db = dbopen( demodb_path,'r' );

db=dblookup_table( db,'instrument' );

db.record=0;

file=dbfilename( db ); 

resp = dbresponse( file );

free_response( resp )

dbclose( db );

echo off
