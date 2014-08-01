display('Running dbexample_eval_response')

dbexample_get_demodb_path;

echo on

db = dbopen( demodb_path,'r' );

db=dblookup_table( db,'instrument' );

db.record=0;

file=dbfilename( db ); 

resp = dbresponse( file );

eval_response( resp,6.28 )

eval_response( resp,transpose( [0.01 0.1 1 10] )*6.28 )

dbclose( db );

echo off
