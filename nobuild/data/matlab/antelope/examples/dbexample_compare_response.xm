display('Running dbexample_compare_response')

echo on

db = dbopen( demodb_path,'r' );

db=dblookup_table( db,'instrument' );

db.record=0;
file1=dbfilename( db ); 

db.record=1;
file2=dbfilename( db ); 

resp1 = dbresponse( file1 );
resp2 = dbresponse( file2 );

compare_response( resp1, resp2 )

free_response( resp1 )
free_response( resp2 )

dbclose( db );

echo off
