echo on

output_dbname = ['/tmp/newdb_' getenv('USER')]

unix( ['rm -f ' output_dbname '*'] );

db=dbopen( output_dbname,'r+' );

dbnextid( db,'orid' ) 

dbclose( db );

echo off
