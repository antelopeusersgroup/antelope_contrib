display('Running dbexample_dbnextid')

echo on

output_dbname = ['/tmp/newdb_' getenv('USER')]

unix( ['/bin/rm -f ' output_dbname '*'] );

db=dbopen( output_dbname,'r+' );

dbnextid( db,'orid' ) 

dbclose( db );

echo off
