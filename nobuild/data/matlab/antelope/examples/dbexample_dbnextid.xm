display('Running dbexample_dbnextid')

echo on

output_dbname = dbexample_get_tempname( 'newdb', 'db' );

unix( ['/bin/rm -f ' output_dbname '*'] );

db=dbopen( output_dbname,'r+' );

dbnextid( db,'orid' ) 

dbclose( db );

unix( ['/bin/rm -f ' output_dbname '*'] );

echo off
