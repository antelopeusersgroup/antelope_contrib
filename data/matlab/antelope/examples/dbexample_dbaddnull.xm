echo on

output_dbname = ['/tmp/newdb_' getenv('USER')]

unix( ['/bin/rm -f ' output_dbname '*'] );

db=dbopen( output_dbname,'r+' );

db=dblookup_table( db,'origin' );

db.record =  dbaddnull( db )

dbclose( db );

echo off
