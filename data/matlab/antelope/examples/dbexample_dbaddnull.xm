display('Running dbexample_dbaddnull')

echo on

output_dbname = dbexample_get_tempname( 'newdb', 'db' );

unix( ['/bin/rm -f ' output_dbname '*'] );

db=dbopen( output_dbname,'r+' );

db=dblookup_table( db,'origin' );

db.record =  dbaddnull( db )

dbclose( db );

unix( ['/bin/rm -f ' output_dbname '*'] );

echo off
