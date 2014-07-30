display('Running dbexample_dbputv')

echo on

output_dbname = dbexample_get_tempname( 'newdb', 'db' );

unix( ['/bin/rm -f ' output_dbname '*'] );

db=dbopen( output_dbname,'r+' );

db=dblookup_table( db,'origin' );

db.record =  dbaddnull( db );

dbputv( db,'lat',61.5922,'lon',-149.130,'depth',20,'time',str2epoch( '9/30/02 11:15 AM' ) ) 

dbclose( db );

unix( ['/bin/rm -f ' output_dbname '*'] );

echo off
