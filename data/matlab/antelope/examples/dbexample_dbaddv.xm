echo on

output_dbname = ['/tmp/newdb_' getenv('USER')]

unix( ['/bin/rm -f ' output_dbname '*'] );

db=dbopen( output_dbname,'r+' );

db=dblookup_table( db,'origin' );

db.record=dbaddv( db,'lat',61.5922,'lon',-149.130,'depth',20,'time',str2epoch( '9/30/2002 11:15 AM' ) )

dbclose( db );

echo off
