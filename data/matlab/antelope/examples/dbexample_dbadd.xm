echo on

output_dbname = ['/tmp/newdb_' getenv('USER')]

unix( ['rm -f ' output_dbname '*'] );

db=dbopen( output_dbname,'r+' );

db=dblookup( db,'','origin','','dbSCRATCH' );

dbputv( db,'lat',61.5922,'lon',-149.130,'depth',20,'time',str2epoch( 'now' ) );

db.record=dbadd( db,'dbSCRATCH' )

dbclose( db );

echo off
