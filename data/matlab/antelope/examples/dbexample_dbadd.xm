display('Running dbexample_dbadd')

echo on

output_dbname = dbexample_get_tempname( 'newdb', 'db' );

unix( ['/bin/rm -f ' output_dbname '*'] );

db=dbopen( output_dbname,'r+' );

db=dblookup( db,'','origin','','dbSCRATCH' );

dbputv( db,'lat',61.5922,'lon',-149.130,'depth',20,'time',str2epoch( '9/30/2002 11:15 AM' ) );

db.record=dbadd( db,'dbSCRATCH' )

dbclose( db );

unix( ['/bin/rm -f ' output_dbname '*'] );

echo off
