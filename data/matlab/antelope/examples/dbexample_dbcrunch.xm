display('Running dbexample_dbcrunch')

echo on

output_dbname = ['/tmp/newdb_' getenv('USER')]

unix( ['/bin/rm -f ' output_dbname '*'] );

db=dbopen( output_dbname,'r+' );

db=dblookup_table( db,'origin' );

% Add four copies of the same quake, all at slightly different times:

db.record=dbaddv( db,'lat',61.5922,'lon',-149.130,'depth',20,'time',str2epoch( '9/30/2002 11:15 AM' ) )

db.record=dbaddv( db,'lat',61.5922,'lon',-149.130,'depth',20,'time',str2epoch( '9/30/2002 11:16 AM' ) );

db.record=dbaddv( db,'lat',61.5922,'lon',-149.130,'depth',20,'time',str2epoch( '9/30/2002 11:17 AM' ) ); 

db.record=dbaddv( db,'lat',61.5922,'lon',-149.130,'depth',20,'time',str2epoch( '9/30/2002 11:18 AM' ) );

% Check how many records we have:
dbquery( db,'dbRECORD_COUNT' )

% Now remove one of the records:
db.record=1;

dbmark( db )

dbcrunch( db )

% Check to see that we have one fewer records
dbquery( db,'dbRECORD_COUNT' )

dbclose( db );

echo off
