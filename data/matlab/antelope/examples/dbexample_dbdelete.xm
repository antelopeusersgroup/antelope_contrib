echo on

output_dbname = ['/tmp/newdb_' getenv('USER')]

unix( ['rm -f ' output_dbname '*'] );

db=dbopen( output_dbname,'r+' );

db=dblookup_table( db,'origin' );

% Add four copies of the same quake, all at slightly different times:

db.record=dbaddv( db,'lat',61.5922,'lon',-149.130,'depth',20,'time',str2epoch( 'now' ) )

db.record=dbaddv( db,'lat',61.5922,'lon',-149.130,'depth',20,'time',str2epoch( 'now' ) );

db.record=dbaddv( db,'lat',61.5922,'lon',-149.130,'depth',20,'time',str2epoch( 'now' ) ); 

db.record=dbaddv( db,'lat',61.5922,'lon',-149.130,'depth',20,'time',str2epoch( 'now' ) );

% Count how many records we have:
dbquery( db,'dbRECORD_COUNT' )

% Now remove one of the records:
db.record=1;

dbdelete( db )

% Count how many records remain:
dbquery( db,'dbRECORD_COUNT' )

dbclose( db );

echo off
