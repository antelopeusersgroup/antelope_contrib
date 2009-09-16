display('Running dbexample_dbdelete')

echo on

output_dbname = dbexample_get_tempname( 'newdb', 'db' );

unix( ['/bin/rm -f ' output_dbname '*'] );

db=dbopen( output_dbname,'r+' );

db=dblookup_table( db,'origin' );

% Add four copies of the same quake, all at slightly different times:

db.record=dbaddv( db,'lat',61.5922,'lon',-149.130,'depth',20,'time',str2epoch( '9/30/02 11:15 AM' ),'nass',0,'ndef',0 )

db.record=dbaddv( db,'lat',61.5922,'lon',-149.130,'depth',20,'time',str2epoch( '9/30/02 11:16 AM' ),'nass',0,'ndef',0 );

db.record=dbaddv( db,'lat',61.5922,'lon',-149.130,'depth',20,'time',str2epoch( '9/30/02 11:17 AM' ),'nass',0,'ndef',0 ); 

db.record=dbaddv( db,'lat',61.5922,'lon',-149.130,'depth',20,'time',str2epoch( '9/30/02 11:18 AM' ),'nass',0,'ndef',0 );

% Count how many records we have:
dbquery( db,'dbRECORD_COUNT' )

% Now remove one of the records:
db.record=1;

dbdelete( db )

% Count how many records remain:
dbquery( db,'dbRECORD_COUNT' )

dbclose( db );

unix( ['/bin/rm -f ' output_dbname '*'] );

echo off
