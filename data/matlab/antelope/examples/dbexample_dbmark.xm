display('Running dbexample_dbmark')

echo on

unix( ['/bin/rm -f ' output_dbname '*'] );

db=dbopen( output_dbname,'r+' );

db=dblookup_table( db,'origin' );

% Add four copies of the same quake, all at slightly different times:

db.record=dbaddv( db,'lat',61.5922,'lon',-149.130,'depth',20,'time',str2epoch( '9/30/02 11:15 AM' ),'nass',0,'ndef',0 )

db.record=dbaddv( db,'lat',61.5922,'lon',-149.130,'depth',20,'time',str2epoch( '9/30/02 11:16 AM' ),'nass',0,'ndef',0 );

db.record=dbaddv( db,'lat',61.5922,'lon',-149.130,'depth',20,'time',str2epoch( '9/30/02 11:17 AM' ),'nass',0,'ndef',0 ); 

db.record=dbaddv( db,'lat',61.5922,'lon',-149.130,'depth',20,'time',str2epoch( '9/30/02 11:18 AM' ),'nass',0,'ndef',0 );

% Mark the second row of the origin table ( i.e. set all its fields to 
%  the null value, one use of which is as a label for which records 
%  to remove completely with the dbcrunch command )

db.record=1;

dbmark( db )

dbclose( db );

echo off
