echo on

unix( ['rm -f ' output_dbname '*'] );

db=dbopen( output_dbname,'r+' );

db=dblookup_table( db,'origin' );

% Add four copies of the same quake, all at slightly different times:

db.record=dbaddv( db,'lat',61.5922,'lon',-149.130,'depth',20,'time',str2epoch( 'now' ) )

db.record=dbaddv( db,'lat',61.5922,'lon',-149.130,'depth',20,'time',str2epoch( 'now' ) );

db.record=dbaddv( db,'lat',61.5922,'lon',-149.130,'depth',20,'time',str2epoch( 'now' ) ); 

db.record=dbaddv( db,'lat',61.5922,'lon',-149.130,'depth',20,'time',str2epoch( 'now' ) );

% Mark the second row of the origin table ( i.e. set all its fields to 
%  the null value, one use of which is as a label for which records 
%  to remove completely with the dbcrunch command )

db.record=1;

dbmark( db )

dbclose( db );

echo off
