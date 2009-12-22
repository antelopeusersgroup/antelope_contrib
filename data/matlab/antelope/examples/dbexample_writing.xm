display('Running dbexample_writing')

echo on

% Open up a new database for writing; choose the origin table:
output_dbname = dbexample_get_tempname( 'newdb', 'db' );

unix( ['/bin/rm -f ' output_dbname '*'] );

db=dbopen( output_dbname,'r+' );
db=dblookup_table( db,'origin' );

%      *****  Preferred method *****
% METHOD ONE: add a new row with with some fields set, all in one step:
dbaddv( db,'lat',41,'lon',-150,'depth',16,'time',str2epoch( '1/16/97 14:25' ),'nass',0,'ndef',0 );

% METHOD TWO: add a null row, then use dbputv to set some fields in it:
db.record=dbaddnull( db );
dbputv( db,'lat',40,'lon',70,'orid',dbnextid( db,'orid' ),'auth','MATLAB' );

% Ask for the number of records in the new database:
dbquery( db,'dbRECORD_COUNT' )

% ( preparation.. ) Get the text representation of two records from the database:
% [note for the tempted: under normal conditions, parsing the output 
%  of dbget would be the 'hard way' to use these database routines--
%  consider using the dbgetv call]
db.record = 0;
record1 = dbget( db )

db.record = 1;
record2 = dbget( db )

dbclose( db );

% Now open a new database

output_dbname_2 = dbexample_get_tempname( 'newdb2', 'db' );

unix( ['/bin/rm -f ' output_dbname_2 '*'] );

db=dbopen( output_dbname_2,'r+' );
db=dblookup_table( db,'origin' );

% METHOD THREE: put the text representation of an entire record into database
db.record=dbaddnull( db );
dbput( db,record1 );

% METHOD FOUR: put the text representation of an entire record into database
dbadd( db,record2 );

dbclose( db );

unix( ['/bin/rm -f ' output_dbname '*'] );
unix( ['/bin/rm -f ' output_dbname_2 '*'] );

echo off
