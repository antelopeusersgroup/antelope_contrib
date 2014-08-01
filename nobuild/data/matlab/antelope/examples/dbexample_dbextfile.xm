display('Running dbexample_dbextfile')

dbexample_get_demodb_path;

echo on

db = dbopen( demodb_path,'r' );

db = dblookup_table( db, 'wfdisc' );
dbt = dblookup_table( db, 'sensor' );
db = dbjoin( db, dbt )
dbt = dblookup_table( db,'instrument' );
db = dbjoin( db, dbt )

% Find the external-file pathnames of the response file
% and the waveform file for the first record in the
% demo-database view formed above:

db.record=0;

dbextfile( db, 'instrument' )                    

dbextfile( db, 'wfdisc' )                    

dbclose( db );

echo off
