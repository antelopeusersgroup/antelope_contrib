display('Running dbexample_dbsever')

dbexample_get_demodb_path;

echo on

db = dbopen( demodb_path,'r' );

dborigin=dblookup_table( db,'origin' );

dbstamag=dblookup_table( db,'stamag' );

db=dbjoin( dborigin, dbstamag );

% Get rid of the stamage values now that we know which orids have stamags:

db= dbsever( db, 'stamag' )

dbgetv( db, 'orid' )

dbclose( db );

echo off
