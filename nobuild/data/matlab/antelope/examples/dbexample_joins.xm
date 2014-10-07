display('Running dbexample_joins')

dbexample_get_demodb_path;

echo on

db = dbopen( demodb_path, 'r' );

db = dblookup_table( db, 'origin' );
dbassoc = dblookup_table( db, 'assoc' );
dbarrival = dblookup_table( db, 'arrival' );

db = dbjoin( db, dbassoc );
db = dbjoin( db, dbarrival );

% Find out the first orid:
db.record = 0;
orid = dbgetv( db, 'orid' );

% Subset our joined view for that orid:
dblookup( db,'','','','dbALL' );
expr = sprintf( 'orid == %d', orid );
db = dbsubset( db, expr );

% Display some results:
[sta, chan, iphase, epoch] = dbgetv( db,'sta','chan','iphase','arrival.time' );
[sta chan iphase epoch2str( epoch,'%H:%M:%S' )]

dbclose( db );

echo off
