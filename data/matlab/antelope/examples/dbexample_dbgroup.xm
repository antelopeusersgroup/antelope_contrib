display('Running dbexample_dbgroup')

dbexample_get_demodb_path;

echo on

db = dbopen( demodb_path,'r' );

db = dblookup_table( db, 'arrival' );

db = dbsort( db, 'sta' );

db = dbgroup( db, { 'sta' } );

% Find the number of arrivals at each station:
for i=1:dbnrecs(db)
	db.record=i-1;
	sta = dbgetv(db,'sta');
	narr = dbeval( db, 'count()' );
	sprintf( '%s %d\n', sta, narr )
end

dbclose( db );

echo off
