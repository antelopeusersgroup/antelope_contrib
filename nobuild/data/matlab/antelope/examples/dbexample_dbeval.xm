display('Running dbexample_dbeval')

dbexample_get_demodb_path;

echo on

db = dbopen( demodb_path,'r' );

db=dblookup_table( db,'origin' );

dbs=dblookup_table( db,'site' );

db.record=0;

db=dbjoin( db,dbs );

db.record=0;

dbeval( db,'arrival( "PKiKP" )-time' )

dbeval( db,'distance( site.lat,site.lon,origin.lat,origin.lon )' )

dbclose( db );

echo off
