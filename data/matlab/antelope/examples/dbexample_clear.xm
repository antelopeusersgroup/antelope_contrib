echo on

pf = dbpf( 'dbloc2' );

clear( pf )


dbexample_get_demodb_path;
db = dbopen( demodb_path,'r' );
db=dblookup_table( db,'instrument' );
db.record=0;
file=dbfilename( db );
dbclose( db );

resp = dbresponse( file );

clear( resp )

echo off
