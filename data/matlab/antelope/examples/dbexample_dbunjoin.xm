display('Running dbexample_dbunjoin')

dbexample_get_demodb_path;

echo on

db = dbopen( demodb_path,'r' );

dbarrival=dblookup_table( db,'arrival' );

dbwfdisc=dblookup_table( db,'wfdisc' );

db=dbjoin( dbarrival,dbwfdisc );

output_dbname = ['/tmp/newdb_' getenv('USER')]

unix( ['/bin/rm -f ' output_dbname '*'] );

dbunjoin( db,output_dbname );

unix( ['ls ' output_dbname '*'] );

dbclose( db );

echo off
