display('Running dbexample_dbtheta')

dbexample_get_demodb_path;

echo on

db = dbopen( demodb_path,'r' );

dbassoc = dblookup_table( db,'assoc' );

dbwfdisc = dblookup_table( db,'wfdisc' );

db=dbtheta( dbassoc,dbwfdisc,'assoc.sta == wfdisc.sta' )                             

dbclose( db );

echo off
