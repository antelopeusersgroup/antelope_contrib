display('Running dbexample_dbjoin')

dbexample_get_demodb_path;

echo on

db = dbopen( demodb_path,'r' );

dbarrival=dblookup_table( db,'arrival' );

dbwfdisc=dblookup_table( db,'wfdisc' );

% Make a database pointer that points to a view, in this case 
%  to a join of the arrival and wfdisc tables
% N.B. In this mode the join keys will be inferred from the schema. 
%  the join keys may also be explicitly specified.

db=dbjoin( dbarrival,dbwfdisc )

dbclose( db );

echo off
