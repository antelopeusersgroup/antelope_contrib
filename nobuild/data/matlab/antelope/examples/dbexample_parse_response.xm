display('Running dbexample_parse_response')

dbexample_get_demodb_path;

echo on

db = dbopen( demodb_path,'r' );

db=dblookup_table( db,'instrument' );

db.record=0;

file=dbfilename( db ); 

resp = dbresponse( file );

parsed = parse_response( resp )

% Display the results:
celldisp( parsed )

% Display some of the component vectors:
parsed{1}.poles
parsed{3}.num_coefs

dbclose( db );

echo off
