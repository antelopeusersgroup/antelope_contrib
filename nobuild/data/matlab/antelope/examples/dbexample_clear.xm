display('Running dbexample_clear')

echo on

% Example of pf/clear:

pf = dbpf( 'dbloc2' );

clear( pf )

% Example of dbresponse/clear:

dbexample_get_demodb_path;
db = dbopen( demodb_path,'r' );
db=dblookup_table( db,'instrument' );
db.record=0;
file=dbfilename( db );
dbclose( db );

resp = dbresponse( file );

clear( resp )

% Example of cggrid/clear:

if( ~ cggrid_supported )
	display('There is no cggrid support in your copy of Antelope')
	echo off
	return
end

[X,Y] = meshgrid(-2:0.2:2,-3:0.3:3);

Z = exp( -X.^2 - Y.^2 );

cgg = cggrid( X, Y, Z )

clear( cgg );

echo off
