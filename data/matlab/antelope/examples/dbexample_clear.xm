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

% Start with a quick test to see if cggrid support is enabled:
try
	cggrid('/dev/null')
catch
	if( strcmp( lasterr, 'No cggrid support in your version of Antelope' ) )
		display('no cggrid support')
		return
	end
end

[X,Y] = meshgrid(-2:0.2:2,-3:0.3:3);

Z = exp( -X.^2 - Y.^2 );

cgg = cggrid( X, Y, Z )

clear( cgg );

echo off
