display('Running dbexample_cggrid_getmesh')

% Start with a quick test to see if cggrid support is enabled:
try
	cggrid('/dev/null')
catch
	if( strcmp( lasterr, 'No cggrid support in your version of Antelope' ) )
		display('no cggrid support')
		return
	end
end

echo on

[X,Y] = meshgrid(-2:0.2:2,-3:0.3:3);
Z = exp( -X.^2 - Y.^2 );
cgg = cggrid( X, Y, Z )

[myx, myy, myz] = cggrid_getmesh( cgg );

mesh( myx, myy, myz )

% (Allow time for figure to come up when running in batch mode)
pause(0.5);

clear( cgg );

echo off
