display('Running dbexample_cggrid_probe')

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
cgg = cggrid( X, Y, Z );

% Probe the grid for the value at an (arbitrarily chosen) point:

a_value = cggrid_probe( cgg, 1.138, -2.045 )

clear( cgg );

echo off
