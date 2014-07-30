display('Running dbexample_cggrid_probe')

if( ~ cggrid_supported )
        display('There is no cggrid support in your copy of Antelope')
        return
end

echo on

[X,Y] = meshgrid(-2:0.2:2,-3:0.3:3);
Z = exp( -X.^2 - Y.^2 );
cgg = cggrid( X, Y, Z );

% Probe the grid for the value at an (arbitrarily chosen) point:

a_value = cggrid_probe( cgg, 1.138, -2.045 )

clear( cgg );

echo off
