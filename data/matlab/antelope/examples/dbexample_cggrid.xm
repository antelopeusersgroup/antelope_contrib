display('Running dbexample_cggrid')

echo on

[X,Y] = meshgrid(-2:0.2:2,-3:0.3:3);

Z = exp( -X.^2 - Y.^2 );

cgg = cggrid( X, Y, Z )

clear( cgg );

echo off
