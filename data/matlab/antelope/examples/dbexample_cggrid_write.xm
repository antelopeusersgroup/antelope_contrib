echo on

[X,Y] = meshgrid(-2:0.2:2,-3:0.3:3);
Z = exp( -X.^2 - Y.^2 );
cgg = cggrid( X, Y, Z )

outfile = ['/tmp/mycggrid_' getenv('USER')];

cggrid_write( cgg, 't4', outfile );

clear( cgg );

unix( ['/bin/rm -f ' outfile] );

echo off
