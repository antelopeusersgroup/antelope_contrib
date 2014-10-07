display('Running dbexample_cggrid_get')

if( ~ cggrid_supported )
        display('There is no cggrid support in your copy of Antelope')
        return
end

echo on

[X,Y] = meshgrid(-2:0.2:2,-3:0.3:3);
Z = exp( -X.^2 - Y.^2 );
cgg = cggrid( X, Y, Z )

[mytriplets, nx, ny] = cggrid_get( cgg );

whos mytriplets
nx
ny

clear( cgg );

echo off
