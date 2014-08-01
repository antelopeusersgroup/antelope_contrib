display('Running dbexample_cggrid_write')

if( ~ cggrid_supported )
        display('There is no cggrid support in your copy of Antelope')
        return
end

echo on

[X,Y] = meshgrid(-2:0.2:2,-3:0.3:3);
Z = exp( -X.^2 - Y.^2 );
cgg = cggrid( X, Y, Z )

outfile = dbexample_get_tempname( 'mycggrid', 'file' );

cggrid_write( cgg, 't4', outfile );

clear( cgg );

unix( ['/bin/rm -f ' outfile] );

echo off
