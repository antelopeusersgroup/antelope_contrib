display('Running dbexample_cggrid_getmesh')

if( ~ cggrid_supported )
        display('There is no cggrid support in your copy of Antelope')
        return
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
