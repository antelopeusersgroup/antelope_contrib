display('Running dbexample_orbget')

echo on

% This presumes that you have connect permission to an orb running 
%  locally on the default port

fd = orbopen( 'localhost', 'r' );

orbreject( fd, '/db/.*|/pf/.*' );

[result, time, srcname, pktid, type] = orbget( fd )

result.record = 0;  

subplot( 1, 1, 1);
plot( trextract_data( result ) );

% (Allow time for figure to come up when running in batch mode)
pause(0.5);

trdestroy( result );

echo off
