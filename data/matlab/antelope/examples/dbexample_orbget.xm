echo on

% This presumes that you have connect permission to an orb running 
%  locally on the default port

fd = orbopen( 'localhost', 'r' );

orbreject( fd, '/db/.*|/pf/.*' );

[result, time, srcname, pktid, type] = orbget( fd )

result.record = 0;  

plot( trextract_data( result ) );

trdestroy( result );

echo off
