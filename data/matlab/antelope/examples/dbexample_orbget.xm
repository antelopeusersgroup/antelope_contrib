echo on

% This presumes that you have connect permission to a running 
%  orb called 'nordic' (you probably don't...)

fd = orbopen( 'nordic', 'r' );

orbreject( fd, '/db/.*|/pf/.*' );

[result, time, srcname, pktid, type] = orbget( fd )

result.record = 0;  

plot( trextract_data( result ) );

trdestroy( result );

echo off
