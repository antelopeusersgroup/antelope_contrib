echo on

% This presumes that you have connect permission to a running 
%  orb called 'nordic' (you probably don't...)

fd = orbopen( 'nordic', 'r' );

[result,time, srcname, pktid] = orbget( fd );
pktid

% Get the next packet with timestamp after the packet we just got:
orbafter( fd, time )

echo off
