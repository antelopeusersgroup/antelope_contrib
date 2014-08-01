display('Running dbexample_orbafter')

echo on

% This presumes that you have connect permission to an orb running 
%  locally on the default port

fd = orbopen( 'localhost', 'r' );

[result,time, srcname, pktid] = orbget( fd );
pktid

% Get the next packet with timestamp after the packet we just got:
orbafter( fd, time )

echo off
