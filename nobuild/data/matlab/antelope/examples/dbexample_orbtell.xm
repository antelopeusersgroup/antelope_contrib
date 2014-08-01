display('Running dbexample_orbtell')

echo on

% This presumes that you have connect permission to an orb running 
%  locally on the default port

fd = orbopen( 'localhost', 'r' );

% Find the packet-id for the current packet:
orbtell( fd )

echo off
