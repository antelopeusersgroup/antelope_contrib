echo on

% This presumes that you have connect permission to a running 
%  orb called 'nordic' (you probably don't...)

fd = orbopen( 'nordic', 'r' );

% Find the packet-id for the current packet:
orbtell( fd )

echo off
