display('Running dbexample_orbopen')

echo on

% This presumes that you have connect permission to an orb running 
%  locally on the default port

fd = orbopen( 'localhost', 'r' )

echo off
