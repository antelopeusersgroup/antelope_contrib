display('Running dbexample_orbping')

echo on

% This presumes that you have connect permission to an orb running 
%  locally on the default port

fd = orbopen( 'localhost', 'r' );

orbping( fd )

echo off
