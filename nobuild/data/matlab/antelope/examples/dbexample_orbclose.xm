display('Running dbexample_orbclose')

echo on

% This presumes that you have connect permission to an orb running 
%  locally on the default port

fd = orbopen( 'localhost', 'r' );

orbclose( fd )

echo off
