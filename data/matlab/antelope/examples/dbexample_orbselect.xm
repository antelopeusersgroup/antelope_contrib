display('Running dbexample_orbselect')

echo on

% This presumes that you have connect permission to an orb running 
%  locally on the default port

fd = orbopen( 'localhost', 'r' );

% Choose all components of station DIV (Divide, Alaska)
% Return the number of selected sources
orbselect( fd, 'AK_DIV_.*' )

echo off
