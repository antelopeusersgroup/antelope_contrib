echo on

% This presumes that you have connect permission to a running 
%  orb called 'nordic' (you probably don't...)

fd = orbopen( 'nordic', 'r' );

% Choose all components of station DIV (Divide, Alaska)
% Return the number of selected sources
orbselect( fd, 'AK_DIV_.*' )

echo off
