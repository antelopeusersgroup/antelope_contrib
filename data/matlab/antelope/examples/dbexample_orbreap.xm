display('Running dbexample_orbreap')

echo on

% This presumes that you have connect permission to an orb running 
%  locally on the default port

fd = orbopen( 'localhost', 'r' );

for i = 1:3,
[result,time,srcname] = orbreap( fd );
srcname
end

echo off
