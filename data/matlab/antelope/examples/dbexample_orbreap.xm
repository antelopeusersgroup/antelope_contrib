echo on

% This presumes that you have connect permission to a running 
%  orb called 'nordic' (you probably don't...)

fd = orbopen( 'nordic', 'r' );

for i = 1:3,
[result,time,srcname] = orbreap( fd );
srcname
end

echo off
