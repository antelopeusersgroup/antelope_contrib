echo on

% This presumes that you have connect permission to a running 
%  orb called 'nordic' (you probably don't...)

fd = orbopen( 'nordic', 'r' );

orbseek( fd, 'ORBOLDEST' )

orbseek( fd, 'ORBNEXT' )

orbseek( fd, 'ORBNEWEST' )

orbseek( fd, 'ORBPREV' )

orbseek( fd, 'ORBPREV' )
mypktid = orbtell( fd )

orbseek( fd, 'ORBPREV' )

orbseek( fd, mypktid )

echo off
