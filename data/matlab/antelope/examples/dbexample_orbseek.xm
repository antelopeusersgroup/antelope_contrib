display('Running dbexample_orbseek')

echo on

% This presumes that you have connect permission to an orb running 
%  locally on the default port

fd = orbopen( 'localhost', 'r' );

orbseek( fd, 'ORBOLDEST' )

orbseek( fd, 'ORBNEXT' )

orbseek( fd, 'ORBNEWEST' )

orbseek( fd, 'ORBPREV' )

orbseek( fd, 'ORBPREV' )
mypktid = orbtell( fd )

orbseek( fd, 'ORBPREV' )

orbseek( fd, mypktid )

echo off
