echo on

% Create a parameter file and put one value in it
unix( 'rm /tmp/myfile.pf' );
unix( 'echo myint 13 > /tmp/myfile.pf' );

% Open the parameter file and extract the parameter:
pf = dbpf( '/tmp/myfile.pf' );
pfget_num( pf, 'myint' )

% Now change the parameter file from outside the Matlab context:
unix( 'echo myint 25 >! /tmp/myfile.pf' );

% a retrieval of the parameter returns the previously cached value:
pfget_num( pf, 'myint' )

% Updating the parameter-file object refreshes the cached values:
[pf, modified] = pfupdate( pf )

% Now the retrieved parameter reflects the changed file:
pfget_num( pf, 'myint' )

unix( 'rm /tmp/myfile.pf' );
pffree( pf );

echo off
