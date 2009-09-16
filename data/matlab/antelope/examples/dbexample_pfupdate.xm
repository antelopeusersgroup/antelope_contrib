display('Running dbexample_pfupdate')

echo on

% Create a parameter file and put one value in it
output_pffile = dbexample_get_tempname( 'myfile', 'pf' );
unix( ['/bin/rm ' output_pffile] );
unix( ['echo myint 13 > ' output_pffile] );

% Open the parameter file and extract the parameter:
pf = dbpf( output_pffile );
pfget_num( pf, 'myint' )

% Now change the parameter file from outside the Matlab context:
unix( ['echo myint 25 >! ' output_pffile] );

% a retrieval of the parameter returns the previously cached value:
pfget_num( pf, 'myint' )

% Updating the parameter-file object refreshes the cached values:
[pf, modified] = pfupdate( pf )

% Now the retrieved parameter reflects the changed file:
pfget_num( pf, 'myint' )

unix( ['/bin/rm ' output_pffile] );
pffree( pf );

echo off
