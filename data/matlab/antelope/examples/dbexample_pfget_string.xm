display('Running dbexample_pfget_string')

echo on

pf = dbpf( 'rtexec' );

pfget_string( pf, 'Database' )

echo off
