display('Running dbexample_pfget')

echo on

pf = dbpf( 'rtexec' );

pfget( pf, 'Database' )       

pfget( pf, 'Failure_threshold' )       

echo off
