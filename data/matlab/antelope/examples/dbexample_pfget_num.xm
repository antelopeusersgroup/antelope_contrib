display('Running dbexample_pfget_num')

echo on

pf = dbpf( 'rtexec' );

pfget_num( pf, 'Time_to_die' )

echo off
