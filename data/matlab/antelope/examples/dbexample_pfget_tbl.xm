display('Running dbexample_pfget_tbl')

echo on

pf = dbpf( 'rtexec' );

pfget_tbl( pf, 'Buttons' )            

echo off
