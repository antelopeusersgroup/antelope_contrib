display('Running dbexample_elog_complain')

echo on

elog_init( 'dbexample_elog_complain' )

elog_complain( 'This is an Antelope complain message' )

echo off
