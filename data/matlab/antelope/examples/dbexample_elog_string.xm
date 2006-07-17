display('Running dbexample_elog_string')

echo on

elog_init( 'dbexample_elog_string' )

elog_log( 'This is an Antelope log message' )

logstring = elog_string( 0 )

echo off
