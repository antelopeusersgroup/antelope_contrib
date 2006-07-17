display('Running dbexample_elog_flush')

echo on

elog_init( 'dbexample_elog_flush' )

elog_log( 'This is an Antelope log message' )

elog_flush( 1, 0 )

echo off
