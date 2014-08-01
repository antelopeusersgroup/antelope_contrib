display('Running dbexample_elog_clear')

echo on

elog_init( 'dbexample_elog_clear' )

elog_log( 'This is an Antelope log message' )

elog_clear()

echo off
