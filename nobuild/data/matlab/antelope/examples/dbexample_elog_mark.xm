display('Running dbexample_elog_mark')

echo on

elog_init( 'dbexample_elog_mark' )

elog_log( 'This is an Antelope log message' )

n = elog_mark()

echo off
