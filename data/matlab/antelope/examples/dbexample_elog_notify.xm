display('Running dbexample_elog_notify')

echo on

elog_init( 'dbexample_elog_notify' )

elog_notify( 'This is an Antelope notify message' )

echo off
