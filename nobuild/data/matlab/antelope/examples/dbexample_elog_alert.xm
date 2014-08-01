display('Running dbexample_elog_alert')

echo on

elog_init( 'dbexample_elog_alert' )

elog_alert( 'This is an Antelope alert message' )

echo off
