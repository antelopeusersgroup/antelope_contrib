display('Running dbexample_epoch2str')

echo on

mytime = str2epoch( '9/30/02 11:15 AM' )

epoch2str( mytime, '%D %H:%M:%S %Z' )

epoch2str( mytime, '%A, %B %d %Y' )

epoch2str( mytime, '%G %l %p' )

echo off
