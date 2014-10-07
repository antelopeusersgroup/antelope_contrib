display('Running dbexample_zepoch2str')

echo on

mytime = str2epoch( '9/30/02 11:15 AM' )

zepoch2str( mytime, '%D %H:%M:%S %Z', 'US/Alaska' )

zepoch2str( mytime, '%A, %B %d %Y', 'US/Pacific' )

zepoch2str( mytime, '%G %l %p', 'GMT+2' )

echo off
