echo on

now = str2epoch( 'now' )

zepoch2str( now, '%D %H:%M:%S %Z', 'US/Alaska' )

zepoch2str( now, '%A, %B %d %Y', 'US/Pacific' )

zepoch2str( now, '%G %l %p', 'GMT+2' )

echo off
