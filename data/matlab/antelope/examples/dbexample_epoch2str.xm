echo on

now = str2epoch( 'now' )

epoch2str( now, '%D %H:%M:%S %Z' )

epoch2str( now, '%A, %B %d %Y' )

epoch2str( now, '%G %l %p' )

echo off
