display('Running dbexample_parsepath')

echo on

[dir, base] = parsepath( '/home/kent/testfile.txt' )

[dir, base, suffix] = parsepath( '/home/kent/testfile.txt' )

echo off
