display('Running dbexample_arrtimes')

echo on

delta = 20;
depth = 10;

[times, phasenames] = arrtimes( delta, depth );

[char(strtdelta(times)) char(phasenames)]

echo off
