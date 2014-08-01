display('Running dbexample_arr_slowness')

echo on

delta = 20;
depth = 10;

[slowness, phasenames] = arr_slowness( delta, depth );

space(1:length(slowness),1) = ' ';

[num2str(slowness) space char(phasenames)]

echo off
