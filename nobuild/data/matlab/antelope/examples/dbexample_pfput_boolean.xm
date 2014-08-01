display('Running dbexample_pfput_boolean')

echo on

pf = dbpf;

pfput_boolean( pf, 'myboolean', 'True' )

pf2string( pf )

echo off
