display('Running dbexample_pf2struct')

echo on

pf=dbpf( 'dbloc2' );
pf = pfget( pf, 'Define' );

pf2struct( pf )

pf2struct( pf,'recursive' )

echo off
