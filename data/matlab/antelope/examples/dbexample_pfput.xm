display('Running dbexample_pfput')

echo on

pf = dbpf;

pfput( pf, 'mydouble', 3.14 )
pfput( pf, 'myint', 24 )
pfput( pf, 'mystring', 'test string' )

z.a = 21;
z.b = 'hello';
z.c = '45.6';
pfput( pf, 'myarray', z )

pfput( pf, 'mytable', {'hello' 'yes' 'no' 'goodbye'} )

pfdbloc2 = dbpf( 'dbloc2' );
complex_pf_entry  = pfget( pfdbloc2, 'Define', 'recursive' );
pfput( pf, 'complexmess', complex_pf_entry )

pf2string( pf )

echo off
