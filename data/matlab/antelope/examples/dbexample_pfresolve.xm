display('Running dbexample_pfresolve')

echo on

pf = dbpf( 'rtexec' );

pfresolve( pf,'Limit{filesize}' )

pfresolve( pf,'Limit{stacksize}' )

echo off
