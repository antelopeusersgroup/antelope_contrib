echo on

pf = dbpf( 'rtexec' );

pfget_arr( pf,'Limit' )

pfget_arr( pf,'Limit','recursive' ) 

echo off
