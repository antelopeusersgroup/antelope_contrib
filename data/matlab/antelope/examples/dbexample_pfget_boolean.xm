display('Running dbexample_pfget_boolean')

echo on

pf=dbpf( 'dbloc2' )

subpf=pfget( pf,'State' )    

pfget_boolean( subpf,'auto_save' )

pfget_boolean( subpf,'auto_associate' )

echo off
