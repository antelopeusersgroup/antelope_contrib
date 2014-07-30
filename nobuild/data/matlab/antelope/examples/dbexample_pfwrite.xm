display('Running dbexample_pfwrite')

echo on

pf = dbpf( 'rtexec' );

output_pffile = dbexample_get_tempname( 'rtexec_copy', 'pf' );

pfwrite( pf, output_pffile )

unix( ['/bin/rm -f ' output_pffile] );

echo off
