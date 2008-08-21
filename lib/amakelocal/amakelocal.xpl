use Datascope;
use sysinfo;
use Cwd;

require "getopts.pl";

$Os = my_os();
$Pf = "amakelocal";
$Pf_proto = "amakelocal_proto";

$Program = $0;
$Program =~ s@.*/@@;

elog_init( $Program, @ARGV );

if( !Getopts( 'ip:v' ) || @ARGV > 1 ) {

	elog_die( "Usage: amakelocal [-i] [-v] [-p pfname]\n" );
}

if( $opt_p ) {

	$Pf = $opt_p;
}

if( pffiles( $Pf ) ) {

	if( $opt_v ) {
		
		elog_notify( "Using parameter-file '$Pf.pf'\n" );
	}

} else {

	if( $opt_v ) {
		
		elog_notify( "Couldn't find '$Pf.pf'; Using parameter-file '$Pf_proto.pf'\n" );
	}

	$Pf = $Pf_proto;
}

%elements = %{pfget($Pf,"elements")}; 
$output_file = pfget( $Pf, "output_file" );
$header = pfget( $Pf, "header" );

open( O, ">$output_file" );

print O "$header\n\n";

foreach $element ( keys( %elements ) ) {
	
	print O "$element = $elements{$element}{$Os}\n";
}

close( O );

if( $opt_v ) {
	
	elog_notify( "Generated '$output_file' from parameter-file '$Pf'\n" );
}

if( $opt_i ) {

	if( $opt_v ) {

		elog_notify( "Installing '$output_file' in $ENV{ANTELOPE}/include" );
	} 

	system( "deposit $output_file $ENV{ANTELOPE}/include" );

	unless( cwd() eq "$ENV{ANTELOPE}/include" ) {

		unlink( $output_file );
	}
}

exit( 0 );
