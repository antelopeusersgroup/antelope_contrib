use Datascope;
use sysinfo;

require "getopts.pl";

$Os = my_os();
$Pf = "localmake";

elog_init( $0, @ARGV );

if( !Getopts( 'p:v' ) || @ARGV > 0 ) {

	die( "Usage: localmake [-v] [-p pfname]\n" );
}

if( $opt_p ) {

	$Pf = $opt_p;
}

%elements = %{pfget($Pf,"elements")};
$output_file = pfget( $Pf, "output_file" );

open( O, ">$output_file" );

print O "# DO NOT MODIFY -- Automatically generated file -- DO NOT MODIFY\n\n";

foreach $element ( keys( %elements ) ) {
	
	print O "$element = $elements{$element}{$Os}\n";
}

close( O );

if( $opt_v ) {
	
	elog_notify( "Generated '$output_file' from parameter-file '$Pf'\n" );
}
