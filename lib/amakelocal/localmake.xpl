use Datascope;
use sysinfo;

$Os = my_os();
$Pf = "localmake";

%elements = %{pfget($Pf,"elements")};
$output_file = pfget( $Pf, "output_file" );

open( O, ">$output_file" );

print O "# DO NOT MODIFY -- Automatically generated file -- DO NOT MODIFY\n\n";

foreach $element ( keys( %elements ) ) {
	
	print O "$element = $elements{$element}{$Os}\n";
}

close( O );
