use Datascope;
use sysinfo;

require "getopts.pl";

$Os = my_os();
$Pf = "localmake";

$Program = $0;
$Program =~ s@.*/@@;

elog_init( $Program, @ARGV );

if( !Getopts( 'p:v' ) || @ARGV > 1 ) {

	elog_die( "Usage: localmake [-v] [-p pfname] [dbxcor]\n" );
}

if( $opt_p ) {

	$Pf = $opt_p;
}

%elements = %{pfget($Pf,"elements")};
%modules = %{pfget($Pf,"modules")};
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

if( @ARGV > 0 ) {
	
	$module = pop( @ARGV );

	@steps = @{$modules{$module}};

	if( @steps <= 0 ) {
	
		elog_die( "No steps listed for module '$module' in parameter-file '$Pf'\n" );

	} elsif( $opt_v ) {

		elog_notify( "Making module '$module'\n" );
	}
	
	foreach $step ( @steps ) {
		
		$dir = "$ENV{ANTELOPE}/$step";

		if( $opt_v ) {
			
			elog_notify( "Changing directory to '$dir'\n" );
		}

		$rc = chdir( $dir );

		if( ! $rc ) {

			elog_die( "Couldn't change directory to '$dir'\n" );
		}

		$cmd = "make clean 2>&1 | cf";

		if( $opt_v ) {
			
			elog_notify( "Executing '$cmd'\n" );
		}

		$rc = system( $cmd );

		if( $rc != 0 ) {

			elog_die( "Command 'make clean' failed in directory '$dir'\n" );
		}

		$cmd = "make install 2>&1 | cf";

		if( $opt_v ) {
			
			elog_notify( "Executing '$cmd'\n" );
		}

		$rc = system( $cmd );

		if( $rc != 0 ) {

			elog_die( "Command 'make install' failed in directory '$dir'\n" );
		}
	}

	if( $opt_v ) {
			
		elog_notify( "Done making module '$module', apparently successfully" );
	}
}
