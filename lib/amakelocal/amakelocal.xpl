use Datascope;
use sysinfo;
use Cwd;

require "getopts.pl";

$Pf = "amakelocal";
$Pf_proto = "amakelocal_proto";

$Program = $0;
$Program =~ s@.*/@@;

elog_init( $Program, @ARGV );

if( !Getopts( 'ip:s:v' ) ) {

	elog_die( "Usage: amakelocal [-i] [-v] [-p pfname] [MACRO [MACRO ...]]\n" );
}

if( @ARGV >= 1 ) {

	$runmode = "verify";

} else {

	$runmode = "construct";
}

if( $opt_p ) {

	$Pf_proto = $Pf = $opt_p;

	if( ! pffiles( $Pf ) ) {

		elog_die( "Couldn't find specified '$Pf.pf'. Bye.\n" );
	}
}

if( pffiles( $Pf ) ) {

	if( $opt_v && $runmode eq "construct" ) {
		
		elog_notify( "Using parameter-file '$Pf.pf'\n" );
	}

} else {

	if( $opt_v && $runmode eq "construct" ) {
		
		elog_notify( "Couldn't find '$Pf.pf'; Using parameter-file '$Pf_proto.pf'\n" );
	}

	$Pf = $Pf_proto;
}

if( $opt_s ) {

	$Os = $opt_s;

} else {

	$Os = my_os();
}

$output_file = pfget( $Pf, "output_file" );
$dest = pfget( $Pf, "dest" );

if( $runmode eq "construct" ) {

	%elements = %{pfget($Pf,"elements")}; 
	$header = pfget( $Pf, "header" );

	open( O, ">$output_file" );

	print O "$header\n\n";

	foreach $element ( keys( %elements ) ) {
		
		if( ! defined( $elements{$element} ) ) {

			next;

		} else {
			
			$contents = $elements{$element};
		}

		if( ref( $contents ) eq "HASH" ) {
		
			if( defined( $contents->{$Os} ) && 
			    $contents->{$Os} ne "" ) {

				print O "$element = $contents->{$Os}\n";
			}

		} else {

			print O "$element = $contents\n";
		}
	}

	close( O );

	if( $opt_v ) {
		
		elog_notify( "Generated '$output_file' from parameter-file '$Pf'\n" );
	}

	if( $opt_i ) {

		if( $opt_v ) {

			elog_notify( "Installing '$output_file' in $dest" );
		} 

		system( "deposit $output_file $dest" );

		unless( cwd() eq "$dest" ) {

			unlink( $output_file );
		}
	}

	exit( 0 );
}

if( $runmode eq "verify" ) {

	open( A, "$dest/$output_file" );

	@antelopemake = <A>;

	close( A );

	$exitcode = 0;

	foreach $macro ( @ARGV ) {
	
		if( ! grep( m/^$macro\s*=/, @antelopemake ) ) {

			$exitcode = 1;

			elog_complain( 
			   "\n\n\t***********\n\n" .
			   "\tRequired macro '$macro' is undefined.\n\n\tCancelling " .
			   "compilation in current subdirectory\n\t'" . cwd() . "'\n" .
			   "\n\tUse amakelocal(1) to configure your local system.\n" .
			   "\n\t***********\n\n" );

		} else {

			if( $opt_v ) {

				elog_complain( "Required macro '$macro' is defined, continuing.\n" );
			}
		}
	}

	exit( $exitcode );
}
