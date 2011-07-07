use Datascope;
use sysinfo;
use Cwd;

use Getopt::Std;

$Pf = "amakelocal";
$Pf_proto = "amakelocal_proto";

$Program = $0;
$Program =~ s@.*/@@;

elog_init( $Program, @ARGV );

if( !getopts( 'icp:s:v' ) ) {

	elog_die( "Usage: amakelocal [-i] [-v] [-c] [-p pfname] [MACRO [MACRO ...]]\n" );
}

if( @ARGV >= 1 || $opt_c ) {

	$runmode = "verify";

} else {

	$runmode = "configure";
}

if( $opt_p ) {

	$Pf_proto = $Pf = $opt_p;

	if( ! pffiles( $Pf ) ) {

		elog_die( "Couldn't find specified '$Pf.pf'. Bye.\n" );
	}
}

if( pffiles( $Pf ) ) {

	if( $opt_v && $runmode eq "configure" ) {
		
		elog_notify( "Using parameter-file '$Pf.pf'\n" );
	}

} else {

	if( $opt_v && $runmode eq "configure" ) {
		
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

if( $runmode eq "configure" ) {

	%macros = %{pfget($Pf,"macros")}; 
	$header = pfget( $Pf, "header" );

	open( O, ">$output_file" );

	print O "$header\n\n";

	foreach $macro ( keys( %macros ) ) {
		
		if( ! defined( $macros{$macro} ) ) {

			next;

		} else {
			
			$contents = $macros{$macro};
		}

		if( ref( $contents ) eq "HASH" ) {
		
			if( defined( $contents->{$Os} ) && 
			    $contents->{$Os} ne "" ) {

				print O "$macro = $contents->{$Os}\n";
			}

		} else {

			print O "$macro = $contents\n";
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

		makedir( $dest );

		system( "deposit $output_file $dest" );

		unless( cwd() eq "$dest" ) {

			unlink( $output_file );
		}
	}

	exit( 0 );
}

if( $runmode eq "verify" ) {

	if( ! -e "$dest/$output_file" ) {

		$exitcode = 1;

		if( $opt_c ) { 

			elog_complain( 
		   	"\n\n\t***********\n\n" .
		   	"\tRequired macro(s) '" . join( ",", @ARGV ) . "' not found because " .
		   	"\n\tlocal configuration file\n\t'$dest/$output_file'" .
		   	"\n\tdoes not exist.\n" .
		   	"\n\tUse amakelocal(1) to configure your local system" .
		   	"\n\tso Antelope-contrib code will link properly to software" .
		   	"\n\texternal to Antelope.\n" .
		   	"\n\t***********\n\n" );

		} else {

			elog_complain( 
		   	"\n\n\t***********\n\n" .
		   	"\tRequired macro(s) '" . join( ",", @ARGV ) . "' not found because " .
		   	"\n\tlocal configuration file\n\t'$dest/$output_file'" .
		   	"\n\tdoes not exist.\n\n\tCancelling " .
		   	"compilation in current subdirectory\n\t'" . cwd() . "'\n" .
		   	"\n\tUse amakelocal(1) to configure your local system" .
		   	"\n\tso code in this directory will link properly to software" .
		   	"\n\texternal to Antelope.\n" .
		   	"\n\t***********\n\n" );
		}

		exit( $exitcode );
	}

	open( A, "$dest/$output_file" );

	@antelopemake_local = <A>;

	close( A );

	$exitcode = 0;

	if( $opt_c && @ARGV <= 0 ) {

		grep( s/\#.*//, @antelopemake_local );
		grep( /^\s*$/ || print, @antelopemake_local );

		exit( $exitcode );
	}

	foreach $macro ( @ARGV ) {
	
		$result = grep( m/^$macro\s*=\s*(.*)/ && ($match = $1), @antelopemake_local );

		if( ! $result ) {

			$exitcode = 1;

			if( $opt_c ) { 

				elog_complain( "Macro '$macro' is undefined\n" );

			} else {

				elog_complain( 
			   	"\n\n\t***********\n\n" .
			   	"\tRequired macro '$macro' is undefined.\n\n\tCancelling " .
			   	"compilation in current subdirectory\n\t'" . cwd() . "'\n" .
			   	"\n\tUse amakelocal(1) to configure your local system" .
			   	"\n\tso code in this directory will link properly to software" .
			   	"\n\texternal to Antelope.\n" .
			   	"\n\t***********\n\n" );
			}

		} else {
			
			if( $opt_c ) {
				
				elog_notify( "$macro = $match\n" );

			} elsif( $opt_v ) {

				elog_complain( "Found required macro '$macro = $match'\n" );
			}
		}
	}

	exit( $exitcode );
}
