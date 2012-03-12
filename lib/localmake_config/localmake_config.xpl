use Datascope;
use sysinfo;
use Cwd;

use Getopt::Std;

sub inform {
	my( $msg ) = @_;

	if( $opt_v ) {

		elog_notify( $msg );
	}

	return;
}

sub set_macros {

	foreach $macro ( keys( %macros ) ) {
		
		if( ! defined( $macros{$macro} ) ) {

			next;

		} else {
			
			$contents = $macros{$macro};
		}

		if( ref( $contents ) eq "HASH" ) {
		
			if( defined( $contents->{$Os} ) && 
			    $contents->{$Os} ne "" ) {

				$$macro = "$contents->{$Os}";
			}
		}
	}
}

sub set_initial_config {
	 
	foreach $macro ( keys( %macros_initial_config ) ) {
		
		if( ! defined( $macros{$macro} ) ) {

			elog_complain( "File '$Pf_config_file' refers to decommissioned macro '$macro'\n" );

			next;

		} else {

			$macros{$macro}->{$Os} = $macros_initial_config{$macro};
		}
	}

	foreach $capability ( keys( %capabilities_initial_config ) ) {

		if( ! defined( $capabilities{$capability} ) ) {

			elog_complain( "File '$Pf_config_file' refers to decommissioned capability '$capability'\n" );

			next;

		} else {

			$capabilities{$capability}{enable}{$Os} = $capabilities_initial_config{$capability};
		}
	}

	return;
}

sub show_capabilities {

format STDOUT = 
   @<<<<<<<<<<< @<<<<<<<<<<<<<<<<<<< @*
   $enabled_string, $c, $capabilities{$c}{Description}
.

	print "\nCapabilities are:\n\n";

	foreach $c ( keys( %capabilities ) ) {

		$enabled = $capabilities{$c}{enable}{$Os};
	
		$enabled_string = $enabled ? "[ enabled]" : "[disabled]";

		write;
	}

	print "\n";
}

sub write_makerules {

	$output_file = pfget( $Pf_localmake, "output_file" );
	$dest = pfget( $Pf_localmake, "dest" );

	$dest_output_file = "$dest/$output_file";
	$temp_output_file = "/tmp/$output_file\_$$\_$>";

	if( -e "$dest_output_file" && ( -M "$dest_output_file" <= -M "$Pf_config_file" ) ) {

		return;

	} else {

		inform( "Rebuilding '$dest_output_file' since it is older than '$Pf_config_file'\n" );
	}

	open( O, ">$temp_output_file" );

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

				$$macro = "$contents->{$Os}";
			}

		} else {

			print O "$macro = $contents\n";
		}
	}

	print O "\n$extra_rules\n";

	close( O );

	makedir( $dest );

	system( "/bin/cp $temp_output_file $dest_output_file" );

	inform( "Generated '$dest_output_file' from parameter-file '$Pf_config'\n" );

	unlink( $temp_output_file );

	return;
}

sub set_orig_enabled {

	foreach $capability ( keys( %capabilities ) ) {

		$orig_enabled{$capability} = $capabilities{$capability}{enable}{$Os};
	}
}


sub test_capability {
	if( ref( $_[0] ) ) { shift( @_ ); }
	my( $c, $mode ) = @_;
	
	my( $passed ) = 1;

	if( ! defined( $capabilities{$c} ) ) {

		if( $mode eq "verify" ) {

			elog_complain( "Requested capability '$c' not defined in '$Pf_config'. " .
					"Stopping compilation.\n" );

			exit( -1 );
		}
	}

	if( ! $capabilities{$c}{enable}{$Os} && $mode eq "verify" ) {

		elog_complain( "Requested capability '$c' marked as disabled in '$Pf_config'.\n" .
			"Run localmake_config(1) (or edit '$Pf_config_file')\nto enable and configure " .
			"'$c' if desired.\n" );

		exit( -1 );

	}

	@required_macros = @{pfget( $Pf_config, "capabilities{$c}{required_macros}" )};
	@tests = @{pfget( $Pf_config, "capabilities{$c}{tests}" )};

	while( $required_macro = shift( @required_macros ) ) {

		if( ! defined( $$required_macro ) || $$required_macro eq "" ) {
				
			if( $mode eq "verify" ) {

				elog_complain( "Macro '$required_macro', required for '$c' capability, " .
						"is not defined.\nRun localmake_config(1) (or edit '$Pf_config_file')\n" .
						"to configure.\n" );

				exit( -1 );
			}
		}
	}

	while( $test = shift( @tests ) ) {
			
		if( ! eval( $test ) ) {

			if( $mode eq "verify" ) {

				elog_complain( "Test failed for capability '$c': $failure_msg\n" );

				exit( -1 );
			}
		}
	}

	return;
}

sub commit_configuration {

	%config_macros = ();
	%config_capabilities = ();

	foreach $macro ( keys( %macros ) ) {

		$config_macros{$macro} = $$macro;
	}

	foreach $capability ( keys( %capabilities ) ) {

		$config_capabilities{$capability} = 0;
	}

	pfput( "macros", \%config_macros, $Pf_config );
	pfput( "capabilities", \%config_capabilities, $Pf_config );

	pfwrite( $Pf_config_file, $Pf_config );

	write_makerules();

	%macros_orig = %macros;

	set_orig_enabled();

	return;
}

sub run_verify {

	while( $r = shift( @ARGV ) ) {

		push( @requested, $r );
	}

	$enable_ok = 1;

	foreach $r ( @requested ) {

		test_capability( $r, "verify" );
	}

	return;
}

sub run_configure {
	
	system( "localmake -c" );
}

$Pf_config = "localmake_config";
$Pf_localmake = "localmake";

$localpf_dir = "$ENV{'ANTELOPE'}/local/data/pf";

$ENV{'PFPATH'} = "$localpf_dir:$ENV{'PFPATH'}";

$Pf_config_file = "$localpf_dir/$Pf_config.pf";

$Program = $0;
$Program =~ s@.*/@@;

elog_init( $Program, @ARGV );

if( ! getopts( 'ilv' ) ) {

	elog_die( "Usage: localmake_config [-ilv] [capability [, capability...]]\n" );
}

if( @ARGV >= 1 ) {

	$mode = "verify";

} else {

	$mode = "configure";
}

$Os = my_os();

%macros = %{pfget($Pf_localmake,"macros")}; 
$header = pfget( $Pf_localmake, "header" );
$extra_rules = pfget( $Pf_localmake, "extra_rules" );
%capabilities = %{pfget( $Pf_localmake, "capabilities" )};

if( ! -e "$localpf_dir/$Pf_config.pf" ) {

	makedir( $localpf_dir );

	commit_configuration();

	if( ! -e "$localpf_dir/$Pf_config.pf" ) {

		elog_die( "Failed to make '$localpf_dir/$Pf_config.pf'; Exiting.\n" );
	}

} else {

	%macros_initial_config = %{pfget($Pf_config,"macros")};
	%capabilities_initial_config = %{pfget($Pf_config,"capabilities")};

	set_initial_config();
}

%macros_orig = %macros;

set_orig_enabled();

set_macros();

write_makerules();

if( $opt_i ) {

	exit( 0 );
}

if( $opt_l ) {

	show_capabilities();

	exit( 0 );
}

if( $mode eq "verify" ) {

	run_verify();

} else {

	run_configure();
}

exit( 0 );
