use Datascope;
use sysinfo;
use Cwd;

require "getopts.pl";

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

sub write_amakelocal {

	$output_file = pfget( $Pf, "output_file" );
	$dest = pfget( $Pf, "dest" );

	$dest_output_file = "$dest/$output_file";
	$temp_output_file = "/tmp/$output_file\_$$\_$>";

	if( -e "$dest_output_file" && ( -M "$dest_output_file" <= -M "$Pf_file" ) ) {

		return;

	} else {

		inform( "Rebuilding '$dest_output_file' since it is older than '$Pf_file'\n" );
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

	close( O );

	makedir( $dest );

	system( "/bin/cp $temp_output_file $dest_output_file" );

	inform( "Generated '$dest_output_file' from parameter-file '$Pf'\n" );

	unlink( $temp_output_file );

	return;
}

$Pf = "amakelocal";
$Pf_proto = "amakelocal_proto";

$localpf_dir = "$ENV{'ANTELOPE'}/local/data/pf";
$Pf_file = "$localpf_dir/$Pf.pf";
$Pf_proto_file = "$ENV{'ANTELOPE'}/data/pf/$Pf_proto.pf";

$Program = $0;
$Program =~ s@.*/@@;

elog_init( $Program, @ARGV );

if( ! Getopts( 'lv' ) ) {

	elog_die( "Usage: amakelocal [-lv] [capability [, capability...]]\n" );
}

@pfproto_files = pffiles( $Pf_proto );
@pf_files = pffiles( $Pf );

$pffiles_ok = 1;

while( $f = shift( @pfproto_files ) ) {

	$f = abspath( $f );

	if( $f ne $Pf_proto_file ) {
		
		elog_complain( "Please move or remove the file '$f'\n" );

		$pffiles_ok = 0;
	}
}

while( $f = shift( @pf_files ) ) {

	$f = abspath( $f );

	if( $f ne $Pf_file ) {
		
		elog_complain( "Please move or remove the file '$f'\n" );

		$pffiles_ok = 0;
	}
}

if( ! $pffiles_ok ) {

	elog_die( "$Program relies on\n\t'$ENV{'ANTELOPE'}/data/pf/$Pf_proto.pf' and\n\t" .
		  "'$ENV{'ANTELOPE'}/local/data/pf/$Pf.pf'\n" .
		  "exclusively. Other versions along PFPATH need to be removed. Exiting.\n" );
}

if( ! -e "$localpf_dir/$Pf.pf" ) {

	makedir( $localpf_dir );

	system( "cd $localpf_dir; pfcp $Pf_proto $Pf" );

	inform( "Copied '$Pf_proto.pf' to '$localpf_dir/$Pf.pf' since the latter didn't exist\n" );

	if( ! -e "$localpf_dir/$Pf.pf" ) {

		elog_die( "Failed to make '$localpf_dir/$Pf.pf'; Exiting.\n" );
	}
}

if( pfrequire( $Pf, pfget_time( $Pf_proto, "pf_revision_time" ) ) < 0 ) {
	
	system( "pfdiff $Pf_proto_file $Pf_file" );

	elog_die( "The pf_revision_time in '$Pf_file' predates that in '$Pf_proto_file'. Please check " .
		  "the former for added features and update it before proceeding. Exiting.\n" );
}

$Os = my_os();

%macros = %{pfget($Pf,"macros")}; 
$header = pfget( $Pf, "header" );
%capabilities = %{pfget( $Pf, "capabilities" )};

set_macros();

write_amakelocal();

format STDOUT = 
   @<<<<<<<<<<<<<<< @<<<<<<<<<<<< @*
   $enabled_string, $c, $capabilities{$c}{Description}
.

if( $opt_l ) {

	print "\nCapabilities are:\n\n";

	foreach $c ( keys( %capabilities ) ) {

		$enabled = pfget_boolean( $Pf, "capabilities{$c}{enable}" );
		
		$enabled_string = $enabled ? "[ enabled]" : "[disabled]";

		write;
	}

	print "\n";

	exit( 0 );
}

if( @ARGV >= 1 ) {

	$runmode = "verify";

	while( $r = shift( @ARGV ) ) {

		push( @requested, $r );
	}

	$enable_ok = 1;

	foreach $r ( @requested ) {

		if( ! defined( $capabilities{$r} ) ) {

			elog_complain( "Requested capability '$r' not defined in '$Pf'. " .
					"Preventing compilation.\n" );

			$enable_ok = 0;

		} elsif( ! pfget_boolean( $Pf, "capabilities{$r}{enable}" ) ) {

			elog_complain( "Requested capability '$r' marked as disabled in '$Pf'.\n" .
				"Run amakelocal(1) to enable and configure '$r' if desired.\n" );

			$enable_ok = 0;
		}
	}

	if( ! $enable_ok ) {

		exit( -1 );
	}

	foreach $r ( @requested ) {

		@required_macros = @{pfget( $Pf, "capabilities{$r}{required_macros}" )};
		@tests = @{pfget( $Pf, "capabilities{$r}{tests}" )};

		$test_ok = 1;

		while( $required_macro = shift( @required_macros ) ) {

			if( ! defined( $$required_macro ) || $$required_macro eq "" ) {
				
				elog_complain( "Macro '$required_macro', required for '$r' capability, " .
						"is not defined. Run amakelocal(1) to configure.\n" );
				
				$test_ok = 0;
			}
		}

		if( ! $test_ok ) {

			exit( -1 );
		}

		while( $test = shift( @tests ) ) {
			
			if( ! eval( $test ) ) {

				elog_complain( "Test failed for capability '$r': $failure_msg\n" );

				$test_ok = 0;
			}
		}

		if( ! $test_ok ) {

			exit( -1 );
		}

	}

} else {

	$runmode = "configure";
}

exit( 0 );
