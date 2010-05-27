use Datascope;
use sysinfo;
use Cwd;

require "getopts.pl";

sub inform {
	my( $msg ) = @_;

	if( $opt_v ) {

		elog_notify( $msg );
	}
}

$Pf = "amakelocal";
$Pf_proto = "amakelocal_proto";

$Program = $0;
$Program =~ s@.*/@@;

elog_init( $Program, @ARGV );

if( ! Getopts( 'lv' ) ) {

	elog_die( "Usage: amakelocal [-lv] [capability [, capability...]]\n" );
}

$localpf_dir = "$ENV{'ANTELOPE'}/local/data/pf";

if( ! -e "$localpf_dir/$Pf.pf" ) {

	makedir( $localpf_dir );

	system( "cd $localpf_dir; pfcp $Pf_proto $Pf" );

	inform( "Copied '$Pf_proto' to '$localpf_dir/$Pf' since the latter didn't exist\n" );
}

#SCAFFOLD warn if proto Pf is newer
#SCAFFOLD fill in antelopemake.local
#SCAFFOLD warn if non-approved amakelocal.pf parameter-files are on PFPATH

%capabilities = %{pfget( $Pf, "capabilities" )};

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

	$compile_ok = 1;

	while( $r = shift( @ARGV ) ) {

		push( @requested, $r );
	}

	foreach $r ( @requested ) {

		if( ! defined( $capabilities{$r} ) ) {

			elog_complain( "Requested capability '$r' not defined in '$Pf'. " .
					"Preventing compilation.\n" );

			$compile_ok = 0;

		} elsif( ! pfget_boolean( $Pf, "capabilities{$r}{enable}" ) ) {

			elog_complain( "Requested capability '$r' marked as disabled in '$Pf'. " .
				"Preventing compilation. Run amakelocal(1) to configure '$r' " .
				"for compilation if desired.\n" );

			$compile_ok = 0;
		}

		if( $compile_ok ) {

			exit( 0 );

		} else {

			exit( -1 );
		}
	}

} else {

	$runmode = "configure";
}

exit( 0 );
