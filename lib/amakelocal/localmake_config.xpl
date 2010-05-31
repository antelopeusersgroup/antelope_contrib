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

sub show_capabilities {

format STDOUT = 
   @<<<<<<<<<<<<<<< @<<<<<<<<<<<< @*
   $enabled_string, $c, $capabilities{$c}{Description}
.

	print "\nCapabilities are:\n\n";

	foreach $c ( keys( %capabilities ) ) {

		$enabled = pfget_boolean( $Pf, "capabilities{$c}{enable}" );
	
		$enabled_string = $enabled ? "[ enabled]" : "[disabled]";

		write;
	}

	print "\n";
}

sub write_makerules {

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

	print O "\n$extra_rules\n";

	close( O );

	makedir( $dest );

	system( "/bin/cp $temp_output_file $dest_output_file" );

	inform( "Generated '$dest_output_file' from parameter-file '$Pf'\n" );

	unlink( $temp_output_file );

	return;
}

sub commit_configuration {

	foreach $macro ( keys( %macros ) ) {

		$macros{$macro}{$Os} = $$macro;
	}

	pfput( "macros", \%macros, $Pf );
	pfput( "capabilities", \%capabilities, $Pf );

	pfwrite( $Pf_file, $Pf );

	write_makerules();

	return;
}

sub test_capability {
	my( $c, $mode ) = @_;

	if( ! defined( $capabilities{$r} ) ) {

		if( $mode eq "verify" ) {

			elog_complain( "Requested capability '$r' not defined in '$Pf'. " .
					"Stopping compilation.\n" );

			exit( -1 );
		}
	}

	if( ! pfget_boolean( $Pf, "capabilities{$r}{enable}" ) ) {

		if( $mode eq "verify" ) {

			elog_complain( "Requested capability '$r' marked as disabled in '$Pf'.\n" .
				"Run localmake_config(1) (or edit '$Pf_file')\nto enable and configure " .
				"'$r' if desired.\n" );

			exit( -1 );
		}
	}

	@required_macros = @{pfget( $Pf, "capabilities{$r}{required_macros}" )};
	@tests = @{pfget( $Pf, "capabilities{$r}{tests}" )};

	while( $required_macro = shift( @required_macros ) ) {

		if( ! defined( $$required_macro ) || $$required_macro eq "" ) {
				
			if( $mode eq "verify" ) {

				elog_complain( "Macro '$required_macro', required for '$r' capability, " .
						"is not defined.\nRun localmake_config(1) (or edit '$Pf_file')\n" .
						"to configure.\n" );

				exit( -1 );
			}
		}
	}

	while( $test = shift( @tests ) ) {
			
		if( ! eval( $test ) ) {

			if( $mode eq "verify" ) {

				elog_complain( "Test failed for capability '$r': $failure_msg\n" );

				exit( -1 );
			}
		}
	}

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

sub init_File_menu {
	my( $w ) = @_;

	my( $menubutton, $filemenu );

	$menubutton = $w->Menubutton (
			    -text => 'File',
			    -pady => 0, 
			    -anchor => 'w', 
			    )->pack( -side => "left" );

	$filemenu = $menubutton->Menu( -tearoff => 0 );

	$filemenu->add( "command", -label => "Quit", -command => \&exit );

	$menubutton->configure( -menu => $filemenu );

	return;
}

sub init_menubar {
	my( $w ) = @_;

	my( $menubar );

	$menubar = $w->Frame( -relief => 'raised', 
			      -borderwidth => 2 );

	init_File_menu( $menubar );

	return $menubar;
}

sub init_capabilities {
	my( $w ) = @_;

	my( $capabilities_window );
	my( @specs, $i );

	$capabilities_window = $w->Frame( -relief => 'raised', 
					  -borderwidth => 2 );

	push( @specs, "notebook capabilities - 0,0 Capabilities" );

	foreach $c ( keys( %capabilities ) ) {

		push( @specs, "notebookpage $c - xxx $c" );
		push( @specs, "label np$c - 0,0:3 Capability:" );
		push( @specs, "label en$c - +,0 Status:" );
		push( @specs, "button b$c - =,1 Toggle" );

		foreach $m ( @{$capabilities{$c}{required_macros}} ) {

			push( @specs, "entry e$c$m 80 +,0:2 $m { $macros{$m}{Description} }" );
			push( @specs, "button b$c$m - =,2 Explain" );
		}

		push( @specs, "rotext t$c - +,0:3 Tests:" );
	}

	push( @specs, "endnotebook" );

	ptkform( $capabilities_window, \%Var, \%Widgets, @specs );

	foreach $c ( keys( %capabilities ) ) {

		$Var{"np$c"} = "$capabilities{$c}{Description}"; 

		$Widgets{"t$c"}->insert( "end", "SCAFFOLD pretend to test $c" );

		$Widgets{"b$c"}->configure( -command => [\&toggle_capability, $c] );

		if( pfget_boolean( $Pf, "capabilities{$c}{enable}" ) ) {

			$capabilities{$c}{enable} = 1;

			$Var{"en$c"} = "Capability '$c' is enabled";

			$Widgets{"b$c"}->configure( -text => "Disable $c" );

		} else {

			$capabilities{$c}{enable} = 0;

			$Var{"en$c"} = "Capability '$c' is disabled";

			$Widgets{"b$c"}->configure( -text => "Enable $c" );
		}

		foreach $m ( @{$capabilities{$c}{required_macros}} ) {
			$Widgets{"b$c$m"}->configure( -command => [ \&explain, $m ] );
			$Widgets{"e$c$m"}->configure( -textvariable => \$$m );
		}
	}

	return $capabilities_window;
}

sub toggle_capability {
	my( $c ) = @_;

		if( $capabilities{$c}{enable} ) {

			$capabilities{$c}{enable} = 0;

			$Var{"en$c"} = "Capability '$c' is disabled";

			$Widgets{"b$c"}->configure( -text => "Enable $c" );

		} else {

			$capabilities{$c}{enable} = 1;

			$Var{"en$c"} = "Capability '$c' is enabled";

			$Widgets{"b$c"}->configure( -text => "Disable $c" );
		}

	return;
}

sub explain {
	my( $macro ) = @_;

	my( $detail ) = $macros{$macro}{Detail};
	$detail =~ s/\n//g;
	$detail =~ s/[[:space:]]+/ /g;
	$detail =~ s/^[[:space:]]+//;
	$detail =~ s/[[:space:]]+$//;

	my( $w ) = $Windows{"Main"}->Toplevel();

	my( $f ) = $w->Frame();
	
	$f->pack( -side => "top",
	      	  -fill => "both", 
		  -expand => "yes" );

	my( $text ) = $f->Scrolled( "ROText",
				    -wrap => "word",
				    -scrollbars => "oe");
				    
	$text->pack( -side => "left", 
		     -fill => "both",
		     -expand => "yes" );

	$text->insert( "end", $detail );

	my( $b ) = $w->Button( -text => "Dismiss", 
		    	       -command => sub { $w->destroy } );

	$b->pack( -side => "top",
	      	  -fill => "both", 
		  -expand => "yes" );

	$w->waitWindow();

	return;
}

sub run_configure {
	use Tk;
	use Tk::Toplevel;

	use ptkform;
	use elog_gui;

	my( $b );

	$Windows{"Main"} = MainWindow->new();

	elog_gui_init( MW => $Windows{"Main"} );
	elog_callback( "::elog_gui" );

	$Windows{"menubar"} = init_menubar( $Windows{"Main"} );

	$Windows{"menubar"}->grid( -row => 0,
				   -column => 0,
				   -sticky => 'new',
				 );

	$b = $Windows{"Main"}->Button( -text => "save configuration",
				       -command => \&commit_configuration );

	$b->grid( -row => 1,
		  -column => 0,
		  -sticky => 'new',
		 );

	$Windows{"capabilities"} = init_capabilities( $Windows{"Main"} );

	$Windows{"capabilities"}->grid( -row => 2,
				   	-column => 0,
				   	-sticky => 'new',
				 	);

	MainLoop;
}

$Pf = "localmake_config";
$Pf_proto = "localmake_config_proto";

$localpf_dir = "$ENV{'ANTELOPE'}/local/data/pf";
$Pf_file = "$localpf_dir/$Pf.pf";
$Pf_proto_file = "$ENV{'ANTELOPE'}/data/pf/$Pf_proto.pf";

$Program = $0;
$Program =~ s@.*/@@;

elog_init( $Program, @ARGV );

if( ! Getopts( 'lv' ) ) {

	elog_die( "Usage: localmake_config [-lv] [capability [, capability...]]\n" );
}

if( @ARGV >= 1 ) {

	$mode = "verify";

} else {

	$mode = "configure";
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

	if( $mode eq "verify" ) {

		elog_complain( "The file '$Pf_file' appears out of date. The file '$Pf_proto_file' is " .
			       "newer than it. You may be missing features. Continuing.\n" );

	} else {
	
		elog_complain( "The file '$Pf_file' appears out of date. The file '$Pf_proto_file' is " .
			       "newer than it, and may contain added features.\n" );

		while( ( $ans = ask( "What to do:\n" .
				     "            [c] continue anyway;\n" .
				     "            [l] list differences with pfdiff;\n" .
				     "            [r] replace existing configuration with new defaults;\n" .
				     "            [q] quit and update by hand\n" .
				     "?: " ) ) !~ /^[crq]$/ ) {

			if( $ans eq "l" ) {

				system( "pfdiff $Pf_file $Pf_proto_file" );
			}
		}

		if( $ans eq "r" ) { 

			unlink( $Pf_file );

			system( "cd $localpf_dir; pfcp $Pf_proto $Pf" );

		} elsif( $ans eq "c" ) {

			; # Fall through

		} elsif( $ans eq "q" ) {

			exit( 0 );

		} else {
			
			elog_die( "Unexpected failure interpreting answer '$ans'. Exiting.\n" );
		}
	}
}

$Os = my_os();

%macros = %{pfget($Pf,"macros")}; 
$header = pfget( $Pf, "header" );
$extra_rules = pfget( $Pf, "extra_rules" );
%capabilities = %{pfget( $Pf, "capabilities" )};

set_macros();

write_makerules();

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
