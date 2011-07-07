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

sub show_capabilities {

format STDOUT = 
   @<<<<<<<<<<< @<<<<<<<<<<<<<<<<<<< @*
   $enabled_string, $c, $capabilities{$c}{Description}
.

	print "\nCapabilities are:\n\n";

	foreach $c ( keys( %capabilities ) ) {

		$enabled = pfget_boolean( $Pf, "capabilities{$c}{enable}{$Os}" );
	
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

sub set_orig_enabled {

	foreach $capability ( keys( %capabilities ) ) {

		$orig_enabled{$capability} = $capabilities{$capability}{enable}{$Os};
	}
}

sub test_configuration_unsaved {

	my( $tf ) = "false";

	foreach $macro ( keys( %macros_orig ) ) {

		if( $$macro ne $macros_orig{$macro}{$Os} ) {

			$tf = "true";

			return $tf;
		}
	}

	foreach $capability ( keys( %capabilities ) ) {

		if( $capabilities{$capability}{enable}{$Os} != $orig_enabled{$capability} ) {

			$tf = "true";

			return $tf;
		}
	}

	return $tf;
}

sub mark_configuration_unsaved {
	my( $tft ) = @_;

	if( $tft eq "test" ) {

		$tft = test_configuration_unsaved();
	}

	if( $tft eq "true" ) {

		$Windows{"save_config"}->configure( -text => "save configuration (SOME CHANGES UNSAVED)",
				       		    -bg => "yellow",
						    -state => "normal" );
	} else {

		$Windows{"save_config"}->configure( -text => "save configuration",
				       		    -bg => "gray",
						    -state => "disabled" );
	}
}

sub commit_configuration {

	foreach $macro ( keys( %macros ) ) {

		$macros{$macro}{$Os} = $$macro;
	}

	pfput( "macros", \%macros, $Pf );
	pfput( "capabilities", \%capabilities, $Pf );

	pfwrite( $Pf_file, $Pf );

	write_makerules();

	mark_configuration_unsaved( "false" );

	%macros_orig = %macros;

	set_orig_enabled();

	return;
}

sub tweak_capability {
	
	mark_configuration_unsaved( "test" );

	test_capability( @_ );
}

sub test_capability {
	if( ref( $_[0] ) ) { shift( @_ ); }
	my( $c, $mode ) = @_;
	
	my( $passed ) = 1;

	if( $mode eq "configure" ) {

		$Widgets{"t$c"}->delete( '0.0', 'end' );
	}

	if( ! defined( $capabilities{$c} ) ) {

		if( $mode eq "verify" ) {

			elog_complain( "Requested capability '$c' not defined in '$Pf'. " .
					"Stopping compilation.\n" );

			exit( -1 );
		}
	}

	if( ! pfget_boolean( $Pf, "capabilities{$c}{enable}{$Os}" ) && $mode eq "verify" ) {

		elog_complain( "Requested capability '$c' marked as disabled in '$Pf'.\n" .
			"Run localmake_config(1) (or edit '$Pf_file')\nto enable and configure " .
			"'$c' if desired.\n" );

		exit( -1 );

	}

	if( ! $capabilities{$c}{enable}{$Os} && $mode eq "configure" ) {

		$Widgets{"t$c"}->insert( "end", "Capability '$c' disabled\n", 'disabled' );

		$passed = 0;

		$Var{"en$c"} = "Capability '$c' is disabled";

		$Widgets{"en$c"}->configure( -fg => "grey30" );

		return $passed;
	} 

	@required_macros = @{pfget( $Pf, "capabilities{$c}{required_macros}" )};
	@tests = @{pfget( $Pf, "capabilities{$c}{tests}" )};

	while( $required_macro = shift( @required_macros ) ) {

		if( ! defined( $$required_macro ) || $$required_macro eq "" ) {
				
			if( $mode eq "verify" ) {

				elog_complain( "Macro '$required_macro', required for '$c' capability, " .
						"is not defined.\nRun localmake_config(1) (or edit '$Pf_file')\n" .
						"to configure.\n" );

				exit( -1 );

			} else {

				$Widgets{"t$c"}->insert( "end", 
					"Failed check for capability '$c': " .
					"Required macro '$required_macro' is not defined\n\n", 
					'failed' );

				$passed = 0;
			}

		} else {

			if( $mode eq "configure" ) {

				$Widgets{"t$c"}->insert( "end", 
					"Passed check for capability '$c': " .
					"Required macro '$required_macro' is defined\n\n", 
					'passed' );
			}
		}
	}

	while( $test = shift( @tests ) ) {
			
		if( ! eval( $test ) ) {

			if( $mode eq "verify" ) {

				elog_complain( "Test failed for capability '$c': $failure_msg\n" );

				exit( -1 );
				
			} else {

				$Widgets{"t$c"}->insert( "end", 
					"Failed: Test failed for capability '$c': $failure_msg\n\n", 
					'failed' );

				$passed = 0;
			}

		} else {

			if( $mode eq "configure" ) {

				$Widgets{"t$c"}->insert( "end", 
					"Passed test for capability '$c': $success_msg\n\n", 
					'passed' );
			}
		}
	}

	if( $mode eq "configure" ) {

		if( $passed ) {

			$Var{"en$c"} = "Capability '$c' is enabled";

			$Widgets{"en$c"}->configure( -fg => "darkgreen" );

		} else {

			$Var{"en$c"} = "Capability '$c' is enabled but failed test(s)";

			$Widgets{"en$c"}->configure( -fg => "red" );
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

sub freeze_size {

	$Windows{"Main"}->resizable( 0, 0 );
}

sub init_File_menu {
	my( $w ) = @_;

	my( $menubutton, $filemenu );

	$menubutton = $w->Menubutton (
			    -text => 'File',
			    -pady => 0, 
			    -anchor => 'w', 
			    );
			    
	$menubutton->pack( -side => "left" );

	$filemenu = $menubutton->Menu( -tearoff => 0 );

	$filemenu->add( "command", -label => "Quit without saving", -command => \&quit );
	$filemenu->add( "command", -label => "Save and Quit", -command => \&save_and_quit );

	$menubutton->configure( -menu => $filemenu );

	return;
}

sub init_menubar {
	my( $w ) = @_;

	my( $menubar );

	$menubar = $w->Frame( -relief => 'raised', 
			      -borderwidth => 2 );

	init_File_menu( $menubar );

	my( $b ) = $menubar->Button( -text => "Run localmake",
				     -bg => "green",
			       	     -command => sub { system( "localmake &" ); } );

	$b->pack( -side => "right" );

	return $menubar;
}

sub resticky {
	my( $w, $sticky ) = @_;

	my( $parent ) = $w->parent();

	my( %params ) = $parent->gridInfo();

	$params{"-sticky"} = $sticky;

	$parent->gridForget();

	$parent->grid( %params );

	return;
}

sub init_capabilities {
	my( $w ) = @_;

	my( $capabilities_window );
	my( @specs, @lefttop, @righttop, @leftbottom, @rightbottom, $i );
	my( $leftwidth ) = 80;

	$capabilities_window = $w->Frame( -relief => 'raised', 
					  -borderwidth => 2 );

	push( @specs, "notebook capabilities - 0,0 Capabilities" );

	foreach $c ( sort( keys( %capabilities ) ) ) {

		push( @specs, "notebookpage $c - xxx $c" );

		@lefttop = ();
		@righttop = ();
		@leftbottom = ();
		@rightbottom = ();

		push( @lefttop, "label np$c $leftwidth 0,0 Capability:" );
		push( @lefttop, "label en$c $leftwidth +,0 Status:" );

		push( @righttop, "button b$c - 0,0 Toggle" );
		push( @righttop, "button e$c - +,0 Explain '$c' capability" );

		foreach $m ( @{$capabilities{$c}{required_macros}} ) {

			push( @leftbottom, "entry e$c$m $leftwidth +,0 $m { $macros{$m}{Description} }" );
			push( @rightbottom, "button b$c$m - +,0 Explain '$m' macro" );
		}

		push( @specs, "frame top$c - 0,0" );

		push( @specs, "frame lefttop$c - 0,0" );
		push( @specs, @lefttop );
		push( @specs, "endframe" );

		push( @specs, "frame righttop$c - 0,1" );
		push( @specs, @righttop );
		push( @specs, "endframe" );

		push( @specs, "endframe" );

		push( @specs, "frame bottom$c - 1,0" );

		push( @specs, "frame leftbottom$c - 0,0" );
		push( @specs, @leftbottom );
		push( @specs, "endframe" );

		push( @specs, "frame rightbottom$c - 0,1" );
		push( @specs, @rightbottom );
		push( @specs, "endframe" );

		push( @specs, "endframe" );

		push( @specs, "rotext t$c - 2,0 Tests:" );
	}

	push( @specs, "endnotebook" );

	ptkform( $capabilities_window, \%Var, \%Widgets, @specs );

	foreach $c ( keys( %capabilities ) ) {

		$Widgets{"t$c"}->tagConfigure( 'failed', -foreground => "red" );
		$Widgets{"t$c"}->tagConfigure( 'passed', -foreground => "darkgreen" );
		$Widgets{"t$c"}->tagConfigure( 'disabled', -foreground => "grey30" );

		resticky( $Widgets{"t$c"}, "nsew" );

		resticky( $Widgets{"b$c"}, "ew" );

		$Var{"np$c"} = "$capabilities{$c}{Description}"; 

		$Widgets{"b$c"}->configure( -command => [\&toggle_capability, $c] );

		if( pfget_boolean( $Pf, "capabilities{$c}{enable}{$Os}" ) ) {

			$capabilities{$c}{enable}{$Os} = 1;

			$test_result = test_capability( $c, "configure" );

			$Widgets{"b$c"}->configure( -text => "Disable '$c' capability", -bg => "#ffdddd" );

		} else {

			$capabilities{$c}{enable}{$Os} = 0;

			$test_result = test_capability( $c, "configure" );

			$Widgets{"b$c"}->configure( -text => "Enable '$c' capability", -bg => "light green" );
		}

		$Widgets{"e$c"}->configure( -command => [ \&explain, $capabilities{$c}{Detail} ] );

		foreach $m ( @{$capabilities{$c}{required_macros}} ) {

			$Widgets{"b$c$m"}->configure( -command => [ \&explain, $macros{$m}{Detail} ] );

			$Widgets{"e$c$m"}->configure( -textvariable => \$$m );

			$Widgets{"e$c$m"}->bind( "<KeyPress-Return>", [ \&tweak_capability, $c, "configure" ] );
			$Widgets{"e$c$m"}->bind( "<KeyPress-Tab>", [ \&tweak_capability, $c, "configure" ] );
			$Widgets{"e$c$m"}->bind( "<Leave>", [ \&tweak_capability, $c, "configure" ] );
		}
	}

	return $capabilities_window;
}

sub toggle_capability {
	my( $c ) = @_;

	if( $capabilities{$c}{enable}{$Os} ) {

		$capabilities{$c}{enable}{$Os} = 0;

		$test_result = test_capability( $c, "configure" );

		$Widgets{"b$c"}->configure( -text => "Enable '$c' capability", -bg => "light green" );

	} else {

		$capabilities{$c}{enable}{$Os} = 1;

		$test_result = test_capability( $c, "configure" );

		$Widgets{"b$c"}->configure( -text => "Disable '$c' capability", -bg => "#ffdddd" );
	}

	mark_configuration_unsaved( "true" );

	return;
}

sub explain {
	my( $detail ) = @_;

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

sub save_and_quit {

	commit_configuration();

	quit();
}

sub quit {

	$Windows{"Main"}->destroy();
}

sub run_configure {
	use Tk;
	use Tk::Toplevel;

	use ptkform;
	use elog_gui;

	$Windows{"Main"} = MainWindow->new();

	elog_gui_init( MW => $Windows{"Main"} );
	elog_callback( "::elog_gui" );

	$Windows{"Main"}->bind( "<Control-c>", \&quit );
	$Windows{"Main"}->bind( "<Control-C>", \&quit );

	$Windows{"menubar"} = init_menubar( $Windows{"Main"} );

	$Windows{"menubar"}->grid( -row => 0,
				   -column => 0,
				   -sticky => 'new',
				 );

	$Windows{"save_config"} = $Windows{"Main"}->Button( -text => "save configuration",
				       -command => \&commit_configuration, 
				       -bg => "gray",
				       -state => "disabled" );

	$Windows{"save_config"}->grid( -row => 1,
		  		       -column => 0,
		  		       -sticky => 'new',
		 			);

	$Windows{"capabilities"} = init_capabilities( $Windows{"Main"} );

	$Windows{"capabilities"}->grid( -row => 2,
				   	-column => 0,
				   	-sticky => 'nsew',
				 	);

	$Windows{"Main"}->gridColumnconfigure( 0, -weight => 1 );
	$Windows{"Main"}->gridRowconfigure( 2, -weight => 1 );

	$Windows{"Main"}->afterIdle( \&freeze_size );

	MainLoop;
}

$Pf = "localmake_config";
$Pf_proto = "localmake_config_proto";

$localpf_dir = "$ENV{'ANTELOPE'}/local/data/pf";

$ENV{'PFPATH'} = "$localpf_dir:$ENV{'PFPATH'}";

$Pf_file = "$localpf_dir/$Pf.pf";
$Pf_proto_file = "$ENV{'ANTELOPE'}/data/pf/$Pf_proto.pf";

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

	system( "cd $localpf_dir; cp $Pf_proto_file $Pf.pf" );

	inform( "Copied '$Pf_proto.pf' to '$localpf_dir/$Pf.pf' since the latter didn't exist\n" );

	if( ! -e "$localpf_dir/$Pf.pf" ) {

		elog_die( "Failed to make '$localpf_dir/$Pf.pf'; Exiting.\n" );
	}
}

if( pfrequire( $Pf, pfget_time( $Pf_proto, "pf_revision_time" ) ) < 0 ) {

	if( $mode eq "verify" ) {

		elog_complain( "Your file '$Pf_file' appears out of date. The default file '$Pf_proto_file' is " .
			       "newer than it. You may be missing features. Continuing.\n" );

	} else {
	
		elog_complain( "Your file '$Pf_file' appears out of date. The default file '$Pf_proto_file' is " .
			       "newer than it, and may contain added features.\n" );

		while( ( $ans = ask( "What to do:\n" .
				     "            [c] continue anyway, risking using the old parameter file;\n" .
				     "            [l] list differences with pfdiff;\n" .
				     "            [r] replace existing configuration with new defaults;\n" .
				     "            [q] quit and update by hand\n" .
				     "?: " ) ) !~ /^[crq]$/ ) {

			if( $ans eq "l" ) {

				elog_notify( "Your configuration file\n\t$Pf_file\ndiffers from the new " .
					     "default file\n\t$Pf_proto_file\nin the following ways:\n\n" );

				system( "pfdiff $Pf_file $Pf_proto_file" );
			}
		}

		if( $ans eq "r" ) { 

			unlink( $Pf_file );

			system( "cd $localpf_dir; cp $Pf_proto_file $Pf.pf" );

		} elsif( $ans eq "c" ) {

			; # Fall through

		} elsif( $ans eq "q" ) {

			elog_notify( "Please update your file\n\t$Pf_file\nto follow the pattern of the " .
				     "new default file\n\t$Pf_proto_file\nExiting.\n" );

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
