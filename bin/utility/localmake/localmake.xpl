use Datascope;
use sysinfo;
use Cwd;
use Term::ANSIColor qw/uncolor/;
use POSIX;
use FileHandle;

use Getopt::Std;

sub show_available {

	if( scalar( @Module_names ) <= 0 ) {
		
		print "\n\n\tNo modules configured in $Pf.pf\n\n";

	} else {

		print "\nAvailable modules:\n\n";

		foreach $module ( @Module_names ) {

			print "\t$module\n";
		}

		print "\n";
	}
}

sub quit {

	$Windows{"Main"}->destroy();
}

sub save_as {

	$FSref = $Windows{"Main"}->FileSelect( -directory => getcwd() );

	$file = $FSref->Show;

	if( defined( $file ) && $file ne "" ) {
		
		open( S, "> $file" );

		print S $Windows{"CompileOut"}->Contents();

		close( S );
	}
}

sub inform {
	my( $msg ) = @_;

	if( $opt_v ) {
		
		if( $Gui_mode ) { 

			$Windows{"CompileOut"}->insert( "end", $msg, "localmake_inform" );

			$Windows{"CompileOut"}->see( 'end' );

			$Windows{"Main"}->update();

		} else {

			elog_notify( $msg );
		}
	}
	
	return;
}

sub load_modules {

	my( %modules, $val, $p );

	my( @exclude ) = ( "tarball_time_format",
			   "tar_command",
			   "make_command",
			   "pf_revision_time" );

	$p = pfget( $Pf, "" );

	foreach $key ( keys( %$p ) ) {

		next if( grep( /^$key$/, @exclude ) );

		next if( $key =~ /src_subdir/ );

		if( $key eq "modules" ) {
			
			elog_die( "Your $Pf.pf file still contains the 'modules' array, indicating " .
				  "that it is out of date. Please update $Pf.pf per the localmake(1) " .
				  "documentation. Exiting.\n" );

		} 
		
		$val = pfget( $Pf, $key );

		if( ref( $val ) eq "HASH" ) {

			$modules{$key} = $val;

		} else {
			
			elog_complain( "Unexpected parameter '$key' in $Pf.pf. " .
				       "Ignoring and attempting to continue \n" );
		}
	}

	return %modules;
}

sub ansicolored_to_tagged {
	my( $line ) = @_;

	if( $line eq "ANSICODE_00 \n" ) {

		return ( "\n" );
	} 

	my( @line_parts ) = split( /(?=ANSICODE_)/, $line );

	my( @tagged ) = ();

	while( @line_parts ) {

		my( @tag_parts ) = ();

		my( $layer ) = "foreground";
		my( $underline ) = 0;
		my( $color ) = "black";
		my( $clear ) = 0;
		my( $bold ) = 0;
		my( $blink ) = 0; 	# ignore

		# Tag parsing is approximate, for standard combinations

		my( $token ) = "";

		while( $line_parts[0] =~ /^ANSICODE_(\d\d)/ && $token eq "" ) {

			my( $tag ) = uncolor( $1 );

			# Only use the last of these on fall-through; preceding should be empty

			if( $tag eq "reverse" ) {
				
				$layer = "background";

			} elsif( $tag eq "underline" ) {

				$underline++;

			} elsif( $tag eq "clear" ) {

				$clear++;

			} elsif( $tag eq "bold" ) {

				$bold++;

			} elsif( $tag eq "blink" ) {

				$blink++;

			} elsif( $tag =~ /black|red|green|yellow|blue|magenta|cyan|white/ ) {

				$color = $tag;

				# HACK to make this show up:
				if( $color eq "cyan" ) { 
					
					$color = "dark cyan";
				}
			}

			push( @tag_parts, $tag );

			$token = substr( shift( @line_parts ), length( "ANSICODE_?? " ) );
		}

		my( $tag_name ) = join( "_", @tag_parts );

		if( ! defined( $Defined_tags{$tag_name} ) ) {

			my( @tagopts ) = ();

			my( $font ) = $Windows{"CompileOut"}->cget( -font );

			if( $clear ) {

				$Windows{"CompileOut"}->tagConfigure( $tag_name, 
								-foreground => "black",
								-background => "",
								-font => $font,
								-underline => 0 );
			}

			if( $underline ) {

				push( @tagopts, "-underline", 1 );
			}

			if( $bold ) {

				$font_bold = $font->Clone( -weight => "bold" );

				push( @tagopts, "-font", $font_bold );
				
			}

			if( $layer eq "foreground" ) {
				
				push( @tagopts, "-foreground", $color );

			} else {

				push( @tagopts, "-background", $color, "-foreground", "black" );
			}

			$Windows{"CompileOut"}->tagConfigure( $tag_name, @tagopts );
		}

		push( @tagged, $token, $tag_name );

		$Defined_tags{$tag_name}++;
	}

	return @tagged;
}

sub make_target {
	my( $target ) = @_;

	my( $cf, $rc );

	if( -x "$ENV{'ANTELOPE'}/bin/cf" ) {

		if( $Gui_mode ) {

			$cf = "| cf -c";

		} else {

			$cf = "| cf";
		}

	} else {

		$cf = "";
	}

	my( $cmd, $quiet );

	if( $target =~ /^VERIFY/ ) {

		$cmd = $target;
		$cmd =~ s/VERIFY/localmake_config/;

		$quiet = $cmd . " > /dev/null 2>&1";
		$cmd   = $cmd . " 2>&1 $cf";

	} else {

		$cmd = "$Make_command $target 2>&1 < /dev/null $cf";
	}

	inform( "localmake: executing '$cmd'\n" );

	if( $Gui_mode ) {

		$fh = new FileHandle;
		
		$fh->open( "$cmd |" );

		$fh->autoflush(1);
			
		while( $line = <$fh> ) {
				
			@tagged = ansicolored_to_tagged( $line );

			$Windows{"CompileOut"}->insert( "end", @tagged );

			$Windows{"CompileOut"}->see( 'end' );

			$Windows{"Main"}->update();
		}

		$fh->close();

		# Re-run the verify command rather than construct an entire 
		# auto-flushing spawn architecture:

		if( $target =~ /^VERIFY/ ) {

			$rc = system( $quiet );

		} else {
			
			$rc = 0;
		}

	} else {

		if( $target =~ /^VERIFY/ ) {

			$rc = system( $quiet );

			if( $rc != 0 ) {

				# Re-run to show output without overwriting return code: 

				system( $cmd );
			}

		} else {

			$rc = system( $cmd );

			if( $rc != 0 ) {

				elog_die( "Command '$cmd' failed in directory '$Dir'\n" );
			}
		}
	}

	return $rc;
}

sub clear_compileout {

	my( $geom ) = $Windows{"Main"}->geometry();

	$Windows{"CompileOut"}->destroy();

	%Defined_tags = ();

	$Windows{"CompileOut"} = $Windows{"compile"}->Scrolled( "ROText", 
						  		-wrap => "word",
						  		-scrollbars => "oe",
								-background => "white" );

	$Windows{"CompileOut"}->tagConfigure( "localmake_inform", -foreground => "brown" );

	$Windows{"CompileOut"}->grid( -row => $CompileOut_Row, -column => 0, -sticky => "nsew" );

	$Windows{"Main"}->gridRowconfigure( $CompileOut_Row, -weight => 1 );

	$Windows{"Main"}->geometry( $geom );
	
	$Windows{"Main"}->update();

	return;
}

sub localmake_module {
	my( $module ) = @_;

	if( $Gui_mode ) {

		$Windows{"compilebutton_$module"}->configure( -relief => "sunken" );

		destroy_followup_buttons();

		clear_compileout();
	}

	my( @steps ) = @{$Modules{$module}{build}};

	if( @steps <= 0 ) {
		
		show_available();

		elog_die( "No steps listed for module '$module' in parameter-file '$Pf'\n" );
	}

	my( $src_subdir, $product );

	$product = $Modules{$module}{product};
	
	if( $opt_s ) {
		
		$src_subdir = $opt_s;

	} else { 
		
		$src_subdir = $Modules{$module}{src_subdir};
	}

	$start_time = now();
	$start_time_str = epoch2str( $start_time, "%A %B %o, %Y at %T %Z", "" );

	inform( "localmake: making module '$module' starting on $start_time_str\n" );

	my( @capabilities ) = @{$Modules{$module}{capabilities_required}};

	if( scalar( @capabilities ) > 0 ) {

		$target = "VERIFY " . join( " ", @capabilities );

		if( make_target( $target ) != 0 ) {

			if( $Gui_mode ) {

				$Windows{"compilebutton_$module"}->configure( -relief => "raised" );
			}

			return -1;
		}
	}

	my( $cwd ) = Cwd::cwd();

	foreach $step ( @steps ) {
		
		if( $step =~ m@^/.*@ ) {

			$Dir = $step;

		} elsif( $src_subdir =~ m@^/.*@ ) {

			$Dir = "$src_subdir/$step";

		} else {

			$Dir = "$product/$src_subdir/$step";
		}

		if( ! -d "$Dir" ) {

			elog_die( "Directory '$Dir' does not exist (Have you downloaded the Antelope " .
				  "contributed source-code distribution and is it in the right place?). " .
				  "Exiting.\n" );
		}

		inform( "localmake: changing directory to '$Dir'\n" );

		my( $rc ) = chdir( $Dir );

		if( ! $rc ) {

			elog_die( "Couldn't change directory to '$Dir'. Exiting.\n" );
		}

		make_target( "clean" );

		make_target( "Include" );

		make_target( "install" );
	}

	chdir( $cwd );

	if( $Gui_mode ) {

		$Windows{"compilebutton_$module"}->configure( -relief => "raised" );
	}

	#HARD-WIRE tag names (colors) interpreted as warnings and errors

	@warning_blocks = $Windows{"CompileOut"}->tagRanges("magenta");
	@error_blocks = $Windows{"CompileOut"}->tagRanges("red");

	$num_warning_blocks = scalar( @warning_blocks ) / 2;
	$num_error_blocks = scalar( @error_blocks ) / 2;

	if( $Gui_mode ) {

		if( $num_warning_blocks > 0 || $num_error_blocks > 0 ) {
			
			add_followup_buttons();
		}

		$end_time = now();
		$end_time_str = epoch2str( $end_time, "%A %B %o, %Y at %T %Z", "" );

		$delta = strtdelta( $end_time - $start_time );
		$delta =~ s/^[[:space:]]+//;
		$delta =~ s/[[:space:]]+$//;

		$msg = "localmake: done making module '$module'\n" .
			"\tStarted:  $start_time_str\n" .
			"\tFinished: $end_time_str\n" .
			"\tElapsed:  $delta\n";

		$Windows{"CompileOut"}->insert( "end", $msg, "localmake_inform" );

		$msg = "\tWarnings: $num_warning_blocks blocks of warning messages\n";

		if( $num_warning_blocks > 0 ) {
			
			$tag = "magenta";

		} else {

			$tag = "localmake_inform";
		}

		$Windows{"CompileOut"}->insert( "end", $msg, $tag );

		$msg = "\tErrors:   $num_error_blocks blocks of error messages\n";

		if( $num_error_blocks > 0 ) {
			
			$tag = "red";

		} else {

			$tag = "localmake_inform";
		}

		$Windows{"CompileOut"}->insert( "end", $msg, $tag );

		$Windows{"CompileOut"}->see( 'end' );

		$Windows{"Main"}->update();

	} else {

		inform( "localmake: done making module '$module' with " .
			"$num_warning_blocks blocks of warning messages and " .
			"$num_error_blocks blocks of error messages\n\n" );
	}

	if( $Gui_mode && $module eq "bootstrap" ) {

		inform( "localmake: RESTARTING localmake IN 5 SECONDS due to bootstrap recompilation\n" );

		sleep( 5 );

		exec( "$0" );

		exit( 0 );
	}

	return 0;
}

sub compute_font_height {

	if( $Windows{"CompileOut"}->height() == 1 ) {
		
		$Windows{"Main"}->after( 100, \&compute_font_height );

		return;
	}

	$Font_height = $Windows{"CompileOut"}->height() / $Windows{"CompileOut"}->cget( -height );

	return;
}

sub get_next_hidden_message {
	my( $listref, $indexref ) = @_;

	my( $height_rows ) = int( $Windows{"CompileOut"}->height() / $Font_height );

	my( $last_visible ) = $Windows{"CompileOut"}->index('@0,0') + $height_rows;

	$$indexref++;

	my( $next_start ) = $$listref[$$indexref * 2];

	while( $next_start <= $last_visible && $$indexref * 2 < scalar( @{$listref} ) ) {

		$$indexref++;

		$next_start = $$listref[$$indexref * 2];
	}

	if( ! defined( $next_start ) || $next_start eq "" || $$indexref >= scalar( @{$listref} ) - 1 ) {

		$$indexref = scalar( @{$listref} ) / 2 - 1;
	}

	return $$indexref;
}

sub show_first_error {
	
	my( $start ) = $errorslist[0];

	$current_error = 0;

	$Windows{"CompileOut"}->see( $start );

	$Windows{"CompileOut"}->update();

	if( ( $current_error + 1 ) * 2 < scalar( @errorslist ) ) {

		$Windows{"NextError"}->configure( -state => "normal" );
	}
}

sub show_first_warning {
	
	my( $start ) = $warningslist[0];

	$current_warning = 0;

	$Windows{"CompileOut"}->see( $start );

	$Windows{"CompileOut"}->update();

	if( ( $current_warning + 1 ) * 2 < scalar( @warningslist ) ) {

		$Windows{"NextWarning"}->configure( -state => "normal" );
	}
}

sub show_next_error {
	
	$current_error = get_next_hidden_message( \@errorslist, \$current_error );

	my( $start ) = $errorslist[$current_error * 2];

	$Windows{"CompileOut"}->see( $start );

	$Windows{"CompileOut"}->update();

	if( ( $current_error + 1 ) * 2 >= scalar( @errorslist ) ) {

		$Windows{"NextError"}->configure( -state => "disabled" );
	}
}

sub show_next_warning {
	
	$current_warning = get_next_hidden_message( \@warningslist, \$current_warning );

	my( $start ) = $warningslist[$current_warning * 2];

	$Windows{"CompileOut"}->see( $start );

	$Windows{"CompileOut"}->update();

	if( ( $current_warning + 1 ) * 2 >= scalar( @warningslist ) ) {

		$Windows{"NextWarning"}->configure( -state => "disabled" );
	}
}

sub create_compile_button {
	my( $w, $module ) = @_;

	my( $b ) = $w->Button( -text => "$module",
			       -relief => "raised",
			       -command => [\&localmake_module, $module] );

	return( $b );
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

	$filemenu->add( "command", -label => "Save as...", -command => \&save_as );
	$filemenu->add( "command", -label => "Quit", -command => \&quit );

	$menubutton->configure( -menu => $filemenu );

	my( $button );

	$button = $w->Button( -text => "Run localmake_config",
			      -bg => "green", 
			      -command => sub { system( "localmake_config &" ); } );

	$button->pack( -side => "right" );

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

sub add_followup_buttons {

	my( $firsterror_state, $nexterror_state, $firstwarning_state, $nextwarning_state );

	@errorslist = $Windows{"CompileOut"}->tagRanges( "red" );
	@warningslist = $Windows{"CompileOut"}->tagRanges( "magenta" );
	$current_error = 0;
	$current_warning = 0;

	if( scalar( @errorslist ) <= 0 ) {

		$firsterror_state = 'disabled';
		$nexterror_state = 'disabled';

	} else {

		$firsterror_state = 'normal';
		$nexterror_state = 'disabled';
	}

	if( scalar( @warningslist ) <= 0 ) {

		$firstwarning_state = 'disabled';
		$nextwarning_state = 'disabled';

	} else {

		$firstwarning_state = 'normal';
		$nextwarning_state = 'disabled';
	}

	$w = $Windows{"Main"};

	my( $frame ) = $w->Frame( -relief => 'raised', -borderwidth => 5 );

	$Windows{"FirstError"} = $frame->Button( -text => "First Error", 
					    -relief => 'raised', 
					    -foreground => 'red', 
					    -state => $firsterror_state,
					    -command => \&show_first_error );

	$Windows{"FirstError"}->pack( -side => 'left', -fill => 'x', -expand => 'yes' );		

	$Windows{"NextError"} = $frame->Button( -text => "Next Error", 
					   -relief => 'raised', 
					   -foreground => 'red', 
					   -state => $nexterror_state,
					   -command => \&show_next_error );

	$Windows{"NextError"}->pack( -side => 'left', -fill => 'x', -expand => 'yes' );		

	$Windows{"FirstWarning"} = $frame->Button( -text => "First Warning", 
					      -relief => 'raised', 
					      -foreground => 'magenta', 
					      -state => $firstwarning_state,
					      -command => \&show_first_warning );

	$Windows{"FirstWarning"}->pack( -side => 'left', -fill => 'x', -expand => 'yes' );		

	$Windows{"NextWarning"} = $frame->Button( -text => "Next Warning", 
					     -relief => 'raised', 
					     -foreground => 'magenta', 
					     -state => $nextwarning_state,
					     -command => \&show_next_warning );

	$Windows{"NextWarning"}->pack( -side => 'left', -fill => 'x', -expand => 'yes' );		

	$frame->grid( -row => 3, -column => 0, -sticky => "new" );

	$w->gridRowconfigure( 3, -weight => 0 );

	$Windows{"FollowUpButtons"} = $frame;

	$Windows{"Main"}->update();

	return;
}

sub destroy_followup_buttons {

	undef( @errorslist );
	undef( @warningslist );

	undef( $current_error );
	undef( $current_warning );

	if( Exists( $Windows{"FollowUpButtons"} ) ) {

		$Windows{"FollowUpButtons"}->destroy();
	}

	return;
}

sub init_compile_window {
	my( $w ) = @_;

        my( $compilewindow );

        $compilewindow = $w->Frame( -relief => 'raised',
                              -borderwidth => 2 );

	$Windows{"buttons"} = $compilewindow->Frame( -relief => 'raised', -borderwidth => 5 );

	$Windows{"buttons"}->grid( -row => 0, -column => 0, -sticky => "new" );

	$compilewindow->gridColumnconfigure( 0, -weight => 1 );

	$buttonrow = $buttoncolumn = 0;

	$gridrank = ceil( sqrt( scalar( @Module_names ) ) );

	foreach $module ( @Module_names ) {

		$Windows{"compilebutton_$module"} = create_compile_button( $Windows{"buttons"}, $module );

		$Windows{"compilebutton_$module"}->grid( -row => $buttonrow, 
							 -column => $buttoncolumn, 
							 -sticky => "new" );

		if( $buttonrow == 0 ) {

			$Windows{"buttons"}->gridColumnconfigure( $buttoncolumn, -weight => 1 );
		}

		if( $buttoncolumn < $gridrank - 1 ) {

			$buttoncolumn++;

		} else {

			$buttoncolumn = 0;

			$buttonrow++;
		}
	}

	$Windows{"CompileOut"} = $compilewindow->Scrolled( "ROText", 
					  		-wrap => "word",
					  		-scrollbars => "oe",
							-background => "white", 
							-width => $ROWidth );

	$Windows{"CompileOut"}->tagConfigure( "localmake_inform", -foreground => "brown" );

	$CompileOut_Row = 1;

	$Windows{"CompileOut"}->grid( -row => $CompileOut_Row, -column => 0, -sticky => "nsew" );

	$compilewindow->gridRowconfigure( $CompileOut_Row, -weight => 1 );

	return $compilewindow;
}

sub init_window {
	use Tk;
	use Tk::ROText;
	use Tk::Font;
	use Tk::FileSelect;
	use elog_gui;
	
	$Windows{"Main"} = MainWindow->new();

	$Windows{"Main"}->title( my_hostname() . ": localmake" );

	elog_gui_init( MW => $Windows{"Main"} );
	elog_callback( "::elog_gui" );

	$Windows{"Main"}->bind( "<Control-c>", \&quit );
	$Windows{"Main"}->bind( "<Control-C>", \&quit );

	$Windows{"menubar"} = init_menubar( $Windows{"Main"} );

	$Windows{"menubar"}->grid( -row => 0, -column => 0, -sticky => "new" );

	$Windows{"compile"} = init_compile_window( $Windows{"Main"} );

	$Windows{"compile"}->grid( -row => 1, -column => 0, -sticky => "nsew" );

	$Windows{"Main"}->gridColumnconfigure( 0, -weight => 1 );

	$Windows{"Main"}->gridRowconfigure( 1, -weight => 1 );

	$Windows{"Main"}->afterIdle( \&compute_font_height );

	MainLoop;
}

$Os = my_os();
$Pf = "localmake";
$ROWidth = 132;

$Program = $0;
$Program =~ s@.*/@@;

elog_init( $Program, @ARGV );

if( !getopts( 'lp:s:tv' ) || scalar( @ARGV ) > 1 ) {

	elog_die( "Usage: localmake [-v] [-l] [-t] [-p pfname] [-s src_subdir] [module]\n" );
}

if( $opt_l && scalar( @ARGV ) > 0 ) {

	elog_complain( "Useless specification of module with -l option, ignoring module\n" );
} 

if( $opt_p ) {

	$Pf = $opt_p;
}

$Tarball_time_format = pfget( $Pf, "tarball_time_format" );
$Tar_command = pfget( $Pf, "tar_command" );
$Make_command = pfget( $Pf, "make_command" );

if( defined( $ENV{'MAKE'} ) ) {

	$Make_command = $ENV{'MAKE'};
}

%Modules = load_modules();

@Module_names = sort( keys( %Modules ) );

$Gui_mode = 0;

if( $opt_l ) {

	show_available();

	exit( 0 );

} elsif( scalar( @ARGV ) == 0 ) {

	$Gui_mode = 1;

	$opt_v = 1; 	# Make automatic for Gui_mode

	init_window();

	exit( 0 );
}

$module = pop( @ARGV );

if( localmake_module( $module ) < 0 && ! $Gui_mode ) {

	elog_die( "Build of module '$module' failed. Exiting.\n" );
}

if( $opt_t ) {
	
	$tarfilelist = "/tmp/localmake_$<_$$";

	if( scalar( @{$Modules{$module}{package}} ) <= 0 ) {

		elog_die( "No package files defined in $Pf.pf for module '$module'. Exiting.\n" );
	}

	open( T, ">$tarfilelist" );
	
	print T map { "$Modules{$module}{product}/$_\n" } @{$Modules{$module}{package}};

	close( T );

	$tarfile = epoch2str( str2epoch( "now" ), $Tarball_time_format );

	$tarfile .= "_$module";
	$tarfile .= "_" . my_hardware(); 
	$tarfile .= "_" . my_os();
	$tarfile .= "_tarball.tar";

	if( $opt_v ) {

		$v = "-v";

	} else {
		
		$v = "";
	}

	$cmd = "$Tar_command -T $tarfilelist -P -c $v -f $tarfile";

	inform( "localmake: executing '$cmd'\n" );

	system( $cmd );

	unlink( $tarfilelist );

	inform( "localmake: executing '$cmd'\n" );

	$cmd = "bzip2 $tarfile";

	inform( "localmake: executing '$cmd'\n" );

	system( $cmd );

	inform( "localmake: created package file '$tarfile.bz2'\n" );
}
