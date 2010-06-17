use Datascope;
use sysinfo;
use Cwd;
use Term::ANSIColor qw/uncolor/;
use POSIX;
use FileHandle;

require "getopts.pl";

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

		if( ! defined( $defined_tags{$tag_name} ) ) {

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
	}

	return @tagged;
}

sub make_target {
	my( $target ) = @_;

	my( $cf );

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

		$cmd .= " 2>&1";

		$quiet = $cmd . " >& /dev/null";
		$cmd   = $cmd . " $cf";

	} else {

		$cmd = "make $target 2>&1 $cf";
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

		my( $rc ) = system( $cmd );

		if( $rc != 0 ) {

			elog_die( "Command '$cmd' failed in directory '$dir'\n" );
		}
	}

	return $rc;
}

sub localmake_module {
	my( $module ) = @_;

	if( $Gui_mode ) {

		$Windows{"compilebutton_$module"}->configure( -relief => "sunken" );

		$Windows{"CompileOut"}->delete( '0.0', 'end' );
		$Windows{"Main"}->update();
	}

	my( @steps ) = @{$Modules{$module}{build}};

	if( @steps <= 0 ) {
		
		show_available();

		elog_die( "No steps listed for module '$module' in parameter-file '$Pf'\n" );
	}

	inform( "localmake: making module '$module'\n" );

	my( @capabilities ) = @{$Modules{$module}{capabilities_required}};

	if( scalar( @capabilities ) > 0 ) {

		$target = "VERIFY " . join( " ", @capabilities );

		if( make_target( $target ) != 0 ) {

			if( $Gui_mode ) {

				$Windows{"compilebutton_$module"}->configure( -relief => "raised" );
			}

			return;
		}
	}

	my( $cwd ) = Cwd::cwd();

	foreach $step ( @steps ) {
		
		my( $dir );

		if( $step =~ m@^/.*@ ) {

			$dir = $step;

		} else {

			$dir = "$ENV{ANTELOPE}/$step";
		}

		if( ! -d "$dir" ) {

			elog_die( "Directory '$dir' does not exist (Have you downloaded the Antelope " .
				  "contributed source-code distribution and is it in the right place?). " .
				  "Exiting.\n" );
		}

		inform( "localmake: changing directory to '$dir'\n" );

		my( $rc ) = chdir( $dir );

		if( ! $rc ) {

			elog_die( "Couldn't change directory to '$dir'. Exiting.\n" );
		}

		make_target( "clean" );

		make_target( "Include" );

		make_target( "install" );
	}

	chdir( $cwd );

	if( $Gui_mode ) {

		$Windows{"compilebutton_$module"}->configure( -relief => "raised" );
	}

	inform( "localmake: done making module '$module', hopefully successfully (review compilation messages for possible errors)\n\n" );

	if( $Gui_mode && $module eq "bootstrap" ) {

		inform( "localmake: RESTARTING localmake IN 5 SECONDS due to bootstrap recompilation\n" );

		sleep( 5 );

		exec( "$0" );

		exit( 0 );
	}

	return;
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

sub init_window {
	use Tk;
	use Tk::ROText;
	use Tk::Font;
	use elog_gui;
	
	$Windows{"Main"} = MainWindow->new();

	elog_gui_init( MW => $Windows{"Main"} );
	elog_callback( "::elog_gui" );

	$Windows{"Main"}->bind( "<Control-c>", \&quit );
	$Windows{"Main"}->bind( "<Control-C>", \&quit );

	$row = 0;

	$Windows{"menubar"} = init_menubar( $Windows{"Main"} );

	$Windows{"menubar"}->grid( -row => $row++, -column => 0, -sticky => "new" );

	$Windows{"buttons"} = $Windows{"Main"}->Frame( -relief => 'raised', -borderwidth => 5 );

	$Windows{"buttons"}->grid( -row => $row++, -column => 0, -sticky => "new" );

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

	$Windows{"CompileOut"} = $Windows{"Main"}->Scrolled( "ROText", 
						  		-wrap => "word",
						  		-scrollbars => "oe",
								-background => "white" );

	$Windows{"CompileOut"}->tagConfigure( "localmake_inform", -foreground => "brown" );

	$Windows{"CompileOut"}->grid( -row => $row++, -column => 0, -sticky => "nsew" );

	$Windows{"Main"}->gridColumnconfigure( 0, -weight => 1 );

	$Windows{"Main"}->gridRowconfigure( 2, -weight => 1 );

	MainLoop;
}

$Os = my_os();
$Pf = "localmake";

$Program = $0;
$Program =~ s@.*/@@;

elog_init( $Program, @ARGV );

if( !Getopts( 'lp:tv' ) || scalar( @ARGV ) > 1 ) {

	elog_die( "Usage: localmake [-v] [-l] [-t] [-p pfname] [module]\n" );
}

if( $opt_l && scalar( @ARGV ) > 0 ) {

	elog_complain( "Useless specification of module with -l option, ignoring module\n" );
} 

if( $opt_p ) {

	$Pf = $opt_p;
}

%Modules = %{pfget($Pf,"modules")};
$Tarball_time_format = pfget( $Pf, "tarball_time_format" );
$Tar_command = pfget( $Pf, "tar_command" );

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

localmake_module( $module );

if( $opt_t ) {
	
	$tarfilelist = "/tmp/localmake_$<_$$";

	open( T, ">$tarfilelist" );
	
	print T map { "$ENV{ANTELOPE}/$_\n" } @{$Modules{$module}{package}};

	close( T );

	$tarfile = epoch2str( str2epoch( "now" ), $Tarball_time_format );

	$tarfile .= "_$module";
	$tarfile .= "_" . my_hardware(); 
	$tarfile .= "_" . my_os();
	$tarfile .= "_tarball.tar";

	$cmd = "$Tar_command -T $tarfilelist -P -c -v -f $tarfile";

	inform( "localmake: executing '$cmd'\n" );

	system( $cmd );

	unlink( $tarfilelist );

	inform( "localmake: executing '$cmd'\n" );

	$cmd = "bzip2 $tarfile";

	inform( "localmake: executing '$cmd'\n" );

	system( $cmd );

	inform( "localmake: created package file '$tarfile.bz2'\n" );
}
