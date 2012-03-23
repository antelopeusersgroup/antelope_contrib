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
		
		if( $Gui_mode && defined( $Windows{"CompileOut"} ) ) { 

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
			   "pf_revision_time",
			   "antelope",
			   "dest",
			   "extra_rules", 
			   "capabilities",
			   "header",
			   "macros",
			   "output_file" );

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

sub freeze_size {

       $Windows{"Main"}->resizable( 0, 0 );
}

sub unfreeze_size {

       $Windows{"Main"}->resizable( 1, 1 );
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

		freeze_size();

		$Windows{"compilebutton_$module"}->configure( -relief => "sunken" );

		destroy_followup_buttons();

		clear_compileout();

		$Windows{"Main"}->afterIdle( \&unfreeze_size );
	}

	my( @steps ) = @{$Modules{$module}{build}};

	if( @steps <= 0 ) {
		
		show_available();

		elog_die( "No steps listed for module '$module' in parameter-file '$Pf'\n" );
	}

	if( grep( /^nobuild/, @steps ) ) {

		my( $msg ) = "WARNING: Module '$module' contains elements in the 'nobuild' directory " .
			     "of the contributed-code repository. Installing them may conflict " .
			     "with your existing Antelope installation, at worst making it necessary " .
			     "to erase Antelope and reinstall from scratch.";

		if( $Gui_mode ) {

			my( $dialog ) = $Windows{"Main"}->Dialog( -title => "nobuild warning",
							  	  -text => "$msg How do you wish to proceed ? ",
							  	  -buttons => ['Install Despite Warning', 'Do Not Install'],
							  	  -default_button => 'Do Not Install' );

			my( $ans ) = $dialog->Show();

			unless( $ans eq 'Install Despite Warning' ) {
				
				elog_complain( "Skipping install of potentially conflicting software module '$module'" );

				$Windows{"compilebutton_$module"}->configure( -relief => "raised" );

				return -1;
			}
		} else {

			elog_complain( "$msg" );

			my( $ans ) = askyn( "Install '$module' despite WARNING [yN] ? " );

			unless( $ans ) {

				elog_complain( "Skipping install of potentially conflicting software module '$module'" );

				return -1;
			}
		}
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

		#HARD-WIRE tag names (colors) interpreted as warnings and errors

		@warning_blocks = $Windows{"CompileOut"}->tagRanges("magenta");
		@error_blocks = $Windows{"CompileOut"}->tagRanges("red");

		$num_warning_blocks = scalar( @warning_blocks ) / 2;
		$num_error_blocks = scalar( @error_blocks ) / 2;

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

sub write_makerules {

	$output_file = pfget( $Pf, "output_file" );
	$dest = pfget( $Pf, "dest" );

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

sub set_orig_enabled {

	foreach $capability ( keys( %capabilities ) ) {

		$orig_enabled{$capability} = $capabilities{$capability}{enable}{$Os};
	}
}

sub freeze_size {

	$Windows{"Main"}->resizable( 0, 0 );
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

sub init_config_File_menu {
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

sub init_localmake_File_menu {
	my( $w ) = @_;

	my( $menubutton, $filemenu );

	$menubutton = $w->Menubutton (
			    -text => 'File',
			    -pady => 0, 
			    -anchor => 'w', 
			    )->pack( -side => "left" );

	$filemenu = $menubutton->Menu( -tearoff => 0 );

	$filemenu->add( "command", -label => "Save compile log as...", -command => \&save_as );
	$filemenu->add( "command", -label => "Quit", -command => \&quit );

	$menubutton->configure( -menu => $filemenu );

	my( $button );

	$button = $w->Button( -text => "configure",
			      -bg => "green", 
			      -command => \&run_configure );

	$button->pack( -side => "right" );

	return;
}

sub init_localmake_menubar {
        my( $w ) = @_;

        my( $menubar );

        $menubar = $w->Frame( -relief => 'raised',
                              -borderwidth => 2 );

        init_localmake_File_menu( $menubar );

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

	if( defined( $Windows{"save_config"} ) ) {

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
}

sub commit_configuration {

	%config_macros = ();
	%config_capabilities = ();

	foreach $macro ( keys( %macros ) ) {

		$config_macros{$macro} = $$macro;
	}

	foreach $capability ( keys( %capabilities ) ) {

		$config_capabilities{$capability} = $capabilities{$capability}{enable}{$Os};
	}

	pfput( "macros", \%config_macros, $Pf_config );
	pfput( "capabilities", \%config_capabilities, $Pf_config );

	pfwrite( $Pf_config_file, $Pf_config );

	write_makerules();

	mark_configuration_unsaved( "false" );

	%macros_orig = %macros;

	set_orig_enabled();

	return;
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

			elog_complain( "Requested capability '$c' not defined in '$Pf_config'. " .
					"Stopping compilation.\n" );

			exit( -1 );
		}
	}

	if( ! pfget_boolean( $Pf, "capabilities{$c}{enable}{$Os}" ) && $mode eq "verify" ) {

		elog_complain( "Requested capability '$c' marked as disabled in '$Pf_config'.\n" .
			"Run localmake_config(1) (or edit '$Pf_config_file')\nto enable and configure " .
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
						"is not defined.\nRun localmake_config(1) (or edit '$Pf_config_file')\n" .
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

sub update_config_pf {

	if( -e $Pf_config_file && ! system( "grep extra_rules $Pf_config_file" ) ) {

		elog_complain( "The file '$Pf_config_file' is out of date. Moving it to '$Pf_config_file-' and updating." );

	} else {

		return 0;
	}

	system( "/bin/mv $Pf_config_file $Pf_config_file-" );

	commit_configuration();

	return 1;
}

sub save_and_quit {

	commit_configuration();

	quit();
}

sub run_configure {

	$Windows{"Main"}->gridForget( $Windows{"localmake_menubar"}, 
				      $Windows{"compile"} );

	destroy_followup_buttons();

	init_configure_window();

	return;
}

sub init_configure_window {

	$Windows{"Main"}->title( my_hostname() . ": localmake_config" );

	$Windows{"config_menubar"} = init_config_menubar( $Windows{"Main"} );

	$Windows{"config_menubar"}->grid( -row => 0,
				   -column => 0,
				   -sticky => 'new',
				 );

	$Windows{"capabilities"} = init_capabilities( $Windows{"Main"} );

	$Windows{"capabilities"}->grid( -row => 1,
				   	-column => 0,
				   	-sticky => 'nsew',
				 	);

	$Windows{"save_config"} = $Windows{"Main"}->Button( -text => "save configuration",
				       -command => \&commit_configuration, 
				       -bg => "gray",
				       -state => "disabled" );

	$Windows{"save_config"}->grid( -row => 2,
		  		       -column => 0,
		  		       -sticky => 'nsew',
		 			);

	$Windows{"Main"}->gridColumnconfigure( 0, -weight => 1 );

	$Windows{"Main"}->gridRowconfigure( 1, -weight => 1 );
	$Windows{"Main"}->gridRowconfigure( 2, -weight => 0 );

	MainLoop;
}

sub init_config_menubar {
	my( $w ) = @_;

	my( $menubar );

	$menubar = $w->Frame( -relief => 'raised', 
			      -borderwidth => 2 );

	init_config_File_menu( $menubar );

	my( $b ) = $menubar->Button( -text => "compile",
				     -bg => "green",
			       	     -command => \&run_compile );

	$b->pack( -side => "right" );

	return $menubar;
}

sub init_capabilities {
	my( $w ) = @_;

	my( $capabilities_window );
	my( @specs, @lefttop, @righttop, @leftbottom, @rightbottom, $i );

	$capabilities_window = $w->Frame( -relief => 'raised', 
					  -borderwidth => 2 );

	push( @specs, "notebook capabilities - 0,0 Capabilities" );

	foreach $c ( sort( keys( %capabilities ) ) ) {

		push( @specs, "notebookpage $c - xxx $c" );

		@lefttop = ();
		@righttop = ();
		@leftbottom = ();
		@rightbottom = ();

		push( @lefttop, "label np$c - 0,0 Capability:" );
		push( @lefttop, "label en$c - +,0 Status:" );

		push( @righttop, "button b$c - 0,0 Toggle" );
		push( @righttop, "button e$c - +,0 Explain '$c' capability" );

		foreach $m ( @{$capabilities{$c}{required_macros}} ) {

			push( @leftbottom, "entry e$c$m - +,0 $m { $macros{$m}{Description} }" );
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

	$capabilities_window->gridColumnconfigure( 0, -weight => 1 );

	foreach $c ( keys( %capabilities ) ) {

		$Widgets{"top$c"}->gridColumnconfigure( 0, -weight => 1 );
		$Widgets{"bottom$c"}->gridColumnconfigure( 0, -weight => 1 );

		$Widgets{"t$c"}->tagConfigure( 'failed', -foreground => "red" );
		$Widgets{"t$c"}->tagConfigure( 'passed', -foreground => "darkgreen" );
		$Widgets{"t$c"}->tagConfigure( 'disabled', -foreground => "grey30" );

		$Widgets{"np$c"}->parent()->parent()->gridColumnconfigure( 0, -weight => 1 );
		resticky( $Widgets{"np$c"}->parent(), "nsew" );

		resticky( $Widgets{"lefttop$c"}, "nsew" );
		resticky( $Widgets{"leftbottom$c"}, "nsew" );

		resticky( $Widgets{"righttop$c"}, "nsew" );

		resticky( $Widgets{"b$c"}, "nsew" );
		resticky( $Widgets{"e$c"}->parent(), "nsew" );

		resticky( $Widgets{"t$c"}, "nsew" );
		
		foreach $m ( @{$capabilities{$c}{required_macros}} ) {

			$Widgets{"e$c$m"}->parent()->parent()->gridColumnconfigure( 0, -weight => 1 );
			resticky( $Widgets{"e$c$m"}->parent(), "nsew" );

			resticky( $Widgets{"b$c$m"}, "nsew" );
			resticky( $Widgets{"e$c$m"}, "ew" );
			resticky( $Widgets{"b$c$m"}->parent(), "nsew" );
			resticky( $Widgets{"e$c$m"}->parent(), "ew" );
		}

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

sub run_compile {

	$Windows{"Main"}->gridForget( $Windows{"config_menubar"}, 
				      $Windows{"save_config"},
				      $Windows{"capabilities"} );

	$Windows{"Main"}->gridRowconfigure( 2, -weight => 0 );

	init_localmake_window();

	return;
}

sub init_localmake_window {

	$Windows{"Main"}->title( my_hostname() . ": localmake" );

	$Windows{"localmake_menubar"} = init_localmake_menubar( $Windows{"Main"} );

	$Windows{"localmake_menubar"}->grid( -row => 0, -column => 0, -sticky => "new" );

	$Windows{"compile"} = init_compile_window( $Windows{"Main"} );

	$Windows{"compile"}->grid( -row => 1, -column => 0, -sticky => "nsew" );

	$Windows{"Main"}->gridColumnconfigure( 0, -weight => 1 );

	$Windows{"Main"}->gridRowconfigure( 1, -weight => 1 );

	$Windows{"Main"}->afterIdle( \&compute_font_height );

	return;
}

sub init_window {
	use Tk;
	use Tk::Toplevel;
	use Tk::ROText;
	use Tk::Font;
	use Tk::FileSelect;
	use Tk::Dialog;
	use ptkform;
	use elog_gui;
	
	$Windows{"Main"} = MainWindow->new();

	$Windows{"Main"}->minsize(40, 20);

	elog_gui_init( MW => $Windows{"Main"} );
	elog_callback( "::elog_gui" );

	$Windows{"Main"}->bind( "<Control-c>", \&quit );
	$Windows{"Main"}->bind( "<Control-C>", \&quit );

	if( $opt_c ) {
		
		init_configure_window();

	} else {

		init_localmake_window();
	}

	MainLoop;
}

$Pf = "localmake";

$Pf_config = "localmake_config";

$localpf_dir = "$ENV{'ANTELOPE'}/local/data/pf";

$ENV{'PFPATH'} = "$localpf_dir:$ENV{'PFPATH'}";

$Pf_config_file = "$localpf_dir/$Pf_config.pf";

$Program = $0;
$Program =~ s@.*/@@;

elog_init( $Program, @ARGV );

if( !getopts( 'clp:s:tv' ) || scalar( @ARGV ) > 1 ) {

	elog_die( "Usage: localmake [-c] [-v] [-l] [-t] [-p pfname] [-s src_subdir] [module]\n" );
}

if( $opt_l && scalar( @ARGV ) > 0 ) {

	elog_complain( "Useless specification of module with -l option, ignoring module\n" );
} 

if( $opt_p ) {

	$Pf = $opt_p;
}

$Os = my_os();

$ROWidth = 132;

$Tarball_time_format = pfget( $Pf, "tarball_time_format" );
$Tar_command = pfget( $Pf, "tar_command" );
$Make_command = pfget( $Pf, "make_command" );

%macros = %{pfget($Pf,"macros")}; 
$header = pfget( $Pf, "header" );
$extra_rules = pfget( $Pf, "extra_rules" );
%capabilities = %{pfget( $Pf, "capabilities" )};

%macros_initial_config = %{pfget($Pf_config,"macros")};
%capabilities_initial_config = %{pfget($Pf_config,"capabilities")};

if( defined( $ENV{'MAKE'} ) ) {

	$Make_command = $ENV{'MAKE'};
}

if( ! update_config_pf() ) {

	set_initial_config();
}

%macros_orig = %macros;

set_orig_enabled();

set_macros();

write_makerules();

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
