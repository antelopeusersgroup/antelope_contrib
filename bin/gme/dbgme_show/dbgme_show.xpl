#   Copyright (c) 2004 Boulder Real Time Technologies, Inc.           
#   All rights reserved. 
#                                                                     
#   Written by Dr. Kent Lindquist, Lindquist Consulting, Inc. 
# 
#   This software may be used freely in any way as long as 
#   the copyright statement above is not removed. 

use Datascope ;
use Tk;
require Tk::ROText;
use Getopt::Std ;

sub append_output {
	my( @text ) = @_;

	$rotext->insert( "end", @text );
	$rotext->yview( "end" );
	$mw->update;

	return;
}

sub append_cmdline {
	my( $text ) = @_;

	append_output( "$text", "greentag" );
	append_output( "\n" );

	return;
}

sub append_error {
	my( $text ) = @_;

	append_output( "$text", "redtag" );
	append_output( "\n" );

	return;
}

sub append_warning {
	my( $text ) = @_;

	append_output( "$text", "orangetag" );
	append_output( "\n" );

	return;
}

sub cleanup_and_quit {

	if( -e "$griddb" ) {
		unlink( $tmpgrd );
		unlink( $tmpgrad );
	}
	unlink( $tmpgme_orig );
	unlink( $tmpgme_resamp );

	if( $tmpstas ne "/dev/null" ) {
		unlink( $tmpstas );
	}

	if( $output_psfile_istemp ) {
		unlink( "$output_psfile" );
	}

	$mw->destroy();
}

sub watch_command_output {
	my( $cmd ) = @_;

	my( $errors ) = 0;

	append_cmdline( "Executing: $cmd\n" );

	open( F, "sh -c '$cmd' 2>&1 |" );

	while( ( $nbytes = sysread( F, $input, 8192 ) ) > 0 ) {
	
		if( map { $expr = $_; grep( /$expr/, $input ) } @error_messages ) {
			
			append_error( $input );

			$errors++;

		} else {

			append_output( "$input" );
		}

	}

	close( F );

	return $errors;
}

sub go {

	&set_maxsize_rotext();

	@helpers = (
		   "xyz2grd", 
		   "grdview", 
		   "grdcut", 
		   "grdsample", 
		   "grdinfo", 
		   "psxy",
		   "cat",
		   "gs",
		   "pnmcrop", 
		   "ppmquant",
		   "ppmtogif",
		   "cggrid_convert", 
		   "cggrid_info"
		   );

	foreach $helper ( @helpers ) {
		next if check_for_executable( $helper );
		append_error( "dbgme_show: Couldn't find the '$helper' executable on path." );
		return;
	}

	if( $opt_p ) {
		$Pf = $opt_p;
	} else {
		$Pf = "dbgme_show";
	}

	@db = dbopen_database( "$dbname", "r" );

	$wfmode = "none";
	@dbw = dblookup( @db, "", "wfmgme", "", "" );
	if( dbquery( @dbw, dbTABLE_PRESENT ) ) {

		$wfmode = "wfmgme";

	} else {

		@dbw = dblookup( @db, "", "wfmeas", "", "" );
		if( dbquery( @dbw, dbTABLE_PRESENT ) ) {
			$wfmode = "wfmeas";
		} 
	}

	if( $dbname ne "-" ) {

		@db = dblookup( @db, "", "qgrid", "", "" );
		if( $db[1] < 0 ) {
			append_error( "dbgme_show: Couldn't find qgrid table " .
			     "in $dbname." );
			return;
		}
	}

	$nrecs = dbquery( @db, "dbRECORD_COUNT" );
	if( $nrecs <= 0 ) {

		append_error( "dbgme_show: No records in $dbname.qgrid." );
		return;
	}

	@db = dbsort( @db, "-r", "lddate" );

	if( $opt_o && $opt_r ) {

		$orid = $opt_o;

		$db[3] = dbfind( @db, "orid == $orid && recipe == \"$opt_r\"", -1 );

		if( $db[3] < 0 ) {

			append_error( "dbgme_show: No grid for orid $orid and recipe " .
			     "$opt_r found in $dbname.qgrid." );
			return;
		}


	} elsif( $opt_o ) {

		$orid = $opt_o;

		$db[3] = dbfind( @db, "orid == $orid", -1 );

		if( $db[3] < 0 ) {

			append_error( "dbgme_show: No grid for orid $orid found " .
			     "in $dbname.qgrid." );
			return;
		}

	} elsif( $opt_r ) {

		$db[3] = dbfind( @db, "recipe == \"$opt_r\"", -1 );

		if( $db[3] < 0 ) {

			append_error( "dbgme_show: No grid for recipe $opt_r found " .
			     "in $dbname.qgrid." );
			return;
		}

	} else {

		$db[3] = 0;
	}

	if( ! defined( $orid ) || $orid < 0 ) {
		
		( $orid ) = dbgetv( @db, "orid" );
	}

	@dbo = dblookup( @db, "", "origin", "orid", "$orid" );

	if( $dbo[3] < 0 ) {

		append_warning( "dbgme_show: No hypocentral information " .
			"for orid $orid" );

		@dbo = dblookup( @db, "", "origin", "", "dbNULL" );
	}

	my( $ml, $origin_time ) = dbgetv( @dbo, "ml", "time" );

	my( $origin_timestring );
	if( $origin_time !~ /-9999999999.999/ ) {

		$origin_timestring = strtime( $origin_time ) . " UTC";

	} else {

		$origin_timestring = "";
	}

	$database_name = abspath( dbquery( @db, dbDATABASE_NAME ) );
	$description = "database: $database_name\n";
	$description .= "orid: $orid" .
			"\tOrigin Time: $origin_timestring\n"; 

	append_output( "Extracting qgrid from database...\n" );

	$qgridfile = dbextfile( @db );

	( $minlat, $maxlat, $minlon, $maxlon, $qdlat, $qdlon, 
	  $maxval, $recipe, $units, $qgridfmt ) 
			= dbgetv( @db,
				  "minlat",
				  "maxlat", 
				  "minlon", 
				  "maxlon",
				  "qdlat",
				  "qdlon",
				  "maxval",
				  "recipe",
				  "units",
				  "qgridfmt" );

	if( $ml != -999 ) {
		$description .= "ml: $ml\t";
	}

	$description .= "recipe: $recipe";

	$description .= "\tmax value: $maxval $units";

	$griddb = pfget( $Pf, "grid_database" );
	$projection = pfget( $Pf, "gmt_projection" );
	%recipe = %{&pfget( $Pf, "recipes{$recipe}" )};
	@error_messages = @{&pfget( $Pf, "error_messages" )};

	if( ( $griddb eq "" ) || ( ! -e "$griddb" ) ) {

		append_warning( "dbgme_show: Can't find grid database '$griddb'." );
	}

	if( ! defined( %recipe ) ) {

		append_error( "dbgme_show: No instructions defined " .
		     "for recipe $recipe." );
		return;

	} elsif( ! defined( $recipe{colormap} ) ) {

		append_error( "dbgme_show: No colormap file defined " .
		     "for recipe $recipe." );
		return;

	} elsif( ! -e "$recipe{colormap}" ) {

		append_error( "dbgme_show: Can't find colormap file " .
		     "'$colors{$recipe}' for recipe $recipe." );
		return;

	} else {

		$colors = $recipe{colormap};
	}

	if( -e "$griddb" ) {
		@dbgrid = dbopen( "$griddb", "r" );
		@dbgrid = dblookup( @dbgrid, "", "grids", "", "" );

		$dbgrid[3]=0; 
		( $dx, $dy ) = dbgetv( @dbgrid, "dx", "dy" );

		$tmpgrd = "/tmp/dbgme_show_tmp_$<_$$.grd";
		$tmpgrad = "/tmp/dbgme_show_tmp_$<_$$.grad";

	} else {

		$dx = $dy = 0.02;
	}

	$tmpgme_orig = "/tmp/dbgme_show_tmp_$<_$$.gme_orig";
	$tmpgme_resamp = "/tmp/dbgme_show_tmp_$<_$$.gme_resamp";
	$tmpstas = "/tmp/dbgme_show_tmp_$<_$$.tmpstas";

	append_output( "Examining qgrid...\n" );

	$cmd = "cggrid_info -s $qgridfile";

	$errors += watch_command_output( $cmd );

	if( $errors > 0 ) {

		append_error( "Stopping due to errors; can't continue." );

		return;
	}

	# Use values from the qgrid file itself to avoid rounding errors from database:

	( $minlon, $maxlon, $minlat, $maxlat,
	  $qdlon, $qdlat, $nlon, $nlat, $units ) = split( /\s+/, `$cmd` );

	chomp( $units );

	$Rectangle = "$minlon/$maxlon/$minlat/$maxlat";

	append_output( "Building commands...\n" );

	@commands = (
		"cggrid_convert $qgridfile | " .
		"xyz2grd -V -H1 -I$qdlon/$qdlat -G$tmpgme_orig -R$Rectangle",
		"grdsample -V -F $tmpgme_orig -G$tmpgme_resamp " .
		"-R$Rectangle -I$dx/$dy" );
	
	if( -e "$griddb" ) {

		push( @commands,
		"dbgmtgrid -V -R$Rectangle $griddb $tmpgrd",
		"grdgradient -V $tmpgrd -G$tmpgrad -A60 -Nt" );
	}

	if( defined( $recipe{meastype} ) && ( $wfmode eq "wfmeas" ) ) {

		$mymeastype = $recipe{meastype};

		append_output( "Finding measurements...\n" );

		@dbt = dbprocess( @db, 
				  "dbopen origin",
				  "dbsubset orid == $orid",
				  "dbjoin assoc",
				  "dbjoin arrival", 
				  "dbjoin site", 
				  "dbjoin wfmeas sta arrival.time#wfmeas.time::wfmeas.endtime",
				  "dbsubset meastype == \"$mymeastype\"" );
		
		$nrecs = dbquery( @dbt, "dbRECORD_COUNT" );

		if( $nrecs <= 0 ) {

			append_warning( "No measurements for orid $orid, " .
			     "meastype $recipe{meastype}." );

			$tmpstas = "/dev/null";

		} else {

			$dbt[3] = 0;
			($olat, $olon ) = dbgetv( @dbt, "origin.lat", "origin.lon" );

			open( F, ">$tmpstas" );
			for( $dbt[3] = 0; $dbt[3] < $nrecs; $dbt[3]++ ) {
				( $slat, $slon, $val1, $units1 ) = dbgetv( @dbt, 
					"site.lat", "site.lon", "val1", "units1" );
				$plotval = convert_units( $val1, $units1, $units );
				if( ! defined( $plotval ) ) {
					return;
				}
				print F "$slon $slat $plotval\n";
			}
			close( F );
		}

		if( $recipe{mode} eq "shade" ) {

			push( @commands, 
			"psbasemap -V -P -R$rectangle $projection -B2 -K > $output_psfile" );

			if( -e "$griddb" ) {
				push( @commands, 
				"grdview -V $tmpgrd $projection -Qi50 -I$tmpgrad " .
				"-C$colors -P -B2 -JZ0.5i -G$tmpgme_resamp " .
				"-O -K >> $output_psfile" );
			}

			push( @commands, 
			"cat $tmpstas | psxy -O -V -P -St0.2i -W3/0/0/0 " .
			"-C$colors -R$Rectangle $projection >> $output_psfile" );

		} else {

			$interval = $maxval / $recipe{nintervals};
			$interval = sprintf( "%e", $interval );
			$interval =~ s/^(\d).*(e.\d\d)$/$1$2/;
			$interval = sprintf( "%f", $interval );

			push( @commands, 
				"psbasemap -V -P -R$rectangle $projection " . 
				"-B2 -K > $output_psfile" );

			if( -e "$griddb" ) {
			push( @commands, 
				"grdimage -V $tmpgrd $projection -I$tmpgrad " .
				"-C$colors -B2 -P -O -K >> $output_psfile" );
			}

			push( @commands, 
			"pscoast -V $projection -R$Rectangle -Na/4/0/0/0 " .
			"-C0/0/255 -Df -O -K >> $output_psfile",
			"grdcontour -V $tmpgme_resamp $projection " .
			"-C$interval -N$units -W8/255/255/0 -Af12/255/255/0 " .
			"-Q8 -L$interval/9999 -O -K >> $output_psfile",
			"cat $tmpstas | psxy -O -K -V -P -St0.2i -W3/0/0/0 " .
			"-R$Rectangle $projection >> $output_psfile",
			"echo $olon $olat | psxy -O -V -P -Sa0.16 " .
			"-W6/255/0/0 -R$Rectangle $projection >> $output_psfile" );

		}

	} else {

		if( $recipe{mode} eq "shade" ) {

			push( @commands, 
			"psbasemap -V -P -R$rectangle $projection -B2 -K > $output_psfile" );

			if( -e "$griddb" ) {
				push( @commands, 
				"grdview -V $tmpgrd $projection -Qi50 -I$tmpgrad " .
				"-C$colors -P -B2 -JZ0.5i -G$tmpgme_resamp -O " .
				">> $output_psfile" );
			}
		} else {

			$interval = $maxval / $recipe{nintervals};
			$interval = sprintf( "%e", $interval );
			$interval =~ s/^(\d).*(e.\d\d)$/$1$2/;
			$interval = sprintf( "%f", $interval );

			push( @commands, 
				"psbasemap -V -P -R$rectangle $projection " . 
				"-B2 -K > $output_psfile" );

			if( -e "$griddb" ) {
			push( @commands, 
				"grdimage -V $tmpgrd $projection -I$tmpgrad " .
				"-C$colors -B2 -P -O -K >> $output_psfile" );
			}

			push( @commands, 
			"pscoast -V $projection -R$Rectangle -Na/4/0/0/0 " .
			"-C0/0/255 -Df -O -K >> $output_psfile",
			"grdcontour -V $tmpgme_resamp $projection " .
			"-C$interval -N$units -W8/255/255/0 -Af12/255/255/0 " .
			"-Q8 -L$interval/9999 -O >> $output_psfile" );
		}
	}

	append_output( "Running display commands...\n" );
	
	$errors = 0;

	while( ( $cmd = shift( @commands ) ) && $errors == 0 ) {
	
		$errors += watch_command_output( "$cmd" );
	}

	if( $errors > 0 ) {
		
		append_error( "Stopping due to errors; can't continue." );

		return;
	}

	$output_gif = $output_psfile;
	$output_gif =~ s/.ps$/.gif/;
	$pixels_per_inch = 100;
	$ncolors = 256;

	$cmd = "cat $output_psfile | gs -sOutputFile=- -q " .
   	       "-sDEVICE=ppm -r$pixels_per_inch - | pnmcrop - " .
   	       "| ppmquant $ncolors | ppmtogif > $output_gif";

	$errors += watch_command_output( $cmd );

	if( $errors > 0 ) {

		append_error( "Stopping due to errors; can't continue." );

		return;
	}

	append_output( "Loading image..." );

	$image = $mw->Photo( "myimage", file => "$output_gif" );
	append_output( "." );
	$canvas = $mw->Scrolled( Canvas, 
			 	-width => $image->width(),
			 	-height => $image->height(),
			 	-scrollbars => "sw", 
			 	-scrollregion => 
			   		[0,
				 	0,
				 	$image->width(),
				 	$image->height()]);
	append_output( "." );
	$canvas->createImage( 0, 0, -image => "myimage", -anchor => "nw" );
	append_output( "Done." );

	$toggle->configure( -state => "normal" );
	$toggle->invoke();

	$mw->update;

	$xscrollbar = $canvas->Subwidget( "xscrollbar" );
	$yscrollbar = $canvas->Subwidget( "yscrollbar" );

	( $first, $last ) = $xscrollbar->get();
	$canvas->xviewMoveto( 0.5 - ($last-$first)/2 );

	( $first, $last ) = $yscrollbar->get();
	$canvas->yviewMoveto( 0.5 - ($last-$first)/2 );

	$mw->update;

	&set_maxsize_canvas();
}

sub toggle_view {

	if( $toggle->cget( -text ) eq "Map" ) {

		$toggle->configure( -text => "Log" );

		$rotext->gridForget();

		$canvas->grid( -row => 1, -columnspan => 2, -sticky => "nsew" );

	} else {

		$toggle->configure( -text => "Map" );

		$canvas->gridForget();

		$rotext->grid( -row => 1, -columnspan => 2, -sticky => "nsew" );
	}

	$mw->gridRowconfigure( 1, -weight => 1 );
}

sub set_maxsize_rotext {
	my( $maxwidth, $maxheight );
	my( $minwidth, $minheight );
	my( $mintext ) = 200;
	my( $maxtext ) = 1000;

	$minwidth = $mintext +
		    $rotext_yscrollbar->width();

	$minheight = $mintext + 
		     $label->height() + 
		     $dismiss->height();

	$maxwidth = $maxtext +
		    $rotext_yscrollbar->width();

	$maxheight = $maxtext +
		     $label->height() + 
		     $dismiss->height();

	$mw->minsize( $minwidth, $minheight );
	$mw->maxsize( $maxwidth, $maxheight );

	return;
}

sub set_maxsize_canvas {
	my( $maxwidth, $maxheight );
	my( $minwidth, $minheight );
	my( $mincanvas ) = 200;

	$minwidth = $mincanvas +
		    $yscrollbar->width();

	$minheight = $mincanvas + 
		     $xscrollbar->height() +
		     $label->height() + 
		     $dismiss->height();

	$maxwidth = $image->width() +
		    $yscrollbar->width();

	$maxheight = $image->height() + 
		     $xscrollbar->height() +
		     $label->height() + 
		     $dismiss->height();

	$mw->minsize( $minwidth, $minheight );
	$mw->maxsize( $maxwidth, $maxheight );

	return;
}

sub convert_units {
	my( $val, $units_in, $units_out ) = @_;

	my( $result ) = `xunits $val $units_in $units_out`;

	my( $out ) = (split($result))[3];

	return $out;
}

sub check_for_executable {
        my( $program ) = @_;

        my( $ok ) = 0;

        foreach $path ( split( ':', $ENV{'PATH'} ) ) {
                if( -x "$path/$program" ) {
                        $ok = 1;
                        last;
                }
        }

        return $ok;
}

if ( ! getopts('p:o:r:') || @ARGV > 2 || @ARGV < 1 ) { 

	elog_die( "Usage: $0 [-p pffile] [-o orid] [-r recipe] " .
		     "database [psfile]\n" ); 

} else {

	if( @ARGV == 2 ) {
		$output_psfile = pop( @ARGV );
		$output_psfile_istemp = 0;
	} else {
		$output_psfile = "/tmp/dbgme_$<_$$.ps";
		$output_psfile_istemp = 1;
	}

	$dbname = pop( @ARGV );
}

$mw = MainWindow->new();
$label = $mw->Label( -textvariable => \$description, 
		     -relief => "sunken",
		     -background => "yellow" );
$label->grid( -row => 0, -columnspan => 2, -sticky => "nsew" );


$mw->gridRowconfigure( 0, -weight => 0 );

$rotext = $mw->Scrolled( ROText, -scrollbars => "w" );
$rotext->tagConfigure( "redtag", -background => "red" );
$rotext->tagConfigure( "orangetag", -background => "orange" );
$rotext->tagConfigure( "greentag", -background => "green" );
$rotext->grid( -row => 1, -columnspan => 2, -sticky => "nsew" );
$mw->gridRowconfigure( 1, -weight => 1 );

$rotext_yscrollbar = $rotext->Subwidget( "yscrollbar" );

$dismiss = $mw->Button( -text => "Dismiss", 
		       -background => "red",
		       -command => sub { \&cleanup_and_quit(); } );
$dismiss->grid( -row => 2, -column => 1, -sticky => "nsew" );
$mw->bind( "<Control-c>" => sub { $dismiss->invoke(); } );
$mw->bind( "<Control-C>" => sub { $dismiss->invoke(); } );

$mw->gridColumnconfigure( 1, -weight => 1 );

$toggle = $mw->Button( -text => "Map",
		       -background => "lightgreen",
		       -state => "disabled",
		       -command => sub { &toggle_view(); } );
$toggle->grid( -row => 2, -column => 0, -sticky => "nsew" );

$mw->gridRowconfigure( 2, -weight => 0 );

$mw->gridColumnconfigure( 0, -weight => 0 );

$mw->update;
$mw->afterIdle( \&go );

MainLoop();
