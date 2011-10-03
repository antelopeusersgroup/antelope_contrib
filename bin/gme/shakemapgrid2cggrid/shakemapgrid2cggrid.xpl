#   Copyright (c) 2005 Lindquist Consulting, Inc.
#   All rights reserved. 
#                                                                     
#   Written by Dr. Kent Lindquist, Lindquist Consulting, Inc. 
# 
#   This software may be used freely in any way as long as 
#   the copyright statement above is not removed. 

use Getopt::Std ;
use Datascope;

my $pgm = $0 ; 
$pgm =~ s".*/"" ;
$Pfname = $pgm;

$qgridfmt = "as";
$event_mode = 0;
$Usage = "Usage: $pgm [-pf pffile] { grid | -event event_id }\n";

elog_init( $0, @ARGV );

while( $arg = shift( @ARGV ) ) {
	
	if( $arg eq "-event" ) { 

		if( $#ARGV < 0 ) { elog_die( $Usage ); }
		$event_id  = shift( @ARGV );
		$event_mode = 1;

	} elsif( $arg eq "-pf" ) { 

		if( $#ARGV < 0 ) { elog_die( $Usage ); }
		$Pfname  = shift( @ARGV );

	} else {

		$smgrid = $arg;
	}
}

if( ! $event_mode && ! defined( $smgrid ) ) {
	
	elog_die( $Usage );
}

$gridmode_cggrid_prefix = pfget( $Pfname, "gridmode_cggrid_prefix" );
@shakemap_grids = @{pfget( $Pfname, "shakemap_grids" )};
%event_mode = %{pfget( $Pfname, "event_mode" )};

foreach $line ( @shakemap_grids ) {

	( $gridtype, $units, $recipe ) = split( /\s+/, $line );

	push( @shakemap_gridtypes, $gridtype );

	$shakemap_grid_units{$gridtype} = $units;
	$shakemap_grid_recipes{$gridtype} = $recipe;
}

if( $event_mode ) {
	
	$smgrid = $event_mode{smgrid_dir};
	$smgrid =~ s/\$event_id/$event_id/;

	$smgrid = concatpaths( $smgrid, $event_mode{smgrid_name} );

	@db = dbopen( $event_mode{dbname}, "r+" );

	if( $db[0] < 0 ) {

		elog_die( "$dbname does not exist\n" );
	}

	@db = dbprocess( @db, "dbopen event",
		      	      "dbjoin origin",
		      	      "dbsubset orid == prefor" );

	@db = dblookup( @db, "", "", "evid", $event_id );

	$db[3] >= 0 || elog_die( "Couldn't find event $event_id\n" );

	( $orid, $time ) = dbgetv( @db, "orid", "time" );
}

if( ! -e "$smgrid" ) {

	elog_die( "ShakeMap grid file '$smgrid' not found. Bye.\n" );

} else {

	open( X, "$smgrid" );
}

$discard_header = <X>;

while( $line = <X> ) {
	
	@pieces = split( /\s+/, $line );

	if( scalar( @pieces ) < 2 ) {

		elog_die( "incomplete row in input file. Bye.\n" );

	} else {
	
		$lon = shift( @pieces );
		$lat = shift( @pieces );
	}

	$Lons{"$lon"}++;
	$Lats{"$lat"}++;

	# Ignore grids for missing columns:

	if( $#shakemap_gridtypes > $#pieces ) {

		splice( @shakemap_gridtypes, $#pieces - $#shakemap_gridtypes );
	}

	foreach $gridtype ( @shakemap_gridtypes ) {

		$Values{$gridtype}{"$lon:$lat"} = shift( @pieces );
	}
}

close( X );

@Lons = sort { $a <=> $b } keys( %Lons );
@Lats = sort { $a <=> $b } keys( %Lats );

$minlon = $Lons[0];
$maxlon = $Lons[$#Lons];

$minlat = $Lats[0];
$maxlat = $Lats[$#Lats];

$nlon = scalar( @Lons );
$nlat = scalar( @Lats );

$qdlon = ( $Lons[$#Lons] - $Lons[0] ) / ( $nlon - 1 );
$qdlat = ( $Lats[$#Lats] - $Lats[0] ) / ( $nlat - 1 );

foreach $gridtype ( @shakemap_gridtypes ) {

	$units = $shakemap_grid_units{$gridtype};

	$header = "qgrd1.0 $qgridfmt $minlon $maxlon $minlat $maxlat " .
	          "$qdlon $qdlat $nlon $nlat $units";

	$maxval = $Values{$gridtype}{"$Lons[0]:$Lats[0]"};

	foreach $lon ( @Lons ) {
   	  foreach $lat ( @Lats ) {
	
		if( $Values{$gridtype}{"$lon:$lat"} > $maxval ) {

			$maxval = $Values{$gridtype}{"$lon:$lat"};
		}
  	  }
	}

	if( $event_mode ) {

		$qgridname = "orid_$orid";

		$recipe = $shakemap_grid_recipes{$gridtype};

		@db = dblookup( @db, "", "qgrid", "", "" );

		if( $db[1] < 0 ) {

			elog_die( "Couldn't find qgrid table in database. Bye!\n" );
		}

		# Fake the trwfname functionality since it's not in perldb:

		$output_file = epoch2str( $time, $event_mode{output_file} );
		$output_file =~ s/{qgridname}/$qgridname/;
		$output_file =~ s/{recipe}/$recipe/;
		$output_file =~ s/{qgridtype}/$qgridtype/;
		$output_file =~ s/{qgridfmt}/$qgridfmt/;
		$output_file =~ s/{auth}/$auth/;

		( $dir, $dfile, $suffix ) = parsepath( $output_file );

		if( defined( $suffix ) && $suffix ne "" ) {
			
			$dfile .= ".$suffix";
		}

		$db[3] = dbaddv( @db, "qgridname", $qgridname,
				      "recipe", $recipe,
				      "qgridtype", $gridtype,
				      "time", $time,
				      "endtime", $time,
				      "minlat", $minlat, 
				      "maxlat", $maxlat, 
				      "minlon", $minlon, 
				      "maxlon", $maxlon, 
				      "qdlat", $qdlat, 
				      "qdlon", $qdlon,
				      "nlat", $nlat, 
				      "nlon", $nlon, 
				      "qgridfmt", $qgridfmt, 
				      "units", $units, 
				      "maxval", $maxval, 
				      "dir", $dir,
				      "dfile", $dfile,
				      "orid", $orid, 
				      "auth", $event_mode{auth} );

		if( $output_file !~ m@^/@ ) {

			$table_dirpath = dbquery( @db, dbTABLE_DIRNAME );
			$output_file = concatpaths( $table_dirpath, $output_file );
		}

	} else {

		$output_file = "$gridmode_cggrid_prefix$gridtype.$qgridfmt";
	}

	( $writedir, $writedfile, $writesuffix ) = parsepath( $output_file );

	makedir( $writedir );

	unless( open( C, ">$output_file" ) ) {

		dbdelete( @db );

		elog_die( "Failed to open $output_file for writing. Bye." );
	}

	print C "$header\n";

	foreach $lon ( @Lons ) {
   	  foreach $lat ( @Lats ) {
	
		print C "$lon $lat " . $Values{$gridtype}{"$lon:$lat"} . "\n";
  	  }
	}

	close( C );
}

exit( 0 );
