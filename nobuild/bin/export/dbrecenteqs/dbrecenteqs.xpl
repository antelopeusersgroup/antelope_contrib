
#
# dbrecenteqs
# 
# Kent Lindquist 
# Lindquist Consulting
# 2003

use Getopt::Std ;
require "dbrecenteqs.pl";
require "dbgmtgrid.pl";
require "winding.pl";
require "compass_from_azimuth.pl";
use Datascope;
use Image::Magick;
use XML::LibXML;
use XML::LibXSLT;
use XML::Writer;
use IO;

sub init_globals {

	setup_State();

	my( @params ) = (
		"dbrecenteqs_title",
		"dbrecenteqs_subdir",
		"institute_url",
		"institute_webdir",
		"institute_description",
		"institute_copyright",
		"page_refresh_seconds",
		"other_region_links",
		"region_string_prefix",
		"nearest_places",
		"credits",
		"authtrans",
		"keep_ndays",
		"max_num_eqs",
		"overview_maps",
		"focus_maps",
		"make_index_html",
		"html_suffix",
		"use_qgrids"
		);

	my( @path_params ) = (
		"wiggle",
		"background_graphic",
		"institute_logo",
		"region_phrases_database",
		"stockmaps_location",
		);
	
	foreach $param ( @params, @path_params ) {

		$State{$param} = pfget( $State{pf}, $param );
	}

	foreach $param ( @path_params ) {

		$State{$param} = datafile_abspath( $State{$param} );
	}
 
	if( ref( $State{authtrans} ) eq "HASH" ) {
		die( "Old-style authtrans table in dbrecenteqs.pf. " .
		     "Please convert to new syntax. Bye.\n" );
	}

	if( $State{use_qgrids} =~ m/y|yes|1|true|t/i ) {
		$State{use_qgrids} = 1;
	} else {
		$State{use_qgrids} = 0;
	}

	$State{"wiggle_filebase"} = `basename $State{"wiggle"}`;
	chomp( $State{"wiggle_filebase"} );
	$State{"institute_logo_filebase"} = `basename $State{"institute_logo"}`;
	chomp( $State{"institute_logo_filebase"} );
	$State{"background_graphic_filebase"} = `basename $State{"background_graphic"}`;
	chomp( $State{"background_graphic_filebase"} );

	if( ! defined( $State{overview_maps} ) || 
	    $#{$State{overview_maps}} < 0 ) {

		die( "Must have at least one entry in the " . 
		     "overview_maps &Tbl of the parameter file. Bye.\n" );
	}

	if( ! defined( $State{focus_maps} ) || 
	    $#{$State{focus_maps}} < 0 ) {

		die( "Must have at least one entry in the " . 
		     "focus_maps &Tbl of the parameter file. Bye.\n" );
	}

	$hashref = pfget_Mapspec( $State{pf}, "${$State{focus_maps}}[0]" );
	$State{main_focusmap_mapname} = $hashref->{mapname};

	if( ! -d $State{institute_webdir} ) {
		die( "The directory $State{institute_webdir} does not exist.\n" .
		     "Are you sure the parameter institute_webdir in\n" .
	  	     "dbrecenteqs.pf is set correctly for your\n" .
		     "installation? Bye.\n" );
	}

	$State{dbrecenteqs_dir} = 
	   concatpaths( $State{institute_webdir}, $State{dbrecenteqs_subdir} );

	if( $State{dbrecenteqs_dir} !~ m@/$@ ) { 
		$State{dbrecenteqs_dir} .= "/"; 
	}

	$State{dbrecenteqs_url} = $State{institute_url};
	if( $State{dbrecenteqs_url} !~ m@/$@ ) {
		$State{dbrecenteqs_url} .= "/";
	}
	$State{dbrecenteqs_url} .= $State{dbrecenteqs_subdir};

	if( $State{dbrecenteqs_url} !~ m@/$@ ) { 
		$State{dbrecenteqs_url} .= "/"; 
	}

	if( ! -d $State{dbrecenteqs_dir} ) {
		die( "The directory $State{dbrecenteqs_dir} does not exist. " .
		     "Please create it before proceeding. Bye.\n" );
	}

	$State{quakesubdir} = "quakes";
	mkdir( "$State{dbrecenteqs_dir}/$State{quakesubdir}", 0755 );

	$clientside_mapname = "clientsidemap";

	my( @db ) = dbopen( $State{region_phrases_database}, "r" );
	if( $db[0] < 0 ) {
		elog_complain "Couldn't open $State{region_phrases_database}\n";
		undef $State{region_phrases_database};
	}
	@db = dblookup( @db, "", "regions", "", "" );
	if( $db[1] < 0 ) {
		elog_complain
			"No regions table in $State{region_phrases_database}\n";
		undef $State{region_phrases_database};
	} else {
		$State{region_phrases_database} = \@db;
	}
}

sub die_if_already_running {

	my( @procs ) = split( /\n/,
		`pgrep -lf '/.*perl.*dbrecenteqs.*' | grep -v grep | egrep -v '^ *$$ '` );

	if( $#procs >= 0 ) {
		die( "dbrecenteqs: already running as \n" .
			join( "\n", @procs ) . "\nBye!\n" );
	}
}

sub cleanup_database {
	my( $dbname ) = @_;
	my( $cmd, $cutoff, $table );

	if( ! defined( $State{keep_ndays} ) || $State{keep_ndays} == 0 ) {
		if( $opt_v ) { 
			elog_notify 
		  	"dbrecenteqs: keep_ndays undefined or set to " .
		  	"zero (cleanup disabled). No cleanup initiated.\n";
		}
		return;
	} else {
		if( $opt_v ) { 
			elog_notify 
		  		"dbrecenteqs: Trimming $dbname to $State{keep_ndays} " .
		  		"most recent days.\n";
		}
	}

	$cutoff = str2epoch( "now" ) - $State{keep_ndays} * 86400;

	$cmd = "orb2db_msg $dbname pause";
	system( $cmd );

	foreach $table ("arrival", "detection", "origin" ) {

		if( $opt_v ) {
			elog_notify "dbrecenteqs: Cleaning $table table\n";
		}

		next if( ! -e "$dbname.$table" );

 		$cmd = "dbsubset $dbname.$table \"time < $cutoff\" | " .
 			"dbdelete - $table";
 		system( $cmd );
 		if( $? ) { elog_complain "\t command error $?\n"; }
	}

	foreach $table ( "assoc", "event", "mapassoc" ) {

		if( $opt_v ) {
			elog_notify "dbrecenteqs: Cleaning $table table\n";
		}

		next if( ! -e "$dbname.$table" );
		next if( ! -e "$dbname.origin" );

 		$cmd = "dbnojoin $dbname.$table origin | dbdelete - $table";
 		system( $cmd );
 		if( $? ) { elog_complain "\t command error $?\n"; }
	}

	foreach $table ( "webmaps" ) {

		if( $opt_v ) {
			elog_notify "dbrecenteqs: Cleaning $table table\n";
		}

		next if( ! -e "$dbname.$table" );
		next if( ! -e "$dbname.event" );

 		$cmd = "dbnojoin $dbname.$table event | " .
 			"dbsubset - \"evid != NULL\" | " .
 			"dbdelete - $table";
 		system( $cmd );
 		if( $? ) { elog_complain "\t command error $?\n"; }
	}

	dbclose( @db );

	@db = dbopen( $dbname, "r+" );

	$cmd = "orb2db_msg $dbname continue";
	system( $cmd );
}

sub auth2quakecolor {
	my( $auth ) = shift( @_ );

	my( $authtrans, $auth_href, $authoritative, $quakecolor ) = 
				translate_author( $auth );

	return $quakecolor;
}

sub age2quakecolor {
	my( $agecolors ) = shift( @_ );
	my( $age ) = shift( @_ );

	my( $agecolor ) = "gray";

	foreach $key ( sort
			    { $agecolors->{$a} <=>
			      $agecolors->{$b}
			    } 
			keys %{$agecolors} ) {

		$agecolor = $key;

		if( $age < $agecolors->{$key} ) {

			last;
		}
	}	

	return $agecolor;
}

sub mag2symsize {
	my( $magsizes ) = shift( @_ );
	my( $mag ) = shift( @_ );

	foreach $key ( sort { $a <=> $b } 
			keys %{$magsizes} ) {

		$symsize  = $magsizes->{$key};

		if( $mag < $key ) {

			last;
		}
	}	

	return $symsize;
}

sub cpt_color {
	my( %Mapspec ) = %{shift( @_ )};
	my( $value ) = pop( @_ );
	
	my( $color ) = "black";

	my( @cpt ) = @{$Mapspec{drape_color_palette}};

	$first = 1;

	while( $line = shift( @cpt ) ) {
		
		my( $z1, $r1, $g1, $b1, $z2, $r2, $g2, $b2 ) = split( " ", $line );

		if( $first && $value < $z1 ) {
			return $color;
		}

		if( $z1 <= $value && $value < $z2 ) {
			
			my( $r, $g, $b );

			my( $factor ) = ( $value - $z1 ) / ( $z2 - $z1 );

			$r = $r1 + $factor * ( $r2 - $r1 );
			$g = $g1 + $factor * ( $g2 - $g1 );
			$b = $b1 + $factor * ( $b2 - $b1 );

			$color = sprintf( "#%2x%2x%2x", $r, $g, $b );

			return $color;
		}

		$first = 0;
	}

	$color = sprintf( "#%2x%2x%2x", $r2, $g2, $b2 );

	return $color;
}

sub set_hypocenter_symbol {
	my( %Mapspec ) = %{shift( @_ )};
	my( $colormode ) = pop( @_ );
	my( @db ) = @_;

	my( $mag, $symsize, $symshape, $symcolor );

	my( $ml, $mb, $ms, $time, $auth ) = 
		dbgetv( @db, "ml", "mb", "ms", "time", "origin.auth" );

	my( $age ) = str2epoch( "now" ) - $time;

	if( $ml != -999 ) {
		$mag = $ml;
	} elsif( $mb != -999 ) {
		$mag = $mb;
	} elsif( $ms != -999 ) {
		$mag = $ms;
	} else {
		$mag = -999;
	}

	$symsize = mag2symsize( $Mapspec{quake_magsize_pixels}, $mag );

	$symshape = $Mapspec{quakeshape};

	if( $colormode eq "age" ) {

		$symcolor = age2quakecolor( $Mapspec{quake_agecolors}, $age );

	} elsif( $colormode eq "auth" ) {

		$symcolor = auth2quakecolor( $auth );

	} elsif( $colormode eq "prefor" ) {

		$symcolor = $Mapspec{prefor_quakecolor};

	} else {

		$symcolor = $Mapspec{nonprefor_quakecolor};
	}	

	return ( $symsize, $symshape, $symcolor );
}

sub mag_description {
	my( @db ) = @_;

	my( $ml, $mb, $ms ) = 
		dbgetv( @db, "ml", "mb", "ms" );

	if( $ml != -999 ) {

		return "$ml ML", $ml;

	} elsif( $mb != -999 ) {

		return "$mb Mb", $mb;

	} elsif( $ms != -999 ) {

		return "$ms Ms", $ms;

	} else {

		return "Unknown", -999;
	}
}

sub translate_author {
	my( $auth ) = @_;

	foreach $authref ( @{$State{authtrans}} ) {

		if( $auth =~ m/$authref->{regex}/ ) {

			return ( $authref->{"text"},
				 $authref->{"url"},
				 $authref->{"authoritative"}, 
				 $authref->{"quakecolor"} );
		}
	}

	return ( $auth, "", 0, "gray" );
}

sub station_vitals {
	my( $writer ) = shift( @_ );
	my( $mapname ) = pop( @_ );
	my( $event_url ) = pop( @_ );
	my( @db ) = @_;

	my( @mystations ) = ();

	my( $orid ) = 	dbgetv( @db, "origin.orid" );

	@db = dbprocess( @db, 
			   "dbopen mapassoc",
			   "dbsubset mapname == \"$mapname\"",
			   "dbsubset orid == $orid",
			   "dbsubset symtype == \"station\"",
			   "dbjoin origin",
			   "dbjoin arrival arid",
			   "dbjoin site sta",
			   "dbsort distance(site.lat,site.lon,origin.lat,origin.lon)" );

	my( $qgrid_units, $qgrid_extfile ) = undef;

	if( $State{use_qgrids} ) {

		@dbtest = dblookup( @db, "", "wfmgme", "", "" );
		$wfmgme_present = dbquery( @dbtest, dbTABLE_PRESENT );

		@dbtest = dblookup( @db, "", "wfmeas", "", "" );
		$wfmeas_present = dbquery( @dbtest, dbTABLE_PRESENT );

		if( $wfmgme_present ) {

			@db = dbprocess( @db, 
			     "dbjoin -o wfmgme arrival.time#wfmgme.time" );

		} elsif( $wfmeas_present ) {

			# SCAFFOLD
			# Need to handle the potential of multiple rows

			elog_complain 
			       "Warning: dbrecenteqs does not yet support" .
			       "wfmeas station-measurements\n";
		}

		@dbqgrid = dblookup( @db, "", "qgrid", "", "" );
		$dbqgrid[3] = dbfind( @dbqgrid, 
		 "orid == $orid && qgridtype == \"pga\"",
		 -1);

		if( $dbqgrid[3] >= 0 ) {

			$qgrid_pga_units = dbgetv( @dbqgrid, "units" );
			$qgrid_pga_extfile = dbextfile( @dbqgrid );
		}

		$dbqgrid[3] = dbfind( @dbqgrid, 
		 "orid == $orid && qgridtype == \"pgv\"",
		 -1);

		if( $dbqgrid[3] >= 0 ) {

			$qgrid_pgv_units = dbgetv( @dbqgrid, "units" );
			$qgrid_pgv_extfile = dbextfile( @dbqgrid );
		}
	}

	my( $nrecs ) = dbquery( @db, dbRECORD_COUNT );
	
	for( $db[3] = 0; $db[3] < $nrecs; $db[3]++ ) {

		my( $dist_km ) = 
			dbex_eval( @db, 
			   "111.195*distance(origin.lat,origin.lon,site.lat,site.lon)" );
		$dist_km = sprintf( "%.0f", $dist_km );

		my( $seaz ) = 
			dbex_eval( @db, 
			   "azimuth(site.lat,site.lon,origin.lat,origin.lon)" );
		$seaz = sprintf( "%.0f", $seaz );

		my( $sta, $arrtime, $lat, $lon, $iphase ) =
			dbgetv( @db, "sta", "arrival.time", 
				     "site.lat", "site.lon", "iphase" );
		my( $shape, $coords, $x, $y, $color ) = imagemap_symbol( @db );

		my( $sta_url ) = $event_url;
		$sta_url =~ s/.$State{html_suffix}$/_station_$sta.$State{html_suffix}/;

		my( $utc_arrtime ) 
			= epoch2str( $arrtime, "%m/%d/%Y %H:%M:%S.%s %Z" );

		$writer->startTag( "station", 
					"name" => "$sta",
					"detected" => "yes" );
		$writer->dataElement( "sta", "$sta" );
		$writer->dataElement( "url", "$sta_url" );
		$writer->dataElement( "arrtime", "$utc_arrtime" );
		$writer->dataElement( "iphase", "$iphase" );
		$writer->dataElement( "lat", "$lat" );
		$writer->dataElement( "lon", "$lon" );
		$writer->dataElement( "x", "$x" );
		$writer->dataElement( "y", "$y" );
		$writer->dataElement( "shape", "$shape" );
		$writer->dataElement( "color", "$color" );
		$writer->dataElement( "coords", "$coords" );
		$writer->dataElement( "dist_km", "$dist_km" );
		$writer->dataElement( "seaz", "$seaz" );

		if( $State{use_qgrids} && $wfmgme_present ) {

			my( $wfmgme_time, 
			    $pva, $trpva, $snrpva, 
			    $pvv, $trpvv, $snrpvv, 
			    $wa, $trwa, $snrwa, $chanwa ) = dbgetv( 
			    @db, "wfmgme.time", "pva", "trpva", "snrpva",
				 "pvv", "trpvv", "snrpvv", "wa", "trwa", 
				 "snrwa", "chanwa" );

			if( $pva == -9.000000e+99 ) {
				$pva = "";
				$pva_units = "";
				$snrpva = "";
				$trpva = "";
			} else {
				$pva = sprintf( "%.3f", $pva );
				$trpva = sprintf( "%.2f", $trpva );
				$pva_units = "milli-g";
			}

			if( $pvv == -9.000000e+99 ) {
				$pvv = "";
				$pvv_units = "";
				$snrpvv = "";
				$trpvv = "";
			} else {
				chomp( $pvv = `xunits -q -- '$pvv nm/sec' cm/sec` );
				$pvv = sprintf( "%.6f", $pvv );
				$trpvv = sprintf( "%.2f", $trpvv );
				$pvv_units = "cm/sec";
			}

			if( $wa == -9.000000e+99 ) {
				$wa = "";
				$wa_units = "";
				$snrwa = "";
				$trwa = "";
				$chanwa = "";
			} else {
				$wa = sprintf( "%.2f", abs( $wa ) );
				$trwa = sprintf( "%.2f", $trwa );
				$wa_units = "mm";
			}

			$writer->startTag( "measurement" );
			$writer->dataElement( "type", "wfmgme" );
			$writer->dataElement( "pva", $pva );
			$writer->dataElement( "pva_units", $pva_units );
			$writer->dataElement( "trpva", $trpva );
			$writer->dataElement( "snrpva", $snrpva );
			$writer->dataElement( "pvv", $pvv );
			$writer->dataElement( "pvv_units", $pvv_units );
			$writer->dataElement( "trpvv", $trpvv );
			$writer->dataElement( "snrpvv", $snrpvv );
			$writer->dataElement( "wa", $wa );
			$writer->dataElement( "wa_units", "mm" );
			$writer->dataElement( "trwa", $trwa );
			$writer->dataElement( "snrwa", $snrwa );
			$writer->dataElement( "chanwa", $chanwa );

			if( defined( $qgrid_pga_extfile ) ) {
				$qgrid_pga_val = 
				   `cggrid_probe -n -f '%.6f' $qgrid_pga_extfile $lon $lat`;

				if( $qgrid_pga_val ne "NaN" && 
				    $qgrid_pga_units eq "gravity" ) {
					$qgrid_pga_val *= 1000;
					$qgrid_pga_units = "milli-g";
				}
				$writer->dataElement( "qgrid_pga_val", $qgrid_pga_val );
				$writer->dataElement( "qgrid_pga_val_units", $qgrid_pga_units );
			}

			if( defined( $qgrid_pgv_extfile ) ) {
				$qgrid_pgv_val = 
				   `cggrid_probe -n -f '%.6f' $qgrid_pgv_extfile $lon $lat`;

				if( $qgrid_pgv_val ne "NaN" && 
				    $qgrid_pgv_units eq "nm/sec" ) {
					$qgrid_pgv_val /= 1000000;
					$qgrid_pgv_units = "cm/sec";
				}
				$writer->dataElement( "qgrid_pgv_val", $qgrid_pgv_val );
				$writer->dataElement( "qgrid_pgv_val_units", $qgrid_pgv_units );
			}

			$writer->endTag( "measurement" );

		}  elsif( $State{use_qgrids} && $wfmeas_present ) {

			# SCAFFOLD: not yet supported
		}

		$writer->endTag( "station" );

		push( @mystations, $sta );
	}

	@db = dbprocess( @db, 
			   "dbopen mapassoc",
			   "dbsubset mapname == \"$mapname\"",
			   "dbsubset orid == $orid",
			   "dbsubset symtype == \"station\"",
			   "dbjoin origin",
			   "dbsubset arid == NULL",
			   "dbjoin site sta",
			   "dbsort distance(site.lat,site.lon,origin.lat,origin.lon)" );
	$nrecs = dbquery( @db, dbRECORD_COUNT );

	for( $db[3] = 0; $db[3] < $nrecs; $db[3]++ ) {

		my( $dist_km ) = 
			dbex_eval( @db, 
			   "111.195*distance(origin.lat,origin.lon,site.lat,site.lon)" );
		$dist_km = sprintf( "%.0f", $dist_km );

		my( $seaz ) = 
			dbex_eval( @db, 
			   "azimuth(site.lat,site.lon,origin.lat,origin.lon)" );
		$seaz = sprintf( "%.0f", $seaz );

		my( $sta, $lat, $lon, ) =
			dbgetv( @db, "sta", "site.lat", "site.lon" );
		my( $shape, $coords, $x, $y, $color ) = imagemap_symbol( @db );

		my( $sta_url ) = $event_url;
		$sta_url =~ s/.$State{html_suffix}$/_station_$sta.$State{html_suffix}/;

		$writer->startTag( "station", 
					"name" => "$sta",
					"detected" => "no" );
		$writer->dataElement( "sta", "$sta" );
		$writer->dataElement( "url", "$sta_url" );
		$writer->dataElement( "lat", "$lat" );
		$writer->dataElement( "lon", "$lon" );
		$writer->dataElement( "x", "$x" );
		$writer->dataElement( "y", "$y" );
		$writer->dataElement( "shape", "$shape" );
		$writer->dataElement( "color", "$color" );
		$writer->dataElement( "coords", "$coords" );
		$writer->dataElement( "dist_km", "$dist_km" );
		$writer->dataElement( "seaz", "$seaz" );
		$writer->endTag( "station" );

		push( @mystations, $sta );
	}

	return @mystations;
}

sub hypocenter_vitals {
	my( $writer ) = shift( @_ );
	my( $type ) = pop( @_ );
	my( @db ) = @_;

	my( $lat, $lon, $depth, $time, $orid, $auth, $nass, $ndef ) = 	
		dbgetv( @db, "lat", "lon", "depth", "time", 
			     "origin.orid", "origin.auth", "nass", "ndef" );
	
	my( $qgrid_units, $qgrid_maxval, $qgrid_extfile ) = undef;

	if( $State{use_qgrids} ) {

		@dbqgrid = dblookup( @db, "", "qgrid", "", "" );
		$dbqgrid[3] = dbfind( @dbqgrid, 
		 "orid == $orid && recipe == \"$Focus_Mapspec{qgrid_recipe}\"",
		 -1);

		if( $dbqgrid[3] >= 0 ) {

			$qgrid_units = dbgetv( @dbqgrid, "units" );

			$qgrid_maxval = dbgetv( @dbqgrid, "maxval" );

			$qgrid_extfile = dbextfile( @dbqgrid );
		}
	}

	my( $name ) = "orid$orid";

	my( $authtrans, $auth_href, $authoritative ) = translate_author( $auth );

	$depth_km = sprintf( "%.0f", $depth );
	$depth_mi = sprintf( "%.0f", $depth_km / 1.609 );

	my( $local_day ) = epoch2str( $time, 
		"%A %B %o, %Y", $ENV{TZ} );

	my( $local_hour ) = epoch2str( $time, 
		"%I:%M %p %Z", $ENV{TZ} );

	my( $utc_time ) = epoch2str( $time, "%m/%d/%Y %H:%M:%S.%s %Z" );

	my( $mag_description, $mag_value ) = mag_description( @db );

	my( $depth_string ) = "$depth_mi miles ($depth_km km)";

	my( $shape, $coords, $x, $y, $color ) = imagemap_symbol( @db );

	$writer->startTag( "origin", "type" => "$type", "name" => "$name" );
	$writer->dataElement( "orid", "$orid" );
	$writer->dataElement( "localdate_string", "$local_day" );
	$writer->dataElement( "localtime_string", "$local_hour" );
	$writer->dataElement( "utc_string", "$utc_time" );
	$writer->dataElement( "mag_string", "$mag_description" );
	$writer->dataElement( "mag_value", "$mag_value" );
	$writer->dataElement( "lat", "$lat" );
	$writer->dataElement( "lon", "$lon" );
	$writer->dataElement( "depth_string", "$depth_string" );
	$writer->dataElement( "depth_km", "$depth_km" );
	$writer->dataElement( "nass", "$nass" );
	$writer->dataElement( "ndef", "$ndef" );
	$writer->dataElement( "auth_href", "$auth_href" );
	$writer->dataElement( "auth", "$authtrans" );
	$writer->dataElement( "authoritative", "$authoritative" );
	$writer->dataElement( "shape", "$shape" );
	$writer->dataElement( "coords", "$coords" );
	$writer->dataElement( "x", "$x" );
	$writer->dataElement( "y", "$y" );
	$writer->dataElement( "depth_km", "$depth_km" );
	$writer->dataElement( "color", "$color" );

	if( defined( $qgrid_units ) && defined( $qgrid_maxval ) ) {

		$writer->dataElement( "qgrid_units", $qgrid_units );
		$writer->dataElement( "qgrid_maxval", $qgrid_maxval );
		$writer->dataElement( "qgrid_extfile", $qgrid_extfile );
	}

	$writer->endTag( "origin" );
}

sub location_header_line {
	my( @db ) = splice( @_, 0, 4 );
	my( $lat, $lon, $orid ) = @_;

	my( $regname ) = quake_region( @db, $lat, $lon, $orid );

	if( $regname =~ /^(in|beneath|off|south of|west of|east of|north of) /i ) {

		return "$State{region_string_prefix} $regname";

	} else {

		return "$State{region_string_prefix}: " . $regname;
	}
}

sub quake_region {
	my( @db ) = splice( @_, 0, 4 );
	my( $lat, $lon, $orid ) = @_;
	my( @regions, $regname );

	@db = dblookup( @db, "", "quakeregions", "orid", $orid );
	
	if( $db[3] >= 0 ) {
		
		$regname = dbgetv( @db, "regname" );

	} elsif( ! defined( $State{region_phrases_database} ) ) {

		$regname = grname( $lat, $lon );
		dbaddv( @db, "orid", $orid, "regname", $regname );

	} else {

		@regions = get_containing_regions( 
				@{$State{region_phrases_database}},
				$lat, $lon );

		if( defined( $where = shift( @regions ) ) ) {
			$regname = $where;	
		} else {
			$regname = grname( $lat, $lon );
		}

		dbaddv( @db, "orid", $orid, "regname", $regname );
	}

	return $regname;
}

sub nearest_locations {
	my( $writer, $lat, $lon ) = @_;

	my( @db ) = dbopen( $State{"nearest_places"}->{"cities_dbname"}, "r" );
	@db = dblookup( @db, "", "places", "", "" );

	my( $expr ) = "distance(lat,lon,$lat,$lon)*111.195 <= " .
			$State{"nearest_places"}->{"max_dist_km"} .
			" || place =~ /" . 
			$State{"nearest_places"}->{"always_include"} .
			"/";
	@db = dbsubset( @db, $expr );

	@db = dbsort( @db, "distance(lat,lon,$lat,$lon)" );

	$writer->startTag( "nearest" );

	my( $nplaces ) = dbquery( @db, "dbRECORD_COUNT" );

	for( $db[3] = 0; $db[3] < $nplaces; $db[3]++ ) {

		my( $azimuth ) = 
			dbex_eval( @db, "azimuth(lat,lon,$lat,$lon)" );

		my( $compass ) = compass_from_azimuth( $azimuth );

		my( $dist_km ) = 
			dbex_eval( @db, "distance(lat,lon,$lat,$lon)*111.195" );
		my( $dist_mi ) = $dist_km / 1.6;
		$dist_km = sprintf( "%.0f", $dist_km );
		$dist_mi = sprintf( "%.0f", $dist_mi );

		my( $place ) = dbgetv( @db, "place" );

		$relative_location = "$dist_mi miles ($dist_km km) " .
			  	     "$compass of $place";

		$writer->dataElement( "nearby_place", "$relative_location" );
	}

	$writer->endTag( "nearest" );

	dbclose( @db );
}

sub create_focusmap_html {
	my( $evid ) = shift;
	my( @db ) = @_;

	@db = dbprocess( @db, "dbopen webmaps",
			      "dbsubset evid == $evid",
			      "dbsubset mapname == \"$Focus_Mapspec{file_basename}\"" );
	$db[3] = 0;

	my( $url, $dfile ) = dbgetv( @db, "url", "dfile" );
	my( $image_extfile ) = dbextfile( @db );
	my( $html_relpath ) = substr( $url, length( $State{dbrecenteqs_url} ) );
	my( $html_filename ) = 
		concatpaths( $State{dbrecenteqs_dir}, $html_relpath );
		
	my( $xml_filename ) = "$html_filename";
	$xml_filename =~ s/\..*//g;
	$xml_filename .= ".xml";
	my( $vrml_filename ) = $xml_filename;
	$vrml_filename =~ s/\.xml$/.wrl/;

	my( $vrml_url ) = $url;
	$vrml_url =~ s/\.[^.]*$//;
	$vrml_url .= ".wrl";

	chomp( my( $dir_relpath ) = `dirname $html_relpath` );

	@db = dbprocess( @db, "dbjoin event",
			      "dbjoin origin evid#evid",
			      "dbjoin -o mapassoc mapname origin.orid#mapassoc.orid",
			      "dbsubset symtype == \"origin\"" );

	my( @dbprefor ) = dbsubset( @db, "origin.orid==prefor" );
	$dbprefor[3] = 0;
	my( @dbnonprefors ) = dbsubset( @db, "origin.orid != prefor" );
	my( $nothers ) = dbquery( @dbnonprefors, "dbRECORD_COUNT" );

	my( $lat, $lon, $mapname, $orid ) = 
		dbgetv( @dbprefor, "lat", "lon", "mapname", "origin.orid" );

	my( $region_string ) = location_header_line( @dbprefor, $lat, $lon, $orid );
	my( $output ) = new IO::File( ">$xml_filename" );

	if( ! defined( $output ) ) {

		elog_complain
			"\n\t************************************\n" . 
			"\tWARNING: Skipping evid $evid--" .
			"\tFailed to open '$xml_filename'\n" .
			"\t************************************\n\n";
		return;
	}

	my( $writer ) = new XML::Writer( OUTPUT => $output, 
					 DATA_MODE => 'true', 
					 DATA_INDENT => 2 );

	$writer->xmlDecl();

	chomp( my( $stylesheet_basename ) = `basename $Focus_Mapspec{stylesheet}` );
	$writer->pi( 'xml-stylesheet', "href=\"$stylesheet_basename\" type=\"text/xsl\"" );

	$writer->startTag( "specific_quake", "name" => "$Focus_Mapspec{file_basename}" );

	$writer->dataElement( "page_title", 
			      "$region_string" );
	$writer->dataElement( "dbrecenteqs_base", 
			      "$State{dbrecenteqs_url}" );
	$writer->dataElement( "page_base", "$url" );
	$writer->dataElement( "page_refresh_seconds", 
			      "$State{page_refresh_seconds}" );
	$writer->dataElement( "wiggle_href", 
			      "$State{dbrecenteqs_url}" . "$State{wiggle_filebase}" );
	$writer->dataElement( "background_graphic_href", 
			      "$State{dbrecenteqs_url}" . "$State{background_graphic_filebase}" );
	if( $Focus_Mapspec{legend} ne "" ) {

		$writer->dataElement( "legend_url", 
			      		"$State{dbrecenteqs_url}" . 
			      		"$Focus_Mapspec{legend_filebase}" );
		$writer->dataElement( "legend_description", 
			      		"$Focus_Mapspec{legend_description}" );

	} else {

		$writer->dataElement( "legend_url", "" );
		$writer->dataElement( "legend_description", "" );
	}

	$writer->dataElement( "institute_url", 
			      "$State{institute_url}" );
	$writer->dataElement( "institute_logo_url",
		      	      "$State{dbrecenteqs_url}" .
			      "$State{institute_logo_filebase}" );
	$writer->dataElement( "institute_description", 
			      "$State{institute_description}" );
	$writer->dataElement( "copyright", 
			      "$State{institute_copyright}" );
	$writer->dataElement( "last_updated", 
		epoch2str( str2epoch( "now" ), "%A %B %d, %Y %l:%M %p %Z", "" ) ) ;

	$writer->dataElement( "vrml_url", $vrml_url );
	my( $abs_dbname ) = `abspath $dbname`;
	chomp( $abs_dbname );
	$writer->dataElement( "dbname", $abs_dbname );
	$writer->dataElement( "region_string", $region_string );

	$writer->dataElement( "subdir", "$dir_relpath" );

	my( $fms ) = Image::Magick->new();
	$fms->Read( $image_extfile );
	my( $width ) = $fms->Get( 'width' );
	my( $height ) = $fms->Get( 'height' );
	my( $ydelmax ) = pfget( $image_extfile, "$Focus_Mapspec{file_basename}\{ydelmax}" );
	my( $ydelmin ) = pfget( $image_extfile, "$Focus_Mapspec{file_basename}\{ydelmin}" );
	my( $ypixperkm ) = $height / ( ( $ydelmax - $ydelmin ) * 111.195 );

	$writer->startTag( "pixmap", "mapclass" => "focus" );
	$writer->dataElement( "file", "$dfile" );
	$writer->dataElement( "width", $width );
	$writer->dataElement( "height", $height );
	$writer->dataElement( "ypixperkm", $ypixperkm );
	$writer->dataElement( "clientside_mapname", "vitals" );
	$writer->endTag( "pixmap" );

	$writer->startTag( "origins" );

	hypocenter_vitals( $writer, @dbprefor, "prefor" );

	for( $dbnonprefors[3]=0; $dbnonprefors[3]<$nothers;$dbnonprefors[3]++ ){

		hypocenter_vitals( $writer, @dbnonprefors, "nonprefor" );
	}

	$writer->endTag( "origins" );

	$writer->startTag( "stations" );
	@mystations = station_vitals( $writer, @dbprefor, 
				      $url, $Focus_Mapspec{file_basename} );
	$writer->endTag( "stations" );

	$State{"nearest_places"}->{"cities_dbname"} =
	   datafile_abspath( $State{"nearest_places"}->{"cities_dbname"} );
	
	if( ! defined( $State{"nearest_places"}->{"cities_dbname"} ) ||
	      $State{"nearest_places"}->{"cities_dbname"} eq "" ) {

		elog_complain
			"\n\t************************************\n" . 
			"\tWARNING: Skipping cities--" .
			"no cities_dbname specified\n" .
			"\t************************************\n\n";

	} elsif( ! -e "$State{nearest_places}->{cities_dbname}" ) {

		elog_complain
			"\n\t************************************\n" . 
			"\tWARNING: Skipping cities--" .
			"$State{nearest_places}->{cities_dbname}.places " .
			" not found\n" .
			"\t************************************\n\n";

	} else {

		nearest_locations( $writer, $lat, $lon );
	}

	other_focusmap_links( $writer, @db, $evid, $url );

	other_map_links( $writer, @db, $mapname ),

	credits( $writer );

	$writer->endTag( "specific_quake" );

	print $output "\n";

	$output->close();

	xml_to_output( $xml_filename, 
		       $Focus_Mapspec{stylesheet}, 
		       $html_filename );

	if( -e "$Focus_Mapspec{vrml_stylesheet}" ) {

		xml_to_output( $xml_filename, 
			       $Focus_Mapspec{vrml_stylesheet}, 
			       $vrml_filename );
	}

	if( $Focus_Mapspec{plot_stations} &&
	    -e "$Focus_Mapspec{stations_stylesheet}" ) {

		foreach $sta ( @mystations ) {

			my( $stations_filename ) = $html_filename;
			$stations_filename =~ s/.$State{html_suffix}$/_station_$sta.$State{html_suffix}/;

			$tmp_stylesheet = 
			   concatpaths( $State{"workdir"}, 
					"sta_stylesheet_$<\_$$\_$sta.xsl" );


			open( S, ">$tmp_stylesheet" );
			print S "<?xml version=\"1.0\"?>";
			print S "<xsl:stylesheet xmlns:xsl=\"http://www.w3.org/1999/XSL/Transform\" version=\"1.0\">\n";
			print S "<xsl:output method=\"html\"/>";

			print S "<xsl:include href=\"$Focus_Mapspec{stations_stylesheet}\"/>";

			print S "<xsl:template match=\"/\">\n";
			print S "<xsl:apply-templates select=\"specific_quake/stations/station[\@name='$sta']\"/>\n";
			print S "</xsl:template>\n";

			print S "</xsl:stylesheet>\n";

			close S;

			xml_to_output( $xml_filename, 
				       $tmp_stylesheet,
			       	       $stations_filename );
			
			#unlink( $tmp_stylesheet );
		}
	}
}

sub create_focusmap {
	my( $evid ) = shift;
	my( @db ) = @_;

	@db = dbprocess( @db, "dbopen event",
			      "dbjoin origin", 
			      "dbsubset evid == $evid" );

	# plot prefor last:
	@db = dbsort( @db, "-r", "abs(orid - prefor)" );

	my( $nhypos ) = dbquery( @db, "dbRECORD_COUNT" );

	@dbprefor = dbsubset( @db, "orid == prefor" );
	$dbprefor[3] = 0; 

	my( $preftime, $preflat, $preflon, $prefor ) =
	  dbgetv( @dbprefor, "time", "lat", "lon", "orid" );

	if( $State{use_qgrids} ) {

		@dbqgrid = dblookup( @db, "", "qgrid", "", "" );
		$dbqgrid[3] = dbfind( @dbqgrid, 
		 "orid == $prefor && recipe == \"$Focus_Mapspec{qgrid_recipe}\"",
		 -1);

		if( $dbqgrid[3] >= 0 ) {

			$Focus_Mapspec{qgridfile} = dbextfile( @dbqgrid );

			$Focus_Mapspec{qgrid_dlat} = 
				dbgetv( @dbqgrid, "dlat" );

			$Focus_Mapspec{qgrid_dlon} = 
				dbgetv( @dbqgrid, "dlon" );

			$Focus_Mapspec{qgrid_minlat} = 
				dbgetv( @dbqgrid, "minlat" );

			$Focus_Mapspec{qgrid_maxlat} = 
				dbgetv( @dbqgrid, "maxlat" );

			$Focus_Mapspec{qgrid_minlon} = 
				dbgetv( @dbqgrid, "minlon" );

			$Focus_Mapspec{qgrid_maxlon} = 
				dbgetv( @dbqgrid, "maxlon" );

			$Focus_Mapspec{qgrid_nlat} = 
				dbgetv( @dbqgrid, "nlat" );

			$Focus_Mapspec{qgrid_nlat} = 
				dbgetv( @dbqgrid, "nlat" );

			$Focus_Mapspec{qgridtype} = 
				dbgetv( @dbqgrid, "qgridtype" );

			$Focus_Mapspec{qgrid_units} = 
				dbgetv( @dbqgrid, "units" );

			$Focus_Mapspec{qgrid_maxval} = 
				dbgetv( @dbqgrid, "maxval" );
		}
	}

	$Focus_Mapspec{dirname} = "evid$evid";
	$Focus_Mapspec{file_basename} = "$Focus_Mapspec{mapname}$evid";
	$Focus_Mapspec{lonc} = unwrapped_lon( \%Focus_Mapspec, $preflon );
	$Focus_Mapspec{latc} = $preflat;

	# Try to keep the directory names short enough for dir field
	my( $reldir ) = concatpaths( $State{quakesubdir}, 
	 	   epoch2str( $preftime, "%Y%j" ) .
			      "_$Focus_Mapspec{dirname}" );
	mkdir( concatpaths( $State{dbrecenteqs_dir}, $reldir ), 0755 );

	$Focus_Mapspec{"psfile"} = concatpaths( $State{"workdir"},
			"$Focus_Mapspec{file_basename}.ps" );
	$Focus_Mapspec{"pixfile"} = concatpaths( $State{dbrecenteqs_dir}, $reldir );
	$Focus_Mapspec{"pixfile"} = concatpaths( $Focus_Mapspec{"pixfile"},
			 "$Focus_Mapspec{file_basename}.$Focus_Mapspec{format}" );
	
	%Focus_Mapspec = %{set_projection( \%Focus_Mapspec )};
	%Focus_Mapspec = %{set_rectangles( \%Focus_Mapspec )};

	%Focus_Mapspec = %{create_map( \%Focus_Mapspec )};

	my( @dbwebmaps ) = dblookup( @db, "", "webmaps", "", "" );
	my( @dbscratch ) = dblookup( @dbwebmaps, "", "", "", "dbSCRATCH" );

	dbputv( @dbscratch, "mapname", $Focus_mapspec{file_basename} );

	my( $url ) = $State{dbrecenteqs_url} . 
		concatpaths( $reldir, "$Focus_Mapspec{file_basename}.$State{html_suffix}" );

	my( @recs ) = dbmatches( @dbscratch, @dbwebmaps, "webmaps", "mapname" );

	my( $dir ) = concatpaths( $State{dbrecenteqs_dir}, $reldir );
	my( $dfile ) = "$Focus_Mapspec{file_basename}.$Focus_Mapspec{format}";

	check_dir_dfile( @dbwebmaps, $dir, $dfile );

	if( defined( $rec = shift( @recs ) ) ) {
		
		$dbwebmaps[3] = $rec;

		dbputv( @dbwebmaps, 
    	    		"dir", $dir,
    	    		"dfile", $dfile,
    	    		"url", $url );
	} else {

		$dbwebmaps[3] = dbaddv( @dbwebmaps, 
	    		"mapname", $Focus_Mapspec{file_basename},
			"evid", $evid,
    	    		"dir", $dir,
    	    		"dfile", $dfile,
    	    		"url", $url );
	}

	my( $webmap_image ) = dbextfile( @dbwebmaps );

	my( $modified_image ) = $Focus_Mapspec{clean_image}->Clone();

	eliminate_from_mapassoc( @db, $Focus_Mapspec{file_basename} );

	for( $db[3]=0; $db[3] < $nhypos; $db[3]++ ) {

		my( $orid ) = dbgetv( @db, "orid" );

		my( @dbmapassoc ) = dblookup( @db, "", "mapassoc", "", "" );

		if( $Focus_Mapspec{plot_stations} ) {

			@dbstas = dbprocess( @db, 
				     	"dbopen origin", 
				     	"dbsubset orid == $orid", 
				     	"dbjoin assoc",
				     	"dbjoin arrival", 
				     	"dbjoin -o wfmgme arrival.time#wfmgme.time", 
				     	"dbjoin site" );


			$nstas = dbquery( @dbstas, dbRECORD_COUNT );

			for( $dbstas[3] = 0; $dbstas[3] < $nstas; $dbstas[3]++ ) {
				my( $lat, $lon, $sta, $arid ) = 
				  dbgetv( @dbstas, "site.lat", "site.lon",
						   "sta", "arid" );

				my( $symtype ) = "station";

				( $x, $y ) = latlon_to_xy( 
				   	$Focus_Mapspec{proj},
				   	$lat,
				   	$lon,
				   	$Focus_Mapspec{latc},
				   	$Focus_Mapspec{lonc},
				   	$Focus_Mapspec{xc},
				   	$Focus_Mapspec{yc},
				   	$Focus_Mapspec{xscale_pixperdeg},
				   	$Focus_Mapspec{yscale_pixperdeg},
				   	);

				my( $symsize ) = $Focus_Mapspec{station_size};
				my( $symtype ) = "station";
				my( $symshape ) = "triangle";
				
				my( $symcolor );

				if( $State{use_qgrids} && 
			    	$Focus_Mapspec{qgrid_nintervals} <= 0 &&
				$Focus_Mapspec{qgridtype} =~ /pga|pgv/ ) {

					( $pva, $pvv ) = 
					    dbgetv( @dbstas, "pva", "pvv" );

					if( $Focus_Mapspec{qgridtype} eq "pga" ) {
						if( $pva == -9.000000e+99 ) {

							$symcolor = "black";

						} else {
	chomp( $val = `xunits -q '$pva milligravity' $Focus_Mapspec{qgrid_units}` );
							$symcolor = 
					  		cpt_color( \%Focus_Mapspec, $val );
						}

					} else {

						if( $pvv == -9.000000e+99 ) {

							$symcolor = "black";

						} else {

	chomp( $val = `xunits -q '$pvv nm/sec' $Focus_Mapspec{qgrid_units}` );

							$symcolor = 
					  		cpt_color( \%Focus_Mapspec, $val );
						}
					}

				} else {

					$symcolor =
						$Focus_Mapspec{station_color};
				}

				my( $primitive ) = "polygon";

				my( $xll ) = $x - $symsize;
				my( $yll ) = $y + $symsize;
				my( $xlr ) = $x + $symsize;
				my( $ylr ) = $y + $symsize;
				my( $xtop ) = $x;
				my( $ytop ) = $y - $symsize;
				my( $points ) = "$xll,$yll $xlr,$ylr $xtop,$ytop";

				$modified_image->Draw(
						fill=>$symcolor,
						primitive=>$primitive,
						stroke=>'black',
						points=>$points );
	
				dbaddv( @dbmapassoc, 
				     "mapname", $Focus_Mapspec{file_basename},
			     	     "orid", $orid,
			     	     "arid", $arid,
			     	     "sta", $sta,
			     	     "x", $x,
			     	     "y", $y, 
			     	     "symsize", $symsize,
			     	     "symshape", $symshape,
			     	     "symcolor", $symcolor,
				     "symtype", $symtype );
			}

			@dbstas = dbsever( @dbstas, "site" );
			@dbsite = dblookup( @dbstas, "", "site", "", "" );
			@dbstas = dbnojoin( @dbsite, @dbstas );
			@dbstas = dbprocess( @dbstas, 
					"dbtheta origin orid == $orid" );
			$nstas = dbquery( @dbstas, dbRECORD_COUNT );

			for( $dbstas[3] = 0; $dbstas[3] < $nstas; $dbstas[3]++ ) {
				$arid = -1;
				my( $lat, $lon, $sta ) = 
				  dbgetv( @dbstas, "site.lat", "site.lon",
						   "sta" );

				my( $symtype ) = "station";

				( $x, $y ) = latlon_to_xy( 
				   	$Focus_Mapspec{proj},
				   	$lat,
				   	$lon,
				   	$Focus_Mapspec{latc},
				   	$Focus_Mapspec{lonc},
				   	$Focus_Mapspec{xc},
				   	$Focus_Mapspec{yc},
				   	$Focus_Mapspec{xscale_pixperdeg},
				   	$Focus_Mapspec{yscale_pixperdeg},
				   	);

				my( $symsize ) = $Focus_Mapspec{station_size};
				my( $symtype ) = "station";
				my( $symshape ) = "triangle";
				my( $symcolor ) = "black";

				my( $primitive ) = "polygon";

				my( $xll ) = $x - $symsize;
				my( $yll ) = $y + $symsize;
				my( $xlr ) = $x + $symsize;
				my( $ylr ) = $y + $symsize;
				my( $xtop ) = $x;
				my( $ytop ) = $y - $symsize;
				my( $points ) = "$xll,$yll $xlr,$ylr $xtop,$ytop";

				$modified_image->Draw(
						fill=>$symcolor,
						primitive=>$primitive,
						stroke=>'black',
						points=>$points );
	
				dbaddv( @dbmapassoc, 
				     "mapname", $Focus_Mapspec{file_basename},
			     	     "orid", $orid,
			     	     "arid", $arid,
			     	     "sta", $sta,
			     	     "x", $x,
			     	     "y", $y, 
			     	     "symsize", $symsize,
			     	     "symshape", $symshape,
			     	     "symcolor", $symcolor,
				     "symtype", $symtype );
			}
		}

		my( $symtype ) = "origin";

		my( $colormode );

		if( $Focus_Mapspec{colormode} eq "prefor" ) {

			$colormode = ( $orid == $prefor ) ? "prefor" :
								  "nonprefor"; 
		} else {

			$colormode = $Focus_Mapspec{colormode};
		}


		my( $linewidth ) = ( $orid == $prefor ) ? 
				$Focus_Mapspec{prefor_linewidth} : 1;

		my( $lat, $lon ) = dbgetv( @db, "lat", "lon" );

		( $x, $y ) = latlon_to_xy( 
				   $Focus_Mapspec{proj},
				   $lat,
				   $lon,
				   $Focus_Mapspec{latc},
				   $Focus_Mapspec{lonc},
				   $Focus_Mapspec{xc},
				   $Focus_Mapspec{yc},
				   $Focus_Mapspec{xscale_pixperdeg},
				   $Focus_Mapspec{yscale_pixperdeg},
				   );

		( $symsize, $symshape, $symcolor ) = 
	  		set_hypocenter_symbol( \%Focus_Mapspec, @db, $colormode );

		if( $symshape eq "square" ) {
			$primitive = "rectangle";
			$xul = $x - $symsize;
			$yul = $y - $symsize;
			$xlr = $x + $symsize;
			$ylr = $y + $symsize;
			$points = "$xul,$yul $xlr,$ylr";
		} else {
			die( "symbol shape $symshape not understood\n" );
		}

		$modified_image->Draw(
				fill=>$symcolor,
				primitive=>$primitive,
				stroke=>'black',
				points=>$points,
				linewidth=>$linewidth );
	
		dbaddv( @dbmapassoc, "mapname", $Focus_Mapspec{file_basename},
			     	     "orid", $orid,
			     	     "x", $x,
			     	     "y", $y, 
			     	     "symsize", $symsize,
			     	     "symshape", $symshape,
			     	     "symcolor", $symcolor,
				     "symtype", $symtype );
	}

	$modified_image->Write(filename=>$webmap_image);

	undef( $modified_image );
	undef( $Focus_Mapspec{clean_image} );

	if( $Focus_Mapspec{legend} ne "" &&
	    ! -e "$State{dbrecenteqs_dir}/$Focus_Mapspec{legend_filebase}" ) {
		system( "/bin/cp $Focus_Mapspec{legend} $State{dbrecenteqs_dir}" );
	}
}

sub stockmap_earthquake_xml {
	my( $writer ) = shift( @_ );
	my( $mapname ) = pop( @_ );
	my( @db ) = @_;

	my( $base ) = $State{main_focusmap_mapname};

	# Go Backwards in time: most recent quake first
	@db = dbprocess( @db, 
		       "dbopen mapassoc",
		       "dbsubset mapname == \"$mapname\"",
		       "dbsubset symtype == \"origin\"",
		       "dbjoin origin",
		       "dbjoin event",
		       "dbsort -r time",
		       "dbjoin webmaps evid",
		       "dbsubset webmaps.mapname =~ /^$base\[0-9]+\$/",
		       "dbsubset origin.orid == prefor" );

	my( $nsymbols ) = dbquery( @db, "dbRECORD_COUNT" );

	$writer->startTag( "quakelist" );

	for( $db[3]=0; $db[3]<$nsymbols; $db[3]++ ) { 

		my( $lat, $lon, $depth, $time, $orid, $url ) = 
			dbgetv( @db, "lat", "lon", "depth", 
				     "time", "origin.orid", "url" );

		$vrml_url = $url;
		$vrml_url =~ s/$State{html_suffix}$/wrl/;

		my( $mag_description, $mag_value ) = mag_description( @db );

		my( $local_time ) = epoch2str( $time, 
		"%I:%M %p %Z %A %B %o, %Y", $ENV{TZ} );

		my( $utc_time ) = epoch2str( $time, 
		"%I:%M %p %Z %A %B %o, %Y" );

		my( $region ) = quake_region( @db, $lat, $lon, $orid );

		my( $shape, $coords, $x, $y, $color ) = imagemap_symbol( @db );

		$writer->startTag( "quake" );

		$writer->dataElement( "href", "$url" );
		$writer->dataElement( "vrml_url", "$vrml_url" );
		$writer->dataElement( "localtime_string", "$local_time" );
		$writer->dataElement( "utctime_string", "$utc_time" );
		$writer->dataElement( "time", "$time" );
		$writer->dataElement( "mag_string", "$mag_description" );
		$writer->dataElement( "mag_value", "$mag_value" );
		$writer->dataElement( "region_string", "$region" );
		$writer->dataElement( "shape", "$shape" );
		$writer->dataElement( "coords", "$coords" );
		$writer->dataElement( "lat", "$lat" );
		$writer->dataElement( "lon", "$lon" );
		$writer->dataElement( "x", "$x" );
		$writer->dataElement( "y", "$y" );
		$writer->dataElement( "depth_km", "$depth" );
		$writer->dataElement( "color", "$color" );

		$writer->endTag( "quake" );
	}

	$writer->endTag( "quakelist" );
}

sub other_region_links {
	my( $writer ) = pop( @_ );
	my( $key, $val );

	$writer->startTag( "other_regions" );

	foreach $key ( keys %{$State{other_region_links}} ) {

		$val = $State{other_region_links}->{$key};

		$writer->startTag( "region" );

		$writer->dataElement( "href", "$val" );
		$writer->dataElement( "text", "$key" );

		$writer->endTag( "region" );
	}

	$writer->endTag( "other_regions" );
}

sub other_focusmap_links {
	my( $writer ) = shift( @_ );
	my( $urlmain ) = pop( @_ );
	my( $evid ) = pop( @_ );
	my( @db ) = @_;

	my( $urlhead ) = $urlmain;
	$urlhead =~ s@/[^/]*$@/@;

	$writer->startTag( "focus_maps" );

	foreach $map ( @{$State{focus_maps}} ) {
	
		$hashref = pfget_Mapspec( $State{pf}, "$map" );

		my( $mapname ) = $hashref->{mapname};

		my( $maplink );

		if( defined( $hashref->{description} ) ) {
			$maplink = $hashref->{description};
		} else {
			$maplink = $mapname;
		}

		if( $urlhead !~ m@/$@ ) { $urlhead .= "/"; }
		my( $url ) = "$urlhead" . "$mapname$evid.$State{html_suffix}";

		$writer->startTag( "focusmap" );
		$writer->dataElement( "href", $url );
		$writer->dataElement( "text", $maplink );
		$writer->endTag( "focusmap" );
	}

	$writer->endTag( "focus_maps" );
}

sub other_map_links {
	my( $writer ) = shift( @_ );
	my( $mapname ) = pop( @_ );
	my( @db ) = @_;

	@db = dbprocess( @db,
			"dbopen mapstock",
			"dbjoin webmaps",
			"dbsubset mapname != \"$mapname\"",
			"dbsubset mapclass != \"detail\"",
			"dbsort -u mapname" );

	my( $nmaps ) = dbquery( @db, "dbRECORD_COUNT" );

	if( $nmaps <= 0 ) { return ""; }

	$writer->startTag( "other_maps" );

	for( $db[3] = 0; $db[3] < $nmaps; $db[3]++ ) {

		my( $mapname, $mapclass, $url ) = 
			dbgetv( @db, "mapname", "mapclass", "url" );

		my( $maplink );

		if( defined( $State{maphashes}->{$mapname}->{description} ) &&
		    $State{maphashes}->{$mapname}->{description} ne "" ) {

			$maplink = $State{maphashes}->{$mapname}->{description};

		} else {

			$maplink = $mapname;
		}

		$writer->startTag( "othermap" );
		$writer->dataElement( "href", $url );
		$writer->dataElement( "text", $maplink );
		$writer->endTag( "othermap" );
	}
	
	$writer->endTag( "other_maps" );
}

sub imagemap_symbol {
	my( @db ) = splice( @_, 0, 4 );
	my( $shape, $coords );
	my( $primitive, $xul, $yul, $xlr, $ylr );

	my( $x, $y, $symsize, $symshape, $symcolor ) = 
	   dbgetv( @db, "x", "y", "symsize", "symshape", "symcolor" );

	if( $symshape eq "square" ) {
		$primitive = "rect";
		$xul = $x - $symsize;
		$yul = $y - $symsize;
		$xlr = $x + $symsize;
		$ylr = $y + $symsize;

		$shape = "$primitive";
		$coords = "$xul,$yul,$xlr,$ylr";

	} elsif( $symshape eq "triangle" ) {
		
		$primitive = "poly";

		$xll = $x - $symsize;
		$yll = $y + $symsize;
		$xlr = $x + $symsize;
		$ylr = $y + $symsize;
		$xtop = $x;
		$ytop = $y - $symsize;
		$shape = "$primitive";
		$coords = "$xll,$yll,$xlr,$ylr,$xtop,$ytop";

	} else {
		die( "symbol shape $symshape not understood\n" );
	}

	return ( $shape, $coords, $x, $y, $symcolor );
}

sub create_stockmap_html {
	my( @db ) = @_;

	my( $mapname ) = dbgetv( @db, "mapname" );

	my( @dbt ) = dblookup( @db, "", "mapstock", "mapname", "$mapname" );
	my( $mapclass, $width, $height, $updellat, $downdellat ) = 
		dbgetv( @dbt, "mapclass", "width", "height", "updellat", "downdellat" );

	my( $ypixperkm ) = $height / ( ( $updellat - $downdellat ) * 111.195 );
	
	if( $opt_v ) {
		elog_notify "dbrecenteqs: Updating html for $mapclass map $mapname\n";
	}

	my( @db ) = dbprocess( @db, 
			       "dbopen webmaps",
			       "dbsubset mapname == \"$mapname\"" );

	$db[3] = 0;
	my( $url ) = dbgetv( @db, "url" );
	my( $html_relpath ) = substr( $url, length( $State{dbrecenteqs_url} ) );
	my( $html_filename ) = 
		concatpaths( $State{dbrecenteqs_dir}, $html_relpath );
	my( $html_temp_filename ) = $html_filename;
	$html_temp_filename =~ s@/([^/]*)$@/-$1@;

	my( $xml_filename ) = $html_filename;
	$xml_filename =~ s/\..*//;
	$xml_filename .= ".xml";

	my( $vrml_url ) = $url;
	$vrml_url =~ s/\.[^.]*$//;
	$vrml_url .= ".wrl";

	my( $vrml_filename ) = $xml_filename;
	$vrml_filename =~ s/.xml$/.wrl/;

	my( $main_index_filename ) = $html_filename;
	$main_index_filename =~ s@/([^/]*)$@/index.$State{html_suffix}@;

	my( $image_relpath ) = dbextfile( @db );
	$image_relpath = substr( $image_relpath, 	
				 length( $State{dbrecenteqs_dir} ) );

	my( $output ) = new IO::File( ">$xml_filename" );

	my( $writer ) = new XML::Writer( OUTPUT => $output, 
					 DATA_MODE => 'true', 
					 DATA_INDENT => 2 );

	my( $stylesheet ) = $State{maphashes}->{$mapname}->{stylesheet};
	chomp( my( $stylesheet_basename ) = `basename $stylesheet` );
	$writer->xmlDecl();
	$writer->pi( 'xml-stylesheet', "href=\"$stylesheet_basename\" type=\"text/xsl\"" );

	$writer->startTag( "dbrecenteqs_main" );

	$writer->dataElement( "page_title", 
			      "$State{dbrecenteqs_title}" );
	$writer->dataElement( "page_base", 
			      "$State{dbrecenteqs_url}" );
	$writer->dataElement( "dbrecenteqs_base", 
			      "$State{dbrecenteqs_url}" );
	$writer->dataElement( "page_refresh_seconds", 
			      "$State{page_refresh_seconds}" );
	$writer->dataElement( "map_description", 
			      "$State{maphashes}->{$mapname}->{description}" );
	$writer->dataElement( "wiggle_href", 
			      "$State{dbrecenteqs_url}" .
				"$State{wiggle_filebase}" );
	$writer->dataElement( "background_graphic_href", 
			      "$State{dbrecenteqs_url}" .
				"$State{background_graphic_filebase}" );
	$writer->dataElement( "institute_url", 
		      		"$State{institute_url}" );

	if( $State{maphashes}->{$mapname}->{legend} ne "" ) {

		$writer->dataElement( "legend_url", 
			      		"$State{dbrecenteqs_url}" .
					"$State{maphashes}->{$mapname}->{legend_filebase}" );

		$writer->dataElement( "legend_description", 
			      		"$State{maphashes}->{$mapname}->{legend_description}" );
	} else {

		$writer->dataElement( "legend_url", "" );
		$writer->dataElement( "legend_description", "" );
	}

	$writer->dataElement( "institute_logo_url",
		      	      "$State{dbrecenteqs_url}" .
			      "$State{institute_logo_filebase}" );
	$writer->dataElement( "institute_description", 
			      "$State{institute_description}" );
	$writer->dataElement( "copyright", 
			      "$State{institute_copyright}" );
	my( $abs_dbname ) = `abspath $dbname`;
	chomp( $abs_dbname );
	$writer->dataElement( "dbname", $abs_dbname );
	$writer->dataElement( "vrml_url", $vrml_url );
	$writer->dataElement( "last_updated", 
		epoch2str( str2epoch( "now" ), "%A %B %d, %Y %l:%M %p %Z", "" ) ) ;

	$writer->startTag( "pixmap", "mapclass" => "$mapclass" );
	$writer->dataElement( "file", "$image_relpath" );
	$writer->dataElement( "width", "$width" );
	$writer->dataElement( "height", "$height" );
	$writer->dataElement( "ypixperkm", "$ypixperkm" );
	$writer->dataElement( "clientside_mapname", "$clientside_mapname" );
	$writer->endTag( "pixmap" );

	other_map_links( $writer, @db, $mapname ),

	stockmap_earthquake_xml( $writer, @db, $mapname ),

	other_region_links( $writer );

	credits( $writer );

	$writer->endTag( "dbrecenteqs_main" );

	print $output "\n";
	
	$output->close();

	xml_to_output( $xml_filename, 
		     $stylesheet,
		     $html_temp_filename );

	system( "/bin/mv $html_temp_filename $html_filename" );

	if( ( $State{make_index_html} =~ m/y|yes|1|true|t/i ) &&
	    ( $mapname eq $State{main_index_mapname} ) &&
	    ( $html_filename !~ m@.*/index.$State{html_suffix}$@ ) ) {

		system( "/usr/bin/cp $html_filename $main_index_filename" );
	}

	my( $vrml_stylesheet ) = $State{maphashes}->{$mapname}->{vrml_stylesheet};

	if( -e "$vrml_stylesheet" ) {

		xml_to_output( $xml_filename, 
		     	$vrml_stylesheet,
		     	$vrml_filename );
	}
}

sub credits {
	my( $writer ) = pop( @_ );
	my( $key, $val );

	$writer->startTag( "credits" );

	foreach $key ( keys %{$State{credits}} ) {

		$val = $State{credits}->{$key};

		$writer->startTag( "credit" );

		$writer->dataElement( "href", "$val" );
		$writer->dataElement( "text", "$key" );

		$writer->endTag( "credit" );
	}

	$writer->endTag( "credits" );
}

sub eliminate_from_mapassoc {
	my( $mapname ) = pop( @_ );
	my( @db ) = @_;

	my( @dbmapassoc ) = dblookup( @db, "", "mapassoc", "", "dbALL" );
	my( @dbscratch ) = dblookup( @dbmapassoc, "", "", "", "dbSCRATCH" );
	dbputv( @dbscratch, "mapname", $mapname );
	my( @records ) = dbmatches( @dbscratch, @dbmapassoc, 
		"mapassoc", "mapname" ); 
	while( defined( $rec = pop( @records ) ) ) {
		$dbmapassoc[3] = $rec;
		dbmark( @dbmapassoc );
	}
	@dbmapassoc = dblookup( @db, "", "mapassoc", "", "" );
}

sub create_stockmap_entry {
	my( @db ) = @_;

	my( $mapname ) = dbgetv( @db, "mapname" );

	my( @dbbundle ) = split( ' ', dbgetv( @db, "bundle" ) );

	@db = dblookup( @dbbundle, "", "", "dbALL", "" );

	my( $mapclass ) = dbgetv( @db, "mapclass" );

	my( @dbwebmaps ) = dblookup( @db, "", "webmaps", "", "dbALL" );
	my( @dbscratch ) = dblookup( @dbwebmaps, "", "", "", "dbSCRATCH" );
	dbputv( @dbscratch, "mapname", $mapname );
	my( @recs ) = dbmatches( @dbscratch, @dbwebmaps, "webmaps", "mapname" );

	my( $url ) = $State{dbrecenteqs_url} . "$mapname.$State{html_suffix}";

	my( $dir ) = "placeholder"; # Not very elegant
	my( $dfile ) = $mapname;

	check_dir_dfile( @dbwebmaps, $dir, $dfile );

	if( defined( $rec = shift( @recs ) ) ) {
		
		$dbwebmaps[3] = $rec;

		dbputv( @dbwebmaps, "url", $url );

	} else {

		$dbwebmaps[3] = dbaddv( @dbwebmaps, 
	    		"mapname", $mapname,
    	    		"dir", $dir,
    	    		"dfile", $dfile,
    	    		"url", $url );
	}
}

sub update_stockmap {
	my( @db ) = @_;
	my( %Mapspec );

	my( $mapname ) = dbgetv( @db, "mapname" );

	if( $opt_v ) { 
		elog_notify "dbrecenteqs: Updating map $mapname\n";
	}

	my( @dbbundle ) = split( ' ', dbgetv( @db, "bundle" ) );

	@db = dblookup( @dbbundle, "", "", "dbALL", "" );
	%Mapspec = %{read_map_from_db( @db )};

	my( $modified_image ) = $Mapspec{clean_image}->Clone();

	eliminate_from_mapassoc( @db, $mapname );
	@dbmapassoc = dblookup( @db, "", "mapassoc", "", "" );

	if( grep( /orid/, dbquery( @dbbundle, "dbTABLE_FIELDS" ) ) ) {

		my( $startrec ) = $dbbundle[3];
		my( $endrec ) = $dbbundle[2];

		if( defined( $State{max_num_eqs} ) && 
		    ( $State{max_num_eqs} > 0 ) &&
		    ( ( $endrec - $startrec ) > $State{max_num_eqs} ) ) {

		    if( $opt_v ) {
			elog_notify "dbrecenteqs: limiting $mapname to $State{max_num_eqs} earthquakes\n";
		    }

		    $startrec = $endrec - $State{max_num_eqs};
		}

		for( $db[3] = $startrec; $db[3] < $endrec; $db[3]++ ) {

			my( $orid, $proj, $lat, $lon, $latc, $lonc, $xc, $yc, 
    				$xscale_pixperdeg, $yscale_pixperdeg ) = 
    				dbgetv( @db, "orid", "proj", "lat", "lon", 
		 			"latc", "lonc",
		 			"xc", "yc", 
		 			"xpixperdeg", "ypixperdeg" );

			( $x, $y ) = latlon_to_xy( $proj, $lat, $lon, 
					   	$latc, $lonc, $xc, $yc, 
					   	$xscale_pixperdeg, $yscale_pixperdeg );

			( $symsize, $symshape, $symcolor ) = 
  			set_hypocenter_symbol( \%Mapspec, @db, "age" );

			my( $symtype ) = "origin";

			my( $primitive, $points, $xul, $yul, $xlr, $ylr );

			if( $symshape eq "square" ) {
				$primitive = "rectangle";
				$xul = $x - $symsize;
				$yul = $y - $symsize;
				$xlr = $x + $symsize;
				$ylr = $y + $symsize;
				$points = "$xul,$yul $xlr,$ylr";
			} else {
				die( "symbol shape $symshape not understood\n" );
			}

			$modified_image->Draw(
					fill=>$symcolor,
					primitive=>$primitive,
					stroke=>'black',
					points=>$points );

			dbaddv( @dbmapassoc,
				"orid", $orid,
				"mapname", $mapname, 
				"x", $x, 
				"y", $y,
				"symsize", $symsize,
				"symshape", $symshape,
				"symcolor", $symcolor, 
				"symtype", $symtype );
		}
	}

	my( @dbwebmaps ) = dblookup( @db, "", "webmaps", "", "dbALL" );
	my( @dbscratch ) = dblookup( @dbwebmaps, "", "", "", "dbSCRATCH" );
	dbputv( @dbscratch, "mapname", $Mapspec{mapname} );
	my( @recs ) = dbmatches( @dbscratch, @dbwebmaps, "webmaps", "mapname" );

	my( $url ) = $State{dbrecenteqs_url} . "$Mapspec{mapname}.$State{html_suffix}";

	my( $dir ) = $State{dbrecenteqs_dir};
	my( $dfile ) = "$Mapspec{mapname}.$Mapspec{format}";

	check_dir_dfile( @dbwebmaps, $dir, $dfile );

	if( defined( $rec = shift( @recs ) ) ) {
		
		$dbwebmaps[3] = $rec;

		dbputv( @dbwebmaps, 
	    		"mapname", $Mapspec{mapname},
    	    		"dir", $dir,
    	    		"dfile", $dfile,
    	    		"url", $url );
	} else {

		$dbwebmaps[3] = dbaddv( @dbwebmaps, 
	    		"mapname", $Mapspec{mapname},
    	    		"dir", $dir,
    	    		"dfile", $dfile,
    	    		"url", $url );
	}

	my( $webmap_image ) = dbextfile( @dbwebmaps );

	my( $webmap_image_tmp ) = $webmap_image . ".new";

	if( $opt_v ) { 
		elog_notify "dbrecenteqs: Beginning write of $webmap_image_tmp\n";
	}

	$modified_image->Write(filename=>$webmap_image_tmp);

	undef $modified_image;

	undef $Mapspec{clean_image};

	if( $Mapspec{legend} ne "" &&
	    ! -e "$State{dbrecenteqs_dir}/$Mapspec{legend_filebase}" ) {
		system( "/bin/cp $Mapspec{legend} $State{dbrecenteqs_dir}" );
	}

	if( $opt_v ) { 
		elog_notify "dbrecenteqs: Finished write of $webmap_image_tmp\n";
		elog_notify "dbrecenteqs: Moving $webmap_image_tmp to $webmap_image\n";
	}

	system( "/bin/mv $webmap_image_tmp $webmap_image" );

	if( $opt_v ) { 
		elog_notify "dbrecenteqs: Done updating map $mapname\n";
	}
}

sub verified_copy {
	my( $sourcedb, $table, $targetdb ) = @_;

	$repeat_interval_sec = 5;

	system( "cp $sourcedb.$table $targetdb.$table" );

	while( ( $rc = system( "dbcheck $targetdb.$table" ) ) != 0 ) {

		elog_complain( "Corrupt copy of $targetdb.$table; will retry " .
			       "in $repeat_interval_sec seconds...\n" );

		sleep( $repeat_interval_sec );

		system( "cp $sourcedb.$table $targetdb.$table" );
	} 

	if( $opt_v  ) {

		elog_notify( "Successfully copied $sourcedb.$table to " .
			     "$targetdb.$table\n" );
	}

	return;
}

$Program = `basename $0`;
chomp( $Program );

elog_init( $Program, @ARGV );

elog_notify( "$Program started at " . 
	     epoch2str( str2epoch( "now" ),  "%D %T %Z", "" ) . "\n" );

if ( ! getopts('ve:p:huc:') || @ARGV != 1 ) {

	die ( "Usage: $Program [-v] [-h] [-u] [-p pffile] " .
	      "[-e evid] [-c sourcedb] database\n" ); 

} else {

	$dbname = $ARGV[0];

	if( $opt_p ) {
		$State{pf} = $opt_p;
	} else {
		$State{pf} = "dbrecenteqs";
	}

	if( $opt_v ) {
		$V = "-V";
	} else {
		$V = "";
	}
}

init_globals();

if( $opt_u ) { die_if_already_running(); }

if( $opt_c ) {

	$sourcedb = $opt_c;

	verified_copy( $sourcedb, "event", $dbname );
	verified_copy( $sourcedb, "origin", $dbname );
	verified_copy( $sourcedb, "assoc", $dbname );
	verified_copy( $sourcedb, "arrival", $dbname );
}

$rc = system( "dbcheck $dbname" );

if( $rc != 0 ) {
	
	die( "Database '$dbname' is invalid; Quitting!\n" );
}

@db = dbopen( $dbname, "r+" );

if( ! expansion_schema_present( @db ) ) {

	die( "Please add dbrecenteqs1.2 expansion schema to $dbname. Bye.\n" );
}

if( $State{use_qgrids} && ! gme_schema_present( @db ) ) {

	elog_complain "Turning off use_qgrids option: gme1.0 expansion " .
		  "schema is not present in $dbname.\n";
	$State{use_qgrids} = 0;
}

@db = dblookup( @db, "", "mapstock", "", "" );

foreach $map ( @{$State{overview_maps}} ) {
	
	$hashref = pfget_Mapspec( $State{pf}, "$map" );

	%Mapspec = %{$hashref};

	$mapname = $Mapspec{mapname};

	$State{maphashes}->{$mapname} = $hashref;

	if( $map eq ${$State{overview_maps}}[0] ) {
		
		$State{main_index_mapname} = $mapname;
	}

	if( (dblookup( @db, "", "mapstock", "mapname", "$mapname" ))[3] >= 0 ) {
		next;
	}

	if( $Mapspec{source} eq "dynamic" ) {

		%Mapspec = %{setup_index_Mapspec( \%Mapspec )};
		%Mapspec = %{create_map( \%Mapspec )};

	} else {

		%Mapspec = %{read_map_from_file( \%Mapspec )};
	}

	add_to_mapstock( \%Mapspec, @db );
}

if( ! -e "$State{dbrecenteqs_dir}/$State{wiggle_filebase}" ) {
	system( "/bin/cp $State{wiggle} $State{dbrecenteqs_dir}" );
}
if( ! -e "$State{dbrecenteqs_dir}/$State{institute_logo_filebase}" ) {
	system( "/bin/cp $State{institute_logo} $State{dbrecenteqs_dir}" );
}
if( ( ! -e "$State{dbrecenteqs_dir}/$State{background_graphic_filebase}" ) &&
    ( -e "$State{background_graphic}" ) ) {
	system( "/bin/cp $State{background_graphic} $State{dbrecenteqs_dir}" );
}

cleanup_database( $dbname );

@dbstockmaps = dbprocess( @db, 
			  "dbopen origin", 
			  "dbjoin event", 
			  "dbsubset orid == prefor", 
			  "dbsever event",
			  "dbtheta mapstock",
			  "dbsort mapname time",
			  "dbgroup mapname" );

$ngroups = dbquery( @dbstockmaps, "dbRECORD_COUNT" );

if( $ngroups <= 0 ) {

	# Allow creation of initialized sites for new databases

	@dbstockmaps = dbprocess( @db, 
				  "dbopen mapstock", 
				  "dbgroup mapname" );

	$ngroups = dbquery( @dbstockmaps, "dbRECORD_COUNT" );
}

# Necessary to create webmaps table entries for other_map_links to work
for( $dbstockmaps[3] = 0; $dbstockmaps[3] < $ngroups; $dbstockmaps[3]++ ) {

	create_stockmap_entry( @dbstockmaps );
}

if( $opt_e ) {
	
	$evid = $opt_e;
	
	@db = dblookup( @db, "", "event", "evid", $evid );

	if( $db[3] < 0 ) {

		die( "dbrecenteqs: Couldn't find evid $evid\n" );
	}

	foreach $map ( @{$State{focus_maps}} ) {
	
		$hashref = pfget_Mapspec( $State{pf}, "$map" );

		%Focus_Mapspec = %{$hashref};

		create_focusmap( $evid, @db );
		create_focusmap_html( $evid, @db );
	}

} elsif( $opt_h ) {

	@dbwebmaps = dbprocess( @db, "dbopen webmaps",
				     "dbsubset evid != NULL",
				     "dbjoin event",
				     "dbjoin origin event.prefor#origin.orid",
				     "dbjoin mapassoc mapname origin.orid#mapassoc.orid",
				     "dbsubset symtype == \"origin\"",
				     );

	$nmaps = dbquery( @dbwebmaps, "dbRECORD_COUNT" );

	if( $opt_v ) {
		elog_notify "dbrecenteqs: updating html for $nmaps focus maps\n";
	}

	for( $dbwebmaps[3]=0; $dbwebmaps[3]<$nmaps; $dbwebmaps[3]++ ) {

		( $evid ) = dbgetv( @dbwebmaps, "evid" );

		create_focusmap_html( $evid, @db );
	}

} else {

	remove_stale_webmaps( @db );

	$min_time = 0;

	if( defined( $State{max_num_eqs} ) && $State{max_num_eqs} > 0 ) {

		@dbmaps = dbprocess( @db, 
				"dbopen origin",
				"dbjoin event",
				"dbsubset orid == prefor",
				"dbsort -r origin.time" );

		$nmaps = dbquery( @dbmaps, "dbRECORD_COUNT" );

		if( $nmaps > $State{max_num_eqs} ) {
			
			$dbmaps[3] = $State{max_num_eqs} - 1;

			$min_time = dbgetv( @dbmaps, "", "origin.time", "", "" );
		}
	}

	@dbneedmaps = dbprocess( @db, 
			 "dbopen origin", 
			 "dbjoin event",
			 "dbsubset orid == prefor",
			 "dbnojoin webmaps evid#evid",
			 "dbsort origin.time",
			 "dbsubset origin.time >= $min_time" ); 

	$nmaps = dbquery( @dbneedmaps, "dbRECORD_COUNT" );

	if( $opt_v ) {
		elog_notify "dbrecenteqs: creating $nmaps focus maps\n";
	}

	for( $dbneedmaps[3]=0; $dbneedmaps[3]<$nmaps; $dbneedmaps[3]++ ) {

		( $evid ) = dbgetv( @dbneedmaps, "evid" );

		if( $opt_v ) {
			elog_notify sprintf "Creating focus map %d of %d:\n",
			      $dbneedmaps[3]+1, $nmaps;
		}

		foreach $map ( @{$State{focus_maps}} ) {
	
			$hashref = pfget_Mapspec( $State{pf}, "$map" );

			%Focus_Mapspec = %{$hashref};

			create_focusmap( $evid, @db );
			create_focusmap_html( $evid, @db );
		}
	}
}

if( $opt_v ) {
	elog_notify "dbrecenteqs: updating $ngroups stock maps\n";
}

for( $dbstockmaps[3] = 0; $dbstockmaps[3] < $ngroups; $dbstockmaps[3]++ ) {

	update_stockmap( @dbstockmaps );
	create_stockmap_html( @dbstockmaps );

}

dbcrunch( dblookup( @db, "", "mapassoc", "", "dbALL" ) );

if( defined( $State{"workdir"} ) && $State{"workdir"} ne "" ) {
	system( "/bin/rm -rf $State{workdir}" );
}

elog_notify( "$Program finished at " . 
	     epoch2str( str2epoch( "now" ),  "%D %T %Z", "" ) . "\n\n" );

