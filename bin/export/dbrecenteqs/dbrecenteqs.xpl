require "getopts.pl" ;
require "dbrecenteqs.pl";
require "winding.pl";
require "compass_from_azimuth.pl";
use Datascope;
use Image::Magick;
 

sub init_globals {

	setup_State();

	my( @params ) = (
		"title",
		"caption_default",
		"html_base",
		"web_topdir",
		"local_html_home",
		"description_of_local_html_home",
		"page_refresh_seconds",
		"other_region_links",
		"nearest_places",
		"credits",
		"authtrans",
		"keep_ndays"
		);

	my( @path_params ) = (
		"wiggle",
		"local_logo",
		"region_phrases_database"
		);
	
	foreach $param ( @params, @path_params ) {
		$State{$param} = pfget( $State{pf}, $param );
	}

	foreach $param ( @path_params ) {

		$State{$param} = datafile_abspath( $State{$param} );
	}

	$State{"wiggle_filebase"} = `basename $State{"wiggle"}`;
	chomp( $State{"wiggle_filebase"} );
	$State{"local_logo_filebase"} = `basename $State{"local_logo"}`;
	chomp( $State{"local_logo_filebase"} );

	if( $State{html_base} !~ m@/$@ ) { $State{html_base} .= "/"; }
	if( $State{web_topdir} !~ m@/$@ ) { $State{web_topdir} .= "/"; }

	if( ! -d $State{web_topdir} ) {
		die( "The directory $State{web_topdir} does not exist. " .
		     "Please create it before proceeding. Bye.\n" );
	}

	$State{quakesubdir} = "quakes";
	mkdir( "$State{web_topdir}/$State{quakesubdir}", 0755 );

	%Focus_Mapspec = %{pfget( "dbrecenteqs", "focus_map" )};
	%Focus_Mapspec = ( %Focus_Mapspec, %{$State{focus_map_config}} );

	$clientside_mapname = "alaska";

	my( @db ) = dbopen( $State{region_phrases_database}, "r" );
	if( $db[0] < 0 ) {
		print STDERR "Couldn't open $State{region_phrases_database}\n";
		undef $State{region_phrases_database};
	}
	@db = dblookup( @db, "", "regions", "", "" );
	if( $db[1] < 0 ) {
		print STDERR
			"No regions table in $State{region_phrases_database}\n";
		undef $State{region_phrases_database};
	} else {
		$State{region_phrases_database} = \@db;
	}
}

sub die_if_already_running {

	my( @procs ) = split( /\n/,
		`pgrep -lf 'dbrecenteqs' | grep -v pgrep | egrep -v '^ *$$ '` );

	if( $#procs >= 0 ) {
		die( "dbrecenteqs: already running as \n" .
			join( "\n", @procs ) . "\nBye!\n" );
	}
}

sub cleanup_database {
	my( $dbname ) = @_;
	my( $cmd, $cutoff, $table );

	if( ! defined( $State{keep_ndays} ) || $State{keep_ndays} == 0 ) {
		print
		  "dbrecenteqs: keep_ndays undefined or set to " .
		  "zero (cleanup disabled). No cleanup initiated.\n";
		return;
	} else {
		print
		  "dbrecenteqs: Trimming $dbname to $State{keep_ndays} most recent days.\n";
	}

	$cutoff = str2epoch( "now" ) - $State{keep_ndays} * 86400;

	$cmd = "orb2db_msg $dbname pause";
	system( $cmd );

	foreach $table ("arrival", "detection", "origin" ) {

		print "dbrecenteqs: Cleaning $table table\n";

		next if( ! -e "$dbname.$table" );

		$cmd = "dbsubset $dbname.$table \"time < $cutoff\" | " .
			"dbdelete - $table";
		system( $cmd );
		if( $? ) { print STDERR "\t command error $?\n"; }
	}

	foreach $table ( "assoc", "event", "mapassoc" ) {

		print "dbrecenteqs: Cleaning $table table\n";

		next if( ! -e "$dbname.$table" );
		next if( ! -e "$dbname.origin" );

		$cmd = "dbnojoin $dbname.$table origin | dbdelete - $table";
		system( $cmd );
		if( $? ) { print STDERR "\t command error $?\n"; }
	}

	foreach $table ( "webmaps" ) {

		print "dbrecenteqs: Cleaning $table table\n";

		next if( ! -e "$dbname.$table" );
		next if( ! -e "$dbname.event" );

		$cmd = "dbnojoin $dbname.$table event | " .
			"dbsubset - \"evid != NULL\" | " .
			"dbdelete - $table";
		system( $cmd );
		if( $? ) { print STDERR "\t command error $?\n"; }
	}
	$cmd = "orb2db_msg $dbname continue";
	system( $cmd );
}

sub set_hypocenter_symbol {
	my( %Mapspec ) = %{shift( @_ )};
	my( $isprefor ) = pop( @_ );
	my( @db ) = @_;

	my( $mag, $symsize, $symshape, $symcolor );

	my( $ml, $mb, $ms ) = dbgetv( @db, "ml", "mb", "ms" );

	if( $ml != -999 ) {
		$mag = $ml;
	} elsif( $mb != -999 ) {
		$mag = $mb;
	} elsif( $ms != -999 ) {
		$mag = $ms;
	} else {
		$mag = -999;
	}

	if( $mag < 2 ) {
		$symsize = 4;
	} else {
		$symsize = int( $mag ) + 3;
	}

	$symshape = $Mapspec{quakeshape};

	if( $isprefor ) {
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

		return "$ml ML";

	} elsif( $mb != -999 ) {

		return "$mb Mb";

	} elsif( $ms != -999 ) {

		return "$ms Ms";

	} else {

		return "Unknown";
	}
}

sub translate_author {
	my( $auth ) = @_;

	if( ! defined( $State{authtrans}->{$auth} ) ) {

		return $auth;

	} else {
		
		return "<A HREF=\"" . 
			$State{authtrans}->{$auth}->{"url"} .
			"\">" . 
			$State{authtrans}->{$auth}->{"text"} .
			"</A>";
	}
}

sub hypocenter_vitals {
	my( $bgcolor ) = pop( @_ );
	my( @db ) = @_;

	my( $lat, $lon, $depth, $time, $auth ) = 	
		dbgetv( @db, "lat", "lon", "depth", "time", "origin.auth" );
	
	$auth = translate_author( $auth );

	$depth_km = sprintf( "%.0d", $depth );
	$depth_mi = sprintf( "%.0d", $depth_km / 1.609 );

	my( $local_day ) = epoch2str( $time, 
		"%A %B %o, %Y", $ENV{TZ} );
	my( $local_hour ) = epoch2str( $time, 
		"%I:%M %p %Z", $ENV{TZ} );
	my( $utc_time ) = epoch2str( $time, "%m/%d/%Y %H:%M:%S.%s %Z" );

	my( $mag_description ) = mag_description( @db );

	my( $table ) = "<TABLE BORDER=6 CELLSPACING=6 BGCOLOR=\"$bgcolor\">\n";
	$table .= "<TR><TD>Local Date:</TD><TD>$local_day</TD></TR>\n";
	$table .= "<TR><TD>Local Time:</TD><TD>$local_hour</TD></TR>\n";
	$table .= "<TR><TD>Universal Time:</TD><TD>$utc_time</TD></TR>\n";
	$table .= "<TR><TD>Magnitude:</TD><TD>$mag_description</TD></TR>\n";
	$table .= "<TR><TD>Latitude:</TD><TD>$lat</TD>\n";
	$table .= "<TR><TD>Longitude:</TD><TD>$lon</TD>\n";
	$table .= "<TR><TD>Depth:</TD><TD>$depth_mi miles ($depth_km km)</TD>\n";
	$table .= "<TR><TD>Author:</TD><TD>$auth</TD>\n";
	$table .= "</TABLE>\n";

	return $table;
}

sub location_header_line {
	my( @db ) = splice( @_, 0, 4 );
	my( $lat, $lon, $orid ) = @_;

	my( $regname ) = quake_region( @db, $lat, $lon, $orid );

	if( $regname =~ /^(in|beneath|off|south of|west of|east of|north of) /i ) {

		return "Earthquake $regname";

	} else {

		return "Earthquake: " . $regname;
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
	my( $lat, $lon ) = @_;

	my( @db ) = dbopen( $State{"nearest_places"}->{"cities_dbname"}, "r" );
	@db = dblookup( @db, "", "places", "", "" );

	my( $expr ) = "distance(lat,lon,$lat,$lon)*111.195 <= " .
			$State{"nearest_places"}->{"max_dist_km"} .
			" || place =~ /" . 
			$State{"nearest_places"}->{"always_include"} .
			"/";
	@db = dbsubset( @db, $expr );

	@db = dbsort( @db, "distance(lat,lon,$lat,$lon)" );

	my( $table ) .= "<TABLE BORDER=6 CELLSPACING=6 BGCOLOR=#99CCFF>\n";

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

		$table .= "<TR><TD>$dist_mi miles ($dist_km km) " .
			  "$compass of $place</TD></TR>\n";
	}

	$table .= "</TABLE>";

	dbclose( @db );

	return $table;
}

sub create_focusmap_html {
	my( $evid ) = shift;
	my( @db ) = @_;

	@db = dbprocess( @db, "dbopen webmaps",
			      "dbsubset evid == $evid" );
	$db[3] = 0;

	my( $url, $dfile ) = dbgetv( @db, "url", "dfile" );
	my( $html_relpath ) = substr( $url, length( $State{html_base} ) );
	my( $html_filename ) = 
		concatpaths( $State{web_topdir}, $html_relpath );

	@db = dbprocess( @db, "dbjoin event",
			      "dbjoin origin evid#evid",
			      "dbjoin -o mapassoc mapname origin.orid#mapassoc.orid" );
	my( @dbprefor ) = dbsubset( @db, "origin.orid==prefor" );
	$dbprefor[3] = 0;
	my( @dbnonprefors ) = dbsubset( @db, "origin.orid != prefor" );
	my( $nothers ) = dbquery( @dbnonprefors, "dbRECORD_COUNT" );

	my( $lat, $lon, $mapname, $orid ) = 
		dbgetv( @dbprefor, "lat", "lon", "mapname", "origin.orid" );

	open( H, ">$html_filename" );
	print H "<HTML>\n";
	print H "<HEAD>\n";
	print H "<META HTTP-EQUIV=\"refresh\" " .
		"CONTENT=\"$State{page_refresh_seconds}\">\n";
	print H "</HEAD>\n";
	print H "<BODY BGCOLOR='white'>\n";
	print H "<MAP NAME=\"vitals\">\n" .
		imagemap_symbol( @dbprefor, "#prefor" ) . "\n";
	for( $dbnonprefors[3]=0; $dbnonprefors[3]<$nothers; $dbnonprefors[3]++ ){
		$nporid = dbgetv( @dbnonprefors, "origin.orid" );
		print H imagemap_symbol( @dbnonprefors, "#orid$nporid" ) . "\n";
	}
	print H	"</MAP>\n";
	print H "<CENTER>";
	print H hyperlinked_logo();
	print H "</CENTER>\n";
	print H "<BR>";
	print H "<CENTER><H1>";
	print H "<A HREF=\"$State{html_base}\"><IMG ALIGN='top' " .
		"SRC=\"$State{html_base}$State{wiggle_filebase}\" " .
		"ALT=\"Link to $State{title}\"></A>";
	print H location_header_line( @dbprefor, $lat, $lon, $orid ) .
		"</H1></CENTER>\n";
	print H "<BR>";
	print H "<CENTER><IMG SRC=\"$dfile\" align=center " .
			"USEMAP=\"#vitals\"></CENTER>\n";
	print H "<BR>";
	print H "<CENTER>\n";
	print H "<A NAME=\"prefor\">";
	print H "<H2>Preferred Hypocentral Solution:</H2>\n";
	print H hypocenter_vitals( @dbprefor, "beige" );
	if( $nothers > 0 ) {
		print H "<H2>Previous Solutions and other agencies:</H2>\n";
	}
	for( $dbnonprefors[3]=0; $dbnonprefors[3]<$nothers;$dbnonprefors[3]++ ){
		$orid = dbgetv( @dbnonprefors, "origin.orid" );
		print H "<A NAME=\"orid$orid\">";
		print H hypocenter_vitals( @dbnonprefors, "white" );
	}
	print H "</CENTER>\n";

	$State{"nearest_places"}->{"cities_dbname"} =
	   datafile_abspath( $State{"nearest_places"}->{"cities_dbname"} );
	
	if( ! defined( $State{"nearest_places"}->{"cities_dbname"} ) ||
	      $State{"nearest_places"}->{"cities_dbname"} eq "" ) {

		print STDERR 
			"\n\t************************************\n" . 
			"\tWARNING: Skipping cities--" .
			"no cities_dbname specified\n" .
			"\t************************************\n\n";

	} elsif( ! -e "$State{nearest_places}->{cities_dbname}" ) {

		print STDERR
			"\n\t************************************\n" . 
			"\tWARNING: Skipping cities--" .
			"$State{nearest_places}->{cities_dbname}.places " .
			" not found\n" .
			"\t************************************\n\n";

	} else {
		print H "<BR>";
		print H "<CENTER>\n";
		print H "<H2>This earthquake was:</H2>\n";
		print H nearest_locations( $lat, $lon );
		print H "</CENTER>\n";
	}

	print H "<BR>\n<CENTER>",
		other_map_links( @db, $mapname ),
		"</CENTER>\n";
	print H "<HR>\n" . credits();
	print H "</BODY>\n";
	print H "</HTML>\n";
	close( H );
}

sub create_focusmap {
	my( $evid ) = shift;
	my( @db ) = @_;

	@db = dbprocess( @db, "dbopen event",
			      "dbjoin origin", 
			      "dbsubset evid == $evid" );

	my( $nhypos ) = dbquery( @db, "dbRECORD_COUNT" );

	@dbprefor = dbsubset( @db, "orid == prefor" );
	$dbprefor[3] = 0; 

	my( $preftime, $preflat, $preflon, $prefor ) =
	  dbgetv( @dbprefor, "time", "lat", "lon", "orid" );

	$Focus_Mapspec{filebase} = "evid$evid";
	$Focus_Mapspec{mapname} = $Focus_Mapspec{filebase};
	$Focus_Mapspec{lonc} = unwrapped_lon( \%Focus_Mapspec, $preflon );
	$Focus_Mapspec{latc} = $preflat;

	# Try to keep the directory names short enough for dir field
	my( $reldir ) = concatpaths( $State{quakesubdir}, 
	 	   epoch2str( $preftime, "%Y%j" ) .
			      "_$Focus_Mapspec{mapname}" );
	mkdir( concatpaths( $State{web_topdir}, $reldir ), 0755 );

	$Focus_Mapspec{"psfile"} = concatpaths( $State{"workdir"},
			"$Focus_Mapspec{filebase}.ps" );
	$Focus_Mapspec{"pixfile"} = concatpaths( $State{web_topdir}, $reldir );
	$Focus_Mapspec{"pixfile"} = concatpaths( $Focus_Mapspec{"pixfile"},
			 "$Focus_Mapspec{filebase}.$Focus_Mapspec{format}" );
	
	%Focus_Mapspec = %{set_projection( \%Focus_Mapspec )};
	%Focus_Mapspec = %{set_rectangles( \%Focus_Mapspec )};

	%Focus_Mapspec = %{create_map( \%Focus_Mapspec )};

	my( @dbwebmaps ) = dblookup( @db, "", "webmaps", "", "" );
	my( @dbscratch ) = dblookup( @dbwebmaps, "", "", "", "dbSCRATCH" );

	dbputv( @dbscratch, "mapname", $Focus_mapspec{mapname} );

	my( $url ) = $State{html_base} . 
		concatpaths( $reldir, "$Focus_Mapspec{filebase}.html" );

	my( @recs ) = dbmatches( @dbscratch, @dbwebmaps, "webmaps", "mapname" );

	if( defined( $rec = shift( @recs ) ) ) {
		
		$dbwebmaps[3] = $rec;

		dbputv( @dbwebmaps, 
    	    		"dir", concatpaths( $State{web_topdir}, $reldir ),
    	    		"dfile", "$Focus_Mapspec{filebase}.$Focus_Mapspec{format}",
    	    		"url", $url );
	} else {

		$dbwebmaps[3] = dbaddv( @dbwebmaps, 
	    		"mapname", $Focus_Mapspec{mapname},
			"evid", $evid,
    	    		"dir", concatpaths( $State{web_topdir}, $reldir ),
    	    		"dfile", "$Focus_Mapspec{filebase}.$Focus_Mapspec{format}",
    	    		"url", $url );
	}

	my( $webmap_image ) = dbextfile( @dbwebmaps );

	my( $modified_image ) = $Focus_Mapspec{clean_image}->Clone();

	eliminate_from_mapassoc( @db, $Focus_Mapspec{mapname} );

	for( $db[3]=0; $db[3] < $nhypos; $db[3]++ ) {

		my( $lat, $lon, $orid ) =
	  	  dbgetv( @db, "lat", "lon", "orid" );

		$isprefor = ( $orid == $prefor ) ? 1 : 0;

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
	  		set_hypocenter_symbol( \%Focus_Mapspec, @db, $isprefor );

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
				stroke=>'blue',
				points=>$points );
	
		my( @dbmapassoc ) = dblookup( @db, "", "mapassoc", "", "" );
		dbaddv( @dbmapassoc, "mapname", $Focus_Mapspec{mapname},
			     	     "orid", $orid,
			     	     "x", $x,
			     	     "y", $y, 
			     	     "symsize", $symsize,
			     	     "symshape", $symshape,
			     	     "symcolor", $symcolor );
	}

	$modified_image->Write(filename=>$webmap_image);

	undef( $modified_image );
	undef( $Focus_Mapspec{clean_image} );
}

sub hyperlinked_logo {

	return "<A HREF=\"$State{local_html_home}\">" .
		"<IMG SRC=\"$State{html_base}$State{local_logo_filebase}\" " .
		"align=center " .
		"ALT=\"Link to $State{description_of_local_html_home}\"></A>";
}

sub hyperlinked_earthquake_table {
	my( $mapname ) = pop( @_ );
	my( @db ) = @_;

	# Go Backwards in time: most recent quake first
	@db = dbprocess( @db, 
		       "dbopen mapassoc",
		       "dbsubset mapname == \"$mapname\"",
		       "dbjoin origin",
		       "dbjoin event",
		       "dbsort -r time",
		       "dbjoin webmaps evid",
		       "dbsubset origin.orid == prefor" );

	my( $nsymbols ) = dbquery( @db, "dbRECORD_COUNT" );

	my( $table ) = "<TABLE BORDER=6 RULES=rows CELLSPACING=6 BGCOLOR=beige>\n";

	$table .= "<TR><TD COLSPAN=3 BGCOLOR=white><CENTER>$nsymbols Earthquakes Shown " .
		  "on This Page:</CENTER></TD></TR>\n";
	
	$table .= "<TR>\n";
	$table .= "<TD><CENTER>Local Time</CENTER></TD>\n";
	$table .= "<TD><CENTER>Magnitude</CENTER></TD>\n";
	$table .= "<TD><CENTER>Region</CENTER></TD>\n";
	$table .= "</TR>\n";
	
	for( $db[3]=0; $db[3]<$nsymbols; $db[3]++ ) { 
		my( $lat, $lon, $depth, $time, $orid, $url ) = 
			dbgetv( @db, "lat", "lon", "depth", 
				     "time", "origin.orid", "url" );
		my( $mag_description ) = mag_description( @db );
		my( $local_time ) = epoch2str( $time, 
		"%I:%M %p %Z %A %B %o, %Y", $ENV{TZ} );
		my( $region ) = quake_region( @db, $lat, $lon, $orid );

		$table .= "<TR>";
		$table .= "<TD><A HREF=\"$url\">";
		$table .= $local_time;
		$table .= "</A></TD>\n";
		$table .= "<TD><A HREF=\"$url\">";
		$table .= $mag_description;
		$table .= "</A></TD>\n";
		$table .= "<TD><A HREF=\"$url\">";
		$table .= $region;
		$table .= "</A></TD>\n";
		$table .= "</TR>\n";
	}

	$table .= "</TABLE>\n";

	return $table;

}

sub other_region_links {
	my( $links, $key, $val );

	$links  = "<TABLE BORDER=6 CELLSPACING=6 BGCOLOR=beige><TR>" .
		  "<TD BGCOLOR=white>Other seismic regions:</TD></TR>\n";

	foreach $key ( keys %{$State{other_region_links}} ) {

		$val = $State{other_region_links}->{$key};
		$links .= "<TR><TD><A HREF=\"$val\">$key</A></TD></TR>\n";
	}

	$links .= "</TABLE>\n";

	return $links;
}

sub other_map_links {
	my( $mapname ) = pop( @_ );
	my( @db ) = @_;

	@db = dbprocess( @db,
			"dbopen mapstock",
			"dbjoin webmaps",
			"dbsubset mapname != \"$mapname\"",
			"dbsubset mapclass != \"detail\"" );

	my( $nmaps ) = dbquery( @db, "dbRECORD_COUNT" );

	if( $nmaps <= 0 ) { return ""; }

	my( $links ) = 
		"<TABLE BORDER=6 CELLSPACING=6 BGCOLOR=beige><TR>" .
		"<TD BGCOLOR=white>Other Maps:</TD>\n";

	for( $db[3] = 0; $db[3] < $nmaps; $db[3]++ ) {

		my( $mapname, $mapclass, $url ) = 
			dbgetv( @db, "mapname", "mapclass", "url" );

		my( $maplink );
		if( $mapclass eq "global" ) {
			$maplink = "Global View";
		} elsif( $mapclass eq "index" ) {
			$maplink = $State{title};
		} else {
			$maplink = $mapname;
		}

		$links .= "<TD><A HREF=\"$url\">$maplink</A></TD>\n";
	}
	$links .= "</TR></TABLE>\n";

	return $links;
}

sub imagemap_symbol {
	my( @db ) = splice( @_, 0, 4 );
	my( $url, $mapelement );
	my( $primitive, $xul, $yul, $xlr, $ylr );

	if( $#_ >= 0 ) {
		$url = pop( @_ );
	} else {
		( $url ) = dbgetv( @db, "url" );
	}

	my( $x, $y, $symsize, $symshape ) = 
	   dbgetv( @db, "x", "y", "symsize", "symshape" );

	if( $symshape eq "square" ) {
		$primitive = "rect";
		$xul = $x - $symsize;
		$yul = $y - $symsize;
		$xlr = $x + $symsize;
		$ylr = $y + $symsize;

		$mapelement = "<AREA SHAPE=$primitive " .
			"COORDS=\"$xul,$yul,$xlr,$ylr\" " .
			"HREF=\"$url\">";

	} else {
		die( "symbol shape $symshape not understood\n" );
	}

	return $mapelement;
}

sub clientside_imagemap_quakes {
	my( $mapname ) = pop( @_ );
	my( @db ) = @_;

	my( $map ) = "<MAP NAME=\"$clientside_mapname\">\n";

	# Go backwards in time: apparently (at least in Netscape)
	# The first clientside imagemap encountered is the one 
	# used. Have the most recent quake on top
	@db = dbprocess( @db, 
		       "dbopen mapassoc",
		       "dbsubset mapname == \"$mapname\"",
		       "dbjoin origin",
		       "dbjoin event",
		       "dbsort -r time",
		       "dbjoin webmaps evid", 
		       "dbsubset origin.orid == prefor" );

	my( $nsymbols ) = dbquery( @db, "dbRECORD_COUNT" );

	for( $db[3]=0; $db[3]<$nsymbols; $db[3]++ ) {

		$map .= imagemap_symbol( @db );
		$map .= "\n";
	}

	$map .= "</MAP>\n";

	return $map;
}

sub create_stockmap_html {
	my( @db ) = @_;

	my( $mapname ) = dbgetv( @db, "mapname" );

	my( @dbt ) = dblookup( @db, "", "mapstock", "mapname", "$mapname" );
	my( $mapclass ) = dbgetv( @dbt, "mapclass" );

	print "dbrecenteqs: Updating html for $mapclass map $mapname\n";

	my( @db ) = dbprocess( @db, 
			       "dbopen webmaps",
			       "dbsubset mapname == \"$mapname\"" );

	$db[3] = 0;
	my( $url ) = dbgetv( @db, "url" );
	my( $html_relpath ) = substr( $url, length( $State{html_base} ) );
	my( $html_filename ) = 
		concatpaths( $State{web_topdir}, $html_relpath );
	my( $html_temp_filename ) = $html_filename;
	$html_temp_filename =~ s@/([^/]*)$@/-$1@;

	my( $image_relpath ) = dbextfile( @db );
	$image_relpath = substr( $image_relpath, 	
				 length( $State{web_topdir} ) );
	
	open( H, ">$html_temp_filename" );
	print H "<HTML><HEAD>\n";
	print H "<BASE HREF=\"$State{html_base}\">\n";
	print H "<META HTTP-EQUIV=\"refresh\" " .
		"CONTENT=\"$State{page_refresh_seconds}\">\n";
	print H "<TITLE>$State{title}</TITLE>\n";
	print H "</HEAD>\n";
	print H "<BODY BGCOLOR='white'>\n";

	print H clientside_imagemap_quakes( @db, $mapname );

	print H "<CENTER>";
	print H hyperlinked_logo();
	print H "</CENTER>\n";
	print H "<H1 ALIGN='center'>";
	if( $mapclass eq "index" ) {
		print H "<IMG align=top src=\"$State{wiggle_filebase}\">" .
			"$State{title}</H1>\n";
	} else {
		print H "<A HREF=\"$State{html_base}\">";
		print H "<IMG align=top SRC=\"$State{wiggle_filebase}\" " .
			"ALT=\"Link to $State{title}\">" .
			"$State{title}</H1>\n";
		print H "</A>\n";
	}

	print H "<CENTER>";
	print H "<IMG SRC=\"$image_relpath\" USEMAP=\"#$clientside_mapname\" align=center></A>";
	print H "</CENTER>\n";

	print H "<BR>\n", 	
		"<CENTER>", other_map_links( @db, $mapname ),
		"</CENTER>";

	print H "<CENTER>",
		hyperlinked_earthquake_table( @db, $mapname ),
		"</CENTER>";

	print H "<CENTER>" . "<HR>\n" . other_region_links() . "</CENTER>";
	print H "<HR>\n" . credits();

	print H "</BODY></HTML>\n";

	close( H );

	system( "/bin/mv $html_temp_filename $html_filename" );
}

sub credits {

	return "<H2>Credits:</H2>$State{credits}\n";
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

	my( $url ) = $State{html_base} . "$mapclass.html";

	if( defined( $rec = shift( @recs ) ) ) {
		
		$dbwebmaps[3] = $rec;

		dbputv( @dbwebmaps, "url", $url );

	} else {

		$dbwebmaps[3] = dbaddv( @dbwebmaps, 
	    		"mapname", $mapname,
    	    		"dir", "placeholder", # Not very elegant
    	    		"dfile", $mapname,
    	    		"url", $url );
	}
}

sub update_stockmap {
	my( @db ) = @_;
	my( %Mapspec );

	my( $mapname ) = dbgetv( @db, "mapname" );

	print "dbrecenteqs: Updating map $mapname\n";

	my( @dbbundle ) = split( ' ', dbgetv( @db, "bundle" ) );

	@db = dblookup( @dbbundle, "", "", "dbALL", "" );
	%Mapspec = %{read_map_from_db( @db )};

	my( $modified_image ) = $Mapspec{clean_image}->Clone();

	eliminate_from_mapassoc( @db, $mapname );
	@dbmapassoc = dblookup( @db, "", "mapassoc", "", "" );

	for( $db[3]=$dbbundle[3]; $db[3]<$dbbundle[2]; $db[3]++ ) {

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
  		set_hypocenter_symbol( \%Mapspec, @db, 1 );

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
				stroke=>'blue',
				points=>$points );

		dbaddv( @dbmapassoc,
			"orid", $orid,
			"mapname", $mapname, 
			"x", $x, 
			"y", $y,
			"symsize", $symsize,
			"symshape", $symshape,
			"symcolor", $symcolor );
	}

	my( @dbwebmaps ) = dblookup( @db, "", "webmaps", "", "dbALL" );
	my( @dbscratch ) = dblookup( @dbwebmaps, "", "", "", "dbSCRATCH" );
	dbputv( @dbscratch, "mapname", $Mapspec{mapname} );
	my( @recs ) = dbmatches( @dbscratch, @dbwebmaps, "webmaps", "mapname" );

	my( $url ) = $State{html_base} . "$Mapspec{mapclass}.html";

	if( defined( $rec = shift( @recs ) ) ) {
		
		$dbwebmaps[3] = $rec;

		dbputv( @dbwebmaps, 
	    		"mapname", $Mapspec{mapname},
    	    		"dir", $State{web_topdir},
    	    		"dfile", "$Mapspec{mapclass}.$Mapspec{format}",
    	    		"url", $url );
	} else {

		$dbwebmaps[3] = dbaddv( @dbwebmaps, 
	    		"mapname", $Mapspec{mapname},
    	    		"dir", $State{web_topdir},
    	    		"dfile", "$Mapspec{mapclass}.$Mapspec{format}",
    	    		"url", $url );
	}

	my( $webmap_image ) = dbextfile( @dbwebmaps );

	$modified_image->Write(filename=>$webmap_image);

	undef $modified_image;

	undef $Mapspec{clean_image};
}

$Program = `basename $0`;
chomp( $Program );

elog_init( $Program, @ARGV );

if ( ! &Getopts('p:hi:g:') || @ARGV != 1 ) {
	die ( "Usage: $Program [-h] [-p pffile] [-i indexmap_pffile] " .
	      "[-g globalmap_pffile] database\n" ); 
} else {
	$dbname = $ARGV[0];
	if( $opt_p ) {
		$State{pf} = $opt_p;
	} else {
		$State{pf} = "dbrecenteqs";
	}
}

init_globals();

die_if_already_running();

@db = dbopen( $dbname, "r+" );

if( ! expansion_schema_present( @db ) ) {

	die( "Please add dbrecenteqs1.1 expansion schema to $dbname. Bye.\n" );
}

if( $opt_i ) {
	
	%Index_Mapspec = %{read_map_from_file( "index_map_config", $opt_i )};
	add_to_mapstock( \%Index_Mapspec, @db );
	exit( 0 );
	
} elsif( $opt_g ) {
	
	%Global_Mapspec = %{read_map_from_file( "global_map_config", $opt_g )};
	add_to_mapstock( \%Global_Mapspec, @db );
	exit( 0 );
} 

@db = dblookup( @db, "", "mapstock", "", "" );
@db = dbsubset( @db, "mapclass == \"index\"" );

if( dbquery( @db, "dbRECORD_COUNT" ) <= 0 ) {

	%Index_Mapspec = %{setup_Index_Mapspec()};
	%Index_Mapspec = %{create_map( \%Index_Mapspec )};
	add_to_mapstock( \%Index_Mapspec, @db );
}

if( ! -e "$State{web_topdir}/$State{wiggle_filebase}" ) {
	system( "/bin/cp $State{wiggle} $State{web_topdir}" );
}
if( ! -e "$State{web_topdir}/$State{local_logo_filebase}" ) {
	system( "/bin/cp $State{local_logo} $State{web_topdir}" );
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

# Necessary to create webmaps table entries for other_map_links to work
for( $dbstockmaps[3] = 0; $dbstockmaps[3] < $ngroups; $dbstockmaps[3]++ ) {

	create_stockmap_entry( @dbstockmaps );
}

if( $opt_h ) {

	@dbwebmaps = dbprocess( @db, "dbopen webmaps",
				     "dbsubset evid != NULL" );

	$nmaps = dbquery( @dbwebmaps, "dbRECORD_COUNT" );
	print "dbrecenteqs: updating html for $nmaps focus maps\n";

	for( $dbwebmaps[3]=0; $dbwebmaps[3]<$nmaps; $dbwebmaps[3]++ ) {

		( $evid ) = dbgetv( @dbwebmaps, "evid" );

		create_focusmap_html( $evid, @db );
	}

} else {

	@dbneedmaps = dbprocess( @db, 
			 "dbopen origin", 
			 "dbjoin event",
			 "dbsubset orid == prefor",
			 "dbnojoin webmaps evid#evid" ); 

	$nmaps = dbquery( @dbneedmaps, "dbRECORD_COUNT" );
	print "dbrecenteqs: creating $nmaps focus maps\n";

	for( $dbneedmaps[3]=0; $dbneedmaps[3]<$nmaps; $dbneedmaps[3]++ ) {

		( $evid ) = dbgetv( @dbneedmaps, "evid" );

		create_focusmap( $evid, @db );
		create_focusmap_html( $evid, @db );
	}
}

print "dbrecenteqs: updating $ngroups stock maps\n";

for( $dbstockmaps[3] = 0; $dbstockmaps[3] < $ngroups; $dbstockmaps[3]++ ) {

	update_stockmap( @dbstockmaps );
	create_stockmap_html( @dbstockmaps );

}

dbcrunch( dblookup( @db, "", "mapassoc", "", "dbALL" ) );
