require "getopts.pl" ;
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
		"legend_description",
		"page_refresh_seconds",
		"other_region_links",
		"nearest_places",
		"credits",
		"authtrans",
		"keep_ndays",
		"max_num_eqs",
		"overview_maps",
		"make_index_html"
		);

	my( @path_params ) = (
		"wiggle",
		"legend",
		"institute_logo",
		"region_phrases_database",
		);
	
	foreach $param ( @params, @path_params ) {

		$State{$param} = pfget( $State{pf}, $param );
	}

	foreach $param ( @path_params ) {

		$State{$param} = datafile_abspath( $State{$param} );
	}

	$State{"wiggle_filebase"} = `basename $State{"wiggle"}`;
	chomp( $State{"wiggle_filebase"} );
	$State{"institute_logo_filebase"} = `basename $State{"institute_logo"}`;
	chomp( $State{"institute_logo_filebase"} );
	$State{"legend_filebase"} = `basename $State{"legend"}`;
	chomp( $State{"legend_filebase"} );

	if( ! defined( $State{overview_maps} ) || 
	    $#{$State{overview_maps}} < 0 ) {

		die( "Must have at least one entry in the " . 
		     "overview_maps &Tbl of the parameter file. Bye.\n" );
	}

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

	%Focus_Mapspec = %{pfget_Mapspec( $State{pf}, "focus_map" )};

	$clientside_mapname = "clientsidemap";

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
		print
		  "dbrecenteqs: keep_ndays undefined or set to " .
		  "zero (cleanup disabled). No cleanup initiated.\n";
		return;
	} else {
		print
		  "dbrecenteqs: Trimming $dbname to $State{keep_ndays} " .
		  "most recent days.\n";
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
	my( $colormode ) = pop( @_ );
	my( @db ) = @_;

	my( $mag, $symsize, $symshape, $symcolor );

	my( $ml, $mb, $ms, $time ) = dbgetv( @db, "ml", "mb", "ms", "time" );

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

	if( $mag < 2 ) {
		$symsize = 4;
	} else {
		$symsize = int( $mag ) + 3;
	}

	$symshape = $Mapspec{quakeshape};

	if( $colormode eq "age" ) {

		foreach $key ( sort
				    { $Mapspec{quake_agecolors}->{$a} <=>
				      $Mapspec{quake_agecolors}->{$b}
				    } 
				keys %{$Mapspec{quake_agecolors}} ) {

			$symcolor = $key;

			if( $age < $Mapspec{quake_agecolors}->{$key} ) {
				last;
			}
		}	

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

	foreach $key ( keys( %{$State{authtrans}} ) ) {

		if( $auth =~ m/$key/ ) {

			return ( $State{authtrans}->{$key}->{"text"},
				 $State{authtrans}->{$key}->{"url"} );
		}
	}

	return ( $auth, "" );
}

sub hypocenter_vitals {
	my( $writer ) = shift( @_ );
	my( $type ) = pop( @_ );
	my( @db ) = @_;

	my( $lat, $lon, $depth, $time, $orid, $auth ) = 	
		dbgetv( @db, "lat", "lon", "depth", "time", 
			     "origin.orid", "origin.auth" );
	
	my( $name ) = "orid$orid";

	my( $authtrans, $auth_href ) = translate_author( $auth );

	$depth_km = sprintf( "%.0d", $depth );
	$depth_mi = sprintf( "%.0d", $depth_km / 1.609 );

	my( $local_day ) = epoch2str( $time, 
		"%A %B %o, %Y", $ENV{TZ} );

	my( $local_hour ) = epoch2str( $time, 
		"%I:%M %p %Z", $ENV{TZ} );

	my( $utc_time ) = epoch2str( $time, "%m/%d/%Y %H:%M:%S.%s %Z" );

	my( $mag_description ) = mag_description( @db );

	my( $depth_string ) = "$depth_mi miles ($depth_km km)";

	my( $shape, $coords ) = imagemap_symbol( @db );

	$writer->startTag( "origin", "type" => "$type", "name" => "$name" );
	$writer->dataElement( "orid", "$orid" );
	$writer->dataElement( "localdate_string", "$local_day" );
	$writer->dataElement( "localtime_string", "$local_hour" );
	$writer->dataElement( "utc_string", "$utc_time" );
	$writer->dataElement( "mag_string", "$mag_description" );
	$writer->dataElement( "lat", "$lat" );
	$writer->dataElement( "lon", "$lon" );
	$writer->dataElement( "depth_string", "$depth_string" );
	$writer->dataElement( "auth_href", "$auth_href" );
	$writer->dataElement( "auth", "$authtrans" );
	$writer->dataElement( "shape", "$shape" );
	$writer->dataElement( "coords", "$coords" );

	$writer->endTag( "origin" );
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
			      "dbsubset evid == $evid" );
	$db[3] = 0;

	my( $url, $dfile ) = dbgetv( @db, "url", "dfile" );
	my( $html_relpath ) = substr( $url, length( $State{dbrecenteqs_url} ) );
	my( $html_filename ) = 
		concatpaths( $State{dbrecenteqs_dir}, $html_relpath );
	my( $xml_filename ) = "$html_filename";
	$xml_filename =~ s/\..*//g;
	$xml_filename .= ".xml";

	chomp( my( $dir_relpath ) = `dirname $html_relpath` );

	@db = dbprocess( @db, "dbjoin event",
			      "dbjoin origin evid#evid",
			      "dbjoin -o mapassoc mapname origin.orid#mapassoc.orid" );
	my( @dbprefor ) = dbsubset( @db, "origin.orid==prefor" );
	$dbprefor[3] = 0;
	my( @dbnonprefors ) = dbsubset( @db, "origin.orid != prefor" );
	my( $nothers ) = dbquery( @dbnonprefors, "dbRECORD_COUNT" );

	my( $lat, $lon, $mapname, $orid ) = 
		dbgetv( @dbprefor, "lat", "lon", "mapname", "origin.orid" );

	my( $region_string ) = location_header_line( @dbprefor, $lat, $lon, $orid );
	my( $output ) = new IO::File( ">$xml_filename" );

	if( ! defined( $output ) ) {

		print STDERR 
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

	$writer->startTag( "specific_quake", "name" => "evid$evid" );

	$writer->dataElement( "page_title", 
			      "$region_string" );
	$writer->dataElement( "dbrecenteqs_base", 
			      "$State{dbrecenteqs_url}" );
	$writer->dataElement( "page_base", "$url" );
	$writer->dataElement( "page_refresh_seconds", 
			      "$State{page_refresh_seconds}" );
	$writer->dataElement( "wiggle_href", 
			      "$State{dbrecenteqs_url}" . "$State{wiggle_filebase}" );
	$writer->dataElement( "legend_url", 
			      "$State{dbrecenteqs_url}" . "$State{legend_filebase}" );
	$writer->dataElement( "institute_url", 
			      "$State{institute_url}" );
	$writer->dataElement( "institute_logo_url",
		      	      "$State{dbrecenteqs_url}" .
			      "$State{institute_logo_filebase}" );
	$writer->dataElement( "institute_description", 
			      "$State{institute_description}" );
	$writer->dataElement( "legend_description", 
			      "$State{legend_description}" );

	$writer->dataElement( "region_string", $region_string );

	$writer->dataElement( "subdir", "$dir_relpath" );

	$writer->startTag( "pixmap", "mapclass" => "focus" );
	$writer->dataElement( "file", "$dfile" );
	$writer->dataElement( "clientside_mapname", "vitals" );
	$writer->endTag( "pixmap" );

	$writer->startTag( "origins" );

	hypocenter_vitals( $writer, @dbprefor, "prefor" );

	for( $dbnonprefors[3]=0; $dbnonprefors[3]<$nothers;$dbnonprefors[3]++ ){

		hypocenter_vitals( $writer, @dbnonprefors, "nonprefor" );
	}

	$writer->endTag( "origins" );

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

		nearest_locations( $writer, $lat, $lon );
	}

	other_map_links( $writer, @db, $mapname ),

	credits( $writer );

	$writer->endTag( "specific_quake" );

	print $output "\n";

	$output->close();

	xml_to_html( $xml_filename, $Focus_Mapspec{stylesheet}, $html_filename );
}

sub create_focusmap {
	my( $evid ) = shift;
	my( @db ) = @_;

	@db = dbprocess( @db, "dbopen event",
			      "dbjoin origin", 
			      "dbsubset evid == $evid" );

	# Use questionable strategy to plot prefor last.
	@db = dbsort( @db, "-r", "abs(orid - prefor)" );

	my( $nhypos ) = dbquery( @db, "dbRECORD_COUNT" );

	@dbprefor = dbsubset( @db, "orid == prefor" );
	$dbprefor[3] = 0; 

	my( $preftime, $preflat, $preflon, $prefor ) =
	  dbgetv( @dbprefor, "time", "lat", "lon", "orid" );

	$Focus_Mapspec{file_basename} = "evid$evid";
	$Focus_Mapspec{mapname} = $Focus_Mapspec{file_basename};
	$Focus_Mapspec{lonc} = unwrapped_lon( \%Focus_Mapspec, $preflon );
	$Focus_Mapspec{latc} = $preflat;

	# Try to keep the directory names short enough for dir field
	my( $reldir ) = concatpaths( $State{quakesubdir}, 
	 	   epoch2str( $preftime, "%Y%j" ) .
			      "_$Focus_Mapspec{mapname}" );
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

	dbputv( @dbscratch, "mapname", $Focus_mapspec{mapname} );

	my( $url ) = $State{dbrecenteqs_url} . 
		concatpaths( $reldir, "$Focus_Mapspec{file_basename}.html" );

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
	    		"mapname", $Focus_Mapspec{mapname},
			"evid", $evid,
    	    		"dir", $dir,
    	    		"dfile", $dfile,
    	    		"url", $url );
	}

	my( $webmap_image ) = dbextfile( @dbwebmaps );

	my( $modified_image ) = $Focus_Mapspec{clean_image}->Clone();

	eliminate_from_mapassoc( @db, $Focus_Mapspec{mapname} );

	for( $db[3]=0; $db[3] < $nhypos; $db[3]++ ) {

		my( $lat, $lon, $orid ) =
	  	  dbgetv( @db, "lat", "lon", "orid" );

		my( $colormode ) = ( $orid == $prefor ) ? "prefor" : "nonprefor"; 

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

sub stockmap_earthquake_xml {
	my( $writer ) = shift( @_ );
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

	$writer->startTag( "quakelist" );

	for( $db[3]=0; $db[3]<$nsymbols; $db[3]++ ) { 

		my( $lat, $lon, $depth, $time, $orid, $url ) = 
			dbgetv( @db, "lat", "lon", "depth", 
				     "time", "origin.orid", "url" );

		my( $mag_description ) = mag_description( @db );

		my( $local_time ) = epoch2str( $time, 
		"%I:%M %p %Z %A %B %o, %Y", $ENV{TZ} );

		my( $region ) = quake_region( @db, $lat, $lon, $orid );

		my( $shape, $coords ) = imagemap_symbol( @db );

		$writer->startTag( "quake" );

		$writer->dataElement( "href", "$url" );
		$writer->dataElement( "localtime_string", "$local_time" );
		$writer->dataElement( "mag_string", "$mag_description" );
		$writer->dataElement( "region_string", "$region" );
		$writer->dataElement( "shape", "$shape" );
		$writer->dataElement( "coords", "$coords" );

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

sub other_map_links {
	my( $writer ) = shift( @_ );
	my( $mapname ) = pop( @_ );
	my( @db ) = @_;

	@db = dbprocess( @db,
			"dbopen mapstock",
			"dbjoin webmaps",
			"dbsubset mapname != \"$mapname\"",
			"dbsubset mapclass != \"detail\"" );

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

	my( $x, $y, $symsize, $symshape ) = 
	   dbgetv( @db, "x", "y", "symsize", "symshape" );

	if( $symshape eq "square" ) {
		$primitive = "rect";
		$xul = $x - $symsize;
		$yul = $y - $symsize;
		$xlr = $x + $symsize;
		$ylr = $y + $symsize;

		$shape = "$primitive";
		$coords = "$xul,$yul,$xlr,$ylr";

	} else {
		die( "symbol shape $symshape not understood\n" );
	}

	return ( $shape, $coords );
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
	my( $html_relpath ) = substr( $url, length( $State{dbrecenteqs_url} ) );
	my( $html_filename ) = 
		concatpaths( $State{dbrecenteqs_dir}, $html_relpath );
	my( $html_temp_filename ) = $html_filename;
	$html_temp_filename =~ s@/([^/]*)$@/-$1@;

	my( $xml_filename ) = $html_filename;
	$xml_filename =~ s/\..*//;
	$xml_filename .= ".xml";

	my( $main_index_filename ) = $html_filename;
	$main_index_filename =~ s@/([^/]*)$@/index.html@;

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
	$writer->dataElement( "legend_url", 
			      "$State{dbrecenteqs_url}" .
				"$State{legend_filebase}" );
	$writer->dataElement( "institute_url", 
			      "$State{institute_url}" );
	$writer->dataElement( "institute_logo_url",
		      	      "$State{dbrecenteqs_url}" .
			      "$State{institute_logo_filebase}" );
	$writer->dataElement( "institute_description", 
			      "$State{institute_description}" );
	$writer->dataElement( "legend_description", 
			      "$State{legend_description}" );

	$writer->startTag( "pixmap", "mapclass" => "$mapclass" );
	$writer->dataElement( "file", "$image_relpath" );
	$writer->dataElement( "clientside_mapname", "$clientside_mapname" );
	$writer->endTag( "pixmap" );

	other_map_links( $writer, @db, $mapname ),

	stockmap_earthquake_xml( $writer, @db, $mapname ),

	other_region_links( $writer );

	credits( $writer );

	$writer->endTag( "dbrecenteqs_main" );

	print $output "\n";
	
	$output->close();

	xml_to_html( $xml_filename, 
		     $stylesheet,
		     $html_temp_filename );

	system( "/bin/mv $html_temp_filename $html_filename" );

	if( ( $State{make_index_html} =~ m/y|yes|1|true|t/i ) &&
	    ( $mapname eq $State{main_index_mapname} ) &&
	    ( $html_filename !~ m@.*/index.html$@ ) ) {

		system( "/usr/bin/cp $html_filename $main_index_filename" );
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

	my( $url ) = $State{dbrecenteqs_url} . "$mapname.html";

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

	print "dbrecenteqs: Updating map $mapname\n";

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

		    print "dbrecenteqs: limiting $mapname to $State{max_num_eqs} earthquakes\n";

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
				"symcolor", $symcolor );
		}
	}

	my( @dbwebmaps ) = dblookup( @db, "", "webmaps", "", "dbALL" );
	my( @dbscratch ) = dblookup( @dbwebmaps, "", "", "", "dbSCRATCH" );
	dbputv( @dbscratch, "mapname", $Mapspec{mapname} );
	my( @recs ) = dbmatches( @dbscratch, @dbwebmaps, "webmaps", "mapname" );

	my( $url ) = $State{dbrecenteqs_url} . "$Mapspec{mapname}.html";

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

	$modified_image->Write(filename=>$webmap_image);

	undef $modified_image;

	undef $Mapspec{clean_image};
}

$Program = `basename $0`;
chomp( $Program );

elog_init( $Program, @ARGV );

if ( ! &Getopts('e:p:huc:') || @ARGV != 1 ) {

	die ( "Usage: $Program [-h] [-u] [-p pffile] " .
	      "[-e evid] [-c sourcedb] database\n" ); 

} else {
	$dbname = $ARGV[0];
	if( $opt_p ) {
		$State{pf} = $opt_p;
	} else {
		$State{pf} = "dbrecenteqs";
	}
}

init_globals();

if( $opt_u ) { die_if_already_running(); }

if( $opt_c ) {

	$sourcedb = $opt_c;

	system( "cp $sourcedb.event $dbname.event" );
	system( "cp $sourcedb.origin $dbname.origin" );
	system( "cp $sourcedb.assoc $dbname.assoc" );
	system( "cp $sourcedb.arrival $dbname.arrival" );
}

@db = dbopen( $dbname, "r+" );

if( ! expansion_schema_present( @db ) ) {

	die( "Please add dbrecenteqs1.1 expansion schema to $dbname. Bye.\n" );
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
if( ! -e "$State{dbrecenteqs_dir}/$State{legend_filebase}" ) {
	system( "/bin/cp $State{legend} $State{dbrecenteqs_dir}" );
}
if( ! -e "$State{dbrecenteqs_dir}/$State{institute_logo_filebase}" ) {
	system( "/bin/cp $State{institute_logo} $State{dbrecenteqs_dir}" );
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

	create_focusmap( $evid, @db );
	create_focusmap_html( $evid, @db );

} elsif( $opt_h ) {

	@dbwebmaps = dbprocess( @db, "dbopen webmaps",
				     "dbsubset evid != NULL",
				     "dbjoin event",
				     "dbjoin origin event.prefor#origin.orid",
				     "dbjoin mapassoc mapname origin.orid#mapassoc.orid"
				     );

	$nmaps = dbquery( @dbwebmaps, "dbRECORD_COUNT" );

	print "dbrecenteqs: updating html for $nmaps focus maps\n";

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

	print "dbrecenteqs: creating $nmaps focus maps\n";

	for( $dbneedmaps[3]=0; $dbneedmaps[3]<$nmaps; $dbneedmaps[3]++ ) {

		( $evid ) = dbgetv( @dbneedmaps, "evid" );

		print sprintf "Creating focus map %d of %d:\n",
			      $dbneedmaps[3]+1, $nmaps;

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

if( defined( $State{"workdir"} ) && $State{"workdir"} ne "" ) {
	system( "/bin/rm -rf $State{workdir}" );
}
