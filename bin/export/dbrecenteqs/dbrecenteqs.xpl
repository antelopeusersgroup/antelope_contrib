require "getopts.pl" ;
require "dbrecenteqs.pl";
require "winding.pl";
use Datascope;
use Image::Magick;
 

sub init_globals {

	setup_State();

	my( $Pf ) = "dbrecenteqs";

	my( @params ) = (
		"title",
		"caption_default",
		"html_base",
		"web_topdir",
		"wiggle",
		"local_logo",
		"local_html_home",
		"description_of_local_html_home",
		"region_phrases_database"
		);
	
	foreach $param ( @params ) {
		$State{$param} = pfget( $Pf, $param );
	}

	if( $State{html_base} !~ m@/$@ ) { $State{html_base} .= "/"; }
	if( $State{web_topdir} !~ m@/$@ ) { $State{web_topdir} .= "/"; }

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

sub set_hypocenter_symbol {
	my( %Mapspec ) = %{shift( @_ )};
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
	$symcolor = $Mapspec{quakecolor};

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

sub hypocenter_vitals {
	my( @db ) = @_;

	my( $lat, $lon, $depth, $time, $auth ) = 	
		dbgetv( @db, "lat", "lon", "depth", "time", "origin.auth" );

	my( $local_day ) = epoch2str( $time, 
		"%A %B %o, %Y", $ENV{TZ} );
	my( $local_hour ) = epoch2str( $time, 
		"%I:%M %p %Z", $ENV{TZ} );
	my( $utc_time ) = epoch2str( $time, "%m/%d/%Y %H:%M:%S.%s %Z" );

	my( $mag_description ) = mag_description( @db );

	my( $table ) = "<TABLE BORDER=6 CELLSPACING=6 BGCOLOR=beige>\n";
	$table .= "<TR><TD>Local Date:</TD><TD>$local_day</TD></TR>\n";
	$table .= "<TR><TD>Local Time:</TD><TD>$local_hour</TD></TR>\n";
	$table .= "<TR><TD>Universal Time:</TD><TD>$utc_time</TD></TR>\n";
	$table .= "<TR><TD>Magnitude:</TD><TD>$mag_description</TD></TR>\n";
	$table .= "<TR><TD>Latitude:</TD><TD>$lat</TD>\n";
	$table .= "<TR><TD>Longitude:</TD><TD>$lon</TD>\n";
	$table .= "<TR><TD>Depth:</TD><TD>$depth km</TD>\n";
	$table .= "<TR><TD>Author:</TD><TD>$auth</TD>\n";
	$table .= "</TABLE>\n";

	return $table;
}

sub location_header_line {
	my( $lat, $lon ) = @_;

	if( ! defined( $State{region_phrases_database} ) ) {

		return "Earthquake: " . grname( $lat, $lon );
	} 

	my( @regions ) = 
		get_containing_regions( @{$State{region_phrases_database}},
					$lat, $lon );

	if( defined( $where = shift( @regions ) ) ) {
		return "Earthquake $where";
	} else {
		return "Earthquake: " . grname( $lat, $lon );
	}

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
			      "dbjoin origin",
			      "dbjoin -o mapassoc mapname orid" );
	my( @dbprefor ) = dbsubset( @db, "orid==prefor" );
	$dbprefor[3] = 0;
	my( @dbnonprefors ) = dbsubset( @db, "orid != prefor" );

	my( $lat, $lon ) = dbgetv( @dbprefor, "lat", "lon" );

	open( H, ">$html_filename" );
	print H "<HTML>\n";
	print H "<BODY BGCOLOR='white'>\n";
	print H "<MAP NAME=\"vitals\">" . 
		imagemap_symbol( @dbprefor, "#prefor" ) .
		"</MAP>\n";
	print H "<CENTER>";
	print H hyperlinked_logo();
	print H "</CENTER>\n";
	print H "<BR>";
	print H "<CENTER><H1>";
	print H "<A HREF=\"$State{html_base}\"><IMG ALIGN='top' " .
		"SRC=\"$State{html_base}$State{wiggle}\" " .
		"ALT=\"Link to $State{title}\"></A>";
	print H location_header_line( $lat, $lon ) .
		"</H1></CENTER>\n";
	print H "<BR>";
	print H "<CENTER><IMG SRC=\"$dfile\" align=center " .
			"USEMAP=\"#vitals\"></CENTER>\n";
	print H "<BR>";
	print H "<CENTER>\n";
	print H "<A NAME=\"prefor\">";
	print H "<H2>Preferred Vital Statistics:</H2>\n";
	print H hypocenter_vitals( @dbprefor );
	my( $nothers ) = dbquery( @dbnonprefors, "dbRECORD_COUNT" );
	if( $nothers > 0 ) {
		print H "<H2>Other solutions:</H2>\n";
	}
	for( $dbnonprefors[3]=0; $dbnonprefors[3]<$nothers;$dbnonprefors[3]++ ){
		print H hypocenter_vitals( @dbnonprefors );
	}
	print H "</CENTER>\n";
	print H "<BR>";
	print H "<BR>\n<CENTER>",
		other_map_links( @db, $mapname ),
		"</CENTER>\n";
	print H "</BODY>\n";
	print H "</HTML>\n";
	close( H );
}

sub create_focusmap {
	my( $evid ) = shift;
	my( @db ) = @_;

	@db = dbprocess( @db, "dbopen event",
			      "dbjoin origin", 
			      "dbsubset evid == $evid",
			      "dbsubset orid == prefor" );

	$db[3] = 0; 

	my( $time, $lat, $lon, $orid ) =
	  dbgetv( @db, "time", "lat", "lon", "orid" );

	$Focus_Mapspec{filebase} = "evid$evid";
	$Focus_Mapspec{mapname} = $Focus_Mapspec{filebase};
	$Focus_Mapspec{lonc} = unwrapped_lon( \%Focus_Mapspec, $lon );
	$Focus_Mapspec{latc} = $lat;

	# Try to keep the directory names short enough for dir field
	my( $reldir ) = concatpaths( $State{quakesubdir}, 
	 	   epoch2str( $time, "%Y%j" ) .
			      "_$Focus_Mapspec{mapname}" );
	mkdir( concatpaths( $State{web_topdir}, $reldir ), 0755 );

	$Focus_Mapspec{"psfile"} = concatpaths( $State{"workdir"},
			"$Focus_Mapspec{filebase}.ps" );
	$Focus_Mapspec{"pixfile"} = concatpaths( $State{web_topdir}, $reldir );
	$Focus_Mapspec{"pixfile"} = concatpaths( $Focus_Mapspec{"pixfile"},
			 "$Focus_Mapspec{filebase}.$Focus_Mapspec{format}" );
	
	%Focus_Mapspec = %{set_projection( \%Focus_Mapspec )};
	%Focus_Mapspec = %{set_rectangle( \%Focus_Mapspec )};

	%Focus_Mapspec = %{create_map( \%Focus_Mapspec )};

	my( $modified_image ) = $Focus_Mapspec{clean_image}->Clone();

	( $x, $y ) = latlon_to_xy( $Focus_Mapspec{proj},
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
	  		set_hypocenter_symbol( \%Focus_Mapspec, @db );

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

	my( @dbwebmaps ) = dblookup( @db, "", "webmaps", "", "" );

	my( $url ) = $State{html_base} . 
		concatpaths( $reldir, "$Focus_Mapspec{filebase}.html" );

	$dbwebmaps[3] = dbaddv( @dbwebmaps, 
	    	"mapname", $Focus_Mapspec{mapname},
		"evid", $evid,
    	    	"dir", concatpaths( $State{web_topdir}, $reldir ),
    	    	"dfile", "$Focus_Mapspec{filebase}.$Focus_Mapspec{format}",
    	    	"url", $url );

	my( $webmap_image ) = dbextfile( @dbwebmaps );

	$modified_image->Write(filename=>$webmap_image);

	undef( $modified_image );
	undef( $Focus_Mapspec{clean_image} );

	eliminate_from_mapassoc( @db, $Focus_Mapspec{mapname} );

	my( @dbmapassoc ) = dblookup( @db, "", "mapassoc", "", "" );
	dbaddv( @dbmapassoc, "mapname", $Focus_Mapspec{mapname},
			     "orid", $orid,
			     "x", $x,
			     "y", $y, 
			     "symsize", $symsize,
			     "symshape", $symshape,
			     "symcolor", $symcolor );
}

sub hyperlinked_logo {

	return "<A HREF=\"$State{local_html_home}\">" .
		"<IMG SRC=\"$State{html_base}$State{local_logo}\" " .
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
		       "dbsort -r time",
		       "dbjoin webmaps evid" );

	my( $nsymbols ) = dbquery( @db, "dbRECORD_COUNT" );

	my( $table ) = "<CENTER><H2>" . 
		       "Earthquakes Shown on This Page:" .
		       "</H2></CENTER>\n";

	my( $table ) .= "<TABLE BORDER=6 CELLSPACING=6 BGCOLOR=beige>\n";

	$table .= "<TR><TD BGCOLOR=white>Earthquakes Shown " .
		  "on This Page:</TD></TR>\n";
	
	for( $db[3]=0; $db[3]<$nsymbols; $db[3]++ ) { 
		my( $lat, $lon, $depth, $time, $url ) = 
			dbgetv( @db, "lat", "lon", "depth", "time", "url" );
		my( $mag_description ) = mag_description( @db );
		my( $local_time ) = epoch2str( $time, 
		"%I:%M %p %Z on %A %B %o, %Y", $ENV{TZ} );
		$table .= "<TR><TD>";
		$table .= "<A HREF=\"$url\">";
		$table .= "Magnitude $mag_description, ";
		$table .= "$local_time";
		$table .= "</A></TD></TR>\n";
	}

	$table .= "</TABLE>\n";

	return $table;

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
		       "dbsort -r time",
		       "dbjoin webmaps evid" );

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

	my( $image_relpath ) = dbextfile( @db );
	$image_relpath = substr( $image_relpath, 	
				 length( $State{web_topdir} ) );
	
	open( H, ">$html_filename" );
	print H "<HTML><HEAD>\n";
	print H "<BASE HREF=\"$State{html_base}\">\n";
	print H "<TITLE>$State{title}</TITLE>\n";
	print H "</HEAD>\n";
	print H "<BODY BGCOLOR='white'>\n";

	print H clientside_imagemap_quakes( @db, $mapname );

	print H "<CENTER>";
	print H hyperlinked_logo();
	print H "</CENTER>\n";
	print H "<H1 ALIGN='center'>";
	if( $mapclass eq "index" ) {
		print H "<IMG align=top src=\"$State{wiggle}\">" .
			"$State{title}</H1>\n";
	} else {
		print H "<A HREF=\"$State{html_base}\">";
		print H "<IMG align=top SRC=\"$State{wiggle}\" " .
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

	print H "</BODY></HTML>\n";

	close( H );
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
  		set_hypocenter_symbol( \%Mapspec, @db );

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

init_globals();

if ( ! &Getopts('') || @ARGV != 1 ) {
	die ( "Usage: $0 database\n" ); 
} else {
	$dbname = $ARGV[0];
}

@db = dbopen( $dbname, "r+" );
@dbmapstock = dblookup( @db, "", "mapstock", "", "" );

if( ! dbquery( @dbmapstock, "dbTABLE_PRESENT" ) ) { 
	die( "Couldn't find $dbname.mapstock\n" );
}

@dbmapstock = dbsubset( @dbmapstock, "mapclass == \"index\"" );
if( dbquery( @dbmapstock, "dbRECORD_COUNT" ) <= 0 ) {
	die( "no index map(s) in $dbname.mapstock\n" );	
}

@dbneedmaps = dbprocess( @db, 
			 "dbopen origin", 
			 "dbnojoin webmaps evid#evid" ); 

$nmaps = dbquery( @dbneedmaps, "dbRECORD_COUNT" );
print "dbrecenteqs: creating $nmaps focus maps\n";

for( $dbneedmaps[3]=0; $dbneedmaps[3]<$nmaps; $dbneedmaps[3]++ ) {

	( $evid ) = dbgetv( @dbneedmaps, "evid" );

	create_focusmap( $evid, @db );
	create_focusmap_html( $evid, @db );
}

# Should really plot only the prefor for each evid on the stock maps
@dbstockmaps = dbprocess( @db, 
			  "dbopen origin", 
			  "dbtheta mapstock",
			  "dbsort mapname time",
			  "dbgroup mapname" );

$ngroups = dbquery( @dbstockmaps, "dbRECORD_COUNT" );
print "dbrecenteqs: updating $ngroups stock maps\n";

for( $dbstockmaps[3] = 0; $dbstockmaps[3] < $ngroups; $dbstockmaps[3]++ ) {

	update_stockmap( @dbstockmaps );
	create_stockmap_html( @dbstockmaps );

}

dbcrunch( dblookup( @db, "", "mapassoc", "", "dbALL" ) );
