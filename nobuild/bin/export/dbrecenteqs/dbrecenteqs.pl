
$dbrecenteqs::scriptlog = "";

sub set_scriptlog {
	my( $scriptfile ) = @_;

	if( $scriptfile ne "" ) {
		
		$dbrecenteqs::scriptlog = $scriptfile;
	}
}

sub system_scriptlog {
	my( $cmd ) = @_;

	if( $dbrecenteqs::scriptlog ne "" ) {
		
		open( L, ">>$dbrecenteqs::scriptlog" );
		print L "$cmd\n";
		close( L );
	}

	system( $cmd );
}

sub pfget_Mapspec {
	my( $pf, $hashname ) = @_;
	my( $mapspec );

	my( $hash ) = pfget( $pf, $hashname );

	if( ! defined( $hash ) ) {
		die( "dbrecenteqs: $hashname not defined in $pf.pf. Bye.\n" );
	} 

	if( defined( $hash->{include} ) ) {
		$mapspec = pfget( $pf, $hash->{include} );
	}

	foreach $key ( keys( %$hash ) ) {
		$mapspec->{$key} = $hash->{$key};
	}
	
	my( @path_params ) = (
		"depth_color_palette_file",
		"map_color_palette_file",
		"map_landmask_palette_file",
		"drape_color_palette_file",
		"cities_dbname",
		"grddb",
		"legend",
		"hypocenter_dbname",
		"stylesheet",
		"vrml_stylesheet",
		"stations_stylesheet"
		);
	# N.B. ( Handle the linefiles hash in plot_linefiles() )

	if( ! defined( $mapspec->{mapname} ) ) {

		die( "No 'mapname' parameter defined in $hashname array\n" );

	} elsif( length( "$mapspec->{mapname}" ) > 20 ) {

		die( "Please shorten mapname \"$mapspec->{mapname}\" to 20 " .
		     "characters or less\n" );
	}

	if( ! defined( $mapspec->{source} ) ) {
		die( "No 'source' parameter defined in $hashname array\n" );
	}

	foreach $param ( @path_params ) {

		if( defined( $mapspec->{$param} ) ) {

			$mapspec->{$param} = datafile_abspath( $mapspec->{$param} );
		}
	}

	$mapspec->{longitude_branchcut_low} =
		$mapspec->{longitude_branchcut_high} - 360;

	if( defined( $mapspec->{drape_color_palette_file} ) && 
	    -e "$mapspec->{drape_color_palette_file}" ) {

		open( C, "$mapspec->{drape_color_palette_file}" );
		while( <C> ) {
			next if /^\s*[#NFB]/;
			next if /^\s*$/;
			push( @{$mapspec->{drape_color_palette}}, $_ );
		}
		close( C );
	}

	if( defined( $mapspec->{legend} ) && $mapspec->{legend} ne "" ) {

		chomp( $mapspec->{legend_filebase} = `basename $mapspec->{legend}` );

	} else {

		$mapspec->{legend} = "";
	}

	return $mapspec;
}

sub setup_State {

	$State{pf} =~ s/\.pf$//;

	if( system("pfecho $State{pf} > /dev/null 2>&1" ) ) {
		die( "Couldn't find $State{pf}.pf. Bye.\n" );
	}

	$pf_change_time = "1244585155";

	if( pfrequire( $State{pf}, $pf_change_time ) < 0 ) {

		elog_flush( 1, 0 );
		die( "The parameter file '$State{pf}.pf' is out of date. " .
		     "Please upgrade to the latest version. Bye.\n" );

	}
	my( @params ) = (
		"pixfile_conversion_method",
		);
	
	foreach $param ( @params ) {
		$State{$param} = pfget( $State{pf}, $param );
	}

	$State{workdir} = "/tmp/dbrecenteqs_$<_$$";
	mkdir( $State{workdir}, 0755 );

	my( @helpers ) = (
		"pscoast",
		"psbasemap",
		"psxy",
		"pstext",
		"grdcut",
		"grdgradient",
		"grdimage",
		"gmtdefaults",
		);

	foreach $helper ( @helpers ) {
		next if check_for_executable( $helper );
		die( "Couldn't find executable named $helper\n" );
	}

	check_gmt_units();
}

sub check_dir_dfile {
	my( $dfile ) = pop( @_ );
	my( $dir ) = pop( @_ );
	my( @db ) = @_;

	@db = dblookup( @db, "", "", "dir", "" );
	$dir_size = dbquery( @db, "dbFIELD_SIZE" );

	if( length( $dir ) > $dir_size ) {
		elog_complain
			"\n\t************************************\n" . 
			"\tWARNING: Truncating dir\n\t'$dir';\n" .
			"\tPROBABLE DATABASE CORRUPTION!!\n" .
			"\t************************************\n\n";
	}

	@db = dblookup( @db, "", "", "dfile", "" );
	$dfile_size = dbquery( @db, "dbFIELD_SIZE" );

	if( length( $dfile ) > $dfile_size ) {
		elog_complain
			"\n\t************************************\n" . 
			"\tWARNING: Truncating dfile\n\t'$dfile';\n" .
			"\tPROBABLE DATABASE CORRUPTION!!\n" .
			"\t************************************\n\n";
	}
}

sub expansion_schema_present {
	my( @db ) = @_;

	my( $present ) = 0;

	my( @tables ) = dbquery( @db, "dbSCHEMA_TABLES" );

	if( grep( /mapstock/, @tables ) &&
    	    grep( /mapassoc/, @tables ) &&
    	    grep( /quakeregions/, @tables ) &&
    	    grep( /webmaps/, @tables ) ) {
	
		$present++;

	} else {

		$present = 0;
	}

	if( $present ) {
		@db = dblookup( @db, "", "webmaps", "", "" );
		$lddate_used = 
			grep( /lddate/, dbquery( @db, "dbTABLE_FIELDS" ) );
		if( ! $lddate_used ) {
			elog_complain "Please upgrade to dbrecenteqs1.2.\n";
			$present = 0;
		}
	}

	if( $present ) {
		@db = dblookup( @db, "", "mapassoc", "", "" );
		$symtype_used = 
			grep( /symtype/, dbquery( @db, "dbTABLE_FIELDS" ) );
		if( ! $symtype_used ) {
			elog_complain "Please upgrade to dbrecenteqs1.2.\n";
			$present = 0;
		}
	}

	return $present;
}

sub gme_schema_present {
	my( @db ) = @_;

	my( $present ) = 0;

	my( @tables ) = dbquery( @db, "dbSCHEMA_TABLES" );

	if( grep( /qgrid/, @tables ) ) {
	
		$present++;

	} else {

		$present = 0;
	}

	return $present;
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

sub remove_stale_webmaps {
	my( @db ) = @_;
	
	if( $State{use_qgrids} ) {

		@db = dbprocess( @db, "dbopen webmaps",
			      "dbjoin event",
			      "dbjoin origin event.evid#origin.evid",
			      "dbjoin -o qgrid origin.orid#qgrid.orid",
			      "dbsubset origin.lddate > webmaps.lddate || event.lddate > webmaps.lddate || qgrid.lddate > webmaps.lddate",
			      "dbseparate webmaps" );

	} else {

		@db = dbprocess( @db, "dbopen webmaps",
			      "dbjoin event",
			      "dbjoin origin event.evid#origin.evid",
			      "dbsubset origin.lddate > webmaps.lddate || event.lddate > webmaps.lddate",
			      "dbseparate webmaps" );
	}

	my( $stale_nrecs ) = dbquery( @db, "dbRECORD_COUNT" );

	if( $opt_v ) {
		elog_notify "Removing $stale_nrecs stale webmap entries\n";
	}

	for( $db[3] = 0; $db[3] < $stale_nrecs; $db[3]++ ) {
		my( $mapname ) = dbgetv( @db, "mapname" );
		@dbwebmaps = dblookup( @db, "", "webmaps", "mapname", "$mapname" );
		if( $dbwebmaps[3] >= 0 ) {
			dbmark( @dbwebmaps );
		}
	}
	if( $stale_nrecs > 0 ) {
		dbcrunch( @dbwebmaps );
	}

	return;
}

sub xml_to_output {
	my( $xml_file, $xsl_file, $output_file ) = @_;

	my( $parser ) = XML::LibXML->new();

	my( $xslt ) = XML::LibXSLT->new();

	my( $source ) = $parser->parse_file( "$xml_file" );

	my( $style_doc ) = $parser->parse_file( "$xsl_file" );

	my $stylesheet = $xslt->parse_stylesheet( $style_doc );

	my( $results ) = $stylesheet->transform( $source );

	my( $outputfd ) = IO::File->new( ">$output_file" );
	
	print $outputfd $stylesheet->output_string( $results );

	$outputfd->close();
}

sub normal_lon {
	my( $unwrapped_lon ) = @_;

	my( $normal_lon );

	$normal_lon = $unwrapped_lon;

	while( $normal_lon < -180 ) { $normal_lon += 360; }
	while( $normal_lon > 180 ) { $normal_lon -= 360; }

	return $normal_lon;
}

sub unwrapped_lon {
	my( %Mapspec ) = %{shift( @_ )};
	my( $normal_lon ) = shift( @_ );

	my( $unwrapped_lon );

	$unwrapped_lon = $normal_lon;

	while( $unwrapped_lon < $Mapspec{"longitude_branchcut_low"} ) {
		$unwrapped_lon += 360;
	}
	while( $unwrapped_lon > $Mapspec{"longitude_branchcut_high"} ) {
		$unwrapped_lon -= 360;
	}

	return $unwrapped_lon;
}

sub edp_lonlat {
	my( %Mapspec ) = %{shift( @_ )};
	my( $lonc, $latc, $dellon, $dellat ) = @_;	
	my( $lon, $lat, $azimuth );

	if( $dellon < 1.e-10 && $dellat < 1.e-10 && 
	    $dellon > -1.e-10 && $dellat > -1.e-10 ) {
		$azimuth = 0.0;
	} else {
		$azimuth = 90.0*atan2($dellon,$dellat)/atan2(1.0,0.0);
	}
	$distance = sqrt($dellon*$dellon+$dellat*$dellat);

	my( @db ) = ( -102, -102, -102, -102 );
	my( $normal_lonc ) = normal_lon( $lonc );

	$lat = dbex_eval( @db, 
		"latitude($latc,$normal_lonc,$distance,$azimuth)" );
	$lon = dbex_eval( @db, 
		"longitude($latc,$normal_lonc,$distance,$azimuth)" );

	return ( unwrapped_lon( \%Mapspec, $lon ), $lat );
}

sub latlon_to_xy {
	my( $proj, $lat, $lon, $latc, $lonc, 
	    $xc, $yc, $xscale_pixperdeg, $yscale_pixperdeg ) = @_;

	if( $proj eq "edp" ) {

		return latlon_to_edpxy( $lat, $lon, 
			$latc, $lonc, $xc, $yc, 
			$xscale_pixperdeg, $yscale_pixperdeg );

	} else {

		die( "Don't understand projection $proj\n" );
	}
}

sub latlon_to_edpxy {
	my( $lat, $lon, $latc, $lonc, 
	    $xc, $yc, 
	    $xscale_pixperdeg, $yscale_pixperdeg ) = @_;

	my( @db ) = dbinvalid();

	my( $dist_deg ) = dbex_eval( @db, 
		"distance($latc,$lonc,$lat,$lon)" );
	my( $az ) = dbex_eval( @db, 
	  	"azimuth($latc,$lonc,$lat,$lon)" );

	my( $x ) = int( $xc + $xscale_pixperdeg * $dist_deg * sin($az*3.14/180) );
	my( $y ) = int( $yc - $yscale_pixperdeg * $dist_deg * cos($az*3.14/180) );

	return( $x, $y );
}

sub set_rectangles {
	my( %Mapspec ) = %{shift( @_ )};

	( $Mapspec{"lon_ll"}, $Mapspec{"lat_ll"} ) = edp_lonlat( 
					\%Mapspec,
					$Mapspec{"lonc"},
					$Mapspec{"latc"},
					$Mapspec{"left_dellon"},
					$Mapspec{"down_dellat"} );

	( $Mapspec{"lon_ul"}, $Mapspec{"lat_ul"} ) = edp_lonlat( 
					\%Mapspec,
					$Mapspec{"lonc"},
					$Mapspec{"latc"},
					$Mapspec{"left_dellon"},
					$Mapspec{"up_dellat"} );

	( $Mapspec{"lon_ur"}, $Mapspec{"lat_ur"} ) = edp_lonlat( 
					\%Mapspec,
					$Mapspec{"lonc"},
					$Mapspec{"latc"},
					$Mapspec{"right_dellon"},
					$Mapspec{"up_dellat"} );

	( $Mapspec{"lon_lr"}, $Mapspec{"lat_lr"} ) = edp_lonlat( 
					\%Mapspec,
					$Mapspec{"lonc"},
					$Mapspec{"latc"},
					$Mapspec{"right_dellon"},
					$Mapspec{"down_dellat"} );

	my( $center_high_lon, $center_high_lat ) = edp_lonlat(
					\%Mapspec,
					$Mapspec{"lonc"},
					$Mapspec{"latc"},
					0,
					$Mapspec{"up_dellat"} );

	my( $center_low_lon, $center_low_lat ) = edp_lonlat(
					\%Mapspec,
					$Mapspec{"lonc"},
					$Mapspec{"latc"},
					0,
					$Mapspec{"down_dellat"} );

	my( $leftmost_lon ) = min( $Mapspec{"lon_ll"}, $Mapspec{"lon_ul"} );

	my( $lowest_lat ) = min( $Mapspec{"lat_ll"}, $Mapspec{"lat_lr"} );
	my( $lowest_lat ) = min( $center_low_lat, $lowest_lat );

	my( $rightmost_lon ) = max( $Mapspec{"lon_ur"}, $Mapspec{"lon_lr"} );

	my( $highest_lat ) = max( $Mapspec{"lat_ur"}, $Mapspec{"lat_ul"} );
	my( $highest_lat ) = max( $center_high_lat, $highest_lat );

	$Mapspec{"Rectangle"} = sprintf( "-R%.4f/%.4f/%.4f/%.4fr", 
					$Mapspec{"lon_ll"},
					$Mapspec{"lat_ll"},
					$Mapspec{"lon_ur"},
					$Mapspec{"lat_ur"} );

	# The small additions and subtractions are for 
	# sign-independent rounding (i.e., this is not hard-coded cheating...):
	$Mapspec{"InclusiveRectangle"} = sprintf( "-R%d/%d/%d/%d", 
					int( $leftmost_lon - 1 ),
					int( $rightmost_lon + 1.5 ),
					int( $lowest_lat - 1 ),
					int( $highest_lat + 1.5 ) );

	return \%Mapspec;
}

sub set_projection {
	my( %Mapspec ) = %{shift( @_ )};

	if( $Mapspec{"proj"} eq "edp" ) {
		$Mapspec{"Projection"} =
			sprintf( "-JE%.4f/%.4f/%.4f",
				  $Mapspec{"lonc"},
				  $Mapspec{"latc"},
				  $Mapspec{"size_inches"} );
	} else {
		die( "Projection $Mapspec{proj} not supported.\n" );
	} 

	return \%Mapspec;
}

sub more_ps {
	my( $position ) = shift( @_ );

	if( $position eq "single" ) { 
		return ( " ", ">" );
	} elsif( $position eq "first" ) { 
		return ( "-K ", ">" );
	} elsif( $position eq "middle" ) {
		return ( "-O -K ", ">>" );
	} elsif( $position eq "last" ) {
		return ( "-O", ">>" );
	} else {
		elog_complain "Unknown position $position in &more_ps\n";
		return "";
	}
}

sub check_gmt_units {
	my( $cmd ) = "gmtdefaults -L | grep MEASURE_UNIT"; 

	my( $units ) = `$cmd`;
	chomp( $units );
	$units =~ s/.*MEASURE_UNIT\s+=\s+//;

	if( $units ne "inch" ) {

		elog_die "Please set your GMT MEASURE_UNIT to 'inch' (see gmtset(1) and gmtdefaults(1) man pages). Bye.\n";
	}
}

sub plot_basemap {
	my( %Mapspec ) = %{shift( @_ )};
	my( $position ) = shift( @_ );

	my( $more, $redirect ) = more_ps( $position );

	my( $cmd ) = "psbasemap " . 
			"-X0 -Y0 -P $V " .
			"-Bg$Mapspec{gridline_interval_deg}wesn " .
			"$Mapspec{Rectangle} $Mapspec{Projection} " . 
			$more . 
			"$redirect $Mapspec{psfile}";
	if( $opt_v ) {
		elog_notify "$cmd\n";
	}
	system_scriptlog( $cmd );
}

sub plot_lakes {
	my( %Mapspec ) = %{shift( @_ )};
	my( $position ) = shift( @_ );

	my( $more, $redirect ) = more_ps( $position );

	my( $cmd ) = "pscoast $V -P -X0 -Y0 " .
			"$Mapspec{Rectangle} $Mapspec{Projection} " .
			"-D$Mapspec{detail_density} " .
			"-C0/0/255 " .
			$more .
			"$redirect $Mapspec{psfile}";
			
	if( $opt_v ) {
		elog_notify "$cmd\n";
	}
	system_scriptlog( $cmd );
}

sub plot_state_boundaries {
	my( %Mapspec ) = %{shift( @_ )};
	my( $position ) = shift( @_ );

	my( $more, $redirect ) = more_ps( $position );

	my( $cmd ) = "pscoast $V -P -X0 -Y0 " .
			"$Mapspec{Rectangle} $Mapspec{Projection} " .
			"-D$Mapspec{detail_density} " .
			"-N2/2/0/0/0 " . 
			$more . 
			"$redirect $Mapspec{psfile}";
			
	if( $opt_v ) {
		elog_notify "$cmd\n";
	}
	system_scriptlog( $cmd );
}

sub plot_national_boundaries {
	my( %Mapspec ) = %{shift( @_ )};
	my( $position ) = shift( @_ );

	my( $more, $redirect ) = more_ps( $position );

	my( $cmd ) = "pscoast $V -P -X0 -Y0 " .
			"$Mapspec{Rectangle} $Mapspec{Projection} " .
			"-D$Mapspec{detail_density} " .
			"-N1/5/0/0/0 " . 
			$more . 
			"$redirect $Mapspec{psfile}";
			
	if( $opt_v ) {
		elog_notify "$cmd\n";
	}
	system_scriptlog( $cmd );
}

sub plot_rivers {
	my( %Mapspec ) = %{shift( @_ )};
	my( $position ) = shift( @_ );

	my( $more, $redirect ) = more_ps( $position );

	my( $cmd ) = "pscoast $V -P -X0 -Y0 " .
			"$Mapspec{Rectangle} $Mapspec{Projection} " .
			"-D$Mapspec{detail_density} " .
			"-Ir/1/0/0/255 " .
			$more .
			"$redirect $Mapspec{psfile}";
			
	if( $opt_v ) {
		elog_notify "$cmd\n";
	}
	system_scriptlog( $cmd );
}

sub min {
	my( $a, $b ) = @_;

	return $a < $b ? $a : $b;
}

sub max {
	my( $a, $b ) = @_;

	return $a > $b ? $a : $b;
}

sub make_cities_tempfiles {
	my( %Mapspec ) = %{shift( @_ )};

	my( $locs_tempfile ) = "$State{workdir}/cities_$<_$$";
	my( $names_tempfile ) = "$State{workdir}/citynames_$<_$$";

	my( @db ) = dbopen( $Mapspec{cities_dbname}, "r" );
	@db = dblookup( @db, "", "places", "", "" );

	my( $nrecs ) = dbquery( @db, "dbRECORD_COUNT" );
			
	open( C, ">$locs_tempfile" );
	open( N, ">$names_tempfile" );

	for( $db[3] = 0; $db[3] < $nrecs; $db[3]++ ) {
		my( $lat, $lon, $place ) = 
			dbgetv( @db, "lat", "lon", "place" );
		print C sprintf( "%.4f %.4f\n", 
			unwrapped_lon( \%Mapspec, $lon ), $lat );
		print N sprintf( "%.4f %.4f %s 0.0 %s %s %s\n",
			$lon+$Mapspec{cityname_shift_deg},
			$lat,
			$Mapspec{cityname_fontsize},
			$Mapspec{cityname_fontno},
			$Mapspec{cityname_fontjustify},
			$place );
	}

	close( C );
	close( N );

	dbclose( @db );

	return ( $locs_tempfile, $names_tempfile );
}

sub make_stations_tempfiles {
	my( %Mapspec ) = %{shift( @_ )};

	my( $stas_tempfile ) = "$State{workdir}/stations_$<_$$";
	my( $stanames_tempfile ) = "$State{workdir}/stanames_$<_$$";
	my( $focus_stas_tempfile ) = "$State{workdir}/focus_stations_$<_$$";
	my( $focus_stanames_tempfile ) = "$State{workdir}/focus_stanames_$<_$$";

	my( @db ) = dbopen( $Mapspec{stations_dbname}, "r" );

	if( defined( @{$Mapspec{stations_subset}} ) ) {

		if( $opt_v ) {
			
			elog_notify( "Filtering stations to plot with stations_subset instructions\n" );
		}

		@db = dbprocess( @db, @{$Mapspec{stations_subset}} );

	} else {

		@db = dblookup( @db, "", "site", "", "" );
		@db = dbsubset( @db, "offdate == NULL" );
	}

	my( $nrecs ) = dbquery( @db, "dbRECORD_COUNT" );
			
	open( S, ">$stas_tempfile" );
	open( N, ">$stanames_tempfile" );
	open( FS, ">$focus_stas_tempfile" );
	open( FN, ">$focus_stanames_tempfile" );

	for( $db[3] = 0; $db[3] < $nrecs; $db[3]++ ) {
		my( $lat, $lon, $sta ) = 
			dbgetv( @db, "lat", "lon", "sta" );

		my( $stafd, $namefd );
		my( $staname_shift_deg );
		my( $staname_fontsize );
		my( $staname_fontno );
		my( $staname_fontjustify );

		if( $Mapspec{focus_sta_expr} ne "" &&
		 dbex_eval( @db, "$Mapspec{focus_sta_expr}" ) ) {

			$stafd = *FS;
			$namefd = *FN;

			$staname_shift_deg = $Mapspec{focus_staname_shift_deg};
			$staname_fontsize = $Mapspec{focus_staname_fontsize};
			$staname_fontno = $Mapspec{focus_staname_fontno};
			$staname_fontjustify = $Mapspec{focus_staname_fontjustify};
			
		} else {

			$stafd = *S;
			$namefd = *N;

			$staname_shift_deg = $Mapspec{staname_shift_deg};
			$staname_fontsize = $Mapspec{staname_fontsize};
			$staname_fontno = $Mapspec{staname_fontno};
			$staname_fontjustify = $Mapspec{staname_fontjustify};
		}

		print $stafd sprintf( "%.4f %.4f\n", 
			unwrapped_lon( \%Mapspec, $lon ), $lat );
		print $namefd sprintf( "%.4f %.4f %s 0.0 %s %s %s\n",
			$lon+$staname_shift_deg,
			$lat,
			$staname_fontsize,
			$staname_fontno,
			$staname_fontjustify,
			$sta );
	}

	close( S );
	close( N );
	close( FS );
	close( FN );

	dbclose( @db );

	return ( $stas_tempfile, $stanames_tempfile,
		 $focus_stas_tempfile, $focus_stanames_tempfile );
}

sub make_hypocenter_tempfile {
	my( %Mapspec ) = %{shift( @_ )};

	my( $tempfile ) = "$State{workdir}/hypos_$<_$$";

	my( @db ) = dbopen( $Mapspec{hypocenter_dbname}, "r" );
	@db = dblookup( @db, "", "origin", "", "" );

	my( $minlon ) = min( $Mapspec{lon_ll}, $Mapspec{lon_ul} );
	my( $maxlon ) = max( $Mapspec{lon_lr}, $Mapspec{lon_ur} );

	$minlon = normal_lon( $minlon );
	$maxlon = normal_lon( $maxlon );

	my( $lon_wrap );

	if( $minlon > $maxlon ) {
		$lon_wrap = "||";
	} else {
		$lon_wrap = "&&";
	}

	my( $expr ) =
		"(mb >= $Mapspec{background_magmin} || " .
		" ml >= $Mapspec{background_magmin} || " .
		" ms >= $Mapspec{background_magmin} ) && " .
		"lat > $Mapspec{lat_ll} && " .
		"lat < $Mapspec{lat_ur} && " .
		"(lon > $minlon $lon_wrap " .
		"lon < $maxlon)";

	if( $opt_v ) {
		elog_notify "Subsetting database for hypocenters...";
	}
	@db = dbsubset( @db, $expr );
	if( $opt_v ) {
		elog_notify "done\n";
	}

	$nrecs = dbquery( @db, "dbRECORD_COUNT" );
			
	if( $opt_v ) {
		elog_notify "Building temp file of $nrecs hypocenters...";
	}
	open( H, ">$tempfile" );

	for( $db[3] = 0; $db[3] < $nrecs; $db[3]++ ) {
		my( $lat, $lon, $depth ) = 
			dbgetv( @db, "lat", "lon", "depth" );
		print H sprintf( "%.4f %.4f %.2f\n", 
			unwrapped_lon( \%Mapspec, $lon ), $lat, $depth );
	}

	close( H );
	if( $opt_v ) {
		elog_notify "done\n";
	}

	dbclose( @db );

	return $tempfile;
}

sub plot_hypocenters {
	my( %Mapspec ) = %{shift( @_ )};
	my( $position ) = shift( @_ );

	if( ! defined( $Mapspec{hypocenter_dbname} ) || 
	      $Mapspec{hypocenter_dbname} eq "" ) {
		elog_complain
			"\n\t************************************\n" . 
			"\tWARNING: Skipping hypocenters--" .
			"\tno hypocenter_dbname specified\n" .
			"\t************************************\n\n";
		return;

	} elsif( ! -e "$Mapspec{hypocenter_dbname}.origin" ) {

		elog_complain
			"\n\t************************************\n" . 
			"\tWARNING: Skipping hypocenters--" .
			"\t$Mapspec{hypocenter_dbname}.origin not found\n" .
			"\t************************************\n\n";
		return;
	}

	my( $more, $redirect ) = more_ps( $position );

	my ( $hypocenter_tempfile ) =
		make_hypocenter_tempfile( \%Mapspec );

	my( $cmd ) = "cat $hypocenter_tempfile | psxy $V -P " .
			"$Mapspec{Rectangle} $Mapspec{Projection} " .
			"-C$Mapspec{depth_color_palette_file} " .
			"-Ss$Mapspec{background_magsize_pixels}p " .
			$more .
			"$redirect $Mapspec{psfile}";

	if( $opt_v ) {
		elog_notify "$cmd\n";
	}
	system_scriptlog( $cmd );

	if( $dbrecenteqs::scriptlog eq "" ) {

		unlink( $hypocenter_tempfile );
	}
}

sub next_round {
	my( $rough, $interval ) = @_;
	my( $clean );

	if( int( $rough / $interval ) == $rough / $interval ) { 

		$clean = $rough;

	} elsif( $rough > 0 ) {

		$clean = int( ( $rough + $interval ) / $interval ) * $interval;

	} else {

		$clean = int( $rough / $interval ) * $interval;
	}

	return $clean;
}

sub plot_qgrid {
	my( %Mapspec ) = %{shift( @_ )};
	my( $position ) = shift( @_ );

	my( @helpers ) = ( "xyz2grd", 
			   "cggrid_convert", 
			   "grdsample", 
			   "grdcontour" );

	foreach $helper ( @helpers ) {
		next if check_for_executable( $helper );
		die( "Couldn't find $helper in path. Fix path or " .
		     "don't enable the use_qgrids parameter.\n" );
	}

	if( $position ne "middle" ) {
		elog_complain "Warning: qgrid only implemented for " .
		 "middle-position plotting\n";
	}

	my( $more, $redirect ) = more_ps( $position );

	my( $gmt_qgrid )  = "$State{workdir}/qgrid_orig_$<_$$.grd";
	my( $gmt_qgrid_rectangle )  = 
		"-R$Mapspec{qgrid_minlon}/$Mapspec{qgrid_maxlon}/" .
		"$Mapspec{qgrid_minlat}/$Mapspec{qgrid_maxlat}";

	my( $cmd ) = "cggrid_convert $Mapspec{qgridfile} | " .
		     "xyz2grd $V -H1 -N0 " .
		     "-I$Mapspec{qgrid_dlon}/$Mapspec{qgrid_dlat} " .
		     "-G$gmt_qgrid " .
		     "$gmt_qgrid_rectangle";

	if( $opt_v ) {
		elog_notify "$cmd\n";
	}
	system_scriptlog( $cmd );

	if( ! defined( $Mapspec{qgrid_nintervals} ) ) {

		die( "qgrid_nintervals undefined in map config block." .
		     "Please add it, or turn off use_qgrids.\n" );
	}

	my( $interval );
	$interval = $Mapspec{qgrid_maxval} / $Mapspec{qgrid_nintervals};
	$interval = sprintf( "%e", $interval );
	$interval =~ s/^(\d).*(e.\d\d)$/$1$2/;
	$interval = sprintf( "%f", $interval );

	my( $units_name );
	if( $Mapspec{qgrid_units} =~ /(.*)gravity$/ ) {

		$units_name = $1 . "g";

	} else {

		$units_name = $Mapspec{qgrid_units};
	}

	$cmd = "grdcontour $V $gmt_qgrid " .
	       "$Mapspec{Rectangle} $Mapspec{Projection} " .
	       "-C$interval -N$units_name " .
	       "-W8/255/255/0 -Af12/255/255/0 " .
	       "-Q8 -L$interval/9999 " .
	       $more . 
	       "$redirect $Mapspec{psfile}";
			
	if( $opt_v ) {
		elog_notify "$cmd\n";
	}
	system_scriptlog( $cmd );

	unlink( "$gmt_qgrid" );
}

sub plot_drape {
	my( %Mapspec ) = %{shift( @_ )};
	my( $position ) = shift( @_ );

	if( $position ne "middle" ) {
		elog_complain "Warning: contours only implemented for " .
		 "middle-position plotting\n";
	}

	my( $more, $redirect ) = more_ps( $position );

	my( $cmd );

	if( ! -e "$Mapspec{grddb}" ) {

		elog_complain
			"\n\t************************************\n" . 
			"\tWARNING: Not plotting draped data values;\n" .
			"\tgrddb '$Mapspec{grddb}' not found\n" .
			"\t************************************\n\n";

		return;
	}
	my( @dbgrid ) = dbopen( "$Mapspec{grddb}", "r" );
	if( $dbgrid[0] < 0 ) {

	 	die( 
	 	"\n\t************************************\n" . 
	 	"\tERROR: Failed to open grddb $Mapspec{grddb}\n" .
	 	"\t************************************\n\n" );

	} 

	@dbgrid = dblookup( @dbgrid, "", "grids", "", "" );
	if( $dbgrid[1] < 0 ) {
		die( 
		"\n\t************************************\n" . 
		"\tERROR: Failed to open grids table of grddb $Mapspec{grddb}\n" .
		"\t************************************\n\n" );
	} 

	$dbgrid[3]=0;
	( $dx, $dy ) = dbgetv( @dbgrid, "dx", "dy" );

	$grdfile = "$State{workdir}/grd_$<_$$.grd";
	$gradfile = "$State{workdir}/grad_$<_$$.grad";
	$gmt_qgrid_file = "$State{workdir}/qgrid__$<_$$.grd";
	$gmt_qgridresamp_file = "$State{workdir}/qgrid_resamp_$<_$$.grd";

	my( $wlimit, $elimit, $slimit, $nlimit );
	if( $Mapspec{InclusiveRectangle} =~ 
		 m@-R([-\.\d]+)/([-\.\d]+)/([-\.\d]+)/([-\.\d]+)@ ) {
		$wlimit = $1;
		$elimit = $2;
		$slimit = $3;
		$nlimit = $4;
	}

	my( $tile ) = "-R$wlimit/$elimit/$slimit/$nlimit";

	$wlimit_normal = normal_lon( $wlimit );
	$elimit_normal = normal_lon( $elimit );
	my( $tile_normal ) = "-R$wlimit_normal/$elimit_normal/$slimit/$nlimit";

	if( $opt_v ) {
		elog_notify "Running dbgmtgrid for $tile, output=$grdfile\n";
		$extractverbose = 1;
	} else {
		$extractverbose = 0;
	}

	@spacing_option = ();

	if( defined( $Mapspec{topo_resolution} ) && 
		     $Mapspec{topo_resolution} ne "" ) {

		@spacing_option = ( "spacing" => "$Mapspec{topo_resolution}" );
	}

	my( $rc ) = dbgmtgrid( @dbgrid, $tile, $grdfile,
			       verbose => $extractverbose,
			       workdir => $State{workdir},
			       @spacing_option );

	if( $rc < 0 ) {
		elog_complain
		"\n\t************************************\n" . 
		"\tWARNING: dbgmtgrid() failed for '$tile'\n" .
		"\t************************************\n\n";
		return;
	}
	dbclose( @dbgrid );

	$cmd = "grdgradient $grdfile -G$gradfile $V $Mapspec{grdgradient_opt}";

	if( $opt_v ) {
		elog_notify "$cmd\n";
	}
	system_scriptlog( $cmd );

	my( $cmd ) = "cggrid_convert $Mapspec{qgridfile} | " .
		     "xyz2grd $V -H1 -N0 " .
		     "-I$Mapspec{qgrid_dlon}/$Mapspec{qgrid_dlat} " .
		     "-G$gmt_qgrid_file " .
		     "$tile_normal";

	if( $opt_v ) {
		elog_notify "$cmd\n";
	}
	system_scriptlog( $cmd );

	my( $cmd ) = "grdedit $V $gmt_qgrid_file $tile";

	if( $opt_v ) {
		elog_notify "$cmd\n";
	}
	system_scriptlog( $cmd );

	my( $cmd ) = "grdsample $V -F $gmt_qgrid_file " .
		     "-G$gmt_qgridresamp_file " .
		     "$tile -I$dx/$dy";

	if( $opt_v ) {
		elog_notify "$cmd\n";
	}
	system_scriptlog( $cmd );

	$cmd = "grdview $V -P -Qi50 -JZ0.5i " .
	      	"$Mapspec{Rectangle} $Mapspec{Projection} " .
		"$grdfile " .
		"-I$gradfile " .
		"-G$gmt_qgridresamp_file " .
		"-C$Mapspec{drape_color_palette_file} " .
		$more .
		"$redirect $Mapspec{psfile}";

	if( $opt_v ) {
		elog_notify "$cmd\n";
	}
	system_scriptlog( $cmd );
}

sub plot_contours {
	my( %Mapspec ) = %{shift( @_ )};
	my( $position ) = shift( @_ );

	if( $position ne "middle" ) {
		elog_complain "Warning: contours only implemented for " .
		 "middle-position plotting\n";
	}

	my( $more, $redirect ) = more_ps( $position );

	my( $cmd );

	if( $Mapspec{contour_mode} eq "none" ) {

		$cmd = "pscoast $V -P " .
			"$Mapspec{Rectangle} $Mapspec{Projection} " .
			"-C200/200/255 -S200/200/255 -G255/243/230 " .
			"-D$Mapspec{detail_density} " .
			$more .
			"$redirect $Mapspec{psfile}";
		if( $opt_v ) {
			elog_notify "$cmd\n";
		}
		system_scriptlog( $cmd );

	} elsif( ( $Mapspec{contour_mode} eq "grddb" ) &&
	    ( ! -e "$Mapspec{grddb}" ) ) {

		elog_complain
			"\n\t************************************\n" . 
			"\tWARNING: Setting contour_mode to \"none\":\n" .
			"\tgrddb '$Mapspec{grddb}' not found\n" .
			"\t************************************\n\n";

		$cmd = "pscoast $V -P " .
			"$Mapspec{Rectangle} $Mapspec{Projection} " .
			"-C200/200/255 -S200/200/255 -G255/243/230 " .
			"-D$Mapspec{detail_density} " .
			$more .
			"$redirect $Mapspec{psfile}";
		if( $opt_v ) {
			elog_notify "$cmd\n";
		}
		system_scriptlog( $cmd );

	} elsif( $Mapspec{contour_mode} eq "grddb" ) {

		my( @dbgrid ) = dbopen( "$Mapspec{grddb}", "r" );
		if( $dbgrid[0] < 0 ) {

		 	die( 
		 	"\n\t************************************\n" . 
		 	"\tERROR: Failed to open grddb $Mapspec{grddb}\n" .
		 	"\t************************************\n\n" );

		} 

		@dbgrid = dblookup( @dbgrid, "", "grids", "", "" );
		if( $dbgrid[1] < 0 ) {
			die( 
			"\n\t************************************\n" . 
			"\tERROR: Failed to open grids table of grddb $Mapspec{grddb}\n" .
			"\t************************************\n\n" );

		} 

		my( $wlimit, $elimit, $slimit, $nlimit );
		if( $Mapspec{InclusiveRectangle} =~ 
			 m@-R([-\.\d]+)/([-\.\d]+)/([-\.\d]+)/([-\.\d]+)@ ) {
			$wlimit = $1;
			$elimit = $2;
			$slimit = $3;
			$nlimit = $4;
		}

		my( $w, $e, $s, $n, $nextsmin, $nextwmin );

		$nexsmin = $nextwmin = -9999;

		my( $tn ) = 0; # tile-number

		for( $s = $slimit; 
		      $s<$nlimit; 
		       $s=$s+$Mapspec{tilesize_deg}<$nexsmin?$nextsmin:$s+$Mapspec{tilesize_deg} ) {

		  # Put the potentially ugly sutures under the grid lines:

		  $n = $s + $Mapspec{tilesize_deg};
		  $n = next_round( $n, $Mapspec{gridline_interval_deg} );
		  $n > $nlimit ? $nlimit : $n;
		  $nextsmin = $n;

		  for( $w = $wlimit; 
			$w<$elimit; 
			 $w=$w+$Mapspec{tilesize_deg}<$nextwmin?$nextwmin:$w+$Mapspec{tilesize_deg} ) {

		    $e = $w + $Mapspec{tilesize_deg};
		    $e = next_round( $e, $Mapspec{gridline_interval_deg} );
		    $e > $elimit ? $elimit : $e;
		    $nextwmin = $e;

		    $grdfile = "$State{workdir}/grd_$<_$$_$tn.grd";
		    $gradfile = "$State{workdir}/grad_$<_$$_$tn.grad";
		    $psclipfile = "$State{workdir}/psclip_$<_$$_$tn.clip";
		    $tn++;

		    my( $tile ) = "-R$w/$e/$s/$n";

		    if( $opt_v ) {
			elog_notify "Running dbgmtgrid for $tile, output=$grdfile\n";
			$extractverbose = 1;
		    } else {
			$extractverbose = 0;
		    }

		    @spacing_option = ();

		    if( defined( $Mapspec{topo_resolution} ) && 
		    	$Mapspec{topo_resolution} ne "" ) {

			@spacing_option = ( "spacing" => "$Mapspec{topo_resolution}" );
		    }

		    my( $rc ) = dbgmtgrid( @dbgrid, $tile,
				           $grdfile,
					   verbose => $extractverbose,
			       		   workdir => $State{workdir},
					   @spacing_option );
		    if( $rc < 0 ) {
			elog_complain
			"\n\t************************************\n" . 
			"\tWARNING: dbgmtgrid() failed for tile '$tile'\n" .
			"\t************************************\n\n";
			next;
		    }

		    $cmd = "grdgradient $grdfile -G$gradfile $V $Mapspec{grdgradient_opt}";
		    if( $opt_v ) {
			elog_notify "$cmd\n";
		    }
		    system_scriptlog( $cmd );

		    open( C, ">$psclipfile" );
		    my( $ewincr ) = ($e - $w) / 100;
		    my( $nsincr ) = ($n - $s) / 100;
		    for( $cliplon=$w; 
			  $cliplon<=$e; 
			   $cliplon += $ewincr ) { print C "$cliplon $s\n"; }
		    for( $cliplat=$s+$nsincr; 
			  $cliplat<$n; 
			   $cliplat += $nsincr ) { print C "$e $cliplat\n"; }
		    for( $cliplon=$e; 
			  $cliplon>=$w; 
			   $cliplon -= $ewincr ) { print C "$cliplon $n\n"; }
		    for( $cliplat=$n-$nsincr; 
			  $cliplat>=$s; 
			   $cliplat -= $nsincr ) { print C "$w $cliplat\n"; }
		    close( C );

		    $cmd = "psclip $V $psclipfile " .
			   "$Mapspec{Rectangle} $Mapspec{Projection} " .
			   $more .
			   "$redirect $Mapspec{psfile}";

		    if( $opt_v ) {
			elog_notify "$cmd\n";
		    }
		    system_scriptlog( $cmd );

		    $cmd = "grdimage $V -P " .
		      	"$Mapspec{Rectangle} $Mapspec{Projection} " .
			"$grdfile " .
			"-I$gradfile " .
			"-C$Mapspec{map_color_palette_file} " .
			$more .
			"$redirect $Mapspec{psfile}";

		    if( $opt_v ) {
			elog_notify "$cmd\n";
		    }
		    system_scriptlog( $cmd );

		    $cmd = "psclip $V -C " .
			   $more .
			   "$redirect $Mapspec{psfile}";

		    if( $opt_v ) {
			elog_notify "$cmd\n";
		    }
		    system_scriptlog( $cmd );

		    if( $dbrecenteqs::scriptlog eq "" ) {
		    	unlink( $grdfile );
		    	unlink( $gradfile );
		    	unlink( $psclipfile );
		    }
		  }
		}

		foreach $maskarea ( keys( %{$Mapspec{landmask_regions}} ) ) {

		    my( $maskregion );

		    ( $maskregion, $w, $e, $s, $n ) = find_overlap( 
				$Mapspec{landmask_regions}{$maskarea},
				$Mapspec{InclusiveRectangle} );

		    next unless defined( $maskregion );

		    $grdfile = "$State{workdir}/grd_$<_$$_$tn.grd";
		    $gradfile = "$State{workdir}/grad_$<_$$_$tn.grad";
		    $psclipfile = "$State{workdir}/psclip_$<_$$_$tn.clip";
		    $tn++;

		    if( $opt_v ) {
			elog_notify "Running dbgmtgrid for Land mask $maskregion, output=$grdfile\n";
			$extractverbose = 1;
		    } else {
			$extractverbose = 0;
		    }

		    my( $rc ) = dbgmtgrid( @dbgrid, $maskregion,
				           $grdfile, verbose => $extractverbose );
		    if( $rc < 0 ) {
			elog_complain
			"\n\t************************************\n" . 
			"\tWARNING: dbgmtgrid() failed for maskregion '$maskregion'\n" .
			"\t************************************\n\n";
			next;
		    }

		    $cmd = "grdgradient $grdfile -G$gradfile $V $Mapspec{grdgradient_opt}";
		    if( $opt_v ) {
			elog_notify "$cmd\n";
		    }
		    system_scriptlog( $cmd );

		    open( C, ">$psclipfile" );
		    my( $ewincr ) = ($e - $w) / 100;
		    my( $nsincr ) = ($n - $s) / 100;
		    for( $cliplon=$w; 
			  $cliplon<=$e; 
			   $cliplon += $ewincr ) { print C "$cliplon $s\n"; }
		    for( $cliplat=$s+$nsincr; 
			  $cliplat<$n; 
			   $cliplat += $nsincr ) { print C "$e $cliplat\n"; }
		    for( $cliplon=$e; 
			  $cliplon>=$w; 
			   $cliplon -= $ewincr ) { print C "$cliplon $n\n"; }
		    for( $cliplat=$n-$nsincr; 
			  $cliplat>=$s; 
			   $cliplat -= $nsincr ) { print C "$w $cliplat\n"; }
		    close( C );

		    $cmd = "psclip $V $psclipfile " .
			   "$Mapspec{Rectangle} $Mapspec{Projection} " .
			   $more .
			   "$redirect $Mapspec{psfile}";

		    if( $opt_v ) {
			elog_notify "$cmd\n";
		    }
		    system_scriptlog( $cmd );

		    $cmd = "grdimage $V -P " .
		      	"$Mapspec{Rectangle} $Mapspec{Projection} " .
			"$grdfile " .
			"-I$gradfile " .
			"-C$Mapspec{map_landmask_palette_file} " .
			$more .
			"$redirect $Mapspec{psfile}";

		    if( $opt_v ) {
			elog_notify "$cmd\n";
		    }
		    system_scriptlog( $cmd );

		    $cmd = "psclip $V -C " .
			   $more .
			   "$redirect $Mapspec{psfile}";

		    if( $opt_v ) {
			elog_notify "$cmd\n";
		    }
		    system_scriptlog( $cmd );

		    if( $dbrecenteqs::scriptlog eq "" ) {
		    	unlink( $grdfile );
		    	unlink( $gradfile );
		    	unlink( $psclipfile );
		    }
		}

		dbclose( @dbgrid );

	} else {

		die( "contour_mode $Mapspec{contour_mode} not supported\n" );
	}
}

sub find_overlap {
	my( $maskarea, $wholeregion ) = @_;

	my( $wregion, $eregion, $sregion, $nregion );
	my( $wmask, $emask, $smask, $nmask );
	my( $wneed, $eneed, $sneed, $nneed );

	if( $wholeregion =~ m@-R([-\.\d]+)/([-\.\d]+)/([-\.\d]+)/([-\.\d]+)@ ) {
			$wregion = $1;
			$eregion = $2;
			$sregion = $3;
			$nregion = $4;
	}

	$wregion = unwrapped_lon( \%Mapspec, $wregion );
	$eregion = unwrapped_lon( \%Mapspec, $eregion );

	if( $maskarea =~ m@-R([-\.\d]+)/([-\.\d]+)/([-\.\d]+)/([-\.\d]+)@ ) {
			$wmask = $1;
			$emask = $2;
			$smask = $3;
			$nmask = $4;
	}

	$wmask = unwrapped_lon( \%Mapspec, $wmask );
	$emask = unwrapped_lon( \%Mapspec, $emask );

	if( $wmask <= $wregion && $eregion <= $emask ) {

		$wneed = $wregion;
		$eneed = $eregion;

	} elsif( $wregion <= $wmask && $emask <= $eregion ) {

		$wneed = $wmask;
		$eneed = $emask;

	} elsif( $wmask <= $wregion && $wregion < $emask ) {

		$wneed = $wregion;
		$eneed = $emask;

	} elsif( $wmask < $eregion && $eregion <= $emask ) {

		$wneed = $wmask;
		$eneed = $eregion;

	} else {

		return undef;
	}

	if( $smask <= $sregion && $nregion <= $nmask ) {

		$sneed = $sregion;
		$nneed = $nregion;

	} elsif( $sregion <= $smask && $nmask <= $nregion ) {

		$sneed = $smask;
		$nneed = $nmask;

	} elsif( $smask <= $sregion && $sregion < $nmask ) {

		$sneed = $sregion;
		$nneed = $smask;

	} elsif( $smask < $nregion && $nregion <= $nmask ) {

		$sneed = $smask;
		$nneed = $nregion;

	} else {

		return undef;
	}

	return ( "-R$wneed/$eneed/$sneed/$nneed", 
		 $wneed, $eneed, $sneed, $nneed );
}

sub plot_coastlines {
	my( %Mapspec ) = %{shift( @_ )};
	my( $position ) = shift( @_ );

	my( $more, $redirect ) = more_ps( $position );

	my( $cmd ) = "pscoast $V -P " .
		     "$Mapspec{Rectangle} $Mapspec{Projection} " .
		     "-W2/0/0/0 " .
		     "-D$Mapspec{detail_density} " .
		     $more .
		     "$redirect $Mapspec{psfile}";
			
	if( $opt_v ) {
		elog_notify "$cmd\n";
	}
	system_scriptlog( $cmd );
}

sub plot_oceans {
	my( %Mapspec ) = %{shift( @_ )};
	my( $position ) = shift( @_ );

	my( $more, $redirect ) = more_ps( $position );

	my( $cmd ) = "pscoast $V -P -S0/0/200 " .
		     "$Mapspec{Rectangle} $Mapspec{Projection} " .
		     "-D$Mapspec{detail_density} " .
		     $more .
		     "$redirect $Mapspec{psfile}";
			
	if( $opt_v ) {
		elog_notify "$cmd\n";
	}
	system_scriptlog( $cmd );
}

sub datafile_abspath {
	my( $file ) = shift( @_ );

	if( ! defined( $file ) ) {

		return undef;

	} elsif( $file eq "" ) {

		return "";
	}

	if( $file !~ m@^\.?/@ ) {

		$file = "$ENV{ANTELOPE}/data/www/dbrecenteqs/" . $file;
	}

	return $file;
}

sub plot_linefiles {
	my( %Mapspec ) = %{shift( @_ )};
	my( $position ) = shift( @_ );

	if( $position ne "middle" ) {
		elog_complain "Warning: linefiles only implemented for " .
		 "middle-position plotting\n";
	}

	my( $more, $redirect ) = more_ps( $position );

	my( $name, $file, $pen, $cmd );

	foreach $line ( @{$Mapspec{linefiles}} ) {

		next if( $line =~ /^\s*$/ );

		( $name, $file, $pen ) = split( /\s+/, $line );

		$file = datafile_abspath( $file );

		if( ! -e $file ) {

			elog_complain
				"\n\t************************************\n" . 
				"\tWARNING: Couldn't find linefile " .
			     	"$file -- skipping\n" .
				"\t************************************\n\n";
			next;

		} elsif( ! defined( $pen ) || $pen eq "" ) {

			elog_complain
				"\n\t************************************\n" . 
				"\tWARNING: No pen for linefile " .
			     	"$file -- default to black\n" .
				"\t************************************\n\n";

			$pen = "4/0/0/0";
		}

		$cmd = "psxy $V -P " .
			"$Mapspec{Rectangle} $Mapspec{Projection} " .
			"$file -M -W$pen " .	
			$more .
			"$redirect $Mapspec{psfile}";
			
		if( $opt_v ) {
			elog_notify "plotting $name:\n$cmd\n";
		}
		system_scriptlog( $cmd );
	}
}

sub plot_stations {
	my( %Mapspec ) = %{shift( @_ )};
	my( $position ) = shift( @_ );

	if( ! defined( $Mapspec{stations_dbname} ) || 
	      $Mapspec{stations_dbname} eq "" ) {
		elog_complain
			"\n\t************************************\n" . 
			"\tWARNING: Skipping stations--" .
			"no stations_dbname specified\n" .
			"\t************************************\n\n";
		return;

	} else {
		
		my( @dbtest ) = dbopen( "$Mapspec{stations_dbname}", "r" );
		@dbtest = dblookup( @dbtest, "", "site", "", "" );
		my( $test_dbnstas ) = dbquery( @dbtest, dbRECORD_COUNT );
		dbclose( @dbtest );

		if( $test_dbnstas <= 0 ) {

			elog_complain
			"\n\t************************************\n" . 
			"\tWARNING: Skipping stations--" .
			"site table in $Mapspec{stations_dbname} " .
			"not found or empty\n" .
			"\t************************************\n\n";

			return;
		}
	}

	my ( $stas_tempfile, $stanames_tempfile,
	     $focus_stas_tempfile, $focus_stanames_tempfile ) =
		make_stations_tempfiles( \%Mapspec );

	my( $more, $redirect );
	
	if( $position eq "first" || $position eq "single" ) {
		( $more, $redirect ) = more_ps( "first" );
	} else {
		( $more, $redirect ) = more_ps( "middle" );
	}

	my( $sta_color ) = $Mapspec{sta_color};
	my( $sta_border_color ) = $Mapspec{sta_border_color};
	my( $sta_symbols_inches ) = $Mapspec{sta_symbols_inches};

	my( $cmd ) = "cat $stas_tempfile | psxy $V -P " .
			"-G$sta_color -W$sta_border_color " .
			"$Mapspec{Rectangle} $Mapspec{Projection} " .
			"-St${sta_symbols_inches}i " .
			$more .
			"$redirect $Mapspec{psfile}";
			
	if( $opt_v ) {
		elog_notify "$cmd\n";
	}
	system_scriptlog( $cmd );

	( $more, $redirect ) = more_ps( "middle" );

	my( $cmd ) = "cat $stanames_tempfile | pstext $V -P " .
			"-G$sta_color " .
			"$Mapspec{Rectangle} $Mapspec{Projection} " .
			$more .
			"$redirect $Mapspec{psfile}";

	if( $opt_v ) {
		elog_notify "$cmd\n";
	}
	system_scriptlog( $cmd );

	my( $sta_color ) = $Mapspec{focus_sta_color};
	my( $sta_border_color ) = $Mapspec{focus_sta_border_color};
	my( $sta_symbols_inches ) = $Mapspec{focus_sta_symbols_inches};

	my( $cmd ) = "cat $focus_stas_tempfile | psxy $V -P " .
			"-G$sta_color -W$sta_border_color " .
			"$Mapspec{Rectangle} $Mapspec{Projection} " .
			"-St${sta_symbols_inches}i " .
			$more .
			"$redirect $Mapspec{psfile}";
			
	if( $opt_v ) {
		elog_notify "$cmd\n";
	}
	system_scriptlog( $cmd );

	if( $position eq "single" ) {
		( $more, $redirect ) = more_ps( "last" );
	} else {
		( $more, $redirect ) = more_ps( "middle" );
	}

	my( $cmd ) = "cat $focus_stanames_tempfile | pstext $V -P " .
			"-G$sta_color " .
			"$Mapspec{Rectangle} $Mapspec{Projection} " .
			$more .
			"$redirect $Mapspec{psfile}";

	if( $opt_v ) {
		elog_notify "$cmd\n";
	}
	system_scriptlog( $cmd );

	if( $dbrecenteqs::scriptlog eq "" ) {

		unlink( $stas_tempfile );
		unlink( $stanames_tempfile );
		unlink( $focus_stas_tempfile );
		unlink( $focus_stanames_tempfile );
	}
}

sub plot_cities {
	my( %Mapspec ) = %{shift( @_ )};
	my( $position ) = shift( @_ );

	if( ! defined( $Mapspec{cities_dbname} ) || 
	      $Mapspec{cities_dbname} eq "" ) {
		elog_complain
			"\n\t************************************\n" . 
			"\tWARNING: Skipping cities--" .
			"no cities_dbname specified\n" .
			"\t************************************\n\n";
		return;

	} elsif( ! -e "$Mapspec{cities_dbname}.places" ) {

		elog_complain
			"\n\t************************************\n" . 
			"\tWARNING: Skipping cities--" .
			"$Mapspec{cities_dbname}.places not found\n" .
			"\t************************************\n\n";
		return;
	}

	my ( $locs_tempfile, $names_tempfile ) =
		make_cities_tempfiles( \%Mapspec );

	my( $more, $redirect );
	
	if( $position eq "first" || $position eq "single" ) {
		( $more, $redirect ) = more_ps( "first" );
	} else {
		( $more, $redirect ) = more_ps( "middle" );
	}

	my( $cmd ) = "cat $locs_tempfile | psxy $V -P " .
			"$Mapspec{Rectangle} $Mapspec{Projection} " .
			"-Ss$Mapspec{city_symbols_inches}i -G0 " .
			$more .
			"$redirect $Mapspec{psfile}";
			
	if( $opt_v ) {
		elog_notify "$cmd\n";
	}
	system_scriptlog( $cmd );

	if( $position eq "first" ) {
		( $more, $redirect ) = more_ps( "middle" );
	} elsif( $position eq "single" ) {
		( $more, $redirect ) = more_ps( "last" );
	} else {
		( $more, $redirect ) = more_ps( $position );
	}

	my( $cmd ) = "cat $names_tempfile | pstext $V -P " .
			"$Mapspec{Rectangle} $Mapspec{Projection} " .
			$more .
			"$redirect $Mapspec{psfile}";

	if( $opt_v ) {
		elog_notify "$cmd\n";
	}
	system_scriptlog( $cmd );

	if( $dbrecenteqs::scriptlog eq "" ) {

		unlink( $locs_tempfile );
		unlink( $names_tempfile );
	}
}

sub plot_template {
	my( %Mapspec ) = %{shift( @_ )};
	my( $position ) = shift( @_ );

	my( $more, $redirect ) = more_ps( $position );

	my( $cmd ) = "";
			
	if( $opt_v ) {
		elog_notify "$cmd\n";
	}
	system_scriptlog( $cmd );
}

sub create_map {
	my( %Mapspec ) = %{shift( @_ )};

	unlink( "$Mapspec{psfile}" );

	if( ( "$Mapspec{mapclass}" ne "focus" ) &&
	    ( -e "$Mapspec{pixfile}" || -e "$Mapspec{pixfile}.pf") ) {

		die( "dbrecenteqs: The files\n\n\t$Mapspec{pixfile}\nand/or\n\t" .
		     "$Mapspec{pixfile}.pf\n\nalready exist. Will not " .
		     "overwrite, Bye.\n" );
	}

	my( $datadisplay_mode );

	if( ! $State{use_qgrids} ||  
	    ! defined( $Mapspec{qgrid_nintervals} ) || 
	    $Mapspec{qgrid_nintervals} > 0 ) {

		$datadisplay_mode = "contour";

	} else {
		
		$datadisplay_mode = "shading";
	}

	if( $datadisplay_mode eq "contour" ) {

		plot_basemap( \%Mapspec, "first" );
		plot_contours( \%Mapspec, "middle" );
		plot_coastlines( \%Mapspec, "middle" );
		plot_lakes( \%Mapspec, "middle" );
		plot_rivers( \%Mapspec, "middle" );

		if( $Mapspec{plot_political_boundaries} ) {

			plot_national_boundaries( \%Mapspec, "middle" );
			plot_state_boundaries( \%Mapspec, "middle" );
		}

		plot_hypocenters( \%Mapspec, "middle" );
		plot_linefiles( \%Mapspec, "middle" );
		plot_basemap( \%Mapspec, "middle" );
		if( $State{use_qgrids} &&
	    	defined( $Mapspec{qgridfile} ) && 
	    	-e "$Mapspec{qgridfile}" ) {
			
			plot_qgrid( \%Mapspec, "middle" );
		}
		plot_cities( \%Mapspec, "last" );

	} else {

		plot_basemap( \%Mapspec, "first" );
		plot_drape( \%Mapspec, "middle" );
		plot_oceans( \%Mapspec, "middle" );

		if( $Mapspec{plot_political_boundaries} ) {

			plot_national_boundaries( \%Mapspec, "middle" );
			plot_state_boundaries( \%Mapspec, "middle" );
		}

		plot_cities( \%Mapspec, "last" );
	}

	%Mapspec = %{pixfile_convert( \%Mapspec )};
	write_pixfile_pffile( \%Mapspec );

	if( ( "$Mapspec{mapclass}" eq "index" ) ) {

		system( "cp $Mapspec{psfile} $State{stockmaps_location}" );
	}

	unlink( "$Mapspec{psfile}" );

	return \%Mapspec;
}

sub set_map_width {
	my( %Mapspec ) = %{shift( @_ )};

	$Mapspec{width} = $Mapspec{clean_image}->Get( 'width' );
	$Mapspec{height} = $Mapspec{clean_image}->Get( 'height' );

	return \%Mapspec;
}

sub set_map_scaling {
	my( %Mapspec ) = %{shift( @_ )};

	my( $xrange ) = $Mapspec{right_dellon} - $Mapspec{left_dellon};
	my( $yrange ) = $Mapspec{up_dellat} - $Mapspec{down_dellat};

	$Mapspec{xc} = $Mapspec{width} * abs( $Mapspec{left_dellon} ) / $xrange;
	$Mapspec{yc} = $Mapspec{height} * abs( $Mapspec{up_dellat} ) / $yrange;

	$Mapspec{xscale_pixperdeg} = $Mapspec{width} / $xrange;
	$Mapspec{yscale_pixperdeg} = $Mapspec{height} / $yrange;

	return \%Mapspec;
}

sub read_map_from_db {
	my( @db ) = @_;
	my( %Mapspec );

	my( $mapname ) = dbgetv( @db, "mapname" );

	%Mapspec = %{$State{maphashes}->{$mapname}};

	$Mapspec{pixfile} = dbextfile( @db );

	if( ! -e "$Mapspec{pixfile}" ) {
		die( "\n\t************************************\n" . 
		     "\tERROR: the file '$Mapspec{pixfile}' has disappeared!\n" . 
		     "\t************************************\n\nBye.\n\n" );
	}

	( $Mapspec{mapname},
	  $Mapspec{mapclass},
	  $Mapspec{latc},
	  $Mapspec{lonc},
	  $Mapspec{up_dellat},
	  $Mapspec{down_dellat},
	  $Mapspec{left_dellon},
	  $Mapspec{right_dellon},
	  $Mapspec{proj},
	  $Mapspec{format},
	  $Mapspec{width},
	  $Mapspec{height},
	  $Mapspec{xc},
	  $Mapspec{yc},
	  $Mapspec{xscale_pixperdeg},
	  $Mapspec{yscale_pixperdeg} )  =

		dbgetv( @db, 
			"mapname",
			"mapclass",
			"latc",
			"lonc",
			"updellat",
			"downdellat",
			"leftdellon",
			"rightdellon",
			"proj",
			"format",
			"width",
			"height",
			"xc",
			"yc",
			"xpixperdeg",
			"ypixperdeg" );

	$Mapspec{clean_image} = Image::Magick->new();
	$Mapspec{clean_image}->Read( $Mapspec{pixfile} );
	
	return \%Mapspec;
}

sub read_map_from_file {
	my( %Mapspec ) = %{ shift( @_ )};

	my( $map_pathname ) = $Mapspec{source};

	$map_pathname =~ s/\.pf$//;

	if( $opt_v ) {
		elog_notify "Re-reading index map $Mapspec{mapname} from file...\n";
	}

	if( "$map_pathname" =~ m@^\.?/@ && -e "$map_pathname" ) {

		; # All set

	} elsif( -e "$ENV{ANTELOPE}/data/dbrecenteqs/" . "$map_pathname" ) {

		$map_pathname = "$ENV{ANTELOPE}/data/dbrecenteqs/" . 
				"$map_pathname";

	} elsif( -e "$ENV{ANTELOPE}/data/maps/images/" . "$map_pathname" ) {

		$map_pathname = "$ENV{ANTELOPE}/data/maps/images/" . 
				"$map_pathname";

	} else {

		die( "Can't find map file \"$map_pathname\"\n" );
	}


	if( ! -e "$map_pathname.pf" ) {

		die( "Can't find map parameter file $map_pathname.pf\n" );
	}

	my( $mapbase ) = `basename $map_pathname`;
	chomp( $mapbase );
	$mapbase =~ s/\..*$//; # remove suffix extension

	$Mapspec{file_basename} = $mapbase;

	my( $hashref ) = pfget( $map_pathname, $Mapspec{file_basename} );

	if( ! defined( $hashref ) ) {
		die( "Didn't find map specifications for map " .
		     "name $Mapspec{mapname} in $map_pathname. Bye.\n" );
	}

	foreach $key ( keys( %$hashref ) ) {
		$Mapspec{$key} = $hashref->{$key};
	}

	$Mapspec{pixfile} = $map_pathname;

	$Mapspec{longitude_branchcut_high} = 180;
	$Mapspec{longitude_branchcut_low} = -180;

	$Mapspec{up_dellat} = delete( $Mapspec{ydelmax} );
	$Mapspec{down_dellat} = delete( $Mapspec{ydelmin} );
	$Mapspec{left_dellon} = delete( $Mapspec{xdelmin} );
	$Mapspec{right_dellon} = delete( $Mapspec{xdelmax} );
	
	$Mapspec{clean_image} = Image::Magick->new();
	$Mapspec{clean_image}->Read( $Mapspec{pixfile} );
	
	%Mapspec = %{set_map_width( \%Mapspec )};
	%Mapspec = %{set_map_scaling( \%Mapspec )};

	return \%Mapspec;
}

sub pixfile_convert {
	my( %Mapspec ) = %{shift( @_ )};
	my( $cmd );

	my( $size_pixels ) = 
		$Mapspec{size_inches} * $Mapspec{pixels_per_inch};

	if( $State{pixfile_conversion_method} eq "alchemy" ) {

		my( $format );
		if( $Mapspec{format} eq "gif" ) {
			$format = "-g";
		} elsif( $Mapspec{format} eq "jpg" ) {
			$format = "-j";
		} else {
			die( "format $Mapspec{format} not supported--bye!\n" );
		} 

		if( ! check_for_executable( "alchemy" ) ) {
			die( "Couldn't find alchemy in path. Use alternate " .
				"image-conversion method or fix path.\n" );
		}

		$cmd = "alchemy -Zm4 -Zc1 -o $format " .
			"$Mapspec{psfile} $Mapspec{pixfile} " .
			"-c 256 $Mapspec{reserve_colors} " .
			"-Xd$size_pixels\p -+";

	} elsif( $State{pixfile_conversion_method} eq "pnm" ) {

		my( $converter );
		if( $Mapspec{format} eq "gif" ) {
			$converter = "ppmtogif";
		} elsif( $Mapspec{format} eq "jpg" ) {
			$converter = "pnmtojpeg";
		} else {
			die( "format $Mapspec{format} not supported--bye!\n" );
		} 

		if( $Mapspec{format} eq "jpg" ) {
			die( "jpg incompatible with pnm conversion\n" );
		}

		my( @helpers ) = ( "gs", "pnmcrop", "ppmquant", "$converter" );
		foreach $helper ( @helpers ) {
			next if check_for_executable( $helper );
			die( "Couldn't find $helper in path. Fix path or " .
			     "don't use image conversion method \"pnm\".\n" );
		}

		my( $ncolors ) = 256 - $Mapspec{reserve_colors};

		$cmd = "cat $Mapspec{psfile} | gs -sOutputFile=- -q " .
		       "-sDEVICE=ppm -r$Mapspec{pixels_per_inch} " .
		       "-g$size_pixels\\x$size_pixels - | pnmcrop - " .
		       "| ppmquant $ncolors | $converter > $Mapspec{pixfile}";

	} elsif( $State{pixfile_conversion_method} eq "imagick" ) {

		if( ! check_for_executable( "convert" ) ) {
			die( "Couldn't find 'convert' in path. Fix path or " .
			   "don't use image conversion method \"imagick\".\n" );
		}

		my( $ncolors ) = 256 - $Mapspec{reserve_colors};

		$cmd = "convert -trim +repage " .
			"-density $Mapspec{pixels_per_inch}x$Mapspec{pixels_per_inch} " .
		       "-resize ${size_pixels}x${size_pixels} -colors $ncolors " .
		       "$Mapspec{psfile} $Mapspec{pixfile}"; 

	} elsif( $State{pixfile_conversion_method} eq "none" ) {

		if( $opt_v ) {

			elog_notify( "Skipping pixfile conversion at user request\n" );
		}

	} else {

		 die( "pixfile_conversion_method " . 
		     "$State{pixfile_conversion_method} " .
		     "not supported." );
	}

	if( $State{pixfile_conversion_method} ne "none" ) {

		if( $opt_v ) {
			elog_notify "$cmd\n";
		}
		system_scriptlog( $cmd );

		if( defined( $Image::Magick::VERSION ) ) {
			$Mapspec{clean_image} = Image::Magick->new();
			$Mapspec{clean_image}->Read( $Mapspec{pixfile} );
	
			%Mapspec = %{set_map_width( \%Mapspec )};
			%Mapspec = %{set_map_scaling( \%Mapspec )};
		}
	}

	return \%Mapspec;
}

sub write_pixfile_pffile {
	my( %Mapspec ) = %{shift( @_ )};

	my( $normal_lonc ) = normal_lon( $Mapspec{lonc} );

	if( ! -e "$Mapspec{pixfile}" ) {

		elog_complain "Won't write $Mapspec{pixfile}.pf; " .
			     "$Mapspec{pixfile} does not exist\n";
		return;
	}

	open( P, ">$Mapspec{pixfile}.pf" );

	print P "$Mapspec{file_basename} &Arr{\n";
	print P "\tfile $Mapspec{pixfile}\n";
	print P "\tformat $Mapspec{format}\n";
	print P "\tproj $Mapspec{proj}\n";
	print P "\tlatc $Mapspec{latc}\n";
	print P "\tlonc $normal_lonc\n";
	print P "\txdelmin $Mapspec{left_dellon}\n";
	print P "\txdelmax $Mapspec{right_dellon}\n";
	print P "\tydelmin $Mapspec{down_dellat}\n";
	print P "\tydelmax $Mapspec{up_dellat}\n";
	print P "}\n";

	close( P );
}

sub add_to_mapstock {
	my( %Mapspec ) = %{ shift( @_ )};
	my( @db ) = @_;

	my( $abspath ) = abspath( $Mapspec{pixfile} );

	if( ! -e "$abspath" ) {
		die( "\n\t************************************\n" . 
		     "\tERROR: file '$abspath' does not exist!\n" . 
		     "\t************************************\n\nBye.\n\n" );
	}

	my( $dir ) = `dirname $abspath`;
	my( $dfile ) = `basename $abspath`;
	chomp( $dir );
	chomp( $dfile );

	@db = dblookup( @db, "", "mapstock", "", "" );

	if( ! dbquery( @db, "dbTABLE_IS_WRITEABLE" ) ) {

		die( "Table mapstock is not writeable\n" );
	}

	check_dir_dfile( @db, $dir, $dfile );

	dbaddv( @db, "mapname", $Mapspec{mapname},
	     "proj", $Mapspec{proj},
	     "mapclass", $Mapspec{mapclass},
	     "format", $Mapspec{format},
	     "latc", $Mapspec{latc},
	     "lonc", normal_lon( $Mapspec{lonc} ),
	     "updellat", $Mapspec{up_dellat},
	     "downdellat", $Mapspec{down_dellat},
	     "leftdellon", $Mapspec{left_dellon},
	     "rightdellon", $Mapspec{right_dellon},
	     "width", $Mapspec{width},
	     "height", $Mapspec{height},
	     "xc", $Mapspec{xc},
	     "yc", $Mapspec{yc},
	     "xpixperdeg", $Mapspec{xscale_pixperdeg},
	     "ypixperdeg", $Mapspec{yscale_pixperdeg},
	     "dir", $dir,
	     "dfile", $dfile
	     );
}

sub setup_index_Mapspec {
	my( %Mapspec ) = %{ shift( @_ )};

	if( $opt_v ) {
		elog_notify "Re-generating index map $Mapspec{mapname} dynamically...\n";
	}
	
	$Mapspec{"file_basename"} = $Mapspec{"mapname"};

	$Mapspec{"lonc"} = unwrapped_lon( \%Mapspec, $Mapspec{"lonc"} );
	
	$Mapspec{"psfile"} = concatpaths( $State{"workdir"},
				"$Mapspec{file_basename}.ps" );

	if( makedir( $State{"stockmaps_location"} ) < 0 ) {

		die( "dbrecenteqs: Failed to make directory $State{stockmaps_location}. Bye.\n" );
	}

	$Mapspec{"pixfile"} = concatpaths( $State{"stockmaps_location"},
				"$Mapspec{file_basename}.$Mapspec{format}" );

	%Mapspec = %{set_projection( \%Mapspec )};
	%Mapspec = %{set_rectangles( \%Mapspec )};

	return \%Mapspec;
}

1;
