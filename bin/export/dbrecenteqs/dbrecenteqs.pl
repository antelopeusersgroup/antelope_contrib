
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
		"cities_dbname",
		"grddb",
		"hypocenter_dbname",
		"stylesheet",
		"vrml_stylesheet"
		);
	# N.B. ( Handle the linefiles hash in plot_linefiles() )

	if( ($hashname ne "focus_map") && (! defined( $mapspec->{mapname} )) ) {

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

	return $mapspec;
}

sub setup_State {

	$State{pf} =~ s/\.pf$//;

	if( system("pfecho $State{pf} > /dev/null 2>&1" ) ) {
		die( "Couldn't find $State{pf}.pf. Bye.\n" );
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
		"grdimage"
		);

	foreach $helper ( @helpers ) {
		next if check_for_executable( $helper );
		die( "Couldn't find executable named $helper\n" );
	}
}

sub check_dir_dfile {
	my( $dfile ) = pop( @_ );
	my( $dir ) = pop( @_ );
	my( @db ) = @_;

	@db = dblookup( @db, "", "", "dir", "" );
	$dir_size = dbquery( @db, "dbFIELD_SIZE" );

	if( length( $dir ) > $dir_size ) {
		print STDERR 
			"\n\t************************************\n" . 
			"\tWARNING: Truncating dir\n\t'$dir';\n" .
			"\tPROBABLE DATABASE CORRUPTION!!\n" .
			"\t************************************\n\n";
	}

	@db = dblookup( @db, "", "", "dfile", "" );
	$dfile_size = dbquery( @db, "dbFIELD_SIZE" );

	if( length( $dfile ) > $dfile_size ) {
		print STDERR 
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
			print STDERR "Please upgrade to dbrecenteqs1.1.\n";
			$present = 0;
		}
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
	
	@db = dbprocess( @db, "dbopen webmaps",
			      "dbjoin event",
			      "dbjoin origin event.evid#origin.evid",
			      "dbsubset origin.lddate > webmaps.lddate || event.lddate > webmaps.lddate",
			      "dbseparate webmaps" );

	my( $stale_nrecs ) = dbquery( @db, "dbRECORD_COUNT" );

	print STDERR "Removing $stale_nrecs stale webmap entries\n";

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
		complain( 1, "Unknown position $position in &more_ps\n" );
		return "";
	}
}

sub plot_basemap {
	my( %Mapspec ) = %{shift( @_ )};
	my( $position ) = shift( @_ );

	my( $more, $redirect ) = more_ps( $position );

	my( $cmd ) = "psbasemap " . 
			"-X0 -Y0 -P -V " .
			"-Bg$Mapspec{gridline_interval_deg}wesn " .
			"$Mapspec{Rectangle} $Mapspec{Projection} " . 
			$more . 
			"$redirect $Mapspec{psfile}";
	print "$cmd\n";
	system( $cmd );
}

sub plot_lakes {
	my( %Mapspec ) = %{shift( @_ )};
	my( $position ) = shift( @_ );

	my( $more, $redirect ) = more_ps( $position );

	my( $cmd ) = "pscoast -V -P -X0 -Y0 " .
			"$Mapspec{Rectangle} $Mapspec{Projection} " .
			"-D$Mapspec{detail_density} " .
			"-C0/0/255 " .
			$more .
			"$redirect $Mapspec{psfile}";
			
	print "$cmd\n";
	system( $cmd );
}

sub plot_state_boundaries {
	my( %Mapspec ) = %{shift( @_ )};
	my( $position ) = shift( @_ );

	my( $more, $redirect ) = more_ps( $position );

	my( $cmd ) = "pscoast -V -P -X0 -Y0 " .
			"$Mapspec{Rectangle} $Mapspec{Projection} " .
			"-D$Mapspec{detail_density} " .
			"-N2/2/0/0/0 " . 
			$more . 
			"$redirect $Mapspec{psfile}";
			
	print "$cmd\n";
	system( $cmd );
}

sub plot_national_boundaries {
	my( %Mapspec ) = %{shift( @_ )};
	my( $position ) = shift( @_ );

	my( $more, $redirect ) = more_ps( $position );

	my( $cmd ) = "pscoast -V -P -X0 -Y0 " .
			"$Mapspec{Rectangle} $Mapspec{Projection} " .
			"-D$Mapspec{detail_density} " .
			"-N1/5/0/0/0 " . 
			$more . 
			"$redirect $Mapspec{psfile}";
			
	print "$cmd\n";
	system( $cmd );
}

sub plot_rivers {
	my( %Mapspec ) = %{shift( @_ )};
	my( $position ) = shift( @_ );

	my( $more, $redirect ) = more_ps( $position );

	my( $cmd ) = "pscoast -V -P -X0 -Y0 " .
			"$Mapspec{Rectangle} $Mapspec{Projection} " .
			"-D$Mapspec{detail_density} " .
			"-Ir/1/0/0/255 " .
			$more .
			"$redirect $Mapspec{psfile}";
			
	print "$cmd\n";
	system( $cmd );
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
		print N sprintf( "%.4f %.4f 9 0.0 1 1 %s\n",
			$lon+$Mapspec{cityname_shift_deg},
			$lat,
			$place );
	}

	close( C );
	close( N );

	dbclose( @db );

	return ( $locs_tempfile, $names_tempfile );

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

	print STDERR "Subsetting database for hypocenters...";
	@db = dbsubset( @db, $expr );
	print STDERR "done\n";

	$nrecs = dbquery( @db, "dbRECORD_COUNT" );
			
	print STDERR "Building temp file of $nrecs hypocenters...";
	open( H, ">$tempfile" );

	for( $db[3] = 0; $db[3] < $nrecs; $db[3]++ ) {
		my( $lat, $lon, $depth ) = 
			dbgetv( @db, "lat", "lon", "depth" );
		print H sprintf( "%.4f %.4f %.2f\n", 
			unwrapped_lon( \%Mapspec, $lon ), $lat, $depth );
	}

	close( H );
	print STDERR "done\n";

	dbclose( @db );

	return $tempfile;
}

sub plot_hypocenters {
	my( %Mapspec ) = %{shift( @_ )};
	my( $position ) = shift( @_ );

	if( ! defined( $Mapspec{hypocenter_dbname} ) || 
	      $Mapspec{hypocenter_dbname} eq "" ) {
		print STDERR 
			"\n\t************************************\n" . 
			"\tWARNING: Skipping hypocenters--" .
			"\tno hypocenter_dbname specified\n" .
			"\t************************************\n\n";
		return;

	} elsif( ! -e "$Mapspec{hypocenter_dbname}.origin" ) {

		print STDERR
			"\n\t************************************\n" . 
			"\tWARNING: Skipping hypocenters--" .
			"\t$Mapspec{hypocenter_dbname}.origin not found\n" .
			"\t************************************\n\n";
		return;
	}

	my( $more, $redirect ) = more_ps( $position );

	my ( $hypocenter_tempfile ) =
		make_hypocenter_tempfile( \%Mapspec );

	my( $cmd ) = "cat $hypocenter_tempfile | psxy -V -P " .
			"$Mapspec{Rectangle} $Mapspec{Projection} " .
			"-C$Mapspec{depth_color_palette_file} " .
			"-Ss$Mapspec{background_magsize_pixels}p " .
			$more .
			"$redirect $Mapspec{psfile}";

	print "$cmd\n";
	system( $cmd );

	unlink( $hypocenter_tempfile );
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

sub plot_contours {
	my( %Mapspec ) = %{shift( @_ )};
	my( $position ) = shift( @_ );

	if( $position ne "middle" ) {
		printf STDERR "Warning: contours only implemented for " .
		 "middle-position plotting\n";
	}

	my( $more, $redirect ) = more_ps( $position );

	my( $cmd );

	if( $Mapspec{contour_mode} eq "none" ) {

		$cmd = "pscoast -V -P " .
			"$Mapspec{Rectangle} $Mapspec{Projection} " .
			"-C200/200/255 -S200/200/255 -G255/243/230 " .
			"-D$Mapspec{detail_density} " .
			$more .
			"$redirect $Mapspec{psfile}";
		print "$cmd\n";
		system( $cmd );

	} elsif( ( $Mapspec{contour_mode} eq "grddb" ) &&
	    ( ! -e "$Mapspec{grddb}" ) ) {

		print STDERR
			"\n\t************************************\n" . 
			"\tWARNING: Setting contour_mode to \"none\":\n" .
			"\tgrddb '$Mapspec{grddb}' not found\n" .
			"\t************************************\n\n";

		$cmd = "pscoast -V -P " .
			"$Mapspec{Rectangle} $Mapspec{Projection} " .
			"-C200/200/255 -S200/200/255 -G255/243/230 " .
			"-D$Mapspec{detail_density} " .
			$more .
			"$redirect $Mapspec{psfile}";
		print "$cmd\n";
		system( $cmd );

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

		$grdfile = "$State{workdir}/grd_$<_$$.grd";
		$gradfile = "$State{workdir}/grad_$<_$$.grad";
		$psclipfile = "$State{workdir}/psclip_$<_$$.clip";

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

		    my( $tile ) = "-R$w/$e/$s/$n";

		    my( $rc ) = dbgmtgrid( @dbgrid, $tile,
				           $grdfile, verbose => 1 );
		    if( $rc < 0 ) {
			print STDERR
			"\n\t************************************\n" . 
			"\tWARNING: dbgmtgrid() failed for tile '$tile'\n" .
			"\t************************************\n\n";
			next;
		    }

		    $cmd = "grdgradient $grdfile -G$gradfile -V $Mapspec{grdgradient_opt}";
		    print "$cmd\n";
		    system( $cmd );

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

		    $cmd = "psclip -V $psclipfile " .
			   "$Mapspec{Rectangle} $Mapspec{Projection} " .
			   $more .
			   "$redirect $Mapspec{psfile}";

		    print "$cmd\n";
		    system( $cmd );

		    $cmd = "grdimage -V -P " .
		      	"$Mapspec{Rectangle} $Mapspec{Projection} " .
			"$grdfile " .
			"-I$gradfile " .
			"-C$Mapspec{map_color_palette_file} " .
			$more .
			"$redirect $Mapspec{psfile}";

		    print "$cmd\n";
		    system( $cmd );

		    $cmd = "psclip -V -C " .
			   $more .
			   "$redirect $Mapspec{psfile}";

		    print "$cmd\n";
		    system( $cmd );

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

sub plot_coastlines {
	my( %Mapspec ) = %{shift( @_ )};
	my( $position ) = shift( @_ );

	my( $more, $redirect ) = more_ps( $position );

	my( $cmd ) = "pscoast -V -P " .
		     "$Mapspec{Rectangle} $Mapspec{Projection} " .
		     "-W2/0/0/0 " .
		     "-D$Mapspec{detail_density} " .
		     $more .
		     "$redirect $Mapspec{psfile}";
			
	print "$cmd\n";
	system( $cmd );
}

sub datafile_abspath {
	my( $file ) = shift( @_ );

	if( ! defined( $file ) ) {

		return undef;

	} elsif( $file eq "" ) {

		return "";
	}

	if( $file !~ m@^\.?/@ ) {

		$file = "$ENV{ANTELOPE}/data/dbrecenteqs/" . $file;
	}

	return $file;
}

sub plot_linefiles {
	my( %Mapspec ) = %{shift( @_ )};
	my( $position ) = shift( @_ );

	if( $position ne "middle" ) {
		printf STDERR "Warning: linefiles only implemented for " .
		 "middle-position plotting\n";
	}

	my( $more, $redirect ) = more_ps( $position );

	my( $name, $file, $pen, $cmd );

	foreach $line ( @{$Mapspec{linefiles}} ) {

		next if( $line =~ /^\s*$/ );

		( $name, $file, $pen ) = split( /\s+/, $line );

		$file = datafile_abspath( $file );

		if( ! -e $file ) {

			print STDERR 
				"\n\t************************************\n" . 
				"\tWARNING: Couldn't find linefile " .
			     	"$file -- skipping\n" .
				"\t************************************\n\n";
			next;

		} elsif( ! defined( $pen ) || $pen eq "" ) {

			print STDERR 
				"\n\t************************************\n" . 
				"\tWARNING: No pen for linefile " .
			     	"$file -- default to black\n" .
				"\t************************************\n\n";

			$pen = "4/0/0/0";
		}

		$cmd = "psxy -V -P " .
			"$Mapspec{Rectangle} $Mapspec{Projection} " .
			"$file -M -W$pen " .	
			$more .
			"$redirect $Mapspec{psfile}";
			
		print "# plotting $name:\n$cmd\n";
		system( $cmd );
	}
}

sub plot_cities {
	my( %Mapspec ) = %{shift( @_ )};
	my( $position ) = shift( @_ );

	if( ! defined( $Mapspec{cities_dbname} ) || 
	      $Mapspec{cities_dbname} eq "" ) {
		print STDERR 
			"\n\t************************************\n" . 
			"\tWARNING: Skipping cities--" .
			"no cities_dbname specified\n" .
			"\t************************************\n\n";
		return;

	} elsif( ! -e "$Mapspec{cities_dbname}.places" ) {

		print STDERR
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

	my( $cmd ) = "cat $locs_tempfile | psxy -V -P " .
			"$Mapspec{Rectangle} $Mapspec{Projection} " .
			"-Ss$Mapspec{city_symbols_inches}i -G0 " .
			$more .
			"$redirect $Mapspec{psfile}";
			
	print "$cmd\n";
	system( $cmd );

	if( $position eq "first" ) {
		( $more, $redirect ) = more_ps( "middle" );
	} elsif( $position eq "single" ) {
		( $more, $redirect ) = more_ps( "last" );
	} else {
		( $more, $redirect ) = more_ps( $position );
	}

	my( $cmd ) = "cat $names_tempfile | pstext -V -P " .
			"$Mapspec{Rectangle} $Mapspec{Projection} " .
			$more .
			"$redirect $Mapspec{psfile}";

	print "$cmd\n";
	system( $cmd );

	unlink( $locs_tempfile );
	unlink( $names_tempfile );
}

sub plot_template {
	my( %Mapspec ) = %{shift( @_ )};
	my( $position ) = shift( @_ );

	my( $more, $redirect ) = more_ps( $position );

	my( $cmd ) = "";
			
	print "$cmd\n";
	system( $cmd );
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

	plot_basemap( \%Mapspec, "first" );
	plot_contours( \%Mapspec, "middle" );
	plot_coastlines( \%Mapspec, "middle" );
	plot_lakes( \%Mapspec, "middle" );
	plot_rivers( \%Mapspec, "middle" );
	plot_national_boundaries( \%Mapspec, "middle" );
	plot_state_boundaries( \%Mapspec, "middle" );
	plot_hypocenters( \%Mapspec, "middle" );
	plot_linefiles( \%Mapspec, "middle" );
	plot_basemap( \%Mapspec, "middle" );
	plot_cities( \%Mapspec, "last" );

	%Mapspec = %{pixfile_convert( \%Mapspec )};
	write_pixfile_pffile( \%Mapspec );

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

	print STDERR "Re-reading index map $Mapspec{mapname} from file...\n";

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

		$cmd = "convert -crop 0x0 -density $Mapspec{pixels_per_inch} " .
		       "-size $size_pixels\\x$size_pixels -colors $ncolors " .
		       "$Mapspec{psfile} $Mapspec{pixfile}"; 

	} else {

		 die( "pixfile_conversion_method " . 
		     "$State{pixfile_conversion_method} " .
		     "not supported." );
	}

	print "$cmd\n";
	system( $cmd );

	$Mapspec{clean_image} = Image::Magick->new();
	$Mapspec{clean_image}->Read( $Mapspec{pixfile} );
	
	%Mapspec = %{set_map_width( \%Mapspec )};
	%Mapspec = %{set_map_scaling( \%Mapspec )};

	return \%Mapspec;
}

sub write_pixfile_pffile {
	my( %Mapspec ) = %{shift( @_ )};

	my( $normal_lonc ) = normal_lon( $Mapspec{lonc} );

	if( ! -e "$Mapspec{pixfile}" ) {

		complain( 1, "Won't write $Mapspec{pixfile}.pf; "
			     "$Mapspec{pixfile} does not exist\n" );
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

	print STDERR "Re-generating index map $Mapspec{mapname} dynamically...\n";
	
	$Mapspec{"file_basename"} = $Mapspec{"mapname"};

	$Mapspec{"lonc"} = unwrapped_lon( \%Mapspec, $Mapspec{"lonc"} );
	
	$Mapspec{"psfile"} = concatpaths( $State{"workdir"},
				"$Mapspec{file_basename}.ps" );

	$Mapspec{"pixfile"} = concatpaths( "$ENV{ANTELOPE}/data/dbrecenteqs",
				"$Mapspec{file_basename}.$Mapspec{format}" );

	%Mapspec = %{set_projection( \%Mapspec )};
	%Mapspec = %{set_rectangles( \%Mapspec )};

	return \%Mapspec;
}

1;
