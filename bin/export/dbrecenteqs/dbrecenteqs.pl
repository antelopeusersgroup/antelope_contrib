
sub read_map_config {
	my( $pf, $hashname ) = @_;
	my( $map_config );

	my( $hash ) = pfget( $pf, $hashname );
	if( defined( $hash->{include} ) ) {
		$map_config = pfget( $pf, $hash->{include} );
	}

	foreach $key ( keys( %$hash ) ) {
		$map_config->{$key} = $hash->{$key};
	}
	
	$map_config->{longitude_branchcut_low} =
		$map_config->{longitude_branchcut_high} - 360;

	$map_config->{mapclass} = $hashname;
	$map_config->{mapclass} =~ s/_map_config//;
		
	return $map_config;
}

sub setup_State {
	$Pf = "dbrecenteqs_setup";

	my( @params ) = (
		"pixfile_conversion_method",
		"stock_mapdir"
		);
	
	foreach $param ( @params ) {
		$State{$param} = pfget( $Pf, $param );
	}

	$State{index_map_config} = read_map_config( $Pf, "index_map_config" );
	$State{global_map_config} = read_map_config( $Pf, "global_map_config" );
	$State{focus_map_config} = read_map_config( $Pf, "focus_map_config" );
	$State{detail_map_config} = read_map_config( $Pf, "detail_map_config" );

	$State{workdir} = "/tmp/dbrecenteqs_$<_$$";
	mkdir( $State{workdir}, 0755 );
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
sub set_rectangle {
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

	$Mapspec{"Rectangle"} = sprintf( "-R%.4f/%.4f/%.4f/%.4fr", 
					$Mapspec{"lon_ll"},
					$Mapspec{"lat_ll"},
					$Mapspec{"lon_ur"},
					$Mapspec{"lat_ur"} );

	$Mapspec{"InclusiveRectangle"} = sprintf( "-R%d/%d/%d/%dr", 
					int( $Mapspec{"lon_ll"} - 1 ),
					int( $Mapspec{"lat_ll"} - 1 ),
					int( $Mapspec{"lon_ur"} + 1.5 ),
					int( $Mapspec{"lat_ur"} + 1.5 ) );

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
			"-X0 -Y0 -P -V -Bg5wesn " .
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

sub plot_political_boundaries {
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

sub plot_contours {
	my( %Mapspec ) = %{shift( @_ )};
	my( $position ) = shift( @_ );

	my( $more, $redirect ) = more_ps( $position );

	my( $cmd );

	if( $Mapspec{contour_mode} eq "premade" ) {

		if( ! -e "$Mapspec{premade_contour_ps}" ) {

			die( "Couldn't find $Mapspec{premade_contour_ps}. Bye!" );
		}

		$cmd = "cat $Mapspec{premade_contour_ps} $redirect $Mapspec{psfile}";
		
	} elsif( $Mapspec{contour_mode} =~ /grdcut|grdimage/ ) {

		my( $grdfile, $gradfile );

		if( $Mapspec{contour_mode} eq "grdimage" ) {

			$grdfile = $Mapspec{grdfile};
			$gradfile = $Mapspec{gradfile};

		} else { # grdcut

			$grdfile = "$State{workdir}/grd_$<_$$.grd";
			$gradfile = "$State{workdir}/grad_$<_$$.grad";

			$cmd = "grdcut $Mapspec{grdfile} -G$grdfile " .
				"-V $Mapspec{InclusiveRectangle}";
			print "$cmd\n";
			system( $cmd );

			$cmd = "grdgradient $grdfile -G$gradfile -V -A60 -Nt";
			print "$cmd\n";
			system( $cmd );
		}

		$cmd = "grdimage -V -P " .
			"$Mapspec{Rectangle} $Mapspec{Projection} " .
			"$grdfile " .
			"-I$gradfile " .
			"-C$Mapspec{map_color_palette_file} " .
			$more .
			"$redirect $Mapspec{psfile}";

	} elsif( $Mapspec{contour_mode} eq "none" ) {

		$cmd = "pscoast -V -P " .
			"$Mapspec{Rectangle} $Mapspec{Projection} " .
			"-C200/200/255 -S200/200/255 -G255/243/230 " .
			"-D$Mapspec{detail_density} " .
			$more .
			"$redirect $Mapspec{psfile}";
			

	} else {
		die( "contour_mode $Mapspec{contour_mode} not supported\n" );
	}

	print "$cmd\n";
	system( $cmd );
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

		( $name, $file, $pen ) = split( /\s+/, $line );

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

	plot_basemap( \%Mapspec, "first" );
	plot_contours( \%Mapspec, "middle" );
	plot_lakes( \%Mapspec, "middle" );
	plot_rivers( \%Mapspec, "middle" );
	plot_political_boundaries( \%Mapspec, "middle" );
	plot_hypocenters( \%Mapspec, "middle" );
	plot_linefiles( \%Mapspec, "middle" );
	plot_basemap( \%Mapspec, "middle" );
	plot_cities( \%Mapspec, "last" );

	%Mapspec = %{pixfile_convert( \%Mapspec )};
	write_pixfile_pffile( \%Mapspec );

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

	my( $mapclass ) = dbgetv( @db, "mapclass" );
	my( $config ) = $mapclass . "_map_config";

	%Mapspec = %{$State{$config}};

	$Mapspec{pixfile} = dbextfile( @db );

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
	my( $config ) = shift;
	my( $map_pathname ) = shift;
	my( %Mapspec );

	if( ! -e "$map_pathname" ) {

		die( "Can't find map file $map_pathname\n" );

	} elsif( ! -e "$map_pathname.pf" ) {

		die( "Can't find map parameter file $map_pathname.pf\n" );
	}

	%Mapspec = %{$State{$config}};

	my( $mapbase ) = `basename $map_pathname`;
	chomp( $mapbase );
	$mapbase =~ s/\..*$//; # remove suffix extension

	$Mapspec{mapname} = $mapbase;

	my( $hashref ) = pfget( $map_pathname, $Mapspec{mapname} );

	if( ! defined( $hashref ) ) {
		die( "Didn't find map specifications for map " .
		     "name $Mapspec{mapname} in $map_pathname. Bye.\n" );
	}

	%Mapspec = ( %Mapspec, %$hashref );

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

		$cmd = "alchemy -Zm4 -Zc1 -o $format " .
			"$Mapspec{psfile} $Mapspec{pixfile} " .
			"-c 256 $Mapspec{reserve_colors} " .
			"-Xd$size_pixels\p -+";

	} elsif( $State{pixfile_conversion_method} eq "pnm" ) {

		if( $Mapspec{format} eq "jpg" ) {
			die( "jpg incompatible with pnm conversion\n" );
		}

		my( $ncolors ) = 256 - $Mapspec{reserve_colors};

		$cmd = "cat $Mapspec{psfile} | gs -sOutputFile=- -q " .
		       "-sDEVICE=ppm -r$Mapspec{pixels_per_inch} " .
		       "-g$size_pixels\\x$size_pixels - | pnmcrop - " .
		       "| ppmquant $ncolors | ppmtogif > $Mapspec{pixfile}";

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

	open( P, ">$Mapspec{pixfile}.pf" );

	print P "$Mapspec{filebase} &Arr{\n";
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

1;
