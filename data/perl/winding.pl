#Perl Reimplementation of winding-number algorithm
#
# Godkin and Pulli, BSSA v.74 1845-1948, 1984. 
#
# K. Lindquist
# Geophysical Institute
# University of Alaska
# 2001

sub get_containing_regions {
	my( @db ) = splice( @_, 0, 4 );
	my( $lat, $lon ) = splice( @_, 0, 2 );
	my( @regnames );
	
	@db = dbsort( @db, "regname" );
	@db = dbgroup( @db, "regname" );

	my( $nregions ) = dbquery( @db, "dbRECORD_COUNT" );

	for( $db[3] = 0; $db[3] < $nregions; $db[3]++ ) {

		my( $regname ) = dbgetv( @db, "regname" );
		my( @polygon ) = get_region_polygon( @db, $regname );

		if( is_geographically_inside( $lat, $lon, @polygon ) ) {

			push( @regnames, $regname );
		}
	}

	return @regnames;
}

sub is_inside_region {
	my( @db ) = splice( @_, 0, 4 );
	my( $lat, $lon ) = splice( @_, 0, 2 );

	if( defined( $region_name = shift( @_ ) ) ) {

		@db = dbsubset( @db, "regname == \"$region_name\"" );

		my( $nrecs ) = dbquery( @db, "dbRECORD_COUNT" );

		if( $nrecs <= 0 ) {
			print STDERR "is_inside_region(): Didn't find " .
				"\"$region_name\" in database.\n";
			return 0;
		}
	}

	my( @polygon ) = dbview_to_polygon( @db );

	return is_geographically_inside( $lat, $lon, @polygon );
}

sub get_region_polygon {
	my( $region_name ) = pop( @_ );
	my( @db ) = @_;

	@db = dblookup( @db, "", "regions", "", "" );
	@db = dbsubset( @db, "regname == \"$region_name\"" );

	return dbview_to_polygon( @db );
}

sub dbview_to_polygon {
	my( @db ) = @_;
	my( $nrecs, @polygon, $lat, $lon, $vertex );
	my( $previous_vertex ) = 0;

	@db = dbsort( @db, "regname", "vertex" );

	$nrecs = dbquery( @db, "dbRECORD_COUNT" );

	if( $nrecs < 1 ) {
		print STDERR 	
			"dbview_to_polygon(): couldn't find region in view\n";
		return ();

	} elsif( $nrecs < 3 ) {
		print STDERR 	
			"dbview_to_polygon(): not enough vertices in view\n";
		return ();
	}

	for( $db[3] = 0; $db[3] < $nrecs; $db[3]++ ) {
		( $lat, $lon, $vertex ) = dbgetv( @db, "lat", "lon", "vertex" );

		if( $vertex < $previous_vertex ) {
			print STDERR "dbview_to_polygon(): More than one " .
				"region in view. Bye.\n";
			return ();
		} else {
			$previous_vertex = $vertex;
		}

		push( @polygon, $lat, $lon );
	}

	return @polygon;
}

# This geographic application of the winding routine is valid
# when the polygon and the test point are entirely within
# the same hemisphere.
sub is_geographically_inside {
	my( $lat, $lon, @polygon ) = @_;
	my( $npoints, $dist, $az, $pi,
	    $point_lat, $point_lon, @geo_polygon );

	if( ! polygon_ok( @polygon ) ) {
		print STDERR "Polygon is Not OK";
		return 0;
	}

	$npoints = ( $#polygon + 1 ) / 2;

	$pi = atan2( 1, 1 ) * 4;

	# polar stereographic projection
	for( $pointnum = 1; $pointnum <= $npoints; $pointnum++ ) {
		$point_lat = $polygon[($pointnum-1)*2];
		$point_lon = $polygon[($pointnum-1)*2+1];

		$dist = dbex_eval( dbinvalid(), 
				"distance($lat,$lon,$point_lat,$point_lon)" );
		$az = dbex_eval( dbinvalid(), 
				"azimuth($lat,$lon,$point_lat,$point_lon)" );

		if( $dist > 90. ) {
			print STDERR "Warning: The routine " .
				"is_geographically_inside() " .
				"is regional only. Polygon and test point " .
				"must be entirely within the same " .
				"hemisphere. Returning FALSE.\n";
			return 0;
		}

		push( @geo_polygon, $dist * sin( $az * $pi/180. ) );
		push( @geo_polygon, $dist * cos( $az * $pi/180. ) );
	}

	return is_inside_polygon( 0, 0, @geo_polygon );
}

sub is_inside_polygon { 
	my( $x, $y, @polygon ) = @_;

	if( ! polygon_ok( @polygon ) ) {
		print STDERR "Polygon is Not OK";
		return 0;
	}

	my( @shifted_polygon ) = shift_polygon( $x, $y, @polygon );

	my( $winding_number ) = winding_number( @shifted_polygon );

	if( $winding_number ) {
		return 1;
	} else {
		return 0;
	}
}

sub shift_polygon {
	my( $x, $y, @polygon ) = @_;
	my( $nsegs, $ix, $iy, $shifted_x, $shifted_y, @shifted_polygon );
	
	$nsegs = ( $#polygon + 1 ) / 2;

	for( $seg = 1; $seg <= $nsegs; $seg++ ) {

		$ix = 2 * ( $seg - 1 );
		$iy = $ix + 1;
		
		$shifted_x = $polygon[$ix] - $x;
		$shifted_y = $polygon[$iy] - $y;
	
		push( @shifted_polygon, $shifted_x );
		push( @shifted_polygon, $shifted_y );
	}

	return @shifted_polygon;
}

sub winding_number {
	my( @shifted_polygon ) = @_;
	my( $ncoords, $nsegs, $winding_number, $seg, @segment, $scn );
	
	# polygon coordinate list must be x1,y1,x2,y2,x3,y3, etc
	
	$ncoords = $#shifted_polygon + 1;
	$nsegs = $ncoords / 2;

	$winding_number = 0;

	for( $seg = 1; $seg <= $nsegs; $seg++ ) {
		
		@segment = segment_number( $seg, @shifted_polygon );
		$scn = signed_crossing_number( @segment );

		$winding_number = $winding_number + $scn;
	}

	return $winding_number;

}

sub polygon_ok {
	my( @polygon ) = @_;

	$ncoords = $#polygon + 1;

	if( $ncoords < 6 ) {
		
		return 0;

	} elsif( is_odd( $ncoords ) ) {
		
		return 0;

	} else {

		return 1;
	}
}

sub is_odd {
	my( $n ) = @_;

	return int( $n / 2 ) != $n / 2;
}

sub segment_number {
	my( $segnum, @polygon ) = @_;
	my( $ncoords, $nsegs, @segment );
	
	$ncoords = $#polygon + 1;

	$nsegs = $ncoords / 2;

	if( $segnum > $nsegs ) {
		print STDERR "ERROR: not that many segments in polygon";
		return ();
	}

	if( $segnum == $nsegs ) {
		push( @segment, $polygon[$ncoords-2] );
		push( @segment, $polygon[$ncoords-1] );
		push( @segment, $polygon[0] );
		push( @segment, $polygon[1] );
	} else {
		push( @segment, $polygon[($segnum-1) * 2] );
		push( @segment, $polygon[($segnum-1) * 2 + 1] );
		push( @segment, $polygon[($segnum-1) * 2 + 2] );
		push( @segment, $polygon[($segnum-1) * 2 + 3] );
	}

	return @segment;
}

sub signed_crossing_number {
	my( $x1, $y1, $x2, $y2 ) = @_;
	my( $xintercept, $slope, $direction );

	if( $y1 * $y2 > 0 ) {
		# no crossing -- both points are on same side of y axis
		return 0;
	}
	
	# Horizontal lines:
	if( $y1 == 0 && $y2 == 0 ) {

		if( $x1 * $x2 > 0 ) {
			# no crossing
			return 0;
		} else {
			# segment crosses or touches origin
			return 2;
		}
	}

	if( $x2 == $x1 ) {
		# Vertical lines
		$xintercept = $x1;
	} else {
		$slope = ( $y2 - $y1 ) / ( $x2 - $x1 );
		$xintercept = $x1 - $y1 / $slope;
	}
	
	if( $y2 > $y1 ) {
		$direction = 1;
	} else {
		$direction = -1;
	}

	if( $xintercept > 0 ) { 

		return 0;

	} elsif( $xintercept == 0 ) {

		return 2;

	} elsif( $y1 == 0 || $y2 == 0 ) {

		return $direction * 0.5;

	} else { 

		return $direction;
	}
}

sub test_winding_signed_crossing_number {
	print STDERR "Above x axis (expect 0): " . 
			signed_crossing_number( 1, 3, 2, 4 ), "\n";
	print STDERR "Below x axis (expect 0): " . 
			signed_crossing_number( 1, -3, 2, -4 ), "\n";
	print STDERR "\n";
	
	print STDERR "Vertical above x axis (expect 0): " . 
			signed_crossing_number( 1, 3, 1, 4 ), "\n";
	print STDERR "On y axis above x axis (expect 0): " . 
			signed_crossing_number( 0, 3, 0, 4 ), "\n";
	print STDERR "On y axis below x axis (expect 0): " . 
			signed_crossing_number( 0, -3, 0, -4 ), "\n";
	print STDERR "Vertical through origin (expect 2): " . 
			signed_crossing_number( 0, -1, 0, 1 ), "\n";
	print STDERR "\n";

	print STDERR "Horizontal on positive x axis (expect 0): " . 
			signed_crossing_number( 2, 0, 4, 0 ), "\n";
	print STDERR "Horizontal on negative x axis (expect 0): " . 
			signed_crossing_number( -2, 0, -4, 0 ), "\n";
	print STDERR "Horizontal through origin (expect 2): " . 
			signed_crossing_number( -1, 0, 1, 0 ), "\n";
	print STDERR "\n";

	print STDERR "starts on origin (expect 2): " . 
			signed_crossing_number( 0, 0, -1, 1 ), "\n";
	print STDERR "ends on origin (expect 2): " . 
			signed_crossing_number( -2, -2, 0, 0 ), "\n";
	print STDERR "\n";

	print STDERR "vertical through positive x axis (expect 0): " . 
			signed_crossing_number( 1, -2, 1, 2 ), "\n";
	print STDERR "oblique through positive x axis (expect 0): " . 
			signed_crossing_number( 1, -2, 2, 2 ), "\n";
	print STDERR "\n";

	print STDERR "vertical up through negative x axis (expect 1): " .
			signed_crossing_number( -1, -2, -1, 2 ), "\n";
	print STDERR "vertical down through negative x axis (expect -1): " .
			signed_crossing_number( -1, 2, -1, -2 ), "\n";
	print STDERR "\n";

	print STDERR "oblique up through negative x axis (expect 1): " .
			signed_crossing_number( -4, -2, -1, 1 ), "\n";
	print STDERR "oblique down through negative x axis (expect -1): " .
			signed_crossing_number( -1, 1, -4, -2 ), "\n";
	print STDERR "\n";

	print STDERR "through origin (expect 2): " . 
			signed_crossing_number( -1, -1, 1, 1 ), "\n";
	print STDERR "through origin other way (expect 2): " . 
			signed_crossing_number( -1, 1, 1, -1 ), "\n";
	print STDERR "\n";

	print STDERR "stops on negative x axis going down (expect -0.5): " .
			signed_crossing_number( -1, 2, -3.3, 0 ), "\n";
	print STDERR "stops on negative x axis going up (expect 0.5): " .
			signed_crossing_number( -1, -2, -3.3, 0 ), "\n";
	print STDERR "starts on negative x axis going down (expect -0.5): " .
			signed_crossing_number( -1, 0, -3.3, -5 ), "\n";
	print STDERR "starts on negative x axis going up (expect 0.5): " .
			signed_crossing_number( -1, 0, -3.3, 4 ), "\n";
	print STDERR "\n";
	
	print STDERR "point in first quadrant (expect 0): " . 
			signed_crossing_number(   1,  1,  1,  1 ), "\n";
	print STDERR "point in second quadrant (expect 0): " . 
			signed_crossing_number( -2,  2, -2,  2 ), "\n";
	print STDERR "point in third quadrant (expect 0): " . 
			signed_crossing_number(  -3, -3, -3, -3 ), "\n";
	print STDERR "point in fourth quadrant (expect 0): " . 
			signed_crossing_number(  4, -4,  4, -4 ), "\n";
	print STDERR "point at origin (expect 2): " . 
			signed_crossing_number( 0, 0, 0, 0 ), "\n";
}

sub test_winding_is_inside_polygon {
	@polygon = ( 1, 1, 1, -1, -1, -1, -1, 1 );
	print STDERR "Inside(expect 1): " .
		is_inside_polygon( 0, 0, @polygon ), "\n";

	@polygon = ( 0, 2, 0, 4, -1, 3, -4, -5 );
	print STDERR "Inside(expect 0): " . 
		is_inside_polygon( 0, 0, @polygon ), "\n";

	@polygon = (  0, 2, 0, 4, -1, 3, -4, -5, 0, 0 );
	print STDERR "Inside(expect 1): " . 
		is_inside_polygon( 0, 0, @polygon ), "\n";

	@polygon = ( -5, 1, -8, 1, -8, -2, -6, 0, -5, -2, );
	print STDERR "Inside(expect 0): " . 
		is_inside_polygon( 0, 0, @polygon ), "\n";

	@polygon = ( -3, -2, -2, 0, 0, 0 );
	print STDERR "Inside(expect 1): " . 
		is_inside_polygon( 0, 0, @polygon ), "\n";

	@polygon = ( 3 );
	print STDERR "Inside(expect blowup): " . 
		is_inside_polygon( 0, 0, @polygon ), "\n";

	@polygon = ( 1, 2, 3, 2, 3, 5, 1, 5, 0, 4 );
	print STDERR "Inside(expect 0): " . 
		is_inside_polygon( -0.001, 4, @polygon ), "\n";
}

# test_winding_signed_crossing_number();
# test_winding_is_inside_polygon();

1;
