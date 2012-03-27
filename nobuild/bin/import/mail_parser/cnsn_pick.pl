
sub cnsn_pick_handler {
	my( $message, $pfarray ) = @_;

	grep {
		s/\*/0/g;

		if( /\(\(.* ARRAY -\s*(.*)\)\)/ ) {
			$msgtype = $1;
		}

		$regexp = "(\\w+)\\s+(\\D+)(\\d+)\\s+(\\w)([\\d.]+).*" .
			  "SLO([\\d.]+)\\s+AZ([\\d.]+)";

		if( /$regexp/ ) {
			( $sta, $month, $day, $phase, $arrtime, $slo, $az ) = 
			( $1, $2, $3, $4, $5, $6, $7 );
		}

		$regexp = "\\(\\(OT([\\d.]+)\\s+LAT([\\d.]+)([NS])\\s+" .
			  "LON([\\d.]+)([EW])\\s+DEP([\\d.]+)\\s+(M\\w)" .
			  "([\\d.]+)\\s*\\)\\)";

		if( /$regexp/ ) {
			( $ot, $lat, $ns, $lon, $ew, $depth, $magtype, $mag ) = 
			( $1, $2, $3, $4, $5, $6, $7, $8 );
		}

	} @{$message->body()};

	$year = epoch2str( str2epoch( "now" ), "%Y" );

	substr( $arrtime, 2, 0 ) = ":";
	substr( $arrtime, 5, 0 ) = ":";

	$phase_time = str2epoch( "$month $day, $year $arrtime" );

	if( $phase_time > str2epoch( "now" ) ) {
		# phase happened last year; mail came this year
		$year--;
		$phase_time = str2epoch( "$month $day, $year $arrtime" );
	}

	substr( $ot, 2, 0 ) = ":";
	substr( $ot, 5, 0 ) = ":";

	$origin_time = str2epoch( "$month $day, $year $ot" );

	if( str2epoch( $ot ) > str2epoch( $arrtime ) ) {
		# phase arrival rolled over the day-boundary
		$origin_time -= 86400;
	}

	$lat = $ns eq "N" ? $lat : -1 * $lat;
	$lon = $ew eq "E" ? $lon : -1 * $lon;

	if( $mag == 0.0 ) { $mag = -999.; }
	$magtype = lc( $magtype );

	%authors = %{%{$pfarray}->{authors}};
	
	if( defined( $authors{$msgtype} ) ) {
		$auth = $authors{$msgtype};
	} else {
		$auth = "-";
	}

	$dbname = epoch2str( $origin_time, %{$pfarray}->{database} );

	if( $verbose ) {
		print "Converting message\n" .
		      "\tSubject: ", $message->get("Subject"), "\n",
		      "\tFrom: ", $message->get("From"), "\n",
		      "\tto database $dbname\n";
	}

	@db = dbopen( $dbname, "r+" );

	# multiple source queues may deliver the same email message. 
	# let dbaddv handle this on first-come, first-serve basis,
	# but wrap it in an eval so the program keeps going
	myeval {
		@db = dblookup( @db, "", "arrival", "", "" );

		$db[3] = dbaddv( @db, 
			"sta", $sta,
			"time", $phase_time,
			"jdate", yearday( $phase_time ),
			"iphase", $phase,
			"azimuth", $az, 
			"slow", $slo, 
			"auth", $auth );
	
		$arid = dbgetv( @db, "arid" );
	
		@db = dblookup( @db, "", "origin", "", "" );
	
		$db[3] = dbaddv( @db,
			"lat", $lat, 
			"lon", $lon,
			"depth", $depth,
			"time", $origin_time,
			"jdate", yearday( $origin_time ),
			"grn", grn( $lat, $lon ),
			"srn", srn( $lat, $lon ),
			"nass", 1,
			"ndef", 1,
			$magtype, $mag,
			"auth", $auth );
				
		$orid = dbgetv( @db, "orid" );

		@db = dblookup( @db, "", "assoc", "", "" );

		dbaddv( @db, 
			"arid", $arid, 
			"orid", $orid,
			"sta", $sta,
			"phase", $phase );
	};

	dbclose( @db );
}

1;

