
sub parse_solution {
	my( $line ) = @_;

	$solution_unpack = "x2 A1 A1 x A4 A2 A2 x A2 A2 ".
		"x A5 x A8 x A9 x A6 x A3 x A3 x4 A7 A5 x A5";

	( $sum_type, $sum_felt, $sum_year, $sum_month, $sum_day,
	  $sum_hour, $sum_minute, $sum_sec, $sum_lat, $sum_lon,
	  $sum_depth, $sum_nsta, $sum_nph, $sum_magtype,
	  $sum_mag, $sum_agency ) = unpack( $solution_unpack, $line );

	$sum_type =~ s/^\s+//;
	$sum_felt =~ s/^\s+//;
	$sum_year =~ s/^\s+//;
	$sum_month =~ s/^\s+//;
	$sum_day =~ s/^\s+//;
	$sum_hour =~ s/^\s+//;
	$sum_minute =~ s/^\s+//;
	$sum_sec =~ s/^\s+//;
	$sum_lat =~ s/^\s+//;
	$sum_lon =~ s/^\s+//;
	$sum_depth =~ s/^\s+//;
	$sum_nsta =~ s/^\s+//;
	$sum_nph =~ s/^\s+//;
	$sum_magtype =~ s/^\s+//;
	$sum_mag =~ s/^\s+//;
	$sum_agency =~ s/^\s+//;

	@dborigin = dblookup( @db, "", "origin", "", "" );
	@dbevent = dblookup( @db, "", "event", "", "" );

	$origin_time = str2epoch( "$sum_month/$sum_day/$sum_year " . 
				  "$sum_hour:$sum_minute:$sum_sec" );

	$orid = dbnextid( @dborigin, "orid" );
	$evid = dbnextid( @dborigin, "evid" );

	if( $sum_type =~ /[LE]/ ) {
		$etype = "eq";
	} elsif( $sum_type =~ /B/ ) {
		$etype = "ex";
	} else {
		$etype = "-";
	}

	undef %Magids;
	$mb = $ms = $ml = -999.00;
	$mbid = $msid = $mlid = -1;

	if( lc( $sum_magtype ) eq "mb" ) {

		$mb = $sum_mag;
		$mbid = dbnextid( @dborigin, "magid" );
		$Magids{"mb"} = $mbid;

	} elsif( lc( $sum_magtype ) eq "ms" ) {

		$ms = $sum_mag;
		$msid = dbnextid( @dborigin, "magid" );
		$Magids{"ms"} = $msid;

	} elsif( lc( $sum_magtype ) eq "ml" ) {

		$ml = $sum_mag;
		$mlid = dbnextid( @dborigin, "magid" );
		$Magids{"ml"} = $mlid;
	}

	dbaddv( @dbevent,
         	"evid", $evid,
	#        "evname", $evname,
	         "prefor", $orid,
	        "auth", $sum_agency
	#        "commid", $commid,
        	);

	dbaddv( @dborigin,
	 	"lat", $sum_lat,
	 	"lon", $sum_lon,
	 	"depth", $sum_depth,
	 	"time", $origin_time,
	 	 "orid", $orid, 	 
	 	 "evid", $evid, 	 
	 	 "jdate", yearday( $origin_time ),
	 	 "nass", $sum_nph, 	 
	 	 "ndef", $sum_nph, 	 
	# 	 "ndp", ndp, 	 
	 	 "grn", grn( $sum_lat, $sum_lon ),
	 	 "srn", srn( $sum_lat, $sum_lon ),
	 	 "etype", $etype, 	 
	# 	 "review", review, 	 
	# 	 "depdp", depdp, 	 
	# 	 "dtype", dtype, 	 # handled during error line
	 	 "mb", $mb, 	 
	 	 "mbid", $mbid, 	 
	 	 "ms", $ms, 	 
	 	 "msid", $msid, 	 
	 	 "ml", $ml, 	 
	 	 "mlid", $mlid, 	 
	# 	 "algorithm", algorithm,  # handled during error line
	 	 "auth", $sum_agency
	# 	 "commid", commid, 	 
	 	);
}

sub parse_errorline {
	my( $line ) = @_;

	$errors_unpack = "x2 A2 x A1 x A6 x A4 x A5 x A8 " .
			 "x A9 x A6 x A5 x A5 x A5 x A5 x A5";

	( $errs_velmodel, $errs_locator, $errs_wgt, $errs_rms, $errs_setime,
	  $errs_selat, $errs_selon, $errs_sedepth, $errs_smajax, 
	  $errs_sminax, $errs_sdepth, $errs_strike, $errs_agency ) =
			unpack( $errors_unpack, $line );

	$errs_velmodel =~ s/^\s+//;
	$errs_locator =~ s/^\s+//;
	$errs_wgt =~ s/^\s+//;
	$errs_rms =~ s/^\s+//;
	$errs_setime =~ s/^\s+//;
	$errs_selat =~ s/^\s+//;
	$errs_selon =~ s/^\s+//;
	$errs_sedepth =~ s/^\s+//;
	$errs_smajax =~ s/^\s+//;
	$errs_sminax =~ s/^\s+//;
	$errs_sdepth =~ s/^\s+//;
	$errs_strike =~ s/^\s+//;
	$errs_agency =~ s/^\s+//;

	@dborigin = dblookup( @dborigin, "", "origin", "orid", "$orid" );
	if( $errs_sedepth =~ /F/ ) {
		dbputv( @dborigin, "dtype", "g" );
	} else {
		dbputv( @dborigin, "dtype", "f" );
	}

	$algorithm = "$locatortrans{$errs_locator}:$velmodeltrans{$errs_velmodel}";
	$algorithm =~ s/\s//g;
	$algorithm =~ s/^://;
	dbputv( @dborigin, "algorithm", $algorithm );

	if( $errs_wgt =~ /OFF/i ) {
		if( $errs_rms > 0 ) {
			$sdobs = $errs_rms;
		} else {
			$sdobs = -1;
		}
	} else {
		$sdobs = -1;
	}

	# Note that the following error ellipsoid description for LocEq
	# generated errors is an approximation to the true error ellipsoid 
	# for this solution. It is based on the lat, lon, depth errors 
	# calculated at the earth's surface from LocEq; these errors are 
	# represented in the ellipsoid such that smajax and sminax always 
	# lie parallel to the earth's surface. Strike direction from Aki &
	# Richards, section 4.5. All definitions are intended to be consistent
	# with antelope css3.0 schema definitions for error ellipsoid.

	$sxx = $errs_selat ** 2 ;
	$syy = $errs_selon ** 2 ;
	$szz = $errs_sedepth ** 2 ;
	$stt = $errs_setime ** 2 ;	# errs_setime=0 for LocEq solns
	$sxy = 0 ;
	$sxz = 0 ;
	$syz = 0 ;
	$stx = 0 ;
	$sty = 0 ;
	$sxy = 0 ;

	if( $errs_selat >= $errs_selon ) {
		$smajax = $errs_selat ;
		$sminax = $errs_selon ;
		$strike = 0;
	} else {
		$smajax = $errs_selon ;
		$sminax = $errs_selat ;
		$strike = 90;
	}

	@dborigerr = dblookup( @db, "", "origerr", "", "" );

	dbaddv( @dborigerr,
	 	"orid", $orid,
	 	 "sxx", $sxx, 	 
	 	 "syy", $syy, 	 
	 	 "szz", $szz, 	 
	 	 "stt", $stt, 	 
	 	 "sxy", $sxy, 	 
	 	 "sxz", $sxz, 	 
	 	 "syz", $syz, 	 
	 	 "stx", $stx, 	 
	 	 "sty", $sty, 	 
	 	 "stz", $stz, 	 
	 	 "sdobs", $sdobs,
	 	 "smajax", $smajax, 	 
	 	 "sminax", $sminax, 	 
	 	 "strike", $strike, 	 
	 	 "sdepth", $errs_sedepth, 	 
	 	 "stime", $errs_setime 	 
	# 	 "conf", conf, 	 
	# 	 "commid", commid, 	 
	 	);
}

sub parse_pick {
	my( $line ) = @_;

	$pick_unpack = "x2 A5 x A1 A1 x A1 A2 A2 x A5 x A5 x " .
	  "A1 A7 x A1 A1 A1 x A5 x A4 x A5 x A7 x A3 x A3 x A5";
	
	( $pick_sta, $pick_inst, $pick_comp, $pick_nextday,
	$pick_hour, $pick_minute, $pick_second, $pick_timecorr,
	$pick_qual, $pick_phase, $pick_fm, $pick_use, $pick_wgt,
	$pick_ttres, $pick_locwgt, $pick_Pdelay, $pick_epidist,
	$pick_esaz, $pick_ema, $pick_agency ) =
		unpack( $pick_unpack, $line );

	$pick_sta =~ s/^\s+//;
	$pick_inst =~ s/^\s+//;
	$pick_comp =~ s/^\s+//;
	$pick_nextday =~ s/^\s+//;
	$pick_hour =~ s/^\s+//;
	$pick_minute =~ s/^\s+//;
	$pick_second =~ s/^\s+//;
	$pick_timecorr =~ s/^\s+//;
	$pick_qual =~ s/^\s+//;
	$pick_phase =~ s/^\s+//;
	$pick_fm =~ s/^\s+//;
	$pick_use =~ s/^\s+//;
	$pick_wgt =~ s/^\s+//;
	$pick_ttres =~ s/^\s+//;
	$pick_locwgt =~ s/^\s+//;
	$pick_Pdelay =~ s/^\s+//;
	$pick_epidist =~ s/^\s+//;
	$pick_esaz =~ s/^\s+//;
	$pick_ema =~ s/^\s+//;
	$pick_agency =~ s/^\s+//;

	@dbarrival = dblookup( @db, "", "arrival", "", "" );

	$arid = dbnextid( @dbarrival, "arid" );

	if( $pick_timecorr =~ /^\s*$/ ) {
		$pick_timecorr = 0;
	}

	$arrival_time = str2epoch( strdate( $origin_time ) . 
		" $pick_hour:$pick_minute:$pick_second" );

	$arrival_time += $pick_timecorr;

	if( $arrival_time < $origin_time ) {
		# handle next-day problem
		$arrival_time += 86400;
	}

	if( $pick_qual =~ /^\s*$/ ) {
		$pick_qual = "-";
	}

	if( $pick_fm =~ /[U+]/ ) {
		$pick_fm = "c.";
	} elsif( $pick_fm =~ /[D-]/ ) {
		$pick_fm = "d.";
	} else {
		$pick_fm = "-";
	}

	if( $pick_ema =~ /^\s*$/ ) {
		$pick_ema = -1;
	} elsif( $pick_ema < 0 ) {
		$pick_ema *= -1;
	}

	dbaddv( @dbarrival,
	 	"sta", $pick_sta,
	 	"time", $arrival_time,
	 	 "arid", $arid,
	 	 "jdate", yearday( $arrival_time ),
	# 	 "stassid", stassid, 	 
	# 	 "chanid", chanid, 	 
	 	 "chan", $chantrans{$pick_inst . $pick_comp},
	 	 "iphase", $pick_phase,
	# 	 "stype", stype, 	 
	# 	 "deltim", deltim, 	 
	# 	 "azimuth", azimuth, 	 
	# 	 "delaz", delaz, 	 
	# 	 "slow", slow, 	 
	# 	 "delslo", delslo, 	 
	 	 "ema", $pick_ema, 	 
	# 	 "rect", rect, 	 
	# 	 "amp", amp, 	 
	# 	 "per", per, 	 
	# 	 "logat", logat, 	 
	# 	 "clip", clip, 	 
	 	 "fm", $pick_fm, 	 
	# 	 "snr", snr, 	 
	 	 "qual", $pick_qual, 	 
	 	 "auth", $pick_agency
	# 	 "commid", commid, 	 
	 	);

	@dbassoc = dblookup( @db, "", "assoc", "", "" );

	dbaddv( @dbassoc,
	 	"arid", $arid,
	 	"orid", $orid,
	 	 "sta", $pick_sta,
	 	 "phase", $pick_phase, 	 
	# 	 "belief", belief, 	 
	 	 "delta", $pick_epidist / 111.195,
	# 	 "seaz", seaz, 	 
	 	 "esaz", $pick_esaz,
	 	 "timeres", $pick_ttres, 	 
	 	 "timedef", $pick_use !~ /x/i ? "d" : "n",
	# 	 "azres", azres, 	 
	# 	 "azdef", azdef, 	 
	# 	 "slores", slores, 	 
	# 	 "slodef", slodef, 	 
	# 	 "emares", emares, 	 
	 	 "wgt", $pick_locwgt,
	 	 "vmodel", $velmodeltrans{$errs_velmodel}
	# 	 "commid", commid, 	 
	 	);
}

sub parse_amplitude {
	my( $line ) = @_;

	$amp_unpack = "x2 A5 x A1 A1 x A1 A2 A2 x A5 x A5 x2 " .
		      "A8 x A6 x A12 x A1 x2 A6 A5 A1 x A5";
	
	( $amp_sta, $amp_inst, $amp_comp, $amp_nextday, $amp_hour,
	  $amp_minute, $amp_second, $amp_timecorr, $amp_phase,
	  $amp_period, $amp_ampl, $amp_units, $amp_magtype,
	  $amp_mag, $amp_use, $amp_agency ) = 
		 	unpack( $amp_unpack, $line );

	$amp_sta =~ s/^\s+//;
	$amp_inst =~ s/^\s+//;
	$amp_comp =~ s/^\s+//;
	$amp_nextday =~ s/^\s+//;
	$amp_hour =~ s/^\s+//;
	$amp_minute =~ s/^\s+//;
	$amp_second =~ s/^\s+//;
	$amp_timecorr =~ s/^\s+//;
	$amp_phase =~ s/^\s+//;
	$amp_period =~ s/^\s+//;
	$amp_ampl =~ s/^\s+//;
	$amp_units =~ s/^\s+//;
	$amp_magtype =~ s/^\s+//;
	$amp_mag =~ s/^\s+//;
	$amp_use =~ s/^\s+//;
	$amp_agency =~ s/^\s+//;

	@dbstamag = dblookup( @db, "", "stamag", "", "" );

	if( defined( $Magids{lc($amp_magtype)} ) ) {
		$magid = $Magids{lc($mag_magtype)};
	} else {
		$magid = -1;
	}

	$amp_arid = dbnextid( @dbstamag, "arid" );

	$amp_time = str2epoch( strdate( $origin_time ) . 
		" $amp_hour:$amp_minute:$amp_second" );

	if( $amp_time < $origin_time ) {
		# handle next-day problem
		$amp_time += 86400;
	}

	# Fake arrival since stamag requires arid
	# dbaddv( @dbarrival, 
	# 	"sta", $amp_sta,
	# 	"chan", $chantrans{$amp_inst . $amp_comp},
	# 	"time", $amp_time,
	# 	"arid", $amp_arid,
	# 	"iphase", $amp_phase,
	# 	"auth", $amp_agency
	# 	);

	dbaddv( @dbstamag,
	        "magid", $magid,
         	"sta", $amp_sta,
         	"arid", $amp_arid,
         	"orid", $orid,
	        "evid", $evid,
	#        "phase", $phase,
         	"magtype", lc($amp_magtype),
	        "magnitude", $amp_mag,
	#        "uncertainty", $uncertainty,
	        "auth", $amp_agency
	#        "commid", $commid,
        	);
}

sub parse_magnitude {
	my( $line ) = @_;

	$mag_unpack = "x2 A1 A6 A5 x2 A4 x2 A3 x50 A5";

	( $mag_prime, $mag_magtype, $mag_magval, $mag_serr, 
	  $mag_nsta, $mag_agency ) = unpack( $mag_unpack, $line );

	$mag_prime =~ s/^\s+//;
	$mag_magtype =~ s/^\s+//;
	$mag_magval =~ s/^\s+//;
	$mag_serr =~ s/^\s+//;
	$mag_nsta =~ s/^\s+//;
	$mag_agency =~ s/^\s+//;

	if( $mag_magtype =~ /^\s*$/ ) {
		# Assume this based on PGC instructions
		$mag_magtype = "ML"; 
	}

	@dbnetmag = dblookup( @db, "", "netmag", "", "" );

	if( defined( $Magids{lc($mag_magtype)} ) ) {
		$magid = $Magids{lc($mag_magtype)};
	} else {
		$magid = dbnextid( @dbnetmag, "magid" );
		$Magids{lc($mag_magtype)} = $magid;
	}

	if( $mag_serr =~ /^\s*$/ ) {
		$mag_serr = -1;
	}

	if( $mag_nsta =~ /^\s*$/ ) {
		$mag_nsta = -1;
	}

	dbaddv( @dbnetmag,
         	"magid", $magid,
	#        "net", $net,
	        "orid", $orid,
	        "evid", $evid,
	        "magtype", lc($mag_magtype),
	        "nsta", $mag_nsta,
	        "magnitude", $mag_magval,
	        "uncertainty", $mag_serr,
	        "auth", $mag_agency
	#        "commid", $commid,
        	);
}

sub parse_groundmotion {
	my( $line ) = @_;
	
	# Not implemented ( no documentation )
}

sub pgc_pickfile_handler {
	my( $message, $pfarray ) = @_;

	$database = %{$pfarray}->{database};
	%chantrans = %{%{$pfarray}->{chantrans}};
	%velmodeltrans = %{%{$pfarray}->{velmodeltrans}};
	%locatortrans = %{%{$pfarray}->{locatortrans}};

	@db = dbopen( $database, "r+" );

	while( $line = shift( @{$message->body()} ) ) {

		chomp( $line );
		
		if( $line =~ /^\s*$/ ) { next; }
		if( $line =~ /^C.*/ ) { next; }
		if( $line =~ /^S.*/ ) { &parse_solution( $line ); }
		if( $line =~ /^  [A-Z].*/ ) { &parse_pick( $line ); }
		if( $line =~ /^A.*/ ) { &parse_amplitude( $line ); }
		if( $line =~ /^E.*/ ) { &parse_errorline( $line ); }
		if( $line =~ /^M.*/ ) { &parse_magnitude( $line ); }
		if( $line =~ /^G.*/ ) { &parse_groundmotion( $line ); }
	}
}

1;

