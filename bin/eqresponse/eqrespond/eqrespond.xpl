# eqrespond
#
# Driver script for earthquake information releases 
#
# K. Lindquist
# Geophysical Institute
# University of Alaska, Fairbanks
# June 1998

use Datascope;

use Getopt::Std;
require "winding.pl";

$Pf = "eqrelease.pf";

sub choose_database_row {
	my( $dbin_name ) = @_;
	my( @tables );

	if( $dbin_name eq "-" ) {

		@db = dbopen_table( $dbin_name, "r" );

		$Dbname = dbquery( @db, "dbDATABASE_NAME" );

		@tables = dbquery( @db, "dbVIEW_TABLES" );

		if( grep( /event/, @tables ) && grep( /origin/, @tables ) ) {
			
			@db = dbsubset( @db, "origin.orid == prefor" );

			@db = choose_unique_origin_row( @db );

		} elsif( grep( /event/, @tables ) ) {

			@dborigin = dblookup( @db, "", "origin", "", "" );

			@db = dbjoin( @db, @dborigin );

			@db = dbsubset( @db, "origin.orid == prefor" );

			@db = choose_unique_origin_row( @db );

		} elsif( grep( /origin/, @tables ) ) {

			@db = choose_unique_origin_row( @db );

		} else {
			MyDie( "No origin information passed to $Prog_name" );
		}
		
	} elsif( $Orid ) {

		$Dbname = `abspath $dbin_name`;
		chop( $Dbname );

		@db = dbopen( $Dbname, "r" );
		@db = dblookup( @db, "", "origin", "", "" );

		$yes = dbquery( @db, "dbTABLE_PRESENT" );
		if( ! $yes ) {
			MyDie( "Origin table not present in $dbin_name" );
		}

		@db = dbsubset( @db, "orid == $Orid" );
		$nrecs = dbquery( @db, "dbRECORD_COUNT" );

		if( $nrecs != 1 ) {
			MyDie( "Couldn't find orid $Orid in $dbin_name" );
		}

		$db[3] = 0;

	} elsif( $Evid ) {

		$Dbname = `abspath $dbin_name`;
		chop( $Dbname );

		@db = dbopen( $Dbname, "r" );
		@dbevent = dblookup( @db, "", "event", "", "" );
		
		$yes = dbquery( @dbevent, "dbTABLE_PRESENT" );
		if( ! $yes ) {
			MyDie( "Event table not present in $dbin_name" );
		}

		@dbscratch = dblookup( @dbevent, "", "event", "", "dbSCRATCH" );
		dbputv( @dbscratch, "evid", $Evid );

		@records = dbmatches( @dbscratch, @dbevent, "myhook", "evid" );

		if( $records[0] < 0 ) {
			MyDie( "Couldn't find evid $Evid in $dbin_name" );
		} else {
			$expr = "evid == $Evid";
			@dbevent = dbsubset( @dbevent, $expr );
		}

		@dborigin = dblookup( @db, "", "origin", "", "dbALL" );

		@db = dbjoin( @dbevent, @dborigin );

		@db = dbsubset( @db, "origin.orid == prefor" );

		@db = choose_unique_origin_row( @db );

	} else {

		$Dbname = `abspath $dbin_name`;
		chop( $Dbname );

		@db = dbopen( $Dbname, "r" );
		@db = dblookup( @db, "", "event", "", "" );
	
		$nevents = dbquery( @db, "dbRECORD_COUNT" );
	
		if( $nevents == 0 ) {
	
			@db = dblookup( @db, "", "origin", "", "" );
	
			@db = choose_unique_origin_row( @db );

		} elsif( $nevents > 1 ) {
	
			MyDie( "Multiple events in $Dbname" );
	
		} else {
	
			@dbtemp = dblookup( @db, "", "origin", "", "" );
			@db = dbjoin( @db, @dbtemp );
			@db = dbsubset( @db, "origin.orid == prefor" );

			@db = choose_unique_origin_row( @db );
		}
	}

	return @db;
}

sub choose_unique_origin_row {
	my( @db ) = @_;

	$norigins = dbquery( @db, "dbRECORD_COUNT" );

	if( $norigins <= 0 ) {

		MyDie( "origin row selection failed for $Dbname" );

	} elsif( $norigins == 1 ) {

		$db[3] = 0;

	} else {

		$db[3] = 0;
		my( $unique_orid ) = dbgetv( @db, "origin.orid" );

		for( $db[3] = 1; $db[3] < $norigins; $db[3]++ ) {

			if( dbgetv( @db, "origin.orid" ) != $unique_orid ) {
				MyDie( "multiple origins in $Dbname view" );
			}
		}

		$db[3] = 0;
	}

	return @db;
}

sub save_subset_database {
	my( @dborigin ) = @_;

	@dbass = dblookup( @dborigin, "", "assoc", "", "" );
	@dbarr = dblookup( @dborigin, "", "arrival", "", "" );

	@db = dbjoin( @dborigin, @dbass );

	my( $nrecs ) = dbquery( @db, "dbRECORD_COUNT" );
	if( $nrecs <= 0 ) {
		dbunjoin( @dborigin, $subset_database );
		return;
	}

	@db = dbjoin( @db, @dbarr );

	$nrecs = dbquery( @db, "dbRECORD_COUNT" );
	if( $nrecs > 0 ) {
		dbunjoin( @db, $subset_database );
	} else {
		dbunjoin( @dborigin, $subset_database );
	}
}

sub epoch_to_yyyymmddhhmmss {
	local( $epoch ) = @_;

	$result = epoch2str( $epoch, "%Y%m%d%H%M%S" );
	return $result;
}

sub establish_correct_cwd {
	local( $event_time ) = @_;

	$base_release_dir =~ s@HOME@$ENV{'HOME'}@;

	if( -e $base_release_dir && ! -d $base_release_dir ) {

		$message = "Intended target directory $base_release_dir ";
		$message .= "but is not a directory";
		MyDie( $message );

	} elsif( ! -e $base_release_dir ) {

		mkdir( $base_release_dir, 0755 );
		if( ! -e $base_release_dir ) {
			MyDie( "Failed to make $base_release_dir" ); 
		}
		chdir( $base_release_dir );

	} elsif( ! -W $base_release_dir ) {

		MyDie( "Directory $base_release_dir is not writable" );

	} else {

		chdir( $base_release_dir );
	}

	$event_timestr = epoch_to_yyyymmddhhmmss( $event_time );
	$! = 0;
	mkdir( "$event_timestr", 0755 );
	if( $! ) {
		MyDie( "Failed to make $base_release_dir/$event_timestr: $!" ); 
	}
	chdir( $event_timestr );

	$release_dir = "$base_release_dir/$event_timestr";
}

sub choose_magnitude {
	local( @db ) = @_;

	( $ml, $mb, $ms ) = dbgetv( @db, "ml", "mb", "ms" );

	if( $ml != -999.00 ) {

		return $ml, "ML";

	} elsif( $mb != -999.00 ) {

		return $mb, "mb";

	} elsif( $ms != -999.00 ) {

		return $ms, "Ms";

	} else {
		MyDie( "$Prog_name: no magnitude available" );
	}
}

sub local_time {
	local( $mytime ) = @_;

	$result = epoch2str( $mytime, "%I:%M %p %Z", $ENV{'TZ'} );
	$result =~ s/^0//;
	$result =~ s/\s*$//;

	return $result;
}

sub describe_strength {
	local( $mymag ) = @_;

	if ( $mymag >= 3.0 && $mymag < 4.0 ) {

		return "a minor earthquake";

	} elsif ( $mymag >= 4.0 && $mymag < 5.0 ) {

		return "a light earthquake";

	} elsif ( $mymag >= 5.0 && $mymag < 6.0 ) {

		return "a moderate earthquake";

	} elsif ( $mymag >= 6.0 && $mymag < 7.0 ) {

		return "a strong earthquake";

	} elsif ( $mymag >= 7.0 && $mymag < 8.0 ) {

		return "a major earthquake";

	} elsif ( $mymag >= 8.0 ) {

		return "a great earthquake";

	} else {
		
		return "an earthquake";
	}
}

sub compass_from_azimuth {
	local( $azimuth ) = @_;

	while( $azimuth < 0. ) { $azimuth += 360.; };
	while( $azimuth > 360. ) { $azimuth -= 360.; };

	if( $azimuth >= 348.75 || $azimuth < 11.25 ) {

		return "N";		# 0.00

	} elsif( $azimuth >= 11.25 && $azimuth < 33.75 ) {

		return "NNE";		# 22.50

	} elsif( $azimuth >= 33.75 && $azimuth < 56.25 ) {

		return "NE";		# 45.00	

	} elsif( $azimuth >= 56.25 && $azimuth < 78.75 ) {

		return "ENE";		# 67.50	

	} elsif( $azimuth >= 78.75 && $azimuth < 101.25 ) {

		return "E";		# 90.00	

	} elsif( $azimuth >= 101.25 && $azimuth < 123.75 ) {

		return "ESE";		# 112.50	

	} elsif( $azimuth >= 123.75 && $azimuth < 146.25 ) {

		return "SE";		# 135.00	

	} elsif( $azimuth >= 146.25 && $azimuth < 168.75 ) {

		return "SSE";		# 157.50	

	} elsif( $azimuth >= 168.75 && $azimuth < 191.25 ) {

		return "S";		# 180.00	

	} elsif( $azimuth >= 191.25 && $azimuth < 213.75 ) {

		return "SSW";		# 202.50	

	} elsif( $azimuth >= 213.75 && $azimuth < 236.25 ) {

		return "SW";		# 225.00 	

	} elsif( $azimuth >= 236.25 && $azimuth < 258.75 ) {

		return "WSW";		# 247.50	

	} elsif( $azimuth >= 258.75 && $azimuth < 281.25 ) {

		return "W";		# 270.00	

	} elsif( $azimuth >= 281.25 && $azimuth < 303.75 ) {

		return "WNW";		# 292.50	

	} elsif( $azimuth >= 303.75 && $azimuth < 326.25 ) {

		return "NW";		# 315.00	

	} elsif( $azimuth >= 326.25 && $azimuth < 348.75 ) {

		return "NNW";		# 337.50	
	} 

	MyDie( "Faulty logic in compass_from_azimuth subroutine" );
}

sub set_distances {

	local( $lat, $lon, $n ) = @_;

	$distances = "Distance to nearby locations:\n\n";

	@dbp = dbopen( $place_database, "r" );
	@dbp = dblookup( @dbp, "", "places", "", "" );
	@dbp = dbsort( @dbp, "distance(lat,lon,$lat,$lon)" );

	$nrecs = dbquery( @dbp, "dbRECORD_COUNT" );

	$nrows = $nrecs > $n ? $n : $nrecs;

	for( $dbp[3] = 0; $dbp[3] < $nrows; $dbp[3]++ ) { 

		$place = dbgetv( @dbp, "place" );

		$dist_deg = dbex_eval( @dbp, "distance(lat,lon,$lat,$lon)" );
		$dist_km = $dist_deg * 111.195;
		$dist_mi = $dist_km / 1.6;

		$azimuth = dbex_eval( @dbp, "azimuth(lat,lon,$lat,$lon)" );

		$compass = &compass_from_azimuth( $azimuth );

		$distances .= sprintf( "\t%4.0lf km (%4.0lf miles) %3s of %s\n",
					$dist_km, $dist_mi, $compass, $place );
	}

	dbclose( @dbp );

	$distances .= "\n";

	return $distances;
}

sub nearest_big_city {

	local( $lat, $lon ) = @_;

	@dbp = dbopen( $common_place_database, "r" );
	@dbp = dblookup( @dbp, "", "places", "", "" );
	@dbp = dbsort( @dbp, "distance(lat,lon,$lat,$lon)" );

	$nrecs = dbquery( @dbp, "dbRECORD_COUNT" );

	$dbp[3] = 0;

	$place = dbgetv( @dbp, "place" );

	$dist_deg = dbex_eval( @dbp, "distance(lat,lon,$lat,$lon)" );
	$dist_km = $dist_deg * 111.195;
	$dist_mi = $dist_km / 1.6;

	$azimuth = dbex_eval( @dbp, "azimuth(lat,lon,$lat,$lon)" );

	$compass = &compass_from_azimuth( $azimuth );

	my( $nearest_city ) = sprintf( "%4.0lf miles %3s of %s",
				$dist_mi, $compass, $place );

	$nearest_city =~ s/^\s+//;

	dbclose( @dbp );

	return $nearest_city;
}

sub MyDie {
	my( $msg ) = @_;

	$msg = "$Prog_name: $msg. Bye!";

	if( $Graphic_death ) {
		$cmd = $Helpers{"tkshow_message"};
		$cmd .= " $msg";
		system( $cmd );
		exit( 1 );
	} else {
		$msg .= "\n";
		die( $msg );
	}
}

sub set_vital_statistics {

	local( @db ) = @_;

	($lat, $lon, $depth_km, 
	$origin_time, $orid) = dbgetv( @db, "lat",
				"lon", "depth", "time", "orid" );
	
	if( $depth_km < 0 ) { $depth_km = 0.; }

	$min = $lat;
	$min =~ s/\d+\./0./;
	$min *= 60 * ( $lat > 0 ? 1. : -1. );
	$lat_string =  sprintf( "%02d %s %02d\'",
			$lat * ( $lat > 0 ? 1. : -1. ),
			$lat > 0 ? 'N' : 'S',
			$min + 0.5 );

	$min = $lon;
	$min =~ s/\d+\./0./;
	$min *= 60 * ( $lon > 0 ? 1. : -1. );
	$lon_string =  sprintf( "%02d %s %02d\'",
			$lon * ( $lon > 0 ? 1. : -1. ),
			$lon > 0 ? 'E' : 'W',
			$min + 0.5 );

	$local_time = &local_time( $origin_time );

	$depth_mi = $depth_km / 1.6;

	$depth_km = sprintf( "%1.0f", $depth_km );
	$depth_mi = sprintf( "%1.0f", $depth_mi );

	( $mag, $magtype ) = &choose_magnitude( @db );

	$mag = sprintf( "%3.1f", $mag );

	$gregion = dbex_eval( @db, "grname(lat,lon)" );

}

sub describe_when {

	$when = epoch2str( $origin_time, "%m/%d/%Y %H:%M" );
	$when_HMS = epoch2str( $origin_time, "%m/%d/%Y %H:%M:%S" );
}

sub find_day_name {
	
	$day_name = epoch2str( $origin_time, "%A, %B %o", $ENV{'TZ'} );

	return $day_name;
}

sub describe_where {

	@dbregions = dbopen( $region_phrases_database, 'r' ); 
	@dbregions = dblookup( @dbregions, "", "regions", "", "" );
	@regions = get_containing_regions( @dbregions, $lat, $lon );

	if( ! defined( $where = shift( @regions ) ) ) {

		$where = "(" . grname( $lat, $lon ) . ")";
	}
}

sub describe_felt_info {

	print "\n$Prog_name:\tLaunching felt_report_tool...\n\n";

	$cmd = "$Helpers{'felt_report_tool'} $subset_database ";

	$felt =`$cmd`;

	if( $felt eq "" ) {
		MyDie( "Abandoning response to earthquake at user request" );
	}

	$felt = `echo "$felt" | tr '\012' ' ' | fmt -w 60`;
	$felt =~ s/\s+$//;
	if( $felt !~ /\.$/ ) { $felt .= "."; }
	$felt .= "\n\n";
}

sub summarize_earthquake {

	$strength = &describe_strength( $mag );

	$day_name = &find_day_name();

	$description = <<"	EODESCRIPTION";
	The Alaska Earthquake Information Center located $strength
	that occurred on $day_name at $local_time $where. This earthquake had a
	preliminary magnitude of $mag and was located at a depth
	of about $depth_mi miles ($depth_km km). The magnitude and location may
	change slightly as additional data are received and processed.
	
	EODESCRIPTION
	
	$description =~ s/^\t//;
	$description =~ s/\n\t/\n/g;

	$description = `echo "$description" | fmt -w 60`;
	chop( $description );
}

sub write_voicemail_message {

	my( $strength ) = &describe_strength( $mag );

	my( $day ) = epoch2str( $origin_time, "%A, %B %e, %Y", $ENV{'TZ'} );

	my( $nearest_city ) = &nearest_big_city( $lat, $lon );

	$myfelt = $felt;
	$myfelt =~ s/\s+$//;

	$voicemail_message = <<"	EOVOICEMAIL";
	The Alaska Earthquake Information Center located $strength
	that occurred on $day at $local_time. The epicenter was $nearest_city,
	$where. This earthquake had a preliminary magnitude of $mag and was
	located at a depth of about $depth_mi miles. $myfelt For more
	information, please call during regular business hours, or visit 
	our web site at www.aeic.alaska.edu. If you wish, leave your name,
	phone number, and message at the tone. Thank you.
	
	EOVOICEMAIL
	
	$voicemail_message =~ s/^\t//;
	$voicemail_message =~ s/\n\t/\n/g;

	$voicemail_message = `echo "$voicemail_message" | fmt -w 60`;
	chop( $voicemail_message );

	open( M, ">$voicemail_file" );

	print M $voicemail_message;

	close( M );
}

sub describe_nearest {

	$distances = &set_distances( $lat, $lon, $num_nearest );
}

sub describe_vitals {

	$vitals = <<"	EOVITALS";
	Preliminary earthquake parameters:
    	 
		Origin Time (UT):		$when_HMS
		Latitude:			$lat_string
		Longitude:			$lon_string
		Depth:				$depth_km km
		Magnitude:			$magtype $mag
	
	EOVITALS

	$vitals =~ s/^\t//;
	$vitals =~ s/\n\t/\n/g;
}

sub make_message_trailer {

	$trailer = <<"	EOTRAILER";
	The Alaska Earthquake Information Center (AEIC) monitors
	earthquakes in Alaska and provides earthquake information
	to the citizens and public officials of Alaska.  The Center
	is a cooperative program of the Geophysical Institute of
	the University of Alaska and the U.S. Geological Survey and
	is located at the Geophysical Institute in Fairbanks with
	the Alaska State Seismologist's Office.
	
	Additional information may be obtained from:
	AEIC, Geophysical Institute, Fairbanks, AK, 99775-7320
	Phone: (907) 474-7320 FAX: (907) 474-5618
	Email: roger\@giseis.alaska.edu or natasha\@giseis.alaska.edu
	 
	NEIC, Denver, CO, 80225
	Phone: (303) 273-8500 FAX: (303) 273-8450
	EOTRAILER
	
	$trailer =~ s/^\t//;
	$trailer =~ s/\n\t/\n/g;
}

sub set_mail_subject {
	$subject =
	   "Subject: AEIC EQ RELEASE: $when UT, $mag $magtype $gregion\n\n";
}

sub write_fm_macro {

	$makertextcode = "$release_dir/$makertextfile";
	$makertextcode =~ s@/@\\/@g;

	$fmrelease_filecode = "$release_dir/$fmrelease_file";
	$fmrelease_filecode =~ s@/@\\/@g;

	$fmpdf_filecode = "$release_dir/$fmpdf_file";
	$fmpdf_filecode =~ s@/@\\/@g;

	$map_epsi_filecode = "$release_dir/$map_epsi_file";
	$map_epsi_filecode =~ s@/@\\/@g;

	$date = `date`;

	$printmacro = <<"	EOPRINTMACRO";

	\\!fp/START_DIALOG ^/Tab /Tab
	/Tab \\s0 /Down /Return
	/Tab /Tab /Tab
	/Tab /End ^u100
	/Tab \\s0 /Return 
	/Tab 1
	/Tab 1
	/Tab 1
	/Tab 0
	/Tab 0
	/Tab 0 /Tab /Tab
	/Tab 0
	/Tab \\s0 /Down /Down /Down /Return
	/Tab \\s0 /Down /Down /Return
	/End ^u$printer
	/Tab 0 /Tab
	/Tab 0 /Tab
	/Tab /Return /END_DIALOG

	EOPRINTMACRO

	if( $opt_n ) { $printmacro = "" };

	$fmmacro = <<"	EOMACRO";
	Macros saved on $date
	eq release macro:
	<Macro Macro2
	<Label Macro2>
	<Trigger ^1>
	<TriggerLabel ^1>
	<Definition \\!fa/START_DIALOG ^/Tab /End
		^u$fmrelease_filecode/Return /END_DIALOG
	\\!fif/START_DIALOG ^/Tab /End
	^u$makertextcode/Tab /Tab /Tab \\s/Tab
	/Return /END_DIALOG /START_DIALOG ^/Tab /Tab /Return /END_DIALOG
	/START_DIALOG ^/Tab /Tab \\s/Tab /Tab /Tab /Return /END_DIALOG
	\\!fif/START_DIALOG ^/Tab /End ^u$map_epsi_filecode
	/Tab /Tab /Tab \\s/Tab /Return /END_DIALOG
	\\!go/START_DIALOG ^/Tab /Tab /Tab /End ^u3.375/Tab /End ^u4.694
	/Tab /Tab /Tab /Tab /Tab /Tab /Tab /Return /END_DIALOG \\!fs
	>
	<Mode NonMath>>
	
	eq closeout macro:
	<Macro Macro3
	<Label Macro3>
	<Trigger ^3>
	<TriggerLabel ^3>
	<Definition \\!fs
	
	$printmacro;

	\\!fp/START_DIALOG ^/Tab /Tab
	/Tab /Tab /Tab /Tab
	/Tab /End ^u100
	/Tab \\s0 /Return
	/Tab 1
	/Tab 1
	/Tab 1
	/Tab 0
	/Tab 0
	/Tab 0 /Tab /Tab
	/Tab 0
	/Tab \\s0 /Down /Down /Down /Return
	/Tab \\s0 /Down /Return
	/End ^u$fmpdf_filecode
	/Tab 0 /Tab
	/Tab 0 /Tab
	/Tab /Return /END_DIALOG
	\\!fs
	\\!fc
	>
	<Mode NonMath>>

	EOMACRO

	$fmmacro =~ s/^\t//;
	$fmmacro =~ s/\n\t/\n/g;

	open( M, ">fmMacros" );

	print M $fmmacro;

	close( M );
}

sub write_email_file {

	open( M,">$email_file" );

	print M $subject;
	print M $description;
	print M $felt;
	print M $distances;
	print M $vitals;
	print M $trailer;
	
	close( M );
}

sub write_makertext_file {

	$description =~ tr/\n/ /;
	$felt =~ tr/\n/ /;
	$vitals =~ s/\(UT\):/(UT):\t/;
	$vitals =~ s/Depth:\t/Depth:/;

	open( M, ">$makertextfile" );

	print M $description;
	print M $felt . "\n\n";
	print M $distances;
	print M $vitals;

	close( M );
}

sub identify_framemaker_pid {
	my( $chosen, $pid );

	open( P, "/usr/bin/ps -e -o'pid args' | grep maker | grep -v grep |" );
	@candidates = <P>;
	close( P );

	chop( @candidates );

	if( $#candidates < 0 ) {

		print STDERR "Can't find framemaker running on system.";
		return 0;

	} elsif( $#candidates == 0 ) {

		$chosen = $candidates[0];

	} else {

		$chosen = "";
		foreach $candidate ( @candidates ) {

			$args = $candidate;
			$args =~ s/^\s*[0-9]+\s+\S+\s*//;

			$len = length( $maker_args ) < length( $args ) ?
				length( $maker_args ) : length( $args );

			# Presume our framemaker will show arguments:
			if( $args ne "" &&	
			    $args eq substr( $maker_args, 0, $len ) ) {
				
				$chosen = $candidate;
			}
		}
		unless( $chosen ) {
			print STDERR
			  "Can't find  correct framemaker running on system.";
			return 0;
		}
	}

	$chosen =~ /^\s*([0-9]+)/;
	$pid = $1;

	return $pid;
}

sub file_being_used {
	local( $filename ) = @_;

	if( ! -e $filename ) { return ""; }

	open( U, "/usr/sbin/fuser $filename 2>&1 | " );
	$Users = join(" ", <U> );
	close( U );

	$Users =~ s/^$filename:\s*//;

	if( $Users eq "" ) { return ""; }

	$userpid = ( split( /\s+/, $Users ) ) [0];
	$userpid =~ s/[a-z]$//;

	$user = `/usr/bin/ps -o'comm' -p $userpid | sed '1d'`;
	chop( $user );

	if( $user eq "" ) { return ""; }

	$user = `/usr/bin/basename $user`;
	chop( $user );

	return $user;

}

sub framemaker_not_done {
	my( $lastfile );
	my( $pname );

	$lastfile = "$release_dir/$fmpdf_file";

	my( $basename ) = `/usr/bin/basename $lastfile`;
	chop( $basename );

	if( ! -e $lastfile ) {
		return 1;
	} elsif( -z $lastfile ) {
		return 1;
	} else {
		$pname = file_being_used( $lastfile );
		if( $pname ) {
			print "Process $pname is still working on $basename ...\n"; 	
			return 1;
		} else {
			return 0;
		}
			
	}
}

sub prepare_eva_log {

	$evatime = epoch2str( $origin_time, "%a %b %d %H:%M:%S %Z", $ENV{'TZ'} );
	
	$user = `whoami`;
	chop( $user );

	open( E, ">$release_dir/$eva_log_file" );
	print E "c*  $evatime $user\n";
	print E "$magtype $mag earthquake. Not felt. No release.\n";
	close( E );
}

sub create_earthquake_map {

	print "\n$Prog_name:\tLaunching thumbnail-map generator...\n\n";

	system( "$Helpers{'dbmaprelease'} $Dbname $orid $map_range" );
	system( "$Helpers{'alchemy'} dbmapevents.ps -Zm1 -Zc1 -e3000 -o" );
	system( "/bin/cp dbmapevents.epsi $map_epsi_file" );

}

sub create_gif_release {
	
	my( $alchemy_options ) = "-Z+ -Xb791p -Yb985p -Za9 -o";

	$cmd = "$Helpers{'alchemy'} -g $alchemy_options $release_dir/$fmpdf_file ";
	$cmd .= "$release_dir/$gif_release_file";
	system( $cmd );
}

sub prepare_custom_release_pffile {

	open(P,">$release_dir/eqrelease.pf");

	print P "gif_release_file $gif_release_file\n";
	print P "fmpdf_file $fmpdf_file\n";

	close( P );
}

sub map_workfile_names {
	my( $origin_time ) = @_;

	@keys = ( "email_file",
		  "makertextfile",
		  "fmrelease_file",
		  "fmpdf_file",
		  "gif_release_file",
		  "map_epsi_file",
		  "eva_log_file" );

	foreach $key ( @keys ) {
		$$key = epoch2str( $origin_time, "$$key" );
	}
}

sub initialize {

	@myhelpers = ( "alchemy",
		       "eqrelease_distributor",
		       "dbmaprelease",
		       "Framemaker",
		       "felt_report_tool",
		       "update_finger" );

	if( ! defined( pfget( $Pf, "Helpers" ) ) ) {
		die( "Helpers array undefined in parameter file.\n" );
	} else {
		my( $hashref ) = pfget( $Pf, "Helpers" );
		%Helpers = %$hashref;
	}

	$Graphic_death = 0 unless( defined( $Helpers{"tkshow_message"} ) );
	$Graphic_death = 0 unless( -x $Helpers{"tkshow_message"} );

	foreach $helper ( @myhelpers ) {

		$message = "Helper application \'$helper\' is not defined ";
		$message .= "in parameter file";
		MyDie( $message ) unless( defined( $Helpers{$helper} ) );

		MyDie( "Helper application $Helpers{$helper} is not available")
		 unless( -x "$Helpers{$helper}" );
	} 

	if( ! defined( pfget( $Pf, "bad_framemaker_displays" ) ) ) {
		die( "bad_framemaker_displays array undefined " .
		     "in parameter file.\n" );
	} else {
		my( $hashref ) = pfget( $Pf, "bad_framemaker_displays" );
		%bad_framemaker_displays = %$hashref;
	}

	@myparameters = (
			 "email_release_recipient",
			 "web_directory",
			 "EVA_LOG",
			 "printer",
			 "place_database",
			 "common_place_database",
			 "region_phrases_database",
			 "contact_database",
			 "calldown_database",
			 "maker_template",
			 "number_of_contacts",
			 "num_nearest",
			 "map_range",
			 "stock_felt_report",
			 "base_release_dir",
			 "email_file",
			 "voicemail_file", 
			 "makertextfile",
			 "fmrelease_file",
			 "fmpdf_file",
			 "gif_release_file",
			 "map_epsi_file",
			 "eva_log_file",
			 "subset_database",
			 "voicemail_instructions",
			 "maintainer"
			);

	
	foreach $param ( @myparameters ) {
		if( ! defined( $$param = pfget( $Pf, "$param" ) ) ) {
			MyDie( "$param undefined in parameter file" );
		} elsif( $$param eq "" ) {
			MyDie( "$param value empty in parameter file" );
		} 
	}

	# Suppress warnings about single use of some variables
	use vars ('$num_nearest', '$map_range', '$eva_log_file',
		  '$email_file', '$voicemail_file', '$common_place_database',
		  '$printer', '$maker_template', '$place_database' );

	if( defined $ENV{'PFPATH'} ) {
		$ENV{'PFPATH'} .= ":.";
	} else {
		$ENV{'PFPATH'} = ".";
	}

}

$Prog_name = $0;
$Prog_name =~ s@.*/@@;
$Usage = "Usage: $Prog_name [-g] [-n] [-o orid] [-e evid] dbname\n";

$opt_n = "";
$opt_o = "";
$opt_e = "";
$opt_g = "";
if( ! getopts('gno:e:') || $#ARGV != 0 ) {
	die( "$Usage" );

} else {
	$dbin_name = $ARGV[0];

	if( $opt_g ) { $Graphic_death = 1; } else { $Graphic_death = 0; }
	if( $opt_o ) { $Orid = $opt_o; } else { $Orid = ""; }
	if( $opt_e ) { $Evid = $opt_e; } else { $Evid = ""; }

	if( ( $opt_o || $opt_e ) && $dbin_name eq "-" ) {
		$message = "$Prog_name: -o/-e options incompatible with ";
		$message .= "taking database from stdin.\n";
		die( $message );
	}
}

&initialize();

@db = &choose_database_row( $dbin_name );

set_vital_statistics( @db );

establish_correct_cwd( $origin_time );

map_workfile_names( $origin_time );

save_subset_database( @db );

&describe_when();

&describe_where();

&set_mail_subject();

&describe_felt_info();

&summarize_earthquake();

&write_voicemail_message();

&describe_nearest();

&describe_vitals();

&make_message_trailer();

&write_fm_macro();

&write_email_file();

&write_makertext_file();

&create_earthquake_map();

system( "/bin/rm -f $fmrelease_file" );

if( $avoid = defined( $bad_framemaker_displays{$ENV{HOST}} ) ) {

	$maker_display = `set_display -a $avoid`;
} else {

	$maker_display = `set_display`;
}
$maker_args = "-f $maker_display $maker_template";

if( $maker_pid = fork ) {

	# The Framemaker launches another process, then exits, 
	#  circumventing a standard waitpid approach

	sleep( 3 ); # allow Framemaker to start up

	$maker_pid = &identify_framemaker_pid();

	while( &framemaker_not_done ) {
		sleep 1; 
	}
	
	if( $maker_pid ) {
		$SIGTERM = 15;
		kill $SIGTERM, $maker_pid;
	} else {
		print "\n\nPlease Exit out of Framemaker\n\n";
	}

} elsif( defined $maker_pid ) {

	print "\n$Prog_name:\tLaunching Framemaker...\n\n";

	exec( "$Helpers{'Framemaker'} $maker_args" );

} else {

	MyDie( "Cannot exec $Helpers{'Framemaker'}" );
}

&create_gif_release();

&prepare_eva_log();

&prepare_custom_release_pffile();

print "\n$Prog_name:\tLaunching eqrelease_distributor...\n\n";

system( "$Helpers{'eqrelease_distributor'} $opt_n" );

print( "\n\n$Prog_name: Successfully finished earthquake response.\n\n" );

exit( 0 );
