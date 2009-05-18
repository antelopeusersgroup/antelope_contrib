sub construction_mail_handler {
	use Mail::Mailer;
	use sysinfo;
	my $sta    = undef;
	my $year   = undef;
	my $month  = undef;
	my $day    = undef;
	my $lddate = undef;
	my $elev   = undef;
	my $unit   = undef;
	my $lat    = undef;
	my $lon    = undef;
	my $epoch  = undef;
	my $yday   = undef;
;
	my $old_elev   = undef;
	my $old_lat    = undef;
	my $old_lon    = undef;
	my $old_yday   = undef;

	my $results   = undef;
	my $mailer    = undef;
	my $mail_body = undef;
	my $errors    = undef;
	my $update    = undef;
	my $record    = undef;
	my $mail_cc   = undef;
	my $mail_from = undef;
	my $mail_sub  = undef;
	my $mail_to   = undef;

	my $rq	= undef;
	my @hd	= undef;
	my @db	= undef;

	my( $message, $pfarray ) = @_;

	my( @body ) = @{$message->body()};
	my $subject = $message->head->get("Subject");
	my $from = $message->get("From");
	my $date = $message->head->get("Date");

	if ( %{$pfarray}->{log} ) {
		print "-------NEW---------";
		print $from;
		print $date;
		print $subject;
		print @body;
		print "-------END---------";
	}

	#
	#Start body of 
	#report e-mail
	#
	$mail_body  = "Report from $0 \n"; 
	$mail_body .= "Date: ". localtime(). "\n" ;
	$mail_body .= "On system: ". my_hostname(). "\n" ;
	$mail_body .= "Running OS: ". my_os(). "\n" ;
	$mail_body .= "e-mail    from: $from \n" ;
	$mail_body .= "e-mail    date: $date \n" ;
	$mail_body .= "e-mail Subject: $subject \n";

	foreach(@body) {	
		if( /.*Station Code\s*(=|:)/i && ! $sta ) {
				$mail_body .= "Parsing line: $_";
			if( /.*Station Code\s*(=|:)\s*(\w+).*/i ) {
				$sta= $2;
				$mail_body .= "\tsta => $sta\n"; 
			}
		}
		elsif( /.*Date\s*(=|:)/i && (!$year || !$month || !$day) ) {
				$mail_body .= "Parsing line: $_"; 
			if( /.*Date\s*(=|:)\s*(\d{2,4})\s(\d{1,2})\s(\d{1,2}).*/i ) {
				$year  = $2;
				$month = $3;
				$day   = $4;
				$mail_body .= "\tyear  => $year \n" ;
				$mail_body .= "\tmonth => $month\n" ;
				$mail_body .= "\tday   => $day  \n" ;
			}
		}
		elsif( /.*Elevation\s*(:|=)/i && ! $elev ) {
				$mail_body .= "Parsing line: $_" ;
			if( /.*Elevation\s*(:|=)\s*(-*\d+)\s?(\w{0,3}).*/i ) {
				$elev = $2;
				$unit = $3;
				$mail_body .= "\telev  => $elev\n" ;
				$mail_body .= "\tunits => $unit\n" ;
			}
		}
		elsif( /.*GPS\s*(=|:)/i && (!$lat || !$lon)  ) {
				$mail_body .= "Parsing line: $_" ;
			if( /([0-9]{1,3}\.[0-9]{1,6})[^0-9]*([0-9]{1,3}\.[0-9]{1,6})/i ) {
				$lat = $1;
				$lon = $2;
				$mail_body .= "\tlat => $lat\n" ;
				$mail_body .= "\tlon => $lon\n" ;
			}
		}
	}

	#
	#Fix sta name
	#force upper case
	#
	$sta = uc($sta);

	#
	#Fix elevation
	#
	$unit = uc($unit);
	if( $unit ne 'KM' ) {
		$elev = $elev/1000;
	}

	#
	#Fix GPS
	#
	$lat = $lat !~ /-/ ? $lat : -1 * $lat;
	$lon = $lon =~ /-/ ? $lon : -1 * $lon;
	
	#
	#Convert time
	#to yyyyjjj
	#
	if($year !~ /\d{4}/ || $month !~ /\d{1,2}/ || $day !~ /\d{1,2}/ ) {
		$date =~ /\w+, (\d{1,2}) (\w+) (\d{4})/;
				$year  = $3;
				$month = $2;
				$day   = $1;
				$mail_body. "There is an error with the date field.\n";
				$mail_body. "Using date on e-mail.\n";
				$mail_body. "Found year=$year , month=$month and day=$day\n";
		$epoch = str2epoch( "$day $month $year 0:00:00" );
	}
	else { 
		$year  = sprintf("%04d", $year);
		$month = sprintf("%02d", $month);
		$day   = sprintf("%02d", $day);
		$epoch = str2epoch( "$year-$month-$day 0:00:00" );
	}
	$yday  = yearday($epoch);
	$lddate= now();

	#
	#Check variables
	#
	if($lat !~ /-?\d{1,3}?\.\d*/) { 
		$mail_body .= "ERROR: Latitude not valid. ($lat)\n" ;
		$errors = 1;
	}
	if($lon !~ /-?\d{1,3}?\.\d*/) { 
		$mail_body .= "ERROR: Longitude not valid. ($lon)\n" ;
		$errors = 1;
	}
	if($elev !~ /-?\d{1,2}?\.*\d*/) { 
		$mail_body .= "ERROR: Eleveation not valid. ($elev)\n" ;
		$errors = 1;
	}
	if($sta !~ /\w{1,9}/ ) { 
		$mail_body .= "ERROR: Station name not valid. ($sta)\n" ;
		$errors = 1;
	}
	if($yday !~ /\d{7}/) { 
		$mail_body .= "ERROR: Ondate not valid. ($yday)\n" ;
		$errors = 1;
	}



	#
	# If all good
	# go to DB
	#
	if (!$errors) {
		$mail_body .= "ADDING: [ $sta | $yday | $lat  | $lon  | $elev | $lddate ]\n" ;
		@db = dbopen( %{$pfarray}->{database}, "r+" );
		@db = dblookup( @db, "", "site", "", "" );
		$record = dbfind(@db, "sta =~ /$sta/", -1);
		if( '-1' == $record ) {
			$mail_body .= "\tERROR: Can't understand station name ($sta). \n";
			$errors = 1;
		}
		elsif( '-2' == $record ) {
			$mail_body .= "\tAdding new station to database.\n";
			$results = dbaddv( @db, 
				"sta", $sta, 
				"ondate", $yday, 
				"lat", $lat, 
				"lon", $lon,
				"elev", $elev,
				"lddate", $lddate);
			$mail_body .= "\tNew line #".$results."\n";
		}
		else {
			$mail_body .= "\t**** Station $sta already in database line $record.**** \n";

			@db = dbsubset(@db, "sta=='$sta'" );
			@db[3]=0;
			($old_yday,$old_lat,$old_lon,$old_elev) = dbgetv(@db,qw/ondate lat lon elev/);
			$mail_body .= "OLD: [ $old_yday | $old_lat  | $old_lon  | $old_elev ]\n" ;
			if ($yday >= $old_yday) {
				$mail_body .= "Updating!\n" ;
				$results = dbputv(@db, 
					"ondate", $yday, 
					"lat", $lat, 
					"lon", $lon,
					"elev", $elev,
					"lddate", $lddate);
				$mail_body .= "\tNew line #".$results."\n";
			}
			else { 
				$mail_body .= "\tTable values are more recent than email.\n";
			}
			$update = 1;
		}	
		dbclose( @db );

		@db = dbopen( %{$pfarray}->{database}, "r" );
		@db = dblookup( @db, "", "site", "", "" );
		@db = dbsubset(@db, "sta=='$sta'" );
		$rq = dbquery( @db, "dbRECORD_COUNT");
		@hd = dbquery( @db, "dbTABLE_FIELDS");
		$mail_body .= "Verifying database for sta=='$sta'\n";
		$mail_body .= "\tFound $rq result(s):\n";
		$mail_body .= "\t";
		foreach (@hd) { $mail_body .= " $_ |"; }
		$mail_body .= "\n";
		for ( $db[3] = 0 ; $db[3] < $rq ; $db[3]++ ) {
			$mail_body .= "\t";
			foreach (@hd) { $mail_body .= " ".dbgetv (@db,$_)." |"; }
			$mail_body .= "\n";
		}
		dbclose( @db);

		if( %{$pfarray}->{include_tb} ) {
			@db = dbopen( %{$pfarray}->{database}, "r" );
			@db = dblookup( @db, "", "site", "", "" );
			$rq = dbquery( @db, "dbRECORD_COUNT");
			@hd = dbquery( @db, "dbTABLE_FIELDS");
			$mail_body .= "-------------------------------------\n";
			$mail_body .= "This is a copy of the updated table.\n";
			foreach (@hd) { $mail_body .= " $_ |"; }
			$mail_body .= "\n";
			for ( $db[3] = 0 ; $db[3] < $rq ; $db[3]++ ) {
				foreach (@hd) { $mail_body .= " ".dbgetv (@db,$_)." |"; }
				$mail_body .= "\n";
			}
			$mail_body .= "-------------------------------------\n";
			dbclose( @db );
		}
	} #end of if !$errors

	#
	#If we want to include 
	#original mail in 
	#report by flag or 
	#in case of errors
	#
	if(%{$pfarray}->{include_body} || $errors) {
		$mail_body .= "Original Message:\n" ;
		$mail_body .= "----------------------\n" ;
		foreach (@body) { 
			$mail_body .= ">$_";
		}
		$mail_body .= "----------------------\n";
	}


	$mail_body .= "\n" ;
	$mail_body .= "--\n" ;
	$mail_body .= "Automatic E-mail Parser Script\n" ;
	$mail_body .= "Admin: reyes\@ucsd.edu\n" ;

	#
	# Send report 
	#
	if( %{$pfarray}->{mail_success} || $errors || $update ) {
		#
		#Check mail 
		#parameters
		#
		if( %{$pfarray}->{report_to} ) { $mail_to = %{$pfarray}->{report_to};}
		else { $mail_to =  $message->get("From");}

		$mail_sub = %{$pfarray} ->{mail_subject};
		if( $errors ) { $mail_sub .= " - ERROR on $sta"; }
		if( $update ) { $mail_sub .= " - UPDATE on $sta"; }

		if( %{$pfarray}->{cc_sender} )   { $mail_cc  = $message->get("From"); }

		if( %{$pfarray}->{report_from} ) { $mail_from = %{$pfarray}->{report_from}; }
		else { $mail_from = $message->get("From"); }

		$mailer = Mail::Mailer->new();
		$mailer->open({ From	=> $mail_from ,
						To      => $mail_to,
						Cc		=> $mail_cc,
						Subject => $mail_sub,
					  })
		  or print "Can't open: $!\n";
		print $mailer $mail_body;
		$mailer->close();
	} #end of if report

	if( %{$pfarray}->{log} || $errors ) { print $mail_body; }
	else{print "ADDED: [ $sta | $yday | $lat  | $lon  | $elev | $lddate ]\n";}
}

1; # Make require happy!
