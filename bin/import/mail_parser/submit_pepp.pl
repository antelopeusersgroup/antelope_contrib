use Expect;

sub cleanup_tempdir {
	
	chdir( $tempdir );
	opendir( D, $tempdir );
	while( $myfile = readdir( D ) ) {
		unlink $myfile;
	}
	closedir( D );
	chdir();

	rmdir( $tempdir );
}

sub submit_pepp_handler {
	my( $message, $pfarray ) = @_;

	my( @body ) = @{$message->body()};

	my( $save_files ) = %{$pfarray}->{save_files};

	$tempdir = "/tmp/pepp_$<_$$";
	mkdir( $tempdir, 0755 );
	chdir( $tempdir );
	if( defined( $ENV{'PFPATH'} ) && $ENV{'PFPATH'} ne "" ) {

		$ENV{'PFPATH'} = $ENV{'PFPATH'} . ":.";
	} else {
		$ENV{'PFPATH'} = ".";
	}

	grep {
		$expr = "Latitude:\\s+(\\S+)\\s*([NS])\\s+" .
			"Longitude:\\s+(\\S+)\\s*([EW])\\s+" .
			"Depth:\\s+(\\S+)\\s+";
		if( /$expr/ ) {
			( $lat, $ns, $lon, $ew, $depth ) =
			( $1, $2, $3, $4, $5 );
		}
		if( /Origin Date:\s+(.*\S)\s+Origin Time:\s+(.*)\s+UTC/ ) {
			( $date, $time ) = ( $1, $2 );
		}
	} @body;

	$lat = $ns =~ /N/i ? $lat : -1 * $lat;
	$lon = $ew =~ /E/i ? $lon : -1 * $lon;

	$time =~ s/ /0/g;
	$origintime = str2epoch( "$date $time" );

	if( $verbose ) {
		print STDERR "submit_pepp:\n" .
			"\tProcessing $lat, $lon, $depth, $date $time\n" .
		 	"\tFrom ", $message->get("From"), "\n";

	}

	open( P, ">trexcerpt.pf" );
	print P <<"	EOPF";
	channel_selection       &Tbl{
	    &Tbl{
		dbopen sensor
		dbjoin sitechan
		dbjoin site
		dbjoin instrument
		dbjoin affiliation
	    }
	}
	EOPF
	close( P );

	@db = dbopen( "tempdb", "r+" );
	@db = dblookup( @db, "", "origin", "", "" );
	dbaddv( @db, 
		"lat", $lat, 
		"lon", $lon,
		"depth", $depth,
		"time", $origintime,
		"orid", 1,
		"evid", 1 );
	@db = dblookup( @db, "", "event", "", "" );
	dbaddv( @db, 
		"evid", 1,
		"prefor", 1 );
	dbclose( @db );

	my( $site_db ) = %{$pfarray}->{site_db};
	my( $waveform_db ) = epoch2str( $origintime, 
					%{$pfarray}->{waveform_db} );

	$site_db =~ s@([^/]*)$@{$1}@;
	$waveform_db =~ s@([^/]*)$@{$1}@;

	open( D, ">tempdb" );
	print D "css3.0\n";
	print D "$site_db:$waveform_db\n";
	close( D );

	my( $net ) = %{$pfarray}->{net};
	my( $wfformat ) = %{$pfarray}->{wfformat};
	my( $pre_P_sec ) = %{$pfarray}->{pre_P_sec};
	my( $post_P_sec ) = %{$pfarray}->{post_P_sec};

	my( $filetime_template ) = %{$pfarray}->{filetime};
	$filetime = epoch2str( $origintime, $filetime_template );

	my( $wffilename ) = %{$pfarray}->{wffilename};
	$wffilename =~ s/FILETIME/$filetime/g;

	if( $wffilename =~ m@/@ ) {
		print STDERR "submit_pepp: wffilename parameter must be for " .
			"a local directory (cannot contain '/' character). " .
			"Bye.\n";
		unless( $save_files ) { cleanup_tempdir(); }
		return;
	}

	system( "$antelope/bin/trexcerpt -vv -c 'net == \"$net\"' " .
		"-o $wfformat " .
		"-m event -w '$wffilename' " .
		"tempdb dbout " .
		"'parrival() - $pre_P_sec' 'parrival() + $post_P_sec'" );

	@db = dbopen( "dbout", "r" );
	@db = dblookup( @db, "", "wfdisc", "", "" );
	$nrecs = dbquery( @db, "dbRECORD_COUNT" );

	if( $verbose ) { print STDERR "Submitting $nrecs files\n"; }

	if( $nrecs <= 0 ) {
		dbclose( @db );
		unless( $save_files ) { cleanup_tempdir(); }
		return;
	}

	my( $ftp_repository ) = %{$pfarray}->{ftp_repository};
	my( $ftp_dir ) = %{$pfarray}->{ftp_dir};
	my( $ftp_user ) = %{$pfarray}->{ftp_user};
	my( $ftp_password ) = %{$pfarray}->{ftp_password};
	my( $ftp_timeout ) = %{$pfarray}->{ftp_timeout};

	$ftp = Expect->spawn( "/usr/bin/ftp", "$ftp_repository" );
	if( ! defined( $ftp ) ) {
		print STDERR "Failed to spawn ftp process. Bye.\n";
		dbclose( @db );
		unless( $save_files ) { cleanup_tempdir(); }
		return;
	}
	($pos, $err, $match, $before, $after) = 
		$ftp->expect( $ftp_timeout, '-re', 'Name\s+\(.*\):\s*' );
	if( defined( $err ) ) {
		print STDERR "Failed to open ftp connection. Bye.\n";
		dbclose( @db );
		unless( $save_files ) { cleanup_tempdir(); }
		return;
	}
	$ftp->send_slow( 0, "$ftp_user\r" );

	($pos, $err, $match, $before, $after) = 
		$ftp->expect( $ftp_timeout, ( "Password:", "Login failed" ) );
	if( defined( $err ) ) {
		print STDERR "Failed to open ftp connection. Bye.\n";
		dbclose( @db );
		unless( $save_files ) { cleanup_tempdir(); }
		return;
	} elsif( $match =~ /failed/ ) {
		print STDERR "\nUnknown ftp user $ftp_user. Bye.\n";
		dbclose( @db );
		unless( $save_files ) { cleanup_tempdir(); }
		return;
	}

	$ftp->send_slow( 0, "$ftp_password\r" );

	($pos, $err, $match, $before, $after) = 
		$ftp->expect( $ftp_timeout, ( "ftp>", "Login failed" ) );
	if( defined( $err ) ) {
		print STDERR "Failed to open ftp connection. Bye.\n";
		dbclose( @db );
		unless( $save_files ) { cleanup_tempdir(); }
		return;
	} elsif( $match =~ /failed/ ) {
		print STDERR "\nBad password for ftp user $ftp_user. Bye.\n";
		dbclose( @db );
		unless( $save_files ) { cleanup_tempdir(); }
		return;
	}

	$ftp->send_slow( 0, "cd $ftp_dir\r" );

	($pos, $err, $match, $before, $after) = 
		$ftp->expect( $ftp_timeout, "ftp>" );
	$ftp->send_slow( 0, "binary\r" );

	for( $db[3] = 0; $db[3] < $nrecs; $db[3]++ ) {

		( $dfile ) = dbgetv( @db, "dfile" );
		
		($pos, $err, $match, $before, $after) = 
			$ftp->expect( $ftp_timeout, "ftp>" );
		$ftp->send_slow( 0, "put $dfile\r" );
	}

	dbclose( @db );

	($pos, $err, $match, $before, $after) = 
		$ftp->expect( $ftp_timeout, "ftp>" );
	$ftp->send_slow( 0, "quit\r" );

	if( $save_files ) {
		print STDERR "Saving temp directory $tempdir\n";
	} else {
		cleanup_tempdir();
	}
}

1; # Make require happy!
