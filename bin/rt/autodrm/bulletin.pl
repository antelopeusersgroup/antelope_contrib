
sub subset_origin_time {
    my (@db) = @_ ;
    my ($from, $to, $n) ;
    if ( defined $Environment{'from'} ) { 
	$from = $Environment{'from'} ; 
	$to = $Environment{'to'} ; 
	@db = dbsubset(@db, "(time>= $from && time<= $to)" ) ;
    }
    return @db ; 
}

sub subset_depth {
    my (@db) = @_ ;
    if ( defined $Environment{'shallow'} ) { 
	my $subset = "depth >= $Environment{'shallow'} || depth <= $Environment{'deep'}" ;
	@db = dbsubset(@db, $subset) ;
	$n = dbquery ( @db, "dbRECORD_COUNT" ) ; 
	push(@log, "$Prefix $n records after restricting depth : $subset");
    }
    return @db ; 
}

sub subset_event_sta_dist {
    my (@db) = @_ ; 
    if ( defined $Environment{'low_dist'} ) { 
	my @dbsite = dblookup ( @db, 0, "site", 0, 0 ) ; 
	@dbsite = subset_sta (@dbsite) ;
	my $lo = $Environment{'low_dist'} ;
	my $condition ;
	if ( $lo > 0 ) { 
	    $condition = "distance(lat,lon,site.lat,site.lon)>$lo" ; 
	}
	my $hi = $Environment{'high_dist'} ;
	if ( $hi < 180.0 ) { 
	    my $hicond = "distance(lat,lon,site.lat,site.lon)<$hi" ; 
	    if ( $condition ne "" ) { 
		$condition .= "&& $hicond" ; 
	    } else {
		$condition = $hicond ; 
	    }
	}
	my @dbj = dbtheta ( @db, @dbsite, $condition ) ;
	@db = dbsever(@dbj, "site" ) ; 
	$n = dbquery ( @db, "dbRECORD_COUNT" ) ; 
	push(@log, "$Prefix $n records after restricting event-sta distance : $condition");
    }
    return @db ;
}

sub subset_arrival_list {
    my (@db) = @_ ;
    my ($re, $n) ;
    if ( defined $Environment{'arrival_list'} ) { 
	$re = list2re($Environment{'arrival_list'}) ;
	@db = dbsubset(@db, "arid =~/$re/" ) if $re ;
    }
    return @db ; 
}

sub subset_event_list {
    my (@db) = @_ ;
    my ($re, $n) ;
    if ( defined $Environment{'event_list'} ) { 
	$re = list2re($Environment{'event_list'}) ;
	@db = dbsubset(@db, "evid =~/$re/" ) if $re ;
    }
    return @db ; 
}

sub subset_origin_list {
    my (@db) = @_ ;
    my ($re, $n) ;
    if ( defined $Environment{'origin_list'} ) { 
	$re = list2re($Environment{'origin_list'}) ;
	@db = dbsubset(@db, "evid =~/$re/" ) if $re ;
    }
    return @db ; 
}

sub subset_mag {
    my (@db) = @_ ;
    if ( defined $Environment{'low_mag'} ) { 
	my $lo = $Environment{'low_mag'} ;
	my $hi = $Environment{'high_mag'} ;
	my $msubset = "mag >= $lo && mag <= $hi" ;
	my ($subset, $s ) ;
	if ( defined $Environment{'mag_type'} ) {
	    my @types = split ' ', $Environment{'mag_type'} ;
	    foreach $type ( @types ) { 
		$s = $msubset ;
		$s =~ s/mag/$type/g ; 
		if ( $subset eq "" ) {
		    $subset = $s ; 
		} else { 
		    $subset .= " && $s" ;
		}
	    }
	} else { 
	    $subset = $msubset ;
	    $subset =~ s/mag/$type/g ; 
	}
	@db = dbsubset(@db, $subset) ;
	my $n = dbquery ( @db, "dbRECORD_COUNT" ) ; 
	push(@log, "$Prefix $n records after restricting mag : $subset");
    }
    return @db ; 
}

sub subset_mb_minus_ms {
    my (@db) = @_ ;
    if ( defined $Environment{'low_mag_diff'} ) { 
	my $lo = $Environment{'low_mag_diff'} ;
	my $hi = $Environment{'high_mag_diff'} ;
	my $subset = "mb-ms >= $lo && mb-ms <= $hi" ;
	@db = dbsubset(@db, $subset) ;
	my $n = dbquery ( @db, "dbRECORD_COUNT" ) ; 
	push(@log, "$Prefix $n records after restricting mag : $subset");
    }
    return @db ; 
}

sub bulletin { 
    my ( $version ) = @_ ;
    my $result = 1 ;
    local @log = () ;
    my @result = () ; 

    my ($type, $database ) ;
    if ( defined $Environment{'bull_type'} ) { 
	$type = $Environment{'bull_type'} ;
	$database = pfget($Pf, "bulletin_types{$type}" ) ; 
    } else { 
	$database = $Database ;
    }
    
    if ( ! defined $database ) { 
	push(@log, &errlog("No bulletin '$type'" )) ; 
	$result++ ;
    } elsif ( (my $no_bulletin = pfget($Pf, "no_bulletin" )) ne "" ) { 
	push(@log, &errlog("$no_bulletin" )) ; 
	$result++ ;
    } else { 
	push(@result, "DATA_TYPE BULLETIN IMS1.0:short") ;
	my $title = pfget($Pf, "bulletin_title") ;
	$title = epoch2str(now(), $title) ; 
	push(@result, $title) ;

	my @db = dbopen ( $database, "r" ) ; 
	my @dbevent = dblookup (@db, 0, "event", 0, 0 ) ; 
	$n = dbquery ( @dbevent, "dbRECORD_COUNT" ) ; 
	push(@log, "$Prefix $n events in database") ;

	my @dborigin = dblookup (@db, 0, "origin", 0, 0 ) ; 
	@db = dbjoin ( @dbevent, @dborigin) ;

	my $restrict = pfget ( $Pf, "restrict_bulletin_origins" ) ;
	if ( $restrict ne "" ) { 
	    @db = dbsubset(@db, $restrict ) ; 
	}

	@db = subset_depth(@db) ;
	@db = subset_event_list (@db) ;
	@db = subset_lat(@db) ;
	@db = subset_lon(@db) ;
	@db = subset_mag(@db) ;
	@db = subset_mb_minus_ms(@db) ;
	@db = subset_origin_list (@db) ;
	@db = subset_origin_time (@db) ;
	@db = subset_event_sta_dist(@db) ;

	my $n = dbquery ( @db, "dbRECORD_COUNT" ) ; 
	if ( $n > 0 ) { 
	    my @dbassoc = dblookup ( @db, 0, "assoc", 0, 0) ;
	    @db = dbjoin (@db, @dbassoc) ;

	    my @dbarrival = dblookup ( @db, 0, "arrival", 0, 0) ;
	    @db = dbjoin (@db, @dbarrival) ;

	    my @dbsite = dblookup (@db, 0, "site", 0, 0 ) ; 
	    @db = dbjoin ( @db, @dbsite, "sta", "time#ondate::offdate") ;

	    @db = subset_sta (@db) ;
	    @db = subset_arrival_list (@db) ;
	}
	$n = dbquery ( @db, "dbRECORD_COUNT" ) ; 
	if ( $n > 0 ) { 
	    my $view = "/tmp/autodrm_view$$" ; 
	    dbsave_view ( @db, $view ) ; 
	    if ( ! -s $view ) { 
		push (@log, "save of bulletin view failed" ) ;
		$Notify_operator = 1 ;
	    }
	    my $tmp = "/tmp/autodrm_bulletin$$" ; 
	    my $errors = "/tmp/autodrm_errors$$" ; 
	    $result = system ( "rtbulletin -A - < $view > $tmp 2> $errors" ) ; 

	    if ( -s $errors ) { 
		open ( ERRORS, $errors ) ; 
		@log = <ERRORS> ; 
		close ERRORS ;
		chomp (@log) ;
		grep(s/^/$Prefix /, @log) ; 
		$Notify_operator = 1 ;
	    }

	    if ( -s $tmp ) { 
		open ( TMP, $tmp ) ; 
		my @bulletin = <TMP> ; 
		close TMP ;
		chomp (@bulletin) ;
		push (@result, @bulletin) ;
	    }

	    unlink $tmp ; 
	    unlink $errors ; 
	    unlink $view ;

	} else { 
	    $result++ ; 
	    @log = ("$Prefix no events are selected" ) ;
	}

    }

    return ($result, \@log, \@result ) ;
}


