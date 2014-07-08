
sub outage { 
    my ( $version ) = @_ ;
    my $result = 0 ;
    my @log = () ;
    my @result = () ; 

    my @dbsensor = dblookup (@Db, 0, "sensor", 0, 0 ) ; 
    my @dbinstrument = dblookup (@Db, 0, "instrument", 0, 0 ) ; 
    my @db = dbjoin (@dbsensor, @dbinstrument);
    my $maxrange = pfget($Pf, "maximum_outage_period") ;
    my $to = $Environment{'to'} ; 
    my $from = $Environment{'from'} ; 
    if ( ! defined $to) { 
	push(@log, &errlog("time range must be specified" )) ; 
	$result++ ;
    } elsif ( $to-$from > $maxrange ) {
	my $range = &strtdelta($to-$from) ;
	$range =~ s/^\s*// ; 
	$range =~ s/\s*$// ; 
	my $maxtime = &strtdelta($maxrange) ;
	$maxtime =~ s/\s*$// ; 
	$maxtime =~ s/^\s*// ; 
	push(@log, 
	    &errlog("time range is $range, but must be less than $maxtime ")) ;
	$result++ ;
    } else {
	@db = subset_net (@db) ; 
	@db = subset_sta (@db) ; 
	@db = subset_chan (@db) ; 
	@db = subset_time (@db) ; 
	@db = dbsort ( @db, "sta", "chan", "-u" ) ;

	my $n = dbquery(@db, "dbRECORD_COUNT" ) ;
	if ( $n > 0 ) { 
	    push(@result, "DATA_TYPE OUTAGE $VersionId") ;

	    my $view = "/tmp/autodrm_view$$" ; 
	    dbsave_view ( @db, $view ) ; 
	    if ( ! -s $view ) { 
		warn ( "dbsave_view failed" ) ; 
	    }
	    my $tmp = "/tmp/autodrm_outage$$" ; 
	    my $errors = "/tmp/autodrm_errors$$" ; 
	    $result = system ( "rtoutage -A - '$from' '$to' < $view > $tmp 2> $errors" ) ; 

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
		my @outage = <TMP> ; 
		close TMP ;
		chomp (@outage) ;
		push (@result, @outage) ;
	    }

	    unlink $tmp ; 
	    unlink $errors ; 
	    unlink $view ;
	} else { 
	    $result++ ; 
	    @log = ("$Prefix no channels are selected" ) ;
	}
    }

    return ($result, \@log, \@result ) ;
}

