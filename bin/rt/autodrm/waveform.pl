
sub waveform { 
    my ( $version ) = @_ ;
    my $result = 0 ;
    my @log = () ;
    my @result = () ; 

    my @dbsensor = dblookup (@Db, 0, "sensor", 0, 0 ) ; 
    my @dbinstrument = dblookup (@Db, 0, "instrument", 0, 0 ) ; 
    my @db = dbjoin ( @dbsensor, @dbinstrument) ;
    my @dbsite = dblookup (@Db, 0, "site", 0, 0 ) ; 
    @db = dbjoin ( @db, @dbsite) ;
    my @dbsitechan = dblookup (@Db, 0, "sitechan", 0, 0 ) ; 
    @db = dbjoin ( @db, @dbsitechan) ;

    my $maxrange = pfget($Pf, "maximum_waveform_period") ;

    my $to = $Environment{'to'} ; 
    my $from = $Environment{'from'} ; 
    if ( ! defined $to) { 
	push(@log, &errlog("time range must be specified" )) ; 
	$result++ ;
    } elsif ( $to-$from > $maxrange ) {
	my $range = &strtdelta($to-$from) ;
	my $maxtime = &strtdelta($maxrange) ;
	$range =~ s/^\s*// ; 
	$range =~ s/\s*$// ; 
	$maxtime =~ s/\s*$// ; 
	$maxtime =~ s/^\s*// ; 
	push(@log, &errlog("time range is $range, but must be less than $maxtime ")) ; 
	$result++ ;
    } elsif ( $to<$from ) {
	push(@log, &errlog("time range is negative")) ; 
	$result++ ;
    } else {
	@db = subset_net (@db) ; 
	@db = subset_sta (@db) ; 
	@db = subset_chan (@db) ; 
	@db = subset_time (@db) ; 

	my $n = dbquery(@db, "dbRECORD_COUNT" ) ;
	if ( $n > 0 ) { 
	    @db = dbsort ( @db, "sta", "chan", "-u" ) ;
	    push(@result, "DATA_TYPE WAVEFORM GSE2.1:CM6") ;

	    my $view = "tmp/autodrm_view$$" ; 
	    dbsave_view ( @db, $view ) ; 
	    if ( ! -s $view ) { 
		warn ( "dbsave_view failed" ) ; 
	    }
	    my $tmpdb	  = "tmp/autodrm$$" ;
	    my $tmpwflock = "tmp/.%autodrm$$.wfdisc" ;
	    my $tmpwf     = "autodrmwf$$" ;
	    my $tmpwffile = "tmp/autodrmwf$$" ;
	    my $errors    = "tmp/autodrm_errors$$" ; 
	    my $cmd = "trexcerpt -A -w $tmpwf - $tmpdb '$from' '$to' < $view " ; 
	    if ( system ( "$cmd > $errors 2> $errors" ) != 0 ) {
		$Notify_operator = 1; 
		$result++ ;
	    }

	    if ( -s $errors ) { 
		open ( ERRORS, $errors ) ; 
		@log = <ERRORS> ; 
		close ERRORS ;
		chomp (@log) ;
		grep(s/^/$Prefix /, @log) ; 
	    }

	    if ( -s "$tmpwffile" ) { 
		($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,
		    $atime,$mtime,$ctime,$blksize,$blocks)
			= stat(_);
		if ( $size < $Environment{'max_email_size'} ) { 
		    open ( WF, $tmpwffile ) ; 
		    my @waveform = <WF> ; 
		    close WF ;
		    chomp (@waveform) ;
		    push (@result, @waveform) ;
		} else { 
		    push (@log, "$Prefix waveforms size of $size exceeds message size limit $Environment{'max_email_size'}.") ;
		    push ( @log, "$Prefix waveform data omitted") ; 
		    $result++ ;
		    $Excessive_size = 1 ;
		}
	    } else { 
		push  @log, "waveform result file $tmpwffile is empty" ;
		$Notify_operator = 1;
		$result++ ;
	    }

	    unlink "$tmpdb.wfdisc" ; 
	    unlink "$tmpdb.lastid" ; 
	    unlink "$tmpwffile" ; 
	    unlink "$tmpwflock" ; 
	    unlink $errors ; 
	    unlink $view ;
	} else { 
	    $result++ ; 
	    @log = ("$Prefix no stations/channels are selected" ) ;
	}
    }
    return ($result, \@log, \@result ) ;
}
