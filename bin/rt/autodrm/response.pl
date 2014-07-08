
sub response { 
    my ( $version ) = @_ ;
    my $result = 0 ;
    my @log = () ;
    my @result = () ; 

    my @db = dblookup (@Db, 0, "stage", 0, 0 ) ; 

    @db = subset_net (@db) ; 
    @db = subset_sta (@db) ; 
    @db = subset_chan (@db) ; 
    @db = subset_time_or_now (@db) ; 

    @db = dbsort ( @db, "sta", "chan", "time", "-u" ) ; 

    my $n = dbquery(@db, "dbRECORD_COUNT" ) ;
    if ( $n > 0 ) { 
	push(@result, "DATA_TYPE RESPONSE $VersionId") ;

	my $view = "/tmp/autodrm_view$$" ; 
	dbsave_view ( @db, $view ) ; 
	if ( ! -s $view ) { 
	    warn ( "dbsave_view failed" ) ; 
	}
	my $tmp = "/tmp/autodrm_response$$" ; 
	my $errors = "/tmp/autodrm_errors$$" ; 
	$result = system ( "dbresp2autodrm - < $view > $tmp 2> $errors" ) ; 

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
	    @response = <TMP> ; 
	    close TMP ;
	    chomp (@response) ;
	    push (@result, @response) ;
	}

	unlink $tmp ; 
	unlink $errors ; 
	unlink $view ;
    } else { 
	$result++ ; 
	@log = ("$Prefix no channels are selected" ) ;
    }
    return ($result, \@log, \@result ) ;
}

