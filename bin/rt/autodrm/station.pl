
sub station { 
    my ( $version ) = @_ ;
    my $result = 0 ;
    my @log = () ;
    my @result = () ; 
    my @db = dblookup (@Db, 0, "site", 0, 0 ) ; 

    @db = dbsort(@db, "sta", "ondate" ) ;
    @db = subset_net (@db) ; 
    @db = subset_sta (@db) ; 
    @db = subset_lat (@db) ; 
    @db = subset_lon (@db) ; 
    @db = subset_date_or_now (@db) ; 

    my $n = dbquery ( @db, "dbRECORD_COUNT" ) ; 
    if ( $n > 0 ) { 
	my ($anet, $sta, $fsta, $statype, $lat, $lon, $elev, $ondate, $offdate, $s);

	push(@result, "DATA_TYPE STATION $VersionId",
"Net       Sta   Type  Latitude  Longitude Coord Sys     Elev   On Date   Off Date" ) ;
	for ($db[3] = 0 ; $db[3] < $n ; $db[3]++ ) { 
	    ($sta, $statype, $lat, $lon, $elev, $ondate, $offdate) = 
		dbgetv (@db, qw(sta statype lat lon elev ondate offdate)) ; 
	    $ondate = &epoch2str(&epoch($ondate), "%Y/%m/%d"); 
	    if ( $offdate < 0 ) { 
		$offdate = "" ; 
	    } else { 
		$offdate = &epoch2str(&epoch($offdate), "%Y/%m/%d") ;
	    }
	    ($anet, $fsta) = autodrm_net ( $sta ) ;
	    $s = sprintf ( "%-9.9s %-5.5s %-4.4s %9.5f %10.5f %-12.12s %5.3f %s %s",
		    $anet, $fsta, $statype, $lat, $lon, 
		    $Reference_coordinate_system, $elev, 
		    $ondate,
		    $offdate ) ;
	    push ( @result, $s ) ;
	}
    } else { 
	$result++ ; 
	@log = ("$Prefix no channels are selected" ) ;
    }

    return ($result, \@log, \@result ) ;
}
