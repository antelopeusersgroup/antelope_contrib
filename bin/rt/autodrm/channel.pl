
sub channel { 
    my ( $version ) = @_ ;
    my $result = 0 ;
    my @log = () ;
    my @result = () ; 

    my @dbsensor = dblookup (@Db, 0, "sensor", 0, 0 ) ; 
    my @dbsitechan = dblookup (@Db, 0, "sitechan", 0, 0 ) ; 
    my @dbinstrument = dblookup (@Db, 0, "instrument", 0, 0 ) ; 
    my @dbsite = dblookup (@Db, 0, "site", 0, 0 ) ; 
    my @db = dbjoin ( @dbsensor, @dbsite) ;
    @db = dbjoin ( @db, @dbsitechan) ;
    @db = dbjoin ( @db, @dbinstrument) ;

    @db = dbsort (@db, "sta", "chan", "time" ) ;
    @db = subset_net (@db) ; 
    @db = subset_sta (@db) ; 
    @db = subset_chan (@db) ; 
    @db = subset_lat (@db) ; 
    @db = subset_lon (@db) ; 
    @db = subset_time_or_now (@db) ; 

    my $n = dbquery(@db, "dbRECORD_COUNT" ) ;
    if ( $n > 0 ) { 
	push(@result, "DATA_TYPE CHANNEL $VersionId",
    "Net       Sta  Chan Aux   Latitude Longitude  Coord Sys       Elev   Depth   Hang  Vang Sample Rate Inst      On Date    Off Date" ) ;

	my $n = dbquery ( @db, "dbRECORD_COUNT" ) ; 
	my ($anet, $sta, $fsta, $chan, $fchan, $aux, $lat, $lon, $elev, $edepth, $hang, $vang, $samprate, $instype, $ondate, $offdate, $s) ;
	for ($db[3] = 0 ; $db[3] < $n ; $db[3]++ ) { 
	    ($sta, $chan, $lat, $lon, $elev, $edepth, $hang, $vang, $samprate, $instype, $ondate, $offdate) =
		dbgetv (@db, 
		    qw(sta chan lat lon elev edepth hang vang samprate instype sitechan.ondate sitechan.offdate)) ;

	    $ondate = &epoch2str(&epoch($ondate), "%Y/%m/%d"); 
	    if ( $offdate < 0 ) { 
		$offdate = "" ; 
	    } else { 
		$offdate = &epoch2str(&epoch($offdate), "%Y/%m/%d") ;
	    }
	    ($anet, $fsta) = autodrm_net ( $sta ) ;
	    ($fchan, $aux) = autodrm_aux ( $sta, $chan ) ;
	    if ( $vang == 0.0 ) { 
		$hang = -1.0 ; # follow autoDRM conventions for vertical channels 
	    }
	    $s = sprintf ( "%-9.9s %-5.5s %-3.3s %-4.4s %9.5f %10.5f %-12.12s %5.3f %5.3f %6.1f %5.1f %11.6f %-6.6s %s %s",
		$anet, $fsta, $fchan, $aux,
		$lat, $lon, 
		$Reference_coordinate_system, 
		$elev, $edepth, $hang, $vang, $samprate, $instype, $ondate, $offdate) ;
	    push ( @result, $s ) ;
	} 
    } else { 
	$result++ ; 
	@log = ("$Prefix no channels are selected" ) ;
    }

    return ($result, \@log, \@result ) ;
}
