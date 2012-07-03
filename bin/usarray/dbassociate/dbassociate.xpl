#
#   program needs:
#
#
    use POSIX ;    
    use strict ;
    use Datascope ;
    use Getopt::Std ;
    use archive ;
    use utilfunct ;
    
    our ( $pgm, $host );
    our ( $opt_V, $opt_e,  $opt_i,  $opt_n, $opt_v );
    
{    #  Main program

    my ( $cat, $cmd, $db, $nar, $need_assoc_ext, $need_assoc_orig, $stime, $usage ) ; 

    $pgm = $0 ; 
    $pgm =~ s".*/"" ;
    elog_init( $pgm, @ARGV );
    $cmd = "\n$0 @ARGV" ;
    
    if ( ! &getopts('einvV') || ( @ARGV != 2 ) ) { 
        $usage  =  "\n\n\nUsage: $0  [-v] [-n] [-e] [-i]  " ;
        $usage .=  "arrival_db catalog_db \n\n"  ;         
        elog_notify( $cmd ) ; 
        elog_die ( $usage ) ; 
    }
    
    $db         = $ARGV[0] ;
    $cat        = $ARGV[1] ;
    
    $opt_v      = defined($opt_V) ? $opt_V : $opt_v ;    
    chop ($host = `uname -n` ) ;
        
    announce( 0, 0 ) ;
    elog_notify( $cmd ) ; 
    $stime      = strydtime( now() ) ;
    elog_notify( "\nstarting execution on	$host	$stime\n\n" ) ;
        
    elog_notify( "\nstarting $db\n\n" ) ;
    
    $need_assoc_orig = &find_internal_assocs( $db ) unless $opt_e ;
    
    $need_assoc_ext  = &find_external_assocs( $db, $cat ) unless $opt_i ;
        
    elog_notify( sprintf ( "%d	arrivals assoc with original $db	%d	arrivals assoc with $cat	\n\n", $need_assoc_orig, $need_assoc_ext ) ) ;
    
    elog_notify ( sprintf ( "Success  $pgm  $host  %d databases processed", $#ARGV + 1 ) );
    exit(0) ;
}

sub find_internal_assocs { # $need_assoc_orig = &find_internal_assocs( $db ) ;
    my ( $db ) = @_ ;
    my ( $arid, $arr_time, $cmd, $dbas, $dbor, $delta, $depth, $evid, $lat, $lon, $nar, $nass, $nassoc, $need_assoc_orig, $orid, $origin, $otime, $phase, $pid, $pttime, $slat, $slon, $sta, $sttime, $timeres ) ;
    my ( @assoc, @db, @dbar, @dbarnj, @dbas, @dbor, @dborscr, @dbrow, @dbscr, @dbsi, @dbtmp, @dbtmpas, @dbtmpor, @origins ) ;
    my ( %orids ) ;
    
    elog_notify ( sprintf( "starting find_internal_assocs $db" ) ) ;

    $need_assoc_orig = 0 ;
    
    @db         = dbopen  ( $db, "r+" ) ;
    @dbor       = dblookup( @db, 0, "origin",   0, 0 ) ;
    @dbas       = dblookup( @db, 0, "assoc",    0, 0 ) ;
    @dbar       = dblookup( @db, 0, "arrival",  0, 0 ) ;
    @dbsi       = dblookup( @db, 0, "site",     0, 0 ) ;
    @dbor       = dbsort  ( @dbor, "time" ) ;
    @dborscr    = dblookup( @db, 0, "origin",   0, "dbSCRATCH" ) ;

    @dbarnj     = dbnojoin( @dbar, @dbas ) ;
    
    $nar        = dbquery ( @dbarnj , "dbRECORD_COUNT" ) ;
    
    if ( $nar == 0 ) {
        elog_notify ( sprintf( "no unassociated arrivals in $db" ) ) ;
        return ( $need_assoc_orig ) ;
    } else {
        elog_notify ( sprintf( "$nar unassociated arrivals in $db" ) ) ;
    }
    
    for ( $dbarnj[3] = 0 ; $dbarnj[3] < $nar ; $dbarnj[3]++ ) {
        ( $sta, $arr_time, $phase, $arid ) = dbgetv( @dbarnj, qw ( sta time iphase arid) ) ;
        dbputv( @dborscr, "time", $arr_time ) ;
        
        $dbsi[3]         = dbfind ( @dbsi, "sta =~ /$sta/", -1 ) ;
        ( $slat, $slon ) = dbgetv ( @dbsi, qw ( lat lon ) ) ;
        @origins         = () ;
        elog_debug ( sprintf ( "%s    %2s    arrival time    %s    %8.4f    %9.4f", $sta, $phase, strydtime( $arr_time ), $slat, $slon ) ) if $opt_V ;   
        
        if ( $phase =~ /^P.*/ ) {
            
            @origins = dbmatches( @dborscr, @dbor, "pinternal", "time#time::time+900" ) ;
            elog_debug ( " origins	@origins " ) if $opt_V; 
            foreach $origin ( @origins ) {
                $dbor[3] = $origin ;
                ( $lat, $lon, $depth, $otime, $orid ) = dbgetv ( @dbor, qw ( lat lon depth time orid ) ) ;
                $delta   = dbex_eval ( @dbor, "distance( $lat, $lon, $slat, $slon )" ) ;
                $pttime  = dbex_eval ( @dbor, "ptime( $delta, $depth )" ) ;
                $timeres = $arr_time - $otime - $pttime ;
                elog_debug ( sprintf ( "              origin  time    %s    %8.4f    %9.4f    %6.2f", strydtime( $otime ), $delta, $pttime, $timeres ) ) if $opt_V;  
                if ( abs( $timeres ) < 3.0 ) {
                    elog_debug ( sprintf ( "arid  %8d    orid  %8d    timeres	%6.2f", $arid, $orid, $timeres ) ) if $opt_V;
                    $need_assoc_orig++ ;
                    @dbrow = split(' ', dbgetv( @dbarnj, "arrival" ) ) ;
                    $orids{$orid}{$arid}     = $dbrow[3] ;
                }
            }
        }
        if ( $phase =~ /^S.*/ ) {
            @origins = dbmatches( @dborscr, @dbor, "sinternal", "time#time-1800::time" ) ;
            foreach $origin ( @origins ) {
                $dbor[3] = $origin ;
                ( $lat, $lon, $depth, $otime, $orid ) = dbgetv ( @dbor, qw ( lat lon depth time orid ) ) ;
                $delta   = dbex_eval ( @dbor, "distance( $lat, $lon, $slat, $slon )" ) ;
                $sttime  = dbex_eval ( @dbor, "stime( $delta, $depth )" ) ;
                $timeres = $arr_time - $otime - $sttime ;
                elog_debug ( sprintf ( "              origin  time    %s    %8.4f    %9.4f    %6.2f", strydtime( $otime ), $delta, $pttime, $timeres ) ) if $opt_V;  
                if ( abs( $timeres ) < 10.0 ) {
                    elog_debug ( sprintf ( "arid  %8d    orid  %8d    timeres	%6.2f", $arid, $orid, $timeres ) ) if $opt_V;
                    $need_assoc_orig++ ;
                    @dbrow = split(' ', dbgetv( @dbarnj, "arrival" ) ) ;
                    $orids{$orid}{$arid}     = $dbrow[3] ;
                }
            }
        }                
    }
    
    dbclose ( @db ) ;


    $pid = open( DBLOC_ASSOC, "| dbloc_assoc $db tmp_assoc $db " ) ;
    
    foreach $orid ( sort { $a <=> $b } keys %orids ) {
        $evid = $orid ;
        $cmd  = "" ;
        foreach $arid ( sort { $a <=> $b } keys %{$orids{$orid}} ) {
            $cmd .= "$orids{$orid}{$arid} " ;
        }
        $cmd = "$evid 3. 10. \@best,PS,first " . $cmd ; 
        elog_debug ( $cmd ) if $opt_V;
        
        print DBLOC_ASSOC "$cmd \n"  ;

    }
    
    close ( DBLOC_ASSOC ) ;

    @db         = dbopen  ( $db, "r+" ) ;
    @dbor       = dblookup( @db, 0, "origin",   0, 0 ) ;
    @dbas       = dblookup( @db, 0, "assoc",    0, 0 ) ;
    @dbscr      = dblookup( @db, 0, "assoc",    0, "dbSCRATCH" ) ;
    
    @dbtmp      = dbopen  ( "tmp_assoc", "r+" ) ;
    @dbtmpor    = dblookup( @dbtmp, 0, "origin",   0, 0 ) ;
    @dbtmpas    = dblookup( @dbtmp, 0, "assoc",    0, 0 ) ;
    
    $nass       = dbquery ( @dbtmpas, "dbRECORD_COUNT" ) ;
    elog_debug ( "$nass associations to process" ) ;
    
    unless ( $opt_n ) {
        for ( $dbtmpas[3] = 0 ; $dbtmpas[3] < $nass ; $dbtmpas[3]++ ) {
            dbput ( @dbscr, dbget( @dbtmpas ) ) ;
            $orid = dbgetv( @dbtmpas, qw ( orid ) ) ;
            $dbtmpor[3] = dbfind( @dbtmpor, "orid == $orid", -1 ) ;
            $orid = dbgetv( @dbtmpor, qw ( evid ) ) ;
            dbputv( @dbscr, "orid", $orid ) ;
            dbadd ( @dbas ) ;
        }
    
        foreach $orid ( sort { $a <=> $b } keys %orids ) {
            $dbor[3] = dbfind ( @dbor, "orid == $orid", -1 ) ;
            $nassoc  = dbgetv ( @dbor, qw ( nass ) ) ;
            @assoc   = dbmatches ( @dbor, @dbas, "assoc", "orid" ) ;
            elog_debug ( "orid	$orid	nass	$nassoc	assoc	$#assoc " ) if $opt_V ; 
            dbputv( @dbor, "nass", $#assoc + 1 ) ;
        }
    }
    
    dbdestroy ( @dbtmp ) unless $opt_V ;
    
    dbclose ( @db ) ;
    
    return ( $need_assoc_orig ) ;
}

    
sub find_external_assocs { # $need_assoc_ext = &find_external_assocs( $db, $cat ) ;
    my ( $db, $cat ) = @_ ;
    my ( $arid, $arr_time, $auth, $cmd, $delta, $depth, $evid, $lat, $lon, $maxtime, $mintime, $nar, $need_assoc_ext, $nopt, $norig, $orid, $origin, $otime, $phase, $pid, $prefor, $pttime, $slat, $slon, $sta, $sttime, $timeres ) ;
    my (  @db, @dbar, @dbarnj, @dbas, @dbcat, @dbcatnm, @dbcatscr, @dbev, @dbor, @dbrow, @dbsi, @origins ) ;
    my ( %orids ) ;
    
    elog_notify ( sprintf( "starting find_external_assocs $db $cat" ) ) ;

    $need_assoc_ext  = 0 ;

    @db         = dbopen  ( $db, "r+" ) ;
    @dbas       = dblookup( @db, 0, "assoc",    0, 0 ) ;
    @dbar       = dblookup( @db, 0, "arrival",  0, 0 ) ;
    @dbsi       = dblookup( @db, 0, "site",     0, 0 ) ;

    @dbarnj     = dbnojoin( @dbar, @dbas ) ;
    
    $nar        = dbquery ( @dbarnj , "dbRECORD_COUNT" ) ;
    if ( $nar == 0 ) {
        elog_notify ( sprintf( "no unassociated arrivals in $db" ) ) ;
        return ( $need_assoc_ext ) ;
    } else {
        elog_notify ( sprintf( "$nar unassociated arrivals in $db" ) ) ;
    }
    
    $mintime    = dbex_eval( @dbarnj, "min(time)" ) - 3600. ;
    $maxtime    = dbex_eval( @dbarnj, "max(time)" ) ;
    elog_notify ( sprintf ( "	start   %s  ", strydtime( $mintime ) ) ) ;  
    elog_notify ( sprintf ( "	end     %s	", strydtime( $maxtime ) ) ) ;  
    
    $cmd = "last_origin_lddate -t $mintime -e $maxtime $cat tmp_cat_$$" ;
    
    if ( $opt_n ) {
        $nopt  = 1 ;
        $opt_n = 0 ;
    }
    
    if ( ! &run_cmd( $cmd ) ) {
        elog_die ( "$cmd failed"  ) ;
    }

    if ( $nopt ) {
        $opt_n  = 1 ;
    }

    @dbcat      = dbopen  ( "tmp_cat_$$", "r+" ) ;
    @dbcat      = dblookup( @dbcat, 0, "origin", 0, 0 ) ;
    @dbcatnm    = dblookup( @dbcat, 0, "netmag", 0, 0 ) ;
    @dbcatscr   = dblookup( @dbcat, 0, "origin", 0, "dbSCRATCH"  ) ;

    for ( $dbarnj[3] = 0 ; $dbarnj[3] < $nar ; $dbarnj[3]++ ) {
        ( $sta, $arr_time, $phase, $arid ) = dbgetv( @dbarnj, qw ( sta time iphase arid) ) ;
                        
        $dbsi[3]         = dbfind ( @dbsi, "sta =~ /$sta/", -1 ) ;
        ( $slat, $slon ) = dbgetv ( @dbsi, qw ( lat lon ) ) ;
        @origins         = () ;
        elog_debug ( sprintf ( "%s    %2s    arrival time    %s    %8.4f    %9.4f", $sta, $phase, strydtime( $arr_time ), $slat, $slon ) ) if $opt_V;   
        dbputv( @dbcatscr, "time", $arr_time ) ;
        
        if ( $phase =~ /^P.*/ ) {           
            @origins = dbmatches( @dbcatscr, @dbcat, "pexternal", "time#time::time+900" ) ;
            elog_debug ( " origins	@origins " ) if $opt_V; 
            foreach $origin ( @origins ) {
                $dbcat[3] = $origin ;
                ( $lat, $lon, $depth, $otime, $orid ) = dbgetv ( @dbcat, qw ( lat lon depth time orid ) ) ;
                $delta   = dbex_eval ( @dbcat, "distance( $lat, $lon, $slat, $slon )" ) ;
                $pttime  = dbex_eval ( @dbcat, "ptime( $delta, $depth )" ) ;
                $timeres = $arr_time - $otime - $pttime ;
                elog_debug ( sprintf ( "              origin  time    %s    %8.4f    %9.4f    %6.2f", strydtime( $otime ), $delta, $pttime, $timeres ) ) if $opt_V;  
                if ( abs( $timeres ) < 3.0 ) {
                    elog_debug ( sprintf ( "arid  %8d    orid  %8d    timeres	%6.2f", $arid, $orid, $timeres ) ) if $opt_V;
                    $need_assoc_ext++ ;
                    @dbrow = split(' ', dbgetv( @dbarnj, "arrival" ) ) ;
                    $orids{$orid}{$arid}     = $dbrow[3] ;
                    last ;
                }
            }
        }
        if ( $phase =~ /^S.*/ ) {
            @origins = dbmatches( @dbcatscr, @dbcat, "sexternal", "time#time-1800::time" ) ;
            foreach $origin ( @origins ) {
                $dbcat[3] = $origin ;
                ( $lat, $lon, $depth, $otime, $orid ) = dbgetv ( @dbcat, qw ( lat lon depth time orid ) ) ;
                $delta   = dbex_eval ( @dbcat, "distance( $lat, $lon, $slat, $slon )" ) ;
                $sttime  = dbex_eval ( @dbcat, "stime( $delta, $depth )" ) ;
                $timeres = $arr_time - $otime - $sttime ;
                elog_debug ( sprintf ( "              origin  time    %s    %8.4f    %9.4f    %6.2f", strydtime( $otime ), $delta, $pttime, $timeres ) ) if $opt_V;  
                if ( abs( $timeres ) < 10.0 ) {
                    elog_notify ( sprintf ( "arid  %8d    orid  %8d    timeres	%6.2f", $arid, $orid, $timeres ) ) ;
                    $need_assoc_ext++ ;
                    @dbrow = split(' ', dbgetv( @dbarnj, "arrival" ) ) ;
                    $orids{$orid}{$arid}     = $dbrow[3] ;
                    last ;
                }
            }
        }                
    }
    
    unless ( $opt_n ) {
        $pid = open( DBLOC_ASSOC, "| dbloc_assoc $db $db tmp_cat_$$" ) ;
    
        foreach $orid ( sort { $a <=> $b } keys %orids ) {
            $evid = dbnextid( @db, "evid" ) ;
            $cmd  = "" ;
            foreach $arid ( sort { $a <=> $b } keys %{$orids{$orid}} ) {
                $cmd .= "$orids{$orid}{$arid} " ;
            }
            $cmd = "$evid 3. 10. \@best,PS,first " . $cmd ; 
            elog_debug ( $cmd ) if $opt_V;
        
            print DBLOC_ASSOC "$cmd \n"  ;

        }
        close ( DBLOC_ASSOC ) ;
    
        @dbor       = dblookup( @db, 0, "origin",   0, 0 ) ;
        @dbev       = dblookup( @db, 0, "event",    0, 0 ) ;
        @dbor       = dbnojoin( @dbor, @dbev ) ;
    
        $norig      = dbquery ( @dbor, "dbRECORD_COUNT"  ) ;
    
        for ( $dbor[3] = 0 ; $dbor[3] < $norig ; $dbor[3]++ ) {
            ( $evid, $prefor, $auth ) = dbgetv ( @dbor, qw ( evid orid auth ) ) ;
            dbaddv ( @dbev , "evid", $evid, "prefor", $prefor, "auth", $auth ) ;
        }
    }

    dbclose   ( @db ) ;
    dbdestroy ( @dbcat ) unless $opt_V ;
    
    unlink 

    return ( $need_assoc_ext ) ;
}

