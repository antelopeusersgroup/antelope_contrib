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
    our ( $opt_V, $opt_p, $opt_v );
    our ( %pf );
    
{    #  Main program

    my ( $Pf, $cat, $cat_magid, $cat_orid, $catmax, $catmin, $cmd, $db, $evid, $lat, $lon, $magid, $arid, $artime, $assoc, $cat_depth, $cat_lat, $cat_lon, $cat_time, $delta, $esaz, $nass, $phase, $seaz, $slat, $slon, $sta, $timeres, $vmodel ) ; 
    my ( $maxevid, $maxtime, $minevid, $mintime, $ncheck, $nev, $nevents, $nmag, $nnetmag, $norig, $nrecord ) ; 
    my ( $orecord, $orid, $stime, $time, $usage, $sdobs, $pptime, $ptime, $sptime, $sttime, $pred_time ) ; 
    my ( @cat, @cnmags, @db, @dbcat, @dbcatnm, @dbcheck, @dbev, @dbj, @dbnm, @dbor, @dbscr, @origins,  @assocs, @dbar, @dbas, @dboe, @dbsi ) ;

    $pgm = $0 ; 
    $pgm =~ s".*/"" ;
    elog_init( $pgm, @ARGV );
    $cmd = "\n$0 @ARGV" ;
    
    if ( ! &getopts('vV') || ( @ARGV < 1 ) ) { 
        $usage  =  "\n\n\nUsage: $0  [-v]  " ;
        $usage .=  "db1 [ db2 ... ]\n\n"  ;         
        elog_notify( $cmd ) ; 
        elog_die ( $usage ) ; 
    }
    
    $opt_v      = defined($opt_V) ? $opt_V : $opt_v ;    
    chop ($host = `uname -n` ) ;
    
    $Pf         = $opt_p || $pgm ;
    %pf         = getparam( $Pf ) ;
    
    announce( 0, 0 ) ;
    elog_notify( $cmd ) ; 
    $stime     = strydtime( now() ) ;
    elog_notify( "\nstarting execution on	$host	$stime\n\n" ) ;
    
    prettyprint( \%pf ) if $opt_v ;
    @cat = @{$pf{netmag}} ;
    
    foreach $db ( @ARGV ) {
    
        elog_notify( "\nstarting $db\n\n" ) ;

        @db        = dbopen  ( $db, "r+" ) ;
        @dbor      = dblookup( @db, 0, "origin",   0, 0 ) ;
        @dbnm      = dblookup( @db, 0, "netmag",   0, 0 ) ;
        @dbev      = dblookup( @db, 0, "event",    0, 0 ) ;
        @dbas      = dblookup( @db, 0, "assoc",    0, 0 ) ;
        @dbar      = dblookup( @db, 0, "arrival",  0, 0 ) ;
        @dboe      = dblookup( @db, 0, "origerr",  0, 0 ) ;
        @dbsi      = dblookup( @db, 0, "site",     0, 0 ) ;
        @dbscr     = dblookup( @db, 0, "origin",   0, "dbSCRATCH" ) ;

        $nev       = dbquery ( @dbev , "dbRECORD_COUNT" ) ;
    
        if ( $nev == 0 ) {
            elog_complain ( sprintf( "no events in $db" ) ) ;
            next ; 
        }
    

        @dbj       = dbjoin  ( @dbev, @dbor ) ;
    
#         @dbj       = dbsubset ( @dbj, "prefor == orid && auth =~ /^ANF.*/" ) ;
        @dbj       = dbsubset ( @dbj, "prefor == orid && auth =~ /$pf{local_auth}/" ) ;
    
        $nev       = dbquery ( @dbj , "dbRECORD_COUNT" ) ;    
        elog_notify ( sprintf( "number of events with $pf{local_auth} prefor                %6d", $nev ) ) ;
    
        @dbcheck   = dbseparate ( @dbj, "event" ) ;
    
        $ncheck    = dbquery ( @dbcheck , "dbRECORD_COUNT" ) ;    
        elog_notify ( sprintf( "number of events to check                       %6d", $ncheck ) ) if $opt_V ;
    
        $mintime   = dbex_eval( @dbor, "min(time)" ) ;
        $maxtime   = dbex_eval( @dbor, "max(time)" ) ;
    
        elog_notify ( sprintf ( "start   %s  ", strydtime( $mintime ) ) ) ;  
        elog_notify ( sprintf ( "end     %s	", strydtime( $maxtime ) ) ) ;  

        $minevid   = dbex_eval( @dbcheck, "min(evid)" ) ;
        $maxevid   = dbex_eval( @dbcheck, "max(evid)" ) ;
        elog_notify ( "evid	$minevid	$maxevid" ) if $opt_V ;

    
        $norig    = 0 ;
        $nnetmag  = 0 ;

        foreach $cat ( @cat )  {
            @dbcat    = dbopen( $cat, "r" ) ;
            @dbcat    = dblookup( @dbcat, 0, "origin", 0, 0 ) ;
            @dbcatnm  = dblookup( @dbcat, 0, "netmag", 0, 0 ) ;
            @dbcat    = dbsort  ( @dbcat, "-r", "lddate" ) ;

            $catmin   = dbex_eval( @dbcat, "min(time)" ) ;
            $catmax   = dbex_eval( @dbcat, "max(time)" ) ;
            
            if ( ( $catmax < $mintime ) || ( $catmin > $maxtime ) ) {
                dbclose ( @dbcat ) ;
                elog_notify ( "	skipping $cat" ) if $opt_v ;
                next ;
            }

            elog_notify ( "processing $cat" ) ;
        
            for ( $dbcheck[3] = 0 ; $dbcheck[3] < $ncheck ; $dbcheck[3]++ ) {    
                
#  find if ANF author is only origin

                @origins      = dbmatches( @dbcheck, @dbor, "origin", "evid" ) ;
                next if ( $#origins > 0 ) ;
            
                $dbor[3]  = $origins[0] ;
                ( $lat, $lon, $time, $evid, $nass ) = dbgetv ( @dbor, qw ( lat lon time evid nass ) ) ;
                $dbcat[3] = dbfind ( @dbcat, "distance( $lat, $lon, lat, lon ) < 0.3 && abs ( $time - time ) < 3.0 ", -1 ) ;
                next if ( $dbcat[3] == -2 ) ;
                
#  Add origin
            
                elog_notify ( sprintf ( "	%8.4f	%8.4f	%s	%6d ", $lat, $lon, strydtime($time), $evid ) )  ;
                ( $cat_orid, $cat_lat, $cat_lon, $cat_time, $cat_depth )    = dbgetv ( @dbcat, qw ( orid lat lon time depth  ) ) ;
                elog_debug ( "	cat orid	$cat_orid" ) if $opt_V ;
                $orecord     = dbget( split( ' ', dbgetv( @dbcat, "origin" ) ) ) ;
            
                dbput( @dbscr, $orecord ) ;
                $orid        = dbnextid ( @db, "orid" ) ;
                if ( $orid == -1 ) {
                    elog_die ( "dbnextid returned $orid for     evid $evid	cat orid	$cat_orid" ) ;
                }
                dbputv( @dbscr, "evid", $evid, "orid", $orid, "nass", $nass ) ;
                dbadd( @dbscr ) ;
                $norig++ ;

#  Add netmags

                @cnmags         = dbmatches( @dbcat, @dbcatnm, "netmag", "orid" ) ;
                foreach $nmag ( @cnmags ) {
                    $dbcatnm[3] = $nmag ; 
                    $nrecord    = dbget( @dbcatnm ) ;
                    $cat_magid  = dbgetv ( @dbcatnm, "magid" ) ;
                    elog_debug ( "		cat magid	$cat_magid" ) if $opt_V ;
                    $dbnm[3]    = dbadd( @dbnm, $nrecord ) ;
                    $magid      = dbnextid ( @dbnm, "magid" ) ;
                    dbputv( @dbnm, "evid", $evid, "orid", $orid, "magid", $magid ) ;
                    $nnetmag++ ;
                }
                
#  Add assocs

                $dbor[3]     = $origins[0] ;
                @assocs      = dbmatches( @dbor, @dbas, "assoc", "orid" ) ;
                $sdobs       = 0 ;
                foreach $assoc ( @assocs ) {
                    $dbas[3] = $assoc ; 
                    elog_debug( "assoc	$assoc" ) if $opt_V ; 
                    ( $arid, $sta, $phase, $vmodel ) = dbgetv( @dbas, qw (arid sta phase vmodel ) ) ;
                    elog_debug( "arid	$arid" ) if $opt_V ; 
                    
                    $dbar[3] = dbfind ( @dbar, "arid == $arid", -1 ) ;
                    elog_debug( "dbfind	$dbar[3]" ) if $opt_V ; 
                    ( $artime ) = dbgetv( @dbar, "time" ) ;
                    elog_debug( "artime	$artime" ) if $opt_V ;
                    
                    $dbsi[3] = dbfind ( @dbsi, "sta =~ /$sta/", -1 ) ;
                    ( $slat, $slon ) = dbgetv ( @dbsi, qw ( lat lon ) ) ;
                    
                    elog_notify( sprintf ("	%s	%s	%d	%s	%s	%.4f	%.4f	%.4f	%.4f", $sta, $phase, $arid, $vmodel, strydtime ($artime), $cat_lat, $cat_lon, $slat, $slon ) ) if $opt_v ;
                    
                    $delta = dbex_eval ( @dbor, "distance( $cat_lat, $cat_lon, $slat,    $slon    )" ) ;
                    $esaz  = dbex_eval ( @dbor, "azimuth(  $cat_lat, $cat_lon, $slat,    $slon    )" ) ;
                    $seaz  = dbex_eval ( @dbor, "azimuth(  $slat,    $slon,    $cat_lat, $cat_lon )" ) ;
                    
                    $dbas[3] = dbaddv ( @dbas, "arid"    , $arid ,
                                               "orid"    , $orid ,
                                               "sta"     , $sta ,
                                               "phase"   , $phase,
                                               "delta"   , $delta,
                                               "seaz"    , $seaz,
                                               "esaz"    , $esaz,
                                               "vmodel"  , $vmodel ) ;
                                               
                    if ( $phase =~ /^P.*/ ) {
                        $pred_time  = dbex_eval ( @dbor, "ptime( $delta, $cat_depth )" ) ;
                        elog_debug( sprintf("	ptime        %5.2f	%8.4f	%5.2f", $pred_time, $delta, $cat_depth ) ) if $opt_V ;
                    }
                    if ( $phase =~ /^S.*/ ) {
                        $pred_time  = dbex_eval ( @dbor, "stime( $delta, $cat_depth )" ) ;
                        elog_debug( sprintf("	stime        %5.2f	%8.4f	%5.2f", $pred_time, $delta, $cat_depth ) ) if $opt_V ;
                    }
                    $timeres = $artime - $cat_time - $pred_time ;
                    dbputv ( @dbas, "timeres", $timeres ) ;
                    $sdobs += $timeres**2 ; 
                }
                
#  Add origerr
                dbaddv ( @dboe, "orid", $orid, "sdobs", sqrt ( $sdobs / $#assocs ) ) ;
            
            }
            dbclose( @dbcat );
        }
        
        elog_notify( sprintf ( "%d	events processed	%d	origins added	%d	netmags added\n\n", $nev, $norig, $nnetmag ) ) ;
    
    }

    elog_notify ( sprintf ( "Success  $pgm  $host  %d databases processed", $#ARGV + 1 ) );
    exit(0) ;
}

