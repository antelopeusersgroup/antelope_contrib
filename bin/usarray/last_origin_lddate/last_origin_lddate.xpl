#
#   program needs:
#
#
    use POSIX ;    
    use strict ;
    use Datascope ;
    use Getopt::Std ;
    use utilfunct ;
    
    our ( $pgm, $host );
    our ( $opt_V, $opt_d, $opt_e, $opt_t, $opt_v, $opt_w );
    
{    #  Main program

    my ( $cmd, $dbin, $dbout, $dist, $end_time, $last, $lddate, $ldhold, $nevents, $norig, $row ) ; 
    my ( $rowf, $rowt, $search, $slat, $slon, $start_time, $stime, $usage, $window ) ;
    my ( @dbev, @dbin, @dbnm, @dbo_ev, @dbo_nm, @dbo_or, @dborow, @dbout, @dbsort, @rows ) ;
    my ( %event ) ;

    $pgm = $0 ; 
    $pgm =~ s".*/"" ;
    elog_init( $pgm, @ARGV );
    $cmd = "\n$0 @ARGV" ;
    
    if ( ! &getopts('vVt:e:w:d:') || ( @ARGV != 2 ) ) { 
        $usage  =  "\n\n\nUsage: $0  [-v] [-d dist_degrees] [-w window_sec] [-t time] [-e endtime] " ;
        $usage .=  "dbin dbout \n\n"  ;         
        elog_notify( $cmd ) ; 
        elog_die ( $usage ) ; 
    }
    
    $opt_v      = defined($opt_V) ? $opt_V : $opt_v ;    
    chop ($host = `uname -n` ) ;
    
    announce( 0, 0 ) ;
    elog_notify( $cmd ) ; 
    $stime = strydtime( now() ) ;
    elog_notify( "\nstarting execution on	$host	$stime\n\n" ) ;
    
    $dbin      = $ARGV[0] ;
    $dbout     = $ARGV[1] ;
    
    $start_time = $opt_t || 0. ;
    $end_time   = $opt_e || now() ;
    $window     = $opt_w || 5.0 ;
    $dist       = $opt_d || 1.0 ;
    
    @dbout     = dbopen  ( $dbout, "r+" ) ;
    @dbo_or    = dblookup( @dbout, 0, "origin", 0, 0 ) ;
    @dbo_nm    = dblookup( @dbout, 0, "netmag", 0, 0 ) ;
    @dbo_ev    = dblookup( @dbout, 0, "event", 0, 0 ) ;

    @dbin      = dbopen  ( $dbin, "r" ) ;
    @dbin      = dblookup( @dbin, 0, "origin", 0, 0 ) ;
    @dbnm      = dblookup( @dbin, 0, "netmag", 0, 0 ) ;
    @dbev      = dblookup( @dbin, 0, "event", 0, 0 ) ;
    
    @dbin 	   = dbsubset( @dbin,   "time < \_$end_time\_ && time >= \_$start_time\_" );
    @dbsort    = dbsort  ( @dbin,   "time" ) ;
    $norig     = dbquery ( @dbsort, "dbRECORD_COUNT" ) ;
    $dbsort[3] = 0 ;
    $nevents   = 0 ;
    $last      = 0 ; 

    elog_notify ( "number of origins	 $norig" ) if $opt_V ;
    
    
    while ( $dbsort[3] < $norig ) {
        $ldhold  = $dbsort[3] ;
        
        ($stime, $slat, $slon, $lddate ) = dbgetv ( @dbsort, qw( time lat lon lddate ) ) ;
        elog_notify( sprintf ( "%s	%f	%f		%s	%d", strydtime( $stime ), $slat, $slon, strydtime( $lddate ), $ldhold ) ) if $opt_v ;

#         $search = "(( time > ( $stime + 2.0 )) || (distance ( lat, lon, $slat, $slon ) >  1.0 ) ) " ;

        $search = " distance ( lat, lon, $slat, $slon ) >  $dist " ;
        $rowf = dbfind ( @dbsort, $search, $ldhold ) ;
        elog_notify ( "	processing distance rowf $dbsort[3]	 $rowf" ) if $opt_V ;
        
        
        $search = " time > ( $stime + $window ) " ;
        $rowt = dbfind ( @dbsort, $search, $ldhold ) ;
        elog_notify ( "	processing time     rowt $dbsort[3]	 $rowt" ) if $opt_V ;
        
        if ( $rowf > 0 && $rowt > 0 ) {
            $rowf = $rowt if $rowf > $rowt ;
            elog_notify ( "	processing distance rowf $rowf" ) if $opt_V ;
        }
        
        @rows = () ;
        %event = () ;
        $nevents++ ;
        
        if ( $rowf == $dbsort[3] + 1 ) {
            elog_notify ( "	processing row $dbsort[3]" ) if $opt_V ;
        } elsif ( $dbsort[3] == $norig - 1 ) {
            elog_notify ( "	processing last row $dbsort[3]" ) if $opt_V ;
            $last = 1 ; 
        } elsif ( $dbsort[3] < $norig - 1 && $rowf < 0 && $rowt < 0 ) {
            foreach $row ( $dbsort[3]..( $norig - 1 ) ) {
                $dbsort[3] = $row ;
                $event{$row} = dbgetv( @dbsort, "lddate" ) ;
            }
            prettyprint ( \%event ) if $opt_V ;
            @rows = sort { $event{$b} <=> $event{$a} } keys %event ;
            elog_notify ( "	processing last multiple origins $rows[0]" ) if $opt_V ;
            $dbsort[3] = $rows[0] ;
            $last = 1 ; 
        } else {
            foreach $row ( $dbsort[3]..( $rowf - 1 ) ) {
                $dbsort[3] = $row ;
                $event{$row} = dbgetv( @dbsort, "lddate" ) ;
            }
            prettyprint ( \%event ) if $opt_V ;
            @rows = sort { $event{$b} <=> $event{$a} } keys %event ;
            elog_notify ( "	processing lddate row $rows[0]" ) if $opt_V ;
            $dbsort[3] = $rows[0] ;
        }
        
        @dborow = split(' ', dbgetv( @dbsort, "origin" ) ) ;
        dbadd( @dbo_or, dbget ( @dborow ) ) ;
        
        @rows = dbmatches ( @dborow, @dbev, "test1" ) ;
        elog_notify ( "	dborow @dborow	dbev	@dbev	rows	@rows " ) if $opt_V ;
        foreach $row ( @rows ) {
            $dbev[3] = $row ;
            dbadd( @dbo_ev, dbget ( @dbev ) ) ;
        }

        @rows = dbmatches ( @dborow, @dbnm, "netmag" ) ;
        elog_notify ( "	dborow @dborow	dbnm	@dbnm	rows	@rows " ) if $opt_V ;
        foreach $row ( @rows ) {
            $dbnm[3] = $row ;
            dbadd( @dbo_nm, dbget ( @dbnm ) ) ;
        }
        
        last if $last ; 
        
        $dbsort[3] = $rowf ;        
        
    }
    
    elog_notify( sprintf ( "%d	events processed	%d	original origins", $nevents, $norig ) ) ;
    exit(0) ;
}

