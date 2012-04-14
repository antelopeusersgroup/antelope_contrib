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

    my ( $Pf, $auth, $cat, $cat_row, $catmax, $catmin, $cmd, $db, $evid, $lat, $lon, $magid ) ; 
    my ( $maxtime, $mintime, $nauth, $norig, $nnetmag, $nsub, $orid, $record, $stime, $time ) ; 
    my ( $tmp, $usage ) ; 
    my ( @auth, @cat, @db, @dbau, @dbcat, @dbcatnm, @dbev, @dbnj, @dbnm, @dbsub ) ;

    $pgm = $0 ; 
    $pgm =~ s".*/"" ;
    elog_init( $pgm, @ARGV );
    $cmd = "\n$0 @ARGV" ;
    
    if ( ! &getopts('v') || ( @ARGV < 1 ) ) { 
        $usage  =  "\n\n\nUsage: $0  [-v]  " ;
        $usage .=  "db1 [ db2 db3 ... ] \n\n"  ;         
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
    
    $db        = $ARGV[0] ;

    prettyprint( \%pf ) if $opt_V ;
    
    foreach $db ( @ARGV ) {
            
      elog_notify( "\nprocessing $db\n\n" ) ;
      
        @db        = dbopen  ( $db, "r+" ) ;
        @db        = dblookup( @db, 0, "origin", 0, 0 ) ;
        @dbnm      = dblookup( @db, 0, "netmag", 0, 0 ) ;
        @dbev      = dblookup( @db, 0, "event",  0, 0 ) ;

        @dbnj      = dbnojoin( @db, @dbnm ) ;
        $norig     = dbquery ( @dbnj , "dbRECORD_COUNT" ) ;    
        elog_notify ( sprintf( "number of origins with no netmags                %6d", $norig ) ) ;
    
#         @dbnj      = dbsubset( @dbnj, "auth !~ /^ANF.*|^UCSD.*|^local.*|^tele.*|^casc.*/" ) ;    
        @dbnj      = dbsubset( @dbnj, "auth !~ /$pf{local_auth}/" ) ;    
        @dbnj      = dbsort  ( @dbnj, "time" ) ;    
        $norig     = dbquery ( @dbnj , "dbRECORD_COUNT" ) ;    
        elog_notify ( sprintf( "number of origins with no netmags and no $pf{local_auth}     %6d", $norig ) ) ;
        
        next if ( $norig == 0 ) ;
    
        $mintime   = dbex_eval( @dbnj, "min(time)" ) ;
        $maxtime   = dbex_eval( @dbnj, "max(time)" ) ;
    
        @dbau      = dbsort( @dbnj, "-u", "auth" ) ;    
        $nauth     = dbquery ( @dbau , "dbRECORD_COUNT" ) ;    
        elog_notify ( sprintf( "number of unique authors                         %6d", $nauth ) ) if $opt_V ;
    
        elog_notify ( sprintf ( "start   %s  ", strydtime( $mintime ) ) );  
        elog_notify ( sprintf ( "end     %s	", strydtime( $maxtime ) ) );  
    
        @auth = () ; 
    
        for ( $dbau[3] = 0 ; $dbau[3] < $nauth ; $dbau[3]++ ) {
            ( $auth, $tmp )  = split( /_|:|-/, dbgetv( @dbau, "auth" ) ) ;
            push ( @auth, $auth ) ;
        }
    
        @auth = get_unique ( @auth ) ;
    
        elog_notify ( "unique auth		@auth " ) ;
            
        foreach $auth ( @auth ) {
            next if ( ! defined ( $pf{auth}{$auth} ) ) ;
        
            @cat = grep ( /$pf{auth}{$auth}/,  @{$pf{netmag}} ) ;
            elog_notify ( "$auth    @cat" ) if $opt_V ;
        
            foreach $cat ( @cat )  {
                @dbcat    = dbopen( $cat, "r" ) ;
                @dbcat    = dblookup( @dbcat, 0, "origin", 0, 0 ) ;
                $nnetmag  = 0 ; 


                $catmin   = dbex_eval( @dbcat, "min(time)" ) ;
                $catmax   = dbex_eval( @dbcat, "max(time)" ) ;
            
                if ( ( $catmax < $mintime ) || ( $catmin > $maxtime ) ) {
                    dbclose ( @dbcat ) ;
                    elog_notify ( "	skipping $cat" ) if $opt_v ;
                    next ;
                }

                elog_notify ( "$auth	processing $cat" ) if $opt_v ;

                @dbcatnm  = dblookup( @dbcat, 0, "netmag", 0, 0 ) ;
                @dbcat    = dbjoin  ( @dbcat, @dbcatnm ) ;
            
                @dbsub    = dbsubset( @dbnj, "auth =~ /^$auth\.*/" ) ; 
                $nsub     = dbquery ( @dbsub , "dbRECORD_COUNT" ) ;    
                for ( $dbsub[3] = 0 ; $dbsub[3] < $nsub ; $dbsub[3]++ ) {
                    ( $lat, $lon, $time, $orid, $evid ) = dbgetv ( @dbsub, qw ( lat lon time orid evid ) ) ;
                    $cat_row  = -1 ;
                    while ( $cat_row != -2 ) {
                        $cat_row = dbfind ( @dbcat, "distance($lat,$lon,lat,lon) < 0.01 && abs ( $time - time ) < .1 ", $cat_row ) ;
                        last if ( $cat_row == -2 ) ;
                        elog_notify ( "	$cat_row" ) if $opt_V ;
                        $dbcat[3] = $cat_row ;
                        $record   = dbget( split(' ', dbgetv(@dbcat, "netmag" ) ) ) ;
                        elog_notify ( $record ) if $opt_V ;
                        $dbnm[3]  = dbadd( @dbnm, $record ) ;
                        $magid    = dbnextid ( @dbnm, "magid" ) ;
                        dbputv( @dbnm, "evid", $evid, "orid", $orid, "magid", $magid ) ;
                        $nnetmag++ ;
                        elog_notify( "	added	evid	$evid	orid	$orid	magid	$magid " ) if $opt_V ;
                    }
                }
            
                dbclose ( @dbcat ) ; 
                elog_notify ( sprintf( "number of origins with netmags for %-40s     %6d    added to db     %6d", $cat, $nsub, $nnetmag ) ) ;
            }
        }
    
        elog_notify( sprintf ( "%d	original origins\n\n", $norig ) ) ;
    
    }
    
    elog_notify( sprintf ( "%d	databases processed\n\n", $#ARGV + 1 ) ) ;
    
    exit(0) ;
}

