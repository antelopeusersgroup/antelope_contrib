#
#   program needs:
#
    use Getopt::Std ;
    use strict ;
    use Datascope ;
    use archive ;
#     use timeslice ;
#     use timeutil ;
    use utilfunct ;
    
    our ( $pgm, $host );
    our ( $opt_v, $opt_V, $opt_n, $opt_p, $opt_s );
    our ( %pf );
    
{    #  Main program

    my ( $Pf, $cmd, $dbactive, $dbb44, $dbfinal, $dbops, $delay_time, $nbaler, $net, $sta, $stime, $subject, $tar_file, $usage ) ;
    my ( @dbbaler, @dbops ) ;
#
#  Program setup
#
    $pgm = $0 ; 
    $pgm =~ s".*/"" ;
    elog_init( $pgm, @ARGV );
    $cmd = "\n$0 @ARGV" ;
    
    if (  ! getopts('vVnp:s:') ||  @ARGV != 1 ) { 
        $usage  =  "\n\n\nUsage: $0  [-v] [-V] [-n] [-p pf] [-s sta_regex] " ;
        $usage .=  "dbops \n\n"  ;         
        elog_notify( $cmd ) ; 
        elog_die   ( $usage ) ; 
    }
    
    elog_notify( $cmd ) ; 
    $stime = strydtime( now() ) ;
    chop ( $host = `uname -n` ) ;
    elog_notify ( "\nstarting execution on	$host	$stime\n\n" ) ;

    $opt_v      = defined($opt_V) ? $opt_V : $opt_v ;    
    
    $Pf = $opt_p || $pgm ;
    %pf = getparam( $Pf );

    if (system_check(0)) {
        $subject = "Problems - $pgm $host	Ran out of system resources";
        elog_die( "\n$subject" );
    }
    
    $dbops     = $ARGV[0] ;
    
    $delay_time = epoch ( yearday ( now ) ) - $pf{ delay_days } * 86400 ; 
        
    @dbops     = dbopen  ( $dbops, "r+" ) ;
    @dbbaler   = dblookup( @dbops,   0, "balerproc",   0, 0 ) ; 
    @dbbaler   = dbsubset( @dbbaler, "completed != NULL && completed < $delay_time && net =~ /$pf{ net }/") ;
    @dbbaler   = dbsubset( @dbbaler, "sta  =~ /$opt_s/ " ) if $opt_s ; 
    @dbbaler   = dbsort  ( @dbbaler, "sta" ) ;
        
    $nbaler   = dbquery ( @dbbaler, "dbRECORD_COUNT" ) ;
    elog_notify "n stas		$nbaler" ;
    
    if (! -d "$pf{ baler44_work }/completed" ) {
        makedir( "$pf{ baler44_work }/completed" ) ;
    }
        
    chdir( $pf{ baler44_work } ) ;

    for ( $dbbaler[3] = 0 ; $dbbaler[3] < $nbaler ; $dbbaler[3]++ ) {
        ( $net, $sta ) = dbgetv( @dbbaler, qw ( net sta ) ) ;
        
        $dbb44    = $pf{ baler44_work }   . "/$sta" ;
        $dbfinal  = $pf{ baler44_final }  . "/$sta" ;
        $dbactive = $pf{ baler44_active } . "/$sta" ;

        if ( ! -d $dbb44 ) {
            elog_debug ( " 	$dbb44 does not exist" ) if $opt_V ;
            next ;
        }

        if ( -d $dbactive ) {
            elog_debug ( " 	$dbactive still exists" ) if $opt_V ;
            next ;
        }

        if ( -d $dbfinal ) {
            elog_debug ( " 	$dbfinal exists" ) if $opt_V ;
        }

        elog_notify ( "$sta" ) ;
        
        $tar_file = "$pf{ tar_dir }/$net\_$sta\_original_baler44_data_tar" ;
        $cmd = "tar cf $tar_file $sta" ;
        &run_cmd( $cmd )  ;

        $cmd = "mv $sta $pf{ baler44_work }/completed " ;
        &run_cmd( $cmd )  ;
    }
    
    dbclose ( @dbops ) ;    
    
#
#  Finish program
#
    $stime = strydtime( now( ) ) ;
    
    elog_notify ( "completed successfully	$stime\n\n" ) ;

    $subject = sprintf( "Success  $pgm  $host " ) ;
    elog_notify ( $subject ) ;    
  
    exit( 0 ) ;
}


