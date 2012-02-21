#
#   program needs:
#
    use Getopt::Std ;
    use strict ;
    use Datascope ;
    use archive ;
    use timeslice ;
    use timeutil ;
    use utilfunct ;
    
    our ( $pgm, $host );
    our ( $opt_v, $opt_V, $opt_n, $opt_p );
    our ( %pf );
    
{    #  Main program

    my ( $Pf, $cmd, $dir, $problems, $dbcentral, $stime, $subject, $time, $usage );
    my ( @db, @dirs );
#
#  Program setup
#
    $pgm = $0 ; 
    $pgm =~ s".*/"" ;
    elog_init( $pgm, @ARGV );
    $cmd = "\n$0 @ARGV" ;
    
    if (  ! getopts('vVnp:') ||  @ARGV != 1 ) { 
        $usage  =  "\n\n\nUsage: $0  [-v] [-V] [-n] " ;
        $usage .=  "[-p pf] " ;
        $usage .=  "dbcentral \n\n"  ;         
        elog_notify( $cmd ) ; 
        elog_die   ( $usage ) ; 
    }
    
    elog_notify( $cmd ) ; 
    $stime = strydtime( now() ) ;
    chop ( $host = `uname -n` ) ;
    elog_notify ( "\nstarting execution on	$host	$stime\n\n" ) ;

    $Pf         = $opt_p || $pgm ;
    
    $dbcentral      = $ARGV[0] ;
    
    %pf = &getparam( $Pf );
    
    $opt_v      = defined($opt_V) ? $opt_V : $opt_v ;    
    
    if (system_check(0)) {
        $subject = "Problems - $pgm $host	Ran out of system resources";
        elog_die( "\n$subject" );
    }
    $problems = 0;
    
    $dir = &get_last_ev_dir( ) ;
    
    elog_notify ( "\ndir	$dir" ) if $opt_v ;
    
    $dir =~ s/_// ;
        
    $time =  &next_month ( $dir ) ;
    $time =  &yearmonth2epoch ( $time ) ;
    
    @db = dbopen( $dbcentral, "r+" ) ;
    
    @db = dblookup( @db, 0, "clusters", 0, 0 ) ;
    
    $db[3] = dbfind( @db, "clustername =~ /$pf{rtcluster}/", 0 ) ; 
    
    dbputv ( @db, "time", $time ) ;
    
    $db[3] = dbfind( @db, "clustername =~ /$pf{evcluster}/", 0 ) ; 
    
    dbputv ( @db, "time", $time - 0.001 ) ;
    
    dbclose ( @db ) ;
    
#
#  Finish program
#
    $stime = strydtime(now());
    
    if ( $problems ) {
        elog_notify ("completed 	$stime\n\n");
        $subject = "Problems - $pgm $host" ;
        elog_die("\n$subject") ;
    }

    elog_notify ("completed successfully	$stime\n\n");

    $subject = sprintf("Success  $pgm  $host ");
    elog_notify ($subject);    
  
    exit( 0 );
}

sub get_last_ev_dir { # $dir = &get_last_ev_dir( ) ;
    my ( $dir ) ;
    my ( @dirs ) ;
    
    @dirs = () ;
    
    elog_debug ( "$pf{evdirbase}" ) if $opt_V ;
    opendir( DIR, "$pf{evdirbase}" ) ;
    @dirs = sort { $b cmp $a } ( grep { /^20[0-9][0-9]_[0-2][0-9]$/  } readdir( DIR ) ) ;
    closedir( DIR ) ;

    elog_debug ( "dirs	@dirs" ) if $opt_V ;
        
    return ( $dirs[0] ) ;
}

