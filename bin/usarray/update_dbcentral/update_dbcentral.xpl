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

    my ( $Pf, $cmd, $dbcentral, $evtime, $first_dir, $last_dir, $rttime, $stime, $subject, $usage ) ;
    my ( @db ) ;
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
    
    $last_dir = &last_ev_dir( ) ;
    
    elog_notify ( "\nlast dir	$last_dir" ) if $opt_v ;
    
    $last_dir =~ s/_// ;
    elog_notify ( "\nlast dir	$last_dir" ) if $opt_v ;
        
    $rttime =  &next_month ( $last_dir ) ;
    elog_notify ( "\nrttime	$rttime" ) if $opt_v ;
    $rttime =  &yearmonth2epoch ( $rttime ) ;
    elog_notify ( "\nrttime	$rttime" ) if $opt_v ;
    
    $first_dir = &first_ev_dir( ) ;
    
    elog_notify ( "\nfirst dir	$first_dir" ) if $opt_v ;
    
    $first_dir =~ s/_// ;
        
    $evtime =  &yearmonth2epoch ( $first_dir ) ;
    
    if ( ! -f $dbcentral && ! $opt_n ) {
        &descriptor ( $dbcentral, $pf{dbcluster_schema}, $pf{dbcluster_dbpath}, $pf{dbcluster_dblocks}, $pf{dbcluster_dbidserver} ) ;
    }
    
    @db = dbopen( $dbcentral, "r+" ) ;
    
    @db = dblookup( @db, 0, "clusters", 0, 0 ) ;
    
    $db[3] = dbfind( @db, "clustername =~ /$pf{rtcluster}/", -1 ) ; 
    
    if ( $db[3] > -1 ) {
        dbputv ( @db, "time", $rttime ) unless $opt_n ;
    } else {
        dbaddv ( @db, "clustername" , $pf{rtcluster} ,
                      "time",         $rttime ,
                      "schema",       $pf{schema} ,
                      "volumes",      $pf{rtvolumes} ,
                      "net",          $pf{net} ,
                      "dir",          $pf{rtdirbase} ,
                      "dfile",        $pf{rtdfile} ,
                      "description",  $pf{rtdescription} ) ;
    }
    
    $db[3] = dbfind( @db, "clustername =~ /$pf{evcluster}/", -1 ) ; 
    
    if ( $db[3] > -1 ) {
        dbputv ( @db, "endtime", $rttime - 0.001 ) unless $opt_n ; 
    } else {
        dbaddv ( @db, "clustername" , $pf{evcluster} ,
                      "time",         $evtime ,
                      "endtime",      $rttime - 0.001 ,
                      "schema",       $pf{schema} ,
                      "volumes",      $pf{evvolumes} ,
                      "net",          $pf{net} ,
                      "dir",          $pf{evdirbase} ,
                      "dfile",        $pf{evdfile} ,
                      "description",  $pf{evdescription} ) ;
        
    }
    
    dbclose ( @db ) ;
    
#
#  Finish program
#
    $stime = strydtime(now());
    
    elog_notify ("completed successfully	$stime\n\n");

    $subject = sprintf("Success  $pgm  $host ");
    elog_notify ($subject);    
  
    exit( 0 );
}

sub last_ev_dir { # $dir = &get_last_ev_dir( ) ;
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

sub first_ev_dir { # $dir = &get_first_ev_dir( ) ;
    my ( $dir ) ;
    my ( @dirs ) ;
    
    @dirs = () ;
    
    elog_debug ( "$pf{evdirbase}" ) if $opt_V ;
    opendir( DIR, "$pf{evdirbase}" ) ;
    @dirs = sort { $a cmp $b } ( grep { /^20[0-9][0-9]_[0-2][0-9]$/  } readdir( DIR ) ) ;
    closedir( DIR ) ;

    elog_debug ( "dirs	@dirs" ) if $opt_V ;
        
    return ( $dirs[0] ) ;
}

sub descriptor {
    my ( $db, $dbschema, $dbpath, $dblocks, $dbidserver ) = @_ ;
    open  DESC, ">$db" or die "Can't open '$db', stopped" ;
    print DESC "# Datascope database\n";
    print DESC "schema		$dbschema\n";
    print DESC "dbpath		$dbpath\n";
    print DESC "dblocks		$dblocks\n";
    print DESC "dbidserver	$dbidserver\n";
    close DESC ;
    return ;
}

