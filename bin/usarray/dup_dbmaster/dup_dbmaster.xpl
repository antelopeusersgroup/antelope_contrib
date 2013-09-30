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
    our ( $opt_v, $opt_V, $opt_n, $opt_p );
    our ( %pf );
    
{    #  Main program

    my ( $cmd, $db, $dbmaster, $dirout, $fix_calib, $stime, $subject, $usage ) ;
    my ( @db, @dbs ) ;
    my ( %files ) ;
#
#  Program setup
#
    $pgm = $0 ; 
    $pgm =~ s".*/"" ;
    elog_init( $pgm, @ARGV );
    $cmd = "\n$0 @ARGV" ;
    
    if (  ! getopts('vVnp:') ||  @ARGV != 3 ) { 
        $usage  =  "\n\n\nUsage: $0  [-v] [-V] [-n] " ;
        $usage .=  "dbmaster_in dbmaster_output_dir fix_chanid_dbs \n\n"  ;         
        elog_notify( $cmd ) ; 
        elog_die   ( $usage ) ; 
    }
    
    elog_notify( $cmd ) ; 
    $stime = strydtime( now() ) ;
    chop ( $host = `uname -n` ) ;
    elog_notify ( "\nstarting execution on	$host	$stime\n\n" ) ;

    $opt_v      = defined($opt_V) ? $opt_V : $opt_v ;    
    
    if (system_check(0)) {
        $subject = "Problems - $pgm $host	Ran out of system resources";
        elog_die( "\n$subject" );
    }
    
    $dbmaster    = $ARGV[0] ;
    $dirout      = $ARGV[1] ;   
    $fix_calib   = $ARGV[2] ;  
        
    @db = dbopen ( $dbmaster, "r" ) ;
    %files = dbquery ( @db, "dbDATABASE_FILES" ) ;
    prettyprint ( \%files ) if $opt_V ;
    dbclose ( @db ) ;
    
    if ( ! $files{sitechan} ) {
        elog_die ( "sitechan table does not exist in $dbmaster" ) ;
    }
    
    if ( -d $dirout ) {
        $cmd = "chmod -R 777 $dirout" ;
        &run_cmd( $cmd ) ;
    
        $cmd = "rm -rf $dirout" ;
        &run_cmd( $cmd ) ;
    }
    
    makedir ( $dirout ) ;
    
    chdir ( $dirout ) ;

    $cmd = "dbcp -f -I lastid -v $dbmaster ." ;
    &run_cmd( $cmd ) ;
    
    @dbs = < $fix_calib > ;
    
    prettyprint ( \@dbs ) if $opt_V ;
    
    foreach $db ( @dbs ) {
        $cmd = "dbfixchanids $db" ;
        &run_cmd( $cmd ) ;
    }
    
    
#
#  Finish program
#
    $stime = strydtime(now());
    
    elog_notify ("completed successfully	$stime\n\n");

    $subject = sprintf("Success  $pgm  $host ");
    elog_notify ($subject);    
  
    exit( 0 );
}


