#
#   program needs:
#
    use Getopt::Std ;
    use strict ;
    use Datascope ;
    use archive ;
    use timeslice ;
    use utilfunct ;
    
    our ($opt_v,$opt_V,$opt_f,$opt_m,$opt_n,$opt_p);
    our ($pgm,$host);
    
{    #  Main program

    my ( $Pf, $archive_db, $cmd, $dbname, $debug, $dirname, $dmcdbname, $endtime, $exists, $month ) ;
    my ( $problems, $replay_orb, $starttime, $stime, $str_end, $str_start ) ;
    my ( $subject, $usage, $verbose, $year );
    my ( @db ) ;
    my ( %pf ) ;

    $pgm = $0 ; 
    $pgm =~ s".*/"" ;
    elog_init($pgm, @ARGV);
    $cmd = "\n$0 @ARGV" ;
    
    if (  ! getopts('vVnfm:p:r') || @ARGV != 3 ) { 
        $usage  =  "\n\n\nUsage: $0  \n	[-v] [-V] [-n] [-f] \n" ;
        $usage .=  "	[-p pf] [-m mail_to]  \n" ;
        $usage .=  "	replay_orb YYYY MM \n\n"  ; 

        elog_notify($cmd) ; 
        elog_die ( $usage ) ; 
    }
    
    &savemail() if $opt_m ; 
    elog_notify($cmd) ; 
    $stime = strydtime(now());
    chop ($host = `uname -n` ) ;
    elog_notify ("\nstarting execution on	$host	$stime\n\n");

#
#  Set options
#

    $Pf         = $opt_p || "proc_gaps" ;
        
    $replay_orb = $ARGV[0];
    $year       = $ARGV[1];
    $month      = $ARGV[2];

    $opt_v      = defined($opt_V) ? $opt_V : $opt_v ;    
    $verbose    = $opt_v;
    $debug      = $opt_V;
    
    %pf = &getparam($Pf, $verbose, $debug);

    if ($pf{period} !~ /year|month/) {
        elog_complain("\n\n Paremeter file error.\nperiod $pf{period} is not \"year\" or \"month\"");
        $subject = "Problems - $pgm $host	Paremeter file error.";
        &sendmail($subject, $opt_m) if $opt_m ; 
        elog_die("\n$subject");
    }
    
    if (system_check(0)) {
        $subject = "Problems - $pgm $host	Ran out of system resources";
        &sendmail($subject, $opt_m) if $opt_m ; 
        elog_die("\n$subject");
    }

    ($starttime,$endtime) = &times($year,$month,$debug);
    $str_start = epoch2str($starttime,"%m/%d/%Y");
    $str_end   = epoch2str($endtime,"%m/%d/%Y");
    
    elog_notify(sprintf ("start of monthly processing	%s",strydtime($starttime))) ;
           
    elog_notify(sprintf ("end of monthly processing		%s",strydtime($endtime))) ;
    
    $cmd  = "rm -rf  $pf{cleanbalerdirbase}/$year\_$month";
    
    $problems = 0 ;

    if  ( -d "$pf{cleanbalerdirbase}/$year\_$month" &&  ! $opt_n && ! $opt_f ) {
        elog_notify("\n$cmd");        
        $problems = run($cmd,$problems) ;
    } else {
        elog_notify("\nskipping $cmd") ;
    } 

    &bad_exit($cmd) if $problems;
    
#
#   build clean database from multiple baler dbs
#

    $cmd  = "build_baler_data ";
    $cmd .= "-v " if $verbose ;
    $cmd .= "-V " if $opt_V;
    $cmd .= "-n " if $opt_n;
    $cmd .= "-d $pf{cleanbalerdirbase}/$year\_$month/baler $pf{balerdb_central} $pf{clustername} \"$str_start\" \"$str_end\" ";
    $cmd .= "> /tmp/tmp_build_baler_data\_$$ 2>&1 " unless $opt_V ;

    if  ( ! $opt_n || $opt_f ) {
        elog_notify("\n$cmd");        
        $problems = run($cmd,$problems) ;
    } else {
        elog_notify("\nskipping $cmd") ;
    } 

    &bad_exit($cmd) if $problems;
    
#
#   check to see if expected dbs are available
#

    ($dirname, $dbname, $exists) =  &mk_db_des(yearday($starttime),$pf{rtdirbase},$pf{dbbase},$pf{period},"wfdisc",$pf{dbpath},$pf{dblocks},$pf{dbidserver},$debug);    
    elog_notify("Realtime database	$dirname	$dbname	$exists") if $opt_v ;
    
    ($dirname, $dmcdbname, $exists) =  &mk_db_des(yearday($starttime),$pf{rtdirbase},$pf{dmcgapbase},$pf{period},"gap",$pf{dbpath},$pf{dblocks},$pf{dbidserver},$debug);    
    elog_notify("DMC database	$dirname	$dmcdbname	$exists") if $opt_v ;

    ($dirname, $archive_db, $exists) =  &mk_db_des(yearday($starttime),$pf{archivebase},$pf{dbbase},$pf{period},"wfdisc",$pf{dbpath},$pf{dblocks},$pf{dbidserver},$debug);    
    elog_notify("Archive database	$dirname	$archive_db	$exists") if $opt_v ;
    
#
#   build database combining rt data with baler data to fill gaps
#

    $cmd  = "fill_gaps -v ";
    $cmd .= "-V " if $opt_V;
    $cmd .= "-T " if $opt_n;
    $cmd .= "-b $pf{cleanbalerdirbase}/$year\_$month/baler -o $archive_db $dbname $dmcdbname \"$str_start\" \"$str_end\" ";
    $cmd .= "> /tmp/tmp_fill_gaps\_$$ 2>&1 " unless $opt_V ;
    
    if  ( ! $opt_n ) {
        elog_notify("\n$cmd");        
        $problems = run($cmd,$problems) ;
    } else {
        elog_notify("\nskipping $cmd") ;
    }     

    &bad_exit($cmd) if $problems;
    
#
#   Evaluate whether TA and DMC gaps have been filled by baler recoveries
#

    $cmd  = "gap_status ";
    $cmd .= "-v " if $debug ;
    $cmd .= "$dmcdbname $pf{balerwfdisc} $pf{cleanbalerdirbase}/$year\_$month/baler $archive_db";
    $cmd .= "> /tmp/tmp_gap_status_dmc\_$$ 2>&1 " unless $opt_V ;
    
    if  ( ! $opt_n ) {
        elog_notify("\n$cmd");        
        $problems = run($cmd,$problems) ;
    } else {
        elog_notify("\nskipping $cmd") ;
    } 
    
    $cmd  = "gap_status ";
    $cmd .= "-v " if $debug ;
    $cmd .= "$dbname $pf{balerwfdisc} $pf{cleanbalerdirbase}/$year\_$month/baler $archive_db";
    $cmd .= "> /tmp/tmp_gap_status_ta\_$$ 2>&1 " unless $opt_V ;
    
    if  ( ! $opt_n ) {
        elog_notify("\n$cmd");        
        $problems = run($cmd,$problems) ;
    } else {
        elog_notify("\nskipping $cmd") ;
    }     

    &bad_exit($cmd) if $problems;
    
#
#   replay final waveform database
#

    $cmd  = "replay_data -v ";
    $cmd .= "-V " if $opt_V;
    $cmd .= "-n " if $opt_n;
    $cmd .= "$archive_db $replay_orb";
    $cmd .= "> /tmp/tmp_replay_data\_$$ 2>&1 " unless $opt_V ;
    
    if  ( ! $opt_n ) {
        elog_notify("\n$cmd");        
        $problems = run($cmd,$problems) ;
    } else {
        elog_notify("\nskipping $cmd") ;
    }     

    &bad_exit($cmd) if $problems;
    
#
#   clean up
#

    $stime = strydtime(now());
    elog_notify ("completed successfully	$stime\n\n");

    $subject = sprintf("Success  $pgm  $host");
    elog_notify ($subject);
    &sendmail ( $subject, $opt_m ) if $opt_m ;

    unlink "/tmp/tmp_fill_gaps\_$$"     if (-e "/tmp/tmp_gap_status_dmc\_$$");
    unlink "/tmp/tmp_gap_status_dmc\_$$"     if (-e "/tmp/tmp_gap_status_dmc\_$$");
    unlink "/tmp/tmp_gap_status_ta\_$$"      if (-e "/tmp/tmp_gap_status_ta\_$$");
    unlink "/tmp/tmp_build_baler_data\_$$"   if (-e "/tmp/tmp_build_baler_data\_$$");
    unlink "/tmp/tmp_replay_data\_$$"      if (-e "/tmp/tmp_baler_request\_$$");
    
    exit(0);
}

# sub getparam { # %pf = getparam($Pf);
#     my ($Pf) = @_ ;
#     my ($subject);
#     my (%pf) ;
#     
#     $pf{rtdirbase}			= pfget( $Pf, "rtdirbase" );
#     $pf{dbbase}				= pfget( $Pf, "dbbase" );
#     $pf{dmcgapbase}			= pfget( $Pf, "dmcgapbase" );
#     $pf{archivebase}		= pfget( $Pf, "archivebase" );
#     $pf{period}				= pfget( $Pf, "period" );
#     $pf{balerdb_central}	= pfget( $Pf, "balerdb_central" );
#     $pf{clustername}		= pfget( $Pf, "clustername" );
#     $pf{balerwfdisc}		= pfget( $Pf, "balerwfdisc" );
#     $pf{cleanbalerdirbase}  = pfget( $Pf, "cleanbalerdirbase" );
#     
#     $pf{dbpath}     		= pfget( $Pf, "dbpath" );
#     $pf{dbidserver} 		= pfget( $Pf, "dbidserver" );
#     $pf{dblocks}    		= pfget( $Pf, "dblocks" );
# 
#     
# 
#     if ($pf{period} !~ /year|month/) {
#         elog_complain("\n\n Paremeter file error.\nperiod $pf{period} is not \"year\" or \"month\"");
#         $subject = "Problems - $pgm $host	Paremeter file error.";
#         &sendmail($subject, $opt_m) if $opt_m ; 
#         elog_die("\n$subject");
#     }
#     
#     if ($opt_V) {
#         elog_notify("\nrtdirbase        $pf{rtdirbase}");
#         elog_notify("dbbase           $pf{dbbase}");
#         elog_notify("dmcgapbase       $pf{dmcgapbase}");
#         elog_notify("archivebase      $pf{archivebase}");
#         elog_notify("period           $pf{period}" );
#         elog_notify("balerdb_central  $pf{balerdb_central}" );
#         elog_notify("clustername      $pf{clustername}" );
#         elog_notify("balerwfdisc      $pf{balerwfdisc}" );
#         elog_notify("dbpath           $pf{dbpath}" );
#         elog_notify("dbidserver       $pf{dbidserver}" );
#         elog_notify("dblocks          $pf{dblocks}\n\n" );
#     }
#         
#     return (%pf) ;
# }
   
sub bad_exit { # &bad_exit($cmd) ;
    my ($cmd) = @_ ;
    my ($stime,$subject);
    elog_complain("\nFAILED:	$cmd");
    $stime = strydtime(now());
    $subject = sprintf("FAILED:	$pgm	$host	$stime");
    elog_complain ($subject);
    &sendmail ( $subject, $opt_m ) if $opt_m ;
    elog_die();
}
