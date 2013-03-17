#
#   program needs:
#
#
    use POSIX ;    
    use strict ;
    use Datascope ;
    use archive;
    use utilfunct ;
    use utility ;
    use Getopt::Std ;
    
    our ( $pgm, $host );
    our ( $opt_V, $opt_d, $opt_f, $opt_m, $opt_n, $opt_p, $opt_s, $opt_v );
    our ( %pf );
    
{    #  Main program

    my ( $Pf, $cmd, $db, $idle, $iowait, $kernel, $max_forks, $nchild, $ncpu, $nstas, $parent ) ;
    my ( $pid, $problems, $sta, $stime, $string, $subject, $swap, $usage, $user ) ;
    my ( @procs, @stas, @the_rest ) ;
    my ( %errors, %logs, %stas ) ;

    $pgm = $0 ; 
    $pgm =~ s".*/"" ;
    elog_init( $pgm, @ARGV );
    $cmd = "\n$0 @ARGV" ;
    
    if ( ! &getopts('vnm:p:') || ( @ARGV < 1 ) ) { 
        $usage  =  "\n\n\nUsage: $0  [-v] [-n] " ;
        $usage .=  "[-p pf] [-m mail_to] sta [sta2 [sta3 ... ] ]\n\n"  ;         
        elog_notify($cmd) ; 
        elog_die ( $usage ) ; 
    }
    
    $opt_v      = defined($opt_V) ? $opt_V : $opt_v ;    
    chop ($host = `uname -n` ) ;
    
    $Pf         = $opt_p || $pgm ;
            
    &savemail() if $opt_m ; 
    announce( 0, 0 ) ;
    elog_notify( $cmd ) ; 
    $stime = strydtime( now() ) ;
    elog_notify( "\nstarting execution on	$host	$stime\n\n" ) ;
    
    @stas = @ARGV ;
    
    %pf = getparam( $Pf ) ;
                        
    STA: foreach $sta ( sort ( @stas ) ) {

        &proc_sta( $sta, $$ ) ;
        
    }
    
#
# Finish up
#
    $stime = strydtime( now() ) ;
    elog_notify ("completed 	$stime\n\n") ;

    if ($problems == 0 ) {
        $subject = sprintf("Success  $pgm  $host  $nstas stations processed") ;
        elog_notify ($subject);
        &sendmail ( $subject, $opt_m ) if $opt_m ;
    } else { 
        $subject = "Problems - $pgm $host	$nstas stations processed, $nchild stations with problems, $problems total problems" ;
        &sendmail($subject, $opt_m) if $opt_m ; 
        elog_complain("\n$subject") ;
        exit(1);
    }
  
    exit(0) ;
}


sub proc_sta { # &proc_sta( $sta, $parent ) ;
    my ( $sta, $parent ) = @_ ;
    my ( $baler44dir, $cmd, $dbname, $prob, $stime, $string ) ;

    $stime = strydtime( now() ) ;        
    $string = "\n$sta - starting processing station $stime" ;
    fork_notify ( $parent, $string ) ;
    
    if  ( -d "$pf{baler14procbase}\/$sta" ) {
        $cmd  = "mv $pf{baler14procbase}\/$sta $pf{baler14dirbase}";
                        
        if ( ! &run_cmd( $cmd ) ) {
            $prob++ ;
            &print_prob ( $prob, "FAILED: $cmd", $parent, *PROB ) ;
        }
    }

    if ( -d "$pf{archivebase}\/$sta" ) {
        $cmd  = "rm -rf  $pf{archivebase}\/$sta";
                        
        if ( ! &run_cmd( $cmd ) ) {
            $prob++ ;
            &print_prob ( $prob, "FAILED: $cmd", $parent, *PROB ) ;
        }
    }

    if ( -d "$pf{activebase}\/$sta" ) {
        $cmd  = "rm -rf  $pf{activebase}\/$sta";
                        
        if ( ! &run_cmd( $cmd ) ) {
            $prob++ ;
            &print_prob ( $prob, "FAILED: $cmd", $parent, *PROB ) ;
        }
    }

    $dbname = "$pf{baler44dirbase}/$sta/$sta\_baler" ;
        
    if ( -f "$dbname" ) {
        $cmd  = "dbset $dbname\.rsyncbaler msdtime \"*\" NULL";
                        
        if ( ! system( $cmd ) ) {
            $prob++ ;
            &print_prob ( $prob, "FAILED: $cmd", $parent, *PROB ) ;
        }
    }

    return ;
}

