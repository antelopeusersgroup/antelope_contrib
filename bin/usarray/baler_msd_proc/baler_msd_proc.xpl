#
#   program needs:
#    process baler 14 data
#    if output db exists assume baler 14 data already processed
#    process unprocessed baler 44 data, appending on existing mseed files
#        make sure that time does not overlap previously processed baler data - from rsync table
#    only process "downloaded" file
#    build hash of sta mseedfile_name time of first BHZ channel
#        msdd -b -n 60 mseed
#        stop when Column 4 is BHZ
#        convert time to epoch time
#    process mseed files sorting by epoch time
#    figure out how to handle SOH data with month long files!
#        process SOH into month waveforms
#    figure out how to run miniseed2days in some sane fashion!
#        run once for BH and LH
#        run once for all others
###############################
#    Run miniseed2db in new program!
#    can only run miniseed2db on complete files!
#    process miniseed2db on complete month based chunks!
#    process data return on complete month based chunks!
#    need to modify building of rt station dbs! - Need to run on a monthly basis and in append mode!
#
#
#    DONE miniseed2days BH and LH data
#    DONE miniseed2days VH, UH, and SOH data into year files
#    DONE miniseed2db into one db
#    DONE attach dbmaster, dbops, and idserver
#    DONE verify if start and endtimes match deployment table
#
#    DONE idservers dbpath locking 
#    DONE net-sta chec check
#    DONE check to make sure correct channels are in file
#
#   check for overlaps
#   
#   check for pffile existance
#
#
    use Getopt::Std ;
    use POSIX ;    
    use strict ;
    use Datascope ;
    use archive;
    use timeslice ;
    use utilfunct ;
    use utility ;
    use sysinfo ; 
    use IO::Handle ;
    
    our ( $pgm, $host );
    our ( $opt_V, $opt_f, $opt_m, $opt_n, $opt_p, $opt_s, $opt_v );
    our ( %pf );
    
{    #  Main program

    my ( $Pf, $cmd, $db, $idle, $iowait, $kernel, $max_forks, $nchild, $ncpu, $nstas, $parent, $pid, $problems, $sta, $stime, $string, $subject, $swap, $usage, $user );
    my ( @procs, @stas, @the_rest );
    my ( %errors, %logs, %stas );


    $pgm = $0 ; 
    $pgm =~ s".*/"" ;
    elog_init( $pgm, @ARGV );
    $cmd = "\n$0 @ARGV" ;
    
    if (  ! getopts('vVnf:m:p:s:') || ( @ARGV != 1 && @ARGV != 2 ) ) { 
        $usage  =  "\n\n\nUsage: $0  [-v] [-V] [-n] " ;
        $usage .=  "[-p pf] [-m mail_to] [-f nforks] [-s sta_regex] db\n\n"  ;         
        $usage .=  "Usage: $0  [-v] [-V] [-n] " ;
        $usage .=  "[-p pf] sta parent_pid \n\n"  ;         
        elog_notify($cmd) ; 
        elog_die ( $usage ) ; 
    }

    
    $opt_v      = defined($opt_V) ? $opt_V : $opt_v ;    
    chop ($host = `uname -n` ) ;
    
    $Pf         = $opt_p || $pgm ;
    if ( @ARGV == 2 && $opt_v ) {
        $opt_v = 0 ;
        %pf = getparam( $Pf ) ;
        $opt_v = 1 ;
    } elsif ( @ARGV == 2 ) {
        %pf = getparam( $Pf ) ;
    } 

    STDOUT->autoflush(1) ;
    $parent   = $$ ;
    $problems = 0;

    if ( @ARGV == 2 ) {
        $parent = $ARGV[1] if ( $ARGV[1] ) ;
        if ( $opt_v ) {
            $opt_v = 0 ;
            %pf = getparam( $Pf ) ;
            $opt_v = 1 ;
        }        
        &proc_sta( $ARGV[0], $parent ) ;
        exit 0 ;
    }

    
    &savemail() if $opt_m ; 
    announce( 0, 0 ) ;
    elog_notify( $cmd ) ; 
    $stime = strydtime( now() ) ;
    elog_notify( "\nstarting execution on	$host	$stime\n\n" );

    $db = $ARGV[0] ;
    
    %pf = getparam( $Pf ) ;
                
    ( $ncpu, $idle, $user, $kernel, $iowait, $swap, @the_rest ) = syscpu() ;
    
    $max_forks = $opt_f || int ( $ncpu / 2 ) ;
    $max_forks = 1  if ( $max_forks < 1 ) ;
    $max_forks = 20 if ( $max_forks > 20 ) ;

    elog_notify ("ncpus	$ncpu	max_forks	$max_forks") if $opt_v ;
            
    if ( system_check(0) ) {
        $subject = "Problems - $pgm $host	Ran out of system resources";
        &sendmail($subject, $opt_m) if $opt_m ; 
        elog_die( "\n$subject" ) ;
    }
    
#        
#   subset for unprocessed data
#
    ( $problems, @stas ) = &get_stas( $db, $problems ) ;

    $nstas = $#stas + 1 ;
    
    elog_notify ( "$nstas stations to process\n\n" ) ;
    elog_notify ( "@stas\n\n" ) ;

#
#  process all new stations
#

    STA: foreach $sta ( sort ( @stas ) ) {

#
# Verify how many procs we have running
#
        @procs = check_procs(@procs) ;

#
# Read messages from pipes
#
        
        nonblock_read( \%stas, \%logs, \%errors ) ;

#
# Wait if we max out of available procs
#
        if ( scalar(@procs) >= $max_forks  ) {
            sleep 1;
            redo STA;
        }
                
        $logs{$sta}{lines}      = 0 ;
        $errors{$sta}{problems} = 0 ;
        elog_notify ( "$sta started processing" ) ;
        
#
# Fork the script
#        
        
        $pid = open($stas{$sta}{fh}, "-|") ;
        
        $stas{$sta}{fh}->autoflush(1);
        fcntl( $stas{$sta}{fh}, F_SETFL, O_NONBLOCK ) ;
        
        if ( $pid ) {
            elog_debug("PID:[$pid] sta:[$sta]") if $opt_V ;
        
            push @procs, $pid ;
            $stas{$sta}{pid} = $pid ;
        
            elog_debug("Now:[".@procs."] => @procs") if $opt_V ;
        } else {

#
# Child process
#

            $cmd   =  "baler_msd_proc " ;
            $cmd  .=  "-n " if $opt_n ;
            $cmd  .=  "-v " if $opt_v ;
            $cmd  .=  "-V " if $opt_V ;
            $cmd  .=  "-p $opt_p " if $opt_p ;
            $cmd  .=  "$sta $parent" ;
            
            fork_debug ( $parent, "$cmd " ) ;
                                    
            exec ( $cmd  ) ;

            exit 0 ;
        }
        
    }

#
# Wait for all children to finish
#
    while ( @procs ) {

        @procs = check_procs( @procs ) ;
        elog_debug("Wait for:[".@procs."] => @procs") if $opt_V ;
        &nonblock_read( \%stas, \%logs, \%errors ) ;

        sleep 1 ;

    }
    
#
# Find aborted child
#
    $string = "finished processing station" ;
    
    &missing_children ( $string, \%logs, \%errors ) ;
    
#
# Print error logs
#
    ( $nchild, $problems ) = &problem_print ( \%errors ) ;

#
# Print logs
#
    &log_print ( \%logs ) ;

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


sub get_stas { # ( $problems, @stas ) = &get_stas( $db, $problems ) ;
    my ( $db, $problems ) = @_ ;
    my ( $nrows, $row, $sta, $stas, $stas_b44, $subject ) ;
    my ( @b44_stas, @db, @dbdeploy, @sta_deploy, @stas ) ;
    
    @db       = dbopen( $db, "r" ) ;
    @dbdeploy = dblookup( @db, 0, "deployment", 0, 0) ;
    if (! dbquery( @dbdeploy, dbTABLE_PRESENT ) ) {
        $problems++ ;
        elog_complain( "\nProblem $problems" ) ;
        elog_complain( "	$db.deployment - does not exist!" ) ;
        $subject = "Problems - $pgm $host	$problems problems" ;
        &sendmail($subject, $opt_m) if $opt_m ; 
        elog_die("\n$subject") ;
    }
    
    @dbdeploy = dbsubset ( @dbdeploy, "snet =~ /$pf{net}/" ) ;
    @dbdeploy = dbsort   ( @dbdeploy, "-u", "sta" ) ;
    
    $nrows    = dbquery ( @dbdeploy, "dbRECORD_COUNT" ) ;
    
    @sta_deploy = ();
    
    for ($row = 0; $row<$nrows; $row++) {
        $dbdeploy[3] = $row ;
        $sta = dbgetv ( @dbdeploy, "sta" ) ;
        push @sta_deploy, $sta ;
    }
     
    dbclose( @db );
    
    $stas = join( "|", @sta_deploy ) ;
    
    $stas = "(" . $stas . ")" ;
    
    elog_debug( "dbdeploy stas - 	$stas" ) if $opt_V ;

    opendir( DIR, $pf{baler44dirbase} ) ;
    @b44_stas = sort ( grep { /^[A-Z0-9][A-Z0-9][A-Z0-9][A-Z0-9]$/  } readdir(DIR) ) ;
    closedir( DIR );
    
    $stas_b44 = join( " ", @b44_stas ) ;
    elog_notify( "b44_stas - 	$stas_b44\n\n" ) if $opt_v ;
    
    @stas = grep ( /$stas/, @b44_stas ) ;

    @stas = grep ( /$opt_s/, @b44_stas ) if $opt_s ;
    
    $stas = join( " ", @stas ) ;
    elog_notify( "stas     - 	$stas" ) ;

    if ( $#stas < 0 ) {
        $problems++ ;
        elog_complain( "\nProblem $problems" ) ;
        elog_complain( "	No stations to process in $pf{baler44dirbase}!" ) ;
        $subject = "Problems - $pgm $host	$problems problems" ;
        &sendmail($subject, $opt_m) if $opt_m ; 
        elog_die("\n$subject") ;
    }
    
    return ( $problems, @stas ) ;
}

# sub diskcheck { # $problems = &diskcheck( $problems );
#     my ( $problems ) = @_ ;
#     my ( $dbavail, $dbdevice, $sta, $sta_base, $sta_size, $subject ) ;
#     
#     elog_debug( "diskcheck" ) if $opt_V ;
#     
#     ( $dbdevice, $dbavail ) = &df( $pf{archivebase} );
#         
#     $sta_base = "$pf{balerdirbase}\/$sta";
#         
#     $sta_size =  `du -sk $sta_base` ;
#         
#     $sta_size =~ /^(\d+)/ ;
#         
#     $sta_size = ( $sta_size / 1024 ) + (5 * 1024) ;  # make sure 5 Gbytes free space
# 
#     if ($dbavail < $sta_size ) {
#         $problems++ ;
#         elog_complain( "Problem #$problems" );
#         elog_complain( sprintf( "Only %d available on $pf{archivebase} ", $dbavail) );
#         elog_complain( sprintf( "Need %d megabytes available ", $sta_size) );
#         $subject = "Problems - $pgm $host	$problems problems" ;
#         &sendmail($subject, $opt_m) if $opt_m ; 
#         elog_die("\n$subject") ;
#     }
#     return ($problems);
# }

sub proc_sta { # &proc_sta( $sta, $parent ) ;
    my ( $sta, $parent ) = @_ ;
    my ( $baler44dir, $bh14name, $cmd, $dirname, $error_code, $full_buf, $prob, $soh14name, $stderr_buf, $stdout_buf, $stime, $string, $subject, $success ) ;
    my ( @output ) ; 

    $stime = strydtime( now() ) ;        
    $string = "$sta - starting processing station $stime" ;
    fork_notify ( $parent, $string ) ;
    
    open( PROB, "> /tmp/prob_$sta\_$$" ) ;
    
#
#  perform existance checks
#

    $prob = 0 ;
            
    ( $dirname, $bh14name, $soh14name, $baler44dir, $prob ) = &sta_db_check( $sta, $parent, $prob  ) ;
    
    if  ( $prob )  {

        $string = "Skipping to go to next station" ;
        
        &fork_notify ( $parent, $string ) ;

        &print_prob ( $prob, $string, $parent, *PROB ) ;
        close( PROB ) ;
        
        $subject = "TA $sta     $prob baler data problems -  $pgm on $host" ;
        $cmd     = "rtmail -C -s '$subject' $pf{prob_mail} < /tmp/prob_$sta\_$$" ;
        
        ( $success, @output )  = &run_cmd( $cmd ) ;

        $stime = strydtime( now() ) ;
        
        $string = "$sta - finished processing station $stime" ;
        fork_notify ( $parent, $string ) ;
        
        unlink "/tmp/prob_$sta\_$$" unless $opt_V ;

        return ;
    }
        
#
#  make output directory
#
    if (! -d $dirname) {
        makedir( $dirname ) ;
    }
        
    chdir( $dirname ) ;
    fork_notify ( $parent, "Changed directory to $dirname" ) if $opt_v ;

    if ( (! -e $sta) && $bh14name =~ /[\w\/].*/ &&  $soh14name =~ /[\w\/].*/ ) {

        fork_notify( $parent, "Processing Baler 14 data " ) ;
        fork_debug ( $parent, "bh14name	$bh14name " ) if $opt_V ;
        fork_debug ( $parent, "soh14name	$soh14name " ) if $opt_V ;

        $prob = &baler14_proc( $sta, $bh14name, $soh14name, $parent, $prob ) ;

        if ( $prob ) {
            &print_prob ( $prob, "baler14_proc	had $prob problems", $parent, *PROB ) ;
        }
    
        if  ( -d "$pf{baler14procbase}\/$sta" && -d "$pf{baler14dirbase}\/$sta" ) {
            $prob++ ;
            &print_prob ( $prob, "$pf{baler14procbase}\/$sta and -d $pf{baler14dirbase}\/$sta both exist", $parent, *PROB ) ;
        } elsif  ( -d "$pf{baler14dirbase}\/$sta" ) {
            $cmd  = "mv $pf{baler14dirbase}\/$sta $pf{baler14procbase}";
                        
            ( $success, @output )  = &run_cmd( $cmd );

            if ( ! $success ) {
                $prob++ ;
                &print_prob ( $prob, "FAILED: $cmd", $parent, *PROB ) ;
            }
        }

    } elsif ( -d "$pf{baler14procbase}\/$sta" ) {

        fork_notify ( $parent, "Baler 14 already processed \n\n " ) ;
        print PROB "         Baler 14 already processed \n\n" ;

    }

    if  ( $prob )  {

        $string = "Skipping to go to next station" ;
        
        &fork_notify ( $parent, $string ) ;

        &print_prob ( $prob, $string, $parent, *PROB ) ;
        close( PROB ) ;
        
        $subject = "TA $sta     $prob baler data problems -  $pgm on $host";
        $cmd     = "rtmail -C -s '$subject' $pf{prob_mail} < /tmp/prob_$sta\_$$";
        
        ( $success, @output )  = &run_cmd( $cmd );

        $stime = strydtime(now());
        
        $string = "$sta - finished processing station $stime" ;
        fork_notify ( $parent, $string ) ;
        
        unlink "/tmp/prob_$sta\_$$" unless $opt_V;

        return ;
    }
                
    $prob = &baler44_proc( $sta, $baler44dir, $parent, $prob ) unless ( $prob );
            
    close(PROB);
 
#
#  clean up
#
        
    if  ( $prob ) {
        $subject = "TA $sta     $prob baler data problems -  $pgm on $host";
        $cmd     = "rtmail -C -s '$subject' $pf{prob_mail} < /tmp/prob_$sta\_$$";
        fork_notify ( $parent, "$cmd") ;
        
        ( $success, @output )  = &run_cmd( $cmd );

        if ( ! $success ) {
            $prob++ ;
            &print_prob ( $prob, "FAILED: $cmd", $parent, *PROB ) ;
        }
        
    } elsif ( $opt_v ) {
        $subject = "TA $sta baler completed successfully -  $pgm on $host";
        $cmd     = "rtmail -C -s '$subject' $pf{success_mail} < /tmp/prob_$sta\_$$";
        fork_notify ( $parent, "$cmd") if $opt_v ;   
        
        ( $success, @output )  = &run_cmd( $cmd );

        if ( ! $success ) {
            $prob++ ;
            &print_prob ( $prob, "FAILED: $cmd", $parent, *PROB ) ;
        }
        
    } 

    unlink "/tmp/prob_$sta\_$$" unless $opt_V;

    $stime = strydtime(now());
        
    $string = "$sta - finished processing station $stime" ;
    fork_notify ( $parent, $string ) ;

    return ;
}

sub sta_db_check { # ( $dirname, $bh14name, $soh14name, $baler44dir, $prob ) = &sta_db_check( $sta, $parent, $prob  ) ;
    my ( $sta, $parent, $prob ) = @_ ;
    my ( $baler44dir, $bh14name, $dbname, $dirname, $prob_check, $rtdb, $rsync, $soh14name, $string, $subject, $table ) ;
    my ( @dbtest ) ;
    
    fork_debug( $parent, "$sta - sta_db_check" ) if $opt_V ;
    
    $prob_check = $prob ;
    
    if ( ! -d $pf{archivebase} ) {
        fork_notify( $parent, "Baler 44 directory $pf{archivebase} does not exist!" ) ;
        fork_notify( $parent, "Creating $pf{baler14procbase} \n\n") ;
        makedir( $pf{archivebase} ) ;
    }
                        
    $dirname  = "$pf{archivebase}\/$sta" ;
        
    $dbname   = "$pf{archivebase}\/$sta\/$sta" ;
    fork_notify( $parent, "dirname	$dirname	dbname	$dbname" ) if ( $opt_v )  ;
    
    if (! -d "$pf{baler14procbase}" ) {
        fork_debug( $parent, "directory $pf{baler14procbase} does not exist" ) if $opt_V ;
        fork_notify( $parent, "creating $pf{baler14procbase}") ;
        makedir( $pf{baler14procbase} ) ;
        $bh14name  = "" ;
        $soh14name = "" ;
    }
    
    if (! -d "$pf{baler14dirbase}\/$sta" ) {
        fork_debug( $parent, "directory $pf{baler14dirbase}\/$sta does not exist") if $opt_V ;
        $string = "No Baler 14 data to process" ; 
        fork_notify( $parent, "$string" ) if $opt_v ;
        print PROB "$sta - $string \n\n" ;

        $bh14name  = "" ;
        $soh14name = "" ;
    } else {
        fork_debug( $parent, "directory $pf{baler14dirbase}\/$sta exists\n\n") if $opt_V ;

        $bh14name  = "$pf{baler14dirbase}\/$sta\/$pf{bhdata_dir}" ;
        $soh14name = "$pf{baler14dirbase}\/$sta\/$pf{sohdata_dir}" ;
        fork_debug( $parent, "bh14name	$bh14name") if $opt_V ;
        fork_debug( $parent, "soh14name	$soh14name") if $opt_V ;
        if (! -d $bh14name ) {
            $prob++ ;
            &print_prob ( $prob, "directory $bh14name does not exist", $parent, *PROB ) ;
        }
                        
        if ( ! -d $soh14name ) {
            $prob++ ;
            &print_prob ( $prob, "directory $soh14name does not exist", $parent, *PROB ) ;
        }
    }
    
    $baler44dir = "$pf{baler44dirbase}\/$sta" ;
    $rsync      = "$pf{baler44dirbase}\/$sta/$sta\_baler.rsyncbaler" ;
    
    if ( ! -d $baler44dir ) {
        $prob++ ;
        &print_prob ( $prob, "Baler 44 directory $baler44dir does not exist", $parent, *PROB ) ;
    }
                        
    if ( ! -e $rsync ) {
        $prob++ ;
        &print_prob ( $prob, "Baler 44 file $rsync does not exist!", $parent, *PROB ) ;
    }
                                
    return ( $dirname, $bh14name, $soh14name, $baler44dir, $prob ) ;
}

sub baler14_proc {  # $prob = &baler14_proc( $sta, $bh14name, $soh14name, $parent, $prob ) ;
    my ( $sta, $bh14name, $soh14name, $parent, $prob ) = @_ ;
    my ( $mseedfile, $string ) ;
    my ( @mseedfilesbh, @mseedfilessoh ) ;

    $string = "starting baler14_proc" ;
    fork_notify( $parent, $string ) ;
    print PROB "$string \n\n" ;

#
#  list mseed files from baler final seismic miniseed directory  
#

    opendir( DIR, $bh14name ) ;
    @mseedfilesbh = grep { /C.*\.bms.*/  } readdir( DIR ) ;
    closedir( DIR );
        
    if ( $#mseedfilesbh == -1 ) {
        $prob++ ;
        &print_prob ( $prob, "no '.*bms.*' files in $bh14name!", $parent, *PROB ) ;
        
    }
        
#
#  list mseed files from baler final soh, VH, UH miniseed directory  
#

    opendir( DIR, $soh14name );
    @mseedfilessoh = grep { /C.*\.bms.*/  } readdir( DIR );
    closedir( DIR );
                
    if ($#mseedfilessoh == -1 ) {
        $prob++ ;
        &print_prob ( $prob, "no '.*bms.*' files in $soh14name!", $parent, *PROB ) ;

    }

    if ( $prob ) {
        $string = "Skipping Baler 14 processing, going to next station" ;
        &fork_complain ( $parent, $string ) ;
        print PROB "$string \n\n" ;        
        return( $prob ) ;
    }

#
#  build station-channel-day seed files from baler final seismic miniseed directory  
#
        
    foreach $mseedfile (@mseedfilesbh) {
        $prob = msd2daysBL( $sta, $bh14name, $mseedfile, $parent, $prob ) ;
    }

#
#  build station-channel-month seed files from baler final soh, VH, UH miniseed directory  
#
    foreach $mseedfile (@mseedfilessoh) {
        $prob = msd2daysSOH( $sta, $soh14name, $mseedfile, $parent, $prob ) ;
    }

    return( $prob );
}

sub baler44_proc { # $prob = &baler44_proc( $sta, $baler44dir, $parent, $prob );
    my ( $sta, $baler44dir, $parent, $prob ) = @_ ;
    my ( $dbname, $go_to_next, $last_good, $last_mseed, $maxtime, $mintime, $msd2days_prob, $mseed, $mseeddir ) ;
    my ( $mseedfile, $ncnt, $ndfiles, $nfiles, $ngap, $nmiss, $nrows, $nsize, $prob_check, $row ) ;
    my ( $string, $suffix ) ;
    my ( @db, @dbbaler, @dbbalerd, @dbbalerdf, @dbscr, @mseed, @rows ) ;
    my ( %msd ) ;
    
    %msd = () ;
    
    $dbname = "$baler44dir/$sta\_baler" ;
    
    $string = "starting baler44_proc	$dbname" ;
    fork_notify ( $parent, $string ) if $opt_v ;
    print PROB "$string \n\n" ;
    
#
#  open database tables
#

    @db       = dbopen  ( $dbname, 'r+' );
    @dbbaler  = dblookup( @db, 0, "rsyncbaler", 0, 0) ;
    @dbscr    = dblookup( @dbbaler, 0, 0, 0,"dbSCRATCH") ; 
    $nrows    = dbquery ( @dbbaler, "dbRECORD_COUNT" ) ;

    fork_debug   ( $parent, "$nrows in rsyncbaler") if $opt_V ;
    
#
#  find unique mseed files in table
#

    @dbbalerdf = dbsort  ( @dbbaler, "-u", "dfile") ;
    $ndfiles   = dbquery ( @dbbalerdf, "dbRECORD_COUNT" );

    $string = "$ndfiles rsyncbaler dfiles that are unique " ;
    fork_notify ( $parent, $string ) if $opt_v ;
    print PROB "$string \n" ;
    
    $prob_check = $prob ;

    ( $ngap, $last_good, $prob, @dbbalerd ) = &dnld_check( $ndfiles, \@dbbaler, \@dbbalerdf, $parent, $prob ) ;
    $nrows    = dbquery ( @dbbalerd, "dbRECORD_COUNT" ) ;
    
    elog_debug ( "dnld_check return	$ngap	$last_good	$prob" ) if $opt_V ;
    
    if ( $nrows == 0 ) {
        $string =  sprintf( "%5d baler44 files to process", $nrows )  ;
        fork_notify ( $parent, $string ) ;
        print PROB "\n$string \n" ;

        dbclose( @db );
    
        return( $prob );
    
    }

#
#  loop over all downloaded mseed files which have not been processed
#

    $nsize = 0 ;
    
    for ($row = 0; $row<$nrows; $row++) {
    
        $dbbalerd[3] = $row ;
    
#
#  skip files with wrong sta code in mseed
#

        $mseed = dbextfile( @dbbalerd ) ;
        ( $go_to_next, $nsize, $prob ) = &mseedcheck( $mseed, $nsize, $parent, $prob ) ;
        
        next if $go_to_next ;
            
#
#  run msdd on mseed file to get a listing of the contents
#

        ( $msd{$sta}{$mseed}{first}, $go_to_next, $prob ) = &msdd( $sta, $mseed, $parent, $prob ) ;

        next if $go_to_next ;


    }
    
    elog_debug ( "downloaded mseed files	$prob" ) if $opt_V ;
    
    prettyprint( \%msd ) if $opt_V ; 

#
#  sort mseed files by start time
#        

    @mseed = sort keys %{$msd{$sta}} ;
    
#
#  Identify if any missing files
#
    $nfiles = 0 ;
    $last_mseed = "" ;
    elog_debug ( "before missing_files	$prob	$nfiles	$last_mseed" ) if $opt_V ;
    foreach $mseed ( 1..$#mseed ) {

        ( $nmiss, $ngap, $prob ) = &missing_files( $mseed[ $mseed - 1 ], $mseed[ $mseed ], $ngap, \@dbbalerd, \@dbbalerdf, $parent, $prob ) ;

        if ( $nmiss > 0 ) {
            fork_notify   ( $parent, "$nmiss missing files,  $ngap gaps,  last_mseed  $mseed[ $mseed - 1 ] " )  ;
            if ( length( $last_mseed ) == 0 ) {
                $last_mseed = $mseed[ $mseed - 1 ] ;
            }
        }
                
        $nfiles = $nfiles + $nmiss ; 
        
    }
    elog_debug ( "missing_files	$prob	$nfiles	$last_mseed" ) if $opt_V ;
#
#  Check to make sure output waveform data does not exist
#
    if ( -e "$sta.wfdisc" ) {
        ( $mintime, $maxtime, $prob ) = &sta_time_check( $sta, $parent, $prob ) ;
        fork_notify ( $parent, sprintf( "	Baler processed  %s    %s", strydtime( $mintime ), strydtime( $maxtime ) ) )  ;
        fork_notify ( $parent, sprintf( "	Baler new        %s    %d", strydtime( $msd{$sta}{$mseed[0]} ), $nrows ) )  ;
        if ( $msd{$sta}{$mseed[0]}{first} < $maxtime && $msd{$sta}{$mseed[0]}{first} > epoch(2004001) && $prob_check == $prob ) {
            $prob++ ;
            &print_prob ( $prob, "New baler download data starts before last processed data - Should be after", $parent, *PROB ) ;

            $string =   sprintf( "	Download start  %s	", strydtime( $msd{$sta}{$mseed[0]} ) )  ;
            fork_complain ( $parent, $string ) ;
            print PROB "         $string \n" ;
            $string =   sprintf( "	Db Maxtime      %s",  strydtime( $maxtime ) )  ;
            fork_complain ( $parent, $string ) ;
            print PROB "         $string \n\n" ;
        }
    } 

    fork_debug   ( $parent, "prob_check	$prob_check	problems	$prob	ngap	$ngap" ) if $opt_V ;

    elog_debug ( "pre-skip	$prob	$nfiles	$last_mseed" ) if $opt_V ;

    if ( $prob != $nfiles ) {  # continue if only problems are undownloaded files
        $string = "Skipping Baler 44 processing, going to next station" ;
        fork_complain ( $parent, $string ) ;
        print PROB "\n$string \n\n" ;        
        $string =  sprintf( "%d baler44 files smaller than 4096000 bytes", $nsize )  ;
        fork_complain ( $parent, $string ) ;
        print PROB "$string \n\n" ;
        return( $prob ) ;
    }
    
#
#  run miniseed2days 
#
    $ncnt = 0 ;
    $msd2days_prob = $prob ; 
    
    &write_fix_miniseed_pf( $sta ) ;

    $string = "First mseed file processed was	$mseed[0] ";
    fork_notify ( $parent, $string ) ;
    print PROB "\n$string \n\n" ;

    foreach $mseed ( @mseed ) {
    
        ( $mseeddir, $mseedfile, $suffix ) = parsepath( $mseed ) ;
        
        if ( $opt_V ) {
            fork_debug   ( $parent, sprintf("$sta - %s	%s	%s	%s", $mseed, $mseeddir, $mseedfile, $suffix ) )  ;
        } elsif ( $opt_v && ! ( $ncnt % 10 ) ) {
            fork_notify ( $parent, sprintf("%s	%d", $mseed, $ncnt ) )  ;
        }
        
        $prob = fix_miniseed( $mseeddir, $mseedfile, $parent, $prob ) ;
        $prob = msd2daysBL(  $sta, $mseeddir, $mseedfile, $parent, $prob ) ; 
        $prob = msd2daysSOH( $sta, $mseeddir, $mseedfile, $parent, $prob ) ;
        
        if ( $msd2days_prob != $prob ) {
            $string = "Last mseed file processed was	$mseed ";
            fork_notify ( $parent, $string ) ;
            print PROB "\n$string \n\n" ;
            last ; 
        }
        
        dbputv( @dbscr, "dfile", $mseedfile, "status", "downloaded" ) ;
        @rows = dbmatches( @dbscr, @dbbaler, "sync_$sta", "dfile", "status") ;
        $dbbaler[3] = $rows[0] ;
        dbputv( @dbbaler, "msdtime", now() ) unless $opt_n ;
        
        $ncnt++ ;
        
        fork_debug   ( $parent, "$last_mseed	$last_good	$mseed") if $opt_V ;
        if ( ( $last_mseed =~ /$mseed/ ) || ( $last_good =~ /$mseed/ ) ) {
            last ; 
        }
                
# last if ($ncnt == 500);
    }

    $string = "Last mseed file processed was	$mseed ";
    fork_notify ( $parent, $string ) ;
    print PROB "\n$string \n\n" ;
    
    unlink ( "fix_miniseed_TA_net_sta_chan_loc.pf" ) ;
    unlink ( "fix_miniseed_TA_sta.pf" ) ;
        
    $string =  sprintf( "%5d baler44 files processed", $ncnt )  ;
    fork_notify ( $parent, $string ) ;
    print PROB "\n$string \n" ;

    $string =  sprintf( "%5d baler44 files smaller than 4096000 bytes", $nsize )  ;
    fork_notify ( $parent, $string ) ;
    print PROB "\n$string \n" ;

    dbclose( @db );
    
    return( $prob );

}

sub dnld_check { # ( $ngap, $last_good, $prob, @dbbalerd ) = &dnld_check( $ndfiles, \@dbbaler, \@dbbalerdf, $parent, $prob ) ;
    my ( $ndfiles, $refb, $reff, $parent, $prob ) = @_ ;
    my ( $dfile, $dfile_check, $last_good, $last_proc, $mseed, $mseed_offset, $ndown, $ndownu, $next_proc, $ngap, $ngap_at_start, $nmiss ) ;
    my ( $nproc, $nrows, $nzfiles, $offset, $row, $string ) ;
    my ( @dbbaler, @dbbaler_proc, @dbbalerd, @dbbalerdf, @dbbalerdn, @dbbalerdu, @dbbalerz, @mseed, @mseed_offset ) ;
    my ( %dfile ) ;
        
    @dbbaler   = @$refb ;      #  rsyncbaler table
    @dbbalerdf = @$reff ;      #  rsyncbaler view of dfiles that are unique

#
#  check times of known mseed files
#
    $nrows    = dbquery ( @dbbalerdf, "dbRECORD_COUNT" );
    
    %dfile = ();
    
    for ($row = 0; $row<$nrows; $row++) {
        $dbbalerdf[3]    = $row ; 
        $dfile           = dbextfile( @dbbalerdf ) ;
        $dfile{ $dfile } = &mseedtime( $dfile, $parent, $prob ) ;
    }

    @mseed        = sort keys %dfile ;
    @mseed_offset = () ;
    
    $last_good    = "" ;
    $ngap         = 0 ; 

    foreach $mseed ( 1..$#mseed ) {
        $mseed_offset = $dfile{ $mseed[ $mseed ] } - $dfile{ $mseed[ $mseed -1 ] } ;
        if ( $mseed_offset > 86400 ) {
            $ngap++ ;
            $string =  sprintf( "%s between	%s and	%s, should be ~8 hours", strtdelta( $mseed_offset ), $mseed[ $mseed -1 ],  $mseed[ $mseed ] )  ;
            fork_debug ( $parent, "last_good  $last_good " ) if $opt_V ;
            if ( length( $last_good ) == 0 ) {
                $last_good = $mseed[ $mseed - 1 ] ;
            }
        }
        push @mseed_offset, $mseed_offset ;
    }
    
    if ($opt_V ) {
        foreach $offset ( sort { $b <=> $a } @mseed_offset ) {
            fork_notify ( $parent, sprintf ("%s", strtdelta( $offset ) ) ) ;
        }
    }

#
#  find downloaded mseed files
#

    @dbbalerd = dbsubset( @dbbaler, "status =~ /downloaded/ ") ;
    $ndown    = dbquery ( @dbbalerd, "dbRECORD_COUNT" );

    $string = "$ndown rsyncbaler dfiles with status =~ /downloaded/ " ;
    fork_notify ( $parent, "$string" ) if $opt_v ;
    print PROB "$string \n" ;

    @dbbalerdu = dbsort  ( @dbbalerd, "-u", "dfile") ;
    $ndownu    = dbquery ( @dbbalerdu, "dbRECORD_COUNT" );
    %dfile = ();

    if ( $ndownu != $ndown ) {
        for ($dbbalerd[3] = 0; $dbbalerd[3]<$ndown; $dbbalerd[3]++) {
            $dfile = dbgetv( @dbbalerd, "dfile" ) ;
            $dfile_check = dbextfile( @dbbalerd ) ;
            if ( exists $dfile{ $dfile } ) {
                $prob++ ;
                $string =  sprintf( "extra row found in rsync baler match for %s, should have 1", $dfile )  ;
                &print_prob ( $prob, $string, $parent, *PROB ) ;
            } elsif (-s $dfile_check ) {   # skip zero length files
                $dfile{ $dfile } = 1 ;
            }
        }
    }
    
    @dbbaler_proc = dbsubset ( @dbbalerd , "msdtime != NULL") ;
    $nproc        = dbquery  ( @dbbaler_proc, "dbRECORD_COUNT" ) ;
    
    $string = "$nproc rsyncbaler dfiles with status =~ /downloaded/  && msdtime != NULL" ;

    if ( $nproc > 0 ) {
        @dbbaler_proc = dbsort ( @dbbaler_proc, "-r", "dfile" ) ;
        $dbbaler_proc[3] = 0 ;
        $last_proc = dbgetv ( @dbbaler_proc, "dfile" );
    }

#
#  find downloaded mseed files which have not been processed
#

    @dbbalerdn = dbsubset( @dbbalerd,  "( msdtime == NULL )" ) ;
    @dbbalerdn = dbsort  ( @dbbalerdn, "dfile" ) ;
    $nrows     = dbquery ( @dbbalerdn, "dbRECORD_COUNT" ) ;

    $string = "$nrows rsyncbaler dfiles with status =~ /downloaded/  && msdtime == NULL" ;
    fork_notify ( $parent, $string ) if $opt_v ;
    print PROB "$string \n\n" ;
    
    $ngap_at_start = 0 ;
    if ( $nproc > 0 && $nrows > 0 ) {
        $dbbalerdn[3] = 0 ;
        $next_proc = dbgetv ( @dbbalerdn, "dfile" ) ;
        ( $nmiss, $ngap_at_start, $prob ) = &missing_files( $last_proc, $next_proc, $ngap_at_start, \@dbbalerd, \@dbbalerdf, $parent, $prob ) ;
    }
    
#
#  find number of zero length files
#

    @dbbalerz = dbsubset( @dbbalerdn, "filebytes <= 0") ;
    $nzfiles  = dbquery ( @dbbalerz, "dbRECORD_COUNT" );

    $string = "$nzfiles rsyncbaler dfiles with filebytes <= 0" ;
    fork_notify ( $parent, $string ) if $opt_v ;
    print PROB "$string \n\n" ;
    
#
#  find number of files not downloaded
#

    if ( $ndfiles != $ndown ) {
        $string = sprintf( "%d files not downloaded", $ndfiles - $ndown )  ;
        fork_complain ( $parent,  $string  ) if $opt_v ;
        print PROB "$string \n\n" ;
    }
    return ( $ngap, $last_good, $prob, @dbbalerdn  ) ;
}

sub mseedtime { # ( $mseedtime ) = &mseedtime( $mseed, $parent, $prob ) ;
    my ( $mseed, $parent, $prob ) = @_ ;
    my ( $day, $hour, $minute, $month, $mseedtime, $second, $tstring, $year ) ;
    my ( @tstring ) ;
#
#  generate time string of previous file from mseed file name
#

    @tstring = split /-/, $mseed;
    fork_debug   ( $parent, "@tstring" ) if $opt_V ;
    $year     = substr( $tstring[2],  0, 4 ) ;
    $month    = substr( $tstring[2],  4, 2 ) ;
    $day      = substr( $tstring[2],  6, 2 ) ;
    $hour     = substr( $tstring[2],  8, 2 ) ;
    $minute   = substr( $tstring[2], 10, 2 ) ;
    $second   = substr( $tstring[2], 12, 2 ) ;
        
    fork_debug   ( $parent, "$mseed	$year	$month	$day	$hour	$minute	$second" ) if $opt_V ;
        
    $tstring = "$year-$month-$day $hour:$minute:$second" ;
        
    fork_debug   ( $parent, $tstring ) if $opt_V ;
    
    $mseedtime = str2epoch ( $tstring ) ;
    
    return $mseedtime ;
    
}

sub mseedcheck { # ( $go_to_next, $nsize, $prob ) = &mseedcheck( $mseed, $nsize, $parent, $prob ) ;
    my ( $mseed, $nsize, $parent, $prob ) = @_ ;
    my ( $filesize, $go_to_next, $string ) ;
    
    $go_to_next = 0 ;
    if ( ! -f $mseed ) {
        $prob++ ;
        $string =   "$mseed does not exist!"  ;
        &print_prob ( $prob, $string, $parent, *PROB ) ;
        
        $go_to_next = 1 ;
        
        return ( $go_to_next, $nsize, $prob ) ;
    }
      
#
#  identify non-standard length files
#

    $filesize = -s $mseed ;
    if ( $filesize != 4096000 ) {
        if ( $opt_V ) {
            $string =   "$mseed"  ;
            fork_debug   ( $parent, $string ) ;
            print PROB "\n$string \n" ;

            $string =   sprintf("size     %8d", $filesize)  ;
            fork_debug   ( $parent, $string ) ;
            print PROB "$string \n" ;

            $string =   sprintf("expected %8d", 4096000)  ;
            fork_debug   ( $parent, $string ) ;
            print PROB "$string \n" ;
        }
            
        $nsize++ ;
    }
    fork_debug   ( $parent, $mseed ) if $opt_V ;
    
#
#  identify zero length files
#

    if ( $filesize == 0 ) {
        $string =   sprintf("%s has zero length", $mseed)  ;
        fork_notify ( $parent, $string ) ;
        print PROB "$string \n" ;
        $go_to_next = 1 ;
    }
    
    return ( $go_to_next, $nsize,  $prob ) ;
}

sub msdd { # ( $time, $go_to_next, $prob ) = &msdd( $sta, $mseed, $parent, $prob ) ;
    my ( $sta, $mseed, $parent, $prob ) = @_ ;
    my ( $cmd, $go_to_next, $key, $line, $string, $success, $time ) ;
    my ( @keys, @line, @msdd, @msdd_BHZ, @tmp) ;
    my ( %chan ) ;
    
    $go_to_next = 0 ;
    $time       = 0 ;
    
    $cmd = "msdd -b $mseed" ;
    fork_debug   ( $parent, $cmd ) if $opt_V ;
    
    if ( $opt_n ) { 
        $opt_n = 0 ;
        ( $success, @msdd )  = &run_cmd( $cmd ) ;
        $opt_n = 1 ;
    } else {
        ( $success, @msdd )  = &run_cmd( $cmd ) ;
    }

    if ( ! $success ) {
        $prob++ ;
        &print_prob ( $prob, "FAILED: $cmd", $parent, *PROB ) ;
        $go_to_next = 1 ;
        return ( $time, $go_to_next, $prob ) ;
    }
        
    $string = join( "", @msdd) ;            #  join all msdd output into one string
    @msdd     = split( /\R/, $string ) ;    #  split string by lines    
    @line     = split (" ", $msdd[$#msdd] );
    @msdd     = grep ( /$sta|EXMP/ , @msdd ) ;   #  keep only lines with sta code

    if ( $#msdd == -1 ) {   # no station data in mseed file
        $string = sprintf( "Mseed file %s is station %s, not the expected %s", $mseed, $line[3], $sta ) ;
        fork_complain ( $parent, $string ) ;
        print PROB "$string \n\n" ;
        $go_to_next = 1 ;
        return ( $time, $go_to_next, $prob ) ;
    }
            
    @tmp      = split (" ", $msdd[0] );
    shift @msdd if ( $#tmp < 10 ) ;         #  remove non-data first line if needed. 
    @msdd_BHZ = grep ( /BHZ/  , @msdd ) ;   #  grab all BHZ lines
    
#  
#  sanity checks on mseed file
#

    %chan = () ;
        
    foreach $line ( @msdd ) {   # extract all blockett times and count channels
        @line     = split (" ", $line );
        if ( ! exists $chan{ $line[4] } ) { # first time channel is found in mseed file
            $chan{ $line[4] }{count} = 1 ;
            if ( $line[14] =~ /B.*/ ) {           # find first chan time - no loc code
                $chan{ $line[4] }{time} = str2epoch( join( " ", @line[5,6,7] ) ) ;
                $chan{ $line[4] }{last} = $chan{ $line[4] }{time} ;
            } elsif ( $line[15] =~ /B.*/ ) {      # find first chan time - with loc code
                $chan{ $line[4] }{time} = str2epoch( join( " ", @line[6,7,8] ) );
                $chan{ $line[4] }{last} = $chan{ $line[4] }{time} ;
            }
        } else {
            $chan{ $line[4] }{count}++ ;
            if ( $line[14] =~ /B.*/ ) {           # find first chan time - no loc code
                $chan{ $line[4] }{last} = str2epoch( join( " ", @line[5,6,7] ) ) ;
            } elsif ( $line[15] =~ /B.*/ ) {      # find first chan time - with loc code
                $chan{ $line[4] }{last} = str2epoch( join( " ", @line[6,7,8] ) );
            }
        }
    }
    
    if ( $#msdd_BHZ == -1 ) {   # no BHZ data in mseed file
        
        $string = "No BHZ blocketts found in $mseed" ;
        fork_notify ( $parent, $string ) ;
        print PROB "$string \n" ;

#  
#  sort largest to smallest number of blocketts for each channel
#
        if ( $opt_V ) {
            foreach $key ( sort { $chan{$b}{count} <=> $chan{$a}{count} } keys %chan ) {
                $string = sprintf ("%s	has %4d blocketts - %s", $key, $chan{$key}{count}, strydtime($chan{$key}{time})) ;
                fork_complain ( $parent, $string ) ;
                print PROB "$string \n" ;
            }
        }

#
#  find latest start time of any channel in mseed file
#
        @keys = sort { $chan{$b}{time} <=> $chan{$a}{time} } keys %chan ;
        $string = sprintf( "%s	has latest start time %s", $keys[0], strydtime( $chan{ $keys[0] }{time} ) ) ;
        fork_debug   ( $parent, $string ) if $opt_V ;
        print PROB "$string \n" if $opt_V ;
            
        $time  = $chan{ $keys[0] }{time} ;
            
    } else {    #  extract time of first BHZ blockett
        
        $line = $msdd_BHZ[0] ;
        @line = split (" ", $line );
        if ( $line[14] =~ /B.*/ ) {        # find first chan time - no loc code
            $time = str2epoch( join( " ", @line[5,6,7] ) ) ;
        } elsif ( $line[15] =~ /B.*/ ) {   # find first chan time - with loc code
            $time  = str2epoch( join( " ", @line[6,7,8] ) ) ;
        }
    }
    
    return ( $time, $go_to_next, $prob ) ;

}

sub missing_files { # ( $nmiss, $ngap, $prob ) = &missing_files( $mseed1, $mseed2, $ngap, \@dbbalerd, \@dbbalerdf, $parent, $prob ) ;
    my ( $mseed1, $mseed2, $ngap, $refd, $reff, $parent, $prob ) = @_ ;
    my ( $dfile, $msd1, $msd2, $mseeddir, $nbytes, $nmiss, $nzerolength ) ;
    my ( $row, $row_1, $row_2, $string, $suffix ) ;
    my ( @dbbalerd, @dbbalerdf ) ;

    @dbbalerd  = @$refd ;
    @dbbalerdf = @$reff ;

#
#  get row numbers for missing mseed files still on baler 44
#

    ( $mseeddir, $msd1, $suffix ) = parsepath( $mseed1 ) ;
    ( $mseeddir, $msd2, $suffix ) = parsepath( $mseed2 ) ;
    $row_1  = dbfind ( @dbbalerdf, " dfile =~ /$msd1/ ", -1 ) ;
    $row_2  = dbfind ( @dbbalerdf, " dfile =~ /$msd2/ ", -1 ) ;
        
                
    $nmiss  = $row_2  - $row_1  - 1 ;
    $nzerolength = 0 ;

    fork_debug   ( $parent, "nmiss  $nmiss	row_2  $row_2	row_1  $row_1 " ) if $opt_V ;
    
#
#  identify missing files
#

    if ( $nmiss > 0 ) {
        fork_debug   ( $parent, "nmiss  $nmiss	row_2  $row_2	row_1  $row_1 " ) if $opt_V ;
        foreach $row ( ( $row_1 + 1 )..( $row_2 - 1 ) ) {
            fork_debug   ( $parent, "row  $row" ) if $opt_V ;
            $dbbalerdf[3] = $row ;
            $dfile        = dbgetv( @dbbalerdf, "dfile" ) ;
            fork_debug   ( $parent, "dfile  $dfile" ) if $opt_V ;
            $dbbalerd[3] = dbfind ( @dbbalerd, " dfile =~ /$dfile/", -1 ) ;
            fork_debug   ( $parent, "dbbalerd[3]  $dbbalerd[3]" ) if $opt_V ;
            next if ( $dbbalerd[3] < 0 ) ;
            $nbytes = dbgetv( @dbbalerd, "filebytes" ) ;
            fork_debug   ( $parent, "row	$row	nbytes	$nbytes") if $opt_V ; 
            $nzerolength++ if ( dbgetv( @dbbalerd, "filebytes" ) == 0 );
        }
        fork_debug   ( $parent, "row_1	$row_1	row_2	$row_2	nzerolength	$nzerolength") if $opt_V ;
            
        if ( $nmiss != $nzerolength ) {
            $prob++ ;
            $ngap++ ;
            
            $string =  sprintf( "Missing baler 44 mseed files!" )  ;
            &print_prob ( $prob, $string, $parent, *PROB ) ;
                        
            $string =  sprintf( "Number of files not downloaded - %d", $nmiss )  ;
            fork_complain ( $parent, $string ) ;
            print PROB "         $string \n" ;
            if  ( $opt_v ) {
                $string =  sprintf( "$msd1	$row_1", $nmiss )  ;
                fork_complain ( $parent, $string ) ;
                print PROB "         $string \n" ;
                $string =  sprintf( "$msd2	$row_2", $nmiss )  ;
                fork_complain ( $parent, $string ) ;
                print PROB "         $string \n\n" ;
            }
            for ($dbbalerdf[3] = $row_1 + 1; $dbbalerdf[3] < $row_2 ; $dbbalerdf[3]++) {
                $dfile = dbgetv( @dbbalerdf, "dfile" ) ;
                $string =  sprintf( "%s    is missing", $dfile )  ;
                fork_complain ( $parent, $string ) ;
                print PROB "         $string \n" ;
            }
        }
    }
    return ( $nmiss, $ngap, $prob ) ;
}

sub write_fix_miniseed_pf { # &write_fix_miniseed_pf( $sta ) ;
    my ( $sta ) = @_ ;
    my ( $element ) ;
    open( FIX, ">fix_miniseed_TA_net_sta_chan_loc.pf" ) ;
    print FIX "net_sta_chan_loc        &Tbl{ \n" ;
    foreach $element ( @{$pf{net_sta_chan_loc}} ) {
        $element =~ s/\$/\\\$/g ;
        print FIX "    $element \n" ;
    }
    print FIX "}\n" ;
    close FIX ;
    
    open( FIX, ">fix_miniseed_TA_sta.pf" ) ;
    print FIX "sta        &Tbl{ \n" ;
    foreach $element ( @{$pf{sta}} ) {
        $element =~ s/CURRENT_STATION/$sta/ ;
        print FIX "    $element \n" ;
    }
    print FIX "}\n" ;
    close FIX ;
}    

sub fix_miniseed { # $prob = &fix_miniseed( $dir, $mseedfile, $parent, $prob ) ;
    my ( $dir, $mseedfile, $parent, $prob ) = @_ ;
    my ( $cmd, $error_code, $full_buf, $stderr_buf, $stdout_buf, $success ) ;
    my ( @output ) ;
    
    fork_debug   ( $parent, "starting fix_miniseed" ) if $opt_V ;
#
#  put md5 check here if needed.
#
    $cmd  = "fix_miniseed -p fix_miniseed_TA_net_sta_chan_loc $dir/$mseedfile ";

    ( $success, @output )  = &run_cmd( $cmd );

    if ( ! $success ) {
        $prob++ ;
        &print_prob ( $prob, "FAILED: $cmd", $parent, *PROB ) ;
    }
        
    @output = () ;
#
#  put md5 check here if needed.
#
    $cmd  = "fix_miniseed -p fix_miniseed_TA_sta $dir/$mseedfile ";

    ( $success, @output )  = &run_cmd( $cmd );

    if ( ! $success ) {
        $prob++ ;
        &print_prob ( $prob, "FAILED: $cmd", $parent, *PROB ) ;
    }
        
    @output = () ;
#
#  put md5 check here if needed.  Update status in rsync_baler row which is being modified, currently "downloaded"
#

    return ( $prob ) ;
}

sub msd2daysBL { # $prob = &msd2daysBL( $sta, $dir, $mseedfile, $parent, $prob ) ;
    my ( $sta, $dir, $mseedfile, $parent, $prob ) = @_ ;
    my ( $cmd, $error_code, $full_buf, $stderr_buf, $stdout_buf, $success ) ;
    my ( @output ) ;
    
    fork_debug   ( $parent, "starting msd2daysBL" ) if $opt_V ;

    $cmd  = "miniseed2days -c -U -m \"$pf{net}\_$sta\_[HBL][HDN]._.*\" ";
    $cmd .= " - < $dir/$mseedfile ";

    ( $success, @output )  = &run_cmd( $cmd );

    if ( ! $success ) {
        $prob++ ;
        &print_prob ( $prob, "FAILED: $cmd", $parent, *PROB ) ;
    }
        
    @output = () ;

    return ( $prob ) ;
}

sub msd2daysSOH { # $prob = &msd2daysSOH( $sta, $dir, $mseedfile, $parent, $prob ) ;
    my ( $sta, $dir, $mseedfile, $parent, $prob ) = @_ ;
    my ( $cmd, $error_code, $full_buf, $stderr_buf, $stdout_buf, $success ) ;
    my ( @output ) ;
    
    fork_debug   ( $parent, "starting msd2daysSOH" ) if $opt_V ;
    
    $cmd  = "miniseed2days -c -U -m \"$pf{net}\_$sta\_.*\" -r \".*_.*_[HBL][HDN]._.*\" -w %Y/month_files/%m/%{net}_%{sta}_%{chan}_%{loc}.msd ";
    $cmd .= " - < $dir/$mseedfile ";
    
    ( $success, @output )  = &run_cmd( $cmd );

    if ( ! $success ) {
        $prob++ ;
        &print_prob ( $prob, "FAILED: $cmd", $parent, *PROB ) ;
    }
        
    @output = () ;
    
    return ( $prob ) ;
}

sub sta_time_check { # ( $mintime, $maxtime, $prob ) = &sta_time_check( $sta, $parent, $prob ) ;
    my ( $sta, $parent, $prob ) = @_ ;
    my ( $dbname, $etime, $maxtime, $maxtime_baler, $maxtime_rt, $mintime, $mintime_baler, $mintime_rt, $rtdb, $stime ) ;
    my ( @db, @dbbh, @dbrt, @dbwfdisc ) ;
    
    fork_debug   ( $parent, "starting sta_time_check" ) if $opt_V ;
    
    @db       = dbopen(  $sta, 'r' ) ;
    @dbwfdisc = dblookup( @db, 0, "wfdisc", 0, 0 ) ;
    @dbbh     = dbsubset( @dbwfdisc, "sta =~/$sta/ && chan =~ /[BL]H[ZNE]/" ) ;
    if (dbquery( @dbbh, "dbRECORD_COUNT" ) == 0 ) {
        $prob++ ;
        fork_complain ( $parent, "\n	$dbname\.wfdisc has no chan =~ /[BL]H[ZNE]/! 
                       \n	Skipping to next station") ;
    }
    $mintime  = dbex_eval( @dbbh, "min(time)" ) ;
    $maxtime  = dbex_eval( @dbbh, "max(endtime)" ) ;
    dbclose( @db ) ;
            
    fork_debug   ( $parent, sprintf( "Baler times  %s    %s", strydtime($mintime), strydtime($maxtime) ) ) if $opt_V ;
    return ( $mintime, $maxtime, $prob ) ;
}




