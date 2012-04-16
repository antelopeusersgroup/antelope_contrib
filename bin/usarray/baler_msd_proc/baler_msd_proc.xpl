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
#    DONE miniseed2days VH, UH, and SOH data into month files
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
    use POSIX ;    
    use strict ;
    use Datascope ;
    use archive;
    use timeslice ;
    use utilfunct ;
    use utility ;
    use sysinfo ;
    use Getopt::Std ;
    use IO::Handle ;
    use Digest::MD5 qw[md5_hex];
    
    our ( $pgm, $host );
    our ( $opt_B, $opt_E, $opt_F, $opt_G, $opt_V, $opt_c, $opt_d, $opt_f, $opt_m, $opt_n, $opt_p, $opt_s, $opt_v );
    our ( %pf );
    
{    #  Main program

    my ( $Pf, $cmd, $db, $idle, $iowait, $kernel, $max_forks, $nchild, $ncpu, $nstas, $parent ) ;
    my ( $pid, $problems, $sta, $stime, $string, $subject, $swap, $usage, $user ) ;
    my ( @procs, @the_rest ) ;
    my ( %errors, %logs, %proc_stas, %stas ) ;


    $pgm = $0 ; 
    $pgm =~ s".*/"" ;
    elog_init( $pgm, @ARGV );
    $cmd = "\n$0 @ARGV" ;
    
    if (  ! &getopts('vVncdBEFGf:m:p:s:') || ( @ARGV != 1 && @ARGV != 4 ) ) { 
        $usage  =  "\n\n\nUsage: $0  [-v] [-V] [-n] [-c] [-d] [-F] [-B] [-E] [-G] " ;
        $usage .=  "[-p pf] [-m mail_to] [-f nforks] [-s sta_regex] db\n\n"  ;         
        $usage .=  "Usage: $0  [-v] [-V] [-n] [-c] [-d] [-F] [-B] [-E] [-G] " ;
        $usage .=  "[-p pf] sta install_time removal_time parent_pid \n\n"  ;         
        elog_notify($cmd) ; 
        elog_die ( $usage ) ; 
    }

    
    $opt_v      = defined($opt_V) ? $opt_V : $opt_v ;    
    chop ($host = `uname -n` ) ;
    
    $Pf         = $opt_p || $pgm ;
    if ( @ARGV == 4 && $opt_v ) {
        $opt_v = 0 ;
        %pf = getparam( $Pf ) ;
        $opt_v = 1 ;
    } elsif ( @ARGV == 4 ) {
        %pf = getparam( $Pf ) ;
    } 

    STDOUT->autoflush(1) ;
    $parent   = $$ ;
    $problems = 0;

    if ( @ARGV == 4 ) {
        $parent = $ARGV[3] if ( $ARGV[3] ) ;
        if ( $opt_v ) {
            $opt_v = 0 ;
            %pf = getparam( $Pf ) ;
            $opt_v = 1 ;
        }        
        &proc_sta( $ARGV[0], $ARGV[1], $ARGV[2], $parent ) ;
        exit 0 ;
    }

    
    &savemail() if $opt_m ; 
    announce( 0, 0 ) ;
    elog_notify( $cmd ) ; 
    $stime = strydtime( now() ) ;
    elog_notify( "\nstarting execution on	$host	$stime\n\n" ) ;
    
#     &md5_check () ;

    $db = $ARGV[0] ;
    
    %pf = getparam( $Pf ) ;
                
    $opt_s  = $opt_s || $pf{day_of_week}{epoch2str( now(), "%A" )} ;
    
    elog_notify( "station subset is $opt_s" ) ;
    
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
    ( $problems, %proc_stas ) = &get_stas( $db, $problems ) ;

    $nstas = ( scalar keys %proc_stas ) ;
    
    elog_notify ( "$nstas stations to process\n\n" ) ;
    
#
#  process all new stations
#

    $stime = now() ;
    
    STA: foreach $sta ( sort ( keys %proc_stas ) ) {
    
        last if ( now() - $stime >  ( 3600 * $pf{limit_hours} ) ) ;  #  exit process if running more than 18 hours.

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
            $cmd  .=  "-c " if $opt_c ;
            $cmd  .=  "-d " if $opt_d ;
            $cmd  .=  "-F " if $opt_F ;
            $cmd  .=  "-B " if $opt_B ;
            $cmd  .=  "-E " if $opt_E ;
            $cmd  .=  "-G " if $opt_G ;
            $cmd  .=  "-p $opt_p " if $opt_p ;
            $cmd  .=  "$sta " ;
            $cmd  .=  sprintf("\" %s \" ", 
                              epoch2str( $proc_stas{$sta}{equip_install}, "%Y%j %H:%M:%S" ) ); 
            if ( $proc_stas{$sta}{equip_remove} > 9999999999. ) {
                $cmd  .=   "$proc_stas{$sta}{equip_remove} " ; 
            } else {
                $cmd  .=  sprintf("\" %s \" ", 
                              epoch2str( $proc_stas{$sta}{equip_remove}, "%Y%j %H:%M:%S" ) ) ; 
            }
            $cmd  .=  "$parent" ;
            
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
#         elog_debug("Wait for:[".@procs."] => @procs") if $opt_V ;
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
    elog_notify ( "completed 	$stime\n\n" ) ;

    if ($problems == 0 ) {
        $subject = sprintf( "Success  $pgm  $host  $nstas stations processed" ) ;
        elog_notify ( $subject );
    } else { 
        $subject = "Problems - $pgm $host	$nstas stations processed, $nchild stations with problems, $problems total problems" ;
        elog_complain( "\n$subject" ) ;
    }
    
    &sendmail ( $subject, $opt_m ) if $opt_m ;
  
    exit(0) ;
}


sub get_stas { # ( $problems, %proc_stas ) = &get_stas( $db, $problems ) ;
    my ( $db, $problems ) = @_ ;
    my ( $equip_install, $equip_remove, $nrows, $row, $sta, $stas, $stas_b44, $subject ) ;
    my ( @b44_stas, @db, @dbdeploy, @sta_deploy, @stas ) ;
    my ( %proc_stas ) ;
    
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
    @dbdeploy = dbsubset ( @dbdeploy, "sta  =~ /$opt_s/ " ) ;
    @dbdeploy = dbsort   ( @dbdeploy, "sta", "time" ) ;
    
    $nrows    = dbquery ( @dbdeploy, "dbRECORD_COUNT" ) ;
    
    @sta_deploy = ();
    
    for ($row = 0; $row<$nrows; $row++) {
        $dbdeploy[3] = $row ;
        $sta = dbgetv ( @dbdeploy, "sta" ) ;
        push @sta_deploy, $sta ;
    }
         
    @sta_deploy = get_unique ( @sta_deploy ) ;
    
    $stas = join( "|", @sta_deploy ) ;
    
    $stas = "(" . $stas . ")" ;
    
    elog_debug( "dbdeploy stas - 	$stas" ) if $opt_V ;

    opendir( DIR, $pf{baler44dirbase} ) ;
    @b44_stas = sort ( grep { /^[A-Z0-9][A-Z0-9][A-Z0-9][A-Z0-9]$/  } readdir(DIR) ) ;
    closedir( DIR );
    
    $stas_b44 = join( " ", @b44_stas ) ;
    elog_notify( "b44_stas - 	$stas_b44\n\n" ) if $opt_V ;
    
    @stas = grep ( /$stas/, @b44_stas ) ;

    elog_debug( "get_stas station subset is $opt_s" ) if $opt_V ;
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
    
    for ($row = 0; $row<$nrows; $row++) {
        $dbdeploy[3] = $row ;
        ( $sta, $equip_install, $equip_remove )  = dbgetv ( @dbdeploy, qw ( sta equip_install equip_remove) ) ;
        next unless grep ( /$sta/, @b44_stas ) ;
        $proc_stas{$sta}{equip_install} = $equip_install ;
        $proc_stas{$sta}{equip_remove}  = $equip_remove ;
    }

    dbclose( @db );
    
    return ( $problems, %proc_stas ) ;
}

sub proc_sta { # &proc_sta( $sta, $install_time, $removal_time, $parent ) ;
    my ( $sta, $install_time, $removal_time, $parent ) = @_ ;
    my ( $bh14name, $cmd, $dirname, $prob, $soh14name, $stime, $string, $subject ) ;

    $stime = strydtime( now() ) ;        
    $string = "$sta - starting processing station $stime" ;
    fork_notify ( $parent, $string ) ;
    
    open( PROB, "> /tmp/prob_$sta\_$$" ) ;
    
#
#  perform existance checks
#

    $prob = 0 ;
            
    ( $dirname, $bh14name, $soh14name, $prob ) = &sta_db_check( $sta, $parent, $prob  ) ;
    
    if  ( $prob )  {

        $string = "Skipping to go to next station" ;
        
        &fork_notify ( $parent, $string ) ;

        &print_prob ( $prob, $string, $parent, *PROB ) ;
        close( PROB ) ;
        
        $subject = "TA $sta     $prob baler data problems -  $pgm on $host" ;
        $cmd     = "rtmail -C -s '$subject' $pf{prob_mail} < /tmp/prob_$sta\_$$" ;
        
        &run_cmd( $cmd ) ;

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
                        
            if ( ! &run_cmd( $cmd ) ) {
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
        
        &run_cmd( $cmd );

        $stime = strydtime(now());
        
        $string = "$sta - finished processing station $stime" ;
        fork_notify ( $parent, $string ) ;
        
        unlink "/tmp/prob_$sta\_$$" unless $opt_V;

        return ;
    }
                
    $prob = &baler44_proc( $sta, str2epoch( $install_time ), str2epoch ( $removal_time ), $parent, $prob ) unless ( $prob );
            
    close(PROB);
    
#
#  clean up
#
        
    if  ( $prob ) {
        $subject = "TA $sta     $prob baler data problems -  $pgm on $host";
        $cmd     = "rtmail -C -s '$subject' $pf{prob_mail} < /tmp/prob_$sta\_$$";
        fork_notify ( $parent, "$cmd") ;
        
        if ( ! &run_cmd( $cmd ) ) {
            $prob++ ;
            &print_prob ( $prob, "FAILED: $cmd", $parent, *PROB ) ;
        }
        
    } elsif ( $opt_v ) {
        $subject = "TA $sta baler completed successfully -  $pgm on $host";
        $cmd     = "rtmail -C -s '$subject' $pf{success_mail} < /tmp/prob_$sta\_$$";
        fork_notify ( $parent, "$cmd") if $opt_v ;   
        
        if ( ! &run_cmd( $cmd ) ) {
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

sub sta_db_check { # ( $dirname, $bh14name, $soh14name, $prob ) = &sta_db_check( $sta, $parent, $prob  ) ;
    my ( $sta, $parent, $prob ) = @_ ;
    my ( $baler44dir, $bh14name, $dbname, $dirname, $prob_check, $rsync, $soh14name, $string ) ;
    
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
                                
    return ( $dirname, $bh14name, $soh14name, $prob ) ;
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

sub baler44_proc { # $prob = &baler44_proc( $sta, $install_time, $removal_time, $parent, $prob );
    my ( $sta, $install_time, $removal_time, $parent, $prob ) = @_ ;
    my ( $dbname, $go_to_next, $last_mseed, $maxtime, $mintime, $msd2days_prob, $mseed, $ncnt ) ;
    my ( $nfiles, $ngap, $nrows, $nrows_to_proc, $nsize, $prob_check, $skip_ref, $string, $time, $unproc_ref ) ;
    my ( @db, @dbbaler, @diff, @mseed, @mseed_test, @skip ) ;
    my ( %msd_proc, %unproc ) ;
    
#
#  Assumes that rsyncbaler table is correct and all msd files on baler are listed
#
#  Convert so all db and file checks are done in one subroutine.
#  Remove last_good.
#
    
    %msd_proc = () ;
        
    $string = "starting baler44_proc" ;
    fork_notify ( $parent, $string ) if $opt_v ;
    print PROB "$string \n\n" ;
    
    if ( now() > ( $removal_time + ( $pf{days_after_removal} * 86400) ) ) {
        $string = sprintf ( "removal time was more than %d days ago", $pf{days_after_removal} )  ;
        fork_notify ( $parent, $string ) if $opt_v ;
        print PROB "$string \n\n" ;
    }
        
    $prob_check = $prob ;
    
#
#  check rsyncbaler table is correct, returns @dbbalerd which has all mseed files to process 
#
    elog_debug ( sprintf ( "starting dnld_check		%s" , strydtime ( now () ) ) ) if $opt_V ;

    ( $nrows_to_proc, $ngap, $prob, $unproc_ref, $skip_ref ) = &dnld_check( $sta, $install_time, $removal_time, $parent, $prob ) ;
    
    %unproc = %$unproc_ref ;
    @skip   = @$skip_ref ;
        
    if ( $prob_check != $prob || $opt_c ) {
        $string =  " "  ;
        fork_complain ( $parent, $string ) ;
        $string =  sprintf( "%6d problems and %6d gaps after dnld_check, skipping processing", $prob, $ngap )  ;
        fork_complain ( $parent, $string ) ;
        print PROB "\n$string \n" ;
        $string =  " "  ;
        fork_complain ( $parent, $string ) ;
        return( $prob ) ;    
    }
    
#     $nrows    = dbquery ( @dbbalerd, "dbRECORD_COUNT" ) ;
#     
    elog_debug ( "dnld_check return		nrows_to_proc	$nrows_to_proc	ngap	$ngap	prob	$prob" ) if $opt_V ;
    
    if ( $nrows_to_proc == 0 ) {
        $string =  sprintf( "%6d baler44 files to process", $nrows_to_proc )  ;
        fork_notify ( $parent, $string ) ;
        print PROB "\n$string \n" ;
        return( $prob );    
    }

    elog_debug ( sprintf ( "completed dnld_check		%s" , strydtime ( now () ) ) ) if $opt_V ;
    
#
#  loop over all downloaded mseed files which have not been processed
#
    
    @mseed_test = sort keys %unproc ;
        
    foreach $mseed ( @mseed_test ) {
    
#
#  skip files with wrong sta code in mseed
#
        ( $go_to_next, $nsize, $prob ) = &mseedcheck( $unproc{ $mseed }{ dfile_full }, $nsize, $parent, $prob ) ;        
        if  ( $go_to_next ) {
            elog_debug ( "$mseed failed mseedcheck" ) if $opt_V ;
            next ;
        }
#
#  run msdd on mseed file to get a listing of the contents
#
        
        ( $time, $go_to_next, $prob ) = &msdd( $sta, $unproc{ $mseed }{ dfile_full }, $parent, $prob ) ;
        if  ( $go_to_next ) {
            elog_debug ( "$mseed failed msdd" ) if $opt_V ;
            next ;
        }
        
        $msd_proc{$mseed}{time}       = $unproc{$mseed}{time} ;
        $msd_proc{$mseed}{dfile_full} = $unproc{$mseed}{dfile_full} ;
        $msd_proc{$mseed}{md5}        = $unproc{$mseed}{md5} ;
        $msd_proc{$mseed}{filebytes}  = $unproc{$mseed}{filebytes} ;
    }
    
    if ( $prob_check != $prob ) {
        $string =  sprintf( "%5d problems after mseedcheck and msdd loop, skipping processing", $prob )  ;
        fork_notify ( $parent, $string ) ;
        print PROB "\n$string \n" ;
    
        return( $prob );    
    }
        
    elog_debug ( sprintf ( "completed mseedcheck loop	%s" , strydtime ( now () ) ) ) if $opt_V ;
    
    elog_debug ( "downloaded mseed files	$prob" ) if $opt_V ;
    
    prettyprint( \%msd_proc ) if $opt_V ; 

#
#  sort mseed files by file time
#        

    @mseed = sort { $msd_proc{$a}{time} <=> $msd_proc{$b}{time} } keys %msd_proc ;
    
    @diff       = sym_diff ( \@mseed, \@mseed_test ) ;
    
    elog_debug  ( "mseed      test    $#mseed    $mseed[0]    $mseed[$#mseed] " ) if $opt_V ;
    elog_debug  ( "mseed_test test    $#mseed_test    $mseed_test[0]    $mseed_test[$#mseed_test] " ) if $opt_V ;
    elog_debug  ( "diff    $#diff    @diff " ) if $opt_V ;
    
    prettyprint( \@mseed ) if $opt_V ; 
    
#
#  Check to make sure output waveform data does not exist
#
    if ( -e "$sta.wfdisc" ) {
        ( $mintime, $maxtime, $prob ) = &sta_time_check( $sta, $parent, $prob ) ;
        fork_notify ( $parent, sprintf( "	$sta.wfdisc already exists; baler_wf_proc has been run before" ) )  ;
        fork_notify ( $parent, sprintf( "	Baler wfdisc range  %s    %s", strydtime( $mintime ), strydtime( $maxtime ) ) )  ;
        fork_notify ( $parent, sprintf( "	Baler mseed new     %s    %d", strydtime( $msd_proc{$mseed[0]}{time} ), $nrows ) )  ;
        if ( $msd_proc{$mseed[0]}{time} < $maxtime && $msd_proc{$mseed[0]}{time} > epoch(2004001)  ) {
            $prob++ ;
            &print_prob ( $prob, "New baler download data starts before last processed data - Should be after", $parent, *PROB ) ;

            $string =   sprintf( "	Download start  %s	", strydtime( $msd_proc{$mseed[0]}{time} ) )  ;
            fork_complain ( $parent, $string ) ;
            print PROB "         $string \n" ;
            $string =   sprintf( "	Db Maxtime      %s",  strydtime( $maxtime ) )  ;
            fork_complain ( $parent, $string ) ;
            print PROB "         $string \n\n" ;
        }
    } 

    fork_debug   ( $parent, "prob_check	$prob_check	problems	$prob	ngap	$ngap" ) ; # if $opt_V ;

    elog_debug ( "pre-skip	$prob	$nfiles	$last_mseed" ) if $opt_V ;

    if ( $prob_check != $prob ) {  
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

    $string = "First mseed file processed was    $mseed[0] ";
    fork_notify ( $parent, $string ) ;
    print PROB "\n$string \n\n" ;
        
    foreach $mseed ( @mseed ) {
            
        ( $msd_proc{$mseed}{md5}, $prob ) = &fix_miniseed( $msd_proc{$mseed}{dfile_full}, $msd_proc{$mseed}{md5}, $parent, $prob ) ;
        
        if ( $msd2days_prob != $prob ) {
            $string = "Last mseed fix_miniseed processed was	$msd_proc{$mseed}{dfile_full} ";
            fork_notify ( $parent, $string ) ;
            print PROB "\n$string \n\n" ;
            last ; 
        }
        
        $prob = msd2daysBL(  $sta, $msd_proc{$mseed}{dfile_full}, $parent, $prob ) ; 
        $prob = msd2daysSOH( $sta, $msd_proc{$mseed}{dfile_full}, $parent, $prob ) ;
        
        if ( $msd2days_prob != $prob ) {
            $string = "Last msd2days mseed file processed was	$msd_proc{$mseed}{dfile_full} ";
            fork_notify ( $parent, $string ) ;
            print PROB "\n$string \n\n" ;
            last ; 
        }
        
        $msd_proc{$mseed}{msdtime} = now() ;
        
        $ncnt++ ;
        
        if ( $last_mseed =~ /$mseed/ ) {
            fork_debug   ( $parent, "last_mseed	$last_mseed	mseed	$mseed") ;
            last ; 
        }
                        
# last if ($ncnt == 10);
    }
    
    elog_debug ( sprintf ( "completed miniseed2days loop	%s" , strydtime ( now () ) ) ) if $opt_V ;
    
    $dbname = "$pf{baler44dirbase}\/$sta/$sta\_baler" ;
    
    @db       = dbopen  ( $dbname, 'r+' );
    @dbbaler  = dblookup( @db, 0, "rsyncbaler", 0, 0) ;
    
    foreach $mseed ( @mseed ) {
        $dbbaler[3] = dbfind ( @dbbaler, "dfile =~ /$mseed/ && status =~ /downloaded/", -1 ) ;
        dbputv( @dbbaler, "msdtime", $msd_proc{$mseed}{msdtime} ) if ( ! $opt_n && $msd_proc{$mseed}{msdtime} > 0 ) ;
    }
    
    @dbbaler  = dbsort( @dbbaler, "-r", "lddate" ) ;
    
    foreach $mseed ( @skip ) {
        $dbbaler[3] = dbfind ( @dbbaler, "dfile =~ /$mseed/", -1 ) ;
        dbputv( @dbbaler, "msdtime", now(), "status", "skipped" ) if ( ! $opt_n  ) ;
        $string = "	$mseed    skipped";
        fork_notify ( $parent, $string ) if ( $opt_n || $opt_v ) ;
    }
    
    dbclose( @db );  

    elog_debug ( sprintf ( "completed db update	%s" , strydtime ( now () ) ) ) if $opt_V ;

    $string = "Last mseed  file processed was    $mseed[$#mseed] ";
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
    
     
    $string =  sprintf( "%5d baler44 files not processed", $nrows_to_proc - $ncnt )  ;
    fork_notify ( $parent, $string ) ;
    print PROB "\n$string \n" ;

    
    elog_debug ( sprintf ( "completed miniseed2days loop	%s" , strydtime ( now () ) ) ) if $opt_V ;
        
    elog_debug ( sprintf ( "completed baler44_proc loop	%s" , strydtime ( now () ) ) ) if $opt_V ;
    
    
    return( $prob );

}

sub dnld_check { # (  $nrows_to_proc, $ngap, $prob, %dfile_unprocessed ) = &dnld_check( $sta, $install_time, $removal_time, $parent, $prob ) ;
    my ( $sta, $install_time, $removal_time, $parent, $prob ) = @_ ;
    my ( $alreadyproc, $dbname, $dfile, $dfile_full, $filebytes, $last_proc, $md5, $media, $msdtime, $ndfiles ) ;
    my ( $ndown, $next_proc, $ngap, $ngap_at_start, $nmiss, $nproc, $nrows, $nrows_to_proc, $nzfiles ) ;
    my ( $prob_check, $string ) ;
    my ( @db, @dbbaler, @dbbaler_unique, @dbbalerd, @dbbalerdn, @dbbalerz, @mseed, @skip, @t1, @t2, @tmp ) ;
    my ( %dfile_unique, %dfile_unprocessed ) ;

#
#  Assumes that rsyncbaler table is correct and all msd files on baler are listed
#
#  Convert so all db and file checks are done in one subroutine.
#  Remove last_good.
#

    %dfile_unique      = () ;
    %dfile_unprocessed = () ;
    @skip              = () ;
            
#
#  open database
#
    $dbname = "$pf{baler44dirbase}\/$sta/$sta\_baler" ;
    
    $string = "starting dnld_check using $dbname" ; 
    fork_notify ( $parent, $string ) ;
    print PROB "$string \n" ;
     
    @db       = dbopen  ( $dbname, 'r' );
    @dbbaler  = dblookup( @db, 0, "rsyncbaler", 0, 0) ;
    $nrows    = dbquery ( @dbbaler, "dbRECORD_COUNT" ) ;

    fork_debug   ( $parent, "$nrows in rsyncbaler") if $opt_V ;

#     $string    = "starting  dnld_check" ;
#     fork_notify ( $parent, $string ) if $opt_v ;
#     print PROB "$string \n" ;
    
    $prob_check = $prob ;
    
#
#  find unique mseed files in rsyncbaler table, build hash of unique mseed file names
#

    ( $prob, @dbbaler_unique ) = unique_dfiles( $prob, $parent, @dbbaler ) ;
    
    if ( $prob_check != $prob ) {
        dbclose ( @db ) ;
        return ( 0, 0, $prob, \%dfile_unprocessed, \@skip  ) ;
    }
    
    for ( $dbbaler_unique[3] = 0 ; $dbbaler_unique[3] < dbquery(  @dbbaler_unique, "dbRECORD_COUNT" ) ; $dbbaler_unique[3]++ ) {
        $dfile                           = dbgetv  ( @dbbaler_unique, "dfile" ) ;
        next if ( length ( $dfile ) != 24 ) ;
        @t1 = split /-/, $dfile ;
        @t2 = split /_/, $t1[1] ;
        if ( $#t1 != 2 || $#t2 != 1 ) {
            $string  = sprintf( "	%s	is not a valid file name", $dfile )  ;
            fork_notify ( $parent,  $string  ) ;
            next;
        }

#         $string  = sprintf( "%s	%d	%d", $dfile, $#t1, $#t2 )  ;
#         fork_notify ( $parent,  $string  ) ;
        $dfile_unique{ $dfile }{ time }  = &mseedtime ( $dfile, $parent, $prob ) ;
    }
    
#
#  find downloaded mseed files in rsyncbaler table, build hash of downloaded mseed file names
#

    ( $prob, @dbbalerd ) = downloaded_mseed( $prob, $parent, @dbbaler ) ;

    for ( $dbbalerd[3] = 0 ; $dbbalerd[3] < dbquery(  @dbbalerd, "dbRECORD_COUNT" ) ; $dbbalerd[3]++ ) {
        ( $dfile, $filebytes, $msdtime )        = dbgetv  ( @dbbalerd, qw ( dfile filebytes msdtime ) ) ;
        next if ( length ( $dfile ) != 24 ) ;
        @t1 = split /-/, $dfile ;
        @t2 = split /_/, $t1[1] ;
        next if ( $#t1 != 2 || $#t2 != 1 ) ;
        $dfile_unique{ $dfile }{ downloaded }   = 1 ;
        $dfile_unique{ $dfile }{ filebytes }    = $filebytes ;
        if ( $msdtime > 0 ) {
            $dfile_unique{ $dfile }{ msdtime }  = $msdtime ;
        }
    }

#
#  review previously processed mseed files in downloaded view
#

    ( $prob, $nproc, $last_proc ) = proc_prev ( $prob, $parent, \%dfile_unique ) ;

#
#  find unprocessed downloaded mseed files in rsyncbaler table, build hash of unprocessed downloaded mseed file names
#
    
    ( $prob, $nrows_to_proc, @dbbalerdn ) = downloaded_for_proc ( $prob, $parent, @dbbaler ) ;
    
    if ( $nrows_to_proc == 0 ) {
        dbclose ( @db ) ;
        return ( $nrows_to_proc, 0, $prob, \%dfile_unprocessed, \@skip  ) ;
    }

    for ( $dbbalerdn[3] = 0 ; $dbbalerdn[3] < dbquery(  @dbbalerdn, "dbRECORD_COUNT" ) ; $dbbalerdn[3]++ ) {
        ( $media, $md5, $dfile, $filebytes )  = dbgetv     ( @dbbalerdn, qw ( media md5 dfile filebytes ) ) ;
        $dfile_full                           = dbextfile  ( @dbbalerdn ) ;
        if ( ! -f $dfile_full ) {
            $prob++ ;
            $string =   "$dfile_full does not exist!"  ;
            &print_prob ( $prob, $string, $parent, *PROB ) ;
            next ; 
        }
        if ( $filebytes <= 0 ) {
            $string =   "$dfile_full has $filebytes bytes!"  ;
            fork_notify ( $parent, $string ) ;
            next ; 
        }
        next if ( length ( $dfile ) != 24 ) ;
        $dfile_unprocessed{ $dfile }{ time }         = &mseedtime ( $dfile_full, $parent, $prob ) ;
        $dfile_unprocessed{ $dfile }{ media }        = $media ; 
        $dfile_unprocessed{ $dfile }{ md5 }          = $md5 ; 
        $dfile_unprocessed{ $dfile }{ filebytes }    = $filebytes ; 
        $dfile_unprocessed{ $dfile }{ dfile_full }   = $dfile_full ; 
    }
    
    @mseed        = sort { $dfile_unprocessed{$a}{time} <=> $dfile_unprocessed{$b}{time} } keys %dfile_unprocessed ;
    $next_proc    = $mseed[0] ;
    $ngap_at_start = 0 ;
    
#  
#  look for missing files at beginning of equipment installation and no baler 44 files already processed  
#  skips check if baler 14 data exists
#
    if ( ! $nproc ) {    
        $prob    = install_missing ( $sta, $install_time, \%dfile_unprocessed, $prob, $parent ) ;
    }
    
    if ( $nproc ) {    

#  
#  Check for delinquent files which have now magically appeared
#

        ( $prob, $alreadyproc ) = &delinquent_files( \%dfile_unique, \%dfile_unprocessed, $parent, $prob ) ;
    
        if ( $alreadyproc > 0 ) {
            dbclose ( @db ) ;
            return ( $nrows_to_proc, -1, $prob, \%dfile_unprocessed, \@skip  ) ;
        }
#
#  see if there are missing mseed files between previously processed files and unprocessed files
#
    
        $string            = sprintf ( "%6d rsyncbaler dfiles to process, starting with	%s", $nrows_to_proc, $next_proc ) ;
        fork_notify ( $parent, $string ) ; # if $opt_v ;
    
        $nmiss = 0 ; 
    
        if ( $nproc > 0 ) {
            ( $nmiss, $ngap_at_start, $prob, @tmp ) = &missing_files_since_last_proc( $last_proc, $next_proc, $ngap_at_start, \%dfile_unique, $parent, $prob ) ;
            push ( @skip, @tmp ) ;
    
            if ( $ngap_at_start ) {
                dbclose ( @db ) ;
                return ( $nrows_to_proc, $ngap_at_start, $prob, \%dfile_unprocessed, \@skip  ) ;
            }
        }
	}	

#
#  look for missing files at end of equipment installation
#

    if ( $removal_time < 9999999999.99900 && $nrows_to_proc ) {

        $prob = removal_missing ( $sta, $mseed[$#mseed], $removal_time, $prob, $parent ) ;

        $string            = sprintf ( "removal_missing completed	prob	%d", $prob ) ;
        fork_notify ( $parent, $string ) if $opt_V ;
    }

#
#  look for missing files based on the list of files to process
#

    ( $nmiss, $ngap, $prob, @tmp ) = &find_missing_files( $nmiss, \%dfile_unique, \%dfile_unprocessed, $parent, $prob ) unless $prob ;
    
    push ( @skip, @tmp ) ;
    
    if ( $prob_check != $prob ) {
        dbclose ( @db ) ;
        return ( $nrows_to_proc, $ngap, $prob, \%dfile_unprocessed, \@skip ) ;
    }

#
#  check times of downloaded mseed files which have not been processed
#  look for unusually long times between adjacent mseed files
#

    ( $prob ) = &check_time_between_files( $sta, \%dfile_unprocessed, $parent, $prob ) ;
        
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
        fork_notify ( $parent,  $string  ) if $opt_v ;
        print PROB "$string \n\n" ;
    }
    dbclose ( @db ) ;
    return ( $nrows_to_proc, $ngap, $prob, \%dfile_unprocessed, \@skip  ) ;
}

sub mseedtime { # ( $mseedtime ) = &mseedtime( $mseed, $parent, $prob ) ;
    my ( $mseed, $parent, $prob ) = @_ ;
    my ( $day, $hour, $minute, $month, $mseedtime, $second, $tstring, $year ) ;
    my ( @tstring ) ;
#
#  generate time string of file from mseed file name
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
    my ( @keys, @line, @msdd, @msdderr, @msdd_BHZ, @tmp) ;
    my ( %chan ) ;
    
    $go_to_next = 0 ;
    $time       = 0 ;
    
    $cmd = "msdd -b $mseed" ;
    fork_debug   ( $parent, $cmd ) if $opt_V ;
    open( MSD, "$cmd 2>&1 |") ;
    @msdd = <MSD> ;
    close MSD ;
    
    @msdderr = grep ( /msdd/ , @msdd ) ;   #  keep lines with errors
    if ( $#msdderr != -1 ) {   # error in mseed file
        $string = sprintf( "Mseed file %s has %d errors", $mseed, $#msdderr + 1 ) ;
        fork_notify ( $parent, $string ) ;
        print PROB "	$string \n" ;
        if ( $opt_v ) {
             foreach $line ( @msdderr ) {   # print errors
                chomp $line ;
                fork_notify ( $parent, "	$line" ) ;
                print PROB "$line \n" ;
            }
        }
        print PROB "\n" ;
    }
            
    @line = split ( " ", $msdd[$#msdd] ) ;
    @msdd = grep ( /$sta|EXMP/ , @msdd ) ;   #  keep only lines with sta code

    if ( $#msdd == -1 ) {   # no station data in mseed file
        $prob++ ;
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
        @line     = split (" ", $line ) ;
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

sub fix_miniseed { # ( $md5_file, $prob ) = &fix_miniseed( $mseed, $md5_file, $parent, $prob ) ;
    my ( $mseed, $md5_file, $parent, $prob ) = @_ ;
    my ( $cmd, $md5_check ) ;
    
    fork_debug   ( $parent, "starting fix_miniseed ( $mseed, $md5_file, $parent, $prob )" ) if $opt_V ;
#
#  put md5 check here if needed.
#

    $md5_check = &md5 ( $mseed, $parent ) ;  
    
#     elog_debug ( "md5_file	$md5_file	md5_check	$md5_check	opt_d	$opt_d" ) if $opt_V ;
    
    if ( $md5_file =~ /error\-verify|error\-download|missing|-/ ) {
        fork_notify   ( $parent, "$mseed	db md5	$md5_file" ) if $opt_v ;
        $md5_file  = $md5_check ;
    }

    if ( ! $opt_d && $md5_file !=  $md5_check ) {
#         $prob++ ;
#         &print_prob ( $prob, "FAILED: $mseed md5_file	$md5_file !=	md5_check	$md5_check", $parent, *PROB ) ;
        fork_notify   ( $parent, "FAILED: $mseed md5_file	$md5_file !=	md5_check	$md5_check" ) ;
        $md5_file  = $md5_check ;
    }

    return ( $md5_file, $prob ) if ( $opt_n && ! $opt_V ) ;
    
    $cmd  = "fix_miniseed -p fix_miniseed_TA_net_sta_chan_loc $mseed ";

    if ( ! &run_cmd( $cmd ) ) {
        $prob++ ;
        &print_prob ( $prob, "FAILED: $cmd", $parent, *PROB ) ;
    }
        
#
#  put md5 check here if needed.
#
    $md5_check = &md5 ( $mseed, $parent ) ;  
    
    if ( $md5_file !=  $md5_check ) {
        fork_notify   ( $parent, "mis-labled EP channels fixed in $mseed" ) ;
        $md5_file  = $md5_check ;
    }

    $cmd  = "fix_miniseed -p fix_miniseed_TA_sta $mseed ";

    if ( ! &run_cmd( $cmd ) ) {
        $prob++ ;
        &print_prob ( $prob, "FAILED: $cmd", $parent, *PROB ) ;
    }

#
#  put md5 check here if needed.  Update status in rsync_baler row which is being modified, currently "downloaded"
#
    $md5_check = &md5 ( $mseed, $parent ) ;  
    
    if ( $md5_file !=  $md5_check ) {
        fork_notify   ( $parent, "mis-labled sta code fixed in $mseed" ) ;
        $md5_file  = $md5_check ;
    }

    return ( $md5_file, $prob ) ;
}

sub msd2daysBL { # $prob = &msd2daysBL( $sta, $mseedfile, $parent, $prob ) ;
    my ( $sta, $mseedfile, $parent, $prob ) = @_ ;
    my ( $cmd, $error_code, $full_buf, $stderr_buf, $stdout_buf ) ;
    
    fork_debug   ( $parent, "starting msd2daysBL" ) if $opt_V ;

    return ( $prob ) if ( $opt_n && ! $opt_V ) ;

    $cmd  = "miniseed2days -f -c -U -m \"$pf{net}\_$sta\_[HBL][HDN]._.*\" ";
    $cmd .= " - < $mseedfile ";

    if ( ! &run_cmd( $cmd ) ) {
        $prob++ ;
        &print_prob ( $prob, "FAILED: $cmd", $parent, *PROB ) ;
    }
        
    return ( $prob ) ;
}

sub msd2daysSOH { # $prob = &msd2daysSOH( $sta, $mseedfile, $parent, $prob ) ;
    my ( $sta, $mseedfile, $parent, $prob ) = @_ ;
    my ( $cmd, $error_code, $full_buf, $stderr_buf, $stdout_buf ) ;
    
    fork_debug   ( $parent, "starting msd2daysSOH" ) if $opt_V ;
    
    return ( $prob ) if ( $opt_n && ! $opt_V ) ;

    $cmd  = "miniseed2days -f -c -U -m \"$pf{net}\_$sta\_.*\" -r \".*_.*_[HBL][HDN]._.*\" -w %Y/month_files/%m/%{net}_%{sta}_%{chan}_%{loc}.msd ";
    $cmd .= " - < $mseedfile ";
    
    if ( ! &run_cmd( $cmd ) ) {
        $prob++ ;
        &print_prob ( $prob, "FAILED: $cmd", $parent, *PROB ) ;
    }
            
    return ( $prob ) ;
}

sub sta_time_check { # ( $mintime, $maxtime, $prob ) = &sta_time_check( $sta, $parent, $prob ) ;
#
#  Checks station active wfdisc to verify that data from rsyncbaler db does not overlap
#
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

sub md5 { # $md5 = &md5 ( $msdfile, $parent ) ;
    my ( $msdfile, $parent ) = @_ ;
    my ( $cmd, $line, $md5, $md5_lib ) ;
    my ( @line, @md5 ) ;
    
#
# Open file and produce local md5
#
    open( FILE, $msdfile ) or fork_complain( "Cannot open file $msdfile for md5 calc." ) ;
    $md5_lib = Digest::MD5->new ;
    $md5_lib->addfile( *FILE ) ;
    $md5     = $md5_lib->hexdigest || 0;
    close FILE;

    return ( $md5 ) ;
}

sub md5_check { # $yesno = &md5_check () ;
    my ( $md5 ) ;
    
    $md5 = datafile ( "PATH", "md5" ) ;
    elog_debug ( "md5 path	$md5" ) if $opt_V ;
    
    if ( $md5 !~ /.*5/ ) {
        elog_die ( "program md5 does not exist on this platform!" ) ;
    } 
    
    return ;
}

sub data_time_check { # $actual_time = &data_time_check ( $time, $endtime, @db ) ;
    my ( $time, $endtime, @db ) = @_ ;
    my ( $actual_time, $nrows, $te, $ts ) ;
    
    @db    = dbsubset ( @db, "endtime > $time && time < $endtime" ) ;
    @db    = dbsort   ( @db, "time" ) ;
    $nrows = dbquery  ( @db, "dbRECORD_COUNT" ) ;
        
    $actual_time = 0 ;
    for ( $db[3] = 0 ; $db[3] < $nrows ; $db[3]++) {
        ( $ts, $te ) = dbgetv ( @db, qw( time endtime ) ) ;
        if ( $time >= $ts && $endtime <= $te ) {
            $actual_time += $endtime - $time ;
        }
        if ( $time < $ts && $endtime > $te ) {
            $actual_time += $te - $ts ;
        }
        if ( $time >= $ts && $endtime > $te ) {
            $actual_time += $te - $time ;
        }
        if ( $time < $ts && $endtime <= $te ) {
            $actual_time += $endtime - $ts ;
        }
    }

    return ( $actual_time );
}

sub complain_or_notify { # &complain_or_notify ( $notify, $parent, $string ) ;
    my ( $notify, $parent, $string ) = @_ ;
    
    if ( $notify ) {
        fork_notify ( $parent, $string ) ;
    } else {
        fork_complain ( $parent, $string ) ;
    }
    
}

sub sym_diff {  # @diff = sym_diff ( \@a, \@b ) ;
    my ( $aref, $bref ) = @_ ;
    
    my ( $e ) ;
    my ( @a, @b, @diff, @union ) ;
    my ( %count ) ;
    
    
    @a = @$aref ;
    @b = @$bref ;
    
    @union = @diff = () ;
    
    %count = () ;
    
    foreach $e ( @a, @b ) { $count{ $e }++ } 
    
    foreach $e ( keys %count ) {
        if ( $count{ $e } != 2 ) {
            push @diff, $e ;
        }
    }
    
    return ( @diff ) ;
}

sub unique_dfiles { # ( $prob, @dbbaler_unique ) = unique_dfiles( $prob, $parent, @dbbaler ) ;
    my ( $prob, $parent, @dbbaler ) = @_ ;
    my ( $ncheck, $ndfiles, $string ) ;
    my ( @dbbaler_check, @dbbaler_unique ) ;
#
#  find unique mseed files in table
#     sorting on "( substr( dfile, strlen(dfile) - 14, strlen(dfile) - 1 ) + 1 )" forces a numerical sort on date
#     in case there is a different sta code on the file like EXMP

    @dbbaler_unique = dbsort  ( @dbbaler, "-u", "dfile") ;
    @dbbaler_unique = dbsort  ( @dbbaler_unique, "( substr( dfile, strlen(dfile) - 14, strlen(dfile) - 1 ) + 1 )" ) ;
    $ndfiles        = dbquery ( @dbbaler_unique, "dbRECORD_COUNT" ) ;

    @dbbaler_check  = dbsort  ( @dbbaler, "-u", "dfile") ;
    $ncheck         = dbquery ( @dbbaler_check, "dbRECORD_COUNT" ) ;
    
    if ( $ndfiles != $ncheck ) {
        $prob++ ;
        $string = "Problem in dfile file names! " ;
        fork_complain ( $parent, $string ) ; 
        print PROB "$string \n\n" ;
        $string = "dfile unique sort != unique sort of ( substr( dfile, strlen(dfile) - 14, strlen(dfile) - 1 ) + 1 ) " ;
        fork_complain ( $parent, $string ) ; 
        print PROB "$string \n\n" ;
        $string = "ncheck	$ncheck	ndfiles	$ndfiles" ;
        fork_complain ( $parent, $string ) ; 
        print PROB "$string \n\n" ;
    }

    $string = sprintf ( "%6d rsyncbaler dfiles that are unique ", $ndfiles ) ;
    fork_notify ( $parent, $string ) ; # if $opt_v ;
    print PROB "$string \n" ;
    
    dbfree ( @dbbaler_check ) ;
    
    return ( $prob, @dbbaler_unique ) ;
}

sub downloaded_mseed { # ( $prob, @dbbalerdu ) = downloaded_mseed( $prob, $parent, @dbbaler ) ;
    my ( $prob, $parent, @dbbaler ) = @_ ;
    my ( $dfile, $dfile_check, $ndown, $ndownu, $string ) ;
    my ( @dbbalerd, @dbbalerdu ) ;
    my ( %dfile ) ;
#
#  find downloaded mseed files
#

    @dbbalerd = dbsubset( @dbbaler, "status =~ /downloaded/ ") ;
    $ndown    = dbquery ( @dbbalerd, "dbRECORD_COUNT" );

    $string = sprintf ( "%6d rsyncbaler dfiles with status =~ /downloaded/ ", $ndown ) ;
    fork_notify ( $parent, "$string" ) ; # if $opt_v ;
    print PROB "$string \n" ;
    elog_debug ( $string ) if $opt_V ;
    
    @dbbalerdu = dbsort  ( @dbbalerd, "-u", "dfile" ) ;
    $ndownu    = dbquery ( @dbbalerdu, "dbRECORD_COUNT" );
    %dfile = ();

#
#  find any duplicate rows of downloaded files - should not happen
#

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
    
    dbfree ( @dbbalerd ) ;

    return ( $prob, @dbbalerdu ) ;
}

sub proc_prev { # ( $prob, $nproc, $last_proc ) = proc_prev ( $prob, $parent, \%dfile_unique ) ;
    my ( $prob, $parent, $refd ) = @_ ;
    my ( $last_proc, $nproc, $string, $unique ) ;
    my ( @unique ) ;
    my ( %dfile_unique ) ;
#
#  find downloaded mseed files which have been processed
#
    %dfile_unique       = %$refd ;     # unique mseed files
    
    $last_proc    = "" ;
    $nproc        = 0 ;

    @unique        = sort { $dfile_unique{$a}{time} <=> $dfile_unique{$b}{time} } keys %dfile_unique ;
    
    $string = sprintf ( "%6d keys in unique", $#unique+1 ) ;
    fork_notify ( $parent, $string ) if $opt_V ;

    foreach $unique ( @unique ) {
        if ( exists $dfile_unique{ $unique }{ msdtime } ) {
            $nproc++ ;
            $last_proc = $unique ;
        }
    }
        
    $string = sprintf ( "%6d rsyncbaler dfiles with status =~ /downloaded/  && msdtime != NULL", $nproc ) ;
    fork_notify ( $parent, $string ) ;

    return ( $prob, $nproc, $last_proc ) ;

}

sub downloaded_for_proc { # ( $prob, $nrows_to_proc, @dbbalerdn ) = downloaded_for_proc ( $prob, $parent, @dbbaler ) ;
    my ( $prob, $parent, @dbbaler ) = @_ ;
    my ( $nrows_to_proc, $ntmp, $string ) ;
    my ( @dbbalerdn, @dbtmp ) ;
#
#  find downloaded mseed files which have not been processed
#

    @dbbalerdn         = dbsubset( @dbbaler,  "( status =~ /downloaded/ && msdtime == NULL )" ) ;
    @dbbalerdn         = dbsort  ( @dbbalerdn, "dfile" ) ;
    $nrows_to_proc     = dbquery ( @dbbalerdn, "dbRECORD_COUNT" ) ;

    $string            = sprintf ( "%6d rsyncbaler dfiles with status =~ /downloaded/  && msdtime == NULL", $nrows_to_proc ) ;
    fork_notify ( $parent, $string ) ; # if $opt_v ;
    print PROB "$string \n\n" ;
    elog_debug ( $string ) if $opt_V ;

#
#  list common md5 errors in downloaded mseed files which have not been processed
#

    @dbtmp             = dbsubset( @dbbalerdn,  "( md5 =~  /error-download/ )" ) ;
    $ntmp              = dbquery ( @dbtmp, "dbRECORD_COUNT" ) ;
    $string            = sprintf ( "%6d rsyncbaler dfiles with md5    =~  /error-download/ ", $ntmp ) ;
    fork_notify ( $parent, $string ) if ( $ntmp ) ; # if $opt_v ;
    print PROB "$string \n\n" ;
    
    @dbtmp             = dbsubset( @dbbalerdn,  "( md5 =~  /error-verify/ )" ) ;
    $ntmp              = dbquery ( @dbtmp, "dbRECORD_COUNT" ) ;
    $string            = sprintf ( "%6d rsyncbaler dfiles with md5    =~  /error-verify/ ", $ntmp ) ;
    fork_notify ( $parent, $string ) if ( $ntmp ) ; # if $opt_v ;
    print PROB "$string \n\n" ;
    
    @dbtmp             = dbsubset( @dbbalerdn,  "( md5 =~  /missing/ )" ) ;
    $ntmp              = dbquery ( @dbtmp, "dbRECORD_COUNT" ) ;
    $string            = sprintf ( "%6d rsyncbaler dfiles with md5    =~  /missing/ ", $ntmp ) ;
    fork_notify ( $parent, $string ) if ( $ntmp ) ; # if $opt_v ;
    print PROB "$string \n\n" ;
    
    @dbtmp             = dbsubset( @dbbalerdn,  "( md5 =~  /-/ )" ) ;
    $ntmp              = dbquery ( @dbtmp, "dbRECORD_COUNT" ) ;
    $string            = sprintf ( "%6d rsyncbaler dfiles with md5    =~  /-/ ", $ntmp ) ;
    fork_notify ( $parent, $string ) if ( $ntmp ) ; # if $opt_v ;
    print PROB "$string \n\n" ;
    
    dbfree ( @dbtmp ) ;
    
    return ( $prob, $nrows_to_proc, @dbbalerdn ) ;

}

sub install_missing { # ( $prob ) = install_missing ( $sta, $install_time, \%dfile_unprocessed, $prob, $parent ) ;
    my ( $sta, $install_time, $refu, $prob, $parent ) = @_ ;
    my ( $actual_time, $mseed, $mseedtime, $pct_return, $rt, $string, $string1 ) ;
    my ( @dbrt, @mseed ) ;
    my ( %dfile_unprocessed ) ;
#
#  look for missing files at beginning of equipment installation and no baler 44 files already processed  
#  skips check if baler 14 data exists
#
    
    fork_notify (  $parent, "starting install check" ) ;

    %dfile_unprocessed  = %$refu ;     # unprocessed mseed files

    @mseed        = sort { $dfile_unprocessed{$a}{time} <=> $dfile_unprocessed{$b}{time} } keys %dfile_unprocessed ;
        
    $string =  "	$pf{rt_sta_dir}\/$sta\/$sta does not exist!"   ;
    if ( ! -f "$pf{rt_sta_dir}\/$sta\/$sta" && ! ( $opt_F || $opt_B ) ) {
        $prob++ ;
        &print_prob ( $prob, $string, $parent, *PROB ) ;
        $rt = 0 ; 
    } elsif ( ! -f "$pf{rt_sta_dir}\/$sta\/$sta"  ) {
        fork_notify ( $parent, $string ) ;
        &print_prob ( $prob, $string, $parent, *PROB ) ;
        $rt = 0 ; 
    } else {
        $rt = 1 ; 
    }
    
    if ( $rt ) {
        @dbrt  = dbopen   ( "$pf{rt_sta_dir}\/$sta\/$sta", "r" ) ;
        @dbrt  = dblookup ( @dbrt, , 0, "wfdisc", 0, 0 ) ;
        @dbrt  = dbsubset ( @dbrt, "chan =~ /BHZ/ " ) ;
    }
    
    if ( ! -d "$pf{baler14procbase}\/$sta" ) {

        ( $mseedtime ) = &mseedtime( $mseed[0], $parent, $prob ) ;
        
        $string            = sprintf ( "	row %d	dfile	%s", 0, $mseed[0] ) ;
        fork_notify ( $parent, $string ) ; # if $opt_v ;
        
        $string            = sprintf ( "	install time        %s", strydtime(  $install_time ) ) ;
        fork_notify ( $parent, $string ) if $opt_v ;
        print PROB "$string \n\n" ;
        $string            = sprintf ( "	first mseed time    %s", strydtime(  $mseedtime ) );
        fork_notify ( $parent, $string ) if $opt_v ;
        print PROB "$string \n\n" ;
        $string            = sprintf ( "	%s  difference", strtdelta(  $mseedtime - $install_time ) ) ;
        fork_notify ( $parent, $string )  if $opt_v ;
        print PROB "$string \n\n" ;
        
        if ( $rt ) {
            $actual_time = &data_time_check ( $install_time, $mseedtime, @dbrt ) ;
        } else {
            $actual_time = 0 ;
        }
        $pct_return  = 100 * ( $actual_time / (  $mseedtime - $install_time ) ) ;
        
        $string  = sprintf ( "	%s is more than %d days of missing data after installation", 
                   strtdelta(  $mseedtime - $install_time ), $pf{days_after_install} ) ;
            
        $string1 = sprintf( "	%s %d%% of rt BHZ data exists between  %s and  %s", strtdelta( $actual_time ), 
                   $pct_return, strydtime ( $install_time ) , strydtime ( $mseedtime )  )  ;

        if ( $mseedtime - $install_time > ( 86400 * $pf{days_after_install} ) && $pct_return < 25. ) {

            fork_notify ( $parent, $string ) ;
            fork_notify ( $parent, $string1 ) ;
            print PROB "$string \n\n" ;
            print PROB "$string1 \n\n" ;
            
        } elsif ( $mseedtime - $install_time > ( 86400 * $pf{days_after_install} ) ) {

            if ( $opt_F || $opt_B ) {
                fork_notify ( $parent, $string ) ;
                fork_notify ( $parent, $string1 ) ;
            } else {
                fork_complain ( $parent, $string ) ;
                fork_complain ( $parent, $string1 ) ;
                $prob++  ;
            }
            print PROB "$string \n\n" ;
            print PROB "$string1 \n\n" ;
            
        } elsif ( $opt_v ) {
            fork_notify ( $parent, $string1 ) ;
            print PROB "$string1 \n\n" ;
        }
        
    }

    dbclose ( @dbrt ) if ( $rt ) ;
    
    return ( $prob ) ;

}

sub removal_missing { # $prob = removal_missing ( $sta, $mseed, $removal_time, $prob, $parent ) ;
    my ( $sta, $mseed, $removal_time, $prob, $parent ) = @_ ;
    my ( $actual_time, $mseedtime, $pct_return, $rt, $string, $string1 ) ;
    my ( @dbrt ) ;
    
    fork_debug (  $parent, "removal_missing ( $sta, $mseed, $removal_time, $prob, $parent)" ) if $opt_V ;
    
    fork_notify (  $parent, "starting removal check" ) ;
#
#  look for missing files at end of equipment installation
#
    $string =  "	$pf{rt_sta_dir}\/$sta\/$sta does not exist!"   ;
    if ( ! -f "$pf{rt_sta_dir}\/$sta\/$sta" && ! ( $opt_F || $opt_B ) ) {
        $prob++ ;
        &print_prob ( $prob, $string, $parent, *PROB ) ;
        $rt = 0 ; 
    } elsif ( ! -f "$pf{rt_sta_dir}\/$sta\/$sta"  ) {
        fork_notify ( $parent, $string ) ;
        &print_prob ( $prob, $string, $parent, *PROB ) ;
        $rt = 0 ; 
    } else {
        $rt = 1 ; 
    }
    
    if ( $rt ) {
        @dbrt  = dbopen   ( "$pf{rt_sta_dir}\/$sta\/$sta", "r" ) ;
        @dbrt  = dblookup ( @dbrt, , 0, "wfdisc", 0, 0 ) ;
        @dbrt  = dbsubset ( @dbrt, "chan =~ /BHZ/ " ) ;
    }
    
    
    ( $mseedtime ) = &mseedtime( $mseed, $parent, $prob ) ;
        
    $string            = sprintf ( "	removal time        %s", strydtime(  $removal_time ) ) ;
    fork_notify ( $parent, $string ) if $opt_v ;
    print PROB "$string \n\n" ;
    $string            = sprintf ( "	last mseed time     %s", strydtime(  $mseedtime ) );
    fork_notify ( $parent, $string ) if $opt_v ;
    print PROB "$string \n\n" ;
    $string            = sprintf ( "	%s difference", strtdelta( $removal_time - $mseedtime ) ) ;
    fork_notify ( $parent, $string ) if $opt_v ;
    print PROB "$string \n\n" ;
        
        
    if ( $rt ) { 
        $actual_time = &data_time_check ( $mseedtime, $removal_time, @dbrt ) ;
    } else {
        $actual_time = 0 ;
    }
    $pct_return  = 100 * ( $actual_time / (  $removal_time - $mseedtime ) ) ;

    if ( $removal_time - $mseedtime > 86400 * $pf{days_before_removal} ) {
            
        $string  = sprintf ( "	%s is more than %d day of missing data immediately before removal  ", 
                   strtdelta(  $removal_time - $mseedtime ), $pf{days_before_removal} ) ;
            
        $string1 = sprintf( "	%s of rt BHZ data exists between    %s and    %s", strtdelta( $actual_time ), 
                   strydtime ( $mseedtime ) , strydtime ( $removal_time )  )  ;

        if ( $opt_F  || $opt_E ) {
            fork_notify ( $parent, $string ) ;
            fork_notify ( $parent, $string1 ) ;
        } else {
            fork_complain ( $parent, $string ) ;
            fork_complain ( $parent, $string1 ) ;
            $prob++ ;
        }
        print PROB "$string \n\n" ;
        print PROB "$string1 \n\n" ;
            
    } else {

        $string  = sprintf ( "	%s missing baler data before removal with %d%% rt data return", 
                   strtdelta(  $removal_time - $mseedtime ), $pct_return ) ;

        fork_notify ( $parent, $string ) if $opt_v ;
        print PROB "$string \n\n" ;
    }

    
    dbclose ( @dbrt ) if ( $rt ) ;
    
    return ( $prob ) ;

}

sub missing_files_since_last_proc { # ( $nmiss, $ngap, $prob, @skip ) = &missing_files_since_last_proc( $mseed1, $mseed2, $ngap, \%dfile_unique, $parent, $prob ) ;
    my ( $mseed1, $mseed2, $ngap, $refd, $parent, $prob ) = @_ ;
    my ( $alreadyproc, $dfile, $msd1, $msd2, $mseeddir, $nf, $nmiss, $onbaler, $row, $row_1, $row_2, $string, $suffix, $zerolength ) ;
    my ( @mseed, @skip ) ;
    my ( %dfile_unique ) ;
    
    fork_notify (  $parent, "starting missing files since last processed check" ) ;
    fork_notify (  $parent, "	previously processed file is    $mseed1" ) ;
    fork_debug  (  $parent, "missing_files_since_last_proc ( $mseed1, $mseed2, $ngap, \%dfile_unique, $parent, $prob)" ) if $opt_V ;

    %dfile_unique  = %$refd ;     # downloaded mseed files
    
    ( $mseeddir, $msd1, $suffix ) = parsepath( $mseed1 ) ;
    ( $mseeddir, $msd2, $suffix ) = parsepath( $mseed2 ) ;

    @mseed         = sort { $dfile_unique{$a}{time} <=> $dfile_unique{$b}{time} } keys %dfile_unique ;

    for ($nf = 0; $nf <= $#mseed ; $nf++ ) {
        $row_1  = $nf if ( $msd1 =~ /$mseed[$nf]/ ) ;
        $row_2  = $nf if ( $msd2 =~ /$mseed[$nf]/ ) ;
    }

    $nmiss       = $row_2  - $row_1  - 1 ;
    $zerolength  = 0 ;
    $alreadyproc = 0 ;
    $onbaler     = 0 ;
    @skip = () ;

    fork_debug   ( $parent, "nmiss  $nmiss	$msd2	row_2  $row_2	$msd1	row_1  $row_1" ) if $opt_V ;

    if ( $nmiss > 0 ) {
        foreach $row ( ( $row_1 + 1 )..( $row_2 - 1 ) ) {
            fork_debug   ( $parent, "row  $row" ) if $opt_V ;
            fork_debug   ( $parent, "dfile  $mseed[$row]" ) if $opt_V ;
            if ( ! exists $dfile_unique{$mseed[$row]}{downloaded} ) {
                $onbaler++ ;
                next ;
            }
            $zerolength++  if ( $dfile_unique{$dfile}{filebytes} == 0 ) ;
        }
        fork_debug   ( $parent, "row_1	$row_1	row_2	$row_2	zerolength	$zerolength") if $opt_V ;
            
        if ( $nmiss != $zerolength ) {
            $prob++  unless ( $opt_F || $opt_G ) ;
            $ngap++ ;
            
            $string =  sprintf( "Problems in processing baler 44 mseed files!" )  ;
            &print_prob ( $prob, $string, $parent, *PROB ) unless ( $opt_F || $opt_G ) ;
                        
            foreach $row ( ( $row_1 + 1 )..( $row_2 - 1 ) ) {                
                if ( ! exists $dfile_unique{ $mseed[$row] }{ downloaded } ) {
                    $string  =  "	$mseed[$row]    is still on baler"  ;
                    push ( @skip, $mseed[$row] ) if ( $opt_F || $opt_G ) ;
                    &complain_or_notify ( ( $opt_F || $opt_G ), $parent, $string )  ;
                    print PROB "         $string \n" ;
                } else {
                    if ( exists $dfile_unique{ $mseed[$row] }{ msdtime } ) {
                        $alreadyproc++ ;
                        $string =  sprintf( "	$mseed[$row]    already processed    msdtime	%s", strydtime( $dfile_unique{ $mseed[$row] }{ msdtime } )  )  ;
                    } else {
                        $string =  sprintf( "	$mseed[$row]    not processed yet	" )  ;
                    }
                    fork_complain   ( $parent, $string ) ; 
                    print PROB "$string \n" ;
                }                
            }
            
        }
        $string =  sprintf( "missing_files_since_last_proc")  ;
        &complain_or_notify ( ( $opt_F || $opt_G ), $parent, $string )  ;
        print PROB "$string \n" ;
        $string =  sprintf( "	Number of files expected                   - %d", $nmiss )  ;
        &complain_or_notify ( ( $opt_F || $opt_G ), $parent, $string )  ;
        print PROB "$string \n" ;
        $string =  sprintf( "	Number of files on baler                   - %d", $onbaler )  ;
        &complain_or_notify ( ( $opt_F || $opt_G ), $parent, $string )  ;
        print PROB "$string \n" ;
        $string =  sprintf( "	Number of expected files not processed     - %d", $nmiss - $alreadyproc )  ;
        &complain_or_notify ( ( $opt_F || $opt_G ), $parent, $string )  ;
        print PROB "$string \n" ;
        $string =  sprintf( "	Number of expected files already processed - %d", $alreadyproc )  ;
        &complain_or_notify ( ( $opt_F || $opt_G ), $parent, $string )  ;
        print PROB "$string \n" ;
    }
    
    fork_debug (  $parent, "end missing_files" ) if $opt_V ;
    return ( $nmiss, $ngap, $prob, @skip ) ;
}

sub find_missing_files { # ( $nmiss, $ngap, $prob, @skip ) = &find_missing_files( $nmiss, \%dfile_unique, \%dfile_unprocessed, $parent, $prob ) ;
    my ( $nmiss, $refu, $refc, $parent, $prob ) = @_ ;
    my ( $last_good, $last_proc, $mseed1, $mseed2, $next_proc, $ngap, $nrows_to_proc, $onbaler, $shift, $string, $tmp_gap, $unproc ) ;
    my ( @skip, @unique, @unprocessed ) ;
    my ( %dfile_unique, %dfile_unprocessed ) ;
    
    fork_notify (  $parent, "starting missing file check" ) ;
    fork_debug (  $parent, "find_missing_files ( $nmiss, \%dfile_unique, \%dfile_unprocessed, $parent, $prob )" ) if $opt_V ;

    %dfile_unique       = %$refu ;     # unique mseed files
    %dfile_unprocessed  = %$refc ;     # unprocessed mseed files
         
    @unique        = sort { $dfile_unique{$a}{time} <=> $dfile_unique{$b}{time} } keys %dfile_unique ;
    @unprocessed   = sort { $dfile_unprocessed{$a}{time} <=> $dfile_unprocessed{$b}{time} } keys %dfile_unprocessed ;
    
    until ( $unique[0] =~ /$unprocessed[0]/ ) {
        shift @unique ;
    }

    fork_debug   ( $parent, "first unprocessed file is $unique[0]	$unprocessed[0] " ) if $opt_V ;
    
    $nrows_to_proc = 0 ;
    $ngap          = 0 ;
    $last_good     = "" ;
    @skip          = () ;
    
    foreach $unproc ( @unprocessed ) {
        last if ( $unproc =~ /$unprocessed[$#unprocessed]/ ) ;
        
        fork_debug   ( $parent, "unprocessed file is $unproc	$unprocessed[$#unprocessed] " )  if $opt_V ;
        $shift     = 0 ;
        $tmp_gap   = 0 ;
        $nmiss     = 0 ;
        $onbaler   = 0 ;
        $last_proc = $unique[0] ;
        $next_proc = $unique[1] ;
                
        until ( $unique[0] =~ /$unproc/ ) {
            shift @unique ;
            
            fork_debug   ( $parent, "$unique[0]	$unproc	shift $shift" ) if $opt_V ;
            
            if ( $shift == 1 ) {
                $string  =  "	$next_proc    is still on baler"  ;
                &complain_or_notify ( ( $opt_F || $opt_G ), $parent, $string )  ;
                print PROB "$string \n" ;
                push ( @skip, $next_proc ) if ( $opt_F || $opt_G ) ;
                $onbaler++ ;
            }
            
            last if ( $unique[0] =~ /$unproc/ ) ;
            
            $shift++ ;
            
            if ( $shift > 1 ) {
                $string  =  "	$unique[0]    is still on baler"  ;
                &complain_or_notify ( ( $opt_F || $opt_G ), $parent, $string )  ;
                print PROB "$string \n" ;
                push ( @skip, $unique[0] ) if ( $opt_F || $opt_G ) ;
                $onbaler++ ;
            }
        }
        
        fork_debug   ( $parent, "$unique[0]	$unproc	shift $shift" ) if ( $shift > 1 && $opt_V ) ;

        if ( $shift > 0 ) {
            $nmiss += $shift ;
            $tmp_gap++ ;
            $prob++ unless ( $opt_F || $opt_G ) ;
            
            $string =  sprintf( "	Number of files expected                   - %d", $nmiss )  ;
            &complain_or_notify ( ( $opt_F || $opt_G ), $parent, $string )  ;
            print PROB "$string \n" ;
            $string =  sprintf( "	Number of files on baler                   - %d", $onbaler )  ;
            &complain_or_notify ( ( $opt_F || $opt_G ), $parent, $string )  ;
            print PROB "$string \n" ;
        } elsif ( ! $last_good ) {
            $nrows_to_proc++ ;
        }
        
        unless ( $opt_F || $opt_G ) {
            $ngap     += $tmp_gap ;
            $last_good = $last_proc if ( $ngap == 1 && ! $last_good ) ;
        }
    }
    
    if ( $ngap > 0 ) {
        $string = " " ;
        fork_notify ( $parent, $string ) if $opt_v ;
        $string = "last dfile to process	$last_good " ;
        fork_notify ( $parent, $string ) if $opt_v ;
        print PROB "$string \n\n" ;

        $string = "$nrows_to_proc rsyncbaler dfiles with status =~ /downloaded/  && msdtime == NULL before first missing file" ;
        fork_notify ( $parent, $string ) if $opt_v ;
        print PROB "$string \n\n" ;
    }    
    
    return ( $nmiss, $ngap, $prob, @skip ) ;
}

sub delinquent_files { # ( $prob, $alreadyproc ) = &delinquent_files( \%dfile_unique, \%dfile_unprocessed, $parent, $prob ) ;
#
#  find any files which have now appeared which are prior to already processed files
#
    my ( $refu, $refc, $parent, $prob ) = @_ ;
    my ( $alreadyproc, $dfile, $ndelinquent, $not_proc, $row, $string, $unique ) ;
    my ( @unique, @unprocessed ) ;
    my ( %dfile_unique, %dfile_unprocessed ) ;
    
    fork_notify (  $parent, "starting delinquent file check" ) ;

    fork_debug (  $parent, "delinquent_files( \%dfile_unique, \%dfile_unprocessed, $parent, $prob )" ) if $opt_V ;

    %dfile_unique       = %$refu ;     # unique mseed files
    %dfile_unprocessed  = %$refc ;     # unprocessed mseed files
         
    @unique        = sort { $dfile_unique{$a}{time} <=> $dfile_unique{$b}{time} } keys %dfile_unique ;
    @unprocessed   = sort { $dfile_unprocessed{$a}{time} <=> $dfile_unprocessed{$b}{time} } keys %dfile_unprocessed ;
    
    until ( $unique[0] =~ /$unprocessed[0]/ ) {
        shift @unique ;
    }

    fork_debug   ( $parent, "first unprocessed file is $unique[0]	$unprocessed[0] " ) if $opt_V ;

    $ndelinquent = 0 ;
    $alreadyproc = 0 ;
    $not_proc    = 0 ;
    
    foreach $unique ( @unique ) {
        
        fork_debug   ( $parent, "delinquent check file is $unique" )  if $opt_V ;
        
        if ( exists $dfile_unique{ $unique }{ msdtime } ) {
            $ndelinquent++ ;
            $prob++ ;
            last ;
        }

    }
    
    if ( $ndelinquent ) {
        
        $string =  sprintf( "" )  ;
        fork_complain ( $parent, $string ) ;
        $string =  sprintf( "delinquent    files have appeared!" )  ;
        fork_complain ( $parent, $string ) ;
        
        foreach $unique ( @unique ) {        
        
            if ( exists $dfile_unique{ $unique }{ msdtime } ) {
                $alreadyproc++ ;
                $string =  sprintf( "	$unique    already processed    msdtime	%s", strydtime( $dfile_unique{ $unique }{ msdtime } )  )  ;
            } else {
                $not_proc++ ;
                $string =  sprintf( "	$unique    not processed yet	" )  ;
            }
            fork_complain ( $parent, $string ) ;
            print PROB "$string \n" ;
        }
        $string =  sprintf( "	Number of files expected                   - %d", $alreadyproc + $not_proc  )  ;
        fork_complain ( $parent, $string ) ;
        print PROB "$string \n" ;
        $string =  sprintf( "	Number of expected files not processed     - %d", $not_proc )  ;
        fork_complain ( $parent, $string ) ;
        print PROB "$string \n" ;
        $string =  sprintf( "	Number of expected files already processed - %d", $alreadyproc )  ;
        fork_complain ( $parent, $string ) ;
        print PROB "$string \n" ;
    }

         
    return ( $prob, $alreadyproc ) ;

}

sub check_time_between_files { # ( $prob ) = &check_time_between_files( $sta, \%dfile_unprocessed, $parent, $prob ) ;
#
#  find any differences between times of adjacent files which are too long indicating possible missing data.
#
    my ( $sta, $refu, $parent, $prob ) = @_ ;
    my ( $actual_time, $mseed, $mseed_offset, $ngap, $offset, $rt, $string, $string1 ) ;
    my ( @dbrt, @mseed, @mseed_offset ) ;
    my ( %dfile_unprocessed ) ;
    
    fork_notify (  $parent, "starting check_time_between_files" ) ;

    %dfile_unprocessed  = %$refu ;     # unprocessed mseed files
    
    @mseed        = sort { $dfile_unprocessed{$a}{time} <=> $dfile_unprocessed{$b}{time} } keys %dfile_unprocessed ;
    @mseed_offset = () ;

    $string =  "	$pf{rt_sta_dir}\/$sta\/$sta does not exist!"   ;
    if ( ! -f "$pf{rt_sta_dir}\/$sta\/$sta" && ! ( $opt_F || $opt_B ) ) {
        $prob++ ;
        &print_prob ( $prob, $string, $parent, *PROB ) ;
        $rt = 0 ; 
    } elsif ( ! -f "$pf{rt_sta_dir}\/$sta\/$sta"  ) {
        fork_notify ( $parent, $string ) ;
        &print_prob ( $prob, $string, $parent, *PROB ) ;
        $rt = 0 ; 
    } else {
        $rt = 1 ; 
    }
    
    if ( $rt ) {
        @dbrt  = dbopen   ( "$pf{rt_sta_dir}\/$sta\/$sta", "r" ) ;
        @dbrt  = dblookup ( @dbrt, , 0, "wfdisc", 0, 0 ) ;
        @dbrt  = dbsubset ( @dbrt, "chan =~ /BHZ/ " ) ;
    }

#
#  look for unusually long times between adjacent mseed files
#
    foreach $mseed ( 1..$#mseed ) {
        $mseed_offset = $dfile_unprocessed{ $mseed[ $mseed ] }{ time } - $dfile_unprocessed{ $mseed[ $mseed - 1 ] }{ time } ;
        
        if ( $mseed_offset > 86400 ) {
            
            if ( $rt ) { 
                $actual_time = &data_time_check ( $dfile_unprocessed{ $mseed[ $mseed - 1 ] }{ time }, $dfile_unprocessed{ $mseed[ $mseed ] }{ time }, @dbrt ) ;
            } else {
                $actual_time = 0 ;
            }

            $string =  sprintf( "	%s between %s      and    %s,     should be ~8 hours", strtdelta( $mseed_offset ), $mseed[ $mseed - 1 ],  $mseed[ $mseed ] )  ;
            
            $string1 =  sprintf( "	%s between %s and    %s of rt BHZ data exists", strtdelta( $actual_time ), 
                       strydtime ( $dfile_unprocessed{ $mseed[ $mseed - 1 ] }{ time } ) , strydtime ( $dfile_unprocessed{ $mseed[ $mseed ] }{ time } )  )  ;

            if ( $dfile_unprocessed{ $mseed[ $mseed ] }{ media } != $dfile_unprocessed{ $mseed[ $mseed - 1 ] }{ media } ) {
                if ( $opt_F || $opt_G ) {
                    fork_notify ( $parent, $string ) ;
                    fork_notify ( $parent, $string1 ) ;
                    $string =  sprintf( "	media changed from    %s to     %s", 
                                         $dfile_unprocessed{ $mseed[ $mseed - 1 ] }{ media },  $dfile_unprocessed{ $mseed[ $mseed ] }{ media } ) ;
                    fork_notify ( $parent, $string ) ;
                    $string =  sprintf( "	ignoring missing data since -F or -G option used" ) ;
                    fork_notify ( $parent, $string ) ;
                } else {
                    fork_complain ( $parent, $string ) ;
                    fork_complain ( $parent, $string1 ) ;
                    $string =  sprintf( "	media changed from    %s to     %s", 
                                         $dfile_unprocessed{ $mseed[ $mseed - 1 ] }{ media },  $dfile_unprocessed{ $mseed[ $mseed ] }{ media } ) ;
                    fork_complain ( $parent, $string ) ;
                    $string =  sprintf( "	download missing data from media     %s",  $dfile_unprocessed{ $mseed[ $mseed ] }{ media } ) ;
                    fork_complain ( $parent, $string ) ;
                    $ngap++ ;
                    $prob++ ;
                }
            } else {
                fork_notify ( $parent, $string ) ;
                fork_notify ( $parent, $string1 ) ;
                $string =  sprintf( "	media did not change, data corrupted on baler 44" ) ;
                fork_notify ( $parent, $string ) ;
            } 
            
        }
        push @mseed_offset, $mseed_offset ;
    }
    
    if ( $opt_V ) {
        foreach $offset ( sort { $b <=> $a } @mseed_offset ) {
            fork_notify ( $parent, sprintf ("%s", strtdelta( $offset ) ) ) ;
        }
    }

    dbclose ( @dbrt ) if ( $rt ) ;
    
    return ( $prob ) ;

}