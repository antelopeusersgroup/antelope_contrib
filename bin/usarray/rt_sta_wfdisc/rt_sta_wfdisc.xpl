#
#   program needs:
#
#
    use Getopt::Std ;
    use POSIX;    
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

    my ( $Pf, $arg, $cmd, $db, $fh, $good, $idle, $iowait, $kernel, $max_forks, $nchild, $ncpu, $nstas, $parent, $pid, $problems, $resp, $sta, $stime, $string, $subject, $swap, $usage, $user );
    my (  @procs, @stas, @the_rest );
    my (  %errors, %logs, %stas );


    $pgm = $0 ; 
    $pgm =~ s".*/"" ;
    elog_init($pgm, @ARGV);
    $cmd = "\n$0 @ARGV" ;
    
    if (  ! getopts('vVnf:m:p:s:') || ( @ARGV != 1 && @ARGV != 3 && @ARGV != 4 ) ) { 
        $usage  =  "\n\n\nUsage: $0  [-v] [-V] [-n] " ;
        $usage .=  "[-p pf] [-m mail_to] [-f nforks] [-s sta_regex] db  \n\n" ;
        $usage .=  "Usage: $0  [-v] [-V] [-n] " ;
        $usage .=  "[-p pf]  " ;
        $usage .=  "sta yearday_start yearday_end\n\n"  ;         
        $usage .=  "Usage: $0  [-v] [-V] [-n] " ;
        $usage .=  "[-p pf]  " ;
        $usage .=  "sta yearday_start yearday_end parent_pid\n\n"  ;         
        elog_notify($cmd) ; 
        elog_die ( $usage ) ; 
    }
    
    announce( 0, 0 ) if ( @ARGV == 1 ) ;
    
    $Pf         = $opt_p || $pgm ;
    if ( @ARGV > 1 && $opt_v ) {
        $opt_v = 0 ;
        %pf = getparam( $Pf );
        $opt_v = 1 ;
    } elsif ( @ARGV > 1 ) {
        %pf = getparam( $Pf );
    }
    $opt_v      = defined($opt_V) ? $opt_V : $opt_v ;    
    chop ($host = `uname -n` ) ;
    $IPC::Cmd::VERBOSE = 1 if $opt_v ;
    STDOUT->autoflush(1) ;
    $parent   = $$ ;
    $problems = 0;

    if ( @ARGV == 3 ) {
        if ( $opt_v ) {
            $opt_v = 0 ;
            %pf = getparam( $Pf ) ;
            $opt_v = 1 ;
        }        
        &proc_sta( $ARGV[0], $ARGV[1], $ARGV[2], $parent ) ;
        exit 0 ;
    }

    if ( @ARGV == 4 ) {
        $parent = $ARGV[3] ;
        if ( $opt_v ) {
            $opt_v = 0 ;
            %pf = getparam( $Pf ) ;
            $opt_v = 1 ;
        }        
        &proc_sta( $ARGV[0], $ARGV[1], $ARGV[2], $parent ) ;
        exit 0 ;
    }

    &savemail() if $opt_m ; 
    elog_notify($cmd) ; 
    $stime = strydtime(now());
    elog_notify ("\nstarting execution on	$host	$stime\n\n");

    $db = $ARGV[0] ;
    
    %pf = getparam( $Pf ) ;

    ( $ncpu, $idle, $user, $kernel, $iowait, $swap, @the_rest ) = syscpu() ;
    
    $max_forks = $opt_f || int ( $ncpu / 2 ) ;
    $max_forks = 1  if ( $max_forks < 1 ) ;
    $max_forks = 20 if ( $max_forks > 20 ) ;

    elog_notify ("ncpus	$ncpu	max_forks	$max_forks") if $opt_v ;
                
    if (system_check(0)) {
        $subject = "Problems - $pgm $host	Ran out of system resources";
        &sendmail($subject, $opt_m) if $opt_m ; 
        elog_die("\n$subject");
    }
    
#        
#   subset for unprocessed data
#
    ( $problems, %stas ) = &get_stas( $db, $problems ) ;
    
    @stas = sort keys %stas ;
    
    $nstas = $#stas + 1 ;
    
    elog_debug ( "get_stas returned" ) if $opt_V ;
    prettyprint ( \%stas ) if $opt_V ; 

    elog_notify ( "$nstas stations to process\n\n" ) ;

#
#  process all new stations
#
        
    STA: foreach $sta ( @stas ) {

#
# Verify how many procs we have running
#
        @procs = check_procs(@procs);

#
# Read messages from pipes
#
        
        &nonblock_read( \%stas, \%logs, \%errors ) ;

#
# Wait if we max out of available procs
#
        if ( scalar(@procs) >= $max_forks  ) {
            sleep 1;
            redo STA;
        }

        $logs{$sta}{lines}      = 0 ;
        $errors{$sta}{problems} = 0 ;
        
#
# Fork the script
#        
        
        $pid = open($stas{$sta}{fh}, "-|") ;
        
        $stas{$sta}{fh}->autoflush(1);
        fcntl( $stas{$sta}{fh}, F_SETFL, O_NONBLOCK );
        
        if ( $pid ) {
            elog_debug("PID:[$pid] sta:[$sta]") if $opt_V;
        
            push @procs, $pid ;
            $stas{$sta}{pid} = $pid ;
        
            elog_debug("Now:[".@procs."] => @procs") if $opt_V;
        } else {

#
# Child process
#

            $cmd   =  "rt_sta_wfdisc " ;
            $cmd  .=  "-n " if $opt_n;
            $cmd  .=  "-v " if $opt_v;
            $cmd  .=  "-V " if $opt_V;
            $cmd  .=  "-p $opt_p " if $opt_p;
            $cmd  .=  "$sta $stas{$sta}{proc_start} $stas{$sta}{proc_end} $parent" ;
            
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
        elog_debug("Wait for:[".@procs."] => @procs") if $opt_V;
        &nonblock_read( \%stas, \%logs, \%errors );
#         &nbfh_read ( \%stas, \%logs, \%errors ) ;

        sleep 1;

    }
    
#
# Find aborted child
#
    $string = "proc_sta complete for" ;
    
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
    $stime = strydtime(now());
    elog_notify ("completed 	$stime\n\n");

    if ($problems == 0 ) {
        $subject = sprintf("Success  $pgm  $host  $nstas stations processed");
        elog_notify ($subject);
        &sendmail ( $subject, $opt_m ) if $opt_m ;
    } else { 
        $subject = "Problems - $pgm $host	$nstas stations processed, $nchild stations with problems, $problems total problems" ;
        &sendmail($subject, $opt_m) if $opt_m ; 
        elog_complain("\n$subject") ;
        exit(1);
    }
  
    exit(0);
}

sub get_stas { # ( $problems, %stas ) = &get_stas( $db, $problems ) ;
    my ( $db, $problems ) = @_ ;
    my ( $dir, $endtime, $endtime_null, $equip_install, $jdate, $key, $more_days, $nrows, $row, $rtdb, $sta, $subject, $time ) ;
    my ( @db, @dbdeploy, @dbnull ) ;
    my ( %stas, %stas_out ) ;
#
#  setup db
#
    @db           = dbopen ( $db, "r" ) ;
    @dbdeploy     = dblookup ( @db, 0, "deployment", 0, 0) ;
    @dbnull       = dblookup ( @dbdeploy, 0, 0, 0, "dbNULL") ;
    $endtime_null = dbgetv ( @dbnull, qw( endtime ) ) ;
    
    if (! dbquery( @dbdeploy, dbTABLE_PRESENT ) ) {
        $problems++ ;
        elog_complain( "\nProblem $problems" ) ;
        elog_complain( "	$db.deployment - does not exist!" ) ;
        $subject = "Problems - $pgm $host	$problems problems" ;
        &sendmail($subject, $opt_m) if $opt_m ; 
        elog_die("\n$subject") ;
    }
    
    @dbdeploy = dbsubset ( @dbdeploy, "snet =~ /$pf{net}/" ) ;
    @dbdeploy = dbsubset ( @dbdeploy, "sta =~ /$opt_s/" ) if $opt_s ;
    @dbdeploy = dbsort   ( @dbdeploy, "sta", "cert_time" ) ;
    
    $nrows    = dbquery ( @dbdeploy, "dbRECORD_COUNT" ) ;
    
    %stas = () ;
#
#  load %stas with time-endtime data from deployment table
#
    for ($row = 0; $row<$nrows; $row++) {
        $dbdeploy[3] = $row ;
        ( $sta, $time, $endtime, $equip_install )  = dbgetv ( @dbdeploy, qw( sta time endtime equip_install ) ) ;
        $stas{$sta}{start}        = yearday( $time ) ;
        $stas{$sta}{proc_start}   = yearday( $equip_install ) ;
        if ( exists $stas{$sta}{endtime} ) {
            if ( $endtime != $endtime_null ) {
                $stas{$sta}{endtime} = $endtime if ( $endtime > $stas{$sta}{endtime} ) ;
            } elsif ( exists $stas{$sta}{endtime} ) {
                delete $stas{$sta}{endtime} ;
                delete $stas{$sta}{end} ;
            }
        } else {
            $stas{$sta}{endtime} = $endtime if ( $endtime != $endtime_null ) ;
            $stas{$sta}{end}     = yearday ( $endtime ) if ( $endtime != $endtime_null ) ;
        }
    }
     
    dbclose( @db ) ;
#
#  refine processing times defined in %stas
#
    prettyprint( \%stas ) if $opt_V ;
    
    foreach $sta (sort keys %stas) {
        $rtdb          = "$pf{rt_sta_dir}/$sta/$sta" ;
#
#  find if sta has some processing completed
#
        if ( -f $rtdb  && -f "$rtdb.wfdisc" ) {
#
#  find last jdate processed
#
            $stas{$sta}{last_jdate} = &rtdb_jdate ( $rtdb ) ;
#
#  find next jdate to process
#
            if ( $stas{$sta}{last_jdate} != $stas{$sta}{end} ) {
                $stas{$sta}{proc_start} = &next_jdate ( $stas{$sta}{last_jdate} ) ; 
            }
            
            elog_debug ( "get_stas check if some processing completed " ) if $opt_V ;
            prettyprint( \%stas ) if $opt_V ;
        }
#
#  if station is closed and some processing has started
#
        if ( exists $stas{$sta}{end} && -f $rtdb ) {
            if ( $stas{$sta}{last_jdate} == $stas{$sta}{end} ) {
                delete $stas{$sta} ;
                elog_notify ( "$sta already complete" ) ;
            }
            if ( $stas{$sta}{last_jdate} < $stas{$sta}{end} ) {
                $jdate = &next_jdate ( $stas{$sta}{last_jdate} ) ; 
                $more_days = 0 ;
                while ( $jdate <= $stas{$sta}{end} ) {
                    $dir         = &jdate_to_dir ( $jdate ) ;
                    if ( &exist_data ( $dir, $sta, $$, 0 ) ) {
                        $more_days++ ;
                        $stas{$sta}{proc_end} = $jdate ;
                    }
                    $jdate       = &next_jdate ( $jdate ) ;
                }
                if ( $more_days ) {
                    elog_notify ( "$sta not complete, $more_days days to process" ) ;
                } else {
                    delete $stas{$sta} ;
                    elog_notify ( "$sta already complete" ) ;
                }
            }
            elog_debug ( "get_stas station closed " ) if $opt_V ;
            prettyprint( \%stas ) if $opt_V ;
#
#  if station is closed and no processing has started
#
        } elsif ( exists $stas{$sta}{end} && ( ! -f $rtdb ) ) {
            $stas{$sta}{proc_end} = $stas{$sta}{end} ;
            elog_debug ( "get_stas station closed and no processing has started" ) if $opt_V ;
            prettyprint( \%stas ) if $opt_V ;
#
#  if station is open and processing has started
#
        } elsif ( ( ! exists $stas{$sta}{end} ) && -f "$rtdb.wfdisc"  ) {
            $stas{$sta}{proc_end} = &find_last_proc( $sta, $stas{$sta}{last_jdate} ) ;
            delete $stas{$sta} if ( $stas{$sta}{proc_end} == $stas{$sta}{last_jdate} ) ; # no new waveforms to process yet
            elog_debug ( "get_stas station is open and processing has started" ) if $opt_V ;
            prettyprint( \%stas ) if $opt_V ;

#
#  if station is open and no processing has started
#
        } elsif ( ( ! exists $stas{$sta}{end} ) && ( ! -f "$rtdb.wfdisc" ) ) {
            $stas{$sta}{proc_end} = &find_last_proc( $sta, $stas{$sta}{proc_start} ) ;
            delete $stas{$sta} if ( $stas{$sta}{proc_end} == 0 ) ; # no waveforms to process yet
            elog_debug ( "get_stas station is open and no processing has started" ) if $opt_V ;
            prettyprint( \%stas ) if $opt_V ;
        }
#
#  this step put in because delete is not behaving as expected.
#
        if ( exists $stas{$sta}{proc_start} ) {
            elog_debug( sprintf( "keys    -    %s", join ( " " , sort ( keys %{$stas{$sta}} ) ) ) ) if $opt_V ;
            foreach $key ( keys %{$stas{$sta}} ) {
                $stas_out{$sta}{$key} = $stas{$sta}{$key} ;
            }
        }
    }
    
    elog_debug ( "get_stas just before return" ) if $opt_V ;
    prettyprint( \%stas ) if $opt_V ;
        
    elog_notify( sprintf( "stas    -    %s", join ( " " , sort ( keys %stas_out ) ) ) ) ;
            
    return ( $problems, %stas_out ) ;
}

sub rtdb_jdate {  # $last_jdate = &rtdb_jdate ( $db ) ;
    my ( $db ) = @_ ;
    my ( $lastdir, $last_jdate, @tree ) ;
    my ( @db, @dbwfdisc ) ;
    
    elog_debug ( "db	$db" )  if $opt_V ;
    
    @db          = dbopen   ( $db, "r" ) ;
    @dbwfdisc    = dblookup ( @db, 0, "wfdisc", 0, 0) ;
    @dbwfdisc    = dbsort ( @dbwfdisc, qw ( -u -r jdate dir ) ) ;
    $dbwfdisc[3] = 0 ;
    
    $lastdir     = dbgetv ( @dbwfdisc, qw ( dir ) ) ;
    
    @tree = split ( "/", $lastdir  ) ;
    
    $last_jdate  = $tree[$#tree - 1] * 1000 + $tree[ $#tree ] ;

    elog_debug ( "lastdir	$lastdir	last_jdate	$last_jdate" )  if $opt_V ;
    
    dbclose ( @db )  ;
    
    return $last_jdate ;
}

sub jdate_to_dir {  # $dir = &jdate_to_dir ( $jdate ) ;
    my ( $jdate ) = @_ ;
    my ( $dir ) ;
    
    $dir = epoch2str ( epoch($jdate), "$pf{wfbase}/%Y/%j" ) ;
    return $dir ;
}

sub exist_data {  # $jdate = &exist_data ( $dir, $sta, $parent ) ;
    my ( $dir, $sta, $parent ) = @_ ;
    my ( $atime, $blksize, $blocks, $ctime, $dev, $file, $gid, $ino, $jdate, $jdate_file ) ; 
    my ( $mode, $mseed, $mtime, $nlink, $rdev, $size, $string, $uid ) ;
    my ( @mseedfiles ) ;
    
    $jdate = 0 ;
    
    opendir( DIR, $dir ) ;
    @mseedfiles = sort ( grep { /.*$sta.*/  } readdir(DIR) ) ;
    closedir( DIR );
        
    if ($#mseedfiles == -1 ) {

        $string = "	$sta has no files in $dir !" ; 
        fork_debug ( $parent, "$sta - $string" ) if $opt_V ;
        
    } else {
        foreach $mseed ( @mseedfiles ) {
            $file = "$dir/$mseed" ;
            ( $dev, $ino, $mode, $nlink, $uid, $gid, $rdev, $size, 
              $atime, $mtime, $ctime, $blksize, $blocks) = stat($file) ;
            $jdate_file = yearday( $mtime ) ; 
            fork_debug ( $parent, "$jdate_file	$file" ) if $opt_V ;
            $jdate = $jdate_file if ( $jdate_file > $jdate ) ;
        }
    }
    return $jdate ;
}

sub find_last_proc { # $proc_end = &find_last_proc( $sta, $last_proc ) ;
    my ( $sta, $last_proc ) = @_ ;
    my ( $dir, $jdate, $jdate_file, $lag, $nday, $ndays, $proc_end ) ;
    
    $jdate     = yearday( now() ) ;
    $proc_end  = yearday( now() ) ;
    
    $ndays = 2 ;  # number of days data needs to be on disk before adding to wfdisc
    
    for ($nday = 0; $nday< $pf{days_delay}; $nday++) {
        $proc_end = &prev_jdate ( $proc_end ) ;
    }
    
    $lag       = 0 ;
    
    while ( $jdate > $last_proc ) {
        $dir        = &jdate_to_dir ( $jdate ) ;
        $jdate_file = &exist_data ( $dir, $sta, $$, 0 ) ;
        $lag++ if ( $jdate_file ) ; 
        elog_debug ( "$dir	$sta	$jdate_file	$jdate	$lag	$proc_end" ) if $opt_V ;
        if ( $lag > 2 && $jdate <= $proc_end ) {
            return $jdate ;
        }
        $jdate      = &prev_jdate ( $jdate ) ;
    }
    
    if ( $jdate == $last_proc ) {
        return $last_proc ;
    }
    return 0 ;
}

sub proc_sta { # &proc_sta( $sta, $proc_start, $proc_end, $parent ) ;
    my ( $sta, $proc_start, $proc_end, $parent ) = @_ ;
    my ( $cmd, $dir, $etime, $jdate, $prob, $prob_check, $rt_sta_dir, $stime, $subject, $success ) ;
    my ( @output ) ;

    $stime = strydtime( now() );
        
    open( PROB,"> /tmp/prob_$sta\_$$") ;

    $subject = "starting processing station $sta    $proc_start    $proc_end    $stime " ;
    elog_notify ( $subject ) ;
    fork_notify ( $parent, $subject ) ;
    print PROB "$subject \n\n" ;

    $prob_check = $prob = 0;
        
    $rt_sta_dir = "$pf{rt_sta_dir}/$sta" ;
    makedir ( $rt_sta_dir ) ;
    
    chdir( $rt_sta_dir ) ;
    fork_notify ( $parent, "Changed directory to $rt_sta_dir " ) if $opt_v ;
    fork_notify ( $parent, "starting miniseed2db processing " ) if $opt_v ;

    &cssdescriptor ($sta,$pf{dbpath},$pf{dblocks},$pf{dbidserver}) unless $opt_n;
    
    $jdate = $proc_start ;
    
    while ( $jdate <= $proc_end ) {
        $dir = &jdate_to_dir ( $jdate ) ;

        if ( &exist_data ( $dir, $sta, $parent ) ) {

            $cmd  = "miniseed2db $dir/\*$sta\* $sta ";

            ( $success, @output )  = &run_cmd( $cmd ) ;

            if ( ! $success ) {
                $prob++ ;
                &print_prob ( $prob, "FAILED: $cmd", $parent, *PROB ) ;
                last ;
            }

        }
        $jdate = &next_jdate ( $jdate ) ;
    }
        
    unless ( $prob ) {
        fork_notify ( $parent, "starting rt_daily_return processing" ) if $opt_v ;
        $stime = $proc_start ;
        $etime = yearday ( epoch( $proc_end ) + 86400 )  ;
        $cmd  = "rt_daily_return ";
        $cmd .= "-t \"$stime\" -e \"$etime\" ";
        $cmd .= "-s \"sta =~/$sta/ && chan=~/[BL]H./\" $sta $sta";
        
        ( $success, @output )  = &run_cmd( $cmd ) ;

        if ( ! $success ) {
            $prob++ ;
            &print_prob ( $prob, "FAILED: $cmd", $parent, *PROB ) ;
        }
        
        $cmd  = "dbfixchanids $sta ";
        fork_notify ( $parent, $cmd ) if $opt_v ;

        ( $success, @output )  = &run_cmd( $cmd ) ;

        if ( ! $success ) {
            $prob++ ;
            &print_prob ( $prob, "FAILED: $cmd", $parent, *PROB ) ;
        }
        
        $prob = &fix_wfdisc( $sta, $parent, $prob ) ;
        
    }
    
    $stime = strydtime( now() );
    print PROB "$stime      end processing \n\n" ;
    close(PROB);
                 
#
#  clean up
#
        
    if  ( $prob ) {
        $subject = "TA $sta     $prob problems -  $pgm on $host";
        $cmd     = "rtmail -C -s '$subject' $pf{prob_mail} < /tmp/prob_$sta\_$$";
        fork_notify ( $parent, $cmd) ;
        
        ( $success, @output )  = &run_cmd( $cmd ) ;

    } elsif ( $opt_v ) {
        $subject = "TA $sta completed successfully -  $pgm on $host";
        $cmd     = "rtmail -C -s '$subject' $pf{success_mail} < /tmp/prob_$sta\_$$";
        fork_notify ( $parent, $cmd) ;
        
        ( $success, @output )  = &run_cmd( $cmd ) ;

    } 

    unlink "/tmp/prob_$sta\_$$" unless $opt_V;
    
    $stime = strydtime( now() );
    fork_notify ( $parent, "proc_sta complete for $sta with $prob problems    $stime") ;

    return ;
}

sub fix_wfdisc {  # $problems = &fix_wfdisc( $db, $parent, $problems ) ;
# 
#  sets calib, calper, segtype in wfdisc
#
    my ( $db, $parent, $problems ) = @_ ; 
    my ( $cmd, $success ) ;
    my ( @db, @dbcalibration, @output ) ; 

    fork_debug ( $parent, "fix_wfdisc  db = $db   problems = $problems") if ( $opt_V ) ;
    
    @db              = dbopen  ( $db, "r+" ) ;
    @dbcalibration   = dblookup( @db, "", "calibration", "", "" ) ;
    

    if (dbquery ( @dbcalibration, "dbRECORD_COUNT" ) ) {
        
        $cmd = "dbjoin $db.wfdisc calibration | dbsubset - \"wfdisc.calib != calibration.calib\" | dbselect -s - \"wfdisc.calib:=calibration.calib\" " ;
        
        $success = run ( $cmd, 60 ) ;
        if ( $success ) {
            $problems++ ;
            &print_prob ( $problems, "FAILED: $cmd", $parent, *PROB ) ;
        }

#         ( $success, @output )  = &run_cmd( $cmd, 60 ) ;
# 
#         if ( ! $success ) {
#             $problems++ ;
#             &print_prob ( $problems, "FAILED: $cmd", $parent, *PROB ) ;
#         }

        $cmd = "dbjoin $db.wfdisc calibration | dbsubset - \"wfdisc.calper != calibration.calper\" | dbselect -s - \"wfdisc.calper:=calibration.calper\"  " ;

        $success = run ( $cmd, 60 ) ;
        if ( $success ) {
            $problems++ ;
            &print_prob ( $problems, "FAILED: $cmd", $parent, *PROB ) ;
        }

#         ( $success, @output )  = &run_cmd( $cmd, 60 ) ;
# 
#         if ( ! $success ) {
#             $problems++ ;
#             &print_prob ( $problems, "FAILED: $cmd", $parent, *PROB ) ;
#         }
        
        $cmd = "dbjoin $db.wfdisc calibration | dbsubset - \"wfdisc.segtype !~ /calibration.segtype/\" | dbselect -s - \"wfdisc.segtype:=calibration.segtype\"  " ;

        $success = run ( $cmd, 60 ) ;
        if ( $success ) {
            $problems++ ;
            &print_prob ( $problems, "FAILED: $cmd", $parent, *PROB ) ;
        }

#         ( $success, @output )  = &run_cmd( $cmd, 60 ) ;
# 
#         if ( ! $success ) {
#             $problems++ ;
#             &print_prob ( $problems, "FAILED: $cmd", $parent, *PROB ) ;
#         }

    }  else {
        fork_notify( $parent, "	no records in calibration for dbjoin wfdisc calibration") if ($opt_v);
    }
    
    dbclose ( @db ) ;
    
    return($problems);
}



