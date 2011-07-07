#
#   program needs:
#      miniseed2db on month files
#      only process month files with one complete month before current month.
#      only process day files to match last month.
#      fill gaps on day files, mark in gap table, "y|n|p"
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
    my (  @keys, @procs, @stas, @the_rest );
    my (  %errors, %logs, %stas );


    $pgm = $0 ; 
    $pgm =~ s".*/"" ;
    elog_init($pgm, @ARGV);
    $cmd = "\n$0 @ARGV" ;
    
    if (  ! getopts('vVnf:m:p:s:') || ( @ARGV != 1 && @ARGV != 9 && @ARGV != 10 ) ) { 
        $usage  =  "\n\n\nUsage: $0  [-v] [-V] [-n] " ;
        $usage .=  "[-p pf] [-m mail_to] [-f nforks] [-s sta_regex] db  \n\n" ;
        $usage .=  "Usage: $0  [-v] [-V] [-n] " ;
        $usage .=  "[-p pf]  " ;
        $usage .=  "sta wf_start_yearday wf_end_yearday soh_start_YYYYMM soh_end_YYYYMM balerperf_start_yearday balerperf_end_yearday allperf_start_yearday allperf_end_yearday \n\n"  ;         
        $usage .=  "Usage: $0  [-v] [-V] [-n] " ;
        $usage .=  "[-p pf]  " ;
        $usage .=  "sta wf_start_yearday wf_end_yearday soh_start_YYYYMM soh_end_YYYYMM balerperf_start_yearday balerperf_end_yearday allperf_start_yearday allperf_end_yearday parent_pid \n\n"  ;         
        elog_notify($cmd) ; 
        elog_die ( $usage ) ; 
    }
    
    
    $Pf         = $opt_p || $pgm ;
    
    if ( @ARGV > 1 && $opt_v ) {
        $opt_v = 0 ;
        %pf = getparam( $Pf );
        $opt_v = 1 ;
    } elsif ( @ARGV > 1 )  {
        %pf = getparam( $Pf );
    }
    
    $opt_v      = defined($opt_V) ? $opt_V : $opt_v ;
    
    chop ($host = `uname -n` ) ;
        
    STDOUT->autoflush(1) ;
    
    $parent   = $$ ;
    $problems = 0;

    if ( @ARGV == 9 ) {
        &proc_sta( $ARGV[0], $ARGV[1], $ARGV[2], $ARGV[3], $ARGV[4], $ARGV[5], $ARGV[6], $ARGV[7], $ARGV[8], $parent ) ;
        exit 0 ;
    }

    if ( @ARGV == 10 ) {
        $parent = $ARGV[9] ;
        &proc_sta( $ARGV[0], $ARGV[1], $ARGV[2], $ARGV[3], $ARGV[4], $ARGV[5], $ARGV[6], $ARGV[7], $ARGV[8], $parent ) ;
        exit 0 ;
    }

    &savemail() if $opt_m ; 
    
    announce( 0, 0 )  ;
    
    elog_notify($cmd) ; 
    
    $stime = strydtime(now());
    elog_notify ("\nstarting execution on	$host	$stime\n\n");

    $db = $ARGV[0] ;
    
    %pf = getparam( $Pf );
    
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
    elog_notify ( "@stas" ) ;
    
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

            $cmd   =  "baler_wf_proc " ;
            $cmd  .=  "-n " if $opt_n;
            $cmd  .=  "-v " if $opt_v;
            $cmd  .=  "-V " if $opt_V;
            $cmd  .=  "-p $opt_p " if $opt_p;
            $cmd  .=  "$sta " ;
            $cmd  .=  "$stas{$sta}{wf}{start_proc} $stas{$sta}{wf}{end_proc} " ;
            $cmd  .=  "$stas{$sta}{soh}{start_proc} $stas{$sta}{soh}{end_proc} " ;
            $cmd  .=  "$stas{$sta}{baler}{start_proc} $stas{$sta}{baler}{end_proc} " ;
            $cmd  .=  "$stas{$sta}{all}{start_proc} $stas{$sta}{all}{end_proc} " ;
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
        elog_debug("Wait for:[".@procs."] => @procs") if $opt_V;
        &nonblock_read( \%stas, \%logs, \%errors );

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
    my ( $baler_db, $endtime, $endtime_null, $equip_install, $key1, $key2, $nrows, $row, $rt_db, $sta, $string, $subject, $time ) ;
    my (  @db, @dbdeploy, @dbnull, @mseed_dirs ) ;
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
        $stas{$sta}{wf}{start}         = yearday  ( $time ) ;
        $stas{$sta}{wf}{start_proc}    = yearday  ( $equip_install ) ;
        $stas{$sta}{soh}{start_proc}   = yearmonth( $equip_install ) ;
        $stas{$sta}{baler}{start_proc} = $stas{$sta}{wf}{start_proc} ;
        $stas{$sta}{all}{start_proc}   = $stas{$sta}{wf}{start_proc} ;
        
        if ( exists $stas{$sta}{wf}{endtime} ) {
            if ( ( $endtime != $endtime_null ) && ( $endtime > $stas{$sta}{endtime} )) {
                $stas{$sta}{wf}{endtime}   = $endtime ;
                $stas{$sta}{wf}{end_proc}  = yearday ( $endtime ) ;
                $stas{$sta}{soh}{end_proc} = yearmonth( $endtime ) ;
                $stas{$sta}{baler}{end_proc} = $stas{$sta}{wf}{end_proc} ;
                $stas{$sta}{all}{end_proc}   = $stas{$sta}{wf}{end_proc} ;
            } elsif ( exists $stas{$sta}{wf}{endtime} ) {
                delete $stas{$sta}{wf}{endtime} ;
                delete $stas{$sta}{wf}{end_proc} ;
                delete $stas{$sta}{soh}{end_proc} ;
                delete $stas{$sta}{baler}{end_proc} ;
                delete $stas{$sta}{all}{end_proc} ;
            }
        } elsif ( $endtime != $endtime_null ) {
            $stas{$sta}{wf}{endtime}     = $endtime ;
            $stas{$sta}{wf}{end_proc}    = yearday ( $endtime ) ;
            $stas{$sta}{soh}{end_proc}   = yearmonth( $endtime )  ;
            $stas{$sta}{baler}{end_proc} = $stas{$sta}{wf}{end_proc} ;
            $stas{$sta}{all}{end_proc}   = $stas{$sta}{wf}{end_proc} ;
        }
    }
     
    dbclose( @db ) ;
#
#  only use stations with baler directories and rt wfdiscs
#
    prettyprint( \%stas ) if $opt_V ;
    elog_debug( " " ) if $opt_V ;
    
    foreach $sta (sort keys %stas) {
        $rt_db             = "$pf{rt_sta_dir}/$sta/$sta" ;
        
        if ( -e "$pf{baler_final}/$sta" ) {
            elog_notify ( "$sta	- Baler data in directory $pf{baler_final}/$sta;	Completed station!" ) if $opt_V ;
            delete $stas{$sta} ;
            next ;
        }
        
        if ( ! -d "$pf{baler_active}/$sta" ) {
            elog_notify ( "$sta	- No baler data directory $pf{baler_active}/$sta;	Skipping station!" ) if $opt_v ;
            delete $stas{$sta} ;
            next ;
        }
        
        opendir( DIR, "$pf{baler_active}/$sta" ) ;
        @mseed_dirs = sort ( grep { /^20[0-9][0-9]/  } readdir(DIR) ) ;
        closedir( DIR ) ;
                
        if ($#mseed_dirs == -1 ) {
            $string = "	$sta has no miniseed data in $pf{baler_active}/$sta !" ; 
            elog_notify ( "$sta - $string" ) ;
            delete $stas{$sta} ;
            next ;
        } elsif ( $opt_V ) {
            $string = "	$sta has data in @mseed_dirs !" ; 
            elog_notify ( "$sta - $string" ) ;
        }
        
        if ( ! -f $rt_db ) {
            elog_notify ( "$sta	- No real time data db $rt_db;	Skipping station!" ) ;
            delete $stas{$sta} ;
            next ;
        }
    }
#
#  refine processing times defined in %stas
#
    prettyprint( \%stas )  if $opt_V ;
    elog_notify( "\n\n\n")  if $opt_V ;
    
    foreach $sta (sort keys %stas) {
        $rt_db             = "$pf{rt_sta_dir}/$sta/$sta" ;
        $baler_db          = "$pf{baler_active}/$sta/$sta" ;
        elog_debug ( "$baler_db	$rt_db" ) if $opt_V ;
#
#  find if sta has some processing completed
#
        
        &last_proc_data ( $sta, \%stas ) ;

#
#  define start processing 
#
        
        &start_proc_data ( $sta, \%stas ) ;
        
#
#  define end processing 
#
        
        &end_proc_data ( $sta, \%stas ) ;

#
#  remove noop stas
#
        
        &noop_stas ( $sta, \%stas ) ;

#
#  this step put in because delete is not behaving as expected.
#
        if ( defined $stas{$sta} ) {
            elog_debug( sprintf( "keys    -    %s", join ( " " , sort ( keys %{$stas{$sta}} ) ) ) ) if $opt_V ;
            foreach $key1 ( keys %{$stas{$sta}} ) {
                foreach $key2 ( keys %{$stas{$sta}{$key1}} ) {
                    $stas_out{$sta}{$key1}{$key2} = $stas{$sta}{$key1}{$key2} ;
                }
            }
        }
    }
    
    elog_debug ( "$sta	- get_stas just before return" ) if $opt_V ;
        
    prettyprint( \%stas_out ) if $opt_V ;
                        
    return ( $problems, %stas_out ) ;
}

sub last_proc_data {  # &last_proc_data ( $sta, \%stas ) ;
    my ( $sta, $ref ) = @_ ;
    my ( $baler_all_db, $baler_db, $baler_perf_db, $jdate, $yearmonth ) ; 
    my ( %stas ) ;
    %stas = %$ref ;

    $baler_db          = "$pf{baler_active}/$sta/$sta" ;
    $baler_perf_db     = "$pf{baler_active}/$sta/$sta\_baler" ;
    $baler_all_db      = "$pf{baler_active}/$sta/$sta\_all" ;
    elog_debug ( "last_proc_data	$sta" ) if $opt_V ;
        
#
#  find if sta has some processing completed
#
    if ( -f $baler_db  && -f "$baler_db.wfdisc" ) {
#
#  find last jdate processed
#
        $jdate                        = &db_jdate ( $baler_db ) ;
        $stas{$sta}{wf}{last_proc}    = $jdate if ( $jdate ) ;
#
#  find last month processed
#
        $yearmonth                    = &db_month ( $baler_db ) ;
        $stas{$sta}{soh}{last_proc}   = $yearmonth if ( $yearmonth ) ;
            
#
#  find last baler_perf processed
#
        $jdate                        = &db_perf ( $baler_perf_db ) ;
        $stas{$sta}{baler}{last_proc} = $jdate if ( $jdate ) ;
#
#  find last all_perf processed
#
        $jdate                        = &db_perf ( $baler_all_db ) ;
        $stas{$sta}{all}{last_proc}   = $jdate if ( $jdate ) ;
        
    }       

    return ;
}

sub start_proc_data {  # &next_proc_data ( $sta, \%stas ) ;
    my ( $sta, $ref ) = @_ ;
    my ( %stas ) ;
    %stas = %$ref ;

    if ( exists $stas{$sta}{wf}{last_proc} ) {
        $stas{$sta}{wf}{start_proc}    = next_jdate ( $stas{$sta}{wf}{last_proc} ) ;
    } 

    if ( exists $stas{$sta}{soh}{last_proc} ) {
        $stas{$sta}{soh}{start_proc}    = next_month ( $stas{$sta}{soh}{last_proc} ) ;
    }

    if ( exists $stas{$sta}{baler}{last_proc} ) {
        $stas{$sta}{baler}{start_proc} = next_jdate ( $stas{$sta}{baler}{last_proc} ) ;
    }

    if ( exists $stas{$sta}{all}{last_proc} ) {
        $stas{$sta}{all}{start_proc}   = next_jdate ( $stas{$sta}{all}{last_proc} ) ;
    }

    return ;
}

sub end_proc_data {  # &next_proc_data ( $sta, \%stas ) ;
    my ( $sta, $ref ) = @_ ;
    my ( $closed, $end_jdate ) ; 
    my ( %stas ) ;
    %stas = %$ref ;
    
    $closed = 0 ;
    
    $closed = 1 if ( defined $stas{$sta}{wf}{endtime} )  ;
    
    if ( defined $stas{$sta}{wf}{last_proc} ) {
        $end_jdate = &find_end_proc ( $sta, $stas{$sta}{wf}{last_proc}, $closed ) ;
        $stas{$sta}{wf}{end_proc}    = $end_jdate ;
        $stas{$sta}{baler}{end_proc} = $end_jdate ;
        $stas{$sta}{all}{end_proc}   = $end_jdate ;
	} else {
        $end_jdate = &find_end_proc ( $sta, $stas{$sta}{wf}{start_proc}, $closed ) ;
        $stas{$sta}{wf}{end_proc}    = $end_jdate ;
        $stas{$sta}{baler}{end_proc} = $end_jdate ;
        $stas{$sta}{all}{end_proc}   = $end_jdate ;
	}
	
    if ( defined $stas{$sta}{soh}{last_proc} ) {
        $stas{$sta}{soh}{end_proc} = &find_end_month ( $sta, $stas{$sta}{soh}{last_proc}, $closed ) ;
	} else {
        $stas{$sta}{soh}{end_proc} = &find_end_month ( $sta, $stas{$sta}{soh}{start_proc}, $closed ) ;
	}

    return ;
}

sub noop_stas {  # &noop_stas ( $sta, \%stas ) ;
    my ( $sta, $ref ) = @_ ;
    my ( @keys ) ;
    my ( %stas ) ;
    %stas = %$ref ;
        
    elog_debug ( "noop_stas	$sta " ) if $opt_V ;
    
    @keys = sort ( keys %{$stas{$sta}} ) ;
            
#     if ( $stas{$sta}{wf}{last_proc}     == $stas{$sta}{wf}{end_proc} )  {
#         delete $stas{$sta}{wf} ;
#     }
#     if ( $stas{$sta}{soh}{last_proc}    == $stas{$sta}{soh}{end_proc} ) {
#         delete $stas{$sta}{soh} ;
#     } 
#     if ( $stas{$sta}{baler}{last_proc}  == $stas{$sta}{baler}{end_proc} ) {
#         delete $stas{$sta}{baler} ;
#     } 
#     if ( $stas{$sta}{all}{last_proc}    == $stas{$sta}{all}{end_proc} )  {
#         delete $stas{$sta}{all} ;
# 	}

    if ( ( $stas{$sta}{wf}{last_proc}     == $stas{$sta}{wf}{end_proc}    )  &&
         ( $stas{$sta}{soh}{last_proc}    == $stas{$sta}{soh}{end_proc}   )  &&
         ( $stas{$sta}{baler}{last_proc}  == $stas{$sta}{baler}{end_proc} )  &&
         ( $stas{$sta}{all}{last_proc}    == $stas{$sta}{all}{end_proc}   ) )  {
        delete $stas{$sta}{all} ;
        delete $stas{$sta}{baler} ;
        delete $stas{$sta}{soh} ;
        delete $stas{$sta}{wf} ;
	}

    @keys = sort ( keys %{$stas{$sta}} ) ;
        
    delete $stas{$sta} if ( $#keys == -1 ) ; 
			
    return ;
}

sub db_jdate {  # $last_jdate = &db_jdate ( $db ) ;
    my ( $db ) = @_ ;
    my ( $lastdir, $last_jdate ) ;
    my ( @db, @tree ) ;
    
    elog_debug ( "db_jdate	$db" )  if $opt_V ;
    
    @db    = dbopen   ( $db, "r" ) ;
    @db    = dblookup ( @db, 0, "wfdisc", 0, 0) ;
    @db    = dbsort   ( @db, qw ( -u -r jdate dir ) ) ;
    $db[3] = 0 ;
    
    $lastdir     = dbgetv ( @db, qw ( dir ) ) ;
    
    @tree = split ( "/", $lastdir  ) ;
    
    $last_jdate  = $tree[$#tree - 1] * 1000 + $tree[ $#tree ] ;

    elog_debug ( "lastdir	$lastdir	last_jdate	$last_jdate" )  if $opt_V ;

    dbclose ( @db ) ;
    
    return $last_jdate ;
}

sub db_month {  # $last_month = &db_jdate ( $db ) ;
    my ( $db ) = @_ ;
    my ( $lastdir, $last_month ) ;
    my ( @db, @tree ) ;
    
    elog_debug ( "db_month	$db" )  if $opt_V ;
    
    @db    = dbopen   ( $db, "r" ) ;
    @db    = dblookup ( @db, 0, "wfdisc", 0, 0) ;
    @db    = dbsubset ( @db, "dir =~ /.*month_files.*/ " ) ;
    
    if ( dbquery( @db, 'dbRECORD_COUNT' ) == 0 ) {
        elog_complain( "$db has no month_files processed" ) ; 
        return ( 0 ) ; 
    }
    @db    = dbsort   ( @db, qw ( -u -r dir ) ) ;
    $db[3] = 0 ;
    
    $lastdir = dbgetv ( @db, qw ( dir ) ) ;
    
    @tree    = split ( "/", $lastdir  ) ;
    
    $last_month  = $tree[$#tree - 2] * 100 + $tree[ $#tree ] ;

    elog_debug ( "lastdir	$lastdir	last_month	$last_month" )  if $opt_V ;
    
    dbclose ( @db ) ;
    
    return ( $last_month ) ;
}

sub db_perf {  # $jdate = &db_perf ( $db ) ;
    my ( $db ) = @_ ;
    my ( $jdate ) ;
    my ( @db ) ;
    
    elog_debug ( "db_baler_perf	$db" )  if $opt_V ;
    
    @db    = dbopen   ( $db, "r" ) ;
    @db    = dblookup ( @db, 0, "netperf", 0, 0) ;

    if ( dbquery( @db, 'dbRECORD_COUNT' ) == 0 ) {
        elog_complain( "$db has no month_files processed" ) ; 
        return ( 0 ) ; 
    }
    
    @db    = dbsort   ( @db, qw ( -u -r time ) ) ;
    $db[3] = 0 ;
    
    $jdate = yearday ( dbgetv ( @db, qw ( time ) ) ) ;
        
    dbclose ( @db ) ;
    
    return ( $jdate ) ;
}

sub yearmonth {  # $yearmonth = &yearmonth ( $epoch ) ;
    my ( $epoch ) = @_ ;
    my ( $yearmonth ) ;
    
    $yearmonth = epoch2str ( $epoch, "%Y%m" ) ;

    return ( $yearmonth ) ;
}

sub jdate_to_dir {  # $dir = &jdate_to_dir ( $sta, $jdate ) ;
    my ( $sta, $jdate ) = @_ ;
    my ( $dir ) ;
    
    $dir = epoch2str ( epoch($jdate), "$pf{baler_active}/$sta/%Y/%j" ) ;
    return $dir ;
}

sub month_to_dir {  # $dir = &month_to_dir ( $sta, $yearmonth ) ;
    my ( $sta, $yearmonth ) = @_ ;
    my ( $dir, $month, $string, $time, $year ) ;
    
    $month  = $yearmonth % 100 ;
    $year   = int( $yearmonth / 100 ) ;
    $string = sprintf( "%4d-%2.2d-01", $year, $month ) ;
    $time  = str2epoch ( $string ) ;
    $dir = epoch2str ( $time, "$pf{baler_active}/$sta/%Y/month_files/%m" ) ;
    return $dir ;
}

sub prev_month {  # $yearmonth = &prev_month ( $yearmonth ) ;
    my ( $yearmonth ) = @_ ;
    my ( $month, $time, $year ) ;
    
    $month = $yearmonth % 100 ;
    $year  = int( $yearmonth / 100 ) ;
    if ( $month == 1 ) {
        $month = 12 ;
        $year-- ;
    } else {
        $month-- ;
    }
    
    $yearmonth = ( $year * 100 ) + $month ;
    return $yearmonth ;
}

sub next_month {  # $yearmonth = &next_month ( $yearmonth ) ;
    my ( $yearmonth ) = @_ ;
    my ( $month, $time, $year ) ;
    
    $month = $yearmonth % 100 ;
    $year  = int( $yearmonth / 100 ) ;
    if ( $month == 12 ) {
        $month = 1 ;
        $year++ ;
    } else {
        $month++ ;
    }
    
    $yearmonth = ( $year * 100 ) + $month ;
    return $yearmonth ;
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
    
        $jdate = 1 ; 
        
    }
    return ( $jdate ) ;
}

sub exist_month {  # $yearmonth = &exist_month ( $dir, $sta, $parent ) ;
    my ( $dir, $sta, $parent ) = @_ ;
    my ( $atime, $blksize, $blocks, $ctime, $dev, $file, $gid, $ino, $jyearmonth_file, $mode ) ;
    my ( $mseed, $mtime, $nlink, $rdev, $size, $string, $uid, $yearmonth, $yearmonth_file ) ;
    my ( @mseedfiles ) ;
    
    $yearmonth = 0 ;
    
    opendir( DIR, $dir ) ;
    @mseedfiles = sort ( grep { /.*$sta.*/  } readdir(DIR) ) ;
    closedir( DIR );
        
    if ($#mseedfiles == -1 ) {

        $string = "	$sta has no files in $dir !" ; 
        fork_debug ( $parent, "$sta - $string" ) if $opt_V ;
        
    } else {
        
        $yearmonth = 1 ;

    }
    return ( $yearmonth ) ;
}

sub find_end_proc { # $proc_end = &find_end_proc( $sta, $last_proc, $closed ) ;
    my ( $sta, $last_proc, $closed ) = @_ ;
    my ( $dir, $jdate, $lag, $nday, $ndays, $proc_end ) ;
    
    $jdate     = yearday( now() ) ;
    $proc_end  = yearday( now() ) ;
        
    unless ( $closed ) { 
        for ($nday = 0; $nday< $pf{days_delay}; $nday++) {
            $proc_end = &prev_jdate ( $proc_end ) ;
        }
    }
    
    $lag       = 0 ;
            
    while ( $jdate > $last_proc ) {
        $dir        = &jdate_to_dir ( $sta, $jdate ) ;
        if ( &exist_data ( $dir, $sta, $$, 0 ) ) {
            $lag++ ;
            return ( $jdate ) if ( $closed ) ;
        }
        elog_debug ( "$dir	$sta	$jdate	$lag	$proc_end" ) if $opt_V ;
        if ( $lag > 2 && $jdate <= $proc_end ) {
            return ( $jdate ) ;
        }
        $jdate      = &prev_jdate ( $jdate ) ;
    }
    
    if ( $jdate == $last_proc ) {
        return ( $last_proc ) ;
    }
    return 0 ;
}

sub find_end_month { # $end_month = &find_end_month( $sta, $last_month, $closed ) ;
    my ( $sta, $last_month, $closed ) = @_ ;
    my ( $dir, $lag, $proc_end, $yearmonth ) ;
    
    $yearmonth = yearmonth( now() ) ;
    $proc_end  = yearmonth( now() ) ;
    
    $proc_end  = &prev_month ( &prev_month ( $proc_end ) ) unless ( $closed ) ;  #  Skip current month and previous month
        
    $lag       = 0 ;

    while ( $yearmonth > $last_month ) {
        $dir        = &month_to_dir ( $sta, $yearmonth ) ;
        if ( &exist_month ( $dir, $sta, $$ )  ) {
            $lag++ ;
            return ( $yearmonth ) if ( $closed ) ; 
        }
        if ( $lag > 2 && $yearmonth <= $proc_end ) {
            return ( $yearmonth ) ;
        }
        $yearmonth = &prev_month ( $yearmonth ) ;
    }
    
    if ( $yearmonth == $last_month ) {
        return ( $last_month ) ;
    }
    return 0 ;
}

sub proc_sta { # &proc_sta( $sta, $wf_start, $wf_end, $soh_start, $soh_end, $bperf_start, $bperf_end, $aperf_start, $aperf_end, $parent ) ;
    my ( $sta, $wf_start, $wf_end, $soh_start, $soh_end, $bperf_start, $bperf_end, $aperf_start, $aperf_end, $parent ) = @_ ;
    my ( $cmd, $dir, $etime, $jdate, $prob, $prob_check, $baler_active, $stime, $string, $subject, $success, $yearmonth ) ;
    my ( @output ) ;

    $stime = strydtime( now() );
    
    $string = "starting processing station $sta    $wf_start    $wf_end    $stime" ,
    elog_notify ( $string );
    fork_notify ( $parent, $string );
    
    open( PROB,"> /tmp/prob_$sta\_$$");
    
    print PROB "$string \n\n" ;

    $prob_check = $prob = 0;
        
    $baler_active = "$pf{baler_active}/$sta" ;
    
    chdir( $baler_active ) ;
    fork_notify ( $parent, "Changed directory to $baler_active " ) if $opt_v ;
    fork_notify ( $parent, "starting miniseed2db processing " ) if $opt_v ;

    &cssdescriptor ($sta,$pf{dbpath},$pf{dblocks},$pf{dbidserver}) unless $opt_n;
    
    open( TR, ">trdefaults.pf" );
    print TR "miniseed_segment_seconds 0\n";
    close( TR );

    $jdate = $wf_start ;
    
    while ( $jdate <= $wf_end ) {
        $dir = &jdate_to_dir ( $sta, $jdate ) ;

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
        
    $yearmonth = $soh_start ;
    

    while ( $yearmonth <= $soh_end ) {
        $dir = &month_to_dir ( $sta, $yearmonth ) ;

        if ( &exist_month ( $dir, $sta, $parent ) ) {

            $cmd  = "miniseed2db -T 0.001 $dir/\*$sta\* $sta ";

            ( $success, @output )  = &run_cmd( $cmd ) ;

            if ( ! $success ) {
                $prob++ ;
                &print_prob ( $prob, "FAILED: $cmd", $parent, *PROB ) ;
                last ;
            }

        }
        $yearmonth = &next_month ( $yearmonth ) ;
    }
        
    unlink("trdefaults.pf");
    
    $prob = &schanloc( $sta, $parent, $prob ) unless $opt_n ;

    unless ( $prob ) {
        fork_notify ( $parent, "starting baler rt_daily_return processing" ) if $opt_v ;
        $stime = $bperf_start ;
        $etime = yearday ( epoch( $bperf_end ) + 86400 )  ;
        $cmd  = "rt_daily_return ";
        $cmd .= "-t \"$stime\" -e \"$etime\" ";
        $cmd .= "-s \"sta =~/$sta/ && chan=~/[BL]H./\" $sta $sta\_baler";
        
        fork_notify ( $parent, $cmd ) if $opt_v ;

        ( $success, @output )  = &run_cmd( $cmd ) ;

        if ( ! $success ) {
            $prob++ ;
            &print_prob ( $prob, "FAILED: $cmd", $parent, *PROB ) ;
        }

        fork_notify ( $parent, "starting fill_gaps processing" ) if $opt_v ;
        $prob = fill_gaps ( $sta, $wf_start, $wf_end, $parent, $prob ) ;

        fork_notify ( $parent, "starting all rt_daily_return processing" ) if $opt_v ;
        $stime = $aperf_start ;
        $etime = yearday ( epoch( $aperf_end ) + 86400 )  ;
        $cmd  = "rt_daily_return ";
        $cmd .= "-t \"$stime\" -e \"$etime\" ";
        $cmd .= "-s \"sta =~/$sta/ && chan=~/[BL]H./\" $sta $sta\_all";
        
        fork_notify ( $parent, $cmd ) if $opt_v ;

        ( $success, @output )  = &run_cmd( $cmd ) ;

        if ( ! $success ) {
            $prob++ ;
            &print_prob ( $prob, "FAILED: $cmd", $parent, *PROB ) ;
        }
    }
    
    $cmd  = "dbfixchanids $sta ";
    fork_notify ( $parent, $cmd ) if $opt_v ;

    ( $success, @output )  = &run_cmd( $cmd ) ;

    if ( ! $success ) {
        $prob++ ;
        &print_prob ( $prob, "FAILED: $cmd", $parent, *PROB ) ;
    }

    $prob = &fix_wfdisc_calib( $sta, $parent, $prob ) ;

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

sub fix_wfdisc_calib {  # $problems = &fix_wfdisc_calib( $db, $parent, $problems ) ;
# 
#  sets calib, calper, segtype in wfdisc
#
    my ( $db, $parent, $problems ) = @_ ; 
    my ( $cmd, $failed, $nfail ) ;
    my ( @db, @dbcalibration, @output ) ; 

    fork_debug ( $parent, "fix_wfdisc_calib  db = $db   problems = $problems") if ( $opt_V ) ;
    
    @db              = dbopen  ( $db, "r+" ) ;
    @dbcalibration   = dblookup( @db, "", "calibration", "", "" ) ;
    

    if (dbquery ( @dbcalibration, "dbRECORD_COUNT" ) ) {
        
        $cmd = "dbjoin $db.wfdisc calibration | dbsubset - \"wfdisc.calib != calibration.calib\" | dbselect -s - \"wfdisc.calib:=calibration.calib\" " ;
        
        $nfail = 0 ;
        while ( $failed = run ( $cmd, 120 ) ) {
            $nfail++ ;
            $problems++ ;
            &print_prob ( $problems, "FAILED: #$nfail	$cmd", $parent, *PROB ) ; 
            last if ( $nfail > 3 ) ;
            sleep 300 ;
        }

        $cmd = "dbjoin $db.wfdisc calibration | dbsubset - \"wfdisc.calper != calibration.calper\" | dbselect -s - \"wfdisc.calper:=calibration.calper\"  " ;

        $nfail = 0 ;
        while ( $failed = run ( $cmd, 120 ) ) {
            $nfail++ ;
            $problems++ ;
            &print_prob ( $problems, "FAILED: #$nfail	$cmd", $parent, *PROB ) ; 
            last if ( $nfail > 3 ) ;
            sleep 300 ;
        }
       
        $cmd = "dbjoin $db.wfdisc calibration | dbsubset - \"wfdisc.segtype !~ /calibration.segtype/\" | dbselect -s - \"wfdisc.segtype:=calibration.segtype\"  " ;

        $nfail = 0 ;
        while ( $failed = run ( $cmd, 120 ) ) {
            $nfail++ ;
            $problems++ ;
            &print_prob ( $problems, "FAILED: #$nfail	$cmd", $parent, *PROB ) ; 
            last if ( $nfail > 3 ) ;
            sleep 300 ;
        }

    }  else {
        fork_notify( $parent, "	no records in calibration for dbjoin wfdisc calibration") if ($opt_v);
    }
    
    dbclose ( @db ) ;
    
    return( $problems );
}

sub schanloc {  # $problems = &schanloc( $db, $parent, $problems ) ;
# 
#  sets calib, calper, segtype in wfdisc
#
    my ( $db, $parent, $problems ) = @_ ; 
    my ( $nojoin ) ;
    my ( @db, @dbschanloc, @dbwfdisc, @missing ) ; 

    fork_debug ( $parent, "schanloc  db = $db   problems = $problems") if ( $opt_V ) ;
    
    @db           = dbopen  ( $db, "r" ) ;
    @dbwfdisc     = dblookup( @db, "", "wfdisc", "", "" ) ;
    fork_notify( $parent, sprintf("%d rows in wfdisc", dbquery( @dbwfdisc, "dbRECORD_COUNT" ) ) ) ;
    @dbwfdisc     = dbsubset( @dbwfdisc, "chan !~ /$pf{wfclean}/ " ) ;
    fork_notify( $parent, sprintf("%d rows in wfdisc after wfclean", dbquery( @dbwfdisc, "dbRECORD_COUNT" ) ) ) ;
    @dbschanloc   = dblookup( @db, "", "schanloc", "", "" ) ;
    @dbwfdisc     = dbnojoin( @dbwfdisc, @dbschanloc ) ;
    fork_notify( $parent, sprintf("%d rows in wfdisc after nojoin", dbquery( @dbwfdisc, "dbRECORD_COUNT" ) ) ) ;
    
    $nojoin = dbquery ( @dbwfdisc, "dbRECORD_COUNT" ) ;
    if ( $nojoin ) {
        @dbwfdisc = dbsort( @dbwfdisc, "-u", "chan" ) ;
        @missing = () ;
        for ( $dbwfdisc[3] = 0; $dbwfdisc[3] < $nojoin; $dbwfdisc[3]++ ) {
            push @missing, dbgetv( @dbwfdisc, "chan" ) ;
        }
        fork_complain( $parent, "$nojoin records in wfdisc do not join to schanloc - @missing") ;
        $problems++ ;
    }
    
    dbclose ( @db ) ;
    
    return( $problems );
}

sub fill_gaps {  # $prob = fill_gaps ( $sta, $proc_start, $proc_end, $parent, $prob ) ;
    my ( $sta, $proc_start, $proc_end, $parent, $prob ) = @_ ;
    my ( $chan, $cmd, $endtime, $gchan, $gsta, $nrows, $problem_check, $row, $string, $success, $tgap, $time ) ;
    my ( @db, @dbgap, @dbgwf, @dbscr, @dbwfchk, @output, @rows ) ;
    
    $time    = epoch( $proc_start ) ;
    $endtime = epoch( $proc_end ) + 86399.999 ;
    
    @db      = dbopen( "tmp_gap_$sta\_$$", "r+" ) ;
    @dbgwf   = dblookup( @db, 0, "wfdisc", 0, 0 ) ;
        
    @dbgap   = dbopen( "$sta\_baler", "r" ) ;
    @dbgap   = dblookup( @dbgap, 0, "gap", 0, 0 ) ;
    @dbgap   = dbsubset( @dbgap, "time >= $time && time < $endtime" ) ;
        
    @dbwfchk = dbopen( "$sta", "r" ) ;
    @dbwfchk = dblookup( @dbgap, 0, "wfdisc", 0, 0 ) ;
    @dbwfchk = dbsubset( @dbwfchk, "time < $endtime && endtime > $time" ) ;
    @dbscr   = dblookup( @dbwfchk, 0, 0, 0, "dbSCRATCH" ) ;
        
    $nrows   = dbquery( @dbgap, "dbRECORD_COUNT" ) ;
        
    $problem_check = $prob;

    for ( $row = 0; $row<$nrows; $row++ ) {
        $dbgap[3] = $row;
        ( $gsta, $gchan, $time, $tgap ) = dbgetv(@dbgap, "sta", "chan", "time", "tgap" );
        $endtime = $time + $tgap - 0.004;
        dbputv( @dbscr, "sta", $gsta, "chan", $gchan, "time", $time, "endtime", $endtime );
        @rows = dbmatches( @dbscr, @dbwfchk, "overlap_$sta", "sta", "chan", "time::endtime" );
        if ($#rows > -1) {
            $string = sprintf( "$chan	already has gap filled data between	%s	and	%s\n", strydtime( $time ), strydtime( $endtime ) ) ;
            fork_complain ( $parent, $string ) ;
            next;
        }
        dbaddv( @dbgwf,"sta",     $gsta,
                       "chan",    $gchan,
                       "time",    $time,
                       "endtime", $endtime ) ;
    }

#     dbclose ( @db ) ; 
#     dbclose ( @dbgap ) ;
#     dbclose ( @dbwfchk ) ;

#
#  Build rt station wfdisc (this is in a different wf naming format for ease of debugging)
#

    $cmd  = "trexcerpt ";
    $cmd  .= "-v  " if $opt_V;
    $cmd  .= "-a -D -E -m explicit -W $pf{rt_sta_dir}/$sta/$sta tmp_gap_$sta\_$$.wfdisc $sta " ;
               
    ( $success, @output )  = &run_cmd( $cmd ) ;

    if ( ! $success ) {
        $prob++ ;
        &print_prob ( $prob, "FAILED: $cmd", $parent, *PROB ) ;
    }
    
    
    unlink ( "tmp_gap_$sta\_$$.wfdisc" ) ; 
    unlink ( "tmp_gap_$sta\_$$.lastid" ) ; 
    
    return ( $prob ) ;

}

