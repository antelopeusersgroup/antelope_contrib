#
#   program needs:
#      only process month files with one complete month before current month.
#      only process day files to match last month.
#
#      test current changes then
#          copy snetsta and schanloc to /tmp at beginning of program
#          make links from sta_db and tmp_db to tmp snetsta schanloc
#          change cssdescriptor for tmpdb
#          cleanup /tmp
#          remove links to snetsta and schanloc for sta_db
#          remove dbpath ... from pf file
#
#      consider implementing retrys if consistent orb connection failures.
#
    use POSIX;    
    use strict ;
    use warnings ;
    use Datascope ;
    use orb ;
    use archive;
    use timeslice ;
    use timeutil ;
    use utilfunct ;
    use utility ;
    use sysinfo ; 
    use Getopt::Std ;
    use IO::Handle ;
    
    our ( $pgm, $host );
    our ( $opt_V, $opt_f, $opt_m, $opt_n, $opt_p, $opt_s, $opt_v );
    our ( %pf );
    
{    #  Main program

    my ( $Pf, $cmd, $db, $nchild, $nstas, $opt_c, $orb, $orbclient, $orbname, $orbsize, $prob, $problems ) ;
    my ( $sta, $stime, $subject, $usage ) ;
    my ( @stas ) ;
    my ( %stas ) ;


    $pgm = $0 ; 
    $pgm =~ s".*/"" ;
    elog_init($pgm, @ARGV);
    $cmd = "\n$0 @ARGV" ;
    
    if (  ! &getopts('vVnc:m:p:s:') || ( @ARGV != 2 ) ) { 
        $usage  =  "\n\n\nUsage: $0  [-v] [-V] [-n] " ;
        $usage .=  "[-p pf] [-m mail_to] [-c orbclient ] [-s sta_regex] db orb \n\n" ;
        elog_notify($cmd) ; 
        elog_die ( $usage ) ; 
    }
    
    &savemail() if $opt_m ; 
    elog_notify($cmd) ; 
    $stime = strydtime(now());
    chop ($host = `uname -n` ) ;
    
    
    elog_notify ("\nstarting execution on	$host	$stime\n\n");

    announce( 0, 0 )  ;
    
    $Pf         = $opt_p || $pgm ;
    
    %pf = getparam( $Pf );
        
    $opt_s  = $opt_s || $pf{day_of_week}{epoch2str( now(), "%A" )} ;
    
    elog_notify( "station subset is $opt_s" ) ;
    
    $opt_v      = defined( $opt_V ) ? $opt_V : $opt_v ;

    $db         = $ARGV[0] ;
    $orbname    = $ARGV[1] ;
    $orbclient  = $opt_c || "orbmsd2days" ;
            
    $problems = 0;

#
#  check orb
#
    $orb                    = orbopen( $orbname, "r+" );
    
    ( $orbsize, $problems ) = &orbcheck( $orb, $orbname, $orbclient, $problems );
    
    ( $problems )           = &orbprime( $orbname, $problems ) unless $opt_n;

    if ( $problems ) {
        elog_complain("\nProblem $problems
                       \n	Failed to prime orb $orbname ");
        $subject = "Problems - $pgm $host	" ;
        &sendmail( $subject, $opt_m ) if $opt_m ; 
        elog_die( "\n$subject" ) ;
    }
               
#        
#   subset for unprocessed data
#
    ( $problems, %stas ) = &get_stas( $db, $problems ) ;
        
    @stas = sort keys %stas ;
    
    $nstas = $#stas + 1 ;
    
    elog_notify ( "$nstas stations to process  " ) ;
    elog_notify ( "@stas\n\n" ) ;

    elog_debug ( "get_stas returned" ) if $opt_V ;
    prettyprint ( \%stas ) if $opt_V ;

#
#  process all stations
#
        
    $nchild = 0 ;
    foreach $sta ( @stas ) {
    
        $prob = 0 ;
        $prob = &proc_sta( $sta, $db, $orbname, $orb, $orbclient, $orbsize, \%stas, $Pf, $prob ) ;
        
        $problems += $prob ;
        
        $nchild++ if ( $prob ) ;
        

    }

#
# Finish up
#

    if ( -d $pf{tmp_dbmaster} ) {
        $cmd = "rm -rf $pf{tmp_dbmaster}" ;
        &run_cmd( $cmd )
    }

    $stime = strydtime(now());
    elog_notify ("completed 	$stime\n\n");

    if ( $problems == 0 ) {
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
    my ( $cmd, $endtime, $endtime_null, $equip_install, $key1, $key2, $nrows, $row, $sta, $subject, $time ) ;
    my ( @db, @dbdeploy, @dbnull ) ;
    my ( %stas, %stas_out ) ;
#
#  setup db
#
    @db           = dbopen   ( $db, "r" ) ;
    @dbdeploy     = dblookup ( @db, 0, "deployment", 0, 0) ;
    @dbnull       = dblookup ( @dbdeploy, 0, 0, 0, "dbNULL") ;
    $endtime_null = dbgetv   ( @dbnull, qw( endtime ) ) ;
    
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
        
        if ( exists ( $stas{$sta}{wf}{endtime} ) ) {
            no warnings 'uninitialized';
            if ( ( $endtime != $endtime_null ) && ( $endtime > $stas{$sta}{endtime} )) {
                $stas{$sta}{wf}{endtime}   = $endtime ;
                $stas{$sta}{wf}{end_proc}  = yearday ( $endtime ) ;
                $stas{$sta}{soh}{end_proc} = yearmonth( $endtime ) ;
            } else {
                delete $stas{$sta}{wf}{endtime} ;
                delete $stas{$sta}{wf}{end_proc} ;
                delete $stas{$sta}{soh}{end_proc} ;
            }
        } elsif ( $endtime != $endtime_null ) {
            $stas{$sta}{wf}{endtime}     = $endtime ;
            $stas{$sta}{wf}{end_proc}    = yearday ( $endtime ) ;
            $stas{$sta}{soh}{end_proc}   = yearmonth( $endtime )  ;
        }
    }
     
    dbclose( @db ) ;
#
#  only use stations with baler directories and rt wfdiscs
#
    prettyprint( \%stas ) if $opt_V ;

    foreach $sta (sort keys %stas) {
        
        if ( !-d "$pf{baler_active}/$sta" ) {
            elog_debug ( "$sta	- No baler data directory $pf{baler_active}/$sta;	Skipping station!" ) if $opt_V ;
            delete $stas{$sta} ;
            next ;
        }
        
        if ( !-f "$pf{baler_active}/$sta/$sta.wfdisc" ) {
            elog_debug ( "$sta	- No baler wfdisc $pf{baler_active}/$sta/$sta.wfdisc;	Skipping station!" ) if $opt_V ;
            delete $stas{$sta} ;
            next ;
        }
        
        if ( !-f "$pf{baler_active}/$sta/$sta\_all.netperf" ) {
            elog_debug ( "$sta	- No baler netperf $pf{baler_active}/$sta/$sta\_all.netperf;	Skipping station!" ) if $opt_V ;
            delete $stas{$sta} ;
            next ;
        }
        
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
    
    if ( -d $pf{tmp_dbmaster} ) {
        $cmd = "rm -rf $pf{tmp_dbmaster}" ;
        &run_cmd( $cmd )
    }
    
    makedir ( $pf{tmp_dbmaster} ) ;
    
    $cmd = "dbcp $db.schanloc $pf{tmp_dbmaster}/tmp_dbmaster" ;
        
    if ( ! &run_cmd( $cmd ) ) {
        elog_complain ( "FAILED:	$cmd" ) ;
        $subject = "Problems - $pgm $host	dbcp dbmaster" ;
        &sendmail($subject, $opt_m) if $opt_m ; 
        elog_die("\n$subject") ;
    }
        
    $cmd = "dbcp $db.snetsta $pf{tmp_dbmaster}/tmp_dbmaster" ;
        
    if ( ! &run_cmd( $cmd ) ) {
        elog_complain ( "FAILED:	$cmd" ) ;
        $subject = "Problems - $pgm $host	dbcp dbmaster" ;
        &sendmail($subject, $opt_m) if $opt_m ; 
        elog_die("\n$subject") ;
    }
        
    elog_debug ( "$sta	- get_stas just before return" ) if $opt_V ;
        
    prettyprint( \%stas_out ) if $opt_V ;
                        
    return ( $problems, %stas_out ) ;
}

sub orbcheck { # ( $orbsize, $problems ) = &orbcheck( $orb, $orbname, $orbclient, $problems ) ;
    my ( $orb, $orbname, $orbclient, $problems ) = @_ ;
    my ( $client, $found_oc, $found_sf, $found_xf, $orbsize, $orbstat, $subject, $when ) ;
    my ( @clients );
    
    if ( $orb < 0 and ! $opt_n) {
        $subject = "Problems - $pgm $host	Failed to open orb $orbname" ;
        &sendmail($subject, $opt_m) if $opt_m ; 
        elog_die("\n$subject") ;
    }
    
    $orbstat = orbstat( $orb ) ;
    $orbsize = $orbstat->maxdata ;
    elog_notify( "orbsize	$orbsize" ) if $opt_v ;
    
#
#  make sure other programs are not running
#    

    ( $when, @clients ) = orbclients( $orb ) ;
    
    $found_sf = 0 ;
    foreach $client ( @clients ) {
        if ( $client->what =~ /$pgm/ ) {
            elog_notify( sprintf "%-8s %s\n", $client->who, $client->what ) if $opt_v ;
            $found_sf++ ;
        }
        if ( $client->what =~ /miniseed2orb/ ) {
            elog_notify( sprintf "%-8s %s\n", $client->who, $client->what ) if $opt_v ;
            elog_complain("\n	A miniseed2orb is currently connected to orb $orbname
                       \n	Restart this instance of $pgm when miniseed2orb is completed") ;
            $subject = "Problems - $pgm $host	$problems problems" ;
            &sendmail( $subject, $opt_m ) if $opt_m ; 
            elog_die( "\n$subject" ) ;
        }
    }
    if ( $found_sf > 1 ) {
        elog_complain("\n	Another $pgm is currently connected to orb $orbname
                       \n	Restart this instance of $pgm when other instance is completed") ;
        $subject = "Problems - $pgm $host	$problems problems" ;
        &sendmail($subject, $opt_m) if $opt_m ; 
        elog_die("\n$subject") ;
    }

#
#  make sure orbminiseed2days and orbxfer2 are attached!!!!!! 
#    

    ( $when, @clients ) = orbclients( $orb ) ;
    
    $found_oc = 0;
    $found_xf = 0;
    foreach $client ( @clients ) {
        if ( $client->what =~ /$orbclient/ ) {
            elog_notify( sprintf "%-8s %s\n", $client->who, $client->what ) if $opt_v ;
            $found_oc = 1 ;
        }
        if ( $client->what =~ /orbxfer2/ ) {
            elog_notify( sprintf "%-8s %s\n", $client->who, $client->what ) if $opt_v ;
            $found_xf = 1 ;
        }
    }
    
    unless ( $found_oc ) {
        elog_complain( "$orbclient not currently connected to orb $orbname" ) ;
        $subject = "Problems - $pgm $host	$problems problems" ;
        &sendmail( $subject, $opt_m ) if $opt_m ; 
        elog_die( "\n$subject" ) ;
    }

    unless ( $found_xf ) {
        elog_complain( "orbxfer2 not currently connected to orb $orbname" ) ;
        $subject = "Problems - $pgm $host	$problems problems" ;
        &sendmail( $subject, $opt_m ) if $opt_m ; 
        elog_die("\n$subject") ;
    }
    return( $orbsize, $problems );
}

sub orbprime { # ( $problems ) = &orbprime( $orbname, $problems ) ;
    my ( $orbname, $problems ) = @_ ;
    my ( $cmd );
    
    unlink "/tmp/JUNK.mseed"      if ( -e "/tmp/JUNK.mseed" ) ;
    unlink "JUNK.lastid"          if ( -e "JUNK.lastid" ) ;
    unlink "JUNK.wfdisc"          if ( -e "JUNK.wfdisc" ) ;
    unlink "JUNK.pf"              if ( -e "JUNK.pf" ) ;
    unlink "miniseed2orb_JUNK.pf" if ( -e "miniseed2orb_JUNK.pf" ) ;
    
    $cmd = "trsignal -d sd -r 40 -s JUNK -w /tmp/JUNK.mseed JUNK";
        
    if ( ! &run_cmd( $cmd ) ) {
        $problems++ ;
    }
    
    unlink "JUNK.lastid" unless $opt_V ;
    unlink "JUNK.wfdisc" unless $opt_V ;
   
    open ( PF, ">JUNK.pf" ) ;
    print  PF "net     \&Tbl\{ \n" ;
    print  PF "	.*	XX \n" ;
    print  PF "\}\n" ;
    close( PF ) ;

    $cmd = "fix_miniseed -p JUNK /tmp/JUNK.mseed " ;
        
    if ( ! &run_cmd( $cmd ) ) {
        $problems++ ;
    }

    unlink "JUNK.pf" unless $opt_V ;
    
    $cmd = "pfcp miniseed2orb miniseed2orb_JUNK " ;
        
    if ( ! &run_cmd( $cmd ) ) {
        $problems++ ;
    }

    open(  MS, ">>miniseed2orb_JUNK.pf") ;
    print  MS  "wait_match \n" ;
    close( MS ) ;
            
    $cmd = "miniseed2orb -p miniseed2orb_JUNK -u /tmp/JUNK.mseed $orbname " ;
        
    if ( ! &run_cmd( $cmd ) ) {
        $problems++ ;
    }

    unlink "/tmp/JUNK.mseed" unless $opt_V ;
    unlink "miniseed2orb_JUNK.pf" unless $opt_V ;

    return( $problems ) ;
}

sub proc_sta { # $problems = &proc_sta( $sta, $db, $orbname, $orb, $orbclient, $orbsize, \%stas, $Pf, $problems ) ;
    my ( $sta, $db, $orbname, $orb, $orbclient, $orbsize, $ref, $Pf, $problems ) = @_ ;
    my ( $baler_active, $chan, $cmd, $comment, $days_after_removal, $dbsize, $endtime ) ;
    my ( $nrows, $prob, $prob_check, $problem_init, $start, $stime, $subject, $sync_dfile ) ;
    my ( $time, $tmp_db, $tmp_sync, $wf_endtime ) ;
    my ( @db, @dbreplayed, @dbtmp, @dbtwf, @dbwfdisc ) ;
    my ( %stas ) = %{$ref} ;

    $start = now() ;
    $stime = strydtime( $start ) ;
            
    elog_notify ( "\nstarting processing station $sta    $stime");
    
    $problem_init = $problems ;
    
    open( PROB,"> /tmp/prob_$sta\_$$");
    
    print PROB "$stime      starting processing \n\n" ;

    $prob_check = $prob = 0;
        
    $baler_active = "$pf{baler_active}/$sta" ;
    
    chdir( $baler_active ) ;
    elog_notify ( "Changed directory to $baler_active " ) if $opt_v ;

    ( $tmp_db, $comment, $dbsize, $prob ) = &build_tmp_db( $sta, $prob ) ;
       
    if ( $dbsize && ! $prob) {
#
#  Transfer wf data to DMC using obsip2orb
#
        $cmd  = "obsip2orb " ;
        $cmd  .= "-X $orbname $tmp_db" if ($orbsize >= $dbsize) ;
        $cmd  .= "-c $orbclient $orbname $tmp_db" if ($orbsize < $dbsize) ;
        
        elog_notify( $cmd ) ; 
         
        if ( ! &run_cmd( $cmd ) ) {
            elog_complain ( "FAILED:	$cmd" ) ;
            $subject = "Problems - $pgm $host	obsip2orb $sta" ;
            &sendmail($subject, $opt_m) if $opt_m ; 
            elog_die("\n$subject") ;
        }
    
#
#  Transfer non-wf data to DMC 
#
        $tmp_sync = proc_nonwf_seed( $sta, $tmp_db, $Pf, $dbsize, $orbname, $orbsize, $orbclient ) ;
#
#  Make sync file 
#  
        ( $sync_dfile, $problems ) = &sync_file ( $sta, $tmp_db, $tmp_sync, $problems ) ;
        
        elog_debug ( "output of sync_file  -	sync_dfile	\"$sync_dfile\" " ) ;
#
#  wait until orblag value become acceptable
#
        &wait_for_orb_to_empty ( $orb ) ;
#
#  Transfer DMC sync file
#
         if ( $problem_init == $problems ) {
             $problems = &send_sync ( $sta, $db, $comment, $orbname, $sync_dfile, $problems ) ;
         } else {
            $subject = "Problems - $pgm $host	sync_file	$sta" ;
            &sendmail($subject, $opt_m) if $opt_m ; 
            elog_die("\n$subject") ;
         }
#
#  update replayed table
#
        @db            = dbopen  (  $sta, "r+" ) ; 
        @dbreplayed    = dblookup(  @db, 0, "replayed", 0, 0 ) ;
        
        @dbwfdisc      = dblookup(  @db, 0, "wfdisc", 0, 0 ) ;
        $wf_endtime    = dbex_eval( @dbwfdisc, "max(endtime)" ) ;
         
        @dbtmp         = dbopen  ( $tmp_db, "r" ) ; 
        @dbtwf         = dblookup( @dbtmp, 0, "wfdisc", 0, 0 ) ;
    
        $nrows         = dbquery( @dbtwf, "dbRECORD_COUNT" ) ;
            
        if ( ! $opt_n && ! ( $problem_init != $problems ) ) {
            for ( $dbtwf[3] = 0; $dbtwf[3]<$nrows; $dbtwf[3]++ ) {
                $dbreplayed[3] = dbaddnull( @dbreplayed ) ;
                ( $sta, $chan, $time, $endtime )  = dbgetv ( @dbtwf, qw ( sta chan time endtime ) ) ;
                dbputv ( @dbreplayed, "sta", $sta, "chan", $chan, "time", $time, "endtime",  $endtime ) ;
            }
        }
    
        dbclose ( @db ) ;
        dbclose ( @dbtmp ) ;
        
        unlink ( "sync/tmp_sync" )                 unless $opt_V ;
        unlink ( "$tmp_db" )                       unless $opt_V ;
        unlink ( "$tmp_db.wfdisc" )                unless $opt_V ;
        
    } elsif ( $prob ) {
        $problems += $prob ;
    }

#
#  If station completed
#

    if ( exists $stas{$sta}{wf}{endtime}  && ! $prob ) {

        $days_after_removal  =  ( now() - $stas{$sta}{wf}{endtime} ) / 86400 ;
        
        if   ( $days_after_removal > $pf{days_after_removal} ||  ( (  $stas{$sta}{wf}{endtime} - $wf_endtime ) / 86400 ) < 1 ) {
            
            &check_deployment( $sta, $db ) ; 
            chdir( $pf{baler_active} ) ;
            
            $cmd = "mv $baler_active $pf{baler_final}" ;
                    
            if ( ! &run_cmd( $cmd ) ) {
                $problems++ ;
                print PROB "FAILED: $cmd \n\n" ;
            } else {
                $comment = "Station completed, moved to $pf{baler_final}" ;
                elog_notify ( $comment ) ;
                print PROB "$comment \n\n" ;
            }
        }

    }
    
#
#  clean up
#

    $comment = sprintf("Transmission time %s		Transfer rate	%d bytes/sec", strtdelta( now() - $start ), $dbsize / ( now() - $start ) ) ;
    elog_notify ( $comment ) ;
    print PROB "$comment \n\n" ;
        
    $stime = strydtime( now() );
    print PROB "$stime      end processing \n\n" ;
    close(PROB);
                 
    if  ( $prob ) {
        $subject = "TA $sta     $prob problems -  $pgm on $host";
        $cmd     = "rtmail -C -s '$subject' $pf{prob_mail} < /tmp/prob_$sta\_$$";
        elog_notify ( $cmd ) ;
        
        &run_cmd( $cmd ) ;

    } elsif ( $opt_v ) {
        $subject = "TA $sta transmission completed -  $pgm on $host";
        $cmd     = "rtmail -C -s '$subject' $pf{success_mail} < /tmp/prob_$sta\_$$";
        elog_notify ( $cmd ) ;
        
        &run_cmd( $cmd ) ;

    } 

    unlink "/tmp/prob_$sta\_$$" unless $opt_V;
    
    $stime = strydtime( now() );
    elog_notify (  "proc_sta complete for $sta with $prob problems    $stime") ;

    return ( $problems ) ;

}

sub build_tmp_db { # ( $tmpdb, $comment, $dbsize, $prob ) = &build_tmp_db( $sta, $prob ) ;
    my ( $sta, $prob ) = @_ ;
    my ( $chan, $comment, $datatype, $dbsize, $dfile, $dir, $endtime, $foff, $line, $max_dir ) ;
    my ( $max_month, $min_dir, $min_month, $nminutes, $nojoin, $nrows, $nsamp, $nwfs, $replay, $row ) ;
    my ( $samprate, $string, $time, $tmpdb ) ;
    my ( @db, @dbblh, @dbnj, @dbreplayed, @dbschan, @dbscr, @dbsize, @dbsoh, @dbtmp, @dbtwf, @dbwfdisc, @missing, @rows ) ;
    
    $tmpdb = "tmp_replay_$sta\_$$" ;
    
    elog_debug( "build_tmp_db $sta begins" ) if $opt_V ;

    $nminutes = 0 ;
    while ( &dblock ( $sta, ( 600 ) ) ) {
        if ( $nminutes > 60 ) {
            $string  = sprintf( "		$sta locked for more that 1 hour, cannot process now "  )  ;
            elog_complain ( $string  ) ;
            $prob++ ;
            return ( $prob ) ;
        }
        $string = "$sta is locked!  build_tmp_db now sleeping" ;
        elog_notify ( $string ) ;
        sleep 60 ; 
        $nminutes++ ;
    }
    
    link ( "$pf{tmp_dbmaster}/tmp_dbmaster.schanloc", "$sta.schanloc" ) ;
    
    @db           = dbopen  ( $sta, "r" ) ; 
    @dbwfdisc     = dblookup( @db, 0, "wfdisc", 0, 0 ) ;
    @dbwfdisc     = dbsubset( @dbwfdisc, "chan !~ /$pf{wfclean}/ " ) ;
    @dbreplayed   = dblookup( @db, 0, "replayed", 0, 0 ) ;
    @dbscr        = dblookup( @dbreplayed, 0, 0, 0, "dbSCRATCH" ) ;
    @dbschan      = dblookup( @db, 0, "schanloc", 0, 0 ) ;
#
# schanloc test
#    
    @dbnj    = dbnojoin( @dbwfdisc, @dbschan ) ;
    $nojoin  = dbquery ( @dbnj, "dbRECORD_COUNT" ) ;
    
    elog_notify( sprintf( "%d rows in wfdisc after nojoin", $nojoin ) ) ;
    
    if ( $nojoin ) {
        @dbnj = dbsort( @dbnj, "-u", "chan" ) ;
        @missing = () ;
        for ( $dbnj[3] = 0; $dbnj[3] < dbquery ( @dbnj, "dbRECORD_COUNT" ); $dbnj[3]++ ) {
            push @missing, dbgetv( @dbnj, "chan" ) ;
        }
        
        $line    = "$nojoin records in $sta wfdisc do not join to schanloc - @missing" ;
        print  PROB "$line \n\n" ;
        elog_complain( $line ) ;  
    
        $line    = "$sta - no data sent to orb" ;
        print  PROB "$line \n\n" ;
        elog_complain( $line ) ;  
    
        dbclose ( @db ) ;
        &dbunlock ( $sta ) ;

        $dbsize = 0 ;
        $prob++;

        return ( "", "", $dbsize, $prob ) ;
    }
#
# open tmp database
#
    @dbtmp        = dbopen  ( $tmpdb, "r+" ) ; 
    @dbtwf        = dblookup( @dbtmp, 0, "wfdisc", 0, 0 ) ;
    
#     $replay       = dbquery( @dbreplayed, "dbTABLE_PRESENT" ) ;
    
    if ( dbquery( @dbreplayed, "dbTABLE_PRESENT" ) )  {
        @dbwfdisc = dbnojoin ( @dbwfdisc, @dbreplayed, qw ( sta chan time endtime ) ) ;
    }
    
    $nwfs         = dbquery( @dbwfdisc, "dbRECORD_COUNT" ) ;
    
    elog_debug( "nwfs	$nwfs" ) if $opt_V ;
    elog_debug( "open	$tmpdb" ) if $opt_V ;
#
#  load tmp wfdisc with all non-replayed waveforms 
#
    for ( $dbwfdisc[3] = 0; $dbwfdisc[3] < $nwfs; $dbwfdisc[3]++ ) {

        ( $sta, $chan, $time, $endtime, $nsamp, $samprate, $datatype, $dir, $dfile, $foff )  = 
            dbgetv ( @dbwfdisc, qw ( sta chan time endtime nsamp samprate datatype dir dfile foff ) ) ;
            
        elog_debug ( sprintf ("%8d	%s	%s	%s	%s	%s	%s", $dbwfdisc[3], $sta, $chan, strydtime($time), strydtime($endtime), $dir, $dfile ) ) if $opt_V ;
 
        $dbtwf[3] = dbaddnull ( @dbtwf ) ;
        dbputv (  @dbtwf, "sta", $sta, 
                          "chan", $chan, 
                          "time", $time, 
                          "endtime", $endtime, 
                          "nsamp", $nsamp, 
                          "samprate", $samprate, 
                          "datatype", $datatype, 
                          "dir", $dir, 
                          "dfile", $dfile, 
                          "foff", $foff ) ;

    }
    
    elog_debug( "$tmpdb.wfdisc built" ) if $opt_V ;
    
    if ( ! dbquery( @dbtwf, "dbTABLE_PRESENT" ) ) {
        elog_notify ( "$sta has no new data to process " ) ;
        print PROB  "$sta has no new data to process\n" ;
        dbclose ( @db ) ;
        dbclose ( @dbtmp ) ;
        &dbunlock ( $sta ) ;
        return ( "", "", 0, $prob ) ;
    }
        
    &cssdescriptor ( $tmpdb, "$pf{tmp_dbmaster}/{tmp_dbmaster}", "none", "" ) ;

    elog_debug( "$tmpdb descriptor" ) if $opt_V ;
    
#  Find directory range

    @dbblh      = dbsubset ( @dbtwf, "chan =~ /[BL]H[ZNE]/" ) ;
    
    @dbblh      = dbsort   ( @dbblh, "dir", "-u" ) ; 
    $dbblh[3]   = 0 ;
    $min_dir    = dbgetv ( @dbblh, "dir" ) ;
    
    @dbblh      = dbsort   ( @dbblh, "dir", "-r", "-u" ) ; 
    $dbblh[3]   = 0 ;
    $max_dir    = dbgetv ( @dbblh, "dir" ) ;

    @dbsoh      = dbsubset ( @dbtwf, "dir =~ /.*month.*/" ) ;
    
    if  ( dbquery( @dbsoh, "dbRECORD_COUNT" ) ) {
    
        @dbsoh      = dbsort   ( @dbsoh, "dir", "-u" ) ; 
        $dbsoh[3]   = 0 ;
        $min_month  = dbgetv ( @dbsoh, "dir" ) ;
    
        @dbsoh      = dbsort   ( @dbsoh, "dir", "-r", "-u" ) ; 
        $dbsoh[3]   = 0 ;
        $max_month  = dbgetv ( @dbsoh, "dir" ) ; 
    
    } else {
        $min_month  = $max_month  = "" ;
    }

#  Check the aggregate size of data files

    @dbsize = dbsort( @dbtwf, "dir", "dfile", "-u" ) ;
    $nrows  = dbquery( @dbsize, "dbRECORD_COUNT" ) ;
            
    $dbsize = 0;
    for ($row = 0; $row<$nrows; $row++) {
        $dbsize[3] = $row ;
        $dbsize += -s dbextfile( @dbsize ) ;
    }
        
    $comment = "$sta active baler data sent to DMC" ;
    $comment .= "    $min_dir    $max_dir    $min_month    $max_month" ;
    $comment .= "    Total Bytes -    $dbsize" ;
    print  PROB "$comment \n" ;
    elog_notify( $comment ) ;        
        
    dbclose ( @db ) ;
    dbclose ( @dbtmp ) ;
    &dbunlock ( $sta ) ;
    unlink ( "$sta.schanloc" ) ;

    return ( $tmpdb, $comment, $dbsize, $prob ) ;
}

sub proc_nonwf_seed { # $tmp_sync = proc_nonwf_seed( $sta, $tmpdb, $Pf, $dbsize, $orbname, $orbsize, $orbclient ) ;
    my ( $sta, $tmpdb, $Pf, $dbsize, $orbname, $orbsize, $orbclient ) = @_ ;
    my ( $base, $chan, $cmd, $dir, $mseedfile, $net, $nrows, $ref, $st1, $st2, $st3, $subject, $suf, $tmp_sync ) ;
    my ( @chans, @dbnonwf, @dbtmp, @dbtwf, @dirs, @files, @line, @msd, @mseedfiles, @pffiles ) ;

#  find file directories for non-wf miniseed data

    @dbtmp    = dbopen  ( $tmpdb, "r+" ) ; 
    @dbtwf    = dblookup( @dbtmp, 0, "wfdisc", 0, 0 ) ;

    @dbnonwf  = dbsubset( @dbtwf,   "chan =~ /$pf{non_wf_chan_proxy}/" ) ;
    @dbnonwf  = dbsort  ( @dbnonwf, "dir", "-u" ) ; 

    @dirs = ();
    $nrows = dbquery(@dbnonwf,"dbRECORD_COUNT") ;
        
    for ( $dbnonwf[3] = 0; $dbnonwf[3]<$nrows; $dbnonwf[3]++ ) {
        ( $dir, $base, $suf ) = parsepath( dbextfile( @dbnonwf ) ) ;
        elog_notify("dir	$dir") if $opt_V ;
        push( @dirs, $dir ) ;
    }
    elog_notify( "dirs	@dirs" ) if $opt_V ;
    
    dbclose ( @dbtmp ) ;
        
#
#  Transfer non-waveform miniseed data to DMC 
#

    @files = ();
    $ref       = pfget( $Pf, "non_wf_chan" ) ;
    @chans     = @$ref ;
    $chan = "(" . join("|",@chans) . ")";

#
#  make sure wait_match specified properly in miniseed2orb.pf
#
    @pffiles = pffiles( "miniseed2orb" ) ;
    elog_notify( "pffiles	@pffiles" ) ;
    unlink( "miniseed2orb_sta_final.pf" ) if ( -e "miniseed2orb_sta_final.pf" ) ;
    $cmd = "cp $pffiles[0] miniseed2orb_sta_final.pf" ;
    system( $cmd );
                    
    if ( $dbsize > $orbsize ) {
        open(  MS, ">>miniseed2orb_sta_final.pf") ;
        print  MS  "wait_match $orbclient\n" ;
        close( MS) ;

    } else {
        open( MS, ">>miniseed2orb_sta_final.pf") ;
        print  MS "wait_match \n" ;
        close( MS ) ;
        elog_notify( "running in expert mode - no wait_match ") ;
    }

#        
#  Assumes that non-waveform miniseed files are in the same directory as $pf{non_wf_chan_proxy}
#  Get list of all non-waveform miniseed files
#
        
    foreach $dir ( @dirs ) {
        elog_notify("	dir	$dir") if $opt_V ;
        opendir( DIR, $dir ) ;
        @mseedfiles = sort( grep { /.*_$sta\_$chan.*/  } readdir(DIR) ) ;
        elog_notify("		chan	$chan	@mseedfiles") if $opt_V ;
        closedir( DIR ) ;
        foreach $mseedfile (@mseedfiles) {
            push(@files,"$dir\/$mseedfile") ;
        }
    }

#        
#  Process each non-waveform miniseed file
#

    makedir("sync") if (! -d "sync" && ! $opt_n);
    
    $tmp_sync = "sync/tmp_sync" ;
        
    open( SYNC, ">$tmp_sync" ) unless $opt_n ;
        
    foreach $mseedfile ( @files ) {
        
#
#  Get start and end times of mseed file
#
        $cmd = "msdd $mseedfile" ;
        elog_notify("	$cmd") if $opt_V ;
            
        open( MSD, "$cmd |") ;
        @msd = <MSD> ;
        close MSD;
            
        @msd = grep { /$sta/ } @msd ;
            
        elog_notify( "	$msd[1]" ) if $opt_V ;
        @line = split " ", $msd[1] ;
        $net = $line[0] ;
        $chan = $line[2] ;
        splice( @line, 0, 3 ) ;
        $#line -= 2 ;
        $st1 = epoch2str( str2epoch( join " ", @line ), "%Y,%j,%H:%M:%S.%u" ) ;
        elog_notify("	$st1 ") if $opt_V ;
            
        elog_notify("	$msd[$#msd] ") if $opt_V ;
        @line = split " ", $msd[$#msd] ;
        splice( @line, 0, 3 ) ;
        $#line -= 2;
        $st2 = epoch2str( str2epoch( join " ", @line ), "%Y,%j,%H:%M:%S.%u" );
        elog_notify( "	$st2" ) if $opt_V ;
            
        @msd = () ;       
            
#
#  Send data to export orb
#
        $cmd = "miniseed2orb -p miniseed2orb_sta_final -u $mseedfile $orbname";
            
        if ( ! &run_cmd( $cmd ) ) {
            elog_complain ( "FAILED:	$cmd" ) ;
            $subject = "Problems - $pgm $host	miniseed2orb $mseedfile" ;
            &sendmail($subject, $opt_m) if $opt_m ; 
            elog_die("\n$subject") ;
        }
            
        $st3 = epoch2str( now(), "%Y,%j" ) ;
        print SYNC "$net|$sta||$chan|$st1|$st2||||||||||$st3\n" unless $opt_n ;
            
    }
        
    close SYNC unless $opt_n ;

    unlink ( "miniseed2orb_sta_final.pf" )    unless $opt_V ;
    
    return ( $tmp_sync ) ;

}

sub sync_file { # ( $sync_dfile, $problems ) = &sync_file ( $sta, $tmp_db, $tmp_sync, $problems ) ;
    my ( $sta, $tmp_db, $tmp_sync, $problems ) = @_ ;
    my ( $cmd,  $subject, $sync_dfile, $yearday ) ;
#
#  Make DMC sync file
#
    $yearday = &yearday( now () ) ;
    
    makedir("sync") if (! -d "sync" && ! $opt_n);

    $sync_dfile = "$sta\_$yearday.sync";
        
    $cmd = "db2sync -h $tmp_db sync/$sync_dfile" ;

    if ( ! &run_cmd( $cmd ) ) {
        $problems++ ;
        elog_complain ( "FAILED: $cmd \n\n" ) ;
        print PROB "FAILED: $cmd \n\n" ;
    }
    sleep 1 ;
        
    if ( ! -e "sync/$sync_dfile" && ! $opt_n ) {
        elog_complain( "sync/$sync_dfile does not exist!" ) ;
        elog_complain( "$cmd	will fail!" ) ;
        $problems++ ;
        $subject = "Problems - $pgm $host	$problems problems" ;
        &sendmail( $subject, $opt_m ) if $opt_m ; 
        elog_die( "\n$subject" ) ;
    }
        
    $cmd = "cat  $tmp_sync >> sync/$sync_dfile" ;
        
    if ( ! &run_cmd( $cmd ) ) {
        $problems++ ;
        elog_complain ( "FAILED: $cmd \n\n" ) ;
        print PROB "FAILED: $cmd \n\n" ;
    }
    
    return ( $sync_dfile, $problems  ) ;
}

sub wait_for_orb_to_empty { # &wait_for_orb_to_empty ( $orb ) ;
    my ( $orb, $problems ) = @_ ;
    my ( $max, $mlag, $n, $nerror, $new, $old, $orbname, $pktid, $range, $subject, $thread, $what, $who ) ;
    my ( @laggards) ;

    if ( $orb < 0 and ! $opt_n ) {
        elog_complain( "\nProblem $problems
                            \n	Failed to open orb $orbname for orblag check" ) ;
        $subject = "Problems - $pgm $host	" ;
        &sendmail( $subject, $opt_m ) if $opt_m ; 
        elog_die( "\n$subject" ) ;
    }
    
    $mlag   = 1.0 ;
    $n      = 0 ;
    $nerror = 0 ;
    while ( $mlag > 0.01 ) {
        eval { ( $old, $new, $max, $range, @laggards ) = orblag( $orb, "orbmsd2days", 0 ) } ;
        if ( $@ ne "" ) {
            if ( $nerror > 2 ) {
                elog_complain( "orblag orbmsd2days failed 3 times in 15 minutes, check $orbname" ) ;
                $subject = "Problems - $pgm $host	$problems problems" ;
                &sendmail( $subject, $opt_m ) if $opt_m ; 
                elog_die( "\n$subject" ) ;
            }
            sleep 300 ;
            $nerror++ ;
            next ;
        }
        elog_notify( "	orbmsd2days	$old	$new	$max	$range	@laggards" ) if $opt_V ;
        ( $mlag, $thread, $pktid, $who, $what ) =  split ( ' ', $laggards[0], 5) ;
        elog_notify( "	orbmsd2days	$mlag" ) unless ( $n %= 10) ;
        $n++ ;
        sleep 60 unless $opt_n ;
    }
        
#     elog_notify( "Sleeping 5 minutes" ) ;
#     sleep 300 unless $opt_n ;
        
    return ;

}

sub send_sync { # $problems = &send_sync ( $sta, $dbops, $comment, $orbname, $sync_dfile, $problems ) ;
    my ( $sta, $dbops, $comment, $orbname, $sync_dfile, $problems ) = @_ ;
    my ( $cmd, $nsuccess, $short_hostname, $sync_dir, $year ) ;
    my ( @dbdmcfiles, @dbops, @pffiles ) ;
    
    elog_debug ( "start of send_sync  -	sync_dfile	\"$sync_dfile\" " ) ;

#
#  make sure wait_match specified properly in miniseed2orb.pf
#
    $nsuccess = 0 ;
    
    @pffiles = pffiles( "orbxfer2" ) ;
    elog_notify( "pffiles	@pffiles" ) ;
    unlink( "orbxfer2.pf" ) if ( -e "orbxfer2.pf" ) ;
    $cmd = "pfcp -d orbxfer2 ." ;
        
    if ( ! &run_cmd( $cmd ) ) {
        $problems++ ;
        elog_complain ( "FAILED:	$cmd" ) ;
        print PROB "FAILED: $cmd \n\n" ;
    }

    open(  OXF, ">>orbxfer2.pf" ) ;
    print  OXF  "wait_match \n" ;
    close( OXF ) ;
    elog_debug( "running in expert mode - no wait_match " ) if $opt_V ;

#
# send sync file twice to make sure it gets there.
#

    $cmd = "orbxfer2 -N sync sync/$sync_dfile  $orbname" ;
        
    if ( ! &run_cmd( $cmd ) ) {
        $problems++ ;
        elog_complain ( "FAILED:	$cmd" ) ;
        print PROB "FAILED: $cmd \n\n" ;
    } else {
        elog_notify ( $cmd ) ;
        $nsuccess++ ;
    }
        
    if ( ! &run_cmd( $cmd ) ) {
        $problems++ ;
        elog_complain ( "FAILED:	$cmd" ) ;
        print PROB "FAILED: $cmd \n\n" ;
    } else {
        elog_notify ( $cmd ) if ( ! $nsuccess ) ;
    }

    unlink ( "orbxfer2.pf" )                  unless $opt_V ;

#
# mv sync file to final resting place.
#

    $year  = epoch2str( now(), "%Y" ) ;
    
    $sync_dir   = "$pf{sync_dir}\/$year/$sta";
    makedir ( $sync_dir ) ;
    $cmd = "mv sync/$sync_dfile $sync_dir" ;
        
    if ( ! &run_cmd( $cmd ) ) {
        $problems++ ;
        print PROB "FAILED: $cmd \n\n" ;
    }
        
    @dbops          = dbopen( $dbops, "r+" );
    @dbdmcfiles     = dblookup(@dbops,0,"dmcfiles",0,0);
    
    $short_hostname = my_hostname() ;
    dbaddv( @dbdmcfiles, "time",    now(),
                         "comment", $comment,
                         "dir",     $sync_dir,
                         "dfile",   $sync_dfile,
                         "orb",     $orbname,
                         "auth",    "$short_hostname:batd" ) unless $opt_n ;
    dbclose( @dbops ) ;
       
    return ( $problems ) ;
 
}

sub check_deployment { # $problems =  &check_deployment( $sta, $db, $problems ) ;
    my ( $sta, $db, $problems ) = @_ ;
    my ( $cmd, $dep, $dep_end, $dep_install, $dep_remove, $dep_start, $line ) ;
    my ( $maxtime, $mintime, $st1, $st2, $subject ) ;
    my ( @dbdeploy, @dbwfdisc ) ;
    
    
    @dbwfdisc = dbopen  ( $sta, 'r' ) ;
    @dbwfdisc = dblookup( @dbwfdisc, 0, "wfdisc", 0, 0 ) ;

    @dbdeploy = dbopen  ( $db, 'r' ) ;
    @dbdeploy = dblookup( @dbdeploy, 0, "deployment", 0, 0 ) ;
    @dbdeploy = dbsubset( @dbdeploy, "sta =~ /$sta/" ) ;
    
    @dbdeploy    = dbsort  ( @dbdeploy, "time" ) ;
    $dbdeploy[3] = 0 ;
    $dep_start   = dbgetv ( @dbdeploy, "time" ) ;

    @dbdeploy    = dbsort  ( @dbdeploy, "endtime", "-r" ) ;
    $dbdeploy[3] = 0 ;
    $dep_end     = dbgetv ( @dbdeploy, "endtime" ) ;

    @dbdeploy    = dbsort  ( @dbdeploy, "equip_install" ) ;
    $dbdeploy[3] = 0 ;
    $dep_install = dbgetv ( @dbdeploy, "equip_install" ) ;

    @dbdeploy    = dbsort  ( @dbdeploy, "equip_remove", "-r" ) ;
    $dbdeploy[3] = 0 ;
    $dep_remove  = dbgetv ( @dbdeploy, "equip_remove" ) ;

    $mintime     = dbex_eval( @dbwfdisc, "min(time)" ) ;
    $maxtime     = dbex_eval( @dbwfdisc, "max(endtime)" ) ;

#
#  Verify start and end times in deployment table
#
    $dep = 0 ;
    open( DEP, "> /tmp/deploy" ) ;
    if ( $mintime < $dep_start ) {
        $st1 = strydtime( $mintime ) ;
        $st2 = strydtime( $dep_start );
        $line =  "Deployment table time field may need changing " ;
        $line .= "-	$sta db time $st2	new suggested time $st1" ;
        print DEP "$line\n" ;
        elog_notify( $line ) ;
        $dep++ ;
    }
    if ( $maxtime > $dep_end ) {
        $st1 = strydtime( $maxtime );
        $st2 = strydtime( $dep_end );
        $line = "Deployment table endtime field may need changing " ;
        $line .= "-	$sta db endtime $st2	new suggested endtime $st1" ; 
        print DEP "$line\n" ; 
        elog_notify( $line ) ;
        $dep++ ;
    }
    if ( $mintime < $dep_install ) {
        $st1 = strydtime( $mintime ) ;
        $st2 = strydtime( $dep_install );
        $line =  "Deployment table equip_install field may need changing " ;
        $line .= "-	$sta db equip_install $st2	new suggested equip_install $st1" ;
        print DEP "$line\n" ;
        $line =  "Dbmaster may need to be rebuilt" ;
        print DEP "$line\n" ;
        elog_notify( $line ) ;
        $dep++ ;
    }
    if ( $maxtime > $dep_remove ) {
        $st1 = strydtime( $maxtime );
        $st2 = strydtime( $dep_remove );
        $line = "Deployment table equip_remove field may need changing " ;
        $line .= "-	$sta db equip_remove $st2	new suggested equip_remove $st1" ; 
        print DEP "$line\n" ; 
        $line =  "Dbmaster may need to be rebuilt" ;
        print DEP "$line\n" ;
        elog_notify( $line ) ;
        $dep++ ;
    }
                
    close( DEP );

    $subject = "ANF TA Deployment table change - $sta" ;
    $cmd     = "rtmail -C -s '$subject' $pf{deploy_mail} < /tmp/deploy" ;
        
    if  ( $dep ) {
        if ( ! &run_cmd( $cmd ) ) {
            $problems++ ;
        }
    }
    
    dbclose ( @dbwfdisc ) ;
    dbclose ( @dbdeploy ) ;

    return ;
}

sub dblock { # $lock_status = &dblock ( $db, $lock_duration ) ;
    my ( $db, $lock_duration ) = @_ ;
    my ( $Pf, $dbloc_pf_file, $host, $pid ) ;
    my ( %pf ) ;
    
    chop ($host = `uname -n` ) ;
    $pid = $$ ;

    $Pf            = $db . "_LOCK" ;
    $dbloc_pf_file = $db . "_LOCK.pf" ;
    elog_debug ( "Pf	$Pf	dbloc_pf_file	$dbloc_pf_file	pid $pid" ) if $opt_V ;
    
    if ( ! -f $dbloc_pf_file ) {
        elog_debug ( sprintf ("$db new lock set to %s", strydtime ( now() + $lock_duration ) ) ) if $opt_V ;
        &write_dblock ( $dbloc_pf_file, $0, $host, $pid, &now(), &now() + $lock_duration ) ;
        return ( 0 ) ; 
    } else { 
        %pf = getparam( $Pf ) ;
        if ( $pf{unlock_time} > &now() && $pf{pid} != $pid ) {
            elog_complain ( sprintf ("$db is locked until %s", strydtime ( $pf{unlock_time} ) ) ) ;
            prettyprint ( \%pf ) ;
            return ( 1 ) ;
        } elsif  ( $pf{unlock_time} > &now() && $pf{pid} == $pid ) {
            elog_debug ( sprintf ("$db lock is extended to %s", strydtime ( now() + $lock_duration ) ) ) if $opt_V ;
            &write_dblock ( $dbloc_pf_file, $0, $host, $pid, $pf{lock_time}, now() + $lock_duration ) ;
            %pf = () ;
            return ( 0 ) ;
        } else {
            elog_debug ( sprintf ("$db lock set to %s", strydtime ( now() + $lock_duration ) ) ) if $opt_V ;
            &write_dblock ( $dbloc_pf_file, $0, $host, $pid, &now(), &now() + $lock_duration ) ;
            %pf = () ;
            return ( 0 ) ;
        }
    }    
}

sub dbunlock { # $lock_status = &dbunlock ( $db ) ;
    my ( $db ) = @_ ;
    my ( $Pf, $dbloc_pf_file, $host, $host1, $lock_time1, $pid, $pid1, $program1, $unlock_time1 ) ;
    my ( %pf ) ;
    
    chop ($host = `uname -n` ) ;
    $pid = $$ ;

    $Pf            = $db . "_LOCK" ;
    $dbloc_pf_file = $db . "_LOCK.pf" ;
    elog_debug ( "Pf	$Pf	dbloc_pf_file	$dbloc_pf_file" ) if $opt_V ;
    
    if ( ! -f $dbloc_pf_file ) {
        elog_complain ( "dbunlock:	$dbloc_pf_file does not exist!" ) ;
        return ( 1 ) ;
    } else { 
        pfupdate ( $Pf ) ; 
        %pf = getparam( $Pf ) ;
        if ( $0 !~ /$pf{program}/ || $pid != $pf{pid} || $host !~ /$pf{host}/ ) {
            elog_complain ( "unable to unlock $db" ) ;
            elog_complain ( "program	$0	$pf{program}" ) ;
            elog_complain ( "pid	$pid	$pf{pid}" ) ;
            elog_complain ( "host	$host	$pf{host}" ) ;            
            return ( 1 ) ;
        }
        if ( $pf{unlock_time} < &now() ) {
            elog_complain ( sprintf ("$db was already unlocked at %s", strydtime ( $pf{unlock_time} ) ) ) ;
            return ( 1 ) ;
        } 
        &write_dblock ( $dbloc_pf_file, $0, $host, $pid, $pf{lock_time}, &now() ) ;
        return ( 0 ) ;
    }    
}

sub write_dblock { # &write_dblock ( $dbloc_pf_file, $program, $host, $pid, $lock_time, $unlock_time ) ;
    my ( $dbloc_pf_file, $program, $host, $pid, $lock_time, $unlock_time ) = @_ ;
    open( LOCK,   ">$dbloc_pf_file" ) ;
    print LOCK    "program      $program\n" ;
    print LOCK    "host         $host\n" ;
    print LOCK    "pid          $pid\n" ;
    printf ( LOCK "lock_time    %d\n", $lock_time ) ;
    printf ( LOCK "unlock_time  %d\n", $unlock_time ) ;
    close LOCK ;
    return ; 
}
