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
    use warnings ;
    use Datascope ;
    use orb ;
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

    my ( $Pf, $cmd, $db, $nchild, $nstas, $opt_c, $orb, $orbclient, $orbname, $orbsize, $problems, $sta, $stime, $subject, $usage );
    my ( @stas );
    my ( %stas );


    $pgm = $0 ; 
    $pgm =~ s".*/"" ;
    elog_init($pgm, @ARGV);
    $cmd = "\n$0 @ARGV" ;
    
    if (  ! getopts('vVnc:m:p:s:') || ( @ARGV != 2 ) ) { 
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
    prettyprint ( \%stas ) ; # if $opt_V ;

#
#  process all stations
#
        
    STA: foreach $sta ( @stas ) {
    
        $problems = &proc_sta( $sta, $db, $orbname, $orb, $orbclient, $orbsize, \%stas, $Pf, $problems ) ;

    }

#
# Finish up
#
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
    my ( $endtime, $endtime_null, $equip_install, $key1, $key2, $nrows, $row, $sta, $subject, $time ) ;
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
    my ( $cmd, $parent, $success );
    my ( @output );
    
    unlink "/tmp/JUNK.mseed"      if ( -e "/tmp/JUNK.mseed" ) ;
    unlink "JUNK.lastid"          if ( -e "JUNK.lastid" ) ;
    unlink "JUNK.wfdisc"          if ( -e "JUNK.wfdisc" ) ;
    unlink "JUNK.pf"              if ( -e "JUNK.pf" ) ;
    unlink "miniseed2orb_JUNK.pf" if ( -e "miniseed2orb_JUNK.pf" ) ;
    
    $parent = $$ ;

    $cmd = "trsignal -d sd -r 40 -s JUNK -w /tmp/JUNK.mseed JUNK";
        
    ( $success, @output )  = &run_cmd( $cmd );
    if ( ! $success ) {
        $problems++ ;
        &print_prob ( $problems, "FAILED: $cmd", $parent, *PROB ) ;
    }
    @output = () ;
    
    unlink "JUNK.lastid" ;
    unlink "JUNK.wfdisc" ;
   
    open ( PF, ">JUNK.pf" ) ;
    print  PF "net     \&Tbl\{ \n" ;
    print  PF "	.*	XX \n" ;
    print  PF "\}\n" ;
    close( PF ) ;

    $cmd = "fix_miniseed -p JUNK /tmp/JUNK.mseed " ;
        
    ( $success, @output )  = &run_cmd( $cmd );
    if ( ! $success ) {
        $problems++ ;
        &print_prob ( $problems, "FAILED: $cmd", $parent, *PROB ) ;
    }
    @output = () ;

    unlink "JUNK.pf" ;
    
    $cmd = "pfcp miniseed2orb miniseed2orb_JUNK " ;
        
    ( $success, @output )  = &run_cmd( $cmd );
    if ( ! $success ) {
        $problems++ ;
        &print_prob ( $problems, "FAILED: $cmd", $parent, *PROB ) ;
    }
    @output = () ;

    open(  MS, ">>miniseed2orb_JUNK.pf") ;
    print  MS  "wait_match \n" ;
    close( MS ) ;
            
    $cmd = "miniseed2orb -p miniseed2orb_JUNK -u /tmp/JUNK.mseed $orbname " ;
        
    ( $success, @output )  = &run_cmd( $cmd );
    if ( ! $success ) {
        $problems++ ;
        &print_prob ( $problems, "FAILED: $cmd", $parent, *PROB ) ;
    }
    @output = () ;

    unlink "/tmp/JUNK.mseed" ;
    unlink "miniseed2orb_JUNK.pf" ;

    return( $problems ) ;
}

sub yearmonth {  # $yearmonth = &yearmonth ( $epoch ) ;
    my ( $epoch ) = @_ ;
    my ( $yearmonth ) ;
    
    $yearmonth = epoch2str ( $epoch, "%Y%m" ) ;

    return ( $yearmonth ) ;
}

sub proc_sta { # $problems = &proc_sta( $sta, $db, $orbname, $orb, $orbclient, $orbsize, \%stas, $Pf, $problems ) ;
    my ( $sta, $db, $orbname, $orb, $orbclient, $orbsize, $ref, $Pf, $problems ) = @_ ;
    my ( $baler_active, $chan, $cmd, $comment, $dbsize, $endtime, $nrows, $parent, $prob, $prob_check, $stime, $subject, $success, $sync_dfile, $time, $tmp_db, $tmp_sync ) ;
    my ( @db, @dbreplayed, @dbtmp, @dbtwf, @output ) ;
    my ( %stas ) = %{$ref} ;

    $stime = strydtime( now() ) ;
    
    $parent = $$ ; 
        
    elog_notify ( "starting processing station $sta    $stime");
    
    open( PROB,"> /tmp/prob_$sta\_$$");
    
    print PROB "$stime      starting processing \n\n" ;

    $prob_check = $prob = 0;
        
    $baler_active = "$pf{baler_active}/$sta" ;
    
    chdir( $baler_active ) ;
    fork_notify ( $parent, "Changed directory to $baler_active " ) if $opt_v ;

    ( $tmp_db, $comment, $dbsize ) = &build_tmp_db( $sta ) ;

    if ( $dbsize ) {
#
#  Transfer wf data to DMC using obsip2orb
#

        $cmd  = "obsip2orb " ;
        $cmd  .= "-X $orbname $tmp_db" if ($orbsize >= $dbsize) ;
        $cmd  .= "-c $orbclient $orbname $tmp_db" if ($orbsize < $dbsize) ;
        
        elog_notify( $cmd ) ; 
         
        ( $success, @output )  = &run_cmd( $cmd ) ;
        if ( ! $success ) {
            $problems++ ;
            $subject = "Problems - $pgm $host	obsip2orb $sta" ;
            &sendmail($subject, $opt_m) if $opt_m ; 
            elog_die("\n$subject") ;
        }
        @output = () ;
    
        @db           = dbopen  ( $sta, "r+" ) ; 
        @dbreplayed   = dblookup( @db, 0, "replayed", 0, 0 ) ;

        @dbtmp        = dbopen  ( $tmp_db, "r" ) ; 
        @dbtwf        = dblookup( @dbtmp, 0, "wfdisc", 0, 0 ) ;
    
        $nrows = dbquery(@dbtwf,"dbRECORD_COUNT") ;
    
        unless ( $opt_n ) {
            for ( $dbtwf[3] = 0; $dbtwf[3]<$nrows; $dbtwf[3]++ ) {
                $dbreplayed[3] = dbaddnull( @dbreplayed ) ;
                ( $sta, $chan, $time, $endtime )  = dbgetv ( @dbtwf, qw ( sta chan time endtime ) ) ;
                dbputv ( @dbreplayed, "sta", $sta, "chan", $chan, "time", $time, "endtime",  $endtime ) ;
            }
        }
    
        dbclose ( @db ) ;
        dbclose ( @dbtmp ) ;
    
#
#  Transfer non-wf data to DMC 
#

        $tmp_sync = proc_nonwf_seed( $sta, $tmp_db, $Pf, $dbsize, $orbname, $orbsize, $orbclient, $problems ) ;
#
#  Make sync file 
#
    
        $sync_dfile = &sync_file ( $sta, $tmp_db, $tmp_sync, $problems ) ;

#
#  wait until orblag value become acceptable
#
    
        &wait_for_orb_to_empty ( $orb, $problems ) ;

#
#  Transfer DMC sync file
#

        $problems = &send_sync ( $sta, $db, $comment, $orbname, $sync_dfile, $problems ) ;

#
#  If station completed
#

        unlink ( "sync/tmp_sync" )                 unless $opt_V ;
        unlink ( "$tmp_db" )                       unless $opt_V ;
        unlink ( "$tmp_db.wfdisc" )                unless $opt_V ;
        
        if ( exists $stas{$sta}{wf}{endtime} ) {
   
            &check_deployment( $sta, $db ) ; 
            chdir( $pf{baler_active} ) ;
            
            $cmd = "mv $baler_active $pf{baler_final}" ;
        
            ( $success, @output )  = &run_cmd( $cmd );
            if ( ! $success ) {
                $problems++ ;
                &print_prob ( $problems, "FAILED: $cmd", $parent, *PROB ) ;
            } else {
                $comment = "Station completed, moved to $pf{baler_final}" ;
                fork_notify ( $comment ) ;
                print PROB "$comment \n\n" ;
            }
            @output = () ;

        }
    }
    
#
#  clean up
#
        
    $stime = strydtime( now() );
    print PROB "$stime      end processing \n\n" ;
    close(PROB);
                 
    if  ( $prob ) {
        $subject = "TA $sta     $prob problems -  $pgm on $host";
        $cmd     = "rtmail -C -s '$subject' $pf{prob_mail} < /tmp/prob_$sta\_$$";
        fork_notify ( $parent, $cmd) ;
        
        ( $success, @output )  = &run_cmd( $cmd ) ;

    } elsif ( $opt_v ) {
        $subject = "TA $sta transmission completed -  $pgm on $host";
        $cmd     = "rtmail -C -s '$subject' $pf{success_mail} < /tmp/prob_$sta\_$$";
        fork_notify ( $parent, $cmd) ;
        
        ( $success, @output )  = &run_cmd( $cmd ) ;

    } 

    unlink "/tmp/prob_$sta\_$$" unless $opt_V;
    
    $stime = strydtime( now() );
    fork_notify ( $parent, "proc_sta complete for $sta with $prob problems    $stime") ;

    return ( $problems ) ;

}

sub build_tmp_db { # ( $tmpdb, $comment, $dbsize ) = &build_tmp_db( $sta ) ;
    my ( $sta ) = @_ ;
    my ( $chan, $comment, $datatype, $dbname, $dbsize, $dfile, $dir, $endtime, $foff, $line, $max_dir ) ;
    my ( $max_month, $min_dir, $min_month, $nojoin, $nrows, $nsamp, $nwfs, $replay, $row, $samprate, $time, $tmpdb ) ;
    my ( @db, @dbblh, @dbreplayed, @dbscr, @dbsize, @dbsoh, @dbtmp, @dbtsc, @dbtwf, @dbwfdisc, @missing, @rows ) ;
    
    $tmpdb = "tmp_replay_$sta\_$$" ;
    
    elog_debug( "build_tmp_db $sta begins" ) if $opt_V ;
    
    @db           = dbopen  ( $sta, "r" ) ; 
    @dbwfdisc     = dblookup( @db, 0, "wfdisc", 0, 0 ) ;
    @dbwfdisc     = dbsubset( @dbwfdisc, "chan !~ /$pf{wfclean}/ " ) ;
    @dbreplayed   = dblookup( @db, 0, "replayed", 0, 0 ) ;
    @dbscr        = dblookup( @dbreplayed, 0, 0, 0, "dbSCRATCH" ) ;

    @dbtmp        = dbopen  ( $tmpdb, "r+" ) ; 
    @dbtwf        = dblookup( @dbtmp, 0, "wfdisc", 0, 0 ) ;
    
    $nwfs         = dbquery( @dbwfdisc, "dbRECORD_COUNT" ) ;
    $replay       = dbquery( @dbreplayed, "dbTABLE_PRESENT" ) ;
    
    elog_debug( "open	$tmpdb" ) if $opt_V ;
#
#  load tmp wfdisc with all non-replayed waveforms 
#
    for ( $dbwfdisc[3] = 0; $dbwfdisc[3] < $nwfs; $dbwfdisc[3]++ ) {

        ( $sta, $chan, $time, $endtime, $nsamp, $samprate, $datatype, $dir, $dfile, $foff )  = 
            dbgetv ( @dbwfdisc, qw ( sta chan time endtime nsamp samprate datatype dir dfile foff ) ) ;
        
        if ( $replay ) {
            
            dbputv( @dbscr, "sta", $sta, "chan", $chan, "time", $time, "endtime", $endtime ) ;
            
            @rows = dbmatches( @dbscr, @dbreplayed, "replayed_$sta", "sta", "chan", "time", "endtime" ) ;
            
            next if ( $#rows == 0 ) ;
            
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
            
        } else {
        
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

    }
    
    elog_debug( "$tmpdb.wfdisc built" ) if $opt_V ;
    
    if ( ! dbquery( @dbtwf, "dbTABLE_PRESENT" ) ) {
        elog_notify ( "$sta has no new data to process " ) ;
        print PROB  "$sta has no new data to process\n" ;
        return ( "", "", 0 ) ;
    }
        
    &cssdescriptor ( $tmpdb, $pf{dbpath}, $pf{dblocks}, $pf{dbidserver} ) ;

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
        
    $line    = "dbname $sta	Total Bytes - 	$dbsize" ;
    $comment = "$sta active baler data sent to DMC    $min_dir $max_dir    $min_month $max_month    Total Bytes \- $dbsize" ;
    print  PROB "$line \n" ;
    print  PROB "$comment \n" ;

    elog_notify( $line ) ;        
    elog_notify( $comment ) if $opt_n ;        
    
    dbclose ( @db ) ;
    dbclose ( @dbtmp ) ;
#
# schanloc test
#
    @dbtmp   = dbopen  ( $tmpdb, "r" ) ; 
    @dbtwf   = dblookup( @dbtmp, 0, "wfdisc", 0, 0 ) ;
    @dbtsc   = dblookup( @dbtmp, 0, "schanloc", 0, 0 ) ;
    
    @dbtwf   = dbnojoin( @dbtwf, @dbtsc ) ;
    $nojoin  = dbquery ( @dbtwf, "dbRECORD_COUNT" ) ;
    
    elog_notify( sprintf( "%d rows in wfdisc after nojoin", $nojoin ) ) ;
    
    if ( $nojoin ) {
        @dbtwf = dbsort( @dbtwf, "-u", "chan" ) ;
        @missing = () ;
        for ( $dbtwf[3] = 0; $dbtwf[3] < dbquery ( @dbtwf, "dbRECORD_COUNT" ); $dbtwf[3]++ ) {
            push @missing, dbgetv( @dbtwf, "chan" ) ;
        }
        elog_complain ( "$nojoin records in wfdisc do not join to schanloc - @missing" ) ;
        elog_complain ( "$sta - no data sent to orb" ) ;
        $dbsize = 0 ;
    }
    
    dbclose ( @dbtmp ) ;
    return ( $tmpdb, $comment, $dbsize ) ;
}

sub proc_nonwf_seed { # $tmp_sync = proc_nonwf_seed( $sta, $tmpdb, $Pf, $dbsize, $orbname, $orbsize, $orbclient, $problems ) ;
    my ( $sta, $tmpdb, $Pf, $dbsize, $orbname, $orbsize, $orbclient, $problems ) = @_ ;
    my ( $base, $chan, $cmd, $dir, $mseedfile, $net, $nrows, $ref, $st1, $st2, $st3, $subject, $success, $suf, $tmp_sync ) ;
    my (  @chans, @dbnonwf, @dbtmp, @dbtwf, @dirs, @files, @line, @msd, @mseedfiles, @output, @pffiles ) ;

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
            
        ( $success, @output )  = &run_cmd( $cmd );
        if ( ! $success ) {
            $problems++ ;
            $subject = "Problems - $pgm $host	miniseed2orb $mseedfile" ;
            &sendmail($subject, $opt_m) if $opt_m ; 
            elog_die("\n$subject") ;
        }
        @output = () ;
            
        $st3 = epoch2str( now(), "%Y,%j" ) ;
        print SYNC "$net|$sta||$chan|$st1|$st2||||||||||$st3\n" unless $opt_n ;
            
    }
        
    close SYNC unless $opt_n ;

    unlink ( "miniseed2orb_sta_final.pf" )    unless $opt_V ;
    
    return ( $tmp_sync ) ;

}

sub sync_file { # $sync_dfile = &sync_file ( $sta, $tmp_db, $tmp_sync, $problems ) ;
    my ( $sta, $tmp_db, $tmp_sync, $problems ) = @_ ;
    my ( $cmd, $parent, $subject, $success, $sync_dfile, $yearday ) ;
    my (  @output ) ;
#
#  Make DMC sync file
#
    $parent = $$ ; 
    $yearday = &yearday( now () ) ;
    
    makedir("sync") if (! -d "sync" && ! $opt_n);

    $sync_dfile = "$sta\_$yearday.sync";
    
    $cmd = "db2sync -h $tmp_db sync/$sync_dfile" ;

    ( $success, @output )  = &run_cmd( $cmd );
    if ( ! $success ) {
        $problems++ ;
        &print_prob ( $problems, "FAILED: $cmd", $parent, *PROB ) ;
    }
    @output = () ;
    sleep 5 ;
        
    $cmd = "cat  $tmp_sync >> sync/$sync_dfile" ;
        
    if ( ! -e "sync/$sync_dfile" && ! $opt_n ) {
        elog_complain( "sync/$sync_dfile does not exist!" ) ;
        elog_complain( "$cmd	will fail!" ) ;
        $problems++ ;
        $subject = "Problems - $pgm $host	$problems problems" ;
        &sendmail( $subject, $opt_m ) if $opt_m ; 
        elog_die( "\n$subject" ) ;
    }
        
    ( $success, @output )  = &run_cmd( $cmd );
    if ( ! $success ) {
        $problems++ ;
        &print_prob ( $problems, "FAILED: $cmd", $parent, *PROB ) ;
    }
    @output = () ;
    
    return ( $sync_dfile ) ;
}

sub wait_for_orb_to_empty { # &wait_for_orb_to_empty ( $orb, $problems ) ;
    my ( $orb, $problems ) = @_ ;
    my ( $max, $mlag, $n, $nerror, $new, $old, $orbname, $pktid, $range, $subject, $thread, $what, $who ) ;
    my ( @laggards) ;

    if ( $orb < 0 and ! $opt_n ) {
        $problems++ ;
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
                elog_complain( "orblag orbmsd2days failed 3 times, check $orbname" ) ;
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
    my ( $cmd, $nsuccess, $parent, $success, $sync_dir, $year ) ;
    my ( @dbdmcfiles, @dbops, @output, @pffiles ) ;
#
#  make sure wait_match specified properly in miniseed2orb.pf
#
    $nsuccess = 0 ;
    
    $parent = $$ ;

    @pffiles = pffiles( "orbxfer2" ) ;
    elog_notify( "pffiles	@pffiles" ) ;
    unlink( "orbxfer2.pf" ) if ( -e "orbxfer2.pf" ) ;
    $cmd = "pfcp -d orbxfer2 ." ;
        
    ( $success, @output )  = &run_cmd( $cmd );
    if ( ! $success ) {
        $problems++ ;
        &print_prob ( $problems, "FAILED: $cmd", $parent, *PROB ) ;
    }
    @output = () ;

    open(  OXF, ">>orbxfer2.pf" ) ;
    print  OXF  "wait_match \n" ;
    close( OXF ) ;
    elog_debug( "running in expert mode - no wait_match " ) if $opt_V ;

#
# send sync file twice to make sure it gets there.
#

    $cmd = "orbxfer2 -N sync sync/$sync_dfile  $orbname" ;
        
    ( $success, @output )  = &run_cmd( $cmd );
    if ( ! $success ) {
        $problems++ ;
        &print_prob ( $problems, "FAILED: $cmd", $parent, *PROB ) ;
    } else {
        elog_notify ( $cmd ) ;
        $nsuccess++ ;
    }
    @output = () ;
        
    ( $success, @output )  = &run_cmd( $cmd );
    if ( ! $success ) {
        $problems++ ;
        &print_prob ( $problems, "FAILED: $cmd", $parent, *PROB ) ;
    } else {
        elog_notify ( $cmd ) if ( ! $nsuccess ) ;
    }
    @output = () ;

    unlink ( "orbxfer2.pf" )                  unless $opt_V ;

#
# mv sync file to final resting place.
#

    $year  = epoch2str( now(), "%Y" ) ;
    
    $sync_dir   = "$pf{sync_dir}\/$year/$sta";
    makedir ( $sync_dir ) ;
    $cmd = "mv sync/$sync_dfile $sync_dir" ;
        
    ( $success, @output )  = &run_cmd( $cmd );
    if ( ! $success ) {
        $problems++ ;
        &print_prob ( $problems, "FAILED: $cmd", $parent, *PROB ) ;
    }
    @output = () ;

    @dbops         = dbopen( $dbops, "r+" );
    @dbdmcfiles    = dblookup(@dbops,0,"dmcfiles",0,0);
    dbaddv( @dbdmcfiles, "time",    now(),
                         "comment", $comment,
                         "dir",     $sync_dir,
                         "dfile",   $sync_dfile,
                         "orb",     $orbname,
                         "auth",    "$host:batd" ) unless $opt_n ;
    dbclose( @dbops ) ;
       
    return ( $problems ) ;
 
}

sub check_deployment { # $problems =  &check_deployment( $sta, $db, $problems ) ;
    my ( $sta, $db, $problems ) = @_ ;
    my ( $cmd, $dep, $dep_end, $dep_start, $line, $maxtime, $mintime, $st1, $st2, $subject, $success ) ;
    my ( @dbdeploy, @dbwfdisc, @output ) ;
    
    
    @dbwfdisc = dbopen  ( $sta, 'r' ) ;
    @dbwfdisc = dblookup( @dbwfdisc, 0, "wfdisc", 0, 0 ) ;

    @dbdeploy = dbopen  ( $db, 'r' ) ;
    @dbdeploy = dblookup( @dbdeploy, 0, "deployment", 0, 0 ) ;
    @dbdeploy = dbsubset( @dbdeploy, "sta =~ /$sta/" ) ;
    
    @dbdeploy = dbsort  ( @dbdeploy, "time" ) ;
    $dbdeploy[3] = 0 ;
    $dep_start = dbgetv ( @dbdeploy, "time" ) ;

    @dbdeploy = dbsort  ( @dbdeploy, "endtime", "-r" ) ;
    $dbdeploy[3] = 0 ;
    $dep_end = dbgetv ( @dbdeploy, "endtime" ) ;

    $mintime  = dbex_eval( @dbwfdisc, "min(time)" ) ;
    $maxtime  = dbex_eval( @dbwfdisc, "max(endtime)" ) ;

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
                
    close( DEP );

    $subject = "ANF TA Deployment table change - $sta" ;
    $cmd     = "rtmail -C -s '$subject' $pf{deploy_mail} < /tmp/deploy" ;
        
    if  ( $dep ) {
        ( $success, @output )  = &run_cmd( $cmd );
        if ( ! $success ) {
            $problems++ ;
        }
        @output = () ;
    }
    
    dbclose ( @dbwfdisc ) ;
    dbclose ( @dbdeploy ) ;

    return ;
}