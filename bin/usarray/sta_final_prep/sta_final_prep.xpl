#
#   program needs:
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
    use strict ;
    use Datascope ;
    use archive ;
    use timeslice ;
    use utilfunct ;
    use orb ;
    
    our ( $pgm, $host );
    our ( $opt_v, $opt_V, $opt_m, $opt_n, $opt_p );
    
{    #  Main program

    my ( $Pf, $bhname, $cmd, $dbavail, $dbdevice, $dbname, $dirname, $etime, $fsta, $line ) ;
    my ( $maxtime, $maxtime_baler, $maxtime_rt, $mintime, $mintime_baler, $mintime_rt, $mseedfile ) ;
    my ( $nrows, $prob, $prob_check, $problems, $row, $rtdb, $snet, $sohname, $sta, $sta_base ) ;
    my ( $sta_size, $statmp, $stime, $subject, $table, $usage ) ;
    my ( @db, @dbbh, @dbrt, @dbsnet, @dbtest, @dbwfdisc, @mseedfiles ) ;
    my ( %pf ) ;

    $pgm = $0 ; 
    $pgm =~ s".*/"" ;
    elog_init( $pgm, @ARGV) ;
    $cmd = "\n$0 @ARGV" ;
    
    if (  ! getopts('vVnm:p:') || @ARGV < 1 ) { 
        $usage  =  "\n\n\nUsage: $0  \n	[-v] [-V] [-n] \n" ;
        $usage .=  "	[-p pf] [-m mail_to]  \n" ;
        $usage .=  "	 sta [sta_1 sta_2 ...]\n\n"  ; 
        
        elog_notify($cmd) ; 
        elog_die ( $usage ) ; 
    }
    
    &savemail() if $opt_m ; 
    elog_notify($cmd) ; 
    $stime = strydtime(now());
    chop ($host = `uname -n` ) ;
    elog_notify ("\nstarting execution on	$host	$stime\n\n");

    $Pf         = $opt_p || $pgm ;
    
    $opt_v      = defined($opt_V) ? $opt_V : $opt_v ;    
    
    %pf = getparam( $Pf );
    
    if (system_check(0)) {
        $subject = "Problems - $pgm $host	Ran out of system resources";
        &sendmail($subject, $opt_m) if $opt_m ; 
        elog_die("\n$subject");
    }
    $problems = 0;


#   subset for unprocessed data

#
#  process all new stations
#

    $table = "wfdisc";
    foreach $sta ( sort ( @ARGV ) ) {
        $stime = strydtime( now() );
        
        ( $dbdevice, $dbavail ) = &df( $pf{archivebase} );
        
        $sta_base = "$pf{balerdirbase}\/$sta";
        
        $sta_size =  `du -sk $sta_base` ;
        
        $sta_size =~ /^(\d+)/ ;
        
        $sta_size = ( $sta_size / 1024 ) + (2 * 1024) ;  # make sure 2 Gbytes free space

        if ($dbavail < $sta_size ) {
            $problems++ ;
            elog_complain( "Problem #$problems" );
            elog_complain( sprintf( "Only %d available on $pf{archivebase} ", $dbavail) );
            elog_complain( sprintf( "Need %d megabytes available ", $sta_size) );
            $subject = "Problems - $pgm $host	$problems problems" ;
            &sendmail($subject, $opt_m) if $opt_m ; 
            elog_die("\n$subject") ;
        }

        elog_notify ("\nstarting processing station $sta\n\n");
    
#
#  perform existance checks
#
        $prob_check = $problems ;
        $bhname  = "$pf{balerdirbase}\/$sta\/$pf{bhdata_dir}";
        $sohname = "$pf{balerdirbase}\/$sta\/$pf{sohdata_dir}";
        elog_notify("bhname	$bhname	sohname	$sohname") if $opt_v ;
        if (! -d $bhname ) {
            $problems++ ;
            elog_complain("\nProblem $problems
                           \n	directory $bhname does not exist
                           \n	Skipping to next station");
        }
                        
        if ( ! -d $sohname ) {
            $problems++ ;
            elog_complain("\nProblem $problems
                           \n	directory $sohname does not exist
                           \n	Skipping to next station");
        }
                        
        $dirname  = "$pf{archivebase}\/$sta";
        
        $dbname   = "$pf{archivebase}\/$sta\/$sta";
        elog_notify("dirname	$dirname	dbname	$dbname") if $opt_v ;
        if (-d $dbname || -e "$dbname.$table") {
            @dbtest = dbopen($dbname,"r") ;
            @dbtest = dblookup(@dbtest,0,"$table",0,0) ;
            if (dbquery(@dbtest,dbTABLE_PRESENT)) {
                $problems++ ;
                elog_complain("\nProblem $problems
                               \n	database $dbname.$table already exists!
                               \n	Skipping to next station") ;
            }
            dbclose(@dbtest);
        }
        
        $rtdb   = "$pf{rt_sta_dir}\/$sta/$sta";
        elog_notify("rtdb	$rtdb	") if $opt_v ;
        if ( ! -f $rtdb ) {
            $problems++ ;
            elog_complain("\nProblem $problems
                           \n	station rt data db $rtdb does not exist
                           \n	Skipping to next station");
        }
        
        next if ($prob_check != $problems);
#
#  make output directory
#
        makedir($dirname);
        chdir($dirname);
        elog_notify("\nChanged directory to $dirname") if $opt_v ;
        
#
#  build station-channel-day seed files from baler final seismic miniseed directory  
#

        opendir( DIR, $bhname );
        @mseedfiles = grep { /C.*\.bms.*/  } readdir(DIR);
        closedir( DIR );
        
        if ($#mseedfiles == -1 ) {
            $problems++ ;
            elog_complain("\nProblem $problems
                           \n	$dbname has no '.*bms.*' files in $bhname! 
                           \n	Skipping to next station") ;
            next;
        }
        
        foreach $mseedfile (@mseedfiles) {
            $cmd  = "miniseed2days -c -U -m \".*_.*_[BL]H._.*\" ";
            $cmd .= "-v " if $opt_V;
            $cmd .= " - < $bhname/$mseedfile ";

            if ( ! &run_cmd( $cmd ) ) {
                $problems++ ;
            }
        
        }
        
#
#  build station-channel-year seed files from baler final soh, VH, UH miniseed directory  
#

        opendir( DIR, $sohname );
        @mseedfiles = grep { /C.*\.bms.*/  } readdir(DIR);
        closedir( DIR );
                
        if ($#mseedfiles == -1 ) {
            $problems++ ;
            elog_complain("\nProblem $problems
                           \n	$dbname has no '.*bms.*' files in $sohname! 
                           \n	Skipping to next station") ;
            next;
        }
        
        foreach $mseedfile (@mseedfiles) {
            $cmd  = "miniseed2days -c -U -r \".*_.*_[BHL]H[ZNE12]_.*\" -w %Y/%{net}_%{sta}_%{chan}.msd ";
            $cmd .= "-v " if $opt_V;
            $cmd .= " - < $sohname/$mseedfile ";

            if ( ! &run_cmd( $cmd ) ) {
                $problems++ ;
            }
        }

#
#  run miniseed2db for all baler data.  set  miniseed_segment_seconds=0 to remove one day wfdisc 
#  default in miniseed2db
#
        unlink($sta) if (-e $sta);
        
        open( TR, ">trdefaults.pf" );
        print TR "miniseed_segment_seconds 0\n";
        close( TR );
        
        $cmd  = "miniseed2db ";
        $cmd .= "-v " if $opt_V;
        $cmd .= "20\*/[0-3][0-9][0-9] $sta ";
        
        if ( ! &run_cmd( $cmd ) ) {
            $problems++ ;
        }
        
        $cmd  = "miniseed2db -T 0.001 ";
        $cmd .= "-v " if $opt_V;
        $cmd .= "20\*/\*.msd $sta ";
        
        if ( ! &run_cmd( $cmd ) ) {
            $problems++ ;
        }
        
        unlink("trdefaults.pf");

#
#  Clean up final station wfdsic
#
        
        $cmd = "dbsubset $sta.wfdisc \"$pf{wfclean}\" | dbdelete -";
                
        if ( ! &run_cmd( $cmd ) ) {
            $problems++ ;
        }
        
#
#  Check for anomolous net and sta
#
        $prob = 0;
        open(PROB,"> /tmp/prob_$sta\_$$");
        
        @db     = dbopen(   $sta, 'r' );
        @dbsnet = dblookup( @db, 0, "snetsta", 0, 0 );
        $nrows  = dbquery(  @dbsnet, "dbRECORD_COUNT" );
        
        if ($nrows > 1) {
            $line = "\nDatabase problem\n	$sta database has $nrows unique net-sta pairs";
            elog_complain($line);
            print PROB "$line\n";

            for ($row = 0; $row<$nrows; $row++) {
                $dbsnet[3] = $row;
                ($snet,$fsta,$statmp) = dbgetv(@dbsnet,"snet","fsta","sta");
                $line = "	snet	$snet	fsta	$fsta	sta	$statmp";
                elog_complain($line) ;
                print PROB "$line\n";
            }
            $prob++;
        }

        unlink("$sta");
        unlink("$sta.lastid");

#
#  Set up descriptor file
#
        
        &cssdescriptor ( $sta, $pf{dbpath}, $pf{dblocks}, $pf{dbidserver} ) unless $opt_n;

#
#  Find start time and end times
#

        unless ($opt_n) {
            @dbwfdisc = dblookup( @db, 0, "wfdisc", 0, 0);
            @dbbh     = dbsubset( @dbwfdisc, "sta =~/$sta/ && chan =~ /[BL]H[ZNE]/");
            if (dbquery( @dbbh, "dbRECORD_COUNT" ) == 0 ) {
                $problems++ ;
                elog_complain("\nProblem $problems
                               \n	$dbname\.wfdisc has no chan =~ /[BL]H[ZNE]/! 
                               \n	Skipping to next station") ;
                next;
            }
            $mintime_baler  = dbex_eval( @dbbh, "min(time)" ) ;
            $maxtime_baler  = dbex_eval( @dbbh, "max(endtime)" ) ;
            
            elog_notify(sprintf("Baler times  %s    %s", strydtime( $mintime_baler ), strydtime( $maxtime_baler )));
            
            @dbrt        = dbopen   ( $rtdb, "r" ) ;
            @dbrt        = dblookup ( @dbrt, 0, "wfdisc", 0, 0 ) ;
            $mintime_rt  = dbex_eval( @dbrt, "min(time)" ) ;
            $maxtime_rt  = dbex_eval( @dbrt, "max(endtime)" ) ;
            dbclose( @dbrt ) ;

            elog_notify(sprintf("Rt times     %s    %s", strydtime( $mintime_rt ), strydtime( $maxtime_rt )));
            
            $mintime = $mintime_baler ;
            $maxtime = $maxtime_baler ;
            
            $mintime = $mintime_rt if ( $mintime_rt < $mintime ) ;
            $maxtime = $maxtime_rt if ( $maxtime_rt > $maxtime ) ;
            
            $maxtime = epoch( yearday( $maxtime + 86400 ) ) if ($maxtime > epoch( yearday( $maxtime ) ) ) ;

        }
        dbclose( @db ) ;
        
        $stime = strtime( $mintime ) ;
        $etime = strtime( $maxtime ) ;

#
#  Check for anomolous channels
#
        
        $prob = unwanted_channels($sta,$prob) unless $opt_n;
        
#
#  identify gaps in baler seismic data
#

        $cmd  = "rt_daily_return ";
        $cmd .= "-v " if $opt_V ;
        $cmd .= "-t \"$stime\" -e \"$etime\" ";
        $cmd .= "-s \"sta =~/$sta/ && chan=~/[BL]H./\" $dbname $dbname";
        
        if ( ! &run_cmd( $cmd ) ) {
            $problems++ ;
        }
         
#
#  evaluate data return
#
        ( $prob, $problems ) = eval_data_return ( $sta, $prob, $problems ) unless $opt_n;       
        
        close(PROB);

 
#
#  clean up
#
        
        $subject = "TA $sta baler data net, station, channel problem";
        $cmd     = "rtmail -C -s '$subject' $pf{prob_mail} < /tmp/prob_$sta\_$$";
        
        if ( ! &run_cmd( $cmd ) ) {
            $problems++ ;
        }
        
        if  ( -d "$pf{balerprocbase}\/$sta" ) {
            $prob++ ;
            $problems++ ;
            elog_complain("\nProblem $problems
                           \n	$pf{balerprocbase}\/$sta already exists");
        } 
        
        $cmd  = "mv $pf{balerdirbase}\/$sta $pf{balerprocbase}";
        
        if ( ! &run_cmd( $cmd ) ) {
            $problems++ ;
        }
        
        unlink "/tmp/$sta\_return_$$" unless $opt_V;
        unlink "/tmp/tmp_miniseed2db\_$$" unless $opt_V;
        unlink "/tmp/prob_$sta\_$$" unless $opt_V;
        
    }
        
    $stime = strydtime(now());
    elog_notify ("completed successfully	$stime\n\n");

    if ($problems == 0 ) {
        $subject = sprintf("Success  $pgm  $host");
        elog_notify ($subject);
        &sendmail ( $subject, $opt_m ) if $opt_m ;
    } else { 
        $subject = "Problems - $pgm $host	$problems problems" ;
        &sendmail($subject, $opt_m) if $opt_m ; 
        elog_die("\n$subject") ;
    }
  
    
    exit(0);
}


sub unwanted_channels { # $prob = unwanted_channels( $sta, $prob );
    my ( $sta, $prob ) = @_ ; 
    my ( $chantmp, $fchan, $line, $nrows, $row, $statmp ) ;
    my ( @db, @dbschan, @dbsensor, @dbwfdisc );
        
    @db       = dbopen( $sta, 'r' );
    @dbschan  = dblookup( @db, 0, "schanloc", 0, 0 );
    @dbschan  = dbsubset( @dbschan, "chan !~ /UH./" );
    @dbsensor = dblookup( @db, 0, "sensor", 0, 0 );
    @dbwfdisc = dblookup( @db, 0, "wfdisc", 0, 0 );
    @dbschan  = dbjoin(   @dbschan, @dbwfdisc );
    @dbschan  = dbseparate( @dbschan, "schanloc" );
    @dbschan  = dbnojoin( @dbschan, @dbsensor );
    $nrows    = dbquery(  @dbschan, "dbRECORD_COUNT" );
    if ($nrows > 0) {
        $line = "\nDatabase problem\n	$sta schanloc has $nrows channels which do not join with sensor table";
        elog_complain($line);
        print PROB "$line\n";
        for ($row = 0; $row<$nrows; $row++) {
            $dbschan[3] = $row;
            ( $statmp, $fchan, $chantmp) = dbgetv( @dbschan, "sta", "fchan", "chan" );
            $line = "	sta	$statmp	fchan	$fchan	chan	$chantmp";
            elog_complain( $line ) ;
            print PROB "$line\n";
            $prob++;
        }
    }
    dbclose(@db);
        
    unlink( "$sta.snetsta" );
    unlink( "$sta.schanloc" );
    return ($prob);

}

sub eval_data_return { # $prob = eval_data_return ( $sta, $prob, $problems ) ;
    my ( $sta, $prob, $problems ) = @_ ;
    my ( $chan, $endtime, $line, $time ) ;
    my ( @db, @dbcd, @dbchanperf, @dbdeploy ) ;
    my ( %staperf );
    
    %staperf = (); 

    elog_notify(" ");
    $staperf{max_ave_perf}  = 0;
    $staperf{max_nperfdays} = 0;
    $staperf{max_datadays}  = 0;
    
    @db = dbopen( $sta, "r" );
    
    @dbdeploy = dblookup( @db, 0, "deployment", 0, 0 );
    @dbdeploy = dbsubset( @dbdeploy, "sta=~/$sta/" );
    if (! dbquery( @dbdeploy, "dbRECORD_COUNT" ) ) {
        $prob++;
        $problems++;
        $line = "$sta does not exist in deployment table!";
        elog_notify("	$line");
        print PROB "$line\n" ;
        return ($prob,$problems);
    }
    $dbdeploy[3] = 0;
    ( $time, $endtime ) = dbgetv( @dbdeploy, "time", "endtime" );
    $staperf{deploy_days} = int( $endtime/86400 ) - int( $time/86400. ) ;

    @db = dblookup( @db, 0, "chanperf", 0, 0 );
    if (! dbquery( @db, "dbTABLE_PRESENT" ) ) {
        $prob++;
        $problems++;
        $line = "$sta\.chanperf table has no records!";
        elog_notify("	$line");
        print PROB "$line\n" ;
        return ( $prob, $problems );
    }

    foreach $chan (qw( BHZ BHN BHE LHZ LHN LHE)) {
        @dbchanperf                = dbsubset( @db, "chan =~ /$chan/" );
        @dbcd                      = dbsubset( @dbchanperf, "perf > 0.0" );
        $staperf{$chan}{days}      = dbquery( @dbcd, "dbRECORD_COUNT" );
        $staperf{max_datadays}     = $staperf{$chan}{days} if ($staperf{$chan}{days} > $staperf{max_datadays});
        if ($staperf{$chan}{days} == 0.0) {
            $staperf{$chan}{ave_perf}  = 0.0;
        } else {
            $staperf{$chan}{ave_perf}  = (dbex_eval(@dbchanperf,"sum(perf)"))/$staperf{$chan}{days};
        }
        $staperf{max_ave_perf}     = $staperf{$chan}{ave_perf} if ($staperf{$chan}{ave_perf} > $staperf{max_ave_perf});
        @dbchanperf                = dbsubset( @dbchanperf, "perf == 100." );
        $staperf{$chan}{nperfdays} = dbquery( @dbchanperf, "dbRECORD_COUNT" );
        $staperf{max_nperfdays}    = $staperf{$chan}{nperfdays} if ($staperf{$chan}{nperfdays} > $staperf{max_nperfdays}) ;
        $line = sprintf("%s  %s	%4d days with 100%% data return	with %5.1f%% average daily data return on days with data",
                         $sta, $chan, $staperf{$chan}{nperfdays}, $staperf{$chan}{ave_perf} );
        elog_notify("	$line");
        print PROB "$line\n" ;
    }
    dbclose(@db);
        
        
    $line = sprintf("maximimum average data return on seismic channels is %5.1f%% - desire 95%% or better",
                     $staperf{max_ave_perf});
        
    if ($staperf{max_ave_perf} < 95.) {
        $prob++;
        print PROB "\n$line\n" ;
        $problems++ ;
        elog_complain("\nProblem $problems
                       \n	$line");
    } else {
        elog_notify("\n	$line\n\n");
    }
        
    $staperf{deploy_days} = 0.1 if ($staperf{deploy_days} < 0.1) ;

    $line = sprintf("%s	%4d deployment days	%4d days with data return	%5.1f%% of possible days\n	Check deployment table",
                     $sta, $staperf{deploy_days}, $staperf{max_datadays}, (100*$staperf{max_datadays}/$staperf{deploy_days}));
        
    if ( ( $staperf{deploy_days} * 1.05) < $staperf{max_datadays} ) {  # Don't worry if within 5%
        $prob++;
        print PROB "\n$line\n" ;
        $problems++ ;
        elog_complain("\nProblem $problems
                       \n	$line");
    } else {
        elog_notify("\n	$line\n\n");
    }
        
    %staperf = (); 
    return ($prob,$problems);
}