#
#   program needs:
#
#   check overlaps with realtime data
#
#
    use Getopt::Std ;
    use strict ;
    use Datascope ;
    use archive;
    use timeslice ;
    use utilfunct ;
    use orb ;
    
    our ($pgm,$host);
    our ($opt_v,$opt_V,$opt_m,$opt_n,$opt_p);
    
{    #  Main program

    my ( $Pf, $chan, $chantmp, $cmd, $dbname, $decert_time, $dep, $dirname, $endtime ) ;
    my ( $equip_install, $equip_remove, $line, $maxtime, $mintime, $nrows, $prob, $problems ) ;
    my ( $ptmp, $row, $st1, $st2, $sta, $statmp, $stime, $subject, $subset, $table, $time ) ;
    my ( $totdays, $usage );
    my ( @db, @dbbh, @dbcd, @dbchanperf, @dbdeploy, @dbdeptmp, @dbdmcfiles, @dbops, @dbschan ) ;
    my ( @dbscrdmc, @dbtmp, @dbwf, @dbwfdisc );
    my ( %pf, %staperf );

    $pgm = $0 ; 
    $pgm =~ s".*/"" ;
    elog_init($pgm, @ARGV);
    $cmd = "\n$0 @ARGV" ;
    
    if (  ! getopts('vVnm:p:') || @ARGV < 1 ) { 
        $usage  =  "\n\n\nUsage: $0  \n	[-v] [-V] [-n] \n" ;
        $usage .=  "	[-p pf] [-m mail_to]  \n" ;
        $usage .=  "	 sta [sta1 sta2 ...]\n\n"  ; 
        
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

#
#  open dbops and check that dbops dmcfiles table exists.
#
    @dbops         = dbopen($pf{dbops},"r+");
    @dbdeploy      = dblookup(@dbops,0,"deployment",0,0);
    @dbdmcfiles    = dblookup(@dbops,0,"dmcfiles",0,0);
    @dbscrdmc      = dblookup(@dbdmcfiles,0,0,0,"dbSCRATCH");
    if (! dbquery(@dbdmcfiles,"dbTABLE_PRESENT") ) {
        $problems++ ;
        elog_complain("\nProblem $problems
                       \n	database table $pf{dbops}.dmcfiles does not exist!") ;
        $subject = "Problems - $pgm $host	dbops problem" ;
        &sendmail($subject, $opt_m) if $opt_m ; 
        elog_die("\n$subject");
    }

#   subset for unprocessed data

#
#  process all new stations
#

    $table = "wfdisc";
    foreach $sta (@ARGV) {
        $subset = "comment =~ /$sta final baler data sent to DMC.*/";
        @dbtmp = dbsubset(@dbdmcfiles,$subset);
        if (dbquery(@dbtmp,"dbRECORD_COUNT")) {
            $dbtmp[3] = 0 ;
            $stime = strydtime(dbgetv(@dbtmp,"time"));
            elog_notify("\n$sta already sent to DMC at $stime, skipping verification") if $opt_v;
            next;
        }
        
        $stime = strydtime(now());
        elog_notify ("\nstarting processing station $sta\n\n");
    
#
#  perform database and file existance checks
#
                        
        $dirname   = "$pf{archivebase}\/$sta";
        $dbname    = "$pf{archivebase}\/$sta\/$sta";
        
        chdir($dirname);
        elog_notify("\nChanged directory to $dirname") if $opt_v;
    
#
#  require descriptor file and wfdisc and gap tables to have the same dir and base names
#

        if (-e $dbname ) {
            $ptmp = $problems;
            $table = "wfdisc";
            if (! -e "$dbname.$table") {
                $problems++ ;
                elog_complain("\nProblem $problems
                               \n	database $dbname.$table does not exist!
                               \n	Need to run station_final_prep first!
                               \n	Skipping to next station") ;
            }
            $table = "gap";
            if (! -e "$dbname.$table") {
                $problems++ ;
                elog_complain("\nProblem $problems
                               \n	database $dbname.$table does not exist!
                               \n	Need to run station_final_prep first!
                               \n	Skipping to next station") ;
            }
            next if ($ptmp != $problems); 
        } else {
            $problems++ ;
            elog_complain("\nProblem $problems
                            \n	database $dbname does not exist!
                            \n	Need to run station_final_prep first!
                            \n	Skipping to next station") ;
            next;
        }
        
#
#  Open db
#
        $prob = 0;
        open(PROB,"> /tmp/prob_$sta\_$$");
        
        @db       = dbopen($sta,'r');
        @dbschan  = dblookup(@db,0,"schanloc",0,0);
        @dbwfdisc = dblookup(@db,0,"wfdisc",0,0);

#
#  Find start time and end times
#

        @dbbh     = dbsubset(@dbwfdisc,"sta =~/$sta/ && chan =~ /[BL]H[ZNE]/");
        $mintime  = int(dbex_eval(@dbbh,"min(time)"));
        $maxtime  = int(dbex_eval(@dbbh,"max(endtime)") + 1);
        $totdays  = int($maxtime/86400) - int($mintime/86400); 
        
#
#  Check for anomolous channels
#
        
        @dbwfdisc = dbnojoin(@dbwfdisc,@dbschan);
        @dbwfdisc = dbsort(@dbwfdisc,"sta","chan","-u");
        @dbwfdisc = dbsubset(@dbwfdisc,"(sta !~ /$sta/ || (sta =~/$sta/ && chan !~ /UH./))");
        $nrows    = dbquery(@dbwfdisc,"dbRECORD_COUNT");
        
        if ($nrows > 0) {
            $line = "\nDatabase problem\n	$sta wfdisc has $nrows which do not join with schanloc table";
            elog_complain($line);
            print PROB "$line\n";
            for ($row = 0; $row<$nrows; $row++) {
                $dbwfdisc[3] = $row;
                ($statmp,$chantmp) = dbgetv(@dbwfdisc,"sta","chan");
                $line = "	sta	$statmp	chan	$chantmp";
                elog_complain($line) ;
                print PROB "$line\n";
                $prob++;
            }
        }
        dbclose(@db);
        
#
#  Verify start and end times in deployment table
#

        @dbdeptmp = dbsubset(@dbdeploy,"sta=~/$sta/");
        $dbdeptmp[3] = 0;
        ($time,$endtime,$equip_install,$equip_remove,$decert_time) = 
                                dbgetv(@dbdeptmp,"time","endtime","equip_install","equip_remove","decert_time");
                             
        %staperf = ();
        $endtime = now() if ($endtime > now());
        $staperf{deploy_days} = int($endtime/86400) - int($time/86400.);
        
        $staperf{deploy_days} = $totdays if ($totdays > $staperf{deploy_days});
                                
        $dep = 0;
        open(DEP,"> /tmp/deploy_$sta\_$$");
        print DEP "results from $pgm\n\n";
        if ($mintime < $time) {
            $st1 = strydtime($mintime);
            $st2 = strydtime($time);
            $line = "Deployment table time field change -\n	$sta old time		$st2	new	$st1\n";
            print DEP "$line\n";
            elog_notify($line);
            dbputv(@dbdeptmp,"time",$mintime);
            $dep++;
        }
        if ($mintime < $equip_install) {
            $st1 = strydtime($mintime);
            $st2 = strydtime($equip_install);
            $line = "Deployment table equip_install field change -\n	$sta old equip_install	$st2	new	$st1\n";
            print DEP "$line\n";
            elog_notify($line) if $opt_v;
            dbputv(@dbdeptmp,"equip_install",$mintime);  
            $dep++;
        }
        if ($maxtime > $endtime) {
            $st1 = strydtime($maxtime);
            $st2 = strydtime($endtime);
            $line = "Deployment table endtime field change -\n	$sta old endtime	$st2	new	$st1\n";
            print DEP "$line\n";
            elog_notify($line);
            dbputv(@dbdeptmp,"endtime",$maxtime);
            $dep++;
        }
        if ($maxtime > $equip_remove) {
            $st1 = strydtime($maxtime);
            $st2 = strydtime($equip_remove);
            $line = "Deployment table equip_remove field change -\n	$sta old equip_remove	$st2	new	$st1\n";
            print DEP "$line\n";
            elog_notify($line);
            dbputv(@dbdeptmp,"equip_remove",$maxtime);
            $dep++;
        }
        if ($endtime > now()) {
            $st1 = strydtime($endtime);
            $line = "Deployment table endtime field 	$sta	$st1\n";
            print DEP "$line\n";
            elog_notify($line);
            $dep++;
        }
        if ($equip_remove > now()) {
            $st1 = strydtime($equip_remove);
            $line = "Deployment table equip_remove field 	$sta	$st1\n";
            print DEP "$line\n";
            elog_notify($line);
            $dep++;
        }
        if ($decert_time > now()) {
            $st1 = strydtime($decert_time);
            $line = "Deployment table decert_time field 	$sta	$st1\n";
            print DEP "$line\n";
            elog_notify($line);
            $dep++;
        }
        
        close(DEP);
        
        $subject = "ANF TA Deployment table change - $sta";
        $cmd     = "rtmail -C -s '$subject' $pf{deploy_mail} < /tmp/deploy_$sta\_$$";
        
        if ( ! &run_cmd( $cmd ) ) {
            $problems++ ;
        }
         
#
#  evaluate data return
#
        
        $line = "days with data	days w/ 100%	days w/o data	data return		start				end";
        elog_notify("			$line");
        print PROB "		$line\n" ;
        
        $staperf{max_ave_perf}    = 0;
        $staperf{max_nperfdays}   = 0;
        $staperf{max_datadays}    = 0;
        $staperf{max_no_datadays} = 0;
        $staperf{min_ave_perf}    = 100.;
        $staperf{min_nperfdays}   = 10000;
        $staperf{min_datadays}    = 10000;
        $staperf{min_no_datadays} = 10000;
        @db       = dbopen($sta,"r");
        @db       = dblookup(@db,0,"chanperf",0,0);
        @dbwfdisc = dblookup(@db,0,"wfdisc",0,0);
        foreach $chan (qw( BHZ BHN BHE LHZ LHN LHE)) {
            @dbchanperf                = dbsubset(@db,"chan =~ /$chan/");
            @dbcd                      = dbsubset(@dbchanperf,"perf > 0.0");
            $staperf{$chan}{days}      = dbquery(@dbcd,"dbRECORD_COUNT");
            $staperf{$chan}{days_nd}   = $staperf{deploy_days} - $staperf{$chan}{days};
            @dbcd                      = dbsubset(@dbchanperf,"perf == 100.");
            $staperf{$chan}{nperfdays} = dbquery(@dbcd,"dbRECORD_COUNT");

            $staperf{$chan}{ave_perf}  = (dbex_eval(@dbchanperf,"sum(perf)"))/$staperf{deploy_days};
            
            $staperf{max_datadays}     = $staperf{$chan}{days}      if ($staperf{$chan}{days}      > $staperf{max_datadays});
            $staperf{max_ave_perf}     = $staperf{$chan}{ave_perf}  if ($staperf{$chan}{ave_perf}  > $staperf{max_ave_perf});
            $staperf{max_nperfdays}    = $staperf{$chan}{nperfdays} if ($staperf{$chan}{nperfdays} > $staperf{max_nperfdays}) ;
            $staperf{max_no_datadays}  = $staperf{$chan}{days_nd}   if ($staperf{$chan}{days_nd}   > $staperf{max_no_datadays}) ;
            
            $staperf{min_datadays}     = $staperf{$chan}{days}      if ($staperf{$chan}{days}      < $staperf{min_datadays});
            $staperf{min_ave_perf}     = $staperf{$chan}{ave_perf}  if ($staperf{$chan}{ave_perf}  < $staperf{min_ave_perf});
            $staperf{min_nperfdays}    = $staperf{$chan}{nperfdays} if ($staperf{$chan}{nperfdays} < $staperf{min_nperfdays}) ;
            $staperf{min_no_datadays}  = $staperf{$chan}{days_nd}   if ($staperf{$chan}{days_nd}   < $staperf{min_no_datadays}) ;

            @dbwf                      = dbsubset(@dbwfdisc,"chan =~ /$chan/");
            $mintime                   = dbex_eval(@dbwf,"min(time)");
            $maxtime                   = dbex_eval(@dbwf,"max(endtime)");    
            
            $line = sprintf("%s  %s	%4d		%4d		%4d		%5.1f%%		%s	%s",
                                $sta,$chan,$staperf{$chan}{days},$staperf{$chan}{nperfdays},$staperf{$chan}{days_nd},
                                $staperf{$chan}{ave_perf},strydtime($mintime),strydtime($maxtime));
            elog_notify("	$line");
            print PROB "$line\n" ;
        }
        dbclose(@db);
        
        $line = sprintf("minimum	%4d		%4d		%4d		%5.1f%%",
                         $staperf{min_datadays},$staperf{min_nperfdays},$staperf{min_no_datadays},$staperf{min_ave_perf});
        elog_notify(" ");
        elog_notify("		$line");
        print PROB "\n	$line\n" ;
        
        $line = sprintf("maximum %4d		%4d		%4d		%5.1f%%",
                         $staperf{max_datadays},$staperf{max_nperfdays},$staperf{max_no_datadays},$staperf{max_ave_perf});
        elog_notify("		$line");
        print PROB "	$line\n" ;
        
        elog_notify(" ");
        $line = sprintf("%s	%4d deployment days	%4d days with data return	%5.1f%% of possible days",
                         $sta,$staperf{deploy_days},$staperf{max_datadays},(100*$staperf{max_datadays}/$staperf{deploy_days}));
        elog_notify("	$line");
        print PROB "\n$line\n" ;
        
        if ($staperf{min_ave_perf} < 95.) {
            $prob++;
            $line = "max_ave_perf < 95.";
            print PROB "\nProblem $prob\n	$line\n" ;
            $problems++ ;
            elog_complain("\nProblem $problems
                           \n			$line");
        }
                
        close(PROB);

        elog_notify("\n\n");
 
#
#  send email if problems are found 
#
        
        $subject = "TA $sta baler data problem";
        $cmd     = "rtmail -C -s '$subject' $pf{prob_mail} < /tmp/prob_$sta\_$$";
        
        if ( ! &run_cmd( $cmd ) ) {
            $problems++ ;
        }
        
        unlink "/tmp/$sta\_return_$$" unless $opt_V;
        unlink "/tmp/tmp_miniseed2db\_$$" unless $opt_V;
        unlink "/tmp/prob_$sta\_$$" unless $opt_V;
        unlink "/tmp/deploy_$sta\_$$" unless $opt_V;        
        
        if ($prob) {
            makedir($pf{purgatory});
            $cmd = "mv $dirname $pf{purgatory}";

            if ( ! &run_cmd( $cmd ) ) {
                $problems++ ;
            }
        }
        %staperf = ();        
    }
    
    dbclose(@dbops);
        
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

