#
#   program needs:
#   DONE	fill gaps from rtsystem
#   DONE	obsip2orb
#
#   gap definition - time is first missing sample time
#                  - time + tgap is time of next sample
#                  - need to request gap.time to gap.time + tgap - 0.5*(1/samprate)
#   need to put dbmaster to $dbname station descriptor file before db2sync
#   check for overlaps
#
#   DONE	need to send ACE, LOG, OCF miniseed 
#			- not waveforms so not in wfdisc, obsip2dmc will not send them
#   DONE	need to get start and end times from msdd for these channels
#   DONE	need to add this information to sync file before sending.
#   DONE	metadata already exists.
#
#   DONE	need to put sync files in /anf/TA/products/dmc_sync/station_final/YYYY
#
#   DONE	need to put in a check that all expected channels operate from about the deployment dates
#
#   check for pffile existance
#   put option in to skip gap filling
#   graceful way to handle orb lag error?
#
#
    require "getopts.pl" ;
    use strict ;
    use Datascope ;
    use archive ;
    use timeslice ;
    use utilfunct ;
    use orb ;
    
    our ($pgm,$host);
    our ($opt_v,$opt_V,$opt_c,$opt_m,$opt_n,$opt_p);
    
{    #  Main program

    my ( $usage,$cmd,$subject,$verbose,$debug,$Pf,$problems,$problem_check );
    my ( $base, $chan, $comment, $dbname, $dbsize, $dep, $dir, $dirname, $endtime ) ;
    my ( $equip_install, $equip_remove, $gchan, $gsta, $line, $max, $maxtime, $mintime, $mlag ) ;
    my ( $mseedfile, $n, $net, $new, $nrows, $nsync, $old, $orb, $orbclient, $orbname, $orbsize ) ;
    my ( $pktid, $range, $ref, $row, $rtsta, $st1, $st2, $st3, $sta, $stime, $subset, $suf ) ;
    my ( $sync_dfile, $sync_dir, $sync_file, $table, $tgap, $thread, $time, $what, $who, $year );
    my ( @chans, @db, @dbdeploy, @dbdeployment, @dbdmcfiles, @dbgap, @dbgwf, @dbops, @dbscr ) ;
    my ( @dbscrdmc, @dbsize, @dbtest, @dbtmp, @dbwfchk, @dirs, @files, @laggards, @line ) ;
    my ( @msd, @mseedfiles, @pffiles, @rows );
    my ( %pf );

    $pgm = $0 ; 
    $pgm =~ s".*/"" ;
    elog_init($pgm, @ARGV);
    $cmd = "\n$0 @ARGV" ;
    
    if (  ! &Getopts('vVnc:m:p:') || @ARGV < 2 ) { 
        $usage  =  "\n\n\nUsage: $0  \n	[-v] [-V] [-n] \n" ;
        $usage .=  "	[-c orbclient] [-p pf] [-m mail_to]  \n" ;
        $usage .=  "	mseed_orb sta [sta1 sta2 ...]\n\n"  ; 
        
        elog_notify($cmd) ; 
        elog_die ( $usage ) ; 
    }
    
    &savemail() if $opt_m ; 
    elog_notify($cmd) ; 
    $stime = strydtime(now());
    chop ($host = `uname -n` ) ;
    elog_notify ("\nstarting execution on	$host	$stime");

    $Pf         = $opt_p || $pgm ;
    
    $orbname   = shift @ARGV;
    

    $opt_v      = defined($opt_V) ? $opt_V : $opt_v ;    
    $verbose    = $opt_v;
    $debug      = $opt_V;
    $orbclient  = $opt_c || "orbmsd2days" ;
    
    
    %pf = getparam($Pf, $verbose, $debug) ;
    makedir $pf{rt_sta_dir} if (! -d $pf{rt_sta_dir});

#
#  check system
#
    $problems = 0;
    $problems = system_check($problems);
    if ($problems) {
        $subject = "Problems - $pgm $host	Ran out of system resources" ;
        &sendmail($subject, $opt_m) if $opt_m ; 
        elog_die("\n$subject");
    }
#
#  open dbops and check that dbops dmcfiles table exists.
#
    @dbops         = dbopen($pf{dbops},"r+");
    @dbdeployment  = dblookup(@dbops,0,"deployment",0,0);
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
    if (! dbquery(@dbdeployment,"dbTABLE_PRESENT") ) {
        $problems++ ;
        elog_complain("\nProblem $problems
                       \n	database table $pf{dbops}.deployment does not exist!") ;
        $subject = "Problems - $pgm $host	dbops problem" ;
        &sendmail($subject, $opt_m) if $opt_m ; 
        elog_die("\n$subject");
    }

#
#  check orb
#
    $orb = orbopen($orbname,"r+");
    
    ($orbsize,$problems) = orbcheck($orb,$orbname,$orbclient,$problems);
    
    ($problems) = &orbprime($orbname,$problems) unless $opt_n;

    if ( $problems ) {
        elog_complain("\nProblem $problems
                       \n	Failed to prime orb $orbname ");
        $subject = "Problems - $pgm $host	" ;
        &sendmail($subject, $opt_m) if $opt_m ; 
        elog_die("\n$subject") ;
    }
    

#
#  process all new stations
#

    $nsync = 0;
    foreach $sta (@ARGV) {
        $subset = "comment =~ /$sta final baler data sent to DMC.*/";
        @dbtmp = dbsubset(@dbdmcfiles,$subset);
        if (dbquery(@dbtmp,"dbRECORD_COUNT")) {
            $dbtmp[3] = 0 ;
            $stime = strydtime(dbgetv(@dbtmp,"time"));
            elog_notify("$sta already processed at $stime") if $opt_v;
            next;
        }
        
        $stime = strydtime(now());
        $year  = epoch2str(now(),"%Y");
        elog_notify ("\nstarting processing station $sta");
    
#
#  perform database and file existance checks
#
                        
        $dirname    = "$pf{archivebase}\/$sta";
        $dbname     = "$pf{archivebase}\/$sta\/$sta";
        $sync_dir   = "$pf{sync_dir}\/$year";
        $sync_dfile = "$sta\_final.sync";
        $sync_file  = "$sync_dir/$sync_dfile";
        elog_notify("dirname	$dirname	dbname	$dbname") if $opt_V;
        elog_notify("	synch_dir	$sync_dir	sync_dfile	$sync_dfile	sync_file	$sync_file") if $opt_V;
        
        chdir($dirname);
        elog_notify("\nChanged directory to $dirname");
    
#
#  require descriptor file and wfdisc and gap tables to have the same dir and base names
#

        if (-e $dbname ) {
            @dbtest = dbopen($dbname,"r") ;
            $table = "wfdisc";
            if (! -e "$dbname.$table") {
                $problems++ ;
                elog_complain("\nProblem $problems
                               \n	database $dbname.$table does not exist!
                               \n	Need to run station_final_prep first!
                               \n	Skipping to next station") ;
                unless ($opt_n) {
                    dbclose(@dbtest);
                    next;
                }        
            }
            $table = "gap";
            if (! -e "$dbname.$table") {
                $problems++ ;
                elog_complain("\nProblem $problems
                               \n	database $dbname.$table does not exist!
                               \n	Need to run station_final_prep first!
                               \n	Skipping to next station") ;
                unless ($opt_n) {
                    dbclose(@dbtest);
                    next;
                }        
            }
            dbclose(@dbtest);
        } else {
            $problems++ ;
            elog_complain("\nProblem $problems
                            \n	database $dbname does not exist!
                            \n	Need to run station_final_prep first!
                            \n	Skipping to next station") ;
            next unless $opt_n;                
        }
    
#
#  verify sync file does not exist
#
        dbputv(@dbscrdmc,"dfile",$sync_dfile);
        @rows = dbmatches(@dbscrdmc,@dbdmcfiles,"dfile_hook","dfile");
        if ($#rows > -1) {
            elog_notify("$sta already processed");
            next;
        }

        if (-e $sync_file)  {
            $problems++ ;
            elog_complain("\nProblem $problems
                           \n	$sync_file exists!
                           \n	Station $sta has already been processed.
                           \n	Skipping to next station");
            next unless $opt_n;                
        }

        makedir($sync_dir) if (! -d $sync_dir );
        makedir("sync") if (! -d "sync" );


#
#  verify rt db exists
#
        $rtsta = "$pf{rt_sta_dir}/$sta";
        if (! -e $rtsta)  {
            $problems++ ;
            elog_complain("\nProblem $problems
                           \n	$rtsta does not exist!
                           \n	Station $sta has not been closed in the deployment table.
                           \n	Skipping to next station");
            next;                
        }

#
#  Build explicit gap filling request while verifying that gap is not filled
#
        if ( ! $opt_n ) {
            @db      = dbopen("tmp_gap_$sta\_$$","r+");
            @dbgwf   = dblookup(@db,0,"wfdisc",0,0);
        
            @dbgap   = dbopen($dbname,"r");
            @dbgap   = dblookup(@dbgap,0,"gap",0,0);
        
            @dbwfchk = dblookup(@dbgap,0,"wfdisc",0,0);
            @dbscr   = dblookup(@dbwfchk,0,0,0,"dbSCRATCH");
        
            $nrows   = dbquery(@dbgap,"dbRECORD_COUNT");
        
            $problem_check = $problems;

            for ($row = 0; $row<$nrows; $row++) {
                $dbgap[3] = $row;
                ($gsta,$gchan,$time,$tgap) = dbgetv(@dbgap,"sta","chan","time","tgap");
                $endtime = $time + $tgap - 0.004;
                dbputv(@dbscr,"sta",$gsta,"chan",$gchan,"time",$time,"endtime",$endtime);
                @rows = dbmatches(@dbscr,@dbwfchk,"overlap_$sta","sta","chan","time::endtime");
                if ($#rows > -1) {
                    $problems++ ;
                    elog_complain("\nProblem $problems");
                    printf STDERR "	$sta	$chan	already has gap filled data between	%s	and	%s\n",
                                strydtime($time),strydtime($endtime);
                }
                dbaddv(@dbgwf,"sta",    $gsta,
                              "chan",   $gchan,
                              "time",   $time,
                              "endtime",$endtime) if ($problem_check == $problems );
            }

            if ( $problem_check != $problems && ! $opt_n) {
                $subject = "Problems - $pgm $host	Gaps already filled in $sta" ;
                &sendmail($subject, $opt_m) if $opt_m ; 
                elog_die("\n$subject") ;
            }
         
#  Check the aggregate size of data files

            @dbsize = dbsort(@dbwfchk,"dir","dfile","-u");
            $nrows  = dbquery(@dbsize,"dbRECORD_COUNT");
        
            $dbsize = 0;
            for ($row = 0; $row<$nrows; $row++) {
                $dbsize[3] = $row;
                $dbsize += -s dbextfile(@dbsize);
            }
        
            $line    = "dbname $dbname	Total Bytes - 	$dbsize";
            $comment = "$sta final baler data sent to DMC     Total Bytes \- $dbsize";
            open(RTSTA,"> /tmp/sta");
            print RTSTA "$line \n";
            close(RTSTA);

		    elog_notify($line);        

#  Check the min and max times of all channels as requested by Jennifer.

            $mintime  = dbex_eval(@dbwfchk,"min(time)");
            $maxtime  = dbex_eval(@dbwfchk,"max(endtime)");

#  find file directories for non-wf miniseed data

            @dbwfchk  = dbsubset(@dbwfchk,"chan =~ /$pf{non_wf_chan_proxy}/");
            @dbwfchk  = dbsort(@dbwfchk,"dir","-u") ; 

            @dirs = ();
            $nrows = dbquery(@dbwfchk,"dbRECORD_COUNT");
        
            for ($row = 0; $row<$nrows; $row++) {
                $dbwfchk[3] = $row;
                ($dir,$base,$suf) = parsepath(dbextfile(@dbwfchk));
                elog_notify("dir	$dir") if $opt_V;
                push(@dirs,$dir);
            }
            elog_notify("dirs	@dirs") if $opt_V;
        
            dbclose(@db);
            dbclose(@dbgap);
        }
        
#
#  Build rt station wfdisc (is this in the correct wf naming format????)
#

        if ($nrows > 0 || $opt_n) {
            
            $cmd  = "trexcerpt ";
            $cmd  .= "-v  " if $opt_V;
            $cmd  .= "-a -D -E -m explicit -W $rtsta tmp_gap_$sta\_$$.wfdisc $dbname ";
        
            if  (! $opt_n && ($nrows > 0)) {
                elog_notify("$cmd");        
                $problem_check = $problems;
                $problems = run($cmd,$problems) ;
                if ( $problem_check != $problems ) {
                    elog_complain("\n	Skipping to next station");
                    next unless $opt_n; 
                }
            } else {
                elog_notify("skipping $cmd") ;
            }
        
        }
        
        unlink("tmp_gap_$sta\_$$.wfdisc") unless $opt_V;
        unlink("tmp_gap_$sta\_$$.lastid") unless $opt_V;
                 
#
#  Transfer data to DMC using obsip2orb
#

        $cmd  = "obsip2orb ";
        $cmd  .= "-v " if $opt_V;
        $cmd  .= "-n " if $opt_n;
        $cmd  .= "-X $orbname $dbname" if ($orbsize >= $dbsize) ;
        $cmd  .= "-c $orbclient $orbname $dbname" if ($orbsize < $dbsize) ;
        $cmd  .= "> /tmp/tmp_obsip2orb\_$sta\_$$ 2>&1 " ;
        
        if  (! $opt_n ) {
            elog_notify("$cmd");
            $problem_check = $problems;
            $problems = run($cmd,$problems) ;
            if ( $problem_check != $problems ) {
                $subject = "Problems - $pgm $host	obsip2orb $sta" ;
                &sendmail($subject, $opt_m) if $opt_m ; 
                elog_die("\n$subject") ;
            }
        } else {
            elog_notify("skipping $cmd") ;
        }
        
                 
#
#  Transfer non-waveform miniseed data to DMC 
#

        @files = ();
        $ref       = pfget( $Pf, "non_wf_chan" );
        @chans     = @$ref ;
        $chan = "(" . join("|",@chans) . ")";

#
#  make sure wait_match specified properly in miniseed2orb.pf
#
        @pffiles = pffiles("miniseed2orb");
        elog_notify("pffiles	@pffiles");
        unlink("miniseed2orb_sta_final.pf") if (-e "miniseed2orb_sta_final.pf");
        $cmd = "cp $pffiles[0] miniseed2orb_sta_final.pf";
        system($cmd);
                    
        if ($dbsize > $orbsize) {
            open(MS,">>miniseed2orb_sta_final.pf");
            print MS "wait_match $orbclient\n";
            close(MS);

        } else {
            open(MS,">>miniseed2orb_sta_final.pf");
            print MS "wait_match \n";
            close(MS);
            elog_notify("running in expert mode - no wait_match ");
        }

#        
#  Assumes that non-waveform miniseed files are in the same directory as $pf{non_wf_chan_proxy}
#  Get list of all non-waveform miniseed files
#
        
        foreach $dir (@dirs) {
            elog_notify("	dir	$dir") if $opt_V;
            opendir(DIR,$dir);
            @mseedfiles = sort( grep { /.*_$sta\_$chan.*/  } readdir(DIR) );
            elog_notify("		chan	$chan	@mseedfiles") if $opt_V;
            closedir(DIR);
            foreach $mseedfile (@mseedfiles) {
                push(@files,"$dir\/$mseedfile");
            }
        }

#        
#  Process each non-waveform miniseed file
#
        open(SYNC,">sync/tmp_sync");
        
        foreach $mseedfile (@files) {
        
#
#  Get start and end times of mseed file
#
            $cmd = "msdd $mseedfile";
            elog_notify("	$cmd") if $opt_V;
            
            open(MSD, "$cmd |");
            @msd = <MSD>;
            close MSD;
            
            @msd = grep { /$sta/ } @msd ;
            
            elog_notify("	$msd[1]") if $opt_V;
            @line = split " ", $msd[1];
            $net = $line[0] ;
            $chan = $line[2] ;
            splice(@line,0,3);
            $#line -= 2;
            $st1 = epoch2str(str2epoch(join " ",@line),"%Y,%j,%H:%M:%S.%u");
            elog_notify("	$st1") if $opt_V;
            
            elog_notify("	$msd[$#msd]") if $opt_V;
            @line = split " ", $msd[$#msd];
            splice(@line,0,3);
            $#line -= 2;
            $st2 = epoch2str(str2epoch(join " ",@line),"%Y,%j,%H:%M:%S.%u");
            elog_notify("	$st2") if $opt_V;
            
            @msd = ();       
            
#
#  Send data to export orb
#
            $cmd = "miniseed2orb -p miniseed2orb_sta_final -u $mseedfile $orbname";
            if  (! $opt_n ) {
                elog_notify("$cmd");
                $problem_check = $problems;
                $problems = run($cmd,$problems) ;
                if ( $problem_check != $problems ) {
                    $subject = "Problems - $pgm $host	miniseed2orb $mseedfile" ;
                    &sendmail($subject, $opt_m) if $opt_m ; 
                    elog_die("\n$subject") ;
                }
            } else {
                elog_notify("skipping $cmd") ;
            }

            $st3 = epoch2str(now(),"%Y,%j");
            print SYNC "$net|$sta||$chan|$st1|$st2||||||||||$st3\n";
            
        }
        
        close SYNC;
#
#  Make DMC sync file
#
    
        $cmd = "db2sync -h $dbname sync/$sync_dfile";

        if  ( ! $opt_n ) {
            elog_notify("$cmd");        
            $problems = run($cmd,$problems) ;
        } else {
            elog_notify("skipping $cmd") ;
        } 
        
        $cmd = "cat  sync/tmp_sync >> sync/$sync_dfile";

        if  ( ! $opt_n ) {
            elog_notify("$cmd");        
            $problems = run($cmd,$problems) ;
        } else {
            elog_notify("skipping $cmd") ;
        } 
    
#
#  Verify start and end times in deployment table
#
        @dbdeploy    = dbsubset(@dbdeployment,"sta=~/$sta/");
        $dbdeploy[3] = 0;
        ($time,$endtime,$equip_install,$equip_remove) = 
                                dbgetv(@dbdeploy,"time","endtime","equip_install","equip_remove");
                                
        $dep = 0;
        open(DEP,"> /tmp/deploy");
        if ($mintime < $time) {
            $st1 = strydtime($mintime);
            $st2 = strydtime($time);
            $line =  "Deployment table time field may need changing ";
            $line .= "-	$sta db time $st2	new suggested time $st1";
            print DEP "$line\n";
            elog_notify($line);
            $dep++;
        }
        if ($mintime < $equip_install) {
            $st1 = strydtime($mintime);
            $st2 = strydtime($time);
            $line = "Deployment table equip_install field may need changing ";
            $line .= "-	$sta db equip_install $st2	new suggested equip_install $st1";
            print DEP "$line\n";
            elog_notify($line);
            $dep++;
        }
        if ($maxtime > $endtime) {
            $st1 = strydtime($maxtime);
            $st2 = strydtime($endtime);
            $line = "Deployment table endtime field may need changing ";
            $line .= "-	$sta db endtime $st2	new suggested endtime $st1";
            print DEP "$line\n";
            elog_notify($line);
            $dep++;
        }
        if ($maxtime > $equip_remove) {
            $st1 = strydtime($maxtime);
            $st2 = strydtime($equip_remove);
            $line = "Deployment table equip_remove field may need changing ";
            $line = "-	$sta db equip_remove $st2	new suggested equip_remove $st1";
            print DEP "$line\n";
            elog_notify($line);
            $dep++;
        }
                
        close(DEP);
        
        $subject = "ANF TA Deployment table change - $sta";
        $cmd     = "rtmail -C -s '$subject' $pf{deploy_mail} < /tmp/deploy";
        
        if  ( ! $opt_n ) {
            elog_notify("$cmd") if $dep ;        
            $problems = run($cmd,$problems) if $dep ;
        } else {
            elog_notify("skipping $cmd") if $dep ;
        } 
 
#
#  wait until orblag value become acceptable
#

        if ( $orb < 0 and ! $opt_n) {
            $problems++ ;
            elog_complain("\nProblem $problems
                           \n	Failed to open orb $orbname for orblag check");
            $subject = "Problems - $pgm $host	" ;
            &sendmail($subject, $opt_m) if $opt_m ; 
            elog_die("\n$subject") ;
        }
    
        $mlag = 1.0;
        $n = 0;
        while ($mlag > 0.01 ) {
            ($old,$new,$max,$range,@laggards) = orblag($orb,"orbmsd2days",0);
            elog_notify("	orbmsd2days	$old	$new	$max	$range	@laggards") if $opt_V;
            ($mlag, $thread, $pktid, $who, $what) =  split (' ', $laggards[0], 5) ;
            elog_notify("	orbmsd2days	$mlag") unless ($n %= 10) ;
            $n++;
            sleep 60 unless $opt_n;
        }
        
        elog_notify("Sleeping 5 minutes");
        sleep 300 unless $opt_n;
#
#  Transfer DMC sync file
#

#
#  make sure wait_match specified properly in miniseed2orb.pf
#
        @pffiles = pffiles("orbxfer2");
        elog_notify("pffiles	@pffiles");
        unlink("orbxfer2.pf") if (-e "orbxfer2.pf");
        $cmd = "pfcp -d orbxfer2 .";
        system($cmd);

        open(OXF,">>orbxfer2.pf");
        print OXF "wait_match \n";
        close(OXF);
        elog_notify("running in expert mode - no wait_match ");

        $cmd = "orbxfer2 -N sync sync/$sync_dfile  $orbname";

        if  ( ! $opt_n ) {
            elog_notify("$cmd");        
            $problems = run($cmd,$problems) ;
            $problems = run($cmd,$problems) unless $nsync;            
        } else {
            elog_notify("skipping $cmd") ;
        } 
            
        $cmd = "mv sync/$sync_dfile $sync_file";

        if  ( ! $opt_n ) {
            elog_notify("$cmd");        
            $problems = run($cmd,$problems) ;
        } else {
            elog_notify("skipping $cmd") ;
        } 

        dbaddv(@dbdmcfiles,"time",now(),
                   "comment",$comment,
                   "dir", $sync_dir,
                   "dfile", $sync_dfile,
                   "orb",$orbname,
                   "auth","$host:sftd") unless $opt_n;
               
        $subject = "$pgm $host	$sta transmission completed" ;
        $cmd     = "rtmail -s '$subject' $opt_m < /tmp/sta";
        if  ( ! $opt_n && $opt_m) {
            elog_notify("$cmd");        
            $problems = run($cmd,$problems) ;
        } else {
            elog_notify("skipping $cmd") if $opt_m;
        } 

        unlink("sync/tmp_sync") unless $opt_V;
        unlink("miniseed2orb_sta_final.pf") unless $opt_V;
        unlink("orbxfer2.pf") unless $opt_V;
        unlink ("/tmp/tmp_obsip2orb\_$sta\_$$") unless $opt_V;
        unlink ("/tmp/sta") unless $opt_V;
        unlink ("/tmp/deploy") unless $opt_V;
        $nsync++;
       
    }
    
    dbclose(@dbops);
    
    orbclose($orb);
        
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


sub orbcheck { # ($orbsize,$problems) = orbcheck($orb,$orbname,$orbclient,$problems);
    my ($orb,$orbname,$orbclient,$problems) = @_ ;
    my ($subject,$orbstat,$orbsize,$orbsize);
    my ($when,$found_sf,$found_oc,$found_xf,$client);
    my (@clients);
    
    if ( $orb < 0 and ! $opt_n) {
        $subject = "Problems - $pgm $host	Failed to open orb $orbname" ;
        &sendmail($subject, $opt_m) if $opt_m ; 
        elog_die("\n$subject") ;
    }
    
    $orbstat = orbstat($orb);
    $orbsize = $orbstat->maxdata;
    elog_notify("orbsize	$orbsize") if $opt_v;
    
#
#  make sure other programs are not running
#    

    ($when,@clients) = orbclients($orb) ;
    
    $found_sf = 0;
    foreach $client (@clients) {
        if ($client->what =~ /$pgm/) {
            elog_notify(sprintf "%-8s %s\n", $client->who, $client->what) if $opt_v;
            $found_sf++;
        }
        if ($client->what =~ /miniseed2orb/) {
            elog_notify(sprintf "%-8s %s\n", $client->who, $client->what) if $opt_v;
            elog_complain("\n	A miniseed2orb is currently connected to orb $orbname
                       \n	Restart this instance of $pgm when miniseed2orb is completed");
            $subject = "Problems - $pgm $host	$problems problems" ;
            &sendmail($subject, $opt_m) if $opt_m ; 
            elog_die("\n$subject") ;
        }
    }
    if ($found_sf > 1 ) {
        elog_complain("\n	Another $pgm is currently connected to orb $orbname
                       \n	Restart this instance of $pgm when other instance is completed");
        $subject = "Problems - $pgm $host	$problems problems" ;
        &sendmail($subject, $opt_m) if $opt_m ; 
        elog_die("\n$subject") ;
    }

#
#  make sure orbminiseed2days and orbxfer2 are attached!!!!!! 
#    

    ($when,@clients) = orbclients($orb) ;
    
    $found_oc = 0;
    $found_xf = 0;
    foreach $client (@clients) {
        if ($client->what =~ /$orbclient/) {
            elog_notify(sprintf "%-8s %s\n", $client->who, $client->what) if $opt_v;
            $found_oc = 1 ;
        }
        if ($client->what =~ /orbxfer2/) {
            elog_notify(sprintf "%-8s %s\n", $client->who, $client->what) if $opt_v;
            $found_xf = 1 ;
        }
    }
    
    unless ($found_oc) {
        elog_complain("$orbclient not currently connected to orb $orbname");
        $subject = "Problems - $pgm $host	$problems problems" ;
        &sendmail($subject, $opt_m) if $opt_m ; 
        elog_die("\n$subject") ;
    }

    unless ($found_xf) {
        elog_complain("orbxfer2 not currently connected to orb $orbname");
        $subject = "Problems - $pgm $host	$problems problems" ;
        &sendmail($subject, $opt_m) if $opt_m ; 
        elog_die("\n$subject") ;
    }
    return($orbsize,$problems);
}

sub orbprime { # ($problems) = &orbprime($orbname,$problems);
    my ($orbname,$problems) = @_ ;
    my ($cmd);
    
    unlink "/tmp/JUNK.mseed"      if (-e "/tmp/JUNK.mseed");
    unlink "JUNK.lastid"          if (-e "JUNK.lastid");
    unlink "JUNK.wfdisc"          if (-e "JUNK.wfdisc");
    unlink "JUNK.pf"              if (-e "JUNK.pf");
    unlink "miniseed2orb_JUNK.pf" if (-e "miniseed2orb_JUNK.pf");

    $cmd = "trsignal -d sd -r 40 -s JUNK -w /tmp/JUNK.mseed JUNK";
    
    elog_notify("$cmd") if $opt_v;        
    $problems = run($cmd,$problems) ;
        
    unlink "JUNK.lastid";
    unlink "JUNK.wfdisc";
   
    open (PF, ">JUNK.pf");
    print PF "net     \&Tbl\{ \n";
    print PF "	.*	XX \n";
    print PF "\}\n";
    close(PF);

    $cmd = "fix_miniseed -p JUNK /tmp/JUNK.mseed > /tmp/fix_miniseed_$$ 2>&1 " ;
    elog_notify("$cmd") if $opt_v;        
    $problems = run($cmd,$problems) ;
    unlink "JUNK.pf";
    
    $cmd = "pfcp miniseed2orb miniseed2orb_JUNK > /tmp/pfcp_JUNK_$$ 2>&1 ";
    elog_notify("$cmd") if $opt_v;        
    $problems = run($cmd,$problems) ;
        
    open(MS,">>miniseed2orb_JUNK.pf");
    print MS "wait_match \n";
    close(MS);
            
    $cmd = "miniseed2orb -p miniseed2orb_JUNK -u /tmp/JUNK.mseed $orbname > /tmp/miniseed2orb_JUNK_$$ 2>&1";
    elog_notify("$cmd") if $opt_v;        
    $problems = run($cmd,$problems) ;
        
    unlink "/tmp/JUNK.mseed";
    unlink "miniseed2orb_JUNK.pf";
    unlink "/tmp/fix_miniseed_$$";
    unlink "/tmp/pfcp_JUNK_$$";
    unlink "/tmp/miniseed2orb_JUNK_$$";

    return($problems);
}