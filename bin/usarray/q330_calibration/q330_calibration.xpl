#
#   program needs:
#

    use Getopt::Std ;
    use strict ;
    use Datascope ;
    use archive ;
    use utilfunct ;
    use orb ;
    
    our ($opt_v,$opt_V,$opt_d,$opt_i,$opt_m,$opt_N,$opt_n,$opt_o,$opt_p,$opt_t,$opt_2,$opt_3);
    our ($pgm,$host);
    our (%pf,%q330);
    
{    #  Main program

    my ($usage,$cmd,$problems,$subject,$debug,$verbose);
    my ($orb,$orbname,$db,$stime,$Pf,$regex,$subset,$target);
    my ($sta,$dlsta,$snmodel,$chident,$time,$endtime,$sleep,$maxsleep);
    my ($offset,$start,$rows,$trow,$row,$irow,$drow,$nocal,$nrowe,$nrown,$stepe,$stepn);
    my (@db,@dbdlcalwf,@dbdlsensor,@dbstaq330,@dbj,@dbmone,@dbmonn,@dbje,@dbjn) ;
    my (@dbdlcale,@dbdlcaln,@dbscrcal);
    my (@sta_0x2, @sta_0x4, @rows);

    $pgm = $0 ; 
    $pgm =~ s".*/"" ;
    elog_init($pgm, @ARGV);
    $cmd = "\n$0 @ARGV" ;

    $problems = 0;
    $usage    =  "\n\n\nUsage: $0  \n	[-v] [-V] [-n]  [-2] [-3]  \n" ;
    $usage   .=  "	[-i] [-N [-d no_calib_days]]\n" ;
    $usage   .=  "	[-o duration_offset_fraction] [-t start_time]  [-p pf] [-m mail_to] \n" ;
    $usage   .=  "	cmdorb db sta_regex [sta_regex1 [sta_regex2 [...]]] \n\n"  ; 
    
    if (  ! getopts('nvVd:im:No:p:t:23') || @ARGV < 3 ) { 
        unless ( ($opt_i || $opt_N) && @ARGV == 2) {
            elog_notify ( $cmd ) ; 
            elog_die    ( $usage ) ;
        }
    }
    
    &savemail() if $opt_m ; 
    elog_notify( $cmd ) ; 
    $stime      = strydtime( now() );
    chop ($host = `uname -n` ) ;
    elog_notify ("\nstarting execution on	$host	$stime\n\n");
                
    $orbname    = shift @ARGV;
    $db         = shift @ARGV;

    $opt_v      = defined($opt_V) ? $opt_V : $opt_v ;    
    $verbose    = $opt_v ;
    $debug      = $opt_V ;
    
    $offset     = $opt_o || .5 ;
    
    if (system_check(0)) {
        $subject = "Problems - $pgm $host	Ran out of system resources" ;
        &sendmail($subject, $opt_m) if $opt_m ; 
        elog_die("\n$subject") ;
    }

#
#  check db
#
    $problems = &check_tables($db,$problems,qw(deployment dlcalwf dlsensor staq330));
    if ($problems) {
        $subject = "Problems - $pgm $host	db $db	missing tables" ;
        &sendmail($subject, $opt_m) if $opt_m ; 
        elog_die("\n$subject") ;
    }
#
#  check orb
#
    $orb = orbopen($orbname,"r") unless $opt_n;

    if ( $orb < 0 && $opt_n) {
        $subject = "Problems - $pgm $host	Failed to open orb $orbname" ;
        &sendmail($subject, $opt_m) if $opt_m ; 
        elog_die("\n$subject") ;
    }
#
#  get pf 
#
    $Pf = $opt_p || $pgm ;
        
    %pf = getparam( $Pf, $verbose, $debug ) ;
#
#  open db
#
    @db            = dbopen($db,'r');
    @dbdlcalwf     = dblookup(@db,0,"dlcalwf",0,0);
    @dbscrcal      = dblookup(@dbdlcalwf,0,0,0,"dbSCRATCH");  
    @dbdlsensor    = dblookup(@db,0,"dlsensor",0,0);
    @dbstaq330     = dblookup(@db,0,"staq330",0,0);
    @dbdlsensor    = dbsubset(@dbdlsensor,"endtime == NULL");
    @dbstaq330     = dbsubset(@dbstaq330,"endtime == NULL");
    if ($opt_t) {
        @dbdlsensor = dbsubset(@dbdlsensor,"time > \_$opt_t\_");
    }

    elog_notify(sprintf("%d	stations in dlsensor",dbquery(@dbdlsensor,'dbRECORD_COUNT')));
    elog_notify(sprintf("%d	stations in staq330",dbquery(@dbstaq330,'dbRECORD_COUNT')));

    if (@ARGV) {
        $subset = "sta =~ /";
        foreach $regex (@ARGV) {
            $subset .= "$regex";
            $subset .= "|" ;
        }
        $subset =~ s"\|$"\/";
        elog_notify("subsetting station list by - 	$subset") ;
        @dbstaq330 = dbsubset(@dbstaq330, $subset) ;
    }
#
#  sort stations alphabetically.
#
    @dbstaq330 = dbsort  (@dbstaq330, "sta") ;

    @dbj = dbjoin(@dbstaq330,@dbdlsensor,"ssident#dlident");
    elog_notify(sprintf("%d	stations in join of staq330 and dlsensor",dbquery(@dbj,'dbRECORD_COUNT')));
    $rows = dbquery(@dbj,'dbRECORD_COUNT');
    elog_notify(sprintf("%d	stations to calibrate",$rows));
    
    for ($dbj[3] = 0; $dbj[3] < $rows; $dbj[3]++) {
        ($sta,$dlsta,$chident,$snmodel,$target) = dbgetv(@dbj,"sta","dlsta","chident","snmodel","target");
        $q330{$sta}{dlsta}   = $dlsta;
        $q330{$sta}{chident} = $chident;
        $q330{$sta}{snmodel} = $snmodel;
        $q330{$sta}{target}  = $target;
    }
    
    @sta_0x2 = ();
    @sta_0x4 = ();
    
    $start = now();
    
    if ($opt_i) {  
    
        $offset = 0.25;

#  process uncalibrated stations
        
#  find stations missing calibrations using BHE monitor channel

        $subset   = "fchan =~ /BHE/ && dlcaltype =~ /white/ && dlcalinput =~ /d/ && ((endtime - time) > 3600)";
        @dbdlcale = dbsubset(@dbdlcalwf,$subset);
        @dbmone   = dbnojoin(@dbdlsensor,@dbdlcale,"dlident#ssident","dlsensor.time::dlsensor.endtime#dlcalwf.time");
        
        @dbje = dbjoin(@dbstaq330,@dbmone,"ssident#dlident");
        elog_notify(sprintf("%d	stations which need calibrations using BHE as monitor",dbquery(@dbje,'dbRECORD_COUNT')));
        
#  find stations missing calibrations using BHN monitor channel

        $subset   = "fchan =~ /BHN/ && dlcaltype =~ /white/ && dlcalinput =~ /d/ && ((endtime - time) > 3600)";
        @dbdlcaln = dbsubset(@dbdlcalwf,$subset);
        @dbmonn   = dbnojoin(@dbdlsensor,@dbdlcaln,"dlident#ssident","dlsensor.time::dlsensor.endtime#dlcalwf.time");
        
        @dbjn = dbjoin(@dbstaq330,@dbmonn,"ssident#dlident");
        elog_notify(sprintf("%d	stations which need calibrations using BHN as monitor",dbquery(@dbjn,'dbRECORD_COUNT')));
                
#  build station-channel pairs need to be processed for 0x4 and 0x2 calibrations

        for ($dbje[3] = 0; $dbje[3] < dbquery(@dbje,'dbRECORD_COUNT'); $dbje[3]++) {
            $sta = dbgetv(@dbje,"sta");
            push(@sta_0x4,$sta);
        }
        for ($dbjn[3] = 0; $dbjn[3] < dbquery(@dbjn,'dbRECORD_COUNT'); $dbjn[3]++) {
            $sta = dbgetv(@dbjn,"sta");
            push(@sta_0x2,$sta);
        }        
        
        dbclose(@db);
        
#
#  calbrate using channel 3 as monitor channel.
#
        $maxsleep = 0;
        unless ($opt_2) {
            foreach $sta (@sta_0x4) {
                ($sleep,$problems) = &calibrate($orbname,$sta,"0x4",$offset,$problems);
                $maxsleep = $sleep if ($sleep > $maxsleep);
            }
            elog_notify ("max sleeping $maxsleep") if $opt_v;
            $sleep = int($maxsleep - (now()-$start));
            elog_notify ("sleeping $sleep") ;
            if ($sleep > 0) {
                sleep ($sleep) unless ($opt_n );
            }
        }
#
#  calbrate using channel 2 as monitor channel.
#             
        unless ($opt_3) {
            foreach $sta (@sta_0x2) {
                ($sleep,$problems) = &calibrate($orbname,$sta,"0x2",$offset,$problems);
            }
            elog_notify ("sleeping $sleep") ;
            sleep ($sleep) unless ($opt_n );
        }
        
        
    } elsif ($opt_N) {  
# 
#  process whole network 10% of the network at one time.  Station codes are randomized to minimize geographical clustering.
#        
        $rows = dbquery(@dbstaq330,'dbRECORD_COUNT');
        $offset = 0;
        $trow   = int(($rows-1)/10) + 1;
        elog_notify("rows	$rows	trow	$trow") if $opt_V;
#
#  get alphabetic list of stations to calibrate for 0x4 and 0x2 channels
#
        for ($dbstaq330[3] = 0; $dbstaq330[3] < $rows; $dbstaq330[3]++) {
            $sta = dbgetv(@dbstaq330, "sta" ) ;
            if ($opt_d) {
                $nocal = epoch(yearday(now())) - (86400 * $opt_d);
                dbputv(@dbscrcal,"fsta",$sta,"fchan","BHE","time",$nocal,"endtime",now(),"dlcalinput","d","dlcaltype","white");
                @rows = dbmatches(@dbscrcal,@dbdlcalwf,"calbhe","fsta","fchan","time::endtime","dlcalinput","dlcaltype");
                if ($#rows > -1) {
                    $dbdlcalwf[3] = $rows[0];
                    ($time,$endtime) = dbgetv(@dbdlcalwf,"time","endtime");
                    elog_notify(sprintf "%s 0x4 already calibrated - starting %s	for	%4.2f	hours",$sta,strydtime( $time ),($endtime-$time)/3600);
                    next;
                }
            }
            push(@sta_0x4,$sta);
        }
        
        $nrowe = $#sta_0x4 + 1;
        $stepe = sprintf "%d" , (($nrowe-1)/$trow) + 1;
        elog_notify (sprintf "%d BHE monitor channels to calibrate	step	%d",$nrowe,$stepe);
        elog_notify ("@sta_0x4") if $opt_v;
        
        for ($dbstaq330[3] = 0; $dbstaq330[3] < $rows; $dbstaq330[3]++) {
            $sta = dbgetv(@dbstaq330, "sta" ) ;
            if ($opt_d) {
                $nocal = epoch(yearday(now())) - (86400 * $opt_d);
                dbputv(@dbscrcal,"fsta",$sta,"fchan","BHN","time",$nocal,"endtime",now(),"dlcalinput","d","dlcaltype","white");
                @rows = dbmatches(@dbscrcal,@dbdlcalwf,"calbhn","fsta","fchan","time::endtime","dlcalinput","dlcaltype");
                if ($#rows > -1) {
                    $dbdlcalwf[3] = $rows[0];
                    ($time,$endtime) = dbgetv(@dbdlcalwf,"time","endtime");
                    elog_notify(sprintf "%s 0x2  already calibrated - starting %s	for	%4.2f	hours",$sta,strydtime( $time ),($endtime-$time)/3600);
                    next;
                }
            }
            push(@sta_0x2,$sta);
        }
        
        $nrown = $#sta_0x2 + 1;
        $stepn = sprintf "%d" , (($nrown-1)/$trow) + 1;
        elog_notify (sprintf "%d BHN monitor channels to calibrate	step	%d",$nrown,$stepn);
        elog_notify ("@sta_0x2") if $opt_v;
        
        dbclose(@db);
        
#
#  calbrate using channel 3 as monitor channel.
#        
        unless ($opt_2) {
            for ($irow = 0; $irow < $stepe ; $irow++ ) {
                for ($drow = $irow; $drow <= $#sta_0x4 ; $drow += $stepe) {
                    elog_notify("0x4	drow	$drow") if $opt_V;
                    ($sleep,$problems) = &calibrate($orbname,$sta_0x4[$drow],"0x4",$offset,$problems);
                }
                elog_notify("row	$row\n\n") if $opt_v ;
                elog_notify ("sleeping $sleep") ;
                sleep ($sleep) unless ($opt_n );
            }
        }
#
#  calbrate using channel 2 as monitor channel.
#             
        unless ($opt_3) {
            for ($irow = 0; $irow < $stepn ; $irow++ ) {
                for ($drow = $irow; $drow <= $#sta_0x2 ; $drow += $stepn) {
                    elog_notify("0x2	drow	$drow") if $opt_V;
                    ($sleep,$problems) = &calibrate($orbname,$sta_0x2[$drow],"0x2",$offset,$problems);
                }
                elog_notify("row	$row\n\n") if $opt_v ;
                elog_notify ("sleeping $sleep") ;
                sleep ($sleep) unless ($opt_n );
            }
        }
        
    } else {  
#
#  process requested stations
#        
        @dbj = dbjoin(@dbstaq330,@dbdlsensor,"ssident#dlident");
        elog_notify(sprintf("%d	stations in join of staq330 and dlsensor",dbquery(@dbj,'dbRECORD_COUNT')));
        $sleep = 0 ;
        
#  build station-channel pairs need to be processed

        for ($dbj[3] = 0; $dbj[3] < dbquery(@dbj,'dbRECORD_COUNT'); $dbj[3]++) {
            $sta = dbgetv(@dbj,"sta");
            push(@sta_0x4,$sta);
        }
        
        dbclose(@db);
        
#
#  calbrate using channel 3 as monitor channel.
#
        $maxsleep = 0;
        unless ($opt_2) {
            foreach $sta (@sta_0x4) {
                ($sleep,$problems) = &calibrate($orbname,$sta,"0x4",$offset,$problems);
                $maxsleep = $sleep if ($sleep > $maxsleep);
            }
            elog_notify ("max sleeping $maxsleep") if $opt_v;
            $sleep = int($maxsleep - (now()-$start));
            elog_notify ("sleeping $sleep") ;
            if ($sleep > 0) {
                sleep ($sleep) unless ($opt_n );
            }
        }
#
#  calbrate using channel 2 as monitor channel.
#             
        unless ($opt_3) {
            foreach $sta (@sta_0x4) {
                ($sleep,$problems) = &calibrate($orbname,$sta,"0x2",$offset,$problems);
            }
        }
       
    }

#
#  clean up and exit
#
    if (-e "/tmp/tmp_dlcmd_$$.pf") {
        $cmd = "rm /tmp/tmp_dlcmd_$$.pf";
        elog_notify ($cmd) if $opt_v;
        $problems = run($cmd,$problems) ;
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

sub calibrate { # ($sleep,$problems) = &calibrate($orbname,$sta,$mon_chan,$offset,$problems);
    my ($orbname, $sta, $mon_chan, $offset, $problems) = @_ ;
    my ($cmd, $subject, $problem_check);
    my ($dlsta, $chident, $snmodel, $target, $duration, $otime,  $sleep);
    
#    elog_notify("calibrate argumeents - 	$orbname	$sta	$mon_chan	$offset	$problems") ;
        
#    ($sta,$dlsta,$chident,$snmodel,$target) = dbgetv(@dbj,"sta","dlsta","chident","snmodel","target");
    $dlsta    = $q330{$sta}{dlsta};
    $chident  = $q330{$sta}{chident};
    $snmodel  = $q330{$sta}{snmodel};
    $target   = $q330{$sta}{target};
    $duration = $pf{sensors}{$snmodel}{duration};
    if ($duration > 16380) {
        $duration = 16380;
        elog_notify ("$sta requested calibration $pf{sensors}{$snmodel}{duration} is too large, changed to 16380 seconds");
    }

    $cmd = "dlcmd $orbname $target q330 $dlsta calibrate ";
    $cmd .= "-duration $pf{sensors}{$snmodel}{duration} "           if exists $pf{sensors}{$snmodel}{duration};
    $cmd .= "-settling_time $pf{sensors}{$snmodel}{settling_time} " if exists $pf{sensors}{$snmodel}{settling_time};
    $cmd .= "-trailer_time $pf{sensors}{$snmodel}{trailer_time} "   if exists $pf{sensors}{$snmodel}{trailer_time};
    $cmd .= "-waveform $pf{sensors}{$snmodel}{waveform} "           if exists $pf{sensors}{$snmodel}{waveform};
    $cmd .= "-period $pf{sensors}{$snmodel}{period} "               if exists $pf{sensors}{$snmodel}{period};
    $cmd .= "-amplitude $pf{sensors}{$snmodel}{amplitude} "         if exists $pf{sensors}{$snmodel}{amplitude};
    $cmd .= "-sensors $chident -monitor_channels $mon_chan " ;
    $cmd .= "> /tmp/tmp_dlcmd_$$.pf 2>&1 ";
    if  (! $opt_n ) {
        elog_notify("$cmd");
        $problem_check = $problems;
        $problems = run($cmd,$problems) ;
        if ( $problem_check != $problems ) {
            $cmd = "cat /tmp/tmp_dlcmd_$$.pf";
            elog_notify("$cmd");
            $problems = run($cmd,$problems) ;
            $subject = "Problem $problems	- $pgm $host	dlcmd $sta $mon_chan" ;
            elog_complain("\n$subject") ;
            elog_complain("	$cmd") ;
        }
        if (! dlcmdpf("/tmp/tmp_dlcmd_$$.pf")) {
            return (0);
        }
    } else {
        elog_notify("skipping $cmd") ;
    }
    $otime  = $offset*$pf{sensors}{$snmodel}{duration};
    $sleep  = $pf{sensors}{$snmodel}{duration} + $pf{sensors}{$snmodel}{settling_time}
              + $pf{sensors}{$snmodel}{trailer_time} + 600;
    elog_notify("offset	$otime") if $opt_V ;
    sleep ($otime) unless $opt_n;

    return($sleep,$problems);
}

sub dlcmdpf { # $done = dlcmdpf($Pf);
    my ($Pf) = @_ ;
    my ($subject,$ref,$done);
    my (@keys);
    my (%pf) ;
    
    if (-z $Pf) {
        $done = 0;
        elog_notify("\n not completed 		done	$done\n\n");
        elog_complain("\n	dlcmd returned empty file\n\n");
        return ($done) ;
    }
    
    pfupdate($Pf);
    
    $ref = pfget($Pf, "");
    %pf = %$ref ;

    elog_notify("$Pf	ref($ref)") if $opt_V;
    
    @keys = sort( keys %pf);

    &prettyprint(\%pf) if $opt_V;
        
    unless (ref(\$pf{$keys[0]}) eq "REF") {
        $done = 0;
        elog_notify("\n not completed 		done	$done\n\n");
        &prettyprint(\%pf);
        elog_complain("\n	dlcmd did not return parameter file\n\n");
        return ($done) ;
    }
 
    if ($pf{$keys[0]}{disposition} =~ /done/) {
        $done = 1;
        elog_notify("completed successfully		done	$done") if $opt_V;
    } else {
        $done = 0;
        elog_notify("not completed 		done	$done");
        &prettyprint(\%pf);
    }
    return ($done) ;
}
