#
#   program needs:
#		DONE	run white noise calibrations
#		DONE	calibration parameters by sensor type
#		DONE	specific command line station regex
#		DONE    offsets for multiple station calibration runs.
#		DONE    option to calibrate 10% of the network at a time, while eventually calibrating whole network
#		DONE    option to only calibrate uncalibrated stations
#       DONE	add target to staq330 table
#

    require "getopts.pl" ;
    use strict ;
    use Datascope ;
    use archive;
    use orb;
    
    our ($opt_v,$opt_V,$opt_i,$opt_m,$opt_N,$opt_n,$opt_o,$opt_p,$opt_t,$opt_2,$opt_3);
    our ($pgm,$host);
    our (%pf);
    
{    #  Main program

    my ($usage,$cmd,$problems,$subject,$debug,$verbose);
    my ($orb,$orbname,$db,$stime,$Pf,$regex,$subset,$target);
    my ($sta,$dlsta,$snmodel,$chident,$sleep,$problem_check);
    my ($offset,$start,$otime,$rows,$trow,$row,$irow,$drow,$group,$ncal,$key);
    my (@db,@dbdlcalwf,@dbdlsensor,@dbstaq330,@dbj,@dbtmp,@dbdeploy,@dbmone,@dbmonn,@dbje,@dbjn) ;
    my (@a,@group);
    my (%a);

    $pgm = $0 ; 
    $pgm =~ s".*/"" ;
    elog_init($pgm, @ARGV);
    $cmd = "\n$0 @ARGV" ;

    $problems = 0;
    $usage    =  "\n\n\nUsage: $0  \n	[-v] [-V] [-n] [-i] [-N] [-2] [-3] \n" ;
    $usage   .=  "	[-o duration_offset_fraction] [-t start_time] [-p pf] [-m mail_to] \n" ;
    $usage   .=  "	cmdorb db sta_regex [sta_regex1 [sta_regex2 [...]]] \n\n"  ; 
    
    if (  ! &Getopts('nvVim:No:p:t:23') || @ARGV < 3 ) { 
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
    $orb = orbopen($orbname,"r");

    if ( $orb < 0 ) {
        $subject = "Problems - $pgm $host	Failed to open orb $orbname" ;
        &sendmail($subject, $opt_m) if $opt_m ; 
        elog_die("\n$subject") ;
    }
#
#  get pf 
#
    $Pf = $opt_p || $pgm ;
        
    %pf = getparam($Pf);
#
#  open db
#
    @db            = dbopen($db,'r');
    @dbdeploy      = dblookup(@db,0,"deployment",0,0);
    @dbdlcalwf     = dblookup(@db,0,"dlcalwf",0,0);
    @dbdlsensor    = dblookup(@db,0,"dlsensor",0,0);
    @dbstaq330     = dblookup(@db,0,"staq330",0,0);
    @dbdlsensor    = dbsubset(@dbdlsensor,"endtime == NULL");
    @dbstaq330     = dbsubset(@dbstaq330,"endtime == NULL");
    
    if ($opt_t) {
        @dbdeploy = dbsubset(@dbdeploy,"time > \_$opt_t\_");
        @dbstaq330 = dbjoin(@dbstaq330,@dbdeploy,"sta","time::endtime");
        @dbstaq330 = dbseparate(@dbstaq330,"staq330");
    }

    elog_notify(sprintf("%d	stations in dlsensor",dbquery(@dbdlsensor,'dbRECORD_COUNT')));
    elog_notify(sprintf("%d	stations in staq330",dbquery(@dbstaq330,'dbRECORD_COUNT')));

    @dbj = dbjoin(@dbstaq330,@dbdlsensor,"ssident#dlident");
    elog_notify(sprintf("%d	stations in join of staq330 and dlsensor",dbquery(@dbj,'dbRECORD_COUNT')));
#
#  subset join of staq330-dlsensor by station regular expressions.
#
    if (@ARGV) {
        $subset = "sta =~ /";
        foreach $regex (@ARGV) {
            $subset .= "$regex";
            $subset .= "|" ;
        }
        $subset =~ s"\|$"\/";
        elog_notify("subsetting station list by - 	$subset") ;
        @dbj = dbsubset(@dbj, $subset) ;
    }
#
#  sort stations alphabetically.
#
    @dbj = dbsort  (@dbj, "sta") ;

    $start = now();
    
    if ($opt_i) {  

#  process uncalibrated stations
    
        $offset = 0.25;
        
#  find stations missing calibrations using BHE monitor channel

        $subset = "fchan =~ /BHE/ && dlcaltype =~ /white/ && dlcalinput =~ /d/ && ((endtime - time) > 3600)";
        @dbmone = dbsubset(@dbdlcalwf,$subset);
        @dbje   = dbnojoin(@dbj,@dbmone,"sta#fsta");
        
        elog_notify(sprintf("%d	stations which need calibrations using BHE as monitor",dbquery(@dbje,'dbRECORD_COUNT')));
        
#  find stations missing calibrations using BHN monitor channel

        $subset = "fchan =~ /BHN/ && dlcaltype =~ /white/ && dlcalinput =~ /d/ && ((endtime - time) > 3600)";
        @dbmonn = dbsubset(@dbdlcalwf,$subset);
        @dbjn = dbnojoin(@dbj,@dbmonn,"sta#fsta");
        
        elog_notify(sprintf("%d	stations which need calibrations using BHN as monitor",dbquery(@dbjn,'dbRECORD_COUNT')));
        
#  find out how many station-channel pairs need to be processed by first character in station name

        %a = ();
        for ($dbje[3] = 0; $dbje[3] < dbquery(@dbje,'dbRECORD_COUNT'); $dbje[3]++) {
            $sta = dbgetv(@dbje,"sta");
            $a{substr($sta,0,1)}++;
        }
        for ($dbjn[3] = 0; $dbjn[3] < dbquery(@dbjn,'dbRECORD_COUNT'); $dbjn[3]++) {
            $sta = dbgetv(@dbjn,"sta");
            $a{substr($sta,0,1)}++;
        }        
        
        prettyprint(\%a) if $opt_V;
        
#  group in sets of station-channels which have at least 10 pairs
        
        $ncal  = 0;
        $group = "";
        @group = ();
        foreach $key (keys(%a)) {
            $ncal  += $a{$key};
            $group .= "$key\.\*|";
            if ($ncal > 10) {
                $group =~ s/\|$//;
                elog_notify($group);
                push(@group,$group);
                
                $ncal = 0;
                $group = "",
            }
        }
        
#  make sure last set is appended
        
        if ($group !~ //) {
            $group =~ s/\|$//;
            elog_notify($group) if $opt_V;
            push(@group,$group);
        }
        
#  loop over groups, calibrating one group at a time
                
        foreach $group (@group) {
            $sleep = 0;
            $start = now();
            $subset = "sta =~ /$group/";
            
            @dbtmp = dbsubset(@dbje,$subset);
            if (dbquery(@dbtmp,"dbRECORD_COUNT") && ! $opt_2 ) {
                elog_notify("processing BHE monitor $subset");
                $sleep = &calibrate_view($orbname,"0x4",$offset,$problems,@dbtmp) ;
            }

            @dbtmp = dbsubset(@dbjn,$subset);
            
            if (dbquery(@dbtmp,"dbRECORD_COUNT") && ! $opt_3 ) {
                elog_notify("processing BHN monitor $subset");
                elog_notify ("sleeping $sleep");
                $sleep = $sleep - (now() - $start); 
                elog_notify ("sleeping $sleep");
                sleep ($sleep) unless ($opt_n || $sleep < 1);
                $sleep = &calibrate_view($orbname,"0x2",$offset,$problems,@dbtmp) ;
            }
        } 
        
    } elsif ($opt_N) {  
# 
#  process whole network
#             
        $rows = dbquery(@dbj,'dbRECORD_COUNT');
        elog_notify(sprintf("%d	stations to calibrate",$rows));
#
#  process 10% of the network at one time.  Station codes are randomized to minimize geographical clustering.
#        
        $offset = 0;
        $trow   = int($rows/10);
#
#  calbrate using channel 3 as monitor channel.
#        
        unless ($opt_2) {
            for ($irow = 0; $irow < 10 ; $irow++ ) {
                for ($drow = 0; $drow <= $trow ; $drow++) {
                    $row = (10*$drow) + $irow;
                    next if ($row >= $rows);
                    $dbj[3] = $row;
                    $sleep = &calibrate_one($orbname,"0x4",$offset,$problems,@dbj);
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
            for ($irow = 0; $irow < 10 ; $irow++ ) {
                for ($drow = 0; $drow <= $trow ; $drow++) {
                    $row = (10*$drow) + $irow;
                    next if ($row >= $rows);
                    $dbj[3] = $row;
                    $sleep = &calibrate_one($orbname,"0x2",$offset,$problems,@dbj);
                }
                elog_notify("row	$row\n\n") if $opt_v ;
                elog_notify ("sleeping $sleep") ;
                sleep ($sleep) unless ($opt_n );
            }
        }
#  
# #  This will calibrate in blocks of 10% of the stations, unfortunately will geographically cluster them
# #  calbrate using channel 3 as monitor channel.
# #        
#         $trow   = 10 unless ($trow > 10 );
#         for ($row = 0; $row < $rows; $row++){
#             $dbj[3] = $row;
#             $sleep = &calibrate_one($orbname,"0x4",$offset,$problems,@dbj);
#             unless (($row + 1) % $trow) {
#                 elog_notify("row	$row\n\n") if $opt_V ;
#                 elog_notify ("sleeping $sleep") ;
#                 sleep ($sleep) unless ($opt_n );
#             }
#         }
#         
#         elog_notify ("sleeping $sleep") ;
#         sleep ($sleep) unless ($opt_n );
# #
# #  calbrate using channel 2 as monitor channel.
# #        
#         
#         for ($row = 0; $row < $rows; $row++){
#             $dbj[3] = $row;
#             $sleep = &calibrate_one($orbname,"0x2",$offset,$problems,@dbj);
#             unless (($row + 1) % $trow) {
#                 elog_notify("row	$row\n\n") if $opt_V ;
#                 elog_notify ("sleeping $sleep") ;
#                 sleep ($sleep) unless ($opt_n );
#             }
#         }        
#  
    } else {  
#
#  process requested stations
#        
        $sleep = 0 ;
        $sleep = &calibrate_view($orbname,"0x4",$offset,$problems,@dbj) unless $opt_2 ;
    
        elog_notify ("sleeping $sleep") ;

        $sleep = $sleep - (now() - $start) ; 
        elog_notify ("sleeping $sleep") ;
        sleep ($sleep) unless ($opt_n || $sleep < 1) ;
    
        $sleep = &calibrate_view($orbname,"0x2",$offset,$problems,@dbj) unless $opt_3 ;
    }

    dbclose(@db);

#
#  clean up and exit
#
    $stime = strydtime(now());
    elog_notify ("completed successfully	$stime\n\n");

    $subject = sprintf("Success  $pgm  $host");
    elog_notify ($subject);
    &sendmail ( $subject, $opt_m ) if $opt_m ;
  
    exit(0);
}


sub prettyprint {
	my $val = shift;
	my $prefix = "";
	if (@_) { $prefix = shift ; }

	if (ref($val) eq "HASH") {
		my @keys = sort ( keys  %$val );
		my %hash = %$val;
		foreach my $key (@keys) {
			my $newprefix = $prefix . "{". $key . "}" ;
			prettyprint ($hash{$key}, $newprefix) ;
		}
	} elsif (ref($val) eq "ARRAY") {
		my $i = 0;
		my @arr = @$val;
		foreach my $entry ( @$val ) {
			my $newprefix = $prefix . "[". $i . "]" ;
			prettyprint ($arr[$i], $newprefix) ;
			$i++;
		}
	} else {
		print $prefix, " = ", $val, "\n";
#        elog_notify("$prefix  =  $val");
	}
}


sub dbdebug { #dbdebug(@db)
    my(@db) = @_;
    my($key,$field) ;
    my(@fields) ;
    my(%table) ;
    
    @fields = dbquery(@db,"dbTABLE_FIELDS");
    
    foreach $field (@fields) {
        elog_notify(sprintf("%s	%s",$field,dbgetv(@db,$field)));
    }
}


sub check_tables { # $problems = &check_tables($db,$problems,@tables);
    my ($db,$problems,@tables) = @_ ;
    my @db ; 
    my $table;

    @db = dbopen($db,"r") ;

    foreach $table (@tables) {
        @db      = dblookup(@db,0,$table,0,0);
        if (! dbquery(@db,"dbTABLE_PRESENT")) {
            elog_complain("No records in $table table of $db") ;
            $problems++;
        }
    }
    dbclose(@db);
    return($problems);
}

sub getparam { # %pf = getparam($Pf);
    my ($Pf) = @_ ;
    my ($subject,$ref);
    my (@keys);
    my (%pf) ;
    
    $ref = pfget($Pf, "");
    %pf = %$ref ;

    elog_notify("$Pf	ref($ref)") if $opt_V;
    
    @keys = sort( keys %pf);

    if ($opt_V) {
        &prettyprint(\%pf);
    }
            
    return (%pf) ;
}

sub calibrate_view { # ($maxdur,$sleep) = &calibrate_view($orbname,$mon_chan,$offset,$problems,@dbj);
    my ($orbname,$mon_chan,$offset,$problems,@dbj) = @_ ;
    my ($cmd, $subject, $problem_check);
    my ($sta, $dlsta, $chident, $snmodel, $target, $otime, $maxdur, $sleep, $ssec);

    $maxdur = 0;
    $sleep  = 0;
        
    for ($dbj[3]=0; $dbj[3] < dbquery(@dbj,'dbRECORD_COUNT'); $dbj[3]++){
        ($sta,$dlsta,$chident,$snmodel,$target) = dbgetv(@dbj,"sta","dlsta","chident","snmodel","target");
        $cmd = "dlcmd $orbname $target q330 $dlsta calibrate ";
        $cmd .= "-duration $pf{sensors}{$snmodel}{duration} "           if exists $pf{sensors}{$snmodel}{duration};
        $cmd .= "-settling_time $pf{sensors}{$snmodel}{settling_time} " if exists $pf{sensors}{$snmodel}{settling_time};
        $cmd .= "-trailer_time $pf{sensors}{$snmodel}{trailer_time} "   if exists $pf{sensors}{$snmodel}{trailer_time};
        $cmd .= "-waveform $pf{sensors}{$snmodel}{waveform} "           if exists $pf{sensors}{$snmodel}{waveform};
        $cmd .= "-period $pf{sensors}{$snmodel}{period} "               if exists $pf{sensors}{$snmodel}{period};
        $cmd .= "-amplitude $pf{sensors}{$snmodel}{amplitude} "         if exists $pf{sensors}{$snmodel}{amplitude};
        $cmd .= "-sensors $chident -monitor_channels $mon_chan " ;
        $cmd .= "> /tmp/tmp_dlcmd.pf 2>&1 ";
        if  (! $opt_n ) {
            elog_notify("$cmd");
            $problem_check = $problems;
            $problems = run($cmd,$problems) ;
            if ( $problem_check != $problems ) {
                $cmd = "cat /tmp/tmp_dlcmd.pf";
                elog_notify("$cmd");
                $problems = run($cmd,$problems) ;
                $subject = "Problems - $pgm $host	dlcmd $sta $mon_chan" ;
                &sendmail($subject, $opt_m) if $opt_m ; 
                elog_die("\n$subject") ;
            }
            if (! dlcmdpf("/tmp/tmp_dlcmd.pf")) {
                next;
            }
        } else {
            elog_notify("skipping $cmd") ;
        }
        $otime  = $offset*$pf{sensors}{$snmodel}{duration};
        $maxdur = $pf{sensors}{$snmodel}{duration} if ($pf{sensors}{$snmodel}{duration} > $maxdur);
        $ssec   = $pf{sensors}{$snmodel}{duration} + $pf{sensors}{$snmodel}{settling_time}
                  + $pf{sensors}{$snmodel}{trailer_time} + 600;
        $sleep  = $ssec if ($ssec > $sleep);
        elog_notify("offset	$otime") if $opt_v ;
        sleep ($otime) unless $opt_n;
    }
    return($maxdur,$sleep);
}

sub calibrate_one { # $sleep = &calibrate_view($orbname,$mon_chan,$offset,$problems,@dbj);
    my ($orbname,$mon_chan,$offset,$problems,@dbj) = @_ ;
    my ($cmd, $subject, $problem_check);
    my ($sta, $dlsta, $chident, $snmodel, $target, $otime,  $sleep);
        
    ($sta,$dlsta,$chident,$snmodel,$target) = dbgetv(@dbj,"sta","dlsta","chident","snmodel","target");
    $cmd = "dlcmd $orbname $target q330 $dlsta calibrate ";
    $cmd .= "-duration $pf{sensors}{$snmodel}{duration} "           if exists $pf{sensors}{$snmodel}{duration};
    $cmd .= "-settling_time $pf{sensors}{$snmodel}{settling_time} " if exists $pf{sensors}{$snmodel}{settling_time};
    $cmd .= "-trailer_time $pf{sensors}{$snmodel}{trailer_time} "   if exists $pf{sensors}{$snmodel}{trailer_time};
    $cmd .= "-waveform $pf{sensors}{$snmodel}{waveform} "           if exists $pf{sensors}{$snmodel}{waveform};
    $cmd .= "-period $pf{sensors}{$snmodel}{period} "               if exists $pf{sensors}{$snmodel}{period};
    $cmd .= "-amplitude $pf{sensors}{$snmodel}{amplitude} "         if exists $pf{sensors}{$snmodel}{amplitude};
    $cmd .= "-sensors $chident -monitor_channels $mon_chan " ;
    $cmd .= "> /tmp/tmp_dlcmd.pf 2>&1 ";
    if  (! $opt_n ) {
        elog_notify("$cmd");
        $problem_check = $problems;
        $problems = run($cmd,$problems) ;
        if ( $problem_check != $problems ) {
            $cmd = "cat /tmp/tmp_dlcmd.pf";
            elog_notify("$cmd");
            $problems = run($cmd,$problems) ;
            $subject = "Problems - $pgm $host	dlcmd $sta $mon_chan" ;
            &sendmail($subject, $opt_m) if $opt_m ; 
            elog_die("\n$subject") ;
        }
        if (! dlcmdpf("/tmp/tmp_dlcmd.pf")) {
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

    return($sleep);
}

sub dlcmdpf { # $done = dlcmdpf($Pf);
    my ($Pf) = @_ ;
    my ($subject,$ref,$done);
    my (@keys);
    my (%pf) ;
    
    pfupdate($Pf);
    
    $ref = pfget($Pf, "");
    %pf = %$ref ;

    elog_notify("$Pf	ref($ref)") if $opt_V;
    
    @keys = sort( keys %pf);

    &prettyprint(\%pf) if $opt_V;
        
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
