#
#   program needs:
#

    use List::Util qw/shuffle/;
    use Getopt::Std ;
    use strict ;
    use Datascope ;
    use archive ;
    use utilfunct ;
    use orb ;

    our ($opt_x,$opt_v,$opt_V,$opt_d,$opt_i,$opt_m,$opt_N,$opt_n,$opt_o,$opt_p,$opt_t,$opt_2,$opt_3);
    our ($offset,$temp_file,$pgm,$host);
    our (@problems, %pf,%q330);
    our (%sta_0x2, %sta_0x4, @rows, @sta_list, @sta_temp);

{    #  Main program

    my ($msg,$usage,$cmd,$subject);
    my ($field,$orb,$orbname,$db,$stime,$regex,$subset,$target);
    my ($sta,$dlsta,$snmodel,$chident,$time,$endtime,$sleep,$maxsleep);
    my ($start,$rows,$trow,$row,$irow,$drow,$nocal,$nrowe,$nrown,$stepe,$stepn);
    my (@db,@dbdlcalwf,@dbdlsensor,@dbstaq330,@dbj,@dbmone,@dbmonn,@dbje,@dbjn) ;
    my (@dbdlcale,@dbdlcaln,@dbscrcal);

    $pgm = $0 ;
    $pgm =~ s".*/"" ;
    elog_init($pgm, @ARGV);
    $cmd = "\n$0 @ARGV" ;

    $usage    =  "\n\n\nUsage: $0  \n	[-v] [-V] [-n]  [-2] [-3]  \n" ;
    $usage   .=  "	[-i] [-N [-d no_calib_days]] [-x reject_regex]\n" ;
    $usage   .=  "	[-o duration_offset_fraction] [-t start_time]  [-p pf] [-m mail_to] \n" ;
    $usage   .=  "	cmdorb db sta_regex [sta_regex1 [sta_regex2 [...]]] \n\n"  ;

    if (  ! getopts('x:nvVd:im:No:p:t:23') || @ARGV < 3 ) {
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

    $offset     = $opt_o || 0 ;
    $offset     =  0.5 if $opt_i and ! $opt_o ;

    $temp_file = "/tmp/tmp_dlcmd_$$.pf";

    if (system_check(0)) {
        log_die( "Problems - $pgm $host	Ran out of system resources" );
    }

    #
    #  check db
    #
    if ( &check_tables($db,0,qw(deployment dlcalwf dlsensor staq330)) ) {
        log_die( "Problems - $pgm $host	db $db	missing tables" ) ;
    }

    #
    #  check orb
    #
    $orb = orbopen($orbname,"r") ;
    log_die( "Problems - $pgm $host	Failed to open orb $orbname" ) if $orb < 0;

    #
    #  get pf
    #
    %pf = getparam( $opt_p || $pgm ) ;

    foreach $snmodel ( keys $pf{sensors} ) {
        #foreach $field (qw/duration settling_time trailer_time waveform period amplitude/) {
        foreach $field (qw/duration settling_time trailer_time waveform amplitude/) {
            unless ( defined $pf{sensors}{$snmodel}{$field} ) {
                log_complain("parameter [$field] missing from file for [$snmodel].");
            }
        }
    }

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

    @dbdlsensor = dbsubset(@dbdlsensor,"time > \_$opt_t\_") if $opt_t ;

    $rows = dbquery(@dbdlsensor,'dbRECORD_COUNT') ;
    elog_notify( "$rows \tstations in dlsensor" ) if $opt_v ;

    $rows = dbquery(@dbstaq330,'dbRECORD_COUNT') ;
    elog_notify( "$rows \tstations in staq330" ) if $opt_v ;


    #
    # Get list of stations
    #
    if (@ARGV) {
        $subset = "sta =~ /" . join('|',@ARGV) . "/" ;

        elog_notify( "subsetting station list by - $subset" ) if $opt_v ;
        @dbstaq330 = dbsubset(@dbstaq330, $subset) ;
    }

    #
    # In case that we have a regex reject then we remove those sites
    #
    if ($opt_x) {
        $subset = "sta !~ /$opt_x/" ;
        elog_notify( "rejecting station - $subset" ) if $opt_v ;
        @dbstaq330 = dbsubset(@dbstaq330, $subset) ;
    }

    #
    #  sort stations alphabetically.
    #
    elog_notify( "dbsort( sta )" ) if $opt_V ;
    @dbstaq330 = dbsort(@dbstaq330, "sta") ;

    elog_notify( "dbjoin( staq330 :ssident#dlident dlsensor )" ) if $opt_V ;
    @dbj = dbjoin( @dbstaq330, @dbdlsensor, "ssident#dlident" ) ;

    $rows = dbquery( @dbj, 'dbRECORD_COUNT' ) ;
    elog_notify( "$rows \tstations in join of staq330 and dlsensor" ) if $opt_V ;

    for ($dbj[3] = 0; $dbj[3] < $rows; $dbj[3]++) {

        ($sta,$dlsta,$chident,$snmodel,$target) =
                dbgetv( @dbj, "sta", "dlsta", "chident", "snmodel", "target" ) ;

        $q330{$sta}{$snmodel}{dlsta}   = $dlsta ;
        $q330{$sta}{$snmodel}{chident} = $chident ;
        $q330{$sta}{$snmodel}{target}  = $target ;
        elog_notify( "[$sta][$snmodel] -  $dlsta $chident $target" ) if $opt_V ;

    }

    $start = now();

    if ($opt_i) {
        #  process uncalibrated stations

        #  find stations missing calibrations using BHE monitor channel
        $subset   = "fchan =~ /BHE/ && dlcaltype =~ /white/ && dlcalinput =~ /d/ && ((endtime - time) > 3600)";
        @dbdlcale = dbsubset( @dbdlcalwf, $subset);
        @dbmone   = dbnojoin( @dbdlsensor, @dbdlcale, "dlident#ssident",
                        "dlsensor.time::dlsensor.endtime#dlcalwf.time");
        @dbje = dbjoin( @dbstaq330, @dbmone, "ssident#dlident");
        $rows = dbquery( @dbje, 'dbRECORD_COUNT' ) ;
        elog_notify("$rows \tstations which need calibrations using BHE as monitor" ) if $opt_v ;

        #  find stations missing calibrations using BHN monitor channel
        $subset   = "fchan =~ /BHN/ && dlcaltype =~ /white/ && dlcalinput =~ /d/ && ((endtime - time) > 3600)";
        @dbdlcaln = dbsubset( @dbdlcalwf, $subset ) ;
        @dbmonn   = dbnojoin( @dbdlsensor, @dbdlcaln, "dlident#ssident",
                        "dlsensor.time::dlsensor.endtime#dlcalwf.time" ) ;
        @dbjn = dbjoin( @dbstaq330, @dbmonn, "ssident#dlident" ) ;
        $rows = dbquery( @dbjn, 'dbRECORD_COUNT' ) ;
        elog_notify("$rows \tstations which need calibrations using BHN as monitor" ) if $opt_v ;


        #  build station-channel pairs need to be processed for 0x4 and 0x2 calibrations
        for ($dbje[3] = 0; $dbje[3] < dbquery(@dbje,'dbRECORD_COUNT'); $dbje[3]++) {
            ($sta,$snmodel) = dbgetv(@dbje,"sta","snmodel");
            $sta_0x4{$sta}{$snmodel} = 1;
        }
        for ($dbjn[3] = 0; $dbjn[3] < dbquery(@dbjn,'dbRECORD_COUNT'); $dbjn[3]++) {
            ($sta,$snmodel) = dbgetv(@dbjn,"sta","snmodel");
            $sta_0x2{$sta}{$snmodel} = 1;
        }

    } elsif ($opt_N) {
        #
        #  process whole network 10% of the network at one time.  Station codes are randomized to minimize geographical clustering.
        #

        #$rows = dbquery(@dbstaq330,'dbRECORD_COUNT');
        $rows = dbquery(@dbj,'dbRECORD_COUNT');

        $trow   = int(($rows-1)/10) + 1;
        elog_notify("rows \t$rows \ttrow \t$trow") if $opt_V;

        # to test if running with $opt_d
        $nocal = epoch(yearday(now())) - (86400 * $opt_d);

        #
        #  get alphabetic list of stations to calibrate for 0x4 and 0x2 channels
        #
        for ($dbj[3] = 0; $dbj[3] < $rows; $dbj[3]++) {

            ($sta,$snmodel) = dbgetv(@dbj,"sta","snmodel");
            if ($opt_d) {
                dbputv(@dbscrcal,"fsta",$sta,"fchan","BHE","time",$nocal,"endtime",now(),"dlcalinput","d","dlcaltype","white");
                @rows = dbmatches(@dbscrcal,@dbdlcalwf,"calbhe","fsta","fchan","time::endtime","dlcalinput","dlcaltype");
                if ($#rows > -1) {
                    $dbdlcalwf[3] = $rows[0];
                    ($time,$endtime) = dbgetv(@dbdlcalwf,"time","endtime");
                    elog_notify(sprintf "%s 0x4 already calibrated - starting %s	for	%4.2f	hours",
                                $sta,strydtime( $time ),($endtime-$time)/3600);
                    next;
                }
            }

            $sta_0x4{$sta}{$snmodel} = 1;

        }

        elog_notify ( join(" ", keys %sta_0x4) ) if $opt_v;

        for ($dbj[3] = 0; $dbj[3] < $rows; $dbj[3]++) {
            ($sta,$snmodel) = dbgetv(@dbj,"sta","snmodel");
            if ($opt_d) {
                dbputv(@dbscrcal,"fsta",$sta,"fchan","BHN","time",$nocal,"endtime",now(),"dlcalinput","d","dlcaltype","white");
                @rows = dbmatches(@dbscrcal,@dbdlcalwf,"calbhn","fsta","fchan","time::endtime","dlcalinput","dlcaltype");
                if ($#rows > -1) {
                    $dbdlcalwf[3] = $rows[0];
                    ($time,$endtime) = dbgetv(@dbdlcalwf,"time","endtime");
                    elog_notify(sprintf "%s 0x2  already calibrated - starting %s	for	%4.2f	hours",
                                $sta,strydtime( $time ),($endtime-$time)/3600);
                    next;
                }
            }
            $sta_0x2{$sta}{$snmodel} = 1;
        }

        elog_notify ( join(" ", keys %sta_0x2) ) if $opt_v;


    } else {
        #
        #  process requested stations
        #

        #  build station-channel pairs need to be processed
        for ($dbj[3] = 0; $dbj[3] < dbquery(@dbj,'dbRECORD_COUNT'); $dbj[3]++) {
            ($sta,$snmodel) = dbgetv(@dbj,"sta","snmodel");
            $sta_0x4{$sta}{$snmodel} = 1;
            $sta_0x2{$sta}{$snmodel} = 1;
            elog_notify("Add $sta $snmodel to calibration list." ) if $opt_v ;
        }

    }

    dbclose(@db);


    #
    #  calbrate using channel 3 as monitor channel.
    #
    $maxsleep = 0;
    $trow ||= scalar keys %sta_0x4 ;
    elog_notify("trow \t$trow") if $opt_V;

    unless ($opt_2) {
        @sta_list = shuffle keys %sta_0x4;
        while ( @sta_list ) {

            @sta_temp = splice( @sta_list , 0, $trow ) ;
            for $sta ( @sta_temp ) {
                for $snmodel ( keys %{$sta_0x4{$sta}} ) {
                    $sleep = &calibrate( $orbname, $sta, $snmodel, "0x4" ) ;
                    $maxsleep = $sleep > $maxsleep ? $sleep : $maxsleep;
                }
            }

            elog_notify ("sleeping $maxsleep") ;
            sleep ( $maxsleep ) unless $opt_n ;
        }
    }

    #
    #  calbrate using channel 2 as monitor channel.
    #
    $maxsleep = 0;
    $trow ||= scalar keys %sta_0x2 ;
    unless ($opt_3) {
        @sta_list = shuffle keys %sta_0x2;
        while ( @sta_list ) {

            @sta_temp = splice( @sta_list ,0,$trow) ;
            for $sta ( @sta_temp ) {
                for $snmodel ( keys %{$sta_0x2{$sta}} ) {
                    $sleep = &calibrate( $orbname, $sta, $snmodel, "0x2" ) ;
                    $maxsleep = $sleep > $maxsleep ? $sleep : $maxsleep;
                }
            }

            elog_notify ("sleeping $maxsleep") ;
            sleep ($maxsleep) unless $opt_n ;
        }
    }

    #
    #  clean up and exit
    #
    if (-e $temp_file) {
        elog_notify ("unlink $temp_file") if $opt_v;
        log_complain(@problems,"unlink $temp_file" ) unless unlink $temp_file ;
    }

    $stime = strydtime(now());

    if (scalar @problems ) {
        $subject = "Problems - $pgm $host	" . scalar @problems . " problems" ;

        elog_complain("") ;
        elog_complain("PROBLEMS:") ;
        $rows = 0 ;
        foreach $msg ( @problems ) {
            $rows += 1 ; 
            elog_complain( "    $rows)   $msg" ) ;
        }
        elog_notify ("completed with ". scalar @problems . " problems\t $stime\n\n");
    } else {
        $subject = "Success \t$pgm \t$host" ;
        elog_notify ("completed successfully\t $stime\n\n");
    }
    &sendmail( $subject, $opt_m ) if $opt_m ;

    elog_die("\n$subject") if scalar @problems;
    elog_notify($subject) ;

    exit(0);
}

sub calibrate { # ($sleep,$problems) = &calibrate($orbname,$sta,$snmodel,$mon_chan,$problems);
    my ($orbname, $sta, $snmodel, $mon_chan ) = @_ ;
    my (@lines, $field, $msg, $cmd, $subject, $problem_check);
    my ($dlsta, $chident, $target, $duration, $otime,  $sleep);

    elog_notify("calibrate($orbname, $sta, $snmodel, $mon_chan)") ;

    unless ( defined $pf{sensors}{$snmodel} ) {
        $msg = "Sensor [$snmodel] not defined in parameter file." ;
        log_complain( $msg );
        $msg = "******* Skip calibration for $sta $snmodel $mon_chan *******" ;
        log_complain( $msg ) ;
        return 0 ;
    }

    $dlsta    = $q330{$sta}{$snmodel}{dlsta};
    $chident  = $q330{$sta}{$snmodel}{chident};
    $target   = $q330{$sta}{$snmodel}{target};

    elog_notify("argumeents - $chident $dlsta $target") if $opt_V ;

    $duration = $pf{sensors}{$snmodel}{duration};

    if ($duration > 16380) {
        $duration = 16380;
        log_complain( "$sta requested calibration is too large, changed to 16380 seconds" ) ;
    }


    $cmd = "dlcmd $orbname $target q330 $dlsta calibrate ";
    $cmd .= "-duration $pf{sensors}{$snmodel}{duration} " if $pf{sensors}{$snmodel}{duration};
    $cmd .= "-settling_time $pf{sensors}{$snmodel}{settling_time} " if $pf{sensors}{$snmodel}{settling_time};
    $cmd .= "-trailer_time $pf{sensors}{$snmodel}{trailer_time} " if $pf{sensors}{$snmodel}{trailer_time};
    $cmd .= "-waveform $pf{sensors}{$snmodel}{waveform} " if $pf{sensors}{$snmodel}{waveform};
    $cmd .= "-period $pf{sensors}{$snmodel}{period} " if $pf{sensors}{$snmodel}{period};
    $cmd .= "-amplitude $pf{sensors}{$snmodel}{amplitude} " if $pf{sensors}{$snmodel}{amplitude};
    $cmd .= "-sensors $chident -monitor_channels $mon_chan " ;
    $cmd .= "> $temp_file 2>&1 ";

    elog_notify("run_cmd:[ $cmd ]") if $opt_v ;

    unless ( &run_cmd( $cmd ) ) {
        my @lines = <$temp_file>;
        log_complain( join("\n",@lines) ) ;

    }

    if ( dlcmdpf($temp_file,$dlsta)) {
        elog_notify("completed successfully		done	1") ;
    }
    else {
        elog_notify("\n not completed 		done	0\n\n") ;
    }


    # setting default offset to 0. Only run if set in command-line with $opt_o
    $otime = $offset*$pf{sensors}{$snmodel}{duration};
    elog_notify("offset \t$otime") if $opt_V ;
    sleep( $otime ) unless $opt_n ;

    $sleep  = $pf{sensors}{$snmodel}{duration} + $pf{sensors}{$snmodel}{settling_time}
              + $pf{sensors}{$snmodel}{trailer_time} + 600 ;

    return $sleep ;
}

sub dlcmdpf { # $done = dlcmdpf($Pf,$dlsta);
    my ($Pf) = shift ;
    my ($dlsta) = shift ;
    my ($subject,$ref,$done);
    my (@keys);
    my (%pf) ;

    # default failure code
    $done = 0;

    return $done if $opt_n ;

    unless (-e $Pf) {
        log_complain("\n	missing file [$Pf] for $dlsta\n\n");
        return ($done) ;
    }

    if (-z $Pf) {
        log_complain("\n	dlcmd returned empty file for $dlsta\n\n");
        return ($done) ;
    }

    pfupdate($Pf);

    $ref = pfget($Pf, "");
    %pf = %$ref ;

    elog_notify("$Pf	ref($ref)") if $opt_V;
    &prettyprint(\%pf) if $opt_V;

    @keys = sort( keys %pf);

    unless (ref(\$pf{$keys[0]}) eq "REF") {
        &prettyprint(\%pf);
        log_complain("\n	dlcmd did not return parameter file for $dlsta\n\n");
        return ($done) ;
    }

    if ($pf{$keys[0]}{disposition} =~ /done/) {
        $done = 1;
    } else {
        if ( defined $pf{$keys[0]}{error} ) {
            log_complain("\n	dlcmd returned error of [$pf{$keys[0]}{error}] on $dlsta \n\n");
        } else  {
            &prettyprint(\%pf);
        }
    }

    return ($done) ;
}

sub log_die {
    my ($msg) = shift ;

    &sendmail($msg, $opt_m) if $opt_m ;
    elog_die("\n$msg\n\n") ;

}
sub log_complain {
    my ($msg) = shift ;

    push(@problems,$msg ) ;
    elog_complain("") ;
    elog_complain("complain:    $msg") ;
    elog_complain("") ;

}
