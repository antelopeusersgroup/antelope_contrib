#   program needs:
# DONE      output balers without dump for 24 hours
# DONE      output balers with more than 8 dumps in 24 hours
# DONE      only notify if balers have not turned on in 24 hours and where there are user messages
# DONE      add user_tag
#
# DONE      Make sure joins with comm table using sta time::endtime
# DONE      Make sure joins with dlsensor table ustng ssident#dlident time::endtime
#
# DONE      use dlcmd foley-4-72:qcmd - q330 - getconfig  or get status
# DONE      Add model,QAPCHP_123 type and version, QAPCHP_456 type and version, fields to staq330 
# DONE      Change firmware size from 8 to 30
# DONE      Add percentage of buffer full on DP1-4  field to staq330 - 
# DONE      Change stapb14 to stabaler 
# DONE      Add model field to stabaler
# DONE      Change firmware size from 8 to 30
# DONE		Add target field to staq330
#
# INFO      Using dlcmd will get status or configuration which is currently stored in the various q3302orb instances
# INFO      Using q330util will get the data by interogating the datalogger directly similar to willard.
#

    require "getopts.pl" ;
    use strict ;
#    use diagnostics;
    use Switch ;
    use Datascope ;
    use archive;
    use orb;
    use baler ;
    
    our ($opt_v,$opt_V,$opt_D,$opt_a,$opt_b,$opt_m,$opt_M,$opt_n,$opt_p,$opt_q,$opt_s);
    our ($pgm,$host);
    our (@targets);
    our (%pb,%q330,%stas);
        
{    #  Main program

    my ($usage,$cmd,$problems,$subject,$orbname,$cmdorb,$orb,$dbops,$debug,$verbose);
    my ($stime,$pfsource);
    my ($subset);
    my (@db,@stas) ;

    $pgm = $0 ; 
    $pgm =~ s".*/"" ;
    elog_init($pgm, @ARGV);
    $cmd = "\n$0 @ARGV" ;
    
    if (  ! &Getopts('vVDabnm:M:p:qs:') || @ARGV != 3 ) { 
        $usage  =  "\n\n\nUsage: $0  \n	[-v] [-V] [-n] [-a] [-b] [-q]\n" ;
        $usage .=  "	[-s subset] [-p pfsource_name] [-m mail_to_operator] [-M mail_to_field_ops]  \n" ;
        $usage .=  "	status_orb cmdorb db \n\n"  ; 

        elog_notify($cmd) ; 
        elog_die ( $usage ) ; 
    }
    
    &savemail() if $opt_m ; 
    elog_notify($cmd) ; 
    $stime = strydtime(now());
    chop ($host = `uname -n` ) ;
    elog_notify ("\nstarting execution on	$host	$stime\n\n");
    
    $subset     = $opt_s || ".*" ;
    $pfsource   = $opt_p || ".*/pf/st" ;
            
    $orbname    = $ARGV[0] ;
    $cmdorb     = $ARGV[1] ;
    $dbops      = $ARGV[2] ;

    $opt_v      = defined($opt_V) ? $opt_V : $opt_v ;    
    $verbose    = $opt_v ;
    $debug      = $opt_V ;
    
    if (system_check(0)) {
        $subject = "Problems - $pgm $host	Ran out of system resources" ;
        &sendmail($subject, $opt_m) if $opt_m ; 
        elog_die("\n$subject") ;
    }

#
#  check orb
#
    $orb = orbopen($orbname,"r+");

    if ( $orb < 0 ) {
        $subject = "Problems - $pgm $host	Failed to open orb $orbname" ;
        &sendmail($subject, $opt_m) if $opt_m ; 
        elog_die("\n$subject") ;
    }
    
    open(NOQ330,">/tmp/tmp_noq330_$$");
    open(Q3302ORB,">/tmp/tmp_q3302orb_$$");
    open(NOREG,">/tmp/tmp_noreg_$$");
    open(NOMSG,">/tmp/tmp_nomsg_$$");
    open(MAXREG,">/tmp/tmp_maxreg_$$");
    open(Q330NOTES,">/tmp/tmp_q330notes_$$");
    open(BALERNOTES,">/tmp/tmp_balernotes_$$");
    open(DBMASTER,">/tmp/tmp_dbmaster_$$");
    open(BALERFULL,">/tmp/tmp_balerfull_$$");

    print NOQ330 "No Q330 connection since q3302orb restarted\n";
    print Q3302ORB "Q3302orb config data problems\n";
    print NOREG "No Baler Registration in past 24 hours\n";
    print NOMSG "No UMSG in past 24 hours\n";
    print MAXREG "Maximum Baler Registration Exceeded\n";
    print Q330NOTES "Q330 Notifications\n\n";
    print BALERNOTES "Baler Notifications\n\n";
    print DBMASTER "Dbmaster possible issues\n\n";
    print BALERFULL "Q330 buffer for baler > 90%\n\n";
    
    &sta_info($orb,$pfsource);
    elog_notify("q3302orb targets -	@targets");
#    exit;
#    &prettyprint(\%stas) if $opt_V;
    
    $problems = q330_proc($cmdorb,$dbops,$subset,$problems) unless $opt_b;
    $problems = pb_proc($orb,$dbops,$subset) unless $opt_q;
        
    orbclose($orb);
 
    print NOQ330 "\n\n";
    print Q3302ORB "\n\n";
    print NOREG "\n\n";
    print NOMSG "\n\n";
    print MAXREG "\n\n";
    print Q330NOTES "\n\n";
    print BALERNOTES "\n\n";
    print DBMASTER "\n\n";
    print BALERFULL "\n\n";
    
    close(NOQ330);
    close(Q3302ORB);
    close(NOREG);
    close(NOMSG);
    close(MAXREG);
    close(Q330NOTES);
    close(BALERNOTES);
    close(DBMASTER);
    close(BALERFULL);
    
    $cmd = "cat /tmp/tmp_balerfull_$$ >> /tmp/tmp_noreg_$$";
    system($cmd);

    $cmd = "cat /tmp/tmp_noq330_$$ >> /tmp/tmp_noreg_$$";
    system($cmd);

    $cmd = "cat /tmp/tmp_q3302orb_$$ >> /tmp/tmp_noreg_$$";
    system($cmd);

    $cmd = "cat /tmp/tmp_nomsg_$$ >> /tmp/tmp_noreg_$$";
    system($cmd);

    $cmd = "cat /tmp/tmp_maxreg_$$ >> /tmp/tmp_noreg_$$";
    system($cmd);

    $cmd = "cat /tmp/tmp_dbmaster_$$ >> /tmp/tmp_noreg_$$";
    system($cmd);
        
    $cmd = "cat /tmp/tmp_balernotes_$$ >> /tmp/tmp_noreg_$$";
    system($cmd);
        
    $cmd = "cat /tmp/tmp_q330notes_$$ >> /tmp/tmp_noreg_$$";
    system($cmd);
        
    $subject = "ANF TA Q330 and Baler status";
    $cmd     = "rtmail -C -s '$subject' $opt_M < /tmp/tmp_noreg_$$";
        
    if  ( ! $opt_n  && $opt_M) {
        elog_notify("\n$cmd");        
        $problems = run($cmd,$problems) ;
    } else {
        elog_notify("\nskipping $cmd") ;
    }
    
    unlink "/tmp/tmp_noq330_$$" unless $opt_V;
    unlink "/tmp/tmp_q3302orb_$$" unless $opt_V;
    unlink "/tmp/tmp_noreg_$$" unless $opt_V;
    unlink "/tmp/tmp_nomsg_$$" unless $opt_V;
    unlink "/tmp/tmp_maxreg_$$" unless $opt_V;
    unlink "/tmp/tmp_q330notes_$$" unless $opt_V;
    unlink "/tmp/tmp_balernotes_$$" unless $opt_V;
    unlink "/tmp/tmp_dbmaster_$$" unless $opt_V;

    $stime = strydtime(now());
    elog_notify ("completed successfully	$stime\n\n");

    $subject = sprintf("Success  $pgm  $host");
    elog_notify ($subject);
    &sendmail ( $subject, $opt_m ) if $opt_m ;
  
    exit(0);
}

   
sub get_q330_stat { #%q330stat = get_q330_stat($cmdorb,$problems);
    my ($cmdorb,$problems) = @_;
    my ($cmd, $Pf, $ref, $key, $subject,$cmd_sel);
    my (@keys) ;
    my (%q330stat);
        
    elog_notify("\nget_q330_stat");
    $Pf = "/tmp/TA_status";
    
    unlink "$Pf.pf" if (-e "$Pf.pf");

    foreach $cmd_sel (@targets) {
        $cmd     = "dlcmd $cmdorb $cmd_sel q330 - getstatus >> $Pf.pf 2>&1 " ; 
        if  ( ! $opt_n ) {
            elog_notify("$cmd") if $opt_v;        
            $problems = run($cmd,$problems) ;
        } else {
            elog_notify("skipping $cmd") ;
        }
    
        if ($problems) {
            elog_notify($cmd);
            $subject = "Problems - $pgm $host	dlcmd getstatus failed";
            &sendmail($subject, $opt_m) if $opt_m ; 
            elog_die("\n$subject");
        }
        sleep 5;
    }

    return if $opt_n; 
    
    $ref = pfget($Pf, "");
    %q330stat = %$ref ;

    elog_notify("$Pf	ref($ref)") if $opt_D; 

    &prettyprint(\%q330stat) if $opt_D;
    
    @keys = sort( keys %q330stat);
        
    foreach $key (@keys) {
        if ( ref($q330stat{$key}) !~ /HASH/ ) {
            delete( $q330stat{$key} ) ;
            next;
        }
        $q330stat{$key}{dlsta} = $q330stat{$key}{dlname};
        elog_notify("$key	$q330stat{$key}{dlsta}") if $opt_V;
    }

    elog_notify(sprintf("%d status dlsta",$#keys+1)) if $opt_v;
    return ($problems,%q330stat);
}


sub get_q330_config { #%q330config = get_q330_config($cmdorb,$problems);
    my ($cmdorb,$problems) = @_;
    my ($cmd, $Pf, $ref, $key, $subject,$cmd_sel);
    my (@keys) ;
    my (%q330config);
    
    elog_notify("\nget_q330_config");
    
    $Pf = "/tmp/TA_config";
    
    unlink "$Pf.pf" if (-e "$Pf.pf");

    foreach $cmd_sel (@targets) {
        $cmd     = "dlcmd $cmdorb $cmd_sel q330 - getconfig >> $Pf.pf 2>&1 " ; 

        if  ( ! $opt_n ) {
            elog_notify("$cmd") if $opt_v;        
            $problems = run($cmd,$problems) ;
        } else {
            elog_notify("skipping $cmd") ;
        }

        if ($problems) {
            elog_notify($cmd);
            $subject = "Problems - $pgm $host	dlcmd getconfig failed";
            &sendmail($subject, $opt_m) if $opt_m ; 
            elog_die("\n$subject");
        }
        sleep 5;
    }

    return if $opt_n; 
    
    $ref = pfget($Pf, "");
    %q330config = %$ref ;

    elog_notify("$Pf	ref($ref)") if $opt_D;
    
    @keys = sort( keys %q330config);
    
    foreach $key (@keys) {
        if ( ref($q330config{$key}) !~ /HASH/ ) {
            delete( $q330config{$key} ) ;
            next;
        }
        $q330config{$key}{dlsta} = $q330config{$key}{dlname};
        elog_notify("$key	$q330config{$key}{dlsta}") if $opt_V;
    }
    elog_notify(sprintf("%d config dlsta",$#keys+1)) if $opt_v;
    
    return ($problems,%q330config);
}


sub q330_proc { # ($problems) = q330_proc($cmdorb,$dbops,$subset,$problems);
    my ($cmdorb,$dbops,$subset,$problems) = @_;
    my ($key,$nstat,$nconf,$dlname,$model,$filter);
    my ($dlsta,$endtime,$row,$field,$nchange,$Notes,$line);
    my ($nrec,$rec,$ctime,$dl,$test,$subject);
    my (@keys,@dlcom,@rows,@fields,@list,@dl,@f) ;
    my (@db,@dbscratch,@dbnull,@dbtest,@dbcal,@dbq330);
    my (%stat,%config,%sta);

    elog_notify("\nQ330 processing");

    $nstat = 0;
    $nconf = 1;
#
#  build status and config pf files
#
    ($problems,%stat)   = get_q330_stat($cmdorb,$problems);
    sleep 2;
    ($problems,%config) = get_q330_config($cmdorb,$problems);
    $nstat = scalar keys %stat;
    $nconf = scalar keys %config; 
    if ($nstat != $nconf) {
        $subject = "Problems - $pgm $host	can not get same number of keys from dlcmd getstat and getconfig";
        &sendmail($subject, $opt_m) if $opt_m ;
        &cleanup;
        elog_die("\n$subject");
    }
    
    return if $opt_n;
        
    &prettyprint(\%stat)   if $opt_D;
    &prettyprint(\%config) if $opt_D;
        
    @keys = sort( keys %stat);
#
#  notify for stations which have not connected
#
    elog_notify("");
    foreach $key (@keys) {
        $dlsta                        = $stat{$key}{dlsta};
        next unless ($dlsta =~ /$subset/);
        if ($stat{$key}{status}{glob}{power_on_time} == 0) {
            $line = "$stat{$key}{dlsta}	$stat{$key}{target}	has not been connected	-	power_on_time = 0 ";
            $line .= sprintf(" -	data latency %s",strtdelta($stas{$dlsta}{dlt}));
            print NOQ330 "$line\n";
            elog_notify($line);
        }
    }
#
#  notify for stations which have not registered or have corrupted net and sta values in dptokens
#
    foreach $key (@keys) {
        elog_notify("$key	$stat{$key}{dlsta}	$config{$key}{dlsta}") if $opt_V;
        
        unless (exists $config{$key}{config}{dptokens}) {
            $line = "key	$key	has not registered, no Q330 data in TA_config.pf";
            print Q3302ORB "$line\n";
            elog_notify($line);
            next;
        }
        
        if ($config{$key}{config}{dptokens}{4}{net} !~ /\w+/) {
            $line = "key	$key	config-dptokens-4-net	 \"$config{$key}{config}{dptokens}{4}{net}\"	config-dptokens-4-sta	 \"$config{$key}{config}{dptokens}{4}{sta}\"";
            print Q3302ORB "$line\n";
            elog_complain($line);
            $line = "dlcmd $cmdorb $config{$key}{target} q330 $key getconfig -force",
            elog_notify($line);
            $problems = run($line,$problems) ;
            next;
        }
        if ($config{$key}{config}{dptokens}{4}{sta} !~ /\w+/) {
            $line = "key	$key	config-dptokens-4-net	 \"$config{$key}{config}{dptokens}{4}{net}\"	config-dptokens-4-sta	 \"$config{$key}{config}{dptokens}{4}{sta}\"";
            print Q3302ORB "$line\n";
            elog_complain($line);
            $line = "dlcmd $cmdorb $config{$key}{target} q330 $key getconfig -force",
            elog_notify($line);
            $problems = run($line,$problems) ;
            next;
        }
#
#  load %q330
#
        @dlcom = split (":",$stat{$key}{dlcom});
        
        $dlsta                        = $stat{$key}{dlsta};
        next unless ($dlsta =~ /$subset/);
        next if ($config{$key}{config}{fix}{kmi_property_tag} == 0);
        $q330{$dlsta}{time}           = $config{$key}{config}{time}; 
        $q330{$dlsta}{net}            = $config{$key}{config}{dptokens}{4}{net}; 
        $q330{$dlsta}{sta}            = $config{$key}{config}{dptokens}{4}{sta};
        $q330{$dlsta}{inp}            = join(":",$dlcom[1],$config{$key}{config}{phyc1}{base_port});

        $q330{$dlsta}{target}         = $stat{$key}{target};
                
        $q330{$dlsta}{power_on_time}  = $stat{$key}{status}{glob}{power_on_time};
        if ($config{$key}{config}{man}{qapchp_1to3_type} == 2 || 
            $config{$key}{config}{man}{qapchp_4to6_type} == 3 ) {
            $q330{$dlsta}{model}          = "q330hr";
        } else {
            $q330{$dlsta}{model}          = "q330";
        }
        
        $q330{$dlsta}{ssident}        = sprintf("%8.8X%8.8X",
                                        $config{$key}{config}{fix}{system_serial_number_high},
                                        $config{$key}{config}{fix}{system_serial_number_low});
        $q330{$dlsta}{idtag}          = $config{$key}{config}{fix}{kmi_property_tag};
        $q330{$dlsta}{firm}           = sprintf("%d.%d", ($config{$key}{config}{fix}{system_version}>>8), 
                                        int($config{$key}{config}{fix}{system_version}%256) );
        $q330{$dlsta}{memory_size}    = $config{$key}{config}{man}{packet_memory_installed};
        if ($config{$key}{config}{man}{qapchp_4to6_version} == 0 ) {
            $q330{$dlsta}{nchan}      = 3;
        } else {
            $q330{$dlsta}{nchan}      = 6;
        }
        $q330{$dlsta}{nreboot}        = $config{$key}{config}{fix}{total_number_of_reboots};
        $q330{$dlsta}{last_reboot}    = epoch(2000001)+$config{$key}{config}{fix}{time_of_last_reboot};
        $q330{$dlsta}{q330_user_tag}  = $config{$key}{config}{glob}{user_tag};

        $q330{$dlsta}{qap_1_3_type}   = $config{$key}{config}{man}{qapchp_1to3_type};
        $q330{$dlsta}{qap_1_3_ver}    = $config{$key}{config}{man}{qapchp_1to3_version};
        $q330{$dlsta}{qap_1_3_ver}    = sprintf("%d.%d", ($config{$key}{config}{man}{qapchp_1to3_version}>>8), 
                                        int($config{$key}{config}{man}{qapchp_1to3_version}%256) );
        $q330{$dlsta}{qap_4_6_type}   = $config{$key}{config}{man}{qapchp_4to6_type};
        $q330{$dlsta}{qap_4_6_ver}    = $config{$key}{config}{man}{qapchp_4to6_version};
        $q330{$dlsta}{qap_4_6_ver}    = sprintf("%d.%d", ($config{$key}{config}{man}{qapchp_4to6_version}>>8), 
                                        int($config{$key}{config}{man}{qapchp_4to6_version}%256) );
 
        switch ( $config{$key}{config}{glob}{filter_bitmap} & 0x3 ) {
            case 0 {$q330{$dlsta}{ch1_3_filter}  = "linear phase filters for all frequencies" } 
            case 1 {$q330{$dlsta}{ch1_3_filter}  = "linear phase filters below 100 hz"         } 
            case 2 {$q330{$dlsta}{ch1_3_filter}  = "linear phase filters below 40 hz"          } 
            case 3 {$q330{$dlsta}{ch1_3_filter}  = "linear phase filters below 20 hz"          } 
        }
        switch ( $config{$key}{config}{glob}{filter_bitmap} & 0xc ) {
            case 0 {$q330{$dlsta}{ch4_6_filter}  = "linear phase filters for all frequencies" } 
            case 4 {$q330{$dlsta}{ch4_6_filter}  = "linear phase filters below 100 hz"         } 
            case 8 {$q330{$dlsta}{ch4_6_filter}  = "linear phase filters below 40 hz"          } 
            case 12 {$q330{$dlsta}{ch4_6_filter} = "linear phase filters below 20 hz"          } 
        }
        switch ( $config{$key}{config}{glob}{gain_bitmap} & 0x3 ) {
            case 0 {$q330{$dlsta}{ch1_preamp} = "disabled" } 
            case 1 {$q330{$dlsta}{ch1_preamp} = "enabled preamp off" } 
            case 2 {$q330{$dlsta}{ch1_preamp} = "enabled preamp on" } 
        }
        switch ( ($config{$key}{config}{glob}{gain_bitmap}>>2) & 0x3 ) {
            case 0 {$q330{$dlsta}{ch2_preamp} = "disabled" } 
            case 1 {$q330{$dlsta}{ch2_preamp} = "enabled preamp off" } 
            case 2 {$q330{$dlsta}{ch2_preamp} = "enabled preamp on" } 
        }
        switch ( ($config{$key}{config}{glob}{gain_bitmap}>>4) & 0x3 ) {
            case 0 {$q330{$dlsta}{ch3_preamp} = "disabled" } 
            case 1 {$q330{$dlsta}{ch3_preamp} = "enabled preamp off" } 
            case 2 {$q330{$dlsta}{ch3_preamp} = "enabled preamp on" } 
        }
        switch ( ($config{$key}{config}{glob}{gain_bitmap}>>6) & 0x3 ) {
            case 0 {$q330{$dlsta}{ch4_preamp} = "disabled" } 
            case 1 {$q330{$dlsta}{ch4_preamp} = "enabled preamp off" } 
            case 2 {$q330{$dlsta}{ch4_preamp} = "enabled preamp on" } 
        }
        switch ( ($config{$key}{config}{glob}{gain_bitmap}>>8) & 0x3 ) {
            case 0 {$q330{$dlsta}{ch5_preamp} = "disabled" } 
            case 1 {$q330{$dlsta}{ch5_preamp} = "enabled preamp off" } 
            case 2 {$q330{$dlsta}{ch5_preamp} = "enabled preamp on" } 
        }
        switch ( ($config{$key}{config}{glob}{gain_bitmap}>>10) & 0x3 ) {
            case 0 {$q330{$dlsta}{ch6_preamp} = "disabled" } 
            case 1 {$q330{$dlsta}{ch6_preamp} = "enabled preamp off" } 
            case 2 {$q330{$dlsta}{ch6_preamp} = "enabled preamp on" } 
        }
        if ( $config{$key}{config}{fix}{logical_port_packet_memory_size}[0] >0 ) {
            $q330{$dlsta}{LP1_buf}    = 
                int(100*$stat{$key}{status}{log}{0}{bytes_of_packet_currently_used}/$config{$key}{config}{fix}{logical_port_packet_memory_size}[0]);
        } else {
            $q330{$dlsta}{LP1_buf}    = -1;
        }
        if ( $config{$key}{config}{fix}{logical_port_packet_memory_size}[1] >0 ) {
            $q330{$dlsta}{LP2_buf}    = 
                int(100*$stat{$key}{status}{log}{1}{bytes_of_packet_currently_used}/$config{$key}{config}{fix}{logical_port_packet_memory_size}[1]);
        } else {
            $q330{$dlsta}{LP2_buf}    = -1;
        }
        if ( $config{$key}{config}{fix}{logical_port_packet_memory_size}[2] >0 ) {
            $q330{$dlsta}{LP3_buf}    = 
                int(100*$stat{$key}{status}{log}{2}{bytes_of_packet_currently_used}/$config{$key}{config}{fix}{logical_port_packet_memory_size}[2]);
        } else {
            $q330{$dlsta}{LP3_buf}    = -1;
        }
        if ( $config{$key}{config}{fix}{logical_port_packet_memory_size}[3] >0 ) {
            $q330{$dlsta}{LP4_buf}    = 
                int(100*$stat{$key}{status}{log}{3}{bytes_of_packet_currently_used}/$config{$key}{config}{fix}{logical_port_packet_memory_size}[3]);
            if ($q330{$dlsta}{LP4_buf}>90) {
                $line  = "$dlsta	bytes_of_packet_currently_used	";
                $line .= "$stat{$key}{status}{log}{3}{bytes_of_packet_currently_used}		";
                $line .= "logical_port_packet_memory_size		";
                $line .= "$config{$key}{config}{fix}{logical_port_packet_memory_size}[3]";
                print BALERFULL "$line\n";
                elog_complain($line);
            }
        } else {
            $q330{$dlsta}{LP4_buf}    = -1;
        }

    }
#
#  open db for writing staq330 table
#
    @db        = dbopen($dbops,"r+");
    @db        = dblookup(@db,0,"staq330",0,0);
    @dbscratch = dblookup(@db,0,0,0,"dbSCRATCH");
    @dbnull    = dblookup(@db,0,0,0,"dbNULL");
    $endtime   = dbgetv(@dbnull,"endtime");
    @fields    = dbquery(@db,"dbTABLE_FIELDS");
    
    @dbtest    = dbsubset(@db,"endtime == NULL");
    @dbtest    = dbsort(@dbtest,"dlsta","time");
    
    $nrec      = dbquery(@dbtest,"dbRECORD_COUNT");
    $ctime     = now() ;
    
    @keys = sort( keys %q330);
    elog_notify(sprintf("%d proc dlsta",$#keys+1));
    elog_notify(sprintf("%d rows with no endtime",$nrec)) if $opt_V;
#
#  update closed stations in staq330 table
#    
    for ($rec = 0 ; $rec < $nrec ; $rec++) {
        $dbtest[3] = $rec ;
        $dl = dbgetv(@dbtest,"dlsta");
        next unless ($dl =~ /$subset/);
        $test = 0;
        foreach $dlsta (@keys) {
            if ($dl =~ /$dlsta/) {
                $test = 1 ;
                elog_notify("$dl	open") if $opt_V;
                last;
            }
        }
        unless ($test ) {
            dbputv(@dbtest,"endtime",$ctime);
            $Notes++ ;
            elog_notify ("\nNotification #$Notes") if $opt_V;
            $line = "$dl	closed in staq330 table";
            elog_notify ( $line ) if $opt_V;
            print Q330NOTES "$line\n";
        }
    }
#
#  update station data in staq330 table
#
    foreach $dlsta (@keys) {
        dbput(@dbscratch,dbget(@dbnull));
        dbputv(@dbscratch, "dlsta",          $dlsta,
                           "time",           $q330{$dlsta}{time},
                           "endtime",        $endtime,
                           "target",         $q330{$dlsta}{target} ,
                           "net",            $q330{$dlsta}{net},
                           "sta",            $q330{$dlsta}{sta},
                           "inp",            $q330{$dlsta}{inp},
                           "model",          $q330{$dlsta}{model},
                           "ssident",        $q330{$dlsta}{ssident},
                           "idtag",          $q330{$dlsta}{idtag},
                           "firm",           $q330{$dlsta}{firm},
                           "memory_size",    $q330{$dlsta}{memory_size},
                           "nchan",          $q330{$dlsta}{nchan},
                           "nreboot",        $q330{$dlsta}{nreboot},
                           "last_reboot",    $q330{$dlsta}{last_reboot},
                           "q330_user_tag",  $q330{$dlsta}{q330_user_tag},
                           "qap_1_3_type",   $q330{$dlsta}{qap_1_3_type},
                           "qap_1_3_ver",    $q330{$dlsta}{qap_1_3_ver},
                           "qap_4_6_type",   $q330{$dlsta}{qap_4_6_type},
                           "qap_4_6_ver",    $q330{$dlsta}{qap_4_6_ver},
                           "ch1_3_filter",   $q330{$dlsta}{ch1_3_filter},
                           "ch4_6_filter",   $q330{$dlsta}{ch4_6_filter},
                           "ch1_preamp",     $q330{$dlsta}{ch1_preamp},
                           "ch2_preamp",     $q330{$dlsta}{ch2_preamp},
                           "ch3_preamp",     $q330{$dlsta}{ch3_preamp},
                           "ch4_preamp",     $q330{$dlsta}{ch4_preamp},
                           "ch5_preamp",     $q330{$dlsta}{ch5_preamp},
                           "ch6_preamp",     $q330{$dlsta}{ch6_preamp},
                           "LP1_buf",        $q330{$dlsta}{LP1_buf},
                           "LP2_buf",        $q330{$dlsta}{LP2_buf},
                           "LP3_buf",        $q330{$dlsta}{LP3_buf},
                           "LP4_buf",        $q330{$dlsta}{LP4_buf}  ) ;
        if ($q330{$dlsta}{nchan} == 3 ) {
            dbputv(@dbscratch, "qap_4_6_type",   dbgetv(@dbnull,"qap_4_6_type"),
                           "qap_4_6_ver",        dbgetv(@dbnull,"qap_4_6_ver"),
                           "ch4_6_filter",       dbgetv(@dbnull,"ch4_6_filter"),
                           "ch4_preamp",         dbgetv(@dbnull,"ch4_preamp"),
                           "ch5_preamp",         dbgetv(@dbnull,"ch5_preamp"),
                           "ch6_preamp",         dbgetv(@dbnull,"ch6_preamp")) ;
            $q330{$dlsta}{qap_4_6_type} = dbgetv(@dbnull,"qap_4_6_type");
            $q330{$dlsta}{qap_4_6_ver}  = dbgetv(@dbnull,"qap_4_6_ver");
            $q330{$dlsta}{ch4_6_filter} = dbgetv(@dbnull,"ch4_6_filter");
            $q330{$dlsta}{ch4_preamp}   = dbgetv(@dbnull,"ch4_preamp");
            $q330{$dlsta}{ch5_preamp}   = dbgetv(@dbnull,"ch5_preamp");
            $q330{$dlsta}{ch6_preamp}   = dbgetv(@dbnull,"ch6_preamp");
        }
        @rows = dbmatches(@dbscratch,@db,"q330","dlsta","time::endtime");
        if ($#rows == -1) {
            dbadd(@db);
            elog_notify("	Adding	$dlsta") if $opt_V;
            $Notes++ ;
            elog_notify ("\nNotification #$Notes") if $opt_V;
            $line = "$dlsta added to staq330 table";
            elog_notify ( $line ) if $opt_V;
            print Q330NOTES "$line\n";
        } elsif ($#rows == 0) {
            $db[3] = $rows[0];
            %sta = ();
            $nchange = 0;
            @list = ();
            foreach $field (@fields) {
                elog_notify("	$field") if $opt_V;
                $sta{$field} = dbgetv(@db,$field);
                next if ($field =~ /dlsta|time|endtime|lddate|nreboot|last_reboot|LP1_buf|LP2_buf|LP3_buf|LP4_buf/);
                
                elog_notify("	$dlsta	$field	$q330{$dlsta}{$field}	$sta{$field}") if $opt_V;
                if ($q330{$dlsta}{$field} != $sta{$field} && $field =~ /nchan|memory_size|q330_user_tag/ ) {
                    elog_notify("	$dlsta	$field	q330	$q330{$dlsta}{$field}	db	$sta{field}"); # if $opt_V;
                    push(@list,$field);
                    $nchange++;
                }
                if ($q330{$dlsta}{$field} !~ /$sta{$field}/ && $field !~ /nchan|memory_size|q330_user_tag/ ) {
                    elog_notify("	$dlsta	$field	q330	$q330{$dlsta}{$field}	db	$sta{field}"); # if $opt_V;
                    push(@list,$field);
                    $nchange++;
                }
            }
            if ($nchange) {
                dbputv(@db,"endtime",($q330{$dlsta}{time} - 1));
                dbadd(@db);
                $Notes++ ;
                elog_notify ("\nNotification #$Notes") if $opt_V;
                $line = "$dlsta - @list fields changed, starting new record";
                elog_notify ( $line ) if $opt_V;
                print Q330NOTES "$line\n";
                next;
            } elsif  ( $q330{$dlsta}{last_reboot} != $sta{last_reboot} ) {
                dbputv(@db,"nreboot",$q330{$dlsta}{nreboot},"last_reboot",$q330{$dlsta}{last_reboot});
                $Notes++ ;
                elog_notify ("\nNotification #$Notes") if $opt_V;
                $line = "$dlsta - nreboot and last_reboot fields changed, updating record";
                elog_notify ( $line ) if $opt_V;
                print Q330NOTES "$line\n";
            }
            dbputv(@db,"LP1_buf",        $q330{$dlsta}{LP1_buf},
                       "LP2_buf",        $q330{$dlsta}{LP2_buf},
                       "LP3_buf",        $q330{$dlsta}{LP3_buf},
                       "LP4_buf",        $q330{$dlsta}{LP4_buf} ) ;
        } else {
            $Notes++ ;
            elog_notify ("\nNotification #$Notes") if $opt_V;
            $line = "\n	Too many open rows for $dlsta in $dbops table staq330." . 
                          "\n	Closing open records and adding new row";
            elog_complain ( $line ) if $opt_V;
            print Q330NOTES "$line\n";
            foreach $row (@rows) {
                $db[3] = $row;
                dbputv(@db,"endtime",($q330{$dlsta}{time} - 1));
            }
            dbadd(@db);
        }

    
    }
    
    dbclose(@db);
#
#  identify possible dbmaster problems
#    
    @db        = dbopen($dbops,"r");
    @dbq330    = dblookup(@db,0,"staq330",0,0);
    @dbcal     = dblookup(@db,0,"calibration",0,0);
    @dbcal     = dbsubset(@dbcal,"endtime == NULL && chan =~ /BHZ/");
    @dbq330    = dbsubset(@dbq330,"endtime == NULL");
    @dbq330    = dbjoin(@dbq330,@dbcal,"-outer","sta","time::endtime");
    $nrec      = dbquery(@dbq330,"dbRECORD_COUNT");    


    for ($rec = 0; $rec < $nrec; $rec++) {
        $dbq330[3] = $rec ; 
        ($dlsta,$dlname,$model,$filter) = dbgetv(@dbq330,"dlsta","calibration.dlname","model","ch1_3_filter");
        @dl = split(/_/,$dlname);
        @f  = split(/ /,$filter);
        if ($model !~ /^$dl[0]$/) {
            $line = sprintf("$dlsta calibration table dlname - %-25s |	q330 model  - 	%s",$dlname,$model);
            elog_notify($line);
            print DBMASTER "$line\n";
        } 
        if ($dlname =~ /.*below.*/) {
            if ( $dlname !~ /.*$f[4].*/ ) {
                $line = sprintf("$dlsta calibration table dlname - %-25s |	q330 filter - 	%s",$dlname,$filter);
                elog_notify($line);
                print DBMASTER "$line\n";
            } 
        } else {
            if ($filter =~ /below/ ) {
                $line = sprintf("$dlsta calibration table dlname - %-25s |	q330 filter - 	%s",$dlname,$filter);
                elog_notify($line);
                print DBMASTER "$line\n";
            } 
        }
    }
    dbclose(@db);

    elog_notify("\n\n")  if $opt_V;    
    &prettyprint(\%q330) if $opt_V;
    elog_notify("\n\n")  if $opt_V;

    return ($problems);
}

sub pb_proc { # ($problems) = pb_proc($orb,$dbops,$subset);
    my ($orb,$dbops,$subset) = @_ ;
    my ($srcname, $npkts, $when, $source, $pktid);
    my ($pkttime, $packet, $nbytes, $result, $pkt, $type, $desc);
    my ($net,$sta,$chan,$loc,$suffix,$subcode) ;
    my ($dlsta,$n,$strtmp,$endtime,$nchange,$field,$row,$Notes,$line);
    my ($nrec,$rec,$ctime,$dl,$test);
    my (@sources,@keys,@fields,@rows,@list,@baler,@noq330,@q330);
    my (@db,@dbscratch,@dbnull,@dbtest);
    my (%sta);
    
    elog_notify("\nBaler processing");
#
#  get log packets from status orb
#
    $srcname = $subset . "/log";

    elog_notify("orb	$orb	srcname	$srcname	") if $opt_V;
    
#    orbreject($orb,$reject);
    
    $npkts = orbselect($orb,$srcname);
    ($when, @sources) = orbsources($orb) ;
    
    $pktid = orbafter($orb,(now()-86400));
    $npkts = 0 ;
    
    foreach $source (@sources) {
        printf "%-15s %8d %6.0f %s\n",
                $source->srcname, $source->npkts, $source->nbytes/1024.,
                strtdelta($when-$source->slatest_time)  if $opt_V;
        $npkts += $source->npkts;
        ($net,$sta,$chan,$loc,$suffix,$subcode) = split_srcname($source->srcname);
        $dlsta = join("_",$net,$sta);
        $pb{$dlsta}{nreg24}  = 0;
        $pb{$dlsta}{nmsg24}  = 0;
        $pb{$dlsta}{nusmg24} = 0;
        $pb{$dlsta}{nlog24}  = 0;
        $pb{$dlsta}{ndbt24}  = 0;
    }

    $line =  "\nBaler processing - number of log packets in status orb over past 24 hours:";
    $line .= "	$npkts	";
    elog_notify($line) ;

    return if $opt_n;
#
#  load %pb
#
    $n = 0;
    while ($pktid > -1) {
        $n++;
        ($pktid, $source, $pkttime, $packet, $nbytes) = orbget($orb,$pktid) ;
        elog_notify("$n	pktid	$pktid	srcname	$srcname	source	$source	time	$pkttime	" . 
                    "nbytes	$nbytes") if $opt_D;
        
        ($net,$sta,$chan,$loc,$suffix,$subcode) = split_srcname($source);
        elog_notify("$net	$sta	$chan	$loc	$suffix	$subcode") if $opt_D;
        
        $dlsta = join("_",$net,$sta);
        
        ($result, $pkt) = unstuffPkt($srcname, $pkttime, $packet, $nbytes) ;
        ($type, $desc) = $pkt->PacketType() ;
    
        $strtmp = $pkt->string ;
        if ( defined $strtmp ) {
            elog_notify($strtmp) if (($opt_a && ($strtmp =~ /tag/)) || $opt_V ) ;
            if ($strtmp =~ /tag/) {
                $pb{$dlsta}{string} = $strtmp ;
                $pb{$dlsta}{nreg24}++;
                $pb{$dlsta}{ptime} = $pkttime;
            }
            if ($strtmp =~ /127.0.0.1/) {
                $pb{$dlsta}{string} = $strtmp ;
                $pb{$dlsta}{ptime} = $pkttime;
            }
            if ($strtmp =~ /UMSG/) {
                elog_notify($strtmp) if $opt_V;
                $pb{$dlsta}{nusmg24}++;
            }
            if ($strtmp =~ /LOG/) {
                $pb{$dlsta}{nlog24}++;
            }
            if ($strtmp =~ /DEBUGT/) {
                $pb{$dlsta}{ndbt24}++;
            }
            if ($strtmp =~ /UMSG: ip = 20.210.0.1/ && $strtmp =~ /UMSG: ip = registered/ ) {
                @baler = split ( ' ', $strtmp ) ;
                $pb{$dlsta}{last_reg}     = join " ", $baler[0],	$baler[1] ;
            }
            $pb{$dlsta}{nmsg24}++;
        }
        
        $pktid = orbseek($orb,"ORBNEXT") ;
        elog_notify("$n	pktid	$pktid") if $opt_D;

    }

    elog_notify("\n\n		TEST")  if $opt_V;    
    &prettyprint(\%pb)              if $opt_V;
    elog_notify("\n\n		")      if $opt_V;
#
#  Compare keys of %q330 and %pb
#
    @keys   = sort( keys %pb);
    @noq330 = ();
    @q330   = ();
    foreach $dlsta (@keys) {
        push(@noq330,$dlsta) unless exists $q330{$dlsta};
        push(@q330,$dlsta)   if     exists $q330{$dlsta};        
    }
    @keys = @q330;
    
    elog_notify("	Baler info with no Q330 - 	@noq330");
#
#  Open db to update stabaler table
#
    @db        = dbopen($dbops,"r+");
    @db        = dblookup(@db,0,"stabaler",0,0);
    @dbscratch = dblookup(@db,0,0,0,"dbSCRATCH");
    @dbnull    = dblookup(@db,0,0,0,"dbNULL");
    $endtime   = dbgetv(@dbnull,"endtime");
    @fields    = dbquery(@db,"dbTABLE_FIELDS");
    
    elog_notify("\n\n		keys	@keys") if $opt_V;
    
    @dbtest    = dbsubset(@db,"endtime == NULL");
    @dbtest    = dbsort(@dbtest,"dlsta","time");
    
    $nrec      = dbquery(@dbtest,"dbRECORD_COUNT");
    $ctime     = now() ;
    
    elog_notify(sprintf("%d proc dlsta",$#keys+1));
    elog_notify(sprintf("%d rows with no endtime",$nrec)) if $opt_V;
#
#  update closed stations in stabaler table
#
    for ($rec = 0 ; $rec < $nrec ; $rec++) {
        $dbtest[3] = $rec ;
        $dl = dbgetv(@dbtest,"dlsta");
        next unless ($dl =~ /$subset/);
        $test = 0;
        foreach $dlsta (@keys) {
            if ($dl =~ /$dlsta/) {
                $test = 1 ;
                elog_notify("$dl	open") if $opt_V;
                last;
            }
        }
        unless ($test ) {
            dbputv(@dbtest,"endtime",$ctime);
            elog_notify ("\nNotification #$Notes") if $opt_V;
            $line = "$dl	closed in stabaler table";
            elog_notify ( $line ) if $opt_V;
            print BALERNOTES "$line\n";
        }
    }
#
#  update station baler data in stabaler table
#
    foreach $dlsta (@keys) {
        dbput(@dbscratch,dbget(@dbnull));
        if ($pb{$dlsta}{string} =~ /tag/) {
            &pb14($dlsta);
        }
        if ($pb{$dlsta}{string} =~ /127.0.0.1/) {
            &pb44($q330{$dlsta}{inp});
        }
        elog_notify("$dlsta	$pb{$dlsta}{net}	$pb{$dlsta}{sta}	$pb{$dlsta}{inp}	" .	
        "$pb{$dlsta}{model}	$pb{$dlsta}{ssident}	$pb{$dlsta}{firm}	$pb{$dlsta}{last_reg}") if $opt_V;
        if ($pb{$dlsta}{nreg24} == 0 ) {
            $line = "$dlsta -	No baler registrations in the last 24 hours, $pb{$dlsta}{nusmg24} umsgs";
            elog_notify("\n$line");
            $line  = "$dlsta	nreg24	$pb{$dlsta}{nreg24}	nmsg24	$pb{$dlsta}{nmsg24}	nusmg24	";
            $line .= "$pb{$dlsta}{nusmg24}	nlog24	$pb{$dlsta}{nlog24}	ndbt24	$pb{$dlsta}{ndbt24}";
            $line .= sprintf("	data latency %s",strtdelta($stas{$dlsta}{dlt}));
            elog_notify($line);
        
            if ($pb{$dlsta}{nusmg24}) {
                print NOREG "$line\n" if (exists($pb{$dlsta}{nreg24}) && $pb{$dlsta}{model} !~ /Baler44/);
            } else {
                $line = "$dlsta -	No baler registrations and no UMSG messages in the last 24 hours";
                $line .= sprintf(" -	data latency %s",strtdelta($stas{$dlsta}{dlt}));
                print NOMSG "$line\n";
                elog_notify("$line");
            }
        }
        if ($pb{$dlsta}{ssident} < 1) {
            elog_notify("$dlsta	- No Baler information");
            next;
        }
        dbputv(@dbscratch, "dlsta",          $dlsta,
                           "time",           $pb{$dlsta}{ptime},
                           "endtime",        $endtime,
                           "net",            $pb{$dlsta}{net},
                           "sta",            $pb{$dlsta}{sta},
                           "inp",            $pb{$dlsta}{inp},
                           "model",          $pb{$dlsta}{model},
                           "ssident",        $pb{$dlsta}{ssident},
                           "nreg24",         $pb{$dlsta}{nreg24},
                           "firm",           $pb{$dlsta}{firm} );
        if (exists($pb{$dlsta}{last_reg})) {
            dbputv(@dbscratch,"last_reg",str2epoch($pb{$dlsta}{last_reg}));
        }
        if (exists($pb{$dlsta}{last_reboot})) {
            dbputv(@dbscratch,"last_reboot",$pb{$dlsta}{last_reboot});
        }
        if (exists($pb{$dlsta}{nreboot})) {
            dbputv(@dbscratch,"nreboot",$pb{$dlsta}{nreboot});
        }
        @rows = dbmatches(@dbscratch,@db,"pb","dlsta","time::endtime");
        if ($#rows == -1) {
            dbadd(@db);
            elog_notify("	Adding	$dlsta") if $opt_V;
            $Notes++ ;
            elog_notify ("\nNotification #$Notes") if $opt_V;
            $line = "$dlsta added to stabaler table";
            elog_notify ( $line ) if $opt_V;
            print BALERNOTES "$line\n";
        } elsif ($#rows == 0) {
            $db[3] = $rows[0];
            %sta = ();
            $nchange = 0;
            @list = ();
            foreach $field (@fields) {
                $sta{$field} = dbgetv(@db,$field);
                elog_notify("	$field	baler	$pb{$dlsta}{$field}	db	$sta{$field}") if $opt_V;
                next if ($field =~ /dlsta|time|endtime|lddate|nreg24|last_reg/);
                next if ($field =~ /nreboot|last_reboot/ && $pb{$dlsta}{model} =~ /Baler14/);
                next if ($field =~ /last_reboot/ && str2epoch($pb{$dlsta}{last_reboot}) == $sta{$field}) ;
                if ($pb{$dlsta}{$field} !~ /$sta{$field}/) {
                    elog_notify("	$field	baler	$pb{$dlsta}{$field}	db	$sta{$field}"); # if $opt_V;
                    push(@list,$field);
                    $nchange++;
                }
            }
            if ($nchange) {
                dbputv(@db,"endtime",($pb{$dlsta}{ptime} - 1));
                dbadd(@db);
                $Notes++ ;
                elog_notify ("\nNotification #$Notes")  if $opt_V;
                $line = "$dlsta - @list fields changed, starting new record";
                elog_notify ( $line ) if $opt_V;
                print BALERNOTES "$line\n";
                next;
            } elsif  ( exists($pb{$dlsta}{last_reg}) && str2epoch($pb{$dlsta}{last_reg}) != $sta{last_reg} ) {
                dbputv(@db,"nreg24",$pb{$dlsta}{nreg24},"last_reg",$pb{$dlsta}{last_reg});
                elog_notify("$dlsta - nreboot and last_reg fields changed, updating record") if $opt_V;
            }
        } else {
            $Notes++ ;
            elog_notify ("\nNotification #$Notes") ;
            $line = "\n	Too many open rows for $dlsta in $dbops table stabaler." . 
                          "\n	Closing open records and adding new row";
            elog_notify ( $line );
            print BALERNOTES "$line\n";
            foreach $row (@rows) {
                $db[3] = $row;
                dbputv(@db,"endtime",($pb{$dlsta}{ptime} - 1));
            }
            dbadd(@db);
        }
        if ($pb{$dlsta}{nreg24} >= 8 ) {
            $line = "$dlsta -	$pb{$dlsta}{nreg24} baler registrations in the last 24 hours";
            elog_notify("\n	$line");
            $line =  "$dlsta	nreg24	$pb{$dlsta}{nreg24}	nmsg24	$pb{$dlsta}{nmsg24}	nusmg	";
            $line .= "$pb{$dlsta}{nusmg24}	nlog24	$pb{$dlsta}{nlog24}	ndbt24	$pb{$dlsta}{ndbt24}";
            elog_notify($line);
            print MAXREG "$line\n";
        }
    }
    elog_notify("\n\n")  if $opt_V;    
    &prettyprint(\%pb)   if $opt_V;
    elog_notify("\n\n")  if $opt_V;

    return;
}


sub pb14 { # &pb14($dlsta);

    my ($dlsta) = @_ ;
    my (@baler);
    @baler = split ( ' ', $pb{$dlsta}{string} ) ;
    $pb{$dlsta}{inp}     = $baler[7] ;
    $pb{$dlsta}{inp}     =~ s","" ;
    $pb{$dlsta}{model}   = $baler[10] ;
    $pb{$dlsta}{model}   =~ s"-.*"" ;
    $pb{$dlsta}{firm}    = $baler[10] ;
    $pb{$dlsta}{firm}    =~ s".*14-"" ;
    $pb{$dlsta}{ssident} = $baler[12] ;
    $pb{$dlsta}{ssident} =~ s":"" ;
    $pb{$dlsta}{net}     = $baler[13] ;
    $pb{$dlsta}{net}     =~ s"-.*"" ;
    $pb{$dlsta}{sta}     = $baler[13] ;
    $pb{$dlsta}{sta}     =~ s".*-"" ;
    $pb{$dlsta}{last_reg}     = join " ", $baler[15],	$baler[16] ;
    return
}

sub pb44 { # ($model,$net,$sta,$inp,$ssident,$firm,$nreboot,$last_reboot) = $pb($inp);

    my ($inp) = @_ ;
    my ($url,$good,$text,$dlsta);
    my (@text,@tmp);

    $inp =~ s/:.*//;
    $inp .= ":5384";
    elog_notify("inp	$inp") if $opt_V;

    $url = "http://$inp";
    
    ($good,@text) = &get_text($url, "stats.html") ; 

    elog_notify ("	$#text - Number of text rows\n") if $opt_V;
    
    foreach $text (@text) {

	    if ( $text =~ /PB44 Status PacketBaler44 Tag (\d+) - Station (\D\D)-(\S+)/ ) { 
            @tmp = split(" ",$text);
            $dlsta = "$2\_$3";
            $pb{$dlsta}{model} = $tmp[2];
            $pb{$dlsta}{net}   = $2;
            $pb{$dlsta}{sta}   = $3;
            $pb{$dlsta}{ssident} = $1;
            
            elog_notify ("	$dlsta	$pb{$dlsta}{net}	$pb{$dlsta}{sta}	$pb{$dlsta}{model}" . 
                         "	$pb{$dlsta}{ssident}\n") if $opt_V;
        }
	    if ( $text =~ /Copyright Quanterra, Inc. (\S+) tag (\d+) at (\S+ \S+)/ ) { 
            $pb{$dlsta}{firm} = $1;
            $pb{$dlsta}{firm} =~ s"BALER44-"";
            
            elog_notify ("	$pb{$dlsta}{firm}\n") if $opt_V;
        }
	    if ( $text =~ /last baler reboot: (\S+ \S+)   reboots: (\d+)   runtime: (\S+)/ ) { 
            $pb{$dlsta}{last_reboot} = $1;
            $pb{$dlsta}{nreboot} = $2;
            
            elog_notify ("	$pb{$dlsta}{last_reg}	$pb{$dlsta}{nreboot}\n") if $opt_V;
        }
	    if ( $text =~ /MEDIA site (\d+) (\S+ \S+) state: (\S+)  media capacity=(\S+)Mb  mediafree=(\S+)%/ ) { 
            $pb{$dlsta}{media_capacity} = $4;
            $pb{$dlsta}{media_free} = $5;
            
            elog_notify ("	$pb{$dlsta}{media_capacity}	$pb{$dlsta}{media_free}\n") if $opt_V;
        }
	    if ( $text =~ /public ip discovered: (\S+)/ ) { 
            $pb{$dlsta}{inp} = $1;
            elog_notify ("	$pb{$dlsta}{inp}\n") if $opt_V;
        }
    }
    return
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

sub sta_info { #&sta_info($orb,$pfsource);
    my ($orb,$pfsource) = @_ ;
    my ($pf,$type);
    my ($when,$src,$srcname,$pktid,$nbytes,$result,$net,$sta,$chan,$q330info,$target);
    my ($pkt,$pkttime,$loc,$suffix,$subcode,$desc,$ref);
    my (@sources,@q330info) ;
    
    @targets = ();
    %stas    = ();
    orbselect( $orb, $pfsource);
    
    ($when, @sources) = orbsources ( $orb );

    foreach $src (@sources) {
        $srcname = $src->srcname() ;
        orbselect ( $orb, $srcname ) ;
        ($pktid, $srcname, $pkttime, $pkt, $nbytes) = orbget ( $orb, "ORBNEWEST" ) ;
        elog_notify(sprintf "%s	%s", $srcname, strydtime($pkttime)) if $opt_V;
        if (!defined $pktid) {
            next ;
        }
        if ( $nbytes == 0 ) {
            next ;
        }
        ($result, $pkt) = unstuffPkt ( $srcname, $pkttime, $pkt, $nbytes ) ;

        if ( $result ne "Pkt_pf" ) {
            if( $opt_v ) {
                print "Received a $result, skipping\n" ;
            }
            next;
        }
    
        printf "%s	%s\n", $srcname, strydtime($pkttime) if $opt_V;

        ($net, $sta, $chan, $loc, $suffix, $subcode) = $pkt->parts() ;
        ($type, $desc) = $pkt->PacketType() ;

        $pf = $pkt->pf ;

        if ( defined $pf ) {
            $ref = pfget($pf, "");
            my @stas = sort keys %{$ref->{dls}};
            foreach my $sta (@stas) {

                if ($opt_n) {
                    next if ($sta !~ /^$opt_n/);
                }

                if ($opt_s) {
                    next if ($sta !~ /$opt_s/);
                }

                my @pars = sort keys %{$ref->{dls}{$sta}};
                $q330info            = $ref->{dls}{$sta}{"inp"};
                @q330info            = split (":",$q330info);
                $stas{$sta}{inp}     = $q330info[1] ;
                $stas{$sta}{ssident} = $ref->{dls}{$sta}{"sn"};
                $stas{$sta}{idtag}   = $ref->{dls}{$sta}{"pt"};
                $stas{$sta}{lat}     = $ref->{dls}{$sta}{"lat"}; 
                $stas{$sta}{lon}     = $ref->{dls}{$sta}{"lon"};
                $stas{$sta}{elev}    = $ref->{dls}{$sta}{"elev"};
                $stas{$sta}{thr}     = $ref->{dls}{$sta}{"thr"};
                $stas{$sta}{con}     = $ref->{dls}{$sta}{"con"};
                $stas{$sta}{dlt}     = $ref->{dls}{$sta}{"dlt"};
               
 
            }
        }
        $target = $srcname;
        $target =~ s"/pf/st"";
        &prettyprint($ref) if $opt_V;
        push (@targets,$target);
    }
    return;
}

sub dbdebug_table { #dbdebug(@db)
    my(@db) = @_;
    my($key,$field) ;
    my(@fields) ;
    my(%table) ;
    
    @fields = dbquery(@db,"dbTABLE_FIELDS");
    
    foreach $field (@fields) {
        elog_notify(sprintf("%s	%s",$field,dbgetv(@db,$field)));
    }
}

sub cleanup {
    close(NOQ330);
    close(NOREG);
    close(NOMSG);
    close(MAXREG);
    close(Q330NOTES);
    close(BALERNOTES);
    close(DBMASTER);
    unlink "/tmp/tmp_noq330_$$" unless $opt_V;
    unlink "/tmp/tmp_noreg_$$" unless $opt_V;
    unlink "/tmp/tmp_nomsg_$$" unless $opt_V;
    unlink "/tmp/tmp_maxreg_$$" unless $opt_V;
    unlink "/tmp/tmp_q330notes_$$" unless $opt_V;
    unlink "/tmp/tmp_balernotes_$$" unless $opt_V;
    unlink "/tmp/tmp_dbmaster_$$" unless $opt_V;
    
}