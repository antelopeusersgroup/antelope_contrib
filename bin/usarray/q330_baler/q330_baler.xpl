#   program needs:
#
# INFO      Using dlcmd will get status or configuration which is currently stored in the various q3302orb instances
# INFO      Using q330util will get the data by interogating the datalogger directly similar to willard.
#
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
#           get time of last resync
#           get number of resyncs
#           
#           identify if VIE present
#           identify if QEP present
#           get QEP base serial number (exists if QEP present)
#           get QEP serial number (exists if Aux 3 channel board present in QEP)
#           get QEP seconds since last boot
#           get QEP seconds since last resync
#           
#           get Baler mac address
#           get Baler ID1 serial number
#           get Baler ID1 state
#           get Baler ID1 amount free %
#           get Baler ID2 serial number
#           get Baler ID2 state
#           get Baler ID2 amount free %
#           get Baler config options (%40s)
#           
#           notify if (last resync time) > (last reboot time + 30 minutes)
#           
#

    use Getopt::Std ;
    use strict ;
#    use diagnostics;
    use Switch ;
    use Datascope ;
    use archive;
    use orb;
    use baler ;
    use utilfunct ;
    
    our ( $opt_v, $opt_V, $opt_D, $opt_a, $opt_m, $opt_P, $opt_n, $opt_p, $opt_q, $opt_s );
    our ( $pgm, $host );
    our ( @targets );
    our ( %pb, %pf, %q330, %stas);
        
{    #  Main program

    my ( $Pf, $cmd, $cmdorb, $dbops, $orb, $orbname, $pfsource, $problems, $stime, $subject, $subset, $success, $usage );
    
    $pgm = $0 ; 
    $pgm =~ s".*/"" ;
    elog_init($pgm, @ARGV);
    $cmd = "\n$0 @ARGV" ;
    
    if (  ! getopts('vVDabnm:M:P:p:qs:') || @ARGV != 3 ) { 
        $usage  =  "\n\n\nUsage: $0  \n	[-v] [-V] [-n] [-a] [-q]\n" ;
        $usage .=  "	[-s sta_subset] [-p pf] [-P pfsource_name] [-m mail_to_operator]  \n" ;
        $usage .=  "	status_orb cmdorb db \n\n"  ; 

        elog_notify($cmd) ; 
        elog_die ( $usage ) ; 
    }
    
    &savemail() if $opt_m ; 
    elog_notify($cmd) ; 
    announce( 0, 0 ) ;
    $stime = strydtime(now());
    chop ($host = `uname -n` ) ;
    elog_notify ("\nstarting execution on	$host	$stime\n\n");
    
    $subset     = $opt_s || ".*" ;
    $pfsource   = $opt_P || ".*/pf/st" ;
            
    $orbname    = $ARGV[0] ;
    $cmdorb     = $ARGV[1] ;
    $dbops      = $ARGV[2] ;

    $opt_v      = defined($opt_V) ? $opt_V : $opt_v ;    

    $Pf         = $opt_p || $pgm ;
    %pf         = getparam( $Pf ) ;
    
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

    &init_files ;
    
    &sta_info($orb,$pfsource);
    elog_notify("q3302orb targets -	@targets");
    
    $problems = q330_proc($cmdorb,$dbops,$subset,$problems) ;
    $problems = pb_proc($orb,$dbops,$subset) unless $opt_q;
        
    orbclose($orb);
    
    &cleanup_files;
         
    $subject = "ANF TA Q330 and Baler status";
    $cmd     = "rtmail -C -s '$subject' $pf{status_mail} < /tmp/tmp_noreg_$$";
        
    &run_cmd( $cmd ) if  ( $pf{status_mail} =~ /[A-Za-z].*/ ) ;
    
    &rm_files  unless $opt_V;
    
    $stime = strydtime(now());
    elog_notify ("completed successfully	$stime\n\n");

    $subject = sprintf("Success  $pgm  $host");
    elog_notify ($subject);
    &sendmail ( $subject, $opt_m ) if $opt_m ;
  
    exit(0);
}
 
sub get_q330_stat { #%q330stat = get_q330_stat($cmdorb,$problems);
    my ( $cmdorb, $problems ) = @_;
    my ( $Pf, $cmd, $cmd_sel, $key, $ref, $subject );
    my ( @keys ) ;
    my ( %q330stat );
        
    elog_notify("\nget_q330_stat");
    $Pf = "/tmp/TA_status";
    
    unlink "$Pf.pf" if (-e "$Pf.pf");

    foreach $cmd_sel (@targets) {
        $cmd     = "dlcmd $cmdorb $cmd_sel q330 - getstatus >> $Pf.pf 2>&1 " ; 
        if ( ! &run_cmd( $cmd ) ) {
            $subject = "Problems - $pgm $host	dlcmd getconfig failed";
            &sendmail($subject, $opt_m) if $opt_m ; 
            elog_die("\n$subject");
        }
        sleep 5;
    }

    return if $opt_n; 
    
    $ref = pfget($Pf, "");
    %q330stat = %$ref ;

    elog_debug("$Pf	ref($ref)") if $opt_D; 

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
    my ( $cmdorb, $problems ) = @_;
    my ( $Pf, $cmd, $cmd_sel, $key, $ref, $subject );
    my ( @keys ) ;
    my ( %q330config );
    
    elog_notify("\nget_q330_config");
    
    $Pf = "/tmp/TA_config";
    
    unlink "$Pf.pf" if (-e "$Pf.pf");

    foreach $cmd_sel (@targets) {
        $cmd     = "dlcmd $cmdorb $cmd_sel q330 - getconfig >> $Pf.pf 2>&1 " ; 

        if ( ! &run_cmd( $cmd ) ) {
            $subject = "Problems - $pgm $host	dlcmd getconfig failed";
            &sendmail($subject, $opt_m) if $opt_m ; 
            elog_die("\n$subject");
        }

        sleep 5;
    }

    return if $opt_n; 
    
    $ref = pfget($Pf, "");
    %q330config = %$ref ;

    elog_debug("$Pf	ref($ref)") if $opt_D;
    
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

sub q330_proc { # ($problems) = q330_proc( $cmdorb, $dbops, $subset, $problems );
    my ( $cmdorb, $dbops, $subset, $problems ) = @_;
    my ( $Notes, $ctime, $dlname, $dlsta, $endnull, $field, $filter, $ignore_sta, $key, $line );
    my ( $model, $nchange, $nconf, $nrec, $nstat, $rec, $row, $sta, $subject );
    my ( @db, @dbcal, @dbcheck, @dbdeploy, @dbdepnull, @dbdepscr, @dbnull, @dbq330, @dbscratch ) ;
    my ( @dbsq_close, @dbsq_open, @dl, @dlcom, @f, @fields, @keys, @list, @rows, @sq_close, @sq_open );
    my ( %config, %sta, %stat );

    elog_notify("\nQ330 processing    $cmdorb    $dbops    $subset    $problems ");

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
            if ( ! &run_cmd( $line ) ) {
                $problems++ ;
            }
            next;
        }
        if ($config{$key}{config}{dptokens}{4}{sta} !~ /\w+/) {
            $line = "key	$key	config-dptokens-4-net	 \"$config{$key}{config}{dptokens}{4}{net}\"	config-dptokens-4-sta	 \"$config{$key}{config}{dptokens}{4}{sta}\"";
            print Q3302ORB "$line\n";
            elog_complain($line);
            $line = "dlcmd $cmdorb $config{$key}{target} q330 $key getconfig -force",
            elog_notify($line);
            if ( ! &run_cmd( $line ) ) {
                $problems++ ;
            }

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
    @db            = dbopen(   $dbops, "r+" ) ;
    @db            = dblookup( @db, 0, "staq330", 0, 0 ) ;
    @dbscratch     = dblookup( @db, 0, 0, 0, "dbSCRATCH" ) ;
    @dbnull        = dblookup( @db, 0, 0, 0, "dbNULL" ) ;
    $endnull       = dbgetv(   @dbnull, "endtime" ) ;
    @fields        = dbquery(  @db, "dbTABLE_FIELDS" ) ;
    
    @dbdeploy      = dblookup( @db, 0, "deployment", 0, 0 ) ;
    @dbdeploy      = dbsubset( @dbdeploy, "endtime == NULL" ) ;
    @dbdeploy      = dbsubset( @dbdeploy, "snet =~ /$pf{net}/" ) ;
    @dbdepscr      = dblookup( @dbdeploy, 0, 0, 0, "dbSCRATCH" ) ;
    @dbdepnull     = dblookup( @dbdeploy, 0, 0, 0, "dbNULL" ) ;

    @dbsq_open     = dbsubset( @db, "endtime == NULL" ) ;
    @dbsq_open     = dbsort(   @dbsq_open, "dlsta", "time" ) ;
    
    $nrec          = dbquery(  @dbsq_open, "dbRECORD_COUNT" ) ;
    $ctime         = now() ;
    
    @keys = sort( keys %q330 ) ;
    elog_debug( sprintf( "%d proc dlsta", $#keys+1 ) ) if $opt_D ;
    elog_debug( sprintf( "%d rows with no endtime", $nrec ) ) if $opt_D ;
#
#  update closed stations in staq330 table
#    

    if (dbquery(@dbdeploy,"dbTABLE_PRESENT")) {
        
        @dbsq_close = dbnojoin( @dbsq_open, @dbdeploy, "sta" ) ;
        $nrec       = dbquery(  @dbsq_close, "dbRECORD_COUNT" ) ;
        
        @sq_close = ();
        STA: for ($dbsq_close[3] = 0 ; $dbsq_close[3] < $nrec ; $dbsq_close[3]++) {
            $sta = dbgetv( @dbsq_close, "sta" ) ;
            push( @sq_close, $sta ) ;
            foreach $ignore_sta ( @{$pf{ignore_sta}} ) {
                if ( $ignore_sta =~ /$sta/ ) {
                    elog_notify "$sta ignored, not closed" if $opt_V ;
                    next STA;
                }
            }
            dbputv( @dbsq_close, "endtime", $ctime ) ;
        }
        
        if ( $nrec ) {
            @sq_close = sort ( @sq_close ) ;
            $line = "$nrec open records in staq330 nojoin to open records in deployment - @sq_close" ;
            elog_notify ( "\n$line" ) ;
            print Q330NOTES "$line\n";
        }

        @dbsq_open  = dbjoin(     @dbsq_open, @dbdeploy, "sta" ) ;
        @dbsq_open  = dbseparate( @dbsq_open, "staq330" ) ;
        @dbcheck    = dbnojoin(   @dbdeploy, @dbsq_open, "sta" ) ;  
        $nrec       = dbquery(    @dbcheck, "dbRECORD_COUNT" ) ;

        @sq_open = ();
        for ( $dbcheck[3] = 0 ; $dbcheck[3] < $nrec ; $dbcheck[3]++) {
            push( @sq_open, dbgetv( @dbcheck, "sta" ) ) ; 
        }

        if ( $nrec ) {
            @sq_open = sort ( @sq_open ) ;
            $line = "$nrec open records in deployment nojoin to open records in staq330 - @sq_open"  ;
            elog_notify ( $line ) ;
            print Q330NOTES "$line\n";
        }

    }
#
#  update station data in staq330 table
#
    foreach $dlsta ( @keys ) {
        elog_debug ( "dlsta	$dlsta" )  if $opt_D ;
        elog_debug ( sprintf("time	%s	endtime	%s", strydtime( $q330{$dlsta}{time} ), strydtime( $endnull ) ) ) if $opt_D ;
        
        if ( $q330{$dlsta}{sta} =~ /EXMP/ ) {
            $line = "$dlsta Q330 has internal station code of $q330{$dlsta}{sta} "  ;
            elog_notify ( $line ) ;
            print Q330NOTES "$line\n" ;
        }

        dbput(@dbscratch,dbget(@dbnull));
        dbputv(@dbscratch, "dlsta",          $dlsta,
                           "time",           $q330{$dlsta}{time},
                           "endtime",        $endnull,
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
        @rows = dbmatches( @dbscratch, @db, "q330", "dlsta", "endtime" );
        elog_debug ( "#rows	$#rows" ) if $opt_D ;
        
        if ($#rows == -1) {
            dbadd(@db) ;
            elog_notify("	Adding	$dlsta") if $opt_V ;
            $Notes++ ;
            elog_notify ("\nNotification #$Notes") if $opt_V ;
            $line = "$dlsta added to staq330 table" ;
            elog_notify ( $line ) ; 
            print Q330NOTES "$line\n" ;
        } elsif ($#rows == 0) {
            elog_debug ( "row	$rows[0]" ) if $opt_D ;

            $db[3] = $rows[0] ;

            elog_debug ( sprintf ("dlsta	%s	time	%s	endtime	%s", dbgetv( @db, "dlsta" ), strydtime( dbgetv( @db, "time") ), strydtime( dbgetv( @db, "endtime") ) ) ) if $opt_D ;
            %sta     = () ;
            $nchange = 0 ;
            @list    = () ;
            foreach $field (@fields) {
                elog_notify( "	$field" ) if $opt_V ;
                $sta{$field} = dbgetv( @db, $field ) ;
                if ( $field =~ /endtime/ && $sta{$field} != $endnull ) {
                    elog_notify ( sprintf ("dlsta  %s q330 match has time    %s    endtime    %s,  reopening", $dlsta, strydtime( dbgetv( @db, "time") ), strydtime( dbgetv( @db, "endtime") ) ) ) ;
                    $nchange++;
                }
                next if ($field =~ /dlsta|time|endtime|lddate|LP1_buf|LP2_buf|LP3_buf|LP4_buf/);
                
                elog_notify("	$dlsta	$field	$q330{$dlsta}{$field}	$sta{$field}") if $opt_V;
                if ($q330{$dlsta}{$field} != $sta{$field} && $field =~ /nchan|memory_size|nreboot|last_reboot/ ) {
                    elog_notify("	$dlsta	$field	q330	$q330{$dlsta}{$field}	db	$sta{field}")  if $opt_V;
                    push(@list,$field);
                    $nchange++;
                }
                if ($q330{$dlsta}{$field} !~ /$sta{$field}/ && $field !~ /nchan|memory_size|nreboot|last_reboot/ ) {
                    elog_notify("	$dlsta	$field	q330	$q330{$dlsta}{$field}	db	$sta{field}") if $opt_V;
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
                elog_notify ( $line ) ; # if $opt_V;
                print Q330NOTES "$line\n";
                next;
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
        ($dlsta,$dlname,$model,$filter) = dbgetv(@dbq330,"dlsta","calibration.dlname","model","ch1_3_filter") ;
        next if ( $dlname =~ /\-/ ) ; 
        @dl = split(/_/,$dlname) ;
        @f  = split(/ /,$filter) ;
        if ($model !~ /^$dl[0]$/) {
            $line = sprintf("$dlsta calibration table dlname - %-25s |	q330 model  - 	%s",$dlname,$model) ;
            elog_notify($line) ;
            print DBMASTER "$line\n" ;
        } 
        if ($dlname =~ /.*below.*/) {
            if ( $dlname !~ /.*$f[4].*/ ) {
                $line = sprintf("$dlsta calibration table dlname - %-25s |	q330 filter - 	%s",$dlname,$filter) ;
                elog_notify($line) ;
                print DBMASTER "$line\n" ;
            } 
        } else {
            if ($filter =~ /below/ ) {
                $line = sprintf("$dlsta calibration table dlname - %-25s |	q330 filter - 	%s",$dlname,$filter) ;
                elog_notify($line) ;
                print DBMASTER "$line\n" ;
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
    my ( $orb,$dbops,$subset ) = @_ ;
    my ( $Notes, $chan, $ctime, $desc, $dlsta, $endnull, $field, $ignore_sta, $line, $loc, $n ) ;
    my ( $nbytes, $nchange, $net, $npkts, $nrec, $packet, $pkt, $pktid, $pkttime, $result, $row ) ;
    my ( $source, $srcname, $sta, $strtmp, $subcode, $suffix, $type, $when );
    my ( @baler, @db, @dbcheck, @dbdeploy, @dbdepnull, @dbdepscr, @dbnull, @dbpb_close, @dbpb_open ) ;
    my ( @dbscratch, @fields, @keys, @list, @noq330, @pb_close, @pb_open, @q330, @rows, @sources ) ;
    my ( %sta );
    
    elog_notify( "\nBaler processing" );
#
#  get log packets from status orb
#
    $srcname = $subset . "/log" ;

    elog_notify( "orb	$orb	srcname	$srcname	" ) if $opt_V ;
    
    orbreject( $orb, $pf{baler_orb_reject} ) ;
    
    $npkts = orbselect( $orb, $srcname ) ;
    ( $when, @sources ) = orbsources( $orb ) ;
    
    $pktid = orbafter( $orb, ( now() - 86400 ) );
    $npkts = 0 ;
    
    foreach $source ( @sources ) {
        elog_notify (sprintf "%-15s %8d %6.0f %s",
                $source->srcname, $source->npkts, $source->nbytes/1024.,
                strtdelta($when-$source->slatest_time) ) if $opt_D ;
        $npkts += $source->npkts ;
        ($net,$sta,$chan,$loc,$suffix,$subcode) = split_srcname($source->srcname) ;
        elog_debug("$net	$sta	$chan	$loc	$suffix	$subcode") if $opt_D ;
        $dlsta = join( "_", $net, $sta ) ;
        elog_debug( "dlsta	$dlsta" ) if $opt_D;
        $pb{$dlsta}{nreg24}  = 0 ;
        $pb{$dlsta}{nmsg24}  = 0 ;
        $pb{$dlsta}{nusmg24} = 0 ;
        $pb{$dlsta}{nlog24}  = 0 ;
        $pb{$dlsta}{ndbt24}  = 0 ;
    }

    $line =  "\nBaler processing - number of log packets in status orb over past 24 hours:" ;
    $line .= "	$npkts	\n" ;
    elog_notify($line) ;

    return if $opt_n ;
#
#  load %pb
#
    $n = 0;
    while ( $pktid > -1 ) {
        $n++;
        ( $pktid, $source, $pkttime, $packet, $nbytes ) = orbget( $orb, $pktid ) ;
        elog_debug( "$n	pktid	$pktid	srcname	$srcname	source	$source	time	$pkttime	" . 
                    "nbytes	$nbytes" ) if $opt_D ;
        
        ( $net, $sta, $chan, $loc, $suffix, $subcode ) = split_srcname( $source ) ;
        elog_debug( "$net	$sta	$chan	$loc	$suffix	$subcode" ) if $opt_D ;
        
        $dlsta = join( "_", $net, $sta );
        elog_debug( "dlsta	$dlsta" ) if $opt_D;
        
        ( $result, $pkt ) = unstuffPkt( $srcname, $pkttime, $packet, $nbytes ) ;
        ( $type, $desc ) = $pkt->PacketType() ;
    
        $strtmp = $pkt->string ;
        if ( defined $strtmp ) {
            elog_debug("strtmp	$strtmp") if ( ( $opt_a && ( $strtmp =~ /tag/ ) ) || $opt_D ) ;
            if ( $strtmp =~ /tag/ ) {
                $pb{$dlsta}{string} = $strtmp ;
                $pb{$dlsta}{nreg24}++ ;
                $pb{$dlsta}{ptime} = $pkttime ;
            }
            if ( $strtmp =~ /127.0.0.1/ ) {
                $pb{$dlsta}{string} = $strtmp ;
                $pb{$dlsta}{ptime} = $pkttime ;
            }
            if ( $strtmp =~ /UMSG/ ) {
                elog_notify($strtmp) if $opt_V ;
                $pb{$dlsta}{nusmg24}++ ;
            }
            if ( $strtmp =~ /LOG/ ) {
                $pb{$dlsta}{nlog24}++ ;
            }
            if ( $strtmp =~ /DEBUGT/ ) {
                $pb{$dlsta}{ndbt24}++ ;
            }
            if ( $strtmp =~ /UMSG: ip = 20.210.0.1/ && $strtmp =~ /UMSG: ip = registered/ ) {
                @baler = split ( ' ', $strtmp ) ;
                $pb{$dlsta}{last_reg}     = join " ", $baler[0],	$baler[1] ;
            }
            $pb{$dlsta}{nmsg24}++;
        }
        
        $pktid = orbseek( $orb, "ORBNEXT" ) ;
        elog_debug( "$n	pktid	$pktid" ) if $opt_D;

    }

    elog_notify ( "baler data from orb loaded" ) if $opt_v ;
    elog_debug( "\n\n		TEST" )   if $opt_D;    
    &prettyprint( \%pb )              if $opt_D;
    elog_debug( "\n\n		" )       if $opt_D;
#
#  Compare keys of %q330 and %pb
#
    @keys   = sort( keys %pb );
    @noq330 = ();
    @q330   = ();
    foreach $dlsta (@keys) {
        push( @noq330, $dlsta ) unless exists $q330{$dlsta};
        push( @q330, $dlsta )   if     exists $q330{$dlsta};        
    }
    @keys = @q330 ;
    
    elog_notify( "Baler info with no Q330 - 	@noq330" ) if $opt_V ; # can happen if source name status orb is > 1 day old like when a station was pulled in the last week

#
#  Open db to update stabaler table
#
    @db        = dbopen(   $dbops, "r+" ) ;
    @db        = dblookup( @db, 0, "stabaler", 0, 0 ) ;
    @dbscratch = dblookup( @db, 0, 0, 0, "dbSCRATCH" ) ;
    @dbnull    = dblookup( @db, 0, 0, 0, "dbNULL" ) ;
    $endnull   = dbgetv(   @dbnull, "endtime" ) ;
    @fields    = dbquery(  @db, "dbTABLE_FIELDS" ) ;

    @dbdeploy  = dblookup( @db, 0, "deployment", 0, 0 ) ;
    @dbdeploy  = dbsubset( @dbdeploy, "endtime == NULL" ) ;
    @dbdeploy  = dbsubset( @dbdeploy, "snet =~ /$pf{net}/" ) ;
    @dbdepscr  = dblookup( @dbdeploy, 0, 0, 0, "dbSCRATCH" ) ;
    @dbdepnull = dblookup( @dbdeploy, 0, 0, 0, "dbNULL" ) ;

    elog_debug( "\n\n		keys	@keys" ) if $opt_D ;
    
    @dbpb_open  = dbsubset( @db, "endtime == NULL" ) ;
    @dbpb_open  = dbsort(   @dbpb_open, "dlsta", "time" ) ;
    
    $nrec       = dbquery( @dbpb_open, "dbRECORD_COUNT" ) ;
    $ctime      = now() ;
    
    elog_notify( sprintf( "%d proc dlsta", $#keys+1 ) ) ;
    elog_debug(  sprintf( "%d rows with no endtime", $nrec ) ) if ( $opt_D || $opt_V ) ;
#
#  update closed stations in stabaler table
#

    if (dbquery(@dbdeploy,"dbTABLE_PRESENT")) {

        @dbpb_close = dbnojoin( @dbpb_open, @dbdeploy, "sta" ) ;
        $nrec       = dbquery(  @dbpb_close, "dbRECORD_COUNT" ) ;
        
        elog_debug ( "$nrec records in dbpb_close" ) if $opt_D ; 
        
        @pb_close = ();
        STA: for ($dbpb_close[3] = 0 ; $dbpb_close[3] < $nrec ; $dbpb_close[3]++) {
            $sta = dbgetv( @dbpb_close, "sta" ) ;
            push( @pb_close, $sta ) ;
            foreach $ignore_sta ( @{$pf{ignore_sta}} ) {
                if ( $ignore_sta =~ /$sta/ ) {
                    elog_notify "$sta ignored, not closed" if $opt_V ;
                    next STA;
                }
            }
            dbputv( @dbpb_close, "endtime", $ctime ) ;
        }
        
        if ( $nrec ) {
            @pb_close = sort ( @pb_close ) ;
            $line = "$nrec open records in stabaler nojoin to open records in deployment - @pb_close" ;
            elog_notify ( $line ) ;
            print BALERNOTES "$line\n";
        }

        @dbpb_open  = dbjoin(  @dbpb_open, @dbdeploy, "sta" ) ;
        $nrec       = dbquery( @dbpb_open, "dbRECORD_COUNT" ) ;
        elog_debug ( "$nrec records in dbpb_open" ) if $opt_D ; 
        
        @dbpb_open  = dbseparate( @dbpb_open, "stabaler" ) ;
        @dbcheck    = dbnojoin(   @dbdeploy, @dbpb_open, "sta" ) ;  
        $nrec       = dbquery(    @dbcheck, "dbRECORD_COUNT" ) ;

        @pb_open = ();
        for ( $dbcheck[3] = 0 ; $dbcheck[3] < $nrec ; $dbcheck[3]++) {
            push( @pb_open, dbgetv( @dbcheck, "sta" ) ) ; 
        }
        
        if ( $nrec ) {
            @pb_open = sort ( @pb_open ) ;
            $line = "$nrec open records in deployment nojoin to open records in stabaler - @pb_open"  ;
            elog_notify ( $line ) ;
            print BALERNOTES "$line\n";
        }
    }
#
#  update station baler data in stabaler table
#
    foreach $dlsta (@keys) {
        dbput( @dbscratch, dbget(@dbnull) ) ;
        if ( $pb{$dlsta}{string} =~ /tag/ ) {
            &pb14( $dlsta ) ;
        }
        if ( $pb{$dlsta}{string} =~ /127.0.0.1/ ) {
            &pb44( $q330{$dlsta}{inp} ) ;
        }
        elog_debug( "$dlsta	$pb{$dlsta}{net}	$pb{$dlsta}{sta}	$pb{$dlsta}{inp}	" .	
        "$pb{$dlsta}{model}	$pb{$dlsta}{ssident}	$pb{$dlsta}{firm}	$pb{$dlsta}{last_reg}" ) if ( $opt_D || $opt_V ) ;
        if ( $pb{$dlsta}{nreg24} == 0 ) {
            $line = "$dlsta -	No baler registrations in the last 24 hours, $pb{$dlsta}{nusmg24} umsgs" ;
            elog_notify( "\n$line" ) ;
            $line  = "$dlsta	nreg24	$pb{$dlsta}{nreg24}	nmsg24	$pb{$dlsta}{nmsg24}	nusmg24	" ;
            $line .= "$pb{$dlsta}{nusmg24}	nlog24	$pb{$dlsta}{nlog24}	ndbt24	$pb{$dlsta}{ndbt24}" ;
            $line .= sprintf( "	data latency %s", strtdelta($stas{$dlsta}{dlt} ) ) ;
            elog_notify($line) ;
        
            if ( $pb{$dlsta}{nusmg24} ) {
                print NOREG "$line\n" if (exists($pb{$dlsta}{nreg24}) && $pb{$dlsta}{model} !~ /Baler44/) ;
            } else {
                $line = "$dlsta -	No baler registrations and no UMSG messages in the last 24 hours" ;
                $line .= sprintf( " -	data latency %s", strtdelta($stas{$dlsta}{dlt} ) ) ;
                print NOMSG "$line\n";
                elog_notify( "$line" ) ;
            }
        }
        if ($pb{$dlsta}{ssident} < 1) {
            elog_notify( "$dlsta	- No Baler information" ) ;
            next ;
        }
        dbputv(@dbscratch, "dlsta",          $dlsta,
                           "time",           $pb{$dlsta}{ptime},
                           "endtime",        $endnull,
                           "net",            $pb{$dlsta}{net},
                           "sta",            $pb{$dlsta}{sta},
                           "inp",            $pb{$dlsta}{inp},
                           "model",          $pb{$dlsta}{model},
                           "ssident",        $pb{$dlsta}{ssident},
                           "nreg24",         $pb{$dlsta}{nreg24},
                           "firm",           $pb{$dlsta}{firm} );
        if ( exists( $pb{$dlsta}{last_reg} ) ) {
            dbputv( @dbscratch, "last_reg", str2epoch($pb{$dlsta}{last_reg} ) );
        }
        if ( exists( $pb{$dlsta}{last_reboot} ) ) {
            dbputv( @dbscratch, "last_reboot", $pb{$dlsta}{last_reboot} ) ;
        }
        if ( exists( $pb{$dlsta}{nreboot} ) ) {
            dbputv( @dbscratch, "nreboot", $pb{$dlsta}{nreboot} ) ;
        }
        @rows = dbmatches( @dbscratch, @db, "pb", "dlsta", "endtime" ) ;
        if ( $#rows == -1 ) {
            dbadd( @db );
            elog_debug( "	Adding	$dlsta" ) if ( $opt_D || $opt_V ) ;
            $Notes++ ;
            elog_debug ( "\nNotification #$Notes" ) if ( $opt_D || $opt_V ) ;
            $line = "$dlsta added to stabaler table" ;
            elog_notify ( $line ) if $opt_V ;
            print BALERNOTES "$line\n" ;
        } elsif ( $#rows == 0 ) {
            $db[3] = $rows[0] ;
            %sta = () ;
            $nchange = 0 ;
            @list = () ;
            foreach $field ( @fields ) {
                $sta{$field} = dbgetv( @db, $field ) ;
                if ( $field =~ /endtime/ && $sta{$field} != $endnull ) {
                    elog_notify ( sprintf ("dlsta  %s baler match has time    %s    endtime    %s,  reopening", $dlsta, strydtime( dbgetv( @db, "time") ), strydtime( dbgetv( @db, "endtime") ) ) ) ;
                    $nchange++;
                }
                elog_debug( "	$field	baler	$pb{$dlsta}{$field}	db	$sta{$field}" ) if ( $opt_D || $opt_V ) ;
                next if ( $field =~ /dlsta|time|endtime|lddate|nreg24|last_reg/ ) ;
                next if ( $field =~ /nreboot|last_reboot/ && $pb{$dlsta}{model} =~ /Baler14/ ) ;
                next if ( $field =~ /last_reboot/ && str2epoch($pb{$dlsta}{last_reboot}) == $sta{$field} ) ;
                if ( $pb{$dlsta}{$field} !~ /$sta{$field}/ ) {
                    elog_debug( "	$field	baler	$pb{$dlsta}{$field}	db	$sta{$field}" ) ; # if ( $opt_D || $opt_V ) ;
                    push( @list, $field ) ;
                    $nchange++ ;
                }
            }
            if ( $nchange ) {
                dbputv( @db, "endtime", ( $pb{$dlsta}{ptime} - 1 ) ) ;
                dbadd( @db );
                $Notes++ ;
                elog_debug ("\nNotification #$Notes")  if ( $opt_D || $opt_V ) ;
                $line = "$dlsta - @list fields changed, starting new record" ;
                elog_notify ( $line ) if $opt_V ;
                print BALERNOTES "$line\n" ;
                next ;
            } elsif  ( exists( $pb{$dlsta}{last_reg} ) && str2epoch( $pb{$dlsta}{last_reg} ) != $sta{last_reg} ) {
                dbputv( @db, "nreg24", $pb{$dlsta}{nreg24}, "last_reg", $pb{$dlsta}{last_reg} ) ;
                elog_debug( "$dlsta - nreboot and last_reg fields changed, updating record" ) if ( $opt_D || $opt_V ) ;
            }
        } else {
            $Notes++ ;
            elog_notify ( "\nNotification #$Notes" ) ;
            $line = "\n	Too many open rows for $dlsta in $dbops table stabaler." . 
                          "\n	Closing open records and adding new row" ;
            elog_notify ( $line ) ;
            print BALERNOTES "$line\n" ;
            foreach $row ( @rows ) {
                $db[3] = $row ;
                dbputv( @db, "endtime", ( $pb{$dlsta}{ptime} - 1 ) ) ;
            }
            dbadd( @db ) ;
        }
        if ( $pb{$dlsta}{nreg24} >= 8 ) {
            $line = "$dlsta -	$pb{$dlsta}{nreg24} baler registrations in the last 24 hours" ;
            elog_notify("\n	$line") ;
            $line =  "$dlsta	nreg24	$pb{$dlsta}{nreg24}	nmsg24	$pb{$dlsta}{nmsg24}	nusmg	" ;
            $line .= "$pb{$dlsta}{nusmg24}	nlog24	$pb{$dlsta}{nlog24}	ndbt24	$pb{$dlsta}{ndbt24}" ;
            elog_notify( $line ) ;
            print MAXREG "$line\n" ;
        }
    }
    elog_notify("\n\n")  if $opt_V ;    
    &prettyprint(\%pb)   if $opt_V ;
    elog_notify("\n\n")  if $opt_V ;

    return;
}

sub pb14 { # &pb14( $dlsta );
    my ( $dlsta ) = @_ ;
    my ( @baler ) ;
    
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

    my ( $inp ) = @_ ;
    my ( $url, $good, $text, $dlsta );
    my ( @text, @tmp );

    $inp =~ s/:.*//;
    $inp .= ":5381";
    elog_debug("pb44 inp	$inp") if ( $opt_D || $opt_V );

    $url = "http://$inp";
    
    ($good,@text) = &get_text($url, "stats.html") ; 

    elog_debug ("	$#text - Number of text rows\n") if ( $opt_D || $opt_V );
    
    foreach $text (@text) {
        elog_debug ("	$text") if ( $opt_D || $opt_V );

	    if ( $text =~ /PB44 Status PacketBaler44 Tag (\d+) - Station (\D\D)-(\S+)/ ) { 
            @tmp = split(" ",$text);
            $dlsta = "$2\_$3";
            $pb{$dlsta}{model} = $tmp[2];
            $pb{$dlsta}{net}   = $2;
            $pb{$dlsta}{sta}   = $3;
            $pb{$dlsta}{ssident} = $1;
            
            elog_debug ("	$dlsta	$pb{$dlsta}{net}	$pb{$dlsta}{sta}	$pb{$dlsta}{model}" . 
                         "	$pb{$dlsta}{ssident}\n") if ( $opt_D || $opt_V );
        }
	    if ( $text =~ /Copyright Quanterra, Inc. (\S+) tag (\d+) at (\S+ \S+)/ ) { 
            $pb{$dlsta}{firm} = $1;
            $pb{$dlsta}{firm} =~ s"BALER44-"";
            
            elog_debug ("	$pb{$dlsta}{firm}\n") if ( $opt_D || $opt_V );
        }
	    if ( $text =~ /last baler reboot: (\S+ \S+)   reboots: (\d+)   runtime: (\S+)/ ) { 
            $pb{$dlsta}{last_reboot} = $1;
            $pb{$dlsta}{nreboot} = $2;
            
            elog_debug ("	$pb{$dlsta}{last_reg}	$pb{$dlsta}{nreboot}\n") if ( $opt_D || $opt_V );
        }
	    if ( $text =~ /MEDIA site (\d+) (\S+ \S+) state: (\S+)  media capacity=(\S+)Mb  mediafree=(\S+)%/ ) { 
            $pb{$dlsta}{media_capacity} = $4;
            $pb{$dlsta}{media_free} = $5;
            
            elog_debug ("	$pb{$dlsta}{media_capacity}	$pb{$dlsta}{media_free}\n") if ( $opt_D || $opt_V );
        }
	    if ( $text =~ /public ip discovered: (\S+)/ ) { 
            $pb{$dlsta}{inp} = $1;
            elog_debug ("	$pb{$dlsta}{inp}\n") if ( $opt_D || $opt_V );
        }
    }
    return
}

sub sta_info { #&sta_info($orb,$pfsource);
    my ( $orb, $pfsource ) = @_ ;
    my ( $chan, $desc, $loc, $nbytes, $net, $pf, $pkt, $pktid, $pkttime, $q330info, $ref, $result ) ;
    my ( $src, $srcname, $sta, $subcode, $suffix, $target, $type, $when );
    my ( @q330info, @sources ) ;
    
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

sub init_files {
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
}

sub cleanup_files {
    my ($cmd) ; 
    
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
}

sub rm_files {
    unlink "/tmp/tmp_noq330_$$" ;
    unlink "/tmp/tmp_q3302orb_$$" ;
    unlink "/tmp/tmp_noreg_$$" ;
    unlink "/tmp/tmp_nomsg_$$" ;
    unlink "/tmp/tmp_maxreg_$$" ;
    unlink "/tmp/tmp_q330notes_$$" ;
    unlink "/tmp/tmp_balernotes_$$" ;
    unlink "/tmp/tmp_dbmaster_$$" ;
}