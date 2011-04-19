#
#   rsync_baler: script to create a local copy of remote baler
#   author: Juan C. Reyes
#   email:  reyes@ucsd.edu
#   No BRTT support
#

#{{{
use strict "vars";
# we want to build vars on the fly. Cannot use strict subs.
#use strict "subs";
use warnings;
use Fcntl;
use POSIX;
use Socket;
use sysinfo;
use Net::FTP;
use Datascope;
use Pod::Usage;
use IO::Handle;
use File::Spec;
use File::Copy;
use Getopt::Std;
use File::Fetch;
use List::Util qw[max min];
use IPC::Cmd qw[can_run run];

our($opt_b,$opt_x,$opt_j,$opt_f,$opt_r,$opt_s,$opt_h,$opt_v,$opt_m,$opt_p,$opt_V,$opt_R);
our(%pf,@db,@db_sta,@db_ip,@db_on,$dbname,$dbpath);
our($dbout,$local_path,$start_of_report,@dbr,$nrecords);
our($station,@errors,%table,$time,$dfile,$bandwidth,$media);
our($parent,$reserve_media,$total_bytes,$bytes);
our($sys_path,$temp_sta,$ps_path);
our($start,$end,$run_time,$run_time_str,$type);
our($sta,@stas,$active_pids,$stations,$table,$folder);
our($pid,$log,$address,$ip_sta);
our($host,$key,$value,$file_fetch);
our($dlsta,$net,$ip);
our($Problems,$problems_hash,$prob,$txt);
our($to_parent);
our($oldout,$olderr,$lsof,$ulimit);

use constant false => 0;
use constant true  => 1;

select STDOUT; $| = 1;
select STDERR; $| = 1;
#}}}

#
#  Program setup
#
#{{{

    elog_init($0, @ARGV);
    $parent = $$;
    $start = now();
    $host = my_hostname();

    unless ( &getopts('bj:fxhVvm:p:s:r:R') || @ARGV > 0 ) { 
        pod2usage({-exitval => 2,
                   -verbose => 2});
    }

    #
    # Print help and exit
    #
    pod2usage({-exitval => 2, -verbose => 2}) if $opt_h;

    #
    # Initialize  mail
    #
    if ($opt_m){
        debug("Initialize mail") if $opt_V;
        savemail();
    }

    logging('');
    logging("$0 @ARGV");
    logging("Starting execution at ".strydtime(now())." on ".my_hostname());
    logging('');
    logging('');

    #
    # Implicit flag
    #
    $opt_v = defined($opt_V) ? $opt_V : $opt_v ;
    $opt_p ||= "rsync_baler.pf" ;

    #
    # Get parameters from config file
    #
    debug("Getting params") if $opt_V;
    %pf = getparam($opt_p);

    #
    ## Set File::Fetch options
    #
    $File::Fetch::WARN    = 0 unless $opt_V; 
    $File::Fetch::DEBUG   = 1 if $opt_V; 
    $File::Fetch::TIMEOUT = $pf{download_timeout};
    #$File::Fetch::BLACKLIST = [qw/lwp netftp lftp lynx iosock ncftp/];
    #   File::Fetch
    #   Below is a mapping of what utilities will be used in what order for what schemes, if available:
    #       file    => LWP, lftp, file
    #       http    => LWP, wget, curl, lftp, lynx, iosock
    #       ftp     => LWP, Net::FTP, wget, curl, lftp, ncftp, ftp
    #       rsync   => rsync

    #
    ## Set IPC::Cmd options
    #
    $IPC::Cmd::VERBOSE = 1 if $opt_V;

    #
    ## Get system $PATH
    #
    $sys_path = File::Spec->path();

    #
    # We want access to ulimit function
    #
    $ulimit = can_run('ulimit') or log_die("'ulimit' missing in PATH:[$sys_path]");

    #
    # We want access to lsof
    #
    $lsof = can_run('lsof') or log_die("'lsof' missing in PATH:[$sys_path]");

    #
    # Check if we have access to extra software: {msfixoffsets} 
    #
    if ($pf{fix_mseed_cmd}) {
        $ps_path   = can_run('msfixoffsets') or log_die("'msfixoffsets' missing in PATH:[$sys_path]");
        debug("\tmsfixoffsets path=$ps_path") if $opt_V;
    }
    else{ debug("\tNot running msfixoffsets...(edit PF file to enable)") if $opt_V; }

    #
    # Verify Database
    #
    debug("Opening $pf{database}:") if $opt_V;

    @db = dbopen ( $pf{database}, "r" ) or log_die("Can't open DB: $pf{database}"); 

    # Open table for list of valid stations 
    @db_on = dblookup(@db, "", "deployment" , "", "");
    table_check(\@db_on);

    # Open table for list of station types ie 'PacketBaler44'
    @db_sta = dblookup(@db, "", "stabaler", "", "");
    table_check(\@db_sta);

    # Open table for list of current ips
    @db_ip = dblookup(@db, "", "staq330" , "", "");
    table_check(\@db_ip);

    #
    # Verify access to directory
    #
    log_die("Can't access dir => $pf{local_data_dir}.") unless -e $pf{local_data_dir};

    #
    # Build absolute path for the output json file
    #
    if ( $opt_j ) {
        $opt_j = File::Spec->rel2abs( $opt_j ); 
        debug("Write table in json file: $opt_j") if $opt_V;
    }

#}}}

#
#  Set mode [Report, JSON or Retrieval]
#
#{{{

    debug('Get list of stations:') if $opt_V;
    $stations = get_stations_from_db(); 

    #
    # Report and/or JSON file export
    #
    if ( $opt_R || $opt_j ) { json_and_report($stations); }

    #
    # Run this part for correcting the databases
    #
    elsif ( $opt_x ) { run_in_threads($stations,"clean_db"); }

    #
    # TEST PIPES 
    #
    elsif ( $opt_b ) { run_in_threads($stations, "test_pipes"); }

    #
    # Get data from the stations
    #
    else { run_in_threads($stations,"get_data"); }

    problem_print();

    #
    # Calc total time for script
    #
    $end = now();
    $run_time = $end - $start;
    $start = strydtime($start);
    $end = strydtime($end);
    $run_time_str = strtdelta($run_time);

    logging("Start: $start End: $end");
    logging("Runtime: $run_time_str");
    sendmail() if $opt_m; 

    exit 0;

#}}}

sub get_stations_from_db {
#{{{
    my ($dlsta,$vnet,$net,$sta,$time,$endtime);
    my %sta_hash;
    my @db_1;
    my $nrecords;
    my $ip;

    #
    # Get stations with baler44s
    #
    logging("dbsubset ( stablaler.model =~ /Packet Baler44/)") if $opt_v;
    @db_1 = dbsubset ( @db_sta, "stabaler.model =~ /PacketBaler44/ ");

    logging("dbsubset ( sta =~ /$opt_s/)") if $opt_v && $opt_s;
    @db_1 = dbsubset ( @db_1, "sta =~ /$opt_s/") if $opt_s;

    logging("dbsubset ( sta !~ /$opt_s/)") if $opt_v && $opt_r;
    @db_1 = dbsubset ( @db_1, "sta !~ /$opt_r/") if $opt_r;

    $nrecords = dbquery(@db_1,dbRECORD_COUNT) or log_die("No records to work with after dbsubset()"); 
    logging("dbsubset => nrecords = $nrecords") if $opt_v;


    for ( $db_1[3] = 0 ; $db_1[3] < $nrecords ; $db_1[3]++ ) { 

        ($dlsta,$net,$sta,$time,$endtime) = dbgetv(@db_1, qw/dlsta net sta time endtime/); 

        debug("[$sta] [$net] [$dlsta] [$time] [$endtime]") if $opt_V;

        $sta_hash{$sta}{dlsta}      = $dlsta; 
        $sta_hash{$sta}{net}        = $net; 
        $sta_hash{$sta}{status}     = 'Decom'; 
        $sta_hash{$sta}{ip}         = 0; 

        push @{ $sta_hash{$sta}{dates} }, [$time,$endtime];

    }


    foreach $sta (sort keys %sta_hash) {

        $dlsta = $sta_hash{$sta}{dlsta};
        $net   = $sta_hash{$sta}{net};

        #
        # Test if station is active
        #
        $sta_hash{$sta}{status} = 'Active' if ( dbfind(@db_on, "sta =~ /$sta/ && snet =~ /$net/ && endtime == NULL", -1)>= 0);

        next if $sta_hash{$sta}{status} eq 'Decom';

        #
        # Get ip for station
        #
        $db_ip[3] = dbfind ( @db_ip, " dlsta =~ /$dlsta/ && endtime == NULL",-1);

        if ( $db_ip[3] >= 0 ) {

            $ip = dbgetv(@db_ip, qw/inp/); 

            # regex for the ip
            $ip =~ /([\d]{1,3}\.[\d]{1,3}\.[\d]{1,3}\.[\d]{1,3})/;
            problem("Failed grep on IP $pf{database}.stabaler{inp}->(ip'$ip',dlsta'$dlsta')") unless $1;
            $sta_hash{$sta}{ip} = $1 if $1; 

        }

        logging("$dlsta $sta_hash{$sta}{status} $sta_hash{$sta}{ip}") if $opt_v; 

        foreach (sort @{$sta_hash{$sta}{dates}}) { debug("\t\t@$_")if $opt_V; }

    }

    #
    # Try open each database and create if missing
    #
    foreach (sort keys %sta_hash) { open_db($_,1); }

    eval { dbclose(@db_sta); };
    eval { dbclose(@db_ip);  };
    eval { dbclose(@db_on);  };

    return \%sta_hash;
#}}}
}

sub json_and_report {
#{{{
    $stations = shift;
    my ($kilos,$megas,@bw);
    my ($dfile, $media,$status);
    my ($errors, $bytes, $bandwidth,@extra,%extra);
    my (@total,%total,@flagged,@downloaded,@missing,%missing,$ratio);
    my ($report,$text,$time, $endtime);
    my (@dbr,@dbr_temp,@queries);
    my ($install_date,$remove_date,$regex);
    my ($start_year,$start_month,$end_year,$end_month);
    my ($total_bytes);
    my ($count,$average,$median);
    my $bandwidth_low;
    my $bandwidth_high;
    my $start_of_report;;


    # 
    # Clear file for data dump
    #
    unlink($opt_j) if ($opt_j && -e $opt_j);

    open ( JSON, ">$opt_j") if $opt_j;

    $text   =  "";
    $report =  "";

    foreach $temp_sta ( sort keys %$stations ) {

        debug("Now report on $temp_sta.") if $opt_V;

        #logging("$report\n");
        $report =  "";

        #
        # clean vars
        #
        undef @bw;
        undef $errors;
        undef $average;
        undef $median;
        undef $kilos;
        undef $megas;
        undef $dfile;
        undef $media;
        undef $status;
        undef $bytes;
        undef $bandwidth;
        undef %total;
        undef @total;
        undef @flagged;
        undef @downloaded;
        undef @extra;
        undef %extra;
        undef @missing;
        undef %missing;
        undef $ratio;
        undef $time;
        undef $endtime;
        undef @dbr_temp;
        undef $total_bytes;
        undef $remove_date;
        undef $install_date;
        undef $start_year;
        undef $start_month;
        undef $end_year;
        undef $end_month;
        undef $start_of_report;
        undef $bandwidth_low;
        undef $bandwidth_high;
        $count = 0;

        #
        # Start JSON text for this station
        #
        $text   .= "\"$temp_sta\": {";
        $report .= sprintf("%6s", $temp_sta) . " :: ";

        #
        # Prepare vars
        #
        $local_path = prepare_path($temp_sta); 

        #
        # Get station info
        #
        $text .= "\n\t\"path\": \"$local_path\"";
        $text .= ",\n\t\"ip\": \"". $stations->{$temp_sta}->{ip} ."\"";
        $text .= ",\n\t\"vnet\": \"". $stations->{$temp_sta}->{net} ."\"";
        $text .= ",\n\t\"active\": \"". $stations->{$temp_sta}->{status} ."\"";

        #
        # Verify Database
        #
        @dbr = open_db($temp_sta);
        eval { dbquery(@dbr,"dbTABLE_PRESENT"); };
        if ( $@ ) {
            $text   .= ",\n\t\"error\": \"No Database!\" },\n";
            $report .="     0           (ERROR: No Database!)";
            dbclose(@dbr);
            next;
        }

        if (dbquery(@dbr, 'dbRECORD_COUNT') < 1) {
            $text   .= ",\n\t\"error\": \"Database empty!\" },\n";
            $report .="     0           (ERROR: Database empty!)";
            dbclose(@dbr);
            next;
        }

        # 
        # Re-write values for status of files...
        #
        #$nrecords = dbquery(@dbr, 'dbRECORD_COUNT') ;
        #for ( $dbr[3] = 0 ; $dbr[3] < $nrecords ; $dbr[3]++ ) {
        #
        #    ($status) = dbgetv (@dbr, "status");
        #
        #    if ( $status =~ /Downloaded/i ) {
        #        dbputv(@dbr,"status","downloaded");
        #    }
        #    elsif ($status =~ /Flagged/i ) {
        #        dbputv(@dbr,"status","flagged");
        #    }
        #    elsif ($status =~ /start/i ) {
        #        dbputv(@dbr,"status","downloading");
        #    }
        #    else {
        #        problem("ERROR: status='$status' on $dbout (@dbr)",$stations->{$temp_sta});
        #    }
        #}
        #
        #
        #
        # Use this to remove values
        #
        #$nrecords = dbquery(@dbr, 'dbRECORD_COUNT') ;
        #for ( $dbr[3] = 0 ; $dbr[3] < $nrecords ; $dbr[3]++ ) {
        #
        #    ($status) = dbgetv (@dbr, "dfile");
        #
        #    if ( $status =~ /stats\.html/i ) {
        #        debug("\n\n\t$temp_sta:\nGot stats.html\n\n\n");
        #        dbmark(@dbr);
        #        $crunch = 1;
        #    }
        #}
        #dbcrunch(@dbr) if $crunch;
        #
        #
        #

        #
        # Get list of flagged files
        #
        @dbr_temp= dbsubset ( @dbr, "status == 'flagged'");
        @dbr_temp = dbsort(@dbr_temp,'-u','dfile');
        $nrecords = dbquery(@dbr_temp, 'dbRECORD_COUNT') ;
        for ( $dbr_temp[3] = 0 ; $dbr_temp[3] < $nrecords ; $dbr_temp[3]++ ) {
            push @flagged, dbgetv (@dbr_temp, 'dfile');
        }

        #
        # Get list of downloaded files
        #
        @dbr_temp= dbsubset ( @dbr, "status == 'downloaded'");
        @dbr_temp = dbsort(@dbr_temp,'-u','dfile');
        $nrecords = dbquery(@dbr_temp, 'dbRECORD_COUNT') ;
        for ( $dbr_temp[3] = 0 ; $dbr_temp[3] < $nrecords ; $dbr_temp[3]++ ) {
            push @downloaded, dbgetv (@dbr_temp, 'dfile');
        }

        #
        # Check for missing files
        #
        @missing = unique_array(\@flagged,\@downloaded);
        if ( scalar @missing ) {

            @missing =  grep { $_ = "\"$_\"" } @missing;
            $text .= ",\n\t\"missing_files\": [" . join(',',@missing) . "]";
            $errors = "Missing ".@missing." files. ";
            debug("Station $temp_sta missing files:[@missing]") if $opt_V;

        }
        $report .= sprintf("%6d",scalar(@missing)) ." ";
        $report .= sprintf("%6d",scalar(@downloaded)) ." ";

        $text .= ",\n\t\"missing\": " . scalar(@missing);
        $text .= ",\n\t\"downloaded\": " . scalar(@downloaded);


        if ($nrecords > 0) {
            #
            # Get list of downloaded files and calculate total downloaded data and bandwidth
            #
            for ( $dbr_temp[3] = 0 ; $dbr_temp[3] < $nrecords ; $dbr_temp[3]++ ) {
                push @bw, dbgetv (@dbr_temp, 'bandwidth');
            }

            #
            # Dont use NULL values
            #
            @bw =  grep { $_ != "0.0" } @bw;
            @bw =  grep { $_ != "0" } @bw;
            @bw =  grep { $_ = sprintf("%0.1f",$_) } @bw;

            if ( scalar @bw ) {

                #
                # Get stats
                #
                $bandwidth_low  = min @bw;
                $bandwidth_high = max @bw;
                $median = median(\@bw);
                $average = average(\@bw);

                $median = sprintf("%0.1f", $median);
                $average = sprintf("%0.1f", $average);
                $bandwidth_high = sprintf("%0.1f", $bandwidth_high);
                $bandwidth_low = sprintf("%0.1f", $bandwidth_low);

                $report .= sprintf("%4d",$bandwidth_low) ." ";
                $report .= sprintf("%4d",$bandwidth_high) ." ";
                $report .= sprintf("%4d",$median) ." ";
                $report .= sprintf("%4d",$average) ." ";
            }
            else{
                $bandwidth_high = '"-"';
                $bandwidth_low  = '"-"';
                $median = '"-"';
                $average  = '"-"';
                $report .= sprintf("%4s",'-') ." ";
                $report .= sprintf("%4s",'-') ." ";
                $report .= sprintf("%4s",'-') ." ";
                $report .= sprintf("%4s",'-') ." ";

            }

            $text .= ",\n\t\"low_b\": $bandwidth_low";
            $text .= ",\n\t\"high_b\": $bandwidth_high";
            $text .= ",\n\t\"median\": $median";
            $text .= ",\n\t\"average\": $average";


            #
            # Get last file in DB
            #
            @dbr_temp = dbsubset ( @dbr, "status == 'downloaded'");
            @dbr_temp = dbsort(@dbr_temp,'time');
            $nrecords = dbquery(@dbr_temp, 'dbRECORD_COUNT') ;
            if ( $nrecords > 0 ) {
                # Last downloaded
                $dbr_temp[3] = $nrecords-1;

                ($dfile,$time) = dbgetv (@dbr_temp, qw/dfile time/);

                $text .= ",\n\t\"last\": \"$dfile\"";
                $text .= ",\n\t\"last_time\": \"$time\"";
            }
            else {
                $text .= ",\n\t\"last\": \"UNKNOWN\"";
                $text .= ",\n\t\"last_time\": \"UNKNOWN\"";
            }

            $total_bytes = total_data_downloaded($temp_sta,30) || 0.0;

            if ($total_bytes > 2000) {
                $errors = "Downloaded $total_bytes Mbts in the last 30 days!";
            }
            $report .= sprintf("%6d",$total_bytes) ."_Mbts ";
            $text .= ",\n\t\"30Mbytes\": " . ($total_bytes);

            $total_bytes = total_data_downloaded($temp_sta,7) || 0.0;
            if ($total_bytes > 1000) {
                $errors = "Downloaded $total_bytes Mbts in the last 7 days!";
            }
            $report .= sprintf("%6d",$total_bytes) ."_Mbts ";
            $text .= ",\n\t\"7Mbytes\": " . ($total_bytes);

            #
            # Get list by month
            #
            $text .= ",\n\t\"files\": {";

            @queries = build_time_regex($temp_sta,$stations->{$temp_sta}->{dates});

            foreach (@queries) {

                #
                # Open db and search for files here
                #
                @dbr_temp= dbsubset ( @dbr, "dfile =~ /($_)/ && status == 'downloaded'");
                @dbr_temp = dbsort(@dbr_temp,'-u','dfile');
                $nrecords = dbquery(@dbr_temp, 'dbRECORD_COUNT') ;

                $text .= " \"$_\": \"$nrecords\",";

            }

            dbclose(@dbr);


            $text .= "{" if (chop($text) eq '{');

            $text .= "}\n";

        }
        if ( $errors ) {
            $text .= ",\n\t\"error\": \"$errors\"";
        }

        $text   .= "\n\t},\n";

        logging("$report\n") if $opt_v;
    }

    chop $text;
    chop $text;
    print JSON "{\n$text\n}" if $opt_j;
    close( JSON ) if $opt_j;

    logging( "{\n$text\n}" ) if $opt_V;

#}}}
}

sub run_in_threads {
#{{{
    my ($stations,$function) = @_;
    my @active_procs;
    my $pid;
    my $max_out = $pf{max_procs};


    STATION: foreach $station (sort keys %$stations) {

        #
        # Verify running procs
        #
        @active_procs = check_pids(@active_procs); 

        #
        # Read messages from pipes
        #
        nonblock_read($stations);

        #
        # Stop if we are at max procs
        #
        redo STATION if scalar(@active_procs) >= $max_out;

        #
        # Test for memory and CPU load
        #
        unless ( test_resources() ) {
            $max_out = scalar @active_procs - 1 ;
            problem("Low on resources. Limit max_out=$max_out ");
            redo STATION;
        }

        logging("Spawn: $function($station). Now:".@active_procs." procs") if $opt_v;
        #logging("Spawn: $function($station). Now:".@active_procs." procs");

        #
        # Send msgs from child to parent
        #
        unless ( socketpair($$station{from_child}, $$station{to_parent}, AF_UNIX, SOCK_STREAM, PF_UNSPEC) ) {  
            problem("run_in_threads(): ERROR... socketpair():$! ");
            $max_out = scalar @active_procs - 1;
            problem("run_in_threads(): setting max_out=$max_out ");
            redo STATION;
        }

        $$station{from_child}->autoflush(1);
        $$station{to_parent}->autoflush(1);

        fcntl($$station{from_child},F_SETFL, O_NONBLOCK);
        fcntl($$station{to_parent},F_SETFL, O_NONBLOCK);

        #
        # Save this in hash for parent access
        #
        #$stations->{$station}->{FROMCHILD} = $$station{from_child};
        #$stations->{$station}->{TOPARENT}  = $$station{from_child};

        $pid = fork();

        # 
        # Parent
        #
        push @active_procs, $pid if $pid;
        $stations->{$station}->{pid} = $pid if $pid;
        next if $pid;

        #
        # Set this global for child only
        #
        $to_parent = $$station{to_parent}; 

        # 
        # Child
        #
        &$function($station,$stations->{$station});

        #
        # We can get to max files opened error if not carefull
        #
        close $$station{from_child};
        close $$station{to_parent};

        exit 0;

    }

    #
    # wait for last proc to end
    #
    nonblock_read($stations) while check_pids(@active_procs);

#}}}
}

sub nonblock_read {
#{{{
    my $stations = shift; 
    my ($msg,$n,$fh,$buf);

    foreach my $station (sort keys %$stations) {

        undef $msg;

        next unless $fh = $$station{from_child};

        unless ( check_pids( $stations->{$station}->{pid} ) ) {
            close $$station{from_child};
            close $$station{to_parent};
            next;
        }

        do {

            undef $buf;
            $n = sysread($fh,$buf,1024*1024);
            $msg .= $buf if $buf;

        } while $n;

        next unless $msg; 
        while ($msg =~ /\[LOG:(.*?)\]/g )     { logging($station.": ".$1); }
        while ($msg =~ /\[DEBUG:(.*?)\]/g )   { debug($station.": ".$1);   }
        while ($msg =~ /\[PROBLEM:(.*?)\]/g ) { problem($1,$station); }

    }
#}}}
}

sub test_pipes {
#{{{
    my $parent;
    my $test = 0;

    do {
        logging("test log msg on ".now()) if $opt_v;
        debug("test debug msg on ".now()) if $opt_v;
        problem("test problem msg on ".now()) if $opt_V;
        sleep rand(3);
        $test++;
    } while ($test < 4 );

#}}}
}

sub test_resources {
#{{{

    #
    # Test memory usage
    #
    my %memory   = sysmem();
    my $physical = $memory{physmem};
    my $used     = $memory{used};
    my $ratio    = ($used/$physical)*100 if $physical;
    $ratio      ||= 0;

    return 0 unless ($ratio && $used && $physical);

    debug( sprintf("Memory in use: %0.1f%% (%0d/%0d)", $ratio, $used, $physical) ) if $opt_V;
    #debug( sprintf("Memory in use: %0.1f%% (%0d/%0d)", $ratio, $used, $physical) );

    #
    # Stop here is we are over 90% of real memory usage (don't care about swap)
    #
    return 0 if $ratio > 85;

    #
    # Test files opend limit
    #
    $ratio = 0;
    my $max = `$ulimit -n`;
    my $count;
    eval { open LSOF, "$lsof -a -p $$ |" or problem("Cannot run: lsof -a -p $$"); };
    problem("Cannot run: $lsof -a -p $$ => $@") if $@; 
    return 0 if $@;
    $count++ while (<LSOF>);
    close LSOF;
    log_die( sprintf("$lsof -a -p $$:%0d $ulimit -n:%0d ", $count, $max) ) unless $count && $max;
    $ratio = ($count/$max)*100;

    return 0 unless ($ratio && $count && $max);

    debug( sprintf("Files limit: %0.1f%% (%0d/%0d)", $ratio, $count, $max) ) if $opt_V;
    #debug( sprintf("Files limit: %0.1f%% (%0d/%0d)", $ratio, $count, $max) );

    #
    # Stop here is we are over 90% of file limit
    #
    return 0 if $ratio > 85;

    #
    # Test CPU loads
    #
    my ($ncpu, $idle, $user, $kernel, $iowait, $swap, @the_rest) = syscpu();
    sleep 1;
    ($ncpu, $idle, $user, $kernel, $iowait, $swap, @the_rest) = syscpu();

    for (1 .. $ncpu) {

        #
        # Look for 1 CPU with less more than 25% idle time
        #
        $idle   = shift @the_rest if $_ != 1;
        $user   = shift @the_rest if $_ != 1;
        $kernel = shift @the_rest if $_ != 1;
        $iowait = shift @the_rest if $_ != 1;
        $swap   = shift @the_rest if $_ != 1;
        debug( sprintf("CPU $_: idle(%0.2f)  user(%0.2f)  kernel(%0.2f) iowait(%0.2f)  swap(%0.2f)\n",
                $idle, $user, $kernel, $iowait, $swap) ) if $opt_V;
        return 1 if $idle > 15; 

    }


    #
    # If all CPUs are over 75% load we return false
    #
    return 0;

#}}}
}

sub check_pids {
#{{{
    my @temp_pids = ();

    foreach (@_) {

        if (waitpid($_,WNOHANG) == -1) {
            #debug("No child running. RESP = $?") if $opt_V;
        }
        elsif (WIFEXITED($?)) {
            #debug("\tDone with $_") if $opt_V;
        }
        else{ 
            push @temp_pids, $_;
        }

    }

    return @temp_pids;

#}}}
}

sub get_data {
#{{{

    my ($station,$table) = @_;
    my $ip      = 0;
    my $folder  = '';
    my $type    = '';
    my $resp    = 0; 
    my ($r_size,%active_media_files,$media,$local_path,@rem_file,$ftp);
    my ($size,$nrecords,@temp_download,@dbwr,@dbr,@dbr_sub,$net);
    my ($local_path_file,$avoid,$replace,$file,$speed,$run_time);
    my ($d_data,$fixed,$start_sta,$start_file,$where,$attempts,@missing);
    my ($rem_s,$loc_s,@diff,$results,$run_time_str,$fixed_files,$dbout);
    my (@dates,$end_file,$record,$dlsta,$time,$endtime,$dir,$dfile);
    my ($k,$m,$g,$total_size,%temp_hash,@total_downloads,@download);
    my (@original_downloads);



    $start_sta = now();

    log_die("DIE: No value for station.") unless $station;

    debug("Station $station is not active in deployment table.") unless $table->{status} eq 'Active';
    return unless $table->{status} eq 'Active';

    log_die("DIE: No IP for station $station") unless $table->{ip};

    $d_data = total_data_downloaded($station,21) || 0.0;

    problem("Downloaded ( $d_data ) Mbts in the last 21 days! Skipping.") if $d_data > 3000;
    return if $d_data > 3000;

    debug("Start time ".strydtime($start_sta)) if $opt_V;

    #
    # Prepare Variables and Folders
    #
    $ip     = $table->{ip};
    $dlsta  = $table->{dlsta};
    $net    = $table->{net};
    @dates  = $table->{dates};
    $local_path = prepare_path($station); 

    #
    # Get list for download
    #
    @download = compare_dirs($station,$dlsta,$net,$ip,@dates);
    @original_downloads = @download; 

    FILE: foreach $file ( @download ) {

        #{{{ Download the file

        $start_file = now();
        $folder = '';
        $where = '';

        #
        # Check if we are over the time limit
        #
        if ( $pf{max_child_run_time} ) { 
            if ( int($pf{max_child_run_time}) < (now() - $start_sta) ) {
                problem("Rsync exceeds allowed time set in max_child_run_time ($pf{max_child_run_time}).");
                last FILE;
            }
        }

        ($folder,$r_size) = test_baler_file($station,$ip,$file); 

        debug("test_baler_file($station,$ip,$file) => ($folder,$r_size)") if $opt_V;

        problem("Cannot locate file in baler file: $file ($folder)") unless $folder;
        next unless $folder;

        #
        # Build local path to file
        #
        $local_path_file = File::Spec->rel2abs( "$local_path/$file" ); 

        #
        # Verify if we have the file in the local dir
        #
        problem("Re-download file:$local_path_file ($r_size) (".(-s $local_path_file).")") if -e $local_path_file;
        remove_file($station,$file,1) if -e $local_path_file;

        #log_die("Got: local_path_file:$local_path_file folder:$folder  r_size:$r_size");
        #
        # Update DB
        #
        @dbr = open_db($station);
        @dbr_sub = dbsubset(@dbr, "dlsta == '$dlsta' && dfile == '$file' && status == 'downloading' " );
        $attempts = dbquery(@dbr_sub,dbRECORD_COUNT) ; 

        $attempts += 1;

        debug("dbaddv: $dlsta | $local_path | $attempts | 'downloading'") if $opt_V;

        dbaddv(@dbr, 
            "net",      $net,
            "dlsta",    $dlsta,
            "dfile",    $file,
            "sta",      $station,
            "time",     now(), 
            "status",   "downloading",
            "dir",      $local_path,
            "attempts", $attempts,
            "lddate",   now() );

        dbclose(@dbr);

        if ( $r_size ) {
            #
            # Prepare download cmd on FTP
            #
            $file_fetch = File::Fetch->new(uri => "ftp://$ip:$pf{ftp_port}/$folder/$file");
            debug("Start ftp download of:".$file_fetch->uri) if $opt_V;


            #
            # Run Fetch cmd.
            #
            eval {  $where = $file_fetch->fetch( to => "$local_path/" ); };
            problem("File::Fetch ".$file_fetch->uri." $@") if $@; 

            if (! $where) {
                #
                # Prepare download cmd on HTTP
                #
                $file_fetch = File::Fetch->new(uri => "http://$ip:$pf{ftp_port}/$folder/$file");
                debug("Start http download of:".$file_fetch->uri) if $opt_V;

                #
                # Run Fetch cmd.
                #
                eval {  $where = $file_fetch->fetch( to => "$local_path/" ); };
                problem("File::Fetch ".$file_fetch->uri." $@") if $@; 

            }

        }
        else {

            #
            # This is a 0 size file. Just touch the file in the local system.
            #
            $where = "$local_path/$file";
            open FILE, ">", $where or log_die("Could not create file ( $where ) :$!");
            close FILE;
            log_die("Could not create file ( $where )") unless -e $where;

        }

        $end_file = now();
        $run_time = $end_file-$start_file;
        $run_time_str = strtdelta($run_time);
        #}}}

        #{{{ if download is SUCCESS
        if( $where ) { 
            #
            # Keep track of total data downloaded
            #
            $size = -s $where; 
            $total_size += $size;

            if ($size == $r_size) {

                #
                # Success download
                #
                $fixed = 'n';

                if ( $r_size > 0 ) {

                    #
                    # For files with data
                    #
                    debug("Success in download of $file after $run_time_str") if $opt_V;

                    push @total_downloads, $file;

                    #
                    # Verify bandwidth of ftp connection
                    #
                    $speed = ((-s $where) / 1024 ) / $run_time;
                    debug("$file $size Kb  $run_time secs $speed Kb/sec") if $opt_V;

                    #
                    # In case we need to fix the miniseed files...
                    #
                    if ( $pf{fix_mseed_cmd} ) { 

                        debug("Fix miniseed: $pf{fix_mseed_cmd} " ) if $opt_V;
                        fix_file($station,$where); 
                        $fixed = 'y';

                    }

                }

                #
                # If empty set to 0.00
                #
                $speed ||= 0.00;


                #
                # Change value of 0 to ascii 
                # for empty files
                #
                $size = '0' unless $size;

                #
                # Add to DB
                #
                debug("dbaddv: $dlsta | $start_file | $end_file | $attempts | 'downloaded' | $size | $speed ") if $opt_V;

                @dbr = open_db($station);
                dbaddv(@dbr, 
                    "net",      $net, 
                    "sta",      $station, 
                    "time",     $start_file, 
                    "endtime",  $end_file, 
                    "dir",      $local_path, 
                    "attempts", $attempts, 
                    "filebytes",$size, 
                    "bandwidth",$speed, 
                    "dlsta",    $dlsta,
                    "fixed",    $fixed,
                    "dfile",    $file,
                    "lddate",   now(),
                    "status",   "downloaded");
                dbclose(@dbr);

            }
            else {
                problem("( $size ) is not the reported ( $r_size ) for ( $where ) "); 
                problem("Remove file ( $where ) from local directory."); 
                unlink $where;
            }



        } # end if $where
        #}}} 

        #{{{ if download FAILS
        else {
            #
            # If download failed... $where == NULL
            #
            $run_time_str = strtdelta(now()-$start_file);
            if ( -e "$local_path/$file") {
                problem("Failed download of $file after $run_time_str. File present in local archive!");
            }
            else { 
                problem("Failed download of $file after $run_time_str. File missing from local archive!");
            }

        }
        #}}}

    } #end of foreach @download 

    @missing = unique_array(\@download,\@total_downloads);

    if ( scalar @missing && scalar @missing == scalar @original_downloads ) {
        problem( "NO DOWNLOADS!!!! Station not downloading any files.");
    }
    elsif ( scalar @missing ) {
        @missing = compare_dirs($station,$dlsta,$net,$ip,@dates);
    }

    problem( "Missing: " . @missing . " files") if @missing;
    debug( "Missing files: \n\n@missing") if @missing && $opt_V;

    #
    # Calc data downloaded
    #
    $total_size ||= 0;
    $k = sprintf("%0.1f",$total_size/1024);
    $m = sprintf("%0.1f",$k/1024);

    #
    # Calc the total time to rsync station
    #
    $run_time = now() - $start_sta;
    $run_time_str = strtdelta($run_time);

    logging("Done rsync of ".@total_downloads." files ($m Mb) out of ".@download." form $ip in $run_time_str") if $opt_v;



#}}}
}

sub build_time_regex {
#{{{
    my $sta      = shift;
    my @dates    = shift;
    my $folder   = shift || '';
    my ($temp_year,$temp_month,$end_year,$end_month);
    my ($flag,$regex,$line,$t,$f,$start,$end,$tuple);
    my (%list,%temp);
    my (@n,%queries);

    $flag = $folder ? '*' : '.*';

    if ($folder =~ /.*reserve.*/) {
        $queries{ "*${sta}_4-*" } = ();
    }
    else {

        # 
        # Build regex for all valid months
        #
        foreach ( @dates ) {

            foreach $tuple ( @$_ ) {
                $start = @{$tuple}[0];
                $end   = @{$tuple}[1];

                #
                # Fix dates
                #
                $start = now() unless ( $start or ! is_epoch_string($start) );
                $end   = now() unless ( $end   or ! is_epoch_string($end) );
                $start = now() if $start > now();
                $end   = now() if $end > now();

                #
                # Overide start time with 
                # the value for the last 4 
                # months. Temp fix for problem
                # with balers. ONLY for baler queries.
                #
                $start= str2epoch("-125days") if $folder;

                next if $end < $start; 

                debug("Create regex for:".strtime($start)."=>".strtime($end)) if $opt_V;

                debug("Overwrite for last 4 months:".strtime($start)."=>".strtime($end)) if $opt_V;

                $temp_year  = int( epoch2str( $start, "%Y") );
                $temp_month = int( epoch2str( $start, "%m") );
                $end_year  = int( epoch2str( $end, "%Y") );
                $end_month = int( epoch2str( $end, "%m") );

                #
                # Build queries
                #
                do {
                    #
                    # Build regex
                    #
                    $regex = str2epoch("$temp_month/1/$temp_year");
                    $regex = "${flag}${sta}_4-" . epoch2str( $regex, "%Y%m") . "${flag}";

                    $queries{ "$regex" } = ();

                    if ( $temp_month == 12 ) {
                        $temp_month = 1;
                        $temp_year++;
                    }
                    else {
                        $temp_month++;
                    }
                    if ( $temp_year == $end_year && $temp_month == $end_month) {
                        #
                        # We don't want to miss the last element...
                        # don't have time to do a better do-loop.
                        #
                        $regex = str2epoch("$temp_month/1/$temp_year");
                        $regex = "${flag}${sta}_4-".epoch2str($regex,"%Y%m")."${flag}";

                        $queries{ "$regex" } = ();
                    }

                } while ( str2epoch("$temp_month/1/$temp_year") < str2epoch("$end_month/1/$end_year") );

            }
        }

    }

    #
    # Lets try with a reverse sort to 
    # get the current data first. Before 
    # the ftp connection decides to stop 
    # working.
    #
    foreach (reverse sort keys %queries) { debug("Regex=$_") if $opt_V};

    return reverse sort keys %queries;

#}}}
}

sub total_data_downloaded {
#{{{
    my $sta  = shift;
    my $days = shift || 1;
    my @db;
    my $start_or_report;
    my $nrecords = 0;
    my $total_bytes = 0.0;

    #
    # Verify Database
    #
    @db = open_db($sta);
    eval { dbquery(@db,"dbTABLE_PRESENT"); };
    if ( $@ ) {
        dbclose(@db);
        return;
    }

    if (dbquery(@db, 'dbRECORD_COUNT') < 1) {
        dbclose(@db);
        return;
    }

    @db = dbsubset ( @db, "status == 'downloaded'");
    $nrecords = dbquery(@db, 'dbRECORD_COUNT') ;
    return unless $nrecords;


    $start_of_report = str2epoch("-${days}days");
    @db= dbsubset ( @db, "time >= $start_of_report");
    $nrecords = dbquery(@db, 'dbRECORD_COUNT') ;
    return unless $nrecords;

    if ($nrecords > 0) {
        for ( $db[3] = 0 ; $db[3] < $nrecords ; $db[3]++ ) {
            $total_bytes += dbgetv (@db, 'filebytes');
        }
    }

    # for Kbytes
    $total_bytes = sprintf("%0.2f", $total_bytes/1024);
    # for Mbytes
    $total_bytes = sprintf("%0.2f", $total_bytes/1024);


    return $total_bytes;
#}}}
}

sub open_db {
#{{{
    # $false is a flag to set 
    # the return of db pointers.
    my $sta = shift;
    my $false = shift || 0;
    my @db;

    #
    # Prepare Folder Name
    #
    my $path = prepare_path($sta);

    #
    # Build station database name
    #
    $dbout = "$path/$sta";
    $dbout .= "_baler";

    #
    # Fix path
    #
    $dbout = File::Spec->rel2abs( $dbout ); 

    debug("Opening database ($dbout).") if $opt_V;

    #
    # Create descriptor file if missing
    #
    unless ( -e $dbout) {
        debug("$sta Creating new database ($dbout).") if $opt_V;

        open FILE, ">", $dbout or log_die("Could not create file [$dbout] :$!");

        print FILE "#Datascope Database Descriptor file\n\n";
        print FILE "schema css3.0\n";
        print FILE "dbpath $dbout\n";
        print FILE "dnblocks nfs\n";
        print FILE "ndbidserver anfops.ucsd.edu:2498\n\n";
        print FILE "Description &Literal{\n";
        print FILE "#\n";
        print FILE "# Baler44 archival database\n";
        print FILE "# Juan Reyes <reyes\@ucsd.edu>\n";
        print FILE "#\n";
        print FILE "}\n";

        close FILE;

    }

    #
    # Open table
    #
    debug("$sta Openning database table  ($dbout.rsyncbaler)") if $opt_V;
    @db  = dbopen($dbout,"r+") or log_die("Can't open DB: $dbout",$sta);
    @db  = dblookup(@db,"","rsyncbaler","","") or log_die("Can't open DB TABLE: $dbout.rsyncbaler",$sta);

    #
    # Close database if we don't need 
    # to return pointer
    #
    dbclose(@db) if $false;

    return @db;
#}}}
}

sub fix_file {
#{{{
    my $sta  = shift;
    my $file  = shift;
    my ($cmd,$success,$error_code,$full_buf,$stdout_buf,$stderr_buf);

    $cmd = "$pf{fix_mseed_cmd} $file";

    debug("$cmd") if $opt_V;

    ($success,$error_code,$full_buf,$stdout_buf,$stderr_buf) = run( command => $cmd, verbose => $opt_v );

    problem("Cmd:$cmd \n\tError_code:$error_code
        \n\tStdout:@$stdout_buf \n\tStderr:@$stderr_buf",$sta) unless $success;

    debug("Cmd:$cmd \n\tError_code:$error_code
        \n\tStdout:@$stdout_buf \n\tStderr:@$stderr_buf",$sta) if $opt_V;

    return;
#}}}
}

sub clean_db {
#{{{
    my ($station,$table)= @_;
    my ($dlsta,$net);
    my (@remove,@dbr,@flagged,%flagged); 
    my ($mode,$record,$lf,$path,@downloaded);
    my ($get_filebytes,$get_status,$get_dlsta);
    my ($get_net,$get_sta,$get_dir,$get_dfile,$get_fixed);

    $dlsta  = $table->{dlsta};
    log_die("Don't have value of 'dlsta' for $station in clean_db()") unless $dlsta;

    $net    = $table->{net};
    log_die("Don't have value of 'net' for $station in clean_db()") unless $net;

    #
    # Prepare PATH
    #
    $path = prepare_path($station);

    #
    # Fix tables for sta and net values
    #
    debug("Test each entry in database for $station ") if $opt_V;
    @dbr = open_db($station);
    $record = dbquery(@dbr, 'dbRECORD_COUNT') ;
    LINE: for ( $dbr[3] = 0 ; $dbr[3] < $record ; $dbr[3]++ ) {
    
        ($get_dlsta,$get_net,$get_sta,$get_dir,$get_dfile,$get_status,$get_fixed,$get_filebytes) = 
                        dbgetv (@dbr, qw/dlsta net sta dir dfile status fixed filebytes/);
    
        unless ( $get_dfile =~ /.*($station).*/ ) {

            problem("File ($get_dfile) is not related to station ($station). Removing.");
            push @remove, $get_dfile;
            next LINE;

        }

        unless ( $get_sta && $get_net ) {

            ($get_net, $get_sta) = split(/_/, $get_dlsta, 2);
            dbputv(@dbr, "sta",$get_sta, "net",$get_net);

        }

        next LINE if $get_status !~ /downloaded/;

        unless ( -f "$get_dir/$get_dfile" ) {

            problem("Fix dir($get_dir)=>($path) for $get_dfile");
            dbputv(@dbr, "dir", $path);
            $get_dir = $path;

        }

        unless ( -f "$get_dir/$get_dfile" ) {

            problem("File missing: ($get_dir/$get_dfile). Removeing.");
            push @remove, $get_dfile;
            next LINE;

        }

        #
        # Some files are 0Kb. Cannot remove them. 
        #
        #unless ( -s "$get_dir/$get_dfile" ) {

        #    problem("File 0 Kb: ($get_dir/$get_dfile). Removeing.");
        #    push @remove, $get_dfile;
        #    next LINE;

        #}

        $mode = (stat("$get_dir/$get_dfile"))[2];   
        $mode = sprintf("0%o", $mode & 07777);
        unless ( $mode == "0664" ) {

            problem("Permissions not 0664 : ($get_dir/$get_dfile) [ $mode ]");

            if ( chmod 0664, "$get_dir/$get_dfile" ) {
                $mode = (stat("$get_dir/$get_dfile"))[2];   
                $mode = sprintf("0%o", $mode & 07777);
                problem("Permissions now: ($get_dir/$get_dfile)  [ $mode ]");
            }
            else {
                problem("Cannot change permissions to 664 on file: ($get_dir/$get_dfile).");
            }

        }

        unless ( -s "$get_dir/$get_dfile" == $get_filebytes) {

            problem("Fix file size in db to (" . (-s "$get_dir/$get_dfile") . ")");
            dbputv(@dbr, "filebytes", -s "$get_dir/$get_dfile");

        }

        #
        # In case we need to fix the miniseed files...
        #
        if ( $pf{fix_mseed_cmd} && $get_fixed !~ /y/ ) { 

            problem("Fix miniseed: $pf{fix_mseed_cmd} $get_dir/$get_dfile" );
            fix_file($station,"$get_dir/$get_dfile"); 
            dbputv(@dbr, "fixed", "y");

        }

    }

    dbclose(@dbr);

    remove_file($station,$_,1) foreach @remove;

    #
    # Verify each entry on the database
    #
    FILE: foreach $lf ( read_local( $station ) ) {

        #
        # Verify the files entered as downloaded
        #
        debug("Subset dfile == $lf && status == downloaded ") if $opt_V;
        @dbr = open_db($station);
        @dbr = dbsubset(@dbr, "dfile =='$lf' && status == 'downloaded'");
        $record = dbquery(@dbr, dbRECORD_COUNT);
        dbclose(@dbr);

        if ( $record == 0 ) {

            #
            # Add file
            #
            @dbr = open_db($station);
            problem("file $lf not in database. Adding as 'downloaded'");
            fix_file($station,"$get_dir/$get_dfile"); 
            dbaddv(@dbr, 
                "net",      $net,
                "sta",      $station,
                "dir",      $path,
                "dlsta",    $dlsta,
                "dfile",    $lf,
                "attempts", 1,
                "time",     now(), 
                "fixed",    "y",
                "lddate",   now(), 
                "status",   "downloaded");
            dbclose(@dbr);

        }
        elsif ( $record > 1 ) {

            #
            # Fix status for files downloaded more than once.
            #
            problem("File $lf entered as 'downloaded' ($record) times.") if $record;
            foreach ( 2 .. $record ) {
                @dbr = open_db($station);
                $dbr[3] = dbfind(@dbr, "dfile == '$lf' && status == 'downloaded' && attempts != 1", -1);
                # If we have more than one with attempts == 1 ....
                $dbr[3] = dbfind(@dbr, "dfile == '$lf' && status == 'downloaded'", -1) if $dbr[3] < 0;
                problem("Error in the pointer:record#($dbr[3])") if $dbr[3] < 0;
                next if $dbr[3] < 0;
                dbputv(@dbr,'status','extra','lddate', now() );
                dbclose(@dbr);
            }

        }

    }

    debug("Done fixing database for $station") if $opt_V;


#}}}
}
sub compare_dirs {
#{{{
    my ($station,$dlsta,$net,$ip,@dates)= @_;
    my (@local_files,@dbr,@flagged,@db_t,@remote_files); 
    my ($rf,$record,$lf,@downloaded,%remote);
    my ($dfile,$time,$endtime,$status,$attempts,$lddate);

    my $path = prepare_path($station);

    @local_files = read_local( $station );

    #
    # Verify missed downloads
    #
    @dbr = open_db($station);
    debug("Subset for status == flagged") if $opt_V;
    @db_t= dbsubset(@dbr, "status == 'flagged'");
    $record  =  dbquery(@db_t, "dbRECORD_COUNT");
    for ( $db_t[3] = 0 ; $db_t[3] < $record ; $db_t[3]++ ) {
    
        push @flagged, dbgetv (@db_t, 'dfile');

    }

    debug("Subset for status == downloaded") if $opt_V;
    @db_t= dbsubset(@dbr, "status == 'downloaded'");
    $record  =  dbquery(@db_t, "dbRECORD_COUNT");
    for ( $db_t[3] = 0 ; $db_t[3] < $record ; $db_t[3]++ ) {
    
        push @downloaded, dbgetv (@db_t, 'dfile');

    }

    #
    # Make unique
    #
    @flagged = unique_array(\@flagged);
    @downloaded = unique_array(\@downloaded);

    logging("Previously flagged: ".@flagged) if $opt_v;
    logging("Previously downloaded: ".@downloaded) if $opt_v;

    @flagged = unique_array(\@flagged,\@downloaded);

    problem("Previously flagged (".@flagged.") files.") if @flagged;
    logging("@flagged") if $opt_V && @flagged;

    # Avoid connecting to the stations
    # for a list of directories if we 
    # have more than 5 files pending.
    problem("Avoid baler list.") if scalar @flagged > 5;
    return @flagged if scalar @flagged > 5;

    # or get new list
    %remote = read_baler( $station, $ip ,@dates);
    @remote_files = sort keys %remote;


    # 
    # Compare local to remote
    #
    foreach $rf ( unique_array(\@remote_files,\@local_files) ) {

        debug("Test:$rf") if $opt_V;

        debug("Subset for dfile == $rf && status == flagged") if $opt_V;
        @db_t= dbsubset(@dbr, "dfile == '$rf' && status == 'flagged'");
        $record  =  dbquery(@db_t, "dbRECORD_COUNT");
        debug("dbaddv: $rf $station 'flagged'") if $opt_V;

        dbaddv(@dbr, 
            "net",      $net,
            "sta",      $station,
            "dlsta",    $dlsta,
            "dfile",    $rf,
            "attempts", $record + 1,
            "time",     now(), 
            "lddate",   now(), 
            "status",   "flagged");

        push @flagged, $rf;

    } #end of foreach $rt

    #
    # Compare size of files
    #
    foreach $rf ( sort keys %remote ) {

        next unless -f "$path/$rf";
        debug("Compare size of ($rf) to local copy") if $opt_V;
        next if ((-s "$path/$rf") == $remote{$rf});
        push @flagged, $rf;
        problem("File size don't match: $rf ".(-s "$path/$rf")." != $remote{$rf}");

        #
        # remove file AND add new Flagged entry to db
        #
        @dbr = open_db($station);
        debug("Subset for dfile == $rf && status == flagged") if $opt_V;
        @db_t= dbsubset(@dbr, "dfile == '$rf' && status == 'flagged'");
        $record  =  dbquery(@db_t, "dbRECORD_COUNT");

        remove_file($station,$rf,1);

        @dbr = open_db($station);
        debug("dbaddv: $rf $station 'flagged'") if $opt_V;

        dbaddv(@dbr, 
            "net",      $net,
            "sta",      $station,
            "dlsta",    $dlsta,
            "dfile",    $rf,
            "attempts", $record + 1,
            "time",     now(), 
            "lddate",   now(), 
            "status",   "flagged");

        # 
        # Remove old downloaded flag
        #
        #debug("Subset for dfile == $rf && status == downloaded") if $opt_V;
        #@db_t= dbsubset(@dbr, "dfile == '$rf' && status == 'downloaded'");
        #$record  =  dbquery(@db_t, "dbRECORD_COUNT");
        #if ( $record ) {
        #    problem("Delete #$record records for $rf and status == 'downloaded' ");
        #    foreach ( 1 .. $record ) { 
        #        @dbr = open_db($station);
        #        $dbr[3] = dbfind(@dbr, "dfile == '$rf' && status == 'downloaded'", -1);
        #        dbdelete(@dbr) if ($dbr[3] >= 0) ; 
        #    }
        #}
    }

    eval { dbclose(@dbr); };

    return unique_array(\@flagged);

#}}}
}

sub loggin_in {
#{{{
    my $ip      = shift;
    my $station = shift;
    my $debug   = shift || 0;
    my $ftp;
    my $test = 0;

    $debug = 1 if $opt_f;

    if ($ip && $pf{ftp_port}) {

        debug("Net::FTP $station=>$ip:$pf{ftp_port}") if $opt_V;
        $ftp = Net::FTP->new(Host=>$ip, Passive=>1, Timeout=>900, Port=>$pf{ftp_port}, Debug=>$debug);

        eval { $ftp->login()  }; 
        problem("Cannot login to $ip:$pf{ftp_port} ($@)") if $@;
        return if $@;

        $test = $ftp->ls();
        problem("$ip:$pf{ftp_port} Will not return data for simple 'ls' command...($test)") unless $test;
        return unless $test;

    }
    else {
        log_die("Missing parameter for ftp connection. sta($station) ip($ip)");
    }

    return $ftp;
#}}}
}

sub read_baler {
#{{{
    my $sta   = shift;
    my $ip    = shift;
    my @dates = shift;
    my ($folder,$ftp,$name,$test);
    my %list;
    my @temp_dir = ();
    my (@n,@queries);
    my $attempt = 1;

    #
    # For each of the folders
    #
    foreach $folder ( @{$pf{remote_folder}} ) {

        #
        # Init Net::FTP connection
        #
        $ftp = loggin_in($ip,$station);
        problem("Cannot connect to $sta:$folder ($ip:$pf{ftp_port})") unless $ftp;
        next unless $ftp;

        #
        # Get list from Baler
        #
        foreach $test ( build_time_regex($sta,@dates,$folder) ) {
            $attempt  = 1;

            while ( $attempt <= 3 ) {

                #
                # Get list for this month
                #
                debug("$sta $ip:$pf{ftp_port} ftp->dir($folder/$test)(connection attempt $attempt).") if $opt_V;
                @temp_dir = ();
                @temp_dir = $ftp->dir("$folder/$test");
                #debug("$sta $ip:$pf{ftp_port} ftp->dir($folder/$test)=> @temp_dir.") if $opt_V;

                problem("RESERVEMEDIA in use!") if (@temp_dir && $folder =~ /.*reserve.*/ );

                #
                # Parse results and get size
                #
                foreach (@temp_dir) {

                    next if /^d.+\s\.\.?$/;
                    @n = split(/\s+/, $_, 9);
                    $name = ( split(/\//,$n[8]) )[-1];
                    $list{$name} = $n[4];
                    logging("Net::FTP $name => {@n}") if $opt_V;

                }

                #
                # If we have files, save them and exit loop.
                #
                last if scalar @temp_dir;

                #
                # RESERVEMEDIA should be empty.
                #
                last if $folder =~ /.*reserve.*/;

                #
                # Prepare for a second attempt.
                #
                $attempt ++;
                eval{ $ftp->quit(); };
                sleep 5;
                $ftp= loggin_in($ip,$sta);
                problem("Cannot connect to $sta:$folder ($ip:$pf{ftp_port})") unless $ftp;
                last unless $ftp;

            } # end of while loop

            problem("Net::FTP $ip empty list for query: $test", $sta) unless ( @temp_dir || $folder !~ /.*reserve.*/);
            debug("$test=>" . @temp_dir) if $opt_v;

        } # end of foreach @queries

        eval{ $ftp->quit(); };


    }


    #return sort keys %list;
    return %list;

#}}}
}

sub test_baler_file {
#{{{
    my $sta   = shift;
    my $ip    = shift;
    my $file  = shift;
    my $name = '';
    my @n;
    my $folder;
    my $ftp;
    my $size;

    debug("test_baler_file($sta,$ip,$file)") if $opt_V;

    #
    # For each of the folders
    #
    foreach $folder ( @{$pf{remote_folder}} ) {

        #
        # Init Net::FTP connection
        #
        $ftp = loggin_in($ip,$station);
        #problem("Cannot connect to $sta ($ip:$pf{ftp_port})") unless $ftp;
        next unless $ftp;

        #
        # Get file info
        #
        debug("$sta $ip:$pf{ftp_port} ftp->size('$folder' '$file').") if $opt_V;

        #
        # Query Baler
        #
        eval { $size = $ftp->size("$folder/$file") };
        problem("Cannot get size: $ip:$pf{ftp_port}->size($folder/$file)($@)") if $@;

        if ( defined $size  ) {

            eval{ $ftp->quit(); };

            debug("FTP->size($folder/$file)=>($size)") if $opt_V;

            problem("$file in RESERVEMEDIA ") if ($folder =~ /.*reserve.*/ );

            debug("test_baler_file return($folder,$size)") if $opt_V;

            return $folder,$size;
        }


    }

    eval{ $ftp->quit(); };

    problem("Cannot locate ($file) in baler. $ip:$pf{ftp_port}");

    return;

#}}}
}

sub read_local {
#{{{
    my $sta = shift;
    my %list;
    my $file;
    my $f;

    debug("Reading local directory") if $opt_V;

    my $path = prepare_path($sta);

    opendir(DIR,$path) or log_die("Failed to open $path: $!");

    while($f = readdir DIR) {

        $file = "$path/$f";

        if(-d "$file"){ next; } 

        elsif($f =~ /..-...._\d-\d+-\d+/ ){ 
            remove_file($sta,$f);
        } 

        elsif($f !~ /.*${sta}.*/ ){ 
            remove_file($sta,$f);
        } 

        elsif($f =~ /.*-${sta}_4-.*/ ){ 
            $list{$f} = ();
        }
    }

    close(DIR);

    foreach (sort keys %list) { debug("LOCAL: $_") if $opt_V; }

    return sort keys %list;

#}}}
}

sub remove_file {
#{{{
    my $sta      = shift;
    my $file     = shift || '';
    my $downloaded = shift || 0;
    my @db; 
    my $nrecords;

    my $path = prepare_path($sta);

    if ( $file ) { problem("Removing $path/$file"); }
    else { problem("Removing NULL entry for $path"); }

    # comment out section
    # we have empty entries for dfile that we need to clean...

    # return if we don't get value
    #problem("Cancel remove_file($path/$file)") unless $file;
    #return unless $file; 

    mkdir "$path/trash" unless -d "$path/trash";

    #
    # Verify file in folder
    #
    if ($file and -f "$path/$file") {
        debug("move $path/$file to $path/trash/$file") if $opt_V;
        move("$path/$file","$path/trash/$file") or problem("Can't move $file to $path/trash");
    }

    #
    # Verify DB for file
    #
    @db = open_db($sta);
    if ( $downloaded ) { @db= dbsubset ( @db, "status == 'downloaded'"); }
    if ( $file ) { @db= dbsubset ( @db, "dfile =~ /$file/"); }
    else { @db= dbsubset ( @db, "dfile == ''"); }
    $nrecords = dbquery(@db, 'dbRECORD_COUNT') ;

    # 
    # If found
    #
    if ( $nrecords ) {
        problem("Delete #$nrecords records for $file");
        foreach ( 1 .. $nrecords ) { 
            @db = open_db($sta);
            if ( $file and $downloaded) { 
                $db[3] = dbfind(@db, "dfile =~ /$file/ && status == 'downloaded'", -1); 
            }
            elsif ( $file ) { 
                $db[3] = dbfind(@db, "dfile =~ /$file/", -1); 
            }
            else { 
                $db[3] = dbfind(@db, "dfile == ''", -1); 
            }
            dbdelete(@db) if ($db[3] >= 0) ; 
        }
    }

    dbclose(@db);

    return;
#}}}
}

sub prepare_path {
#{{{
    my $station  = shift;

    log_die("prepare_path(). Cannot produce path! We need a station name...") unless $station;

    my $path = File::Spec->rel2abs( "$pf{local_data_dir}/$station" ); 

    makedir($path) unless -e $path;

    log_die("Cannot create folder $path") unless -e $path;

    return $path;
#}}}
}

sub getparam {
#{{{
    my $PF = shift ;
    my %pf;

    foreach  (qw/local_data_dir remote_folder max_child_run_time
                download_timeout database print_miniseed_errors reservemedia
                activemedia http_port fix_mseed_cmd max_procs ftp_port/){
        $pf{$_} = pfget($PF,$_);

        log_die("Missing value for $_ in PF:$PF") unless defined($pf{$_});

        debug( sprintf("\t%-22s -> %s", ($_,$pf{$_})) ) if $opt_V;
    }

    return (%pf);
#}}}
}

sub table_check {
#{{{
    my $db = shift;
    my $sta = shift;

    $sta ||= '';

    debug("Verify Database: ".dbquery(@$db,"dbDATABASE_NAME") ) if $opt_V;

    log_die( dbquery(@$db,"dbTABLE_NAME")." not available.",$sta) unless dbquery(@$db,"dbTABLE_PRESENT");

    debug("\t".dbquery(@$db,"dbDATABASE_NAME")."{ ".dbquery(@$db,"dbTABLE_NAME")." }: --> OK") if $opt_V;

#}}}
}

sub savemail {
#{{{
    my $proc = $0;

    $proc =~ s/\W//g ;

    my $tmp = "/tmp/#${proc}_maillog_$$";

    logging("Start savemail in temp file ($tmp)");

    unlink($tmp) if -e $tmp;

    open $oldout, ">&STDOUT" or log_die("Cannot save value for STDOUT: $!");
    open $olderr, ">&STDERR" or log_die("Cannot save value for STDERR: $!");

    select STDOUT; $| = 1;
    select STDERR; $| = 1;

    open STDOUT, ">$tmp";
    open STDERR, ">&STDOUT";

    log_die("Cannot produce tempfile:($tmp) to save logs.($!)") unless -e $tmp;

#}}}
}

sub sendmail {
#{{{
    my $subject = shift || "Done $0 on $host";

    my $proc = $0;

    $proc =~ s/\W//g ;

    my $tmp = "/tmp/#${proc}_maillog_$$";

    $opt_m =~ s/,/ /g ;

    my $res = system ( "rtmail -C -s '$subject' $opt_m < $tmp");

    open STDOUT, ">&", $oldout;
    open STDERR, ">&", $olderr;

    warn ( "rtmail fails: $@\n" ) if $res;

    system("cat $tmp") if -e $tmp;

    unlink $tmp unless $res; 
#}}}
}

sub average {
#{{{
    # usage: $average = average(\@array)
    my ($array_ref) = @_; 
    my $sum; 
    my $count = scalar @$array_ref; 
    return unless $count;
    foreach (@$array_ref) { $sum += $_; } 
    return $sum / $count; 
#}}}
} 

sub median {
#{{{
    # usage: $median = median(\@array)
    my ($array_ref) = @_; 
    my $count = scalar @$array_ref; 
    my @array = sort @$array_ref; 
    return unless $count;

    if ($count == 1 ) {
        return $array[0];
    }
    elsif ($count == 2) { 
        return ($array[0] + $array[1])/2; 
    } 
    elsif ($count % 2) { 
        return $array[int($count/2)]; 
    } 
    else { 
        return ($array[$count/2] + $array[$count/2 - 1]) / 2; 
    } 
#}}}
}

sub unique_array {
#{{{
    # usage 1: $unique = unique_array(\@array)
    # usage 2: $unique = unique_array(\@array,\@delete)
    my $original = shift;
    my $delete   = shift;
    my (@temp,%temp);

    @temp{@$original} = ();
    delete @temp {@$delete} if $delete;
    return sort keys %temp;
#}}}
}

sub log_die {
#{{{
    my $msg = shift;

    problem($msg);

    elog_die($msg) if $parent != $$;

    sendmail("ERROR: $0 DIED ON $host") if $opt_m; 

    elog_die($msg);

#}}}
}

sub logging {
#{{{
    my $msg = shift;

    if ( $parent != $$ ) {
        if ( $to_parent ) {
            print { $to_parent } "[LOG:$msg]";
            return;
        }
    }

    elog_notify($msg);

#}}}
}

sub debug {
#{{{
    my $msg = shift;

    if ( $parent != $$ ) {
        if ( $to_parent ) {
            print { $to_parent } "[DEBUG:$msg]";
            return;
        }
    }

    elog_debug($msg);

#}}}
}

sub problem { 
#{{{
    my $text = shift; 
    my $station = shift || '*MAIN*';
    my $string;

    if ( $parent != $$ ) {
        if ( $to_parent ) {
            print { $to_parent } "[PROBLEM:$text]";
            return;
        }
    }

    $Problems++;

    $string = sprintf("%03s",$Problems);

    $problems_hash->{$station}->{$string} = $text;

    elog_complain("\n\n\t* \n\t* Problem #$Problems: \n\t* $station: $text\n\t* \n") if $opt_v;

#}}}
}

sub problem_print {
#{{{

    my $s_v; 
    my $p_v; 

    return unless $Problems;

    elog_complain('');
    elog_complain('');
    elog_complain("-------- Problems: --------");
    elog_complain('');

    for  $s_v ( sort keys %$problems_hash ) {
        elog_complain("\tOn station $s_v:");
        for $p_v ( sort keys %{$problems_hash->{$s_v}} ) {
            elog_complain("\t\t $p_v) $problems_hash->{$s_v}->{$p_v}");
        }
        elog_complain('');
    }

    elog_complain("-------- End of problems: --------");
    elog_complain('');

#}}}
}
__END__
#{{{
=pod

=head1 NAME

rsync_baler - Sync a remote baler directory to a local copy

=head1 SYNOPSIS

rsync_baler [-h] [-v] [-V] [-f] [-x] [-R] [-j FILE] [-s sta_regex] [-r sta_regex] [-p pf] [-m email,email]

=head1 ARGUMENTS

Recognized flags:

=over 2

=item B<-h> 

Help. Produce this documentation

=item B<-v> 

Produce verbose output while running

=item B<-V>

Produce very-verbose output (debuggin)

=item B<-f>

Debug FTP connection. Run the FPT connections at full verbosity.

=item B<-x>

Verify and fix errors in the databases for each station. Don't produce 
reports or connect to any station.

=item B<-p file>

Parameter file name to use.

=item B<-j FILE>

Produce report of databases in json format and dump the data in FILE. Avoid running
in archival mode.

=item B<-s regex>

Select station regex. ('STA1|STA2' or 'A...|B.*')

=item B<-r regex>

Reject station regex. ('STA1|STA2' or 'A...|B.*')

=item B<-R>

Produce report of the archive.

=item B<-m email,email,email>

List of emails to send output

=back

=head1 DESCRIPTION

This script  creates a local repository of a field Baler44 station.
The script is simple and may fail if used outside ANF-TA installation. 

=head1 AUTHOR

Juan C. Reyes <reyes@ucsd.edu>

=head1 SEE ALSO

Perl(1).

=cut
#}}}
