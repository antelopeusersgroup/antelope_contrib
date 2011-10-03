#
#   rsync_baler: script to create a local copy of remote baler
#   author: Juan C. Reyes
#   email:  reyes@ucsd.edu
#   No BRTT support
#

#
#  Program setup
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
use Digest::MD5 qw[md5_hex];
use List::Util qw[max min];
use IPC::Cmd qw[can_run run];

our($opt_b,$opt_x,$opt_j,$opt_f,$opt_r,$opt_s,$opt_h,$opt_v,$opt_m,$opt_p,$opt_d,$opt_R);
our(%pf,@db,@db_sta,@db_ip,@db_on,$dbname,$dbpath);
our($dbout,$local_path,$start_of_report,@dbr,$nrecords);
our($station,@errors,%table,$time,$dfile,$bandwidth,$media);
our($parent,$reserve_media,$total_bytes,$bytes);
our($temp_sta,$ps_path);
our($start,$end,$run_time,$run_time_str,$type);
our($sta,@stas,$active_pids,$stations,$table,$folder);
our($pid,$log,$address,$ip_sta);
our($host,$key,$value,$file_fetch);
our(%ftp_hash,$dlsta,$net,$ip);
our($Problems,$problems_hash,$prob,$txt);
our($to_parent);
our($oldout,$olderr,$lsof,$ulimit);

use constant false => 0;
use constant true  => 1;

select STDOUT; $| = 1;
select STDERR; $| = 1;
#}}}

#{{{

    $parent = $$;
    $start = now();
    $host = my_hostname();

    unless ( &getopts('bj:fxhdvm:p:s:r:R') || @ARGV > 0 ) { 
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
        savemail();
        debug("Initialize mail") if $opt_d;
    }


    logging('');
    logging("$0 @ARGV");
    logging("Starting at ".strydtime(now())." on ".my_hostname());
    logging('');
    logging('');

    #
    # Implicit flag
    #
    $opt_v = defined($opt_d) ? $opt_d : $opt_v ;
    $opt_p ||= "rsync_baler.pf" ;

    #
    # Get parameters from config file
    #
    debug("Getting params") if $opt_d;
    %pf = getparam($opt_p);

    #
    ## Set File::Fetch options
    #
    $File::Fetch::WARN    = 0 unless $opt_d; 
    $File::Fetch::DEBUG   = 1 if $opt_d; 
    $File::Fetch::TIMEOUT = $pf{download_timeout};
    $File::Fetch::BLACKLIST = [qw/lwp netftp lftp lynx iosock ncftp/];
    #   File::Fetch
    #   Below is a mapping of what utilities will be used in what order for each schemes (if available):
    #       file    => LWP, lftp, file
    #       http    => LWP, wget, curl, lftp, lynx, iosock
    #       ftp     => LWP, Net::FTP, wget, curl, lftp, ncftp, ftp
    #       rsync   => rsync

    #
    ## Set IPC::Cmd options
    #
    $IPC::Cmd::VERBOSE = 1 if $opt_d;

    #
    # Verify Database
    #
    debug("Opening $pf{database}:") if $opt_d;

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
        debug("Write table in json file: $opt_j") if $opt_d;
    }

#}}}

#
#  Set mode [Report, JSON or Retrieval]
#
#{{{

    debug('Get list of stations:') if $opt_d;
    $stations = get_stations_from_db(); 

    #
    # Report and/or JSON file export
    #
    if ( $opt_R || $opt_j ) { json_and_report($stations); }

    #
    # Run this part for fixing the databases
    #
    elsif ( $opt_x ) { run_in_threads($stations,"clean_db"); }

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

    logging("dbsubset ( sta !~ /$opt_r/)") if $opt_v && $opt_r;
    @db_1 = dbsubset ( @db_1, "sta !~ /$opt_r/") if $opt_r;

    $nrecords = dbquery(@db_1,dbRECORD_COUNT) or log_die("No records to work with after dbsubset()"); 
    logging("dbsubset => nrecords = $nrecords") if $opt_v;


    for ( $db_1[3] = 0 ; $db_1[3] < $nrecords ; $db_1[3]++ ) { 

        ($dlsta,$net,$sta,$time,$endtime) = dbgetv(@db_1, qw/dlsta net sta time endtime/); 

        debug("[$sta] [$net] [$dlsta] [$time] [$endtime]") if $opt_d;

        $sta_hash{$sta}{dlsta}      = $dlsta; 
        $sta_hash{$sta}{net}        = $net; 
        $sta_hash{$sta}{status}     = 'Decom'; 
        $sta_hash{$sta}{ip}         = 0; 

        push @{ $sta_hash{$sta}{dates} }, [$time,$endtime];

    }

    dbfree(@db_1);


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

        foreach (sort @{$sta_hash{$sta}{dates}}) { debug("\t\t@$_") if $opt_d; }

    }

    #
    # Try open each database and create if missing
    # Set $close to 1 to avoid returning a db pointer
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
    my (@dbr_sorted,@dbr,@dbr_temp,@queries);
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

        debug("Now report on $temp_sta.") if $opt_d;

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
        undef @dbr_sorted;
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
        # Get list of flagged files
        #
        @dbr_temp= dbsubset ( @dbr, "status == 'flagged'");
        @dbr_sorted = dbsort(@dbr_temp,'-u','dfile');
        $nrecords = dbquery(@dbr_sorted, 'dbRECORD_COUNT') ;
        for ( $dbr_sorted[3] = 0 ; $dbr_sorted[3] < $nrecords ; $dbr_sorted[3]++ ) {
            push @flagged, dbgetv (@dbr_sorted, 'dfile');
        }
        dbfree(@dbr_temp);
        dbfree(@dbr_sorted);

        #
        # Get list of downloaded files
        #
        @dbr_temp= dbsubset ( @dbr, "status == 'downloaded'");
        @dbr_sorted = dbsort(@dbr_sorted,'-u','dfile');
        $nrecords = dbquery(@dbr_sorted, 'dbRECORD_COUNT') ;
        for ( $dbr_sorted[3] = 0 ; $dbr_sorted[3] < $nrecords ; $dbr_sorted[3]++ ) {
            push @downloaded, dbgetv (@dbr_sorted, 'dfile');
        }
        dbfree(@dbr_sorted);

        #
        # Check for missing files
        #
        @missing = unique_array(\@flagged,\@downloaded);
        if ( scalar @missing ) {

            @missing =  grep { $_ = "\"$_\"" } @missing;
            $text .= ",\n\t\"missing_files\": [" . join(',',@missing) . "]";
            $errors = "Missing ".@missing." files. ";
            debug("Station $temp_sta missing files:[@missing]") if $opt_d;

        }
        $report .= sprintf("%6d",scalar(@missing)) ." ";
        $report .= sprintf("%6d",scalar(@downloaded)) ." ";

        $text .= ",\n\t\"missing\": " . scalar(@missing);
        $text .= ",\n\t\"downloaded\": " . scalar(@downloaded);

        @dbr_sorted = dbsort(@dbr_temp,'time');
        $nrecords = dbquery(@dbr_sorted, 'dbRECORD_COUNT') ;

        if ( $nrecords > 0 ) {
            # Last downloaded
            $dbr_sorted[3] = $nrecords-1;

            ($dfile,$time) = dbgetv (@dbr_sorted, qw/dfile time/);

            $text .= ",\n\t\"last\": \"$dfile\"";
            $text .= ",\n\t\"last_time\": \"$time\"";
        }
        else {
            $text .= ",\n\t\"last\": \"UNKNOWN\"";
            $text .= ",\n\t\"last_time\": \"UNKNOWN\"";
        }
        dbfree(@dbr_sorted);
        dbfree(@dbr_temp);


        $nrecords = dbquery(@dbr, 'dbRECORD_COUNT') ;

        if ($nrecords > 0) {
            #
            # Calculate total downloaded data and bandwidth
            #
            for ( $dbr[3] = 0 ; $dbr[3] < $nrecords ; $dbr[3]++ ) {
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
            # Get md5 checks
            #
            @dbr_temp = dbsubset ( @dbr, "status == 'downloaded' && md5 == 'error-verify'");
            $nrecords = dbquery(@dbr_temp, 'dbRECORD_COUNT') ;
            if ( $nrecords >= 0 ) {
                $text .= ",\n\t\"error-verify\": \"$nrecords\"";
            }
            else {
                $text .= ",\n\t\"error-verify\": \"UNKNOWN\"";
            }
            dbfree(@dbr_temp);

            #
            # Get md5 checks
            #
            @dbr_temp = dbsubset ( @dbr, "status == 'downloaded' && md5 == 'error-download'");
            $nrecords = dbquery(@dbr_temp, 'dbRECORD_COUNT') ;
            if ( $nrecords > 0 ) {
                $text .= ",\n\t\"error-download\": \"$nrecords\"";
            }
            else {
                $text .= ",\n\t\"error-download\": \"UNKNOWN\"";
            }
            dbfree(@dbr_temp);

            #
            # Get md5 checks
            #
            @dbr_temp = dbsubset ( @dbr, "status == 'downloaded' && md5 == 'missing'");
            $nrecords = dbquery(@dbr_temp, 'dbRECORD_COUNT') ;
            if ( $nrecords > 0 ) {
                $text .= ",\n\t\"md5-missing\": \"$nrecords\"";
            }
            else {
                $text .= ",\n\t\"md5-missing\": \"UNKNOWN\"";
            }
            dbfree(@dbr_temp);

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
                @dbr_sorted = dbsort(@dbr_temp,'-u','dfile');
                $nrecords = dbquery(@dbr_sorted, 'dbRECORD_COUNT') ;

                $text .= " \"$_\": \"$nrecords\",";
                dbfree(@dbr_sorted);
                dbfree(@dbr_temp);

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

    logging( "{\n$text\n}" ) if $opt_d;

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
        # throttle the reading engine
        #
        sleep 0.5;

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
            $max_out = scalar @active_procs || 1;
            problem("Low on resources. Limit max_out=$max_out ") if $opt_v;
            redo STATION;
        }

        logging("Spawn: $function($station). Now: ".@active_procs." procs") if $opt_d;

        #
        # Send msgs from child to parent
        #
        #unless ( socketpair($$station{from_child}, $$station{to_parent}, AF_UNIX, SOCK_STREAM, PF_UNSPEC) ) {  
        unless ( pipe($$station{from_child}, $$station{to_parent}) ) {  
            problem("run_in_threads(): ERROR... pipe():$! ");
            $max_out = scalar @active_procs || 1;
            problem("run_in_threads(): setting max_out=$max_out ");
            redo STATION;
        }

        $$station{from_child}->autoflush(1);
        $$station{to_parent}->autoflush(1);

        fcntl($$station{from_child},F_SETFL, O_NONBLOCK);
        fcntl($$station{to_parent},F_SETFL, O_NONBLOCK);

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
        # We can get to max files opened (test with ulimit -n) if not carefull
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

        while ( <$fh> ) {

            while (/\[LOG:(.*?)\]/g )     { logging($station.": ".$1); }
            while (/\[DEBUG:(.*?)\]/g )   { debug($station.": ".$1);   }
            while (/\[PROBLEM:(.*?)\]/g ) { problem($1,$station); }

        }

        unless ( check_pids( $stations->{$station}->{pid} ) ) {
            close $$station{from_child};
            close $$station{to_parent};
            undef $$station{from_child};
            undef $$station{to_parent};
        }

    }
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

    debug( sprintf("Memory in use: %0.1f%% (%0d/%0d)", $ratio, $used, $physical) ) if $opt_d;

    return 0 unless ($ratio && $used && $physical);

    #
    # Stop here is we are over 85% of real memory usage (don't care about swap)
    #
    return 0 if $ratio > 85;

    #
    # Test CPU loads
    #
    my ($ncpu, $idle, $user, $kernel, $iowait, $swap, @the_rest) = syscpu();
    sleep 1; # This is the only way syscpu() works... don't ask me :-P
    ($ncpu, $idle, $user, $kernel, $iowait, $swap, @the_rest) = syscpu();

    for (1 .. $ncpu) {

        #
        # Look for 1 CPU with more than 15% idle time
        #
        $idle   = shift @the_rest if $_ != 1;
        $user   = shift @the_rest if $_ != 1;
        $kernel = shift @the_rest if $_ != 1;
        $iowait = shift @the_rest if $_ != 1;
        $swap   = shift @the_rest if $_ != 1;
        debug( sprintf("CPU $_: idle(%0.2f)  user(%0.2f)  kernel(%0.2f) iowait(%0.2f)  swap(%0.2f)\n",
                $idle, $user, $kernel, $iowait, $swap) ) if $opt_d;
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
            #debug("No child running. RESP = $?") if $opt_d;
        }
        elsif (WIFEXITED($?)) {
            #debug("\tDone with $_") if $opt_d;
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
    my ($d_data,$start_sta,$start_file,$where,$attempts,@missing);
    my ($rem_s,$loc_s,@diff,$results,$run_time_str,$dbout);
    my (@dates,$end_file,$record,$dlsta,$time,$endtime,$dir,$dfile);
    my ($k,$m,$g,$total_size,%temp_hash,@total_downloads,@download);
    my ($digest,$hexd,$md5,$remote_file_content, $remote_file_handle);
    my ($http_folder,$md5_lib,@original_downloads);


    #
    # Prepare Variables and Folders
    #
    $ip     = $table->{ip};
    $dlsta  = $table->{dlsta};
    $net    = $table->{net};
    @dates  = $table->{dates};
    $local_path = prepare_path($station); 


    $start_sta = now();

    log_die("DIE: No value for station.") unless $station;

    if ( $opt_v ) {
        debug("Station $station is not active in deployment table.") unless $table->{status} eq 'Active';
    }
    return unless $table->{status} eq 'Active';

    log_die("DIE: No IP for station $station") unless $ip;

    $d_data = total_data_downloaded($station,21) || 0.0;

    problem("Downloaded ( $d_data ) Mbts in the last 21 days! Skipping.") if $d_data > 3000;
    return if $d_data > 3000;

    $ftp = loggin_in($ip,$station);
    log_die("DIE: $station DOWN!!!!") unless $ftp;

    debug("Start time ".strydtime($start_sta)) if $opt_d;

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

        #
        # Check file in baler first
        #
        ($folder,$r_size) = test_baler_file($station,$net,$dlsta,$ip,$file);
        $folder ||= 0;
        $r_size ||= 0; 

        debug("test_baler_file($station,$net,$dlsta,$ip,$file) => ($folder,$r_size)") if $opt_d;
        problem("Cannot locate $file in $ip") unless $folder;
        next unless $folder;

        #
        # Build local path to file
        #
        $local_path_file = File::Spec->rel2abs( "$local_path/$file" ); 

        #
        # Verify if we have the file in the local dir
        #
        problem("Wont re-download file:$local_path_file") if -e $local_path_file;
        next if -e $local_path_file;

        #
        # Update DB
        #
        @dbr = open_db($station);
        @dbr_sub = dbsubset(@dbr, "dlsta == '$dlsta' && dfile == '$file' && status == 'downloading' " );
        $attempts = dbquery(@dbr_sub,dbRECORD_COUNT) ; 

        $attempts += 1;

        debug("dbaddv: $dlsta | $local_path | $attempts | 'downloading'") if $opt_d;

        dbaddv(@dbr, 
            "net",      $net,
            "dlsta",    $dlsta,
            "dfile",    $file,
            "sta",      $station,
            "time",     now(), 
            "status",   "downloading",
            "filebytes",$r_size, 
            "dir",      $local_path,
            "attempts", $attempts,
            "lddate",   now() );

        dbclose(@dbr);

        if ( $r_size ) {

            $where = '';

            #
            # download on FTP
            #
            eval{ $file_fetch = File::Fetch->new(uri => "ftp://$ip:$pf{ftp_port}/$folder/$file"); };
            problem("File::Fetch -> $@") if $@; 

            debug("Start ftp download of:".$file_fetch->uri) if $opt_d;

            eval {  $where = $file_fetch->fetch( to => "$local_path/" ); };
            problem("File::Fetch ".$file_fetch->uri." $@") if $@; 


            unless ( $where ) {
                #
                # download on HTTP
                #

                $http_folder = $folder eq 'activemedia' ? 'WDIR' : 'WDIR2';

                eval{ $file_fetch = File::Fetch->new(uri => "http://$ip:$pf{ftp_port}/$http_folder/$file"); };
                problem("File::Fetch -> $@") if $@; 

                debug("Start http download of:".$file_fetch->uri) if $opt_d;

                eval {  $where = $file_fetch->fetch( to => "$local_path/" ); };
                problem("File::Fetch ".$file_fetch->uri." $@") if $@; 

            }

        }
        else {

            #
            # This is a 0 size file. Just touch the file in the local system.
            #
            problem("0 size file => $file"); 
            $where = "$local_path/$file";
            open FILE, ">", $where or log_die("Could not create file ( $where ) :$!");
            close FILE;
            log_die("Could not create file ( $where )") unless -e $where;

        }

        $end_file = now();
        $run_time = $end_file-$start_file;
        $run_time_str = strtdelta($run_time);
        #}}}

        if( $where ) { 
        #{{{ if download is SUCCESS
            #
            # Keep track of total data downloaded
            #
            $size = -s $where; 
            $total_size += $size;

            if ($size == $r_size) {
            #{{{
                #
                # Success download
                #
                if ( $r_size > 0 ) {

                    #
                    # For files with data
                    #
                    debug("Success in download of $file after $run_time_str") if $opt_d;

                    push @total_downloads, $file;

                    #
                    # Verify bandwidth of ftp connection
                    #
                    $speed = ((-s $where) / 1024 ) / $run_time;
                    debug("$file $size Kb  $run_time secs $speed Kb/sec") if $opt_d;

                    #
                    # Check md5 of file
                    #
                    $md5 = get_md5($station,$net,$dlsta,$ip,$file) || 0;

                    debug("Reported MD5 for $file : $md5") if $opt_d and $md5;

                    problem("Cannot get: ($file.md5)") unless $md5;

                    if ( $md5 ) {

                        #
                        # Open file and get local md5
                        #
                        open(FILE,$where) or log_die("Cannot open file $where for md5 calc.");
                        $md5_lib = Digest::MD5->new;
                        $md5_lib->addfile(FILE);
                        $digest = $md5_lib->hexdigest || 0;
                        close FILE;

                        debug("Calculated MD5 for $file : $digest") if $opt_d;

                        problem("Cannot produce MD5 for: ($file)") unless $digest;

                        if ( $digest eq $md5 ) {
                            debug("Matched MD5 for $file." ) if $opt_d;
                        }
                        else {
                            problem("$file MD5 problem:: reported:$md5 calc:$digest " );
                            $md5 = 'error-download';
                        }
                    }
                    else {
                        $md5 = 'missing';
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
                debug("dbaddv: $dlsta | $start_file | $end_file | $attempts | 'downloaded' | $size | $speed | $md5") if $opt_d;

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
                    "fixed",    'n', 
                    "md5",      $md5,
                    "dfile",    $file,
                    "lddate",   now(),
                    "status",   "downloaded");
                dbclose(@dbr);
            #}}}
            }
            else {
            #{{{

                problem("( $size ) is not the reported ( $r_size ) for ( $where ) "); 
                problem("Remove file ( $where ) from local directory."); 
                #
                # Add to DB
                #
                debug("dbaddv: $dlsta | $start_file | $end_file | $attempts | 'error-download' | $size ");

                @dbr = open_db($station);
                dbaddv(@dbr, 
                    "net",      $net, 
                    "sta",      $station, 
                    "time",     $start_file, 
                    "endtime",  $end_file, 
                    "dir",      $local_path, 
                    "attempts", $attempts, 
                    "filebytes",$size, 
                    "dlsta",    $dlsta,
                    "dfile",    $file,
                    "lddate",   now(),
                    "status",   "error-download");
                dbclose(@dbr);
                unlink $where;
            #}}}
            }
        #}}} 
        } # end if $where
        else {
        #{{{ if download FAILS
            #
            # If download failed... $where == NULL
            #
            $run_time_str = strtdelta(now()-$start_file);
            if ( -e "$local_path/$file") {
                problem("Failed download of $file after $run_time_str. File present in local archive!");
                $size = -s "$local_path/$file";
                #
                # Add to DB
                #
                debug("dbaddv: $dlsta | $start_file | $end_file | $attempts | 'error-download' | $size ");

                @dbr = open_db($station);
                dbaddv(@dbr, 
                    "net",      $net, 
                    "sta",      $station, 
                    "time",     $start_file, 
                    "endtime",  $end_file, 
                    "dir",      $local_path, 
                    "attempts", $attempts, 
                    "filebytes",$size, 
                    "dlsta",    $dlsta,
                    "dfile",    $file,
                    "lddate",   now(),
                    "status",   "error-download");
                dbclose(@dbr);
                unlink "$local_path/$file";
            }
            else { 
                problem("Failed download of $file after $run_time_str. File missing from local archive!");
            }
        #}}}
        }

    } #end of foreach @download 

    @missing = unique_array(\@download,\@total_downloads);

    if ( scalar @missing && scalar @missing == scalar @original_downloads ) {
        problem( "NO DOWNLOADS!!!! Station not downloading any files.");
    }
    elsif ( scalar @missing ) {
        @missing = compare_dirs($station,$dlsta,$net,$ip,@dates);
    }

    problem( "Missing: " . @missing . " files") if @missing;
    debug( "Missing files: \n\n@missing") if @missing && $opt_d;

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
                $end   = now() if $end < $start;


                #
                # Overide start time with 
                # the value for the last 4 
                # months. Temp fix for problem
                # with balers. ONLY for baler queries.
                #
                #$start= str2epoch("-125days") if $folder;

                debug("Create regex for:".strtime($start)."=>".strtime($end)) if $opt_d;

                #debug("Overwrite for last 4 months:".strtime($start)."=>".strtime($end)) if $opt_d;

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
    # working. It happens...
    #
    foreach (reverse sort keys %queries) { debug("Regex=$_") if $opt_d};

    return reverse sort keys %queries;

#}}}
}

sub total_data_downloaded {
#{{{
    my $sta  = shift;
    my $days = shift || 1;
    my @db;
    my @db_temp;
    my @db_subset;
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

    @db_subset = dbsubset ( @db, "status == 'downloaded' || status == 'error-download'");
    $nrecords = dbquery(@db_subset, 'dbRECORD_COUNT') ;
    return unless $nrecords;


    $start_of_report = str2epoch("-${days}days");
    @db_temp= dbsubset ( @db, "time >= $start_of_report");
    $nrecords = dbquery(@db_temp, 'dbRECORD_COUNT') ;
    return unless $nrecords;

    if ($nrecords > 0) {
        for ( $db_temp[3] = 0 ; $db_temp[3] < $nrecords ; $db_temp[3]++ ) {
            $total_bytes += dbgetv (@db_temp, 'filebytes');
        }
    }
    dbfree(@db_subset);
    dbfree(@db_temp);
    dbclose(@db);

    # for Kbytes
    $total_bytes = sprintf("%0.2f", $total_bytes/1024);
    # for Mbytes
    $total_bytes = sprintf("%0.2f", $total_bytes/1024);


    return $total_bytes;
#}}}
}

sub open_db {
#{{{
    # $close is a flag to set 
    # avoids the return of db pointers.
    my $sta = shift;
    my $close = shift || 0;
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

    debug("Opening database ($dbout).") if $opt_d;

    #
    # Create descriptor file if missing
    #
    unless ( -e $dbout) {
        debug("$sta Creating new database ($dbout).") if $opt_d;

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
    debug("$sta Openning database table  ($dbout.rsyncbaler)") if $opt_d;
    @db  = dbopen($dbout,"r+") or log_die("Can't open DB: $dbout",$sta);
    @db  = dblookup(@db,"","rsyncbaler","","") or log_die("Can't open DB TABLE: $dbout.rsyncbaler",$sta);

    #
    # Close database if we don't need 
    # to return pointer
    #
    dbclose(@db) if $close;

    return if $close;

    return @db;
#}}}
}

sub clean_db {
#{{{
    my ($station,$table)= @_;
    my ($dlsta,$net,@db_sort);
    my (@remove,@dbr,@dbr_temp,@db_subset,@flagged,%flagged); 
    my ($size,$get_md5,$mode,$record,$lf,$path,@downloaded);
    my ($md5,$md5_lib,$ip,$get_filebytes,$get_status,$get_dlsta);
    my ($line,$digest,$get_net,$get_sta,$get_dir,$get_dfile);

    $dlsta  = $table->{dlsta};
    log_die("Don't have value of 'dlsta' for $station in clean_db()") unless $dlsta;

    $net    = $table->{net};
    log_die("Don't have value of 'net' for $station in clean_db()") unless $net;

    $ip = $table->{ip} if $table->{status} eq 'Active';

    $ip ||= 0;

    #
    # Prepare PATH
    #
    $path = prepare_path($station);

    #
    # Verify each entry on the directory
    #
    FILE: foreach $lf ( read_local( $station ) ) {

        #
        # Verify the files entered as downloaded
        #
        debug("Subset dfile == $lf && status == downloaded ") if $opt_d;
        @dbr = open_db($station);
        @dbr_temp = dbsubset(@dbr, "dfile =='$lf' && status == 'downloaded'");
        $record = dbquery(@dbr_temp, dbRECORD_COUNT);
        dbfree(@dbr_temp);
        dbclose(@dbr);

        if ( $record == 0 ) {
            #
            # Add the missing file
            #
            $size = -s "$path/$lf" || 0; 
            @dbr = open_db($station);
            problem("file $lf not in database. Adding as 'downloaded'");
            dbaddv(@dbr, 
                "net",      $net,
                "sta",      $station,
                "dir",      $path,
                "dlsta",    $dlsta,
                "dfile",    $lf,
                "filebytes",$size, 
                "attempts", 1,
                "time",     now(), 
                "fixed",    "n",
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
                # If we have more than one with attempts == 1 the prev find will fail....
                $dbr[3] = dbfind(@dbr, "dfile == '$lf' && status == 'downloaded'", -1) if $dbr[3] < 0;
                problem("Error in the pointer:record#($dbr[3])") if $dbr[3] < 0;
                next if $dbr[3] < 0;
                dbputv(@dbr,'status','extra','lddate', now() );
                dbclose(@dbr);
            }

        }

    }

    #
    # Fix tables for sta and net values
    #
    debug("Test each entry in database for $station ") if $opt_d;
    @dbr = open_db($station);
    @db_sort = dbsort(@dbr,'dfile');
    $record = dbquery(@db_sort, 'dbRECORD_COUNT') ;
    LINE: for ( $db_sort[3] = 0 ; $db_sort[3] < $record ; $db_sort[3]++ ) {
    
        ($get_dlsta,$get_net,$get_sta,$get_dir,$get_dfile,$get_status,$get_filebytes,$get_md5) = dbgetv (@db_sort, qw/dlsta net sta dir dfile status filebytes md5/);
    
        unless ( $get_dfile =~ /.*($station).*/ ) {

            problem("File ($get_dfile) is not related to station ($station). Removing.");
            push @remove, $get_dfile;
            next LINE;

        }

        unless ( $get_sta && $get_net ) {

            #
            # Add sta and net code to entry
            #
            ($get_net, $get_sta) = split(/_/, $get_dlsta, 2);
            dbputv(@db_sort, "sta",$get_sta, "net",$get_net);

        }

        next LINE if $get_status !~ /downloaded/;

        unless ( -f "$get_dir/$get_dfile" ) {

            #
            # Fix path on entry
            #
            problem("Fix dir($get_dir)=>($path) for $get_dfile");
            dbputv(@db_sort, "dir", $path);
            debug("Update dir value for $get_dfile" ) if $opt_d;
            $get_dir = $path;

        }

        unless ( -f "$get_dir/$get_dfile" ) {

            problem("File missing: ($get_dir/$get_dfile). Removeing.");
            push @remove, $get_dfile;
            next LINE;

        }

        #
        # Verify mode 0664
        #
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
            dbputv(@db_sort, "filebytes", -s "$get_dir/$get_dfile");
            debug("Update filebytes value for $get_dfile" ) if $opt_d;

        }

        # fix NULL value
        $get_md5 = 0 if $get_md5 eq '-';
        $get_md5 = 0 if $get_md5 eq 'error';
        $get_md5 = 0 if $get_md5 eq 'missing';

        if ( $ip && ! $get_md5 ) {

            debug("Start MD5 test $get_dfile ") if $opt_d;

            #
            # Check md5 of file
            #
            $md5 = get_md5($get_sta,$net,$dlsta,$ip,$get_dfile) || 0;

            debug("Reported MD5 for $get_dfile : $md5") if $opt_d and $md5;

            problem("Missing: ($get_dfile.md5)") unless $md5;

            if ( $md5 ) {

                #
                # Open file and get local md5
                #
                open(FILE,"$get_dir/$get_dfile") or log_die("Cannot open file $get_dir/$get_dfile for md5 calc.");
                $md5_lib = Digest::MD5->new;
                $md5_lib->addfile(FILE);
                $digest = $md5_lib->hexdigest || 0;
                close FILE;

                debug("Calculated MD5 for $get_dfile : $digest") if $opt_d;

                problem("Cannot produce MD5 for: ($get_dfile)") unless $digest;

                if ( $digest eq $md5 ) {
                    debug("Matched MD5 for file." ) if $opt_d;
                }
                else {
                    problem("MD5 does not match: file:$get_dfile reported:$md5 calc:$digest " );
                    $md5 = 'error-verify';
                }
            }
            else { 
                $md5 = 'missing';
            }

            if ( $md5 ) {
                dbputv (@db_sort, 'md5', $md5);
                debug("Update MD5 value for $get_dfile" ) if $opt_d;
            }

        }

    }

    dbfree(@db_sort);
    dbclose(@dbr);

    remove_file($station,$_,1) foreach @remove;


    debug("Done fixing database for $station") if $opt_d;


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
    debug("Subset for status == flagged") if $opt_d;
    @db_t= dbsubset(@dbr, "status == 'flagged'");
    $record  =  dbquery(@db_t, "dbRECORD_COUNT");
    for ( $db_t[3] = 0 ; $db_t[3] < $record ; $db_t[3]++ ) {
    
        push @flagged, dbgetv (@db_t, 'dfile');

    }
    dbfree(@db_t);

    debug("Subset for status == downloaded") if $opt_d;
    @db_t= dbsubset(@dbr, "status == 'downloaded'");
    $record  =  dbquery(@db_t, "dbRECORD_COUNT");
    for ( $db_t[3] = 0 ; $db_t[3] < $record ; $db_t[3]++ ) {
    
        push @downloaded, dbgetv (@db_t, 'dfile');

    }
    dbfree(@db_t);

    #
    # Make unique
    #
    @flagged = unique_array(\@flagged);
    @downloaded = unique_array(\@downloaded);

    logging("Files flagged: ".@flagged) if $opt_v;
    logging("Files downloaded: ".@downloaded) if $opt_v;

    @flagged = unique_array(\@flagged,\@downloaded);

    problem("Previously flagged and missing (".@flagged.") files.") if @flagged;
    logging("@flagged") if $opt_d && @flagged;

    # Avoid connecting to the stations
    # for a list of directories if we 
    # have more than 30 files pending.
    problem("Avoid baler listing. Missing (".@flagged.") files.") if scalar @flagged > 30;
    return @flagged if scalar @flagged > 30;

    # or get new list
    %remote = read_baler( $station, $ip ,@dates);
    @remote_files = sort keys %remote;


    # 
    # Compare local to remote
    #
    foreach $rf ( unique_array(\@remote_files,\@local_files) ) {

        debug("Test local<=>remote: $rf") if $opt_d;

        debug("Subset for dfile == $rf && status == flagged") if $opt_d;
        @db_t= dbsubset(@dbr, "dfile == '$rf' && status == 'flagged'");
        $record  =  dbquery(@db_t, "dbRECORD_COUNT");
        dbfree(@db_t);
        debug("dbaddv: $rf $station 'flagged'") if $opt_d;

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
        debug("Compare size of ($rf) to local copy") if $opt_d;
        next if ($remote{$rf} == 0);
        next if ((-s "$path/$rf") == $remote{$rf});
        push @flagged, $rf;
        problem("File size don't match: $rf (local)".(-s "$path/$rf")." != (remote)$remote{$rf}");

        #
        # remove file AND add new Flagged entry to db
        #
        @dbr = open_db($station);
        debug("Subset for dfile == $rf && status == flagged") if $opt_d;
        @db_t= dbsubset(@dbr, "dfile == '$rf' && status == 'flagged'");
        $record  =  dbquery(@db_t, "dbRECORD_COUNT");
        dbfree(@db_t);
        dbclose(@dbr);

        remove_file($station,$rf,1);

        @dbr = open_db($station);
        debug("dbaddv: $rf $station 'flagged'") if $opt_d;

        dbaddv(@dbr, 
            "net",      $net,
            "sta",      $station,
            "dlsta",    $dlsta,
            "dfile",    $rf,
            "attempts", $record + 1,
            "time",     now(), 
            "lddate",   now(), 
            "status",   "flagged");

    }

    dbclose(@dbr);

    return unique_array(\@flagged);

#}}}
}

sub loggin_in {
#{{{
    my $ip      = shift || 0;
    my $station = shift || 0;
    my $debug   = shift || 0;
    my ($ftp,$p,$res);
    my $test = 0;

    log_die("Missing parameter for ftp connection. sta($station) ip($ip)") unless $ip and $pf{ftp_port};

    if ( defined $ftp_hash{$station} ) {

        $ftp = $ftp_hash{$station};
        eval { $test = $ftp->ls('/'); };
        problem("Cannot ftp->ls(/) on $ip:$pf{ftp_port} ($@)") if $@;

        debug("Success in ftp->ls() to $station $ip") if $opt_d and $test;

        return $ftp if $test;
    } 


    $debug = 1 if $opt_f;

    for ( 1..5 ) {

        $ftp = 0;
        $res = '';
        $ftp_hash{$station} = 0;


        sleep 30 unless $_ == 1;

        #
        # Build FTP connection
        #
        debug("Net::FTP $station=>$ip:$pf{ftp_port}") if $opt_d;
        eval { $ftp = Net::FTP->new(Host=>$ip, Passive=>1, Timeout=>900, Port=>$pf{ftp_port}, Debug=>$debug) };
        if ( $@ or not $ftp ) {
            problem("Cannot build Net::FTP object for $station $ip:$pf{ftp_port}");
            next;
        }

        eval { $ftp->login() or problem("Cannot login:" . $ftp->message . " Attempt:$_"); };
        problem("Cannot ftp->loggin() on $ip:$pf{ftp_port} ($@)") if $@;

        eval { $res = $ftp->ls('/') };
        problem("Cannot ftp->ls(/) on $ip:$pf{ftp_port} ($@)") if $@;
    
        if ( $res ) {

            debug("Success in ftp->ls('/') to $station $ip") if $opt_d;

            $ftp_hash{$station} = $ftp;

            return $ftp;

        }
        
        problem("Cannot ftp->ls(/):" . $ftp->message . " Reconnect. Attempt:$_") if $opt_v;

    }

    problem("$ip:$pf{ftp_port} failed connection command") if $opt_v;

    return;

#}}}
}

sub read_baler {
#{{{
    my $sta   = shift;
    my $ip    = shift;
    my @dates = shift;
    my ($dir,$folder,$path,$ftp,$name,$test);
    my %list;
    my @temp_dir = ();
    my (@n,@queries);
    my $attempt = 1;

    #
    # For each of the folders
    #
    foreach $dir ( qw/activemedia reservemedia/ ) {

        foreach $folder ( qw/recover data/ ) {
        #{{{
            $path = "/$dir/$folder/";

            debug("Read baler $sta $path") if $opt_d;

            $ftp = loggin_in($ip,$station);
            problem("Cannot connect to $sta $folder ($ip:$pf{ftp_port})") unless $ftp;
            next unless $ftp;

            #
            # Get list from Baler
            #
            foreach $test ( build_time_regex($sta,@dates,$dir) ) {
            #{{{

                next unless $test;

                $attempt  = 1;

                while ( $attempt <= 3 ) {
                    #{{{
                    #
                    # Get list for this month
                    #
                    debug("$sta $ip:$pf{ftp_port} ftp->dir($path)(connection attempt $attempt).") if $opt_d;
                    @temp_dir = ();
                    @temp_dir = $ftp->dir("$path/$test");
                    debug("$sta $ip:$pf{ftp_port} ftp->dir($path)=> @temp_dir.") if $opt_d;

                    problem("RESERVEMEDIA in use!") if (@temp_dir && $dir eq 'reservemedia' );

                    #
                    # Parse results and get size
                    #
                    foreach (@temp_dir) {

                        next unless $_;
                        next if /^d.+\s\.\.?$/;
                        @n = split(/\s+/, $_, 9);
                        next unless $n[8];
                        $name = ( split(/\//,$n[8]) )[-1];
                        if ( $name =~ /\.md5/ ) {
                            $name =~ s/\.md5//;
                            $list{$name} = 0;
                        }
                        else {
                            $list{$name} = int($n[4]);
                        }
                        logging("Parsed: $name => $n[4]") if $opt_d;
                    }

                    #
                    # If we have files, save them and exit loop.
                    #
                    last if scalar @temp_dir;

                    #
                    # RESERVEMEDIA should be empty.
                    #
                    last if $dir eq 'reservemedia';

                    #
                    # Prepare for a second attempt.
                    #
                    $attempt ++;
                    #eval{ $ftp->quit(); };
                    sleep 5;
                    $ftp= loggin_in($ip,$sta);
                    problem("Cannot connect to $sta $path ($ip:$pf{ftp_port})") unless $ftp;
                    last unless $ftp;

                    #}}}
                } # end of while loop

                #problem("Net::FTP $ip empty list for query: $test", $sta) unless ( scalar @temp_dir || $dir eq 'activemedia');
            #}}}
            }

            #eval{ $ftp->quit(); }; #}}}
        }
    }

    return %list;

#}}}
}

sub get_md5 {
#{{{
    my $sta   = shift;
    my $net   = shift;
    my $dlsta = shift;
    my $ip    = shift;
    my $file  = shift;
    my ($ftp,$local_path,$md5,$folder,$size);
    my @md5_raw;

    debug("get_md5($sta,$net,$dlsta,$ip,$file)") if $opt_d;

    return unless  $sta and $ip and $file;

    $local_path = prepare_path($sta); 

    $local_path .= '/md5/';

    $md5 = $file . '.md5';

    mkdir $local_path unless -d $local_path;

    log_die("Cannot make directory ($local_path)") unless -d $local_path;

    debug("Get MD5 file $file.md5 -> $local_path") if $opt_d;

    chdir $local_path or log_die("Cannot change to directory ($local_path)");

    unless ( -s "$local_path/$md5" ) {

        debug("Lets download MD5 file $md5 -> $local_path") if $opt_d;

        ($folder,$size) = test_baler_file($sta,$net,$dlsta,$ip,$file,1);
        $folder ||= 0;
        $size ||= 0; 

        debug("test_baler_file($station,$net,$dlsta,$ip,$md5) => ($folder,$size)") if $opt_d;

        problem("Cannot locate $md5 in baler") unless $folder;
        return unless $folder;

        $ftp = loggin_in($ip,$sta) 
            or problem("Cannot connect to $sta $ip");

        debug("FTP get MD5 file $md5 ") if $opt_d;

        eval { $ftp->get("$folder/$md5") or problem("get failed ". $ftp->message); };
        problem("get failed ". $ftp->message) if $@;

        debug("ftp->get($folder/$md5) => ". $ftp->message) if $opt_d;

        problem("Cannot get MD5 for: (file:$file,ip:$ip,sta:$sta)") unless -s "$local_path/$md5";

        return unless -s "$local_path/$md5";

    }

    debug("Lets read MD5 file $md5 from local archive $local_path") if $opt_d;

    open(DAT, '<', "$local_path/$md5") or log_die("Cannot open $local_path/$md5!");

    while (<DAT>) {
        chomp;
        debug("MD5:: $_") if $opt_d;
        push @md5_raw, split;
    }

    close(DAT);

    debug("Got MD5 of: " .$md5_raw[0]) if $opt_d;

    return $md5_raw[0];

#}}}
}

sub test_baler_file {
#{{{
    my $sta   = shift;
    my $net   = shift;
    my $dlsta = shift;
    my $ip    = shift;
    my $file  = shift;
    my $md5   = shift || 0;
    my $name = '';
    my @n;
    my ($path,$ftp,$size,$dir);

    debug("test_baler_file($sta,$net,$dlsta,$ip,$file)") if $opt_d;

    #
    # For each of the folders
    #
    foreach $dir ( qw/activemedia reservemedia/ ) {

        #
        # Init Net::FTP connection
        #
        $ftp = loggin_in($ip,$station);
        problem("Cannot connect to $station $ip:$pf{ftp_port}") unless $ftp;
        return unless $ftp;

        unless ( $md5 ) {
            #
            # if we are testing for a data file...
            #

            $path = "/$dir/data/";


            #
            # Get file info
            #
            debug("$sta $ip:$pf{ftp_port} ftp->size('$path' '$file').") if $opt_d;
            eval { $size = $ftp->size("$path/$file") };
            problem("$ip:$pf{ftp_port}->size('$path' '$file')($@)") if $@;

            if ( defined $size  ) {

                debug("FTP->size('$path' '$file')=>($size)") if $opt_d;

                problem("$file in RESERVEMEDIA ") if ($dir eq 'reservemedia' );

                return $path,$size;
            }
        }

        #
        # Lets try to get the md5 of the file
        #
        $path = "/$dir/recover/";

        #
        # Get file info
        #
        debug("$sta $ip:$pf{ftp_port} ftp->size('$path/$file.md5').") if $opt_d;
        eval { $size = $ftp->size("$path/$file.md5") };
        problem("$ip:$pf{ftp_port}->size($path/$file.md5)($@)") if $@;

        if ( defined $size ) {

            problem("$file in RESERVEMEDIA ") if ($dir eq 'reservemedia' );


            unless ( $md5 ) {

                problem("$file.md5 in ($path) but not in ($dir/data/)");

                #
                # Add file as missing
                #
                @dbr = open_db($sta);
                debug("Adding $file as 'missing'") if $opt_d;
                dbaddv(@dbr, 
                    "net",      $net,
                    "sta",      $sta,
                    "dlsta",    $dlsta,
                    "dfile",    $file,
                    "time",     now(), 
                    "lddate",   now(), 
                    "status",   "missing");
                dbclose(@dbr);

                debug("$file added as 'missing'.") if $opt_d;

                return;
            }

            return $path,$size;

        }

    }

    problem("Cannot locate ($file) in baler. $ip:$pf{ftp_port}") if $opt_v;

    return;

#}}}
}

sub read_local {
#{{{
    my $sta = shift;
    my %list;
    my $file;
    my $f;

    debug("Reading local directory") if $opt_d;

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

    foreach (sort keys %list) { debug("LOCAL: $_") if $opt_d; }

    return sort keys %list;

#}}}
}

sub incomplete_file {
#{{{
    my $sta      = shift;
    my $file     = shift;
    my @db; 
    my $nrecords;
    my $downloaded;

    my $path = prepare_path($sta);

    problem("Removing $path/$file");

    mkdir "$path/trash" unless -d "$path/trash";

    log_die("Cannot make directory ($path/trash/)") unless -d "$path/trash";

    #
    # Verify file in folder
    #
    if ($file and -f "$path/$file") {
        debug("move $path/$file to $path/trash/$file") if $opt_d;
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
        problem("Re-tag #$nrecords records for $file");
        foreach ( 1 .. $nrecords ) { 
            @db = open_db($sta);
            $db[3] = dbfind(@db, "dfile =~ /$file/ && status == 'downloaded'", -1); 
            dbputv(@db,"status","error-download") if ($db[3] >= 0);;
        }
    }

    dbclose(@db);

    return;
#}}}
}

sub remove_file {
#{{{
    my $sta      = shift;
    my $file     = shift || '';
    my $downloaded = shift || 0;
    #
    # downloaded is for files that we 
    # want to download again...
    #
    my @db; 
    my @db_subset; 
    my @db_temp; 
    my $nrecords;

    my $path = prepare_path($sta);

    if ( $file ) { problem("Removing $path/$file"); }
    else { problem("Removing NULL entry for $path"); }

    mkdir "$path/trash" unless -d "$path/trash";

    log_die("Cannot make directory ($path/trash/)") unless -d "$path/trash";

    #
    # Verify file in folder
    #
    if ($file and -f "$path/$file") {
        debug("move $path/$file to $path/trash/$file") if $opt_d;
        move("$path/$file","$path/trash/$file") or problem("Can't move $file to $path/trash");
    }

    #
    # Verify DB for file
    #
    @db = open_db($sta);
    if ( $downloaded ) { 
        @db_temp = dbsubset ( @db, "status == 'downloaded'"); 
        dbfree(@db);
        @db = @db_temp;
        dbfree(@db_temp);
    }

    if ( $file ) { 
        @db_temp = dbsubset ( @db, "dfile =~ /$file/"); 
        dbfree(@db);
        @db = @db_temp;
        dbfree(@db_temp);
    }
    else { 
        @db_temp = dbsubset ( @db, "dfile == ''"); 
        dbfree(@db);
        @db = @db_temp;
        dbfree(@db_temp);
    }

    $nrecords = dbquery(@db, 'dbRECORD_COUNT') ;
    problem("Delete #$nrecords records for $file");
    dbclose(@db);

    # 
    # If found
    #
    if ( $nrecords ) {
        foreach ( 1 .. $nrecords ) { 
            @db = open_db($sta);
            if ( $file and $downloaded) { 
                $db[3] = dbfind(@db, "dfile =~ /$file/ && status == 'downloaded'", -1); 
                dbputv(@db,'status','error-download','lddate', now() );
            }
            elsif ( $file ) { 
                $db[3] = dbfind(@db, "dfile =~ /$file/", -1); 
                dbdelete(@db) if ($db[3] >= 0) ; 
            }
            else { 
                $db[3] = dbfind(@db, "dfile == ''", -1); 
                dbdelete(@db) if ($db[3] >= 0) ; 
            }
            dbclose(@db);
        }
    }

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

    foreach  (qw/local_data_dir max_child_run_time download_timeout 
                database http_port max_procs ftp_port/){
        $pf{$_} = pfget($PF,$_);

        log_die("Missing value for $_ in PF:$PF") unless defined($pf{$_});

        debug( sprintf("\t%-22s -> %s", ($_,$pf{$_})) ) if $opt_d;
    }

    return (%pf);
#}}}
}

sub table_check {
#{{{
    my $db = shift;
    my $sta = shift;

    $sta ||= '';

    debug("Verify Database: ".dbquery(@$db,"dbDATABASE_NAME") ) if $opt_d;

    log_die( dbquery(@$db,"dbTABLE_NAME")." not available.",$sta) unless dbquery(@$db,"dbTABLE_PRESENT");

    debug("\t".dbquery(@$db,"dbDATABASE_NAME")."{ ".dbquery(@$db,"dbTABLE_NAME")." }: --> OK") if $opt_d;

#}}}
}

sub savemail {
#{{{
    my $proc = $0;

    $proc =~ s/\W//g ;

    my $tmp = "/tmp/${proc}_maillog_$$";

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

    my $tmp = "/tmp/${proc}_maillog_$$";

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
            print { $to_parent } "[LOG: $msg]";
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
            print { $to_parent } "[DEBUG: $msg]";
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
    my $now = strtime(now());
    my $string;

    if ( $parent != $$ ) {
        if ( $to_parent ) {
            print { $to_parent } "[PROBLEM: (@ $now): $text]";
            return;
        }
    }

    $Problems++;

    $string = sprintf("%04s",$Problems);

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

rsync_baler [-h] [-v] [-d] [-f] [-x] [-R] [-j FILE] [-s sta_regex] [-r sta_regex] [-p pf] [-m email,email]

=head1 ARGUMENTS

Recognized flags:

=over 2

=item B<-h> 

Help. Produce this documentation

=item B<-v> 

Produce verbose output while running

=item B<-d>

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
