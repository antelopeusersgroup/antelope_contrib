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
use LWP;
use Fcntl;
use POSIX;
use Socket;
use archive;
use sysinfo;
use Net::Ping;
use Datascope;
use Pod::Usage;
use IO::Handle;
use File::Spec;
use File::Copy;
use Getopt::Std;
use File::Fetch;
use Digest::MD5 qw[md5_hex];
use IO::Uncompress::AnyUncompress qw(anyuncompress $AnyUncompressError) ;

our($opt_r,$opt_s,$opt_h,$opt_v,$opt_m,$opt_p,$opt_d);
our(%pf,@db,@db_sta,@db_ip,@db_on,$dbname,$dbpath);
our($dbout,$local_path,$start_of_report,@dbr,$nrecords);
our($station,@errors,%table,$time,$dfile,$bandwidth,$media);
our($parent,$reserve_media,$total_bytes,$bytes);
our($temp_sta,$ps_path);
our($start,$end,$run_time,$run_time_str,$type);
our($sta,@stas,$active_pids,$stations,$table,$folder);
our($pid,$log,$address,$ip_sta);
our($host,$key,$value,$file_fetch);
our($dlsta,$net,$ip);
our($problems_hash,$prob,$txt);
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

    unless ( &getopts('hdvm:p:s:r:') || @ARGV > 0 ) { 
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
        debug("Initialize mail") if $opt_v;
    }


    logging('');
    logging("$0 @ARGV");
    logging("Starting at ".strydtime(now())." on ".my_hostname());
    logging('');
    logging('');

    #
    # Implicit flag
    #
    $opt_v = $opt_d ? $opt_d : $opt_v ;
    $opt_p ||= "rsync_baler.pf" ;

    #
    # Get parameters from config file
    #
    debug("Getting params") if $opt_v;
    %pf = getparam($opt_p);

    #
    ## Set File::Fetch options
    #
    $File::Fetch::WARN    = 0 unless $opt_d; 
    $File::Fetch::DEBUG   = 1 if $opt_d; 
    $File::Fetch::TIMEOUT = $pf{download_timeout};
    $File::Fetch::BLACKLIST = [qw/LWP ncftp lftp lynx iosock/];
    #   File::Fetch
    #   Below is a mapping of what utilities will be used in what order for each schemes (if available):
    #       file    => LWP, lftp, file
    #       http    => LWP, wget, curl, lftp, lynx, iosock
    #       ftp     => LWP, Net::FTP, wget, curl, lftp, ncftp, ftp
    #       rsync   => rsync

    #
    # Verify Database
    #
    debug("Opening $pf{database}:") if $opt_v;

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


#}}}

#
#  Set mode [Retrieval or fixing dbs]
#
#{{{

    debug('Get list of stations:') if $opt_v;
    $stations = get_stations_from_db(); 

    #
    # Get data from the stations
    #
    run_in_threads($stations,"get_data");

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
    sendmail("$0 @ARGV",$opt_m,"/tmp/#rtsys$$") if $opt_m; 

    exit 0;

#}}}

sub get_stations_from_db {
#{{{
    my ($dlsta,$vnet,$net,$sta,$time,$endtime,$ip,$nrecords);
    my %sta_hash;
    my @db_1;

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

    for ( $db_1[3] = 0 ; $db_1[3] < $nrecords ; $db_1[3]++ ) { 

        ($dlsta,$net,$sta) = dbgetv(@db_1, qw/dlsta net sta/); 

        $sta_hash{$sta}{dlsta}      = $dlsta; 
        $sta_hash{$sta}{net}        = $net; 
        $sta_hash{$sta}{status}     = 'Decom'; 
        $sta_hash{$sta}{ip}         = 0; 

        $sta_hash{$sta}{status} = 'Active' if ( dbfind(@db_on, "sta =~ /$sta/ && snet =~ /$net/ && endtime == NULL", -1)>= 0);

        if ($sta_hash{$sta}{status} eq 'Active') {

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
        }

        logging("[$sta] [$net] [$dlsta] [$sta_hash{$sta}{status}] [$sta_hash{$sta}{ip}]") if $opt_v; 
    }

    eval { dbfree(@db_1); };
    eval { dbclose(@db_sta); };
    eval { dbclose(@db_ip);  };
    eval { dbclose(@db_on);  };

    return \%sta_hash;
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
        # Child only
        #

        #
        # Set this global for child only
        #
        $to_parent = $$station{to_parent}; 

        &$function($station,$stations->{$station});

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

            while (/\[LOG:(.*?)\]/g )     { logging($1,$station)}
            while (/\[DEBUG:(.*?)\]/g )   { debug($1,$station);}
            while (/\[PROBLEM:(.*?)\]/g ) { problem($1,$station);}

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
            debug("No child running. RESP = $?") if $opt_d;
        }
        elsif (WIFEXITED($?)) {
            debug("\tDone with $_") if $opt_d;
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
    log_die("No value for station in child.") unless $station;

    my (%active_media_files,$media,@rem_file);
    my (%remote,$size,$nrecords,@temp_download,@dbwr,@dbr,@dbr_sub);
    my (%clean,$local_path_file,%avoid,$replace,$file,$speed,$run_time);
    my ($d_data,$start_file,$where,$attempts,@missing);
    my ($p,$rem_s,$loc_s,@diff,$results,$run_time_str,$dbout);
    my ($end_file,$record,$time,$endtime,$dir,$dfile);
    my ($k,$m,$g,$total_size,%temp_hash,@total_downloads,@download);
    my ($digest,$hexd,$md5,$remote_file_content, $remote_file_handle);
    my ($status,$mode,$f,$http_folder,$md5_lib,@original_downloads);
    my (@db_r,%remove,%flagged,@db,@db_t);
    my ($media_active, $media_reserve,%downloaded);

    #
    # Prepare Variables and Folders
    #
    my $type    = '';
    my $resp    = 0; 
    my $ip     = $table->{ip};
    my $dlsta  = $table->{dlsta};
    my $net    = $table->{net};
    my @dates  = $table->{dates};
    my $path = prepare_path($station); 
    my $start_sta = now();

    #
    # Read local files and fix errors
    #
    my @local_files = read_local( $station,$net,$dlsta );

    #
    # Review all entries on the database
    #
    @db = open_db($station);
    $record  =  dbquery(@db, "dbRECORD_COUNT");
    LINE: for ( $db[3] = 0 ; $db[3] < $record ; $db[3]++ ) {
    #{{{
        
        check_time($start_sta);

        ($dfile,$status,$bytes,$dir,$md5,$attempts) = dbgetv(@db,qw/dfile status filebytes dir md5 attempts/);

        unless ( $dfile =~ /$station|EXMP/ ) {
            problem("Not relevant: $path/$dfile. Removing from database.") if $opt_v;
            $remove{$dfile} = ();
            next LINE;
        }

        if ( $dfile =~ /wfdisc/ ) {
            problem("Not relevant: $path/$dfile. Removing from database.") if $opt_v;
            $remove{$dfile} = ();
            next LINE;
        }

        if ( $status =~ /missing/ ) {
            problem("Flagged as $status: $path/$dfile. Removing from database.") if $opt_v;
            $remove{$dfile} = ();
            next LINE;
        }

        #
        # Set to download again
        #
        $flagged{$dfile} = $dir if $status =~ /flagged|error-download/;
        $avoid{$dfile} = () if $status =~ /flagged|error-download/ and $attempts >= 5;

        #
        # If we have the file then clean
        #
        next LINE if $status !~ /downloaded/;

        $downloaded{$dfile} = ();

        #
        # If missing checksum then connect to station
        #
        dbputv(@db, "md5", get_md5($station,$dfile,$ip) ) if $md5 =~ /^(-)$/ and $ip;

        #
        # Fix "downloaded" entries on the database
        #
        dbputv(@db, "dir", $path) unless -f "$dir/$dfile";

        unless ( -f "$path/$dfile" ) {

            problem("Missing: $path/$dfile. Removing from database.") if $opt_v;
            $remove{$dfile} = ();
            next LINE;

        }

        #
        # Verify mode 0664
        #
        $mode = (stat("$path/$dfile"))[2];   
        $mode = sprintf("0%o", $mode & 07777);
        unless ( $mode eq "0664" ) {

            problem("Fix $path/$dfile mode $mode => 0664");
            chmod 0664, "$path/$dfile";

        }

        DBputv(@db, "filebytes", -s "$path/$dfile") unless -s "$path/$dfile" == $bytes;
    #}}}
    }
    dbclose(@db);

    #
    # Cleanup
    #
    remove_file_from_db($station,$_) foreach keys %remove;
    foreach (keys %downloaded ){
        clean_db($station,$_) if defined $flagged{$_};
    }

    #
    # Files that we won't download
    #
    delete $flagged{$_} foreach keys %downloaded;
    delete $flagged{$_} foreach keys %avoid;

    #
    # For DECOM stations stop here
    #
    return unless $table->{status} eq 'Active';

    log_die("No IP") unless $ip;

    #
    # Limit the downloads to lest than 3 Gbs in last 21 days
    #
    $d_data = total_data_downloaded($station,21) || 0.0;
    log_die("Downloaded ( $d_data ) Mbts in the last 21 days!") if $d_data > 3000;

    #
    # Ping the station
    #
    $p = Net::Ping->new("tcp", 10);
    $p->port_number($pf{http_port});
    log_die("Not Responding!!!!") unless $p->ping($ip);
    undef($p);

    #
    # Get medias ID's
    #
    ($media_active, $media_reserve) = get_medias($station,$ip);
    $media_active ||= 'unknown';
    $media_reserve ||= 'unknown';

    %remote = read_baler( $station, $ip, $media_active, $media_reserve);
    check_time($start_sta);

    # 
    # Compare local to remote
    #
    @db = open_db($station);
    foreach $f ( sort keys %remote ) {
    #{{{
        next if $f =~ /\.md5/;

        #
        # Check if we have the file
        #
        $record = dbfind(@db, "dfile =~ /$f/ && status =~ /downloaded/", -1);
        if ( $record >= 0 ) {
            next if $media_reserve eq 'unknown' or $media_active eq 'unknown';
            $db[3] = $record;
            $media = dbgetv(@db,'media');
            dbputv(@db,"media", $remote{$f} =~ /WDIR2/ ? $media_reserve : $media_active ) if $media =~ /-|unknown|activemedia|reservemedia/;
            next;
        }

        @db_t= dbsubset(@db, "dfile =~ /$f/ && status == 'flagged'");
        $record  =  dbquery(@db_t, "dbRECORD_COUNT");
        dbfree(@db_t);
        debug("dbaddv: $f 'flagged'") if $opt_v;

        #
        # Clean the value of dir
        #

        dbaddv(@db, 
            "net",      $net,
            "sta",      $station,
            "dlsta",    $dlsta,
            "dfile",    $f,
            "dir",      $remote{$f},
            "media",    ($remote{$f} =~ /WDIR2/) ? $media_reserve : $media_active,
            "attempts", $record + 1,
            "time",     now(), 
            "lddate",   now(), 
            "status",   "flagged");

        #
        # Add the files to the list we want to downlaod
        #
        $flagged{$f} = $remote{$f};

    #}}}
    } #end of foreach $rt
    dbclose(@db);

    log_die('No new files.') unless keys %flagged;

    logging('Files flagged:' . join(' ' ,sort keys %flagged)) if $opt_v;

    # 
    # Download the missing files
    #
    FILE: foreach $file ( sort keys %flagged ) {
    #{{{
        logging("Now download file $file") if $opt_v;
        problem("ERROR on value of FLAGGED file:$file") unless $file;
        next unless $file;
        #{{{ Download the file

        $start_file = now();
        $where = '';

        check_time($start_sta);

        #
        # Verify if we have the file in the local dir
        #
        logging("Check if $path/$file exists") if $opt_v;
        problem("Cannot re-download file:$path/$file") if -f "$path/$file";
        next if -f "$path/$file";

        #
        # Update DB
        #
        logging("Open db and add $file as downloading") if $opt_v;
        @dbr = open_db($station);
        @dbr_sub = dbsubset(@dbr, "dlsta == '$dlsta' && dfile == '$file' && status == 'downloading' " );
        $attempts = dbquery(@dbr_sub,dbRECORD_COUNT) ; 
        $attempts += 1;

        dbaddv(@dbr, 
            "net",      $net,
            "dlsta",    $dlsta,
            "dfile",    $file,
            "sta",      $station,
            "time",     now(), 
            "status",   "downloading",
            "dir",      $path,
            "attempts", $attempts,
            "lddate",   now() );

        dbclose(@dbr);

        if ( $flagged{$file} =~ /-/ ) {
            foreach (qw/WDIR WDIR2/) {
                $flagged{$file} = "$_/data/";
                logging("download_file($flagged{$file}$file,$path,$ip)") if $opt_v;
                $where = download_file("$flagged{$file}$file",$path,$ip);
                last if $where;
            }
        } 
        else {
            logging("download_file($flagged{$file}$file,$path,$ip)") if $opt_v;
            $where = download_file("$flagged{$file}$file",$path,$ip);
        }

        $end_file = now();
        $run_time = $end_file-$start_file;
        $run_time_str = strtdelta($run_time);
        #}}}

        if( $where ) { 
        #{{{ if download is SUCCESS
            logging("$file where()=> $where") if $opt_v;
            #
            # Keep track of total data downloaded
            #
            $size = -s $where; 
            $total_size += $size;

            debug("Success in download of $file after $run_time_str") if $opt_v;

            push @total_downloads, $file;

            #
            # Verify bandwidth of connection
            #
            $speed = ((-s $where) / 1024 ) / $run_time;
            debug("$file $size Kb  $run_time secs $speed Kb/sec") if $opt_v;

            $md5 = get_md5($station,$file,$ip) || 'missing';

            debug("Reported MD5 for $file : $md5") if $opt_v;

            $speed ||= 0.00;
            $size = '0' unless $size;

            #
            # Add to DB
            #
            @dbr = open_db($station);
            if ( $md5 =~ /error-download/ ) {
                debug("dbaddv: $dlsta | $start_file | $end_file | $attempts | 'error-download' | $size | $speed | $md5") if $opt_v;
                dbaddv(@dbr, 
                    "net",      $net, 
                    "sta",      $station, 
                    "time",     $start_file, 
                    "endtime",  $end_file, 
                    "dir",      $path, 
                    "attempts", $attempts, 
                    "filebytes",$size, 
                    "media",    $flagged{$file} =~ /WDIR2/ ? $media_reserve : $media_active,
                    "bandwidth",$speed, 
                    "dlsta",    $dlsta,
                    "fixed",    'n', 
                    "md5",      $md5,
                    "dfile",    $file,
                    "lddate",   now(),
                    "status",   "error-download");
            } 
            else {
                debug("dbaddv: $dlsta | $start_file | $end_file | $attempts | 'downloaded' | $size | $speed | $md5") if $opt_v;
                dbaddv(@dbr, 
                    "net",      $net, 
                    "sta",      $station, 
                    "time",     $start_file, 
                    "endtime",  $end_file, 
                    "dir",      $path, 
                    "attempts", $attempts, 
                    "filebytes",$size, 
                    "media",    $flagged{$file} =~ /WDIR2/ ? $media_reserve : $media_active,
                    "bandwidth",$speed, 
                    "dlsta",    $dlsta,
                    "fixed",    'n', 
                    "md5",      $md5,
                    "dfile",    $file,
                    "lddate",   now(),
                    "status",   "downloaded");
            }
            dbclose(@dbr);
        
        #}}} 
        }
        else {
        #{{{ if download FAILS
            #
            # If download failed... $where == NULL
            #
            $run_time_str = strtdelta(now()-$start_file);
            problem("Failed download of $file after $run_time_str.");
            $size = -s "$path/$file";
            $size ||= 0;

            @dbr = open_db($station);
            dbaddv(@dbr, 
                "net",      $net, 
                "sta",      $station, 
                "time",     $start_file, 
                "endtime",  $end_file, 
                "dir",      $path, 
                "attempts", $attempts, 
                "filebytes",$size, 
                "dlsta",    $dlsta,
                "dfile",    $file,
                "lddate",   now(),
                "status",   "error-download");
            dbclose(@dbr);

            unlink "$path/$file" if -e "$path/$file";
        #}}}
        }
        logging("Next file") if $opt_v;
    #}}}
    }

    log_die( "NO DOWNLOADS!!!! Station not downloading any files.") unless scalar @total_downloads;

    delete $flagged{$_} foreach @total_downloads;

    problem('Missing:' . join(' ' ,sort keys %flagged));

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

sub check_time {
#{{{
    $start = shift;

    #
    # Check if we are over the time limit
    #
    if ( $pf{max_child_run_time} ) { 
        if ( int($pf{max_child_run_time}) < (now() - $start) ) {
            log_die("Rsync exceeds allowed time set in max_child_run_time ($pf{max_child_run_time}).");
        }
    }
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

    @db_subset = dbsubset ( @db, "status =~ /downloaded|error-download/");
    $nrecords = dbquery(@db_subset, 'dbRECORD_COUNT') ;
    return unless $nrecords;


    $start_of_report = str2epoch("-${days}days");
    @db_temp= dbsubset ( @db_subset, "time >= $start_of_report");
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

sub download_file {
#{{{
    my $file = shift;
    my $path = shift;
    my $ip = shift;
    my ($file_fetch,$where);
    my @temp_new;


    foreach ( split(/[\s|\/]/,$file) ) {
        next if /^\/$/;
        push(@temp_new,$_) if /^\S+$/;
    }

    $file = join('/',@temp_new);

    debug("DOWNLOAD: http://$ip:$pf{http_port}/$file") if $opt_v;

    eval{ $file_fetch = File::Fetch->new(uri => "http://$ip:$pf{http_port}/$file"); };
    problem("File::Fetch -> $@") if $@; 

    problem("ERROR in build of File::Fetch -> http://$ip:$pf{http_port}/$file") unless $file_fetch; 
    return unless $file_fetch;

    debug("Download: ".$file_fetch->uri) if $opt_v;

    eval {  $where = $file_fetch->fetch( to => "$path/" ); };
    problem("File::Fetch ".$file_fetch->uri." $@") if $@; 

    problem("Cannot download http://$ip:$pf{http_port}/$file") unless $where;
    return $where;
#}}}
}

sub open_db {
#{{{
    my $sta = shift;
    my ($path,$dbout);

    #
    # Prepare Folder Name
    #
    $path = prepare_path($sta);

    #
    # Fix path
    #
    $dbout = File::Spec->rel2abs( "${path}/${sta}_baler" ); 

    debug("Opening database ($dbout).") if $opt_v;

    #
    # Create descriptor file if missing
    #
    unless ( -e $dbout) {
        debug("$sta Creating new database ($dbout).") if $opt_v;

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
    $dbout .= ".rsyncbaler";
    debug("$sta Openning database table  ($dbout)") if $opt_v;
    return  dbopen_table($dbout,"r+") or log_die("Can't open DB: $dbout",$sta);

#}}}
}

sub read_local {
#{{{
    my $sta = shift;
    my $net = shift;
    my $dlsta = shift;
    my %list;
    my ($record,@db_r,@db,@files);
    my ($extra,$size,$path,$f);

    debug("Reading local directory") if $opt_v;

    $path = prepare_path($sta);

    opendir(DIR,$path) or log_die("Failed to open $path: $!");

    while($f = readdir DIR) {
        next if -d "$path/$f";
        $list{$f} = () if $f =~ /(${sta}_4-\d*|EXMP_4-\d*)/;
    }

    close(DIR);

    @files = sort keys %list;

    @db = open_db($sta);

    foreach $f ( @files ) { 
        @db_r = dbsubset(@db, "dfile =='$f' && status == 'downloaded'");
        $record = dbquery(@db_r , dbRECORD_COUNT);
        dbfree(@db_r);

        next if $record == 1; 

        if ( $record > 1 ) {
            $extra = 0;
            while ( 1 ) {
                $extra++;
                $db[3] = dbfind(@db, "dfile == '$f' && status == 'downloaded'", -1);
                last unless $db[3] >= 0;
                dbputv(@db,'status','extra','lddate', now(),'attempts',$extra );
            }

        }

        #
        # Add the missing file
        #
        $size = -s "$path/$f" || 0; 
        logging("file $f not in database. Adding as 'downloaded'");
        dbaddv(@db, 
            "net",      $net,
            "sta",      $sta,
            "dlsta",    $dlsta,
            "dir",      $path,
            "dfile",    $f,
            "filebytes",$size, 
            "attempts", 1,
            "time",     now(), 
            "fixed",    "n",
            "lddate",   now(), 
            "status",   "downloaded");
    }

    dbclose(@db);

    debug("LOCAL FILES: " . @files ) if $opt_d;

    return @files;
#}}}
}

sub read_baler {
#{{{
    my $sta   = shift;
    my $ip    = shift;
    my $media_active    = shift;
    my $media_reserve    = shift;
    my ($dir,$path,$name,$test,$nrecords);
    my %list;
    my ($input,$where,$files);
    my @temp_dir = ();
    my (@db,@n,@queries);
    my $attempt = 1;

    $path = prepare_path($sta); 

    $path .= '/lists/';

    mkdir $path unless -d $path;

    log_die("Cannot make directory ($path)") unless -d $path;

    chdir $path or log_die("Cannot change to directory ($path)");


    #
    # For each of the folders
    #
    foreach $dir ( qw/active reserve/ ) {

        $path = "list.$dir.data.gz";

        debug("Get: $path") if $opt_v;

        unlink $path if -e $path;
        #
        # download list of files now
        #
        eval{ $file_fetch = File::Fetch->new(uri => "http://$ip:$pf{http_port}/$path"); };
        problem("File::Fetch($path) => $@") if $@ and $opt_v; 

        eval {  $where = $file_fetch->fetch( to => "./" ); };
        problem("File::Fetch(".$file_fetch->uri.") => $@") if $@ and $opt_v; 


        problem("Error downloading list of files http://$ip:$pf{http_port}/$path") unless $where;
        next unless $where;

        problem("ERROR! on $path") unless -e $path;
        open $input, "<$path"; 
        $files = new IO::Uncompress::AnyUncompress $input or problem("IO::Uncompress::AnyUncompress failed: $AnyUncompressError");

        while ( <$files> ) {
            #
            # Parse results and get size
            #
            s/\n//;
            next unless /($sta|EXMP)/;
            next if  /\.md5/;
            @temp_dir = split(/\//,$_);
            $name = pop(@temp_dir);
            push(@temp_dir," ");
            unshift(@temp_dir, $dir =~ /active/ ? 'WDIR' : 'WDIR2' );
            unshift(@temp_dir," ");

            $list{$name} = join('/',@temp_dir);
            debug("$name => $list{$name}") if $opt_d;
        }

        $input->close();
        $files->close();
    }

    log_die("Can't get any lists of files: $ip:$pf{http_port})") unless keys %list;

    #
    # Fix the media id on the database
    #
    @db = open_db($sta);
    $nrecords = dbquery(@db,dbRECORD_COUNT);

    debug("Fix media id on database.") if $opt_v;

    for ( $db[3] = 0 ; $db[3] < $nrecords ; $db[3]++ ) { 
        $name = dbgetv(@db, 'dfile'); 
        next unless $list{$name}; 
        #debug("Fix media id for $name $list{$name} $media_active $media_reserve") if $opt_v;
        if ( $list{$name} =~ /.*WDIR2.*/ ) {
            dbputv(@db,'media', $media_reserve );
            #debug( "$name media_reserve" );
        } 
        else {
            dbputv(@db,'media', $media_active );
            #debug( "$name media_active" );
        }
    }

    dbclose(@db);

    return %list;

#}}}
}

sub get_md5 {
#{{{
    my $sta  = shift;
    my $file  = shift;
    my $ip    = shift;
    my ($md5_lib,$digest,$md5,$local_path,$folder);
    my @md5_raw;

    debug("Get MD5 $file") if $opt_v;

    $local_path = prepare_path($sta) . '/md5/'; 

    $file .= '.md5' unless $file =~ /\.md5/;

    mkdir $local_path unless -d $local_path;

    log_die("Cannot make directory ($local_path)") unless -d $local_path;

    chdir $local_path or log_die("Cannot change to directory ($local_path)");

    unless ( -s "$local_path/$file" ) {

        unlink "$local_path/$file";
        debug("Lets download MD5 file $file -> $local_path") if $opt_v;

        foreach $folder (qw/WDIR WDIR2/) {
            last if download_file("$folder/recover/$file",$local_path,$ip);
        }

    }

    problem("Missing $file")  if $opt_v and  not -e "$local_path/$file";
    return 'missing' unless -e "$local_path/$file";

    open(DAT, '<', "$local_path/$file") or log_die("Cannot open $local_path/$file!");

    while (<DAT>) {
        chomp;
        debug("MD5:: $_") if $opt_v;
        push @md5_raw, split;
    }

    close(DAT);

    $md5 = $md5_raw[0];

    problem("Error in file $local_path/$file")  unless $md5;
    unlink "$local_path/$file" unless $md5;

    return 'missing' unless $md5;

    #
    # Open file and get local md5
    #
    $file  =~ s/\.md5//g;
    $local_path = prepare_path($sta); 
    open(FILE,"$local_path/$file") or log_die("Cannot open $local_path/$file for md5 calc.");
    $md5_lib = Digest::MD5->new;
    $md5_lib->addfile(FILE);
    $digest = $md5_lib->hexdigest || 0;
    close FILE;

    problem("Cannot produce MD5 for ($file)") unless $digest;

    return $md5 if ( $digest eq $md5 );
    
    problem("$file MD5 problem:: reported:$md5 calc:$digest " );

    return 'error-download';

#}}}
}

sub get_medias {
#{{{
    my $sta = shift;
    my $ip = shift;
    my (@text,$line,$browser, $resp);
    my ($active,$reserve);

    $browser = LWP::UserAgent->new;

    $resp = $browser->timeout(120);

    logging("$sta:\tLWP::UserAgent->get(http://$ip:$pf{http_port}/stats.html)") if $opt_v;
    $resp = $browser->get("http://$ip:$pf{http_port}/stats.html");

    unless ( $resp->is_success ) {
        logging("$sta:\tLWP::UserAgent->get(http://$ip:$pf{http_port}/stats.html)") if $opt_v;
        $resp = $browser->get("http://$ip:$pf{http_port}/stats.html");

        problem("Missing http://$ip:$pf{http_port}/stats.html") unless $resp;
        return unless $resp; 
    }

    if ( $resp->is_success ) {
        @text =  split '\n', $resp->content;

        for ($line=0; $line < scalar @text; $line++){
            $text[$line] =~ m/MEDIA site \d crc=(\S+) IN USE/;
            $active = $1; 
            last if $active;
        }

        for ($line=0; $line < scalar @text; $line++){
            $text[$line] =~ m/MEDIA site \d crc=(\S+) RESERVE/;
            $reserve = $1; 
            last if $reserve;
        }
    }
    else {
        problem("problem reading http://$ip:$pf{http_port}/stats.html");
        return;
    }

    problem("Cannot find MEDIA site 1 in http://$ip:$pf{http_port}/stats.html") unless $active;
    problem("Cannot find MEDIA site 2 in http://$ip:$pf{http_port}/stats.html") unless $reserve;

    logging("get(http://$ip:$pf{http_port}/stats.html) => ($active,$reserve)") if $opt_v;
    return ($active,$reserve);

#}}}
}

sub remove_file {
#{{{
    my $sta  = shift;
    my $file = shift;
    my @db; 

    return unless $file;

    my $path = prepare_path($sta);

    problem("Removing $path/$file");

    mkdir "$path/trash" unless -d "$path/trash";

    log_die("Cannot make directory ($path/trash/)") unless -d "$path/trash";

    #
    # Verify file in folder
    #
    if ($file and -f "$path/$file") {
        debug("move $path/$file to $path/trash/$file") if $opt_v;
        move("$path/$file","$path/trash/$file") or problem("Can't move $file to $path/trash");
    }

    @db = open_db($sta);

    while ( 1 ) { 
        $db[3] = dbfind(@db, "dfile =~ /$file/ && status =~ /downloaded/", -1); 
        last unless $db[3] >= 0;
        dbputv(@db,'status','error-download','lddate', now() );
    }

    dbclose(@db);

    return;
#}}}
}

sub remove_file_from_db {
#{{{
    my $sta      = shift;
    my $file     = shift;
    my (@db,$records,$f); 

    logging("Clean DB of dfile =~/$file/") if $opt_v;

    remove_file($sta,$file);

    @db = open_db($sta);
    $records = dbquery(@db,dbRECORD_COUNT);

    for ($db[3]=0; $db[3] < $records; $db[3]++) {
        dbmark(@db) if dbgetv(@db, 'dfile') =~ /$file/; 
    }

    dbcrunch(@db);
    dbclose(@db);

    return;
#}}}
}

sub clean_db {
#{{{
    my $sta      = shift;
    my $file     = shift;
    my (@db,$records,$f,$s); 

    logging("Clean dfile =~/$file/ ") if $opt_v;

    @db = open_db($sta);

    $records = dbquery(@db,dbRECORD_COUNT);

    for ($db[3]=0; $db[3] < $records; $db[3]++) {
        ($f,$s) = dbgetv(@db, qw/dfile status/); 
        dbmark(@db) if ($f =~ /$file/ && $s =~ /error-download|extra|downloading|flagged/); 
    }

    dbcrunch(@db);
    dbclose(@db);

    return;
#}}}
}

sub prepare_path {
#{{{
    my $station  = shift;
    my $path = '';

    log_die("prepare_path(). Cannot produce path! We need a station name...") unless $station;

    $path = File::Spec->rel2abs( "$pf{local_data_dir}/$station" ); 

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
                database http_port max_procs/){
        $pf{$_} = pfget($PF,$_);

        log_die("Missing value for $_ in PF:$PF") unless defined($pf{$_});

        debug( sprintf("\t%-22s -> %s", ($_,$pf{$_})) ) if $opt_v;
    }

    return (%pf);
#}}}
}

sub table_check {
#{{{
    my $db = shift;
    my $sta = shift;

    $sta ||= '';

    debug("Verify Database: ".dbquery(@$db,"dbDATABASE_NAME") ) if $opt_v;

    log_die( dbquery(@$db,"dbTABLE_NAME")." not available.",$sta) unless dbquery(@$db,"dbTABLE_PRESENT");

    debug("\t".dbquery(@$db,"dbDATABASE_NAME")."{ ".dbquery(@$db,"dbTABLE_NAME")." }: --> OK") if $opt_v;

#}}}
}

sub log_die {
#{{{
    my $msg = shift;

    problem($msg);

    exit if $parent != $$;

    sendmail("ERROR: $0 @ARGV DIED ON $host",$opt_m,"/tmp/#rtsys$$") if $opt_m; 

    elog_die($msg);

#}}}
}

sub logging {
#{{{
    my $msg = shift;
    my $sta = shift || '';
    my $string = 0;
    my $now = strtime(now());

    if ( $parent != $$ ) {
        if ( $to_parent ) {
            print { $to_parent } "[LOG:($now): $msg]";
            return;
        }
    }

    if ( $sta ) {
        $string = keys %{$problems_hash->{'log'}->{$sta}};
        $problems_hash->{'log'}->{$sta}->{$string+1} = $msg;
        return;
    }

    elog_notify($msg);

#}}}
}

sub debug {
#{{{
    my $msg = shift;
    my $sta = shift;
    my $string = 0;
    my $now = strtime(now());

    if ( $parent != $$ ) {
        if ( $to_parent ) {
            print { $to_parent } "[DEBUG:($now): $msg]";
            return;
        }
    }

    if ( $sta ) {
        $string = keys %{$problems_hash->{'log'}->{$sta}};
        $problems_hash->{'log'}->{$sta}->{$string+1} = $msg;
        return;
    }

    elog_debug($msg);

#}}}
}

sub problem { 
#{{{
    my $text = shift; 
    my $sta = shift;
    my $now = strtime(now());
    my $string = 0;

    if ( $parent != $$ ) {
        if ( $to_parent ) {
            print { $to_parent } "[PROBLEM:($now): $text]";
            return;
        }
    }

    if ( $sta ) {
        $string = keys %{$problems_hash->{'problems'}->{$sta}};
        $problems_hash->{'problems'}->{$sta}->{$string+1} = $text;
        elog_complain("$sta: $text") if $opt_v;
    }else {
        elog_complain("\n\n\t* \n\t* Problem #$string: \n\t* $sta: $text\n\t* \n") if $opt_v;
    }

#}}}
}

sub problem_print {
#{{{

    my $s_v; 
    my $p_v; 

    #return unless $Problems;

    elog_complain('');
    elog_complain('');
    elog_complain("-------- Problems: --------");
    elog_complain('');

    for  $s_v ( sort keys %{$problems_hash->{'problems'}} ) {
        elog_complain("\tOn station $s_v:");
        for $p_v ( sort{ $a <=> $b } keys %{$problems_hash->{'problems'}->{$s_v}} ) {
            elog_complain("\t\t $p_v) $problems_hash->{'problems'}->{$s_v}->{$p_v}");
        }
        elog_complain('');
    }

    elog_complain("-------- End of problems: --------");
    for  $s_v ( sort keys %{$problems_hash->{'log'}} ) {
        elog_notify("\tOn station $s_v:");
        for $p_v ( sort{ $a <=> $b } keys %{$problems_hash->{'log'}->{$s_v}} ) {
            elog_notify("\t\t $p_v) $problems_hash->{'log'}->{$s_v}->{$p_v}");
        }
        elog_notify('');
    }
    elog_notify('');

#}}}
}
__END__
#{{{
=pod

=head1 NAME

rsync_baler - Sync a remote baler directory to a local copy

=head1 SYNOPSIS

rsync_baler [-h] [-v] [-d] [-s sta_regex] [-r sta_regex] [-p pf] [-m email,email]

=head1 ARGUMENTS

Recognized flags:

=over 2

=item B<-h> 

Help. Produce this documentation

=item B<-v> 

Produce verbose output while running

=item B<-d>

Produce very-verbose output (debuggin)

=item B<-p file>

Parameter file name to use.

=item B<-s regex>

Select station regex. ('STA1|STA2' or 'A...|B.*')

=item B<-r regex>

Reject station regex. ('STA1|STA2' or 'A...|B.*')

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
