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
use File::Basename;
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
        debug("Initialize mail");
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
    debug("Getting params");
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
    debug("Opening $pf{database}:");

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
#{{{ get_stations_from_db()
    my ($dlsta,$vnet,$net,$sta,$time,$endtime,$ip,$nrecords);
    my %sta_hash;
    my @db_1;

    debug('Get list of stations:');

    #
    # Get stations with baler44s
    #
    debug("dbsubset ( stablaler.model =~ /Packet Baler44/)");
    @db_1 = dbsubset ( @db_sta, "stabaler.model =~ /PacketBaler44/ ");

    debug("dbsubset ( sta =~ /$opt_s/)") if $opt_s;
    @db_1 = dbsubset ( @db_1, "sta =~ /$opt_s/") if $opt_s;

    debug("dbsubset ( sta !~ /$opt_r/)") if $opt_r;
    @db_1 = dbsubset ( @db_1, "sta !~ /$opt_r/") if $opt_r;

    $nrecords = dbquery(@db_1,dbRECORD_COUNT) or log_die("No records to work with after dbsubset()"); 

    for ( $db_1[3] = 0 ; $db_1[3] < $nrecords ; $db_1[3]++ ) { 

        ($dlsta,$net,$sta) = dbgetv(@db_1, qw/dlsta net sta/); 

        $sta_hash{$sta}{dlsta}      = $dlsta; 
        $sta_hash{$sta}{net}        = $net; 
        $sta_hash{$sta}{status}     = 'Decom'; 
        $sta_hash{$sta}{ip}         = 0; 

        $sta_hash{$sta}{status} = 'Active' if ( dbfind(@db_on, "sta =~ /$sta/ && snet =~ /$net/ && endtime == NULL", -1) >= 0);

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

        logging("[$sta] [$net] [$dlsta] [$sta_hash{$sta}{status}] [$sta_hash{$sta}{ip}]");
    }

    eval { dbfree(@db_1); };
    eval { dbclose(@db_sta); };
    eval { dbclose(@db_ip);  };
    eval { dbclose(@db_on);  };

    return \%sta_hash;
#}}}
}

sub run_in_threads {
#{{{ run_in_threads()
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
        # throttle parent process
        #
        sleep 0.5;

        #
        # Stop if we are at max procs
        #
        redo STATION if scalar(@active_procs) >= $max_out;

        logging("Spawn: $function($station). Now: ".@active_procs." procs");

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
#{{{ nonblock_read(sta)
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

sub check_pids {
#{{{ check_pids(@pids)
    my @temp_pids = ();

    foreach (@_) {

        if (waitpid($_,WNOHANG) == -1) {
            debug("No child running. RESP = $?");
        }
        elsif (WIFEXITED($?)) {
            debug("\tDone with $_");
        }
        else{ 
            push @temp_pids, $_;
        }

    }

    return @temp_pids;

#}}}
}

sub get_data {
#{{{  get_data(sta,%metadata)

    my ($sta,$table) = @_; 
    log_die("No value for station in child.") unless $sta;

    my (%active_media_files,$media,@rem_file);
    my (%remote,$size,$nrecords,@temp_download,@dbwr,@dbr,@dbr_sub);
    my (%clean,$local_path_file,%avoid,$replace,$file,$speed,$run_time);
    my ($d_data,$start_file,$where,$attempts,@missing);
    my ($p,$rem_s,$loc_s,@diff,$results,$run_time_str,$dbout);
    my ($msdtime,$end_file,$record,$time,$endtime,$dir,$dfile);
    my ($k,$m,$g,$total_size,%temp_hash,@total_downloads,@download);
    my ($digest,$hexd,$md5,$remote_file_content, $remote_file_handle);
    my ($status,$mode,$f,$http_folder,$md5_lib,@original_downloads);
    my (@data,@db_r,%remove,%flagged,@db,@db_t);
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
    my $path = prepare_path($sta); 
    my $start_sta = now();

    #
    # Set umask for process
    #
    umask 002;

    #
    # Read local files and fix errors
    #
    fix_local( $sta,$net,$dlsta );

    #
    # Review all entries on the database
    #
    @db = open_db($sta);
    $record  =  dbquery(@db, dbRECORD_COUNT);
    LINE: for ( $db[3] = 0 ; $db[3] < $record ; $db[3]++ ) {
    #{{{

        check_time($start_sta);

        ($dfile,$status,$bytes,$dir,$md5,$attempts,$msdtime,$time) = dbgetv(@db,qw/dfile status filebytes dir md5 attempts msdtime time/);

        debug("Check db: $dfile,$status,$bytes,$dir,$md5,$attempts,$msdtime,$time");

        problem("WRONG NAME OF FILE: $dfile" ) if $dfile !~ /^(..-(${sta}|EXMP)_4-\d{14})$/;
        next if $dfile !~ /^(..-(${sta}|EXMP)_4-\d{14})$/;

        if ($status =~ /skip/ ) {
            #
            # Avoid file
            #
            next LINE;
        }

        next LINE if $msdtime > 1; # file already processed

        if ($status !~ /downloaded/ ) {
            #
            # Set to download again
            #
            $flagged{$dfile} = $dir if $attempts < 5;
            next LINE;
        }

        #
        # Fix "downloaded" entries on the database
        #
        dbputv(@db, "dir", $path) unless -f "$dir/$dfile";

        #
        # Verify mode 0664
        #
        $mode = (stat("$path/$dfile"))[2] || 0;
        $mode = sprintf("0%o", $mode & 07777);
        problem("$dfile mode $mode => 0660") unless int($mode) >= 660;

        #
        # Fix size of file in database
        #
        dbputv(@db, "filebytes", -s "$path/$dfile") unless (-s "$path/$dfile" == int($bytes));

        next LINE if $attempts > 3;

        next LINE if $md5 =~ /missing/;

        #
        # If missing checksum then connect to station
        #
        dbputv(@db, "md5", get_md5($sta,$dfile,$ip) ) if $md5 =~ /-|error/ and $ip;
        $md5 = dbgetv(@db,'md5');
        dbputv(@db,'attempts',int(dbgetv(@db,'attempts'))+1) if $md5 =~ /error/;

        $flagged{$dfile} = $dir if $md5 =~ /error/;


    #}}}
    }
    dbclose(@db);

    logging("Flagged from database: $_") foreach ( sort keys %flagged );


    #
    # For DECOM stations stop here
    #
    return unless $table->{status} eq 'Active';

    log_die("No IP") unless $ip;

    #
    # Limit the downloads to lest than 3 Gbs in last 21 days
    #
    $d_data = total_data_downloaded($sta,21) || 0.0;
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
    ($media_active, $media_reserve) = get_medias($sta,$ip);
    $media_active ||= 'unknown';
    $media_reserve ||= 'unknown';

    %remote = read_baler( $sta, $ip, $media_active, $media_reserve);
    check_time($start_sta);

    #
    # Compare local to remote
    #
    foreach $f ( sort keys %remote ) {
    #{{{
        next if $f =~ /\.md5/;

        #
        # Check if we have the file
        #
        @db = open_db($sta);
        $db[3] = dbfind(@db, "dfile =~ /$f/", -1);

        if ($db[3] >= 0 ){

            next if dbgetv(@db,'status') =~ /downloaded/;

            next if dbgetv(@db,'attempts') > 5;
            if ( dbgetv(@db,'status') =~ /flagged/ ) {
                logging("Already in db, increase attemtps: $f");
                dbputv(@db, 
                    "net",      $net,
                    "sta",      $sta,
                    "dlsta",    $dlsta,
                    "dfile",    $f,
                    "dir",      $remote{$f},
                    "media",    ($remote{$f} =~ /WDIR2/) ? $media_reserve : $media_active,
                    "attempts", int( dbgetv(@db,'attempts') )+1,
                    "time",     now(), 
                    "lddate",   now(), 
                    "status",   "flagged");
            }

        } else { 

            logging("Add to database from Baler list: $f");
            dbaddv(@db, 
                "net",      $net,
                "sta",      $sta,
                "dlsta",    $dlsta,
                "dfile",    $f,
                "dir",      $remote{$f},
                "media",    ($remote{$f} =~ /WDIR2/) ? $media_reserve : $media_active,
                "attempts", 1,
                "time",     now(), 
                "lddate",   now(), 
                "status",   "flagged");

        }

        #
        # Add the files to the list we want to downlaod
        #
        $flagged{$f} = $remote{$f};

    #}}}
    } 
    dbclose(@db);

    log_die('No new files.') unless keys %flagged;

    logging('Files flagged: ' . join(' ' ,sort keys %flagged));

    #
    # Download the missing files
    #
    FILE: foreach $file ( sort keys %flagged ) {
    #{{{

        logging("Now try to download: $file");

        if ( $file !~ /^(..-(${sta}|EXMP)_4-\d{14})$/) {
            problem("ERROR on value of FLAGGED file:$file");
            next;
        }

        #{{{ Download the file

        $start_file = now();
        $where = '';

        check_time($start_sta);

        #
        # Verify if we have the file in the local dir
        #
        #logging("Check if $path/$file exists");
        #problem("Cannot re-download file:$path/$file") if -f "$path/$file";
        #next if -f "$path/$file";
        if (-f "$path/$file") {
            problem("move $path/$file to $path/trash/$file");
            move("$path/$file","$path/trash/$file") or problem("Can't move $file to $path/trash");
        }

        #
        # Download file
        #
        foreach (qw/WDIR WDIR2/) {
            logging("download_file($_/data/$file,$path,$ip)");
            $where = download_file("$_/data/$file",$path,$ip);
            last if $where;
        }

        $end_file = now();
        $run_time = $end_file-$start_file;
        $run_time_str = strtdelta($run_time);
        #}}}

        #
        # Resurrect the file if we have previous copy
        #
        if (! -f "$path/$file" && -f "$path/trash/$file") {
            problem("Resurrect file: $path/trash/$file to $path/$file");
            move("$path/trash/$file","$path/$file") or problem("Can't move $file to $path");
            $status = 'downloaded';
            $md5 = get_md5($sta,$file,$ip) || 'error';
        } elsif (-f "$path/$file") {
            $status = 'downloaded';
            $status = 'error' unless $where;
            $md5 = get_md5($sta,$file,$ip) || 'error';
            problem("$file => status:$status, md5:$md5") if $md5 =~ /error/;
            logging("Success in download of $file after $run_time_str") if $md5 !~ /error/;
            push @total_downloads, $file;
        } else {
            problem("$file => Not in local directory");
            $md5 = 'error';
            $status = 'error';
        }

        #
        # Verify bandwidth of connection
        #
        $size = -s "$path/$file";
        $size ||= 0;
        $speed = ($size / 1024 ) / $run_time;
        $speed ||= 0.00;

        #
        # Keep track of total data downloaded
        #
        $total_size += $size;

        #
        # Add to DB
        #
        @db = open_db($sta);
        $db[3] = dbfind(@db, "dfile =~ /$file/", -1);

        if ($db[3] >= 0) {
            $attempts = int( dbgetv(@db,'attempts') )+1;
            $attempts = 1 if dbgetv(@db,'status') !~ $status;
        } else {
            $attempts = 1;
        }

        debug("$file | $dlsta | $start_file | $end_file | $status | $attempts | $size | $speed | $md5");

        if ($db[3] >= 0) {
            dbputv(@db,
                "net",      $net, 
                "sta",      $sta, 
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
                "status",   $status);
        } else {
            dbaddv(@db,
                "net",      $net, 
                "sta",      $sta, 
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
                "status",   $status);
        }

        dbclose(@db);

        debug("Next file");
    #}}}
    }

    log_die( "NO DOWNLOADS!!!! Station not downloading any files.") unless scalar @total_downloads;

    delete $flagged{$_} foreach @total_downloads;

    problem('Missing:' . join(' ' ,sort keys %flagged)) if scalar keys %flagged;

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

    logging("Done rsync of ".@total_downloads." files ($m Mb) form $ip in $run_time_str");



#}}}
}

sub check_time {
#{{{ check_time(sta)
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
#{{{ total_data_downloaded(sta,days)
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
    eval { dbquery(@db,dbTABLE_PRESENT); };
    if ( $@ ) {
        dbclose(@db);
        return;
    }

    if (dbquery(@db, dbRECORD_COUNT) < 1) {
        dbclose(@db);
        return;
    }

    @db_subset = dbsubset ( @db, "status =~ /downloaded|error/");
    $nrecords = dbquery(@db_subset, dbRECORD_COUNT) ;
    return unless $nrecords;


    $start_of_report = str2epoch("-${days}days");
    @db_temp= dbsubset ( @db_subset, "time >= $start_of_report");
    $nrecords = dbquery(@db_temp, dbRECORD_COUNT) ;
    return unless $nrecords;

    if ($nrecords > 0) {
        for ( $db_temp[3] = 0 ; $db_temp[3] < $nrecords ; $db_temp[3]++ ) {
            #$total_bytes += (dbgetv (@db_temp, 'filebytes') * dbgetv (@db_temp, 'attempts') ) ;
            $total_bytes += (dbgetv (@db_temp, 'filebytes')) ;
        }
    }
    dbfree(@db_subset);
    dbfree(@db_temp);
    dbclose(@db);

    # for Kbytes
    $total_bytes = sprintf("%0.2f", $total_bytes/1024);
    # for Mbytes
    $total_bytes = sprintf("%0.2f", $total_bytes/1024);

    debug("Total bytes downloaded: $total_bytes");


    return $total_bytes;
#}}}
}

sub download_file {
#{{{ downlaod_file(file,path,ip)
    my $file = shift;
    my $path = shift;
    my $ip = shift;
    my ($file_fetch,$where);
    my @temp_new;


    #foreach ( split(/[\s|\/]/,$file) ) {
    #    next if /^\/$/;
    #    push(@temp_new,$_) if /^\S+$/;
    #}

    #$file = join('/',@temp_new);

    debug("Build File::Fetch object: http://$ip:$pf{http_port}/$file");

    eval{ $file_fetch = File::Fetch->new(uri => "http://$ip:$pf{http_port}/$file"); };
    problem("File::Fetch -> $@") if $@; 

    problem("ERROR in build of File::Fetch -> http://$ip:$pf{http_port}/$file") unless $file_fetch; 
    return unless $file_fetch;

    debug("Download: ".$file_fetch->uri);

    #eval {  $where = $file_fetch->fetch( to => "$path/" ); };
    #problem("File::Fetch ".$file_fetch->uri." $@") if $@; 
    $where = $file_fetch->fetch( to => "$path/" ) || '';

    #problem("ERROR on download of http://$ip:$pf{http_port}/$file") unless -f $where;

    #problem("ERROR on $file after download. Problem with name") unless $where =~ /$temp_new[-1]/;

    #problem("Cannot see $file after download") unless -f $where;

    return $where if -f $where;
    return
#}}}
}

sub open_db {
#{{{ open_db(sta)
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

    debug("Opening database ($dbout).");

    #
    # Create descriptor file if missing
    #
    unless ( -e $dbout) {
        debug("$sta Creating new database ($dbout).");

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
    debug("$sta Openning database table  ($dbout)");
    return  dbopen_table($dbout,"r+") or log_die("Can't open DB: $dbout",$sta);

#}}}
}

sub fix_local {
#{{{ fix_local(sta,net,dlsta)
    my $sta = shift;
    my $net = shift;
    my $dlsta = shift;
    my %list;
    my %llist;
    my ($r,$record,@db_sub,@db_r,@db);
    my ($file,$status,$extra,$size,$path,$f);

    debug("Reading and fixing of local directory");

    $path = prepare_path($sta);

    #
    # Clean database
    #
    @db = open_db($sta);
    $r = dbquery(@db , dbRECORD_COUNT);
    for ($db[3]=0; $db[3] < $r; $db[3]++){
        $list{ dbgetv(@db,'dfile') } = 1;
    }
    dbclose(@db);

    foreach $file (sort keys %list) {
        logging("Clean dfile =~/$file/ ");

        @db = open_db($sta);
        @db_sub = dbsubset(@db, "dfile =~ /$file/");
        $r = dbquery(@db_sub,dbRECORD_COUNT);

        #
        # Make sure that we only have one entry for the file
        #
        if ( $r > 1 ) {

            problem("More than one entry for $file");
            while(1) {
                $db[3] = dbfind(@db, "dfile =~ /$file/", -1); 
                last unless $db[3] >= 0;
                last if $r == 1;
                problem("remove: $file ($r)=> " . dbgetv(@db,'status'));
                dbmark(@db);
                $r -= 1;
            }
            debug("Crunch table.");
            dbcrunch(@db);

        }

        dbclose(@db);
    }

    #
    # Verify that every file on the database exists
    #
    @db = open_db($sta);
    $r = dbquery(@db , dbRECORD_COUNT);
    for ($db[3]=0; $db[3] < $r; $db[3]++){
        $f = dbgetv(@db,'dfile');
        debug("local database: $f");

        if ( $f !~ /^(..-(${sta}|EXMP)_4-\d{14})$/ ) {
            problem("remove(format): $f");
            dbmark(@db);
            next;
        }

        next unless dbgetv(@db,'status') =~ /downloaded/;

        unless ( -f "$path/$f"  ) {
            problem("remove(not in directory): $f");
            dbmark(@db);
        }
    }

    dbcrunch(@db);
    dbclose(@db);

    #
    # Verify every file in the folder
    #
    opendir(DIR,$path) or log_die("Failed to open $path: $!");

    @db = open_db($sta);
    while($f = readdir DIR) {
        next if -d "$path/$f";
        next if $f !~ /^(..-(${sta}|EXMP)_4-\d{14})$/;

        debug("local file: $f");

        #
        # Subset database for file in directory
        #
        @db_r = dbsubset(@db, "dfile =~ /$f/");
        $r = dbquery(@db_r , dbRECORD_COUNT);

        if ( $r == 1  ) {

            $db_r[3] = 0;

            if ( dbgetv(@db_r, 'status') =~ /downloaded/) {

                debug("$f already in database as downloaded");

            } elsif ( dbgetv(@db_r, 'status') =~ /skip/) {

                problem("$f flagged as 'skipped'");

            } else {

                problem("$f updated to 'downloaded'");
                dbputv(@db_r,'status', 'downloaded', 'attempts', 1, 'time', now(), 'lddate', now()  );

             }

        } else {

            #
            # Add the missing file
            #
            $size = -s "$path/$f" || 0; 
            logging("$f adding as 'downloaded'");
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

        dbfree(@db_r);
    }

    dbclose(@db);
    close(DIR);

    return;
#}}}
}

sub read_baler {
#{{{ read_baler(sta,ip,m_active,m_reserve)
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

    log_die("Cannot make directory ($path)") unless -d $path;

    chdir $path or log_die("Cannot change to directory ($path)");


    #
    # For each of the folders
    #
    foreach $dir ( qw/active reserve/ ) {

        $path = "list.$dir.data.gz";

        debug("Get: $path");

        unlink $path if -e $path;
        #
        # download list of files now
        #
        eval{ $file_fetch = File::Fetch->new(uri => "http://$ip:$pf{http_port}/$path"); };
        problem("File::Fetch($path) => $@") if $@;

        eval {  $where = $file_fetch->fetch( to => "./" ); };
        problem("File::Fetch(".$file_fetch->uri.") => $@") if $@;


        unless ( $where ) {
            problem("Error fetching:  http://$ip:$pf{http_port}/$path");
            next;
        }

        problem("ERROR after download of: $path") unless -e $path;
        open $input, "<$path"; 
        $files = new IO::Uncompress::AnyUncompress $input or problem("IO::Uncompress::AnyUncompress failed: $AnyUncompressError");

        while ( <$files> ) {
            #
            # Parse results and get size
            #
            s/\n//;
            next unless /(..-(${sta}|EXMP)_4-\d{14})/;
            @temp_dir = split(/\//,$_);
            $name = pop(@temp_dir);
            push(@temp_dir," ");
            unshift(@temp_dir, $dir =~ /active/ ? 'WDIR' : 'WDIR2' );
            unshift(@temp_dir," ");

            $list{$name} = join('/',@temp_dir);
            debug("$name => $list{$name}");
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

    debug("Fix media id on database.");

    for ( $db[3] = 0 ; $db[3] < $nrecords ; $db[3]++ ) { 
        $name = dbgetv(@db, 'dfile'); 
        next unless $list{$name}; 
        if ($list{$name} =~ /.*WDIR2.*/ ) {
            debug("Update $name to media $media_reserve");
            dbputv(@db,'media', $media_reserve );
        } 
        else {
            debug("Update $name to media $media_active");
            dbputv(@db,'media', $media_active );
        }
    }

    dbclose(@db);

    return %list;

#}}}
}

sub get_md5 {
#{{{ get_md5(sta,file,ip)
    my $sta  = shift;
    my $file  = shift;
    my $ip    = shift;
    my ($old,$md5_lib,$digest,$md5,$local_path,$folder);
    my @md5_raw;

    debug("Get MD5 $file");

    $local_path = prepare_path($sta) . '/md5/'; 

    $old = $file;
    $file .= '.md5' unless $file =~ /\.md5/;

    chdir $local_path or log_die("Cannot change to directory ($local_path)");

    unless ( -s "$local_path/$file" ) {

        unlink "$local_path/$file";
        debug("Lets download MD5 file $file -> $local_path");

        foreach (qw/WDIR WDIR2/) {
            last if download_file("$_/recover/$file",$local_path,$ip);
        }

    }

    problem("Error downloading: $file")  unless -e "$local_path/$file";
    return 'missing' unless -e "$local_path/$file";

    open(DAT, '<', "$local_path/$file") or log_die("Cannot open $local_path/$file!");

    while (<DAT>) {
        chomp;
        debug("MD5:: $_");
        push @md5_raw, split;
    }

    close(DAT);

    unless ( $md5_raw[1] ) {
        problem("Error in file $local_path/$file @md5_raw");
        unlink "$local_path/$file";
        return 'error';
    }

    $md5 = $md5_raw[0];

    if ( $md5_raw[1] !~ /($old)/ ) {
        problem("Error in file $local_path/$file @md5_raw");
        unlink "$local_path/$file";
        return 'error';
    }


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

    return $md5 if $digest eq $md5;
    
    problem("$file MD5 problem:: reported:$md5 calc:$digest " );

    return 'error';

#}}}
}

sub get_medias {
#{{{ get_medias(sta,ip)
    my $sta = shift;
    my $ip = shift;
    my (@text,$line,$browser, $resp);
    my $active = '';
    my $reserve = '';

    $browser = LWP::UserAgent->new;

    $resp = $browser->timeout(120);

    debug("$sta:\tLWP::UserAgent->get(http://$ip:$pf{http_port}/stats.html)");
    $resp = $browser->get("http://$ip:$pf{http_port}/stats.html");

    unless ( $resp->is_success ) {
        debug("$sta:\tLWP::UserAgent->get(http://$ip:$pf{http_port}/stats.html)");
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

    $active  ||= '';
    $reserve ||= '';

    debug("get(http://$ip:$pf{http_port}/stats.html) => ($active,$reserve)");
    return ($active,$reserve);

#}}}
}

sub remove_file {
#{{{ remove_file(sta,file,all)
    my $sta  = shift;
    my $file = shift;
    my $all = shift;
    my @db; 

    return unless $file;

    my $path = prepare_path($sta);

    problem("Remove file: $path/$file");


    #
    # Verify file in folder
    #
    if ($file and -f "$path/$file") {
        debug("move $path/$file to $path/trash/$file");
        move("$path/$file","$path/trash/$file") or problem("Can't move $file to $path/trash");
    }

    @db = open_db($sta);

    if ( $all ) {
        while ( 1 ) { 
            $db[3] = dbfind(@db, "dfile =~ /$file/", -1); 
            last unless $db[3] >= 0;
            debug("remove entry: $db[3] => $file");
            dbmakr(@db);
        }
        dbcrunch(@db);
    } else {
        while ( 1 ) { 
            $db[3] = dbfind(@db, "dfile =~ /$file/", -1); 
            last unless $db[3] >= 0;
            dbputv(@db,'status','error','lddate',now());
            dbputv(@db,'attempts',int(dbgetv(@db,'attempts'))+1);
            debug("add 'error' : $db[3] => $file");
        }
    }

    dbclose(@db);

    return;
#}}}
}

sub prepare_path {
#{{{ prepare_path(sta)
    my $station  = shift;
    my $path = '';

    log_die("prepare_path(). Cannot produce path! We need a station name...") unless $station;

    $path = File::Spec->rel2abs( "$pf{local_data_dir}/$station" ); 

    makedir($path) unless -e $path;
    log_die("Cannot create folder $path") unless -e $path;

    mkdir "$path/trash" unless -d "$path/trash";
    log_die("Cannot make directory ($path/trash/)") unless -d "$path/trash";

    mkdir "$path/md5" unless -d "$path/md5";
    log_die("Cannot make directory ($path/md5/)") unless -d "$path/md5";

    mkdir "$path/lists" unless -d "$path/lists";
    log_die("Cannot make directory ($path/lists/)") unless -d "$path/lists";


    return $path;
#}}}
}

sub getparam {
#{{{ getparam(pf_file)
    my $PF = shift ;
    my %pf;

    foreach  (qw/local_data_dir max_child_run_time download_timeout 
                database http_port max_procs/){
        $pf{$_} = pfget($PF,$_);

        log_die("Missing value for $_ in PF:$PF") unless defined($pf{$_});

        debug( sprintf("\t%-22s -> %s", ($_,$pf{$_})) );
    }

    return (%pf);
#}}}
}

sub table_check {
#{{{ table_check(db,sta)
    my $db = shift;
    my $sta = shift;

    $sta ||= '';

    debug("Verify Database: ".dbquery(@$db,dbDATABASE_NAME) );

    log_die( dbquery(@$db,dbTABLE_NAME)." not available.",$sta) unless dbquery(@$db,dbTABLE_PRESENT);

    debug("\t".dbquery(@$db,dbDATABASE_NAME)."{ ".dbquery(@$db,dbTABLE_NAME)." }: --> OK");

#}}}
}

sub log_die {
#{{{ log_die(msg)
    my $msg = shift;

    problem($msg);

    exit if $parent != $$;

    sendmail("ERROR: $0 @ARGV DIED ON $host",$opt_m,"/tmp/#rtsys$$") if $opt_m; 

    elog_die($msg);

#}}}
}

sub logging {
#{{{ logging(msg,sta)
    my $msg = shift;
    my $sta = shift || '';
    my $string = 0;
    my $now = strtime(now());

    return unless $opt_v;

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
#{{{ debug(msg,sta)
    my $msg = shift;
    my $sta = shift;
    my $string = 0;
    my $now = strtime(now());

    return unless $opt_d; 

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
#{{{ problem(msg,sta)
    my $text = shift; 
    my $sta = shift || 'parent';
    my $now = strtime(now());
    my $string = 0;

    if ( $parent != $$ ) {
        if ( $to_parent ) {
            print { $to_parent } "[PROBLEM:($now): $text]";
            return;
        }
    }

    if ( $sta ne 'parent' ) {
        $string = keys %{$problems_hash->{'problems'}->{$sta}};
        $problems_hash->{'problems'}->{$sta}->{$string+1} = $text;
        elog_complain("$sta: $text") if $opt_v;
        return;
    }

    elog_complain("\n\t* Problem : \n\t* $sta: $text\n");

#}}}
}

sub problem_print {
#{{{ problem_print()

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
        for $p_v ( sort{ $a <=> $b } keys %{$problems_hash->{'problems'}->{$s_v}} ) {
            elog_notify("\t\t $p_v) $problems_hash->{'problems'}->{$s_v}->{$p_v}");
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
