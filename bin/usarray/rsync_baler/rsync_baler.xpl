#
#  *** No BRTT support ***
#
#   rsync_baler: script to create a local copy of remote baler
#
#  The software will connect to any active baler44 station and will
#  download all missing miniseed files on the local archive. The
#  code will get the list of stations and the ips from the information
#  present in tables [deployment, stabaler, staq330] (all of them
#  are required for the process to run). The process will ping each
#  station for connectivity and then will download a compressed list
#  of files present on the remote media. The list is verified with
#  the local list of files and any missing file is flagged for
#  download. Each file will also have a md5 checksum present on the
#  remote baler. The checksum is also downloaded and verified with
#  the locally calculated checksum. Each step of the process is
#  archived on a rsyncbaler table in the local directory.
#
#   author: Juan C. Reyes
#   email:  reyes@ucsd.edu
#   last update 05/2013
#

#
#  Program setup
#
# we want to build vars on the fly. Cannot use strict subs.
#use strict "subs" ;
use strict "vars" ;
use warnings ;

use LWP ;
use Fcntl ;
use POSIX ;
use archive ;
use utilfunct qw[getparam check_procs];
use sysinfo ;
use Net::Ping ;
use Datascope ;
use Pod::Usage ;
use IO::Handle ;
use File::Spec ;
use File::Copy qw[move];
use Getopt::Std ;
use File::Fetch ;
#use File::Basename ;
use Digest::MD5 qw[md5_hex] ;
use IO::Uncompress::AnyUncompress qw[anyuncompress $AnyUncompressError] ;

our(%pf) ;
our(%logs,%errors) ;
our($to_parent,$nstas,$get_sta,$parent,$host) ;
our($string,$problems,$nchild,$file_fetch) ;
our($subject,$start,$end,$run_time_str) ;
our($opt_n,$opt_x,$opt_V,$opt_r,$opt_s,$opt_h,$opt_v,$opt_m,$opt_p,$opt_d) ;

use constant false => 0 ;
use constant true  => 1 ;

STDOUT->autoflush(1) ;

$ENV{'ELOG_MAXMSG'} = 0 ;
$ENV{'ELOG_TAG'} = 'rsync_baler *%s*' ;
$parent = $$ ;
$start = now() ;
$host = my_hostname() ;

elog_init($0,@ARGV) ;

unless ( &getopts('nxhdvm:p:s:r:') || @ARGV > 1 ) {
    pod2usage({-exitval => 99, -verbose => 2}) ;
}

#
# Print help and exit
#
pod2usage({-exitval => 99, -verbose => 2}) if $opt_h ;


#
# If we want to fork the process this will take
# the argument  for the station that needs processing.
#
$get_sta = $ARGV[0] || '' ;

if ( $get_sta ) {
    # run as child...
    $parent = 0 ;
    fork_notify("Running as child process for station: $get_sta")
        if $opt_v ;

} else {
    # run as parent (deamon)...
    fork_notify("Running as parent process.\n") if $opt_v ;
}

#
# Initialize  mail
#
if ($opt_m){
    savemail() ;
    fork_notify("Initialize mail") if $opt_v ;
}

fork_notify("$0 @ARGV") if $opt_v ;
fork_notify("Starting at ".strydtime($start)." on $host")
    if $opt_v ;

#
# Implicit flags
#
$opt_v = $opt_d ? $opt_d : $opt_v ;
$opt_V = $opt_d ;
$opt_p ||= "rsync_baler.pf" ;

#
# Get parameters from config file
#
fork_notify("Getting params from: $opt_p") if $opt_v ;
%pf = getparam($opt_p) ;

#
# Verify reject station subset
# Note:
#   If we have a reject subset from command-line
#   then we use that string. If we don't then
#   we look into the parameter file for a possible
#   definition set on days of the week. This is
#   related to the second process that we run
#   over the files and parses the data. We want
#   to avoid collision with that process.
#
if ( $pf{avoid_on_day_of_week} ) {
    $opt_r ||= $pf{avoid_on_day_of_week}{epoch2str( now(), "%A" )} ;
}

#
#  Set File::Fetch options
#
#   File::Fetch
#   Below is a mapping of what utilities will
#   be used in what order for each schemes (if available):
#       file    => LWP, lftp, file
#       http    => LWP, wget, curl, lftp, lynx, iosock
#       ftp     => LWP, Net::FTP, wget, curl, lftp, ncftp, ftp
#       rsync   => rsync
#
$File::Fetch::WARN      = 0 unless $opt_d ;
$File::Fetch::DEBUG     = 1 if $opt_d ;
$File::Fetch::TIMEOUT   = $pf{download_timeout} ;
$File::Fetch::BLACKLIST = [qw/LWP ncftp lftp lynx iosock/] ;

#
# Verify access to directory
#
fork_die("Can't access dir => $pf{local_data_dir}.")
    unless -e $pf{local_data_dir} ;


#
# ************************
# **    MAIN LOOP       **
# Two options here...
# 1) Parent process loop for thread spawning.
# 2) Child process func call to get data.
#

$string = "finished processing station" ;

#
# Get data from the stations
#
if ( $get_sta ) {
    get_data($get_sta) ;
    fork_notify("$get_sta - $string") ;
    exit ;
    # CHILD PROCESS WILL END HERE!!!!
}
else {
    $nstas = run_in_threads() ;
}


#
# ONLY PARENT PROC SHOULD COME HERE!!!!
#

#
# Find aborted child
#
&test_missing_children ( $string, \%logs, \%errors ) ;

#
# Print error logs
#
( $nchild, $problems ) = &test_problem_print ( \%errors ) ;

#
# Print logs
#
&test_log_print ( \%logs ) ;


if ( $opt_v ) {
    fork_notify("started at ".strydtime($start)." on $host") ;
    fork_notify ("completed at ".strydtime(now())." \n\n" ) ;
}

if ($problems == 0 ) {
    $subject = sprintf("Success $0  $host - $nstas stations") ;
}
else {
    $subject = "Problems $0 $host - $nstas stations($nchild errors)" ;
}

fork_notify( "\n$subject" ) ;
sendmail($subject,$opt_m) if $opt_m ;

exit ;

#
# **    End of script   **
# ************************
#

#
# ************************
# **   Functions:       **
#
sub get_stations_from_db {
    my ($ip,$dlsta,$net,$sta,$nrecords) ;
    my %sta_hash ;
    my (@db,@db_sta) ;

    fork_notify('Get list of stations:') if $opt_v ;

    #
    # Verify Database
    #
    fork_notify("Using database $pf{database}") if $opt_v ;

    @db = dbopen( $pf{database}, "r" )
        or fork_die("Can't open DB: $pf{database}") ;

    # Open table for list of station types ie 'PacketBaler44'
    @db_sta = dblookup(@db, "", "stabaler", "", "") ;

    #
    # Get stations with baler44s
    #
    fork_notify("dbsubset ( stablaler.model =~ /$pf{baler_model}/)") if $opt_v ;
    @db_sta = dbsubset( @db_sta, "stabaler.model =~ /$pf{baler_model}/ ") ;

    if ( $opt_s ) {
        fork_notify("dbsubset ( sta =~ /$opt_s/)") ;
        @db_sta = dbsubset( @db_sta, "sta =~ /$opt_s/") ;
    }

    if ( $opt_r ) {
        fork_notify("dbsubset ( sta !~ /$opt_r/)") ;
        @db_sta = dbsubset( @db_sta, "sta !~ /$opt_r/") ;
    }

    $nrecords = dbquery(@db_sta,dbRECORD_COUNT)
        or fork_die("No records after dbsubset()") ;

    fork_notify("$nrecords after subset") ;

    for ( $db_sta[3]=0 ; $db_sta[3] < $nrecords ; $db_sta[3]++ ) {

        fork_debug("Get $db_sta[3] of $nrecords") if $opt_d ;

        ($dlsta,$net,$sta) = dbgetv(@db_sta, qw/dlsta net sta/) ;

        $sta_hash{$sta}{dlsta} = $dlsta ;
        $sta_hash{$sta}{net}   = $net ;

        fork_debug("selected: $net $sta") if $opt_d ;
    }

    dbclose(@db) ;

    fork_die("NO STATIONS SELECTED FROM DATABASE.")
        unless %sta_hash ;

    return %sta_hash ;
}

sub get_info_for_sta {
    my $sta = shift ;
    my ($ip,$dlsta,$net) ;
    my %sta_hash ;
    my (@db,@db_sta,@db_ip,@db_on) ;

    #
    # Verify Database
    #
    fork_debug("Using db $pf{database}") if $opt_v ;

    @db = dbopen( $pf{database}, "r" )
        or fork_die("Can't open DB: $pf{database}") ;

    # Open table for list of valid stations
    @db_on = dblookup(@db, "", "deployment" , "", "") ;

    # Open table for list of station types ie 'PacketBaler44'
    @db_sta = dblookup(@db, "", "stabaler", "", "") ;

    # Open table for list of current ips
    @db_ip = dblookup(@db, "", "staq330" , "", "") ;

    #
    # Net and dlsta values
    #
    $db_sta[3] = dbfind(@db_sta, " sta =~ /$sta/ ",-1) ;

    #
    # NOTE:
    #   Get the value ONLY if we get a valid pointer at $db_sta[3]
    #
    ($dlsta,$net) = dbgetv(@db_sta, qw/dlsta net/)
        if ( $db_sta[3] >= 0 ) ;

    $sta_hash{dlsta}  = $dlsta ;
    $sta_hash{net}    = $net ;
    $sta_hash{status} = 'Decom' ;
    $sta_hash{ip}     = 0 ;

    if ( dbfind(@db_on,
            "sta =~ /$sta/ && snet =~ /$net/ && endtime == NULL", -1)
            >= 0 )
    {
        #
        # If we find it then it is ACTIVE...
        #
        $sta_hash{status} = 'Active' ;

        #
        # Get ip for station
        #
        $db_ip[3] = dbfind( @db_ip,
            " dlsta =~ /$dlsta/ && endtime == NULL",-1) ;

        if ( $db_ip[3] >= 0 ) {

            $ip = dbgetv(@db_ip, 'inp') ;

            #
            # Use this regex to clean the ip string...
            #
            $ip =~ /([\d]{1,3}\.[\d]{1,3}\.[\d]{1,3}\.[\d]{1,3})/ ;

            if ( $1 ) {
                $sta_hash{ip} = $1 if $1 ;
            }
            else {
                fork_complain("Failed grep on IP (ip'$ip',dlsta'$dlsta')") ;
            }
        }
    }

    if ( $opt_d ) {
        fork_notify("$dlsta => $sta $net") ;
        fork_notify("$dlsta => $sta_hash{status}") ;
    }

    fork_notify("$dlsta => $sta_hash{ip}:$pf{http_port}") ;

    dbclose(@db) ;

    return %sta_hash ;
}

sub run_in_threads {
    my @procs ;
    my $station ;
    my $pid ;
    my $cmd ;
    my $max_out = $pf{max_procs} ;
    my %archive = get_stations_from_db() ;

    $nstas = scalar keys %archive ;

    STATION: foreach $station (sort keys %archive) {

        fork_debug("run_in_threads($station)") if $opt_d ;

        #
        # throttle the reading engine
        #
        #sleep (10/scalar @procs) if scalar @procs ;
        sleep (5) ;

        #
        # Verify running procs
        #
        @procs = check_procs(@procs) ;

        #
        # Read messages from pipes
        #
        test_nonblock_read(\%archive,\%logs,\%errors) ;

        #
        # Stop if we are at max procs
        #
        redo STATION if scalar(@procs) >= $max_out ;

        if ( $opt_d ) {
            fork_debug("Spawn: get_data($station).") ;
            fork_debug("Now: ".@procs." procs") ;
        }

        #
        # Fork the script (not using &fork)
        #
        $cmd   =  "$0 " ;
        $cmd  .=  "-n " if $opt_n ;
        $cmd  .=  "-x " if $opt_x ;
        $cmd  .=  "-v " if $opt_v ;
        $cmd  .=  "-d " if $opt_d ;
        $cmd  .=  "$station " ;


        if ( $opt_v ) {
            fork_notify("Now: ".@procs." procs") ;
            fork_notify("Starting $station: $cmd ") ;
        }

        ##################
        #   OLD METHOD   #
        ##################
        #pipe($archive{$station}{fh}, $to_parent)
        #   or fork_die('Cannot create pipe()') ;
        #$pid = fork() ;
        #exec( $cmd ) unless $pid ;

        undef $pid ;
        $pid = open($archive{$station}{fh}, "-|") ;

        #
        # Test for possible errors on fork process
        #
        fork_die("fork() failed: $!") unless defined $pid ;

        if ($pid) {

            #
            # Parent continues here.
            #
            fcntl($archive{$station}{fh},F_SETFL, O_NONBLOCK) ;
            $archive{$station}{fh}->autoflush(1) ;

            fork_debug("Got pid $pid for $station") if $opt_d ;
            push @procs, $pid ;
            $archive{$station}{pid} = $pid ;

        }
        else {

            #
            # Child continues here
            #
            exec($cmd) ;
        }


    }

    fork_debug("Initiated all stations." ) if $opt_v ;

    #
    # wait for all children to finish
    #
    #nonblock_read(\%archive,\%logs,\%errors) while check_procs(@procs) ;
    while ( 1 ){

        test_nonblock_read(\%archive,\%logs,\%errors) ;

        last unless scalar check_procs(@procs) ;

        #
        # throttle the reading engine
        #
        sleep (5) ;

    }
    #while (<$to_parent>) { fork_notify($_) ; }

    return $nstas ;

}

sub test_missing_children { # &missing_children ( $string, \%logs, \%errors ) ; 
    my ( $string, $logs, $errors ) = @_ ;
    my ( $line ) ;


    LOG: for my $log ( sort keys %$logs) {

        for my $lineno ( sort keys %{$logs->{$log}} ) {
            next if ( $lineno =~ /lines/ ) ;
            if ( $logs->{$log}->{$lineno} =~ /.*$string.*/ ) {
                next LOG ;
            }
        }

        $line = "$log DID NOT COMPLETE!" ;
        $logs->{$log}->{lines}++ ;
        $logs->{$log}->{ $logs->{$log}->{lines} } = $line ;
        $errors->{$log}->{problems}++ ;
        $errors->{$log}{ $errors->{$log}->{problems} } = $line ;

    }

    return ;
}

sub test_problem_print { # ( $nchild, $problems ) = &problem_print ( \%errors ) ;
    my $errors = shift ;
    my ( @total ) ;
    my ( $nchild, $nerr, $nprob ) ;

    $nchild = $nerr = $nprob = 0 ;


    fork_complain('') ;
    fork_complain('') ;
    fork_complain("-------- Problems: --------") ;
    fork_complain('') ;


    for my $k ( sort keys %$errors) {

        next if ( $errors->{$k}->{problems} == 0 ) ;
        $nchild++ ;
        $nprob++ ;
        fork_complain("On child $k:") ;

        @total = () ;
        for my $j ( keys %{$errors->{$k}} ) {
            next if ( $j =~ /problems/ ) ;
            push( @total, int($j) ) ;
        }

        for my $j ( sort {$a <=> $b} @total ) {
            fork_complain("   $j) $errors->{$k}->{$j}") ;
            $nerr++ ;
        }

        fork_complain('') ;
    }

    fork_complain("No problems in script.") unless $nprob ;

    fork_complain("-------- End of problems: --------") ;
    fork_complain('') ;

    return ( $nchild, $nerr ) ;
}

sub test_log_print { # &log_print ( \%logs ) ;
    my $logs = shift ;
    my ( @total ) ;

    fork_notify('') ;
    fork_notify('') ;
    fork_notify("-------- Logs: --------") ;
    fork_notify('') ;

    for my $k ( sort keys %$logs) {

        fork_notify("On child $k:") ;

        @total = () ;
        for my $j ( keys %{$logs->{$k}} ) {
            next if ( $j =~ /lines/ ) ;
            push( @total, int($j) ) ;
        }

        for my $j ( sort {$a <=> $b} @total ) {
            fork_notify("   $j) $logs->{$k}->{$j}") ;
        }

        fork_notify('') ;
    }

    fork_notify("-------- End of logs: --------") ;
    fork_notify('') ;
}

sub test_nonblock_read { # &nonblock_read ( \%stas, \%logs, \%errors ) ;
    my ( $stas, $logs, $errors ) = @_ ;
    my ( $fh, $fileline, $line )  ;

    foreach my $sta (sort keys %$stas) {

        next unless $fh = $stas->{$sta}->{fh} ;
        #fork_debug ( $parent, "nonblock_read $sta    $stas->{$sta}->{fh}" );

        while ( $fileline = <$fh> ) {

            while ( $fileline =~ /\[LOG:(.*?)\]$/g )     {
                $line = $1 ;
                $logs->{$sta}->{lines}++ ;
                $logs->{$sta}->{ $logs->{$sta}->{lines} } = $line ;
            }
            while ( $fileline =~ /\[NOTIFY:(.*?)\]$/g )     {
                $line = $1 ;
                $logs->{$sta}->{lines}++ ;
                $logs->{$sta}->{ $logs->{$sta}->{lines} } = $line ;
            }
            while ( $fileline =~ /\[DEBUG:(.*?)\]$/g )     {
                $line = $1 ;
                $logs->{$sta}->{lines}++ ;
                $logs->{$sta}->{ $logs->{$sta}->{lines} } = $line ;
            }
            while ( $fileline =~ /\[PROBLEM:(.*?)\]$/g )     {
                $line = $1 ;
                $logs->{$sta}->{lines}++ ;
                $logs->{$sta}->{ $logs->{$sta}->{lines} } = $line ;
                $errors->{$sta}->{problems}++ ;
                $errors->{$sta}->{ $errors->{$sta}->{problems} } = $line ;
            }
        }

        unless ( check_procs( $stas->{$sta}->{pid} ) ) {

            close $stas->{$sta}->{fh} ;
            undef $stas->{$sta}->{fh} ;
        }
    }
    return ;
}

sub get_data {

    my $sta = shift ;

    fork_die("No value for station in child.") unless $sta ;

    my (%active_media_files,$media,@rem_file) ;
    my (%remote,$size,$nrecords,@temp_download,@dbwr,@dbr,@dbr_sub) ;
    my (%clean,$local_path_file,%avoid,$replace,$file,$speed,$run_time) ;
    my ($bytes,$d_data,$start_file,$where,$attempts,@missing) ;
    my ($p,$rem_s,$loc_s,@diff,$results,$run_time_str,$dbout) ;
    my ($msdtime,$end_file,$record,$time,$endtime,$dir,$dfile) ;
    my ($k,$m,$g,$total_size,%temp_hash,@total_downloads,@download) ;
    my ($digest,$hexd,$md5,$remote_file_content, $remote_file_handle) ;
    my ($stat,$mode,$f,$http_folder,$md5_lib,@original_downloads) ;
    my (@lists,@dbscr,@recs,@data,@db_r,%remove,%flagged,@db,@db_t) ;
    my ($media_active, $media_reserve,%downloaded) ;
    my ($start_of_report) ;

    my %table = get_info_for_sta($sta) ;

    #
    # Prepare Variables and Folders
    #
    my $type    = '' ;
    my $resp    = 0 ;
    my $ip     = $table{ip} ;
    my $dlsta  = $table{dlsta} ;
    my $net    = $table{net} ;
    my $status = $table{status} ;
    my @dates  = $table{dates} ;
    my $path = prepare_path($sta,$status) ;
    my $start_sta = now() ;


    #
    # Stop if this is a DECOM station
    # and the folder is not present
    # in the archive.
    #
    unless ( -e $path ) {
        fork_notify("STOP! $sta DECOMMISSION with no folder.") ;
        return 0 ;
    }

    #
    # Set umask for process
    #
    umask 002 ;


    #
    # Try to lock baler database.
    #
    if ( dblock("${path}/${sta}_baler",$pf{max_child_run_time}) ) {
        fork_die("Cannot lock database ${path}/${sta}_baler") ;
    }

    #
    # Read local files and fix errors
    #
    fix_local( $sta,$net,$dlsta ) ;

    #
    # For DECOM stations stop here
    #
    unless ( $status eq 'Active') {
        dbunlock("${path}/${sta}_baler") ;
        fork_notify("$sta is under DECOMMISSION status.") ;
        return ;
    }

    #
    # No more to do in this case.
    #
    unless ( $ip ) {
        dbunlock("${path}/${sta}_baler") ;
        fork_die("$sta has no IP") ;
    }

    #
    # Limit the downloads to lest than 3 Gbs in last 21 days
    #
    $d_data = total_data_downloaded($sta,21) || 0.0 ;
    if ( $d_data > 3000 ) {
        dbunlock("${path}/${sta}_baler") ;
        fork_die("$sta downloaded ( $d_data ) Mbts in the last 21 days!") ;
    }

    #
    # Ping the station
    #
    $p = Net::Ping->new("tcp", 5) ;
    $p->port_number($pf{http_port}) ;
    $record=0 ;

    while ( 1 ) {
        $record++;

        last if $p->ping($ip) ;

        sleep 5 ;

        if ( $record == 20 ) {
            dbunlock("${path}/${sta}_baler") ;
            fork_die("$sta on http://$ip:$pf{http_port} NOT RESPONDING!") ;
        }
    }

    undef($p) ;

    #
    # Get medias ID's
    #
    ($media_active, $media_reserve, @lists)
        = get_medias_and_lists($sta,$ip) ;
    $media_active ||= 'unknown' ;
    $media_reserve ||= 'unknown' ;

    #
    # Review all entries on the database
    #
    @db = open_db($sta) ;
    unless ( @db  ) {
        dbunlock("${path}/${sta}_baler") ;
        fork_die("$sta Problems on db open!") ;
    }
    $record  =  dbquery(@db, dbRECORD_COUNT) ;
    LINE: for ( $db[3] = 0 ; $db[3] < $record ; $db[3]++ ) {

        last unless check_time($start_sta) ;

        ($dfile,$stat,$bytes,$dir,$md5,$attempts,$msdtime,$time)
            = dbgetv(@db,qw/dfile status filebytes dir md5 attempts msdtime time/) ;

        if ( $opt_d ) {
            fork_debug("$dfile,$stat,$bytes,$dir,$md5,$attempts,$msdtime,$time") ;
        }

        #
        # Files to avoid
        #
        if ($dfile !~ /(${sta}|EXMP)/ ) {
            fork_complain("$dfile station name is wrong") ;
            next LINE ;
        }
        if ($dfile !~ /($pf{regex_for_files})/ ) {
            fork_complain("$dfile name is of wrong format") ;
            next LINE ;
        }

        if ($stat =~ /skip/ ) {
            fork_complain("$dfile status set to skiped") ;
            next LINE ;
        }

        if ( $msdtime > 1 ) {
            # file already processed
            fork_debug("$dfile already processed msdtime=>$msdtime")
                if $opt_d ;
            next LINE ;
        }

        if ($attempts > 5) {
            # Too many attempts
            # Add file if we are using the opt_x flag at runtime
            if ( $opt_x ) {

                fork_complain("$dfile will not be rejected based on -x flag") ;

            } else {

                fork_complain("$dfile *AVOID* attempts=>$attempts status=>$stat md5=$md5") ;
                next LINE ;

            }

        }

        #
        # Files to get
        #
        if ($stat !~ /downloaded/) {
            #
            # We already have an entry that IS NOT downloaded.
            # Get file one more time.
            #

            fork_debug("$dfile Already in db and flagged for download")
                if $opt_v ;

            $flagged{$dfile} = '' ;
            next LINE ;

        }

        #
        # This is a typical filie with an error
        # on the download that creates a html
        # only file with some logs in it.
        #
        if ( -s "$path/$dfile" == 591 ) {
            fork_complain("$dfile error in file size == 591") ;
            fork_complain("$dfile add to download list") ;
            $flagged{$dfile} = '' ;
            next LINE ;
        }

        #
        # Verify for valid checksum
        #
        if ($md5 =~ /(\S{32})/) {

            if ( $opt_d ) {
                fork_debug("$dfile verified md5=>$md5") ;
            }
            next LINE ;

        }

        fork_debug("$dfile fixing md5=>$md5") if $opt_v ;

        #
        # If missing checksum then connect to station
        #

        if ( $ip and not $opt_n) {

            fork_debug("Get md5 for $dfile") if $opt_d ;

            dbputv(@db, "md5", get_md5($sta,$dfile,$ip,\@lists),
                    "lddate",dbgetv(@db,"lddate") ) ;

            # Verify...
            $md5 = dbgetv(@db,'md5') ;

            # Keep track of attemtps.
            dbputv(@db,'attempts',int(dbgetv(@db,'attempts'))+1,"lddate",dbgetv(@db,"lddate")) ;

        }

        next if $md5 =~ /missing/ ;

        #
        # Verify for valid checksum
        #
        if ($md5 =~ /(\S{32})/) {

            if ( $opt_d ) {
                fork_debug("$dfile verified md5=>$md5") ;
            }
            next LINE ;

        }

        fork_complain("$dfile Problem with md5.") ;
        fork_complain("$dfile Add to download list.") ;
        $flagged{$dfile} = '' ;

    }


    dbclose(@db) ;

    if ( $opt_v ) {
        fork_notify("F-DB: $_") foreach ( sort keys %flagged ) ;
    }

    %remote = read_baler( $sta, $ip, \@lists, $media_active, $media_reserve) ;

    unless ( keys %remote ) {
        dbunlock("${path}/${sta}_baler") ;
        fork_complain("Can't get any lists of files: $ip:$pf{http_port})") ;
        return ;
    }

    unless ( check_time($start_sta) ) {
        dbunlock("${path}/${sta}_baler") ;
        fork_complain("No more time! EXIT!!!!") ;
        return ;
    }

    #
    # Compare local to remote
    #
    @db = open_db($sta) ;
    unless ( @db  ) {
        dbunlock("${path}/${sta}_baler") ;
        fork_die("$sta Problems on db open!") ;
    }
    foreach $f ( sort keys %remote ) {
        next if $f =~ /\.md5/ ;

        #
        # Check if we have the file
        #
        $db[3] = dbfind(@db, "dfile =~ /$f/", -1) ;

        if ($db[3] >= 0 ){

            next if dbgetv(@db,'status') =~ /downloaded/ ;

            unless ( $opt_x ) {
                next if dbgetv(@db,'attempts') > 5 ;
            }

            if ( dbgetv(@db,'status') =~ /flagged/ ) {
                fork_complain("Increase attemtps: $f") ;
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
                    "status",   "flagged") unless $opt_n ;
            }

        } else {

            fork_notify("F-Baler: $f") ;
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
                "status",   "flagged") unless $opt_n ;

        }

        #
        # Add the files to the list we want to downlaod
        #
        $flagged{$f} = $remote{$f} ;

    }


    # Fix media ids in database
    foreach $f ( sort keys %remote ) {
        next if $f =~ /\.md5/ ;
        next unless $media_active ;
        next unless $media_reserve ;
        next unless $remote{$f} ;

        next if $opt_n ;

        #
        # Check if we have the file
        #
        $db[3] = dbfind(@db, "dfile =~ /$f/", -1) ;

        if ($db[3] >= 0 ){
            dbputv(@db,
                "media",  ($remote{$f} =~ /WDIR2/) ? $media_reserve : $media_active,
                "lddate", dbgetv(@db,"lddate")
            ) ;

        }

    }

    dbclose(@db) ;

    unless (keys %flagged) {
        fork_notify("No new files. http://$ip:$pf{http_port}") ;
        return ;
    }

    if ( $opt_d ) {
        foreach $file ( sort keys %flagged ) {
            fork_notify("$file => $flagged{$file}") ;
        }
    }
    elsif ( $opt_v ) {
        fork_notify('Files: ' . join(' ' ,sort keys %flagged)) ;
    }

    #
    # Download the missing files
    #
    FILE: foreach $file ( sort keys %flagged ) {

        $dir = $flagged{$file} ;
        $where = '' ;
        last unless check_time($start_sta) ;

        fork_notify("Start download: $dir/$file") if $opt_v ;

        if ($file !~ /($pf{regex_for_files})/ ) {
            fork_complain("ERROR ON FLAGGED NAME: $file") ;
            next ;
        }


        next if $opt_n ;

        #
        # Verify if we have a copy of the file in the local dir
        #
        if (-f "$path/$file") {
            #
            # Move the file to the trash folder.
            #
            fork_notify("move $path/$file to $path/trash/") if $opt_v ;

            #
            # Clean old copy on trash folder first.
            #
            if (-f "$path/trash/$file" ) {
                 unlink $file or fork_complain("Can't remove old $path/trash/$file") ;
            }

            move("$path/$file","$path/trash/$file")
                or fork_complain("Can't move $file to $path/trash/") ;

        }

        #
        # Download file
        #
        if ( $dir ) {

            fork_notify("download_file($dir/$file,$path,$ip)") if $opt_v ;

            $start_file = now() ;
            $where = download_file("$dir/$file",$path,$ip) || '' ;
            $end_file = now() ;

        } else {

            #
            # We don't know the correct URL for
            # the file. Lets loop over all alternatives.
            #
            foreach $f (@lists) {

                fork_notify("Now with directory $f") if $opt_d ;

                $dir = ( $f =~ /active/ ? 'WDIR' : 'WDIR2' ) ;

                #
                # For now ALL data files are in some "data" 
                # directory. The md5's are in "recover"
                # direcotries that we get on a different
                # function.
                #
                # Lists may vary in names that are 
                # impossible to predict.
                # Example: TA_445A
                # list.active.admin.gz
                # list.active.cont.gz
                # list.active.data-20130101060955.gz
                # list.active.data.gz
                # list.active.gz
                # list.active.recover-20130101060955.gz
                # list.active.recover-20130101060955.other-20111110225028.gz
                # list.active.recover.gz
                # list.active.sdata-20130101060955.gz
                # list.active.sdata.gz
                # list.active.wfdisc-20130101060955.gz
                # list.active.wfdisc.gz
                # list.reserve.cont.gz
                # list.reserve.data.gz
                # list.reserve.gz
                # list.reserve.recover.gz
                # list.reserve.recover.other-20111110225253.gz
                # list.reserve.sdata.gz
                # list.reserve.wfdisc.gz
                #
                # The regex will allow us to get the possible 
                # variations on the names. Then we can build
                # a good URL for the file.
                #
                ################################################
                # NOTE:
                # Adding option to look into sdata directories.
                # OLD: $f =~ m/list\.\w+\.(data\S*)\.gz/ ;
                # 4/14
                #

                $f =~ m/list\.\w+\.(s?data\S*)\.gz/ ;
                next unless $1 ;

                $start_file = now() ;
                fork_notify("attempt download: $dir/$1/$file") if $opt_v ;
                $where = download_file("$dir/$1/$file",$path,$ip) || '' ;
                $end_file = now() ;
                last if $where ;

            }

        }

        $run_time = $end_file-$start_file ;
        $run_time_str = strtdelta($run_time) ;
        if ( -f $where and $opt_v ) {
            fork_notify("Success download $file $run_time_str") ;
        }

        #
        # Verify downloaded file
        #
        if ( -f $where) {

            $status = 'downloaded' ;
            $md5 = get_md5($sta,$file,$ip,\@lists) || 'error' ;

            if ( $md5 =~ /(\S{32})/ ) {
                fork_notify("$file verified with md5:$md5") if $opt_v ;
            }
            else {
                fork_complain("$file => status:$status md5:$md5") ;
            }

            push @total_downloads, $file ;

        } elsif ( -f "$path/trash/$file") {

            #
            # Resurrect the file if we have previous copy
            #
            fork_complain("Resurrect file from $path/trash/$file") ;
            move("$path/trash/$file","$path/$file")
                or fork_complain("Can't move $file to $path") ;
            $status = 'downloaded' ;
            $md5 = get_md5($sta,$file,$ip,\@lists) || 'error' ;

        } else {

            fork_complain("$file => Not present after download") ;
            $md5 = 'error' ;
            $status = 'error' ;
            $dir = '' ;

        }

        #
        # Verify bandwidth of connection
        #
        $size = -s "$path/$file" ;
        $size ||= 0 ;
        $speed = ($size / 1024 ) / $run_time ;
        $speed ||= 0.00 ;

        #
        # Keep track of total data downloaded
        #
        $total_size += $size ;

        #
        # Add to DB
        #
        @db = open_db($sta) ;
        unless ( @db  ) {
            dbunlock("${path}/${sta}_baler") ;
            fork_die("$sta Problems on db open!") ;
        }
        $db[3] = dbfind(@db, "dfile =~ /$file/", -1) ;

        #
        # Update attempts
        #
        $attempts = 1 ;
        if ($db[3] >= 0) {

            $attempts = int( dbgetv(@db,'attempts') )+1 ;
            $attempts = 1 if dbgetv(@db,'status') !~ $status ;

        }

        fork_debug("$file | $dlsta | $start_file | $end_file "
            ."| $status | $attempts | $size | $speed | $md5")
            if $opt_v ;

        if ($db[3] >= 0) {
            dbputv(@db,
                "net",      $net,
                "sta",      $sta,
                "time",     $start_file,
                "endtime",  $end_file,
                "dir",      $path,
                "attempts", $attempts,
                "filebytes",$size,
                "bandwidth",$speed,
                "dlsta",    $dlsta,
                "fixed",    'n',
                "md5",      $md5,
                "dfile",    $file,
                "lddate",   now(),
                "status",   $status) ;
        } else {
            dbaddv(@db,
                "net",      $net,
                "sta",      $sta,
                "time",     $start_file,
                "endtime",  $end_file,
                "dir",      $path,
                "attempts", $attempts,
                "filebytes",$size,
                "bandwidth",$speed,
                "dlsta",    $dlsta,
                "fixed",    'n',
                "md5",      $md5,
                "dfile",    $file,
                "lddate",   now(),
                "status",   $status) ;
        }

        dbclose(@db) ;

        fork_debug("Next file") if $opt_d ;

    }

    dbunlock("${path}/${sta}_baler") ;

    unless ( scalar @total_downloads ) {
        fork_die("NO DOWNLOADS!!!! "
                    ."Station not downloading any files. "
                    ."http://$ip:$pf{http_port}");
    }

    delete $flagged{$_} foreach @total_downloads ;

    if ( scalar keys %flagged ) {
        fork_complain('Missing: '.join(' ',sort keys %flagged)) ;
    }

    #
    # Calc data downloaded
    #
    $total_size ||= 0 ;
    $k = sprintf("%0.1f",$total_size/1024) ;
    $m = sprintf("%0.1f",$k/1024) ;

    #
    # Calc the total time to rsync station
    #
    $run_time = now() - $start_sta ;
    $run_time_str = strtdelta($run_time) ;

    fork_notify( "$sta with ".@total_downloads." files "
        ."($m Mb) from $ip in $run_time_str") ;

    return ;

}

sub check_time {
    $start = shift ;

    #
    # Check if we are over the time limit
    #
    if ( $pf{max_child_run_time} ) {
        if ( int($pf{max_child_run_time}) < (now() - $start) ) {
            fork_complain("Rsync exceeds allowed time "
                ."set in max_child_run_time "
                ."($pf{max_child_run_time}).") ;
            #
            # Ran out of time. Return false.
            #
            return 0 ;
        }
    }

    #
    # Good to go. Return true.
    #
    return 1 ;

}

sub total_data_downloaded {
    my $sta  = shift ;
    my $days = shift || 1 ;
    my ($start,$r) ;
    my (@db_temp,@db,@dbscr,@recs) ;
    my $total_bytes = 0.0 ;

    #
    # Verify Database
    #
    fork_notify("Get data downloaded in last $days for $sta")
        if $opt_d ;

    @db = open_db($sta) ;

    unless ( @db  ) {
        return 0;
    }

    eval { dbquery(@db,dbTABLE_PRESENT) ; } ;
    if ( $@ ) {
        fork_debug("$total_bytes bytes downloaded") if $opt_d ;
        dbclose(@db) ;
        return ;
    }

    if (dbquery(@db, dbRECORD_COUNT) < 1) {
        fork_debug("$total_bytes bytes downloaded") if $opt_d ;
        dbclose(@db) ;
        return ;
    }

    $start = str2epoch("-${days}days") ;
    @db= dbsubset ( @db,
        "status =~ /downloaded|error/ && time >= $start ") ;

    unless ( dbquery(@db, dbRECORD_COUNT) ){
        fork_debug("$total_bytes bytes downloaded") if $opt_d ;
        dbclose(@db) ;
        return ;
    }

    #
    # Build scratch record for matching.
    #
    @dbscr = dblookup(@db,0,0,0,"dbSCRATCH") ;
    dbputv(@dbscr,"status",'downloaded') ;
    @recs = dbmatches(@dbscr,@db,"stat","status") ;

    unless ( scalar @recs ){
        dbclose(@db) ;
        return ;
    }

    for $r ( @recs ) {
        $db[3] = $r ;
        $total_bytes += (dbgetv (@db, 'filebytes')) ;
    }

    dbclose(@db) ;

    fork_debug("$total_bytes downloaded") if $opt_d ;

    # for Kbytes
    $total_bytes = sprintf("%0.2f", $total_bytes/1024) ;
    # for Mbytes
    $total_bytes = sprintf("%0.2f", $total_bytes/1024) ;

    fork_debug("Total bytes downloaded: $total_bytes") if $opt_d ;

    return $total_bytes ;

}

sub download_file {
    my $file = shift ;
    my $path = shift ;
    my $ip = shift ;
    my ($file_fetch,$where) ;
    my @temp_new ;


    #foreach ( split(/[\s|\/]/,$file) ) {
    #    next if /^\/$/ ;
    #    push(@temp_new,$_) if /^\S+$/ ;
    #}

    #$file = join('/',@temp_new) ;

    fork_debug( "Build File::Fetch object: "
        ."http://$ip:$pf{http_port}/$file")
        if $opt_d ;

    eval{
        $file_fetch = File::Fetch->new(
            uri => "http://$ip:$pf{http_port}/$file") ;
    } ;
    fork_complain("File::Fetch -> $@") if $@ ;

    fork_complain("ERROR in build of File::Fetch -> "
        ."http://$ip:$pf{http_port}/$file")
        unless $file_fetch ;

    return unless $file_fetch ;

    fork_debug("Download: ".$file_fetch->uri) if $opt_d ;

    #eval {  $where = $file_fetch->fetch( to => "$path/" ) ; } ;
    #fork_complain("File::Fetch ".$file_fetch->uri." $@") if $@ ;

    $where = $file_fetch->fetch( to => "$path/" ) || '' ;

    #fork_complain(
    #   "ERROR on download of http://$ip:$pf{http_port}/$file")
    #   unless -f $where ;

    return unless -f $where ;

    fork_complain("Maybe there is a problem with the file. "
        ."$where from http://$ip:$pf{http_port}/$file")
        if -s $where == 591 ;

    #fork_complain("ERROR on $file after download. Problem with name")
    #   unless $where =~ /$temp_new[-1]/ ;

    #fork_complain("Cannot see $file after download") unless -f $where ;

    return $where if -f $where ;

    return

}

sub open_db {
    my $sta = shift ;
    my (@db,$path,$dbout) ;

    #
    # Prepare Folder Name
    #
    $path = prepare_path($sta) ;

    #
    # Fix path
    #
    $dbout = File::Spec->rel2abs( "${path}/${sta}_baler" ) ;

    fork_debug("Opening database ($dbout).") if $opt_d ;

    #
    # Create descriptor file if missing
    #
    unless ( -e $dbout) {
        fork_debug("$sta Creating new database ($dbout).") if $opt_d ;

        open FILE, ">", $dbout
            or fork_die("Could not create file [$dbout] :$!") ;

        print FILE "# Datascope Database Descriptor file\n\n" ;
        print FILE "schema css3.0\n" ;
        print FILE "dbpath $dbout\n" ;
        print FILE "dnblocks nfs\n" ;
        print FILE "ndbidserver anfops.ucsd.edu:2498\n\n" ;
        print FILE "Description &Literal{\n" ;
        print FILE "#\n" ;
        print FILE "# Baler44 archival database\n" ;
        print FILE "# Juan Reyes <reyes\@ucsd.edu>\n" ;
        print FILE "#\n" ;
        print FILE "}\n" ;

        close FILE ;

    }

    #
    # Open table
    #
    $dbout .= ".rsyncbaler" ;
    fork_debug("$sta Openning database table  ($dbout)") if $opt_d ;

    @db = dbopen_table($dbout,"r+") or 0;

    return @db if @db;

    fork_complain("Can't open DB: $dbout",$sta) ;
    return;

}

sub fix_local {
    my $sta = shift ;
    my $net = shift ;
    my $dlsta = shift ;
    my %list ;
    my %llist ;
    my ($r,$record,@db_sub,@db_r,@db,@dbscr,@recs) ;
    my ($unique,$file,$status,$extra,$size,$path,$f) ;
    my $nulls = 0 ;
    my ($mode,@fields) ;

    fork_debug("Reading and fixing of local directory") if $opt_d ;

    $path = prepare_path($sta) ;

    #
    # Clean database
    #
    @db = open_db($sta) ;
    unless ( @db  ) {
        fork_die("$sta Problems on db open!") ;
    }
    $r = dbquery(@db , dbRECORD_COUNT) ;
    for ($db[3]=0; $db[3] < $r; $db[3]++){
        $list{ dbgetv(@db,'dfile') } = 1 ;
    }

    #
    # Open new pointer to SCRATCH record
    #
    @dbscr = dblookup(@db,0,0,0,"dbSCRATCH") ;

    foreach $file (sort keys %list) {
        fork_debug("Clean table for dfile =~/$file/ ") if $opt_d ;

        #
        # Add file name to scratch record
        #
        dbputv(@dbscr,"dfile",$file) ;

        #
        # Search for all entries with the same filename
        #
        @recs = dbmatches(@dbscr,@db,"file","dfile") ;

        #
        # Force them to be in order
        #
        @recs = sort {$a <=> $b} @recs ;

        #
        # Make sure that we only have one entry for the file
        #
        if (scalar @recs > 1 ) {

            fork_complain("Lines (@recs) in database for $file") ;

            #
            # Removes the last value of the list
            #
            $r = pop(@recs) ;
            $db[3] = $r ;
            fork_complain("keep record $r: "
                .join(' ',dbgetv(@db,dbquery(@db,"dbTABLE_FIELDS")))
                ) ;
            $unique = $r ;

            #
            # Mark all extra rows with blank
            #
            foreach $r (@recs) {
                $db[3] = $r ;
                fork_complain( "remove record $r: "
                    .join(' ',dbgetv(@db,dbquery(@db,"dbTABLE_FIELDS" )))
                    ) ;
                dbmark(@db) unless $opt_n ;
                $nulls = 1 ;
            }


        } elsif (scalar @recs == 1) {

            #
            # We should only have one for each
            #
            $unique = pop(@recs) ;

        } else {

            #
            # It should never get here!
            #
            fork_die("Problem looking for file $file in "
                ."database. dbmatch returned (@recs)") ;

        }

        #
        # Set pointer to our record...
        #
        $db[3] = $unique ;

        #
        # Verify file name is for this station
        #
        if ( $file !~ /(${sta}|EXMP)/ or $file !~ /($pf{regex_for_files})/ ) {
            fork_complain("filename failed regex match "
                ."for station: remove(): $file") ;
            fork_complain("remove record $unique: "
                .join(' ',dbgetv(@db,dbquery(@db,"dbTABLE_FIELDS" )))
                ) ;
            dbmark(@db) unless $opt_n ;
            $nulls = 1 ;
            next ;
        }

        next unless dbgetv(@db,'status') =~ /downloaded/ ;

        unless ( -f "$path/$file"  ) {
            fork_complain("remove(not in directory): $file") ;
            dbmark(@db) unless $opt_n ;
            $nulls = 1 ;
        }

        #
        # Clean database fields
        #
        if (dbgetv(@db,'dir') ne $path ) {
            #
            # Fix "downloaded" entries on the database
            #
            dbputv(@db, "dir", $path,"lddate",dbgetv(@db,"lddate")) unless $opt_n ;
        }

        #
        # Verify mode 0664
        #
        $mode = (stat("$path/$file"))[2] || 0 ;
        $mode = sprintf("0%o", $mode & 07777) ;
        fork_complain("$file mode $mode=>0660") unless int($mode) >= 660 ;

        #
        # Fix size of file in database
        #
        unless ( $opt_n ) {
            dbputv(@db,
                "filebytes", -s "$path/$file",
                "lddate",dbgetv(@db,"lddate")
                ) unless (-s "$path/$file" == int(dbgetv(@db,"filebytes"))) ;
        }

    }


    #
    # Verify every file in the folder
    #
    opendir(DIR,$path) or fork_die("Failed to open $path: $!") ;

    while($f = readdir DIR) {
        next if -d "$path/$f" ;
        next if $f !~ /(${sta}|EXMP)/ ;
        next if $f !~ /($pf{regex_for_files})/ ;

        fork_debug("local file: $f") if $opt_d ;

        #
        # Add file name to scratch record
        #
        dbputv(@dbscr,"dfile",$f) unless $opt_n ;

        #
        # Search for all entries with the same filename
        #
        @recs = dbmatches(@dbscr,@db,"file","dfile") ;

        #
        # Force them to be in order
        #
        @recs = sort {$a <=> $b} @recs ;

        fork_debug("Got subset for enties for $f: (@recs)")
            if $opt_d ;

        if (scalar @recs == 1 ) {

            #
            # Verify the file is set to the proper status
            #
            $db[3] = $recs[0] ;

            if ( dbgetv(@db, 'status') =~ /downloaded/) {

                fork_debug("$f already in database as downloaded") if $opt_d ;

                #} elsif ( dbgetv(@db, 'status') =~ /skip/) {

                #fork_complain("$f already flagged as 'skipped'") ;

            } else {

                fork_complain("$f updated to 'downloaded'") ;
                dbputv(@db,
                    'status', 'downloaded',
                    'attempts', 1,
                    'time', now(),
                    'dir',$path,
                    'lddate', now()
                    ) unless $opt_n ;

             }

        } else {

            #
            # Add the missing file
            #
            $size = -s "$path/$f" || 0 ;
            fork_complain("$f adding as 'downloaded'") ;
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
                "status",   "downloaded"
                ) unless $opt_n ;

        }

    }

    #
    # Clean memory pointers and null entries
    #
    fork_complain("Crunch table.") if $nulls ;
    dbcrunch(@db) if $nulls and not $opt_n ;
    dbclose(@db) ;

    #
    # Close directory pointer
    #
    close(DIR) ;

    return ;

}

sub read_baler {
    my $sta   = shift ;
    my $ip    = shift ;
    my $dir   = shift ;
    my $media_active    = shift ;
    my $media_reserve    = shift ;
    my ($path,$name,$test,$nrecords) ;
    my %list ;
    my ($input,$list,$where,$files) ;
    my @temp_dir = () ;
    my (@temp,@db,@n,@queries) ;
    my $attempt = 1 ;

    $path = prepare_path($sta) ;

    $path .= '/lists/' ;

    fork_die("Cannot make directory ($path)") unless -d $path ;

    chdir $path or fork_die("Cannot go to directory ($path)") ;

    fork_debug("Full array for directories: @$dir") if $opt_d ;

    #
    # For each of the folders
    #
    foreach $list ( @$dir ) {

        #$list = "list.$dir.data.gz" ;

        next unless $list =~ /$pf{folder_with_files}/ ;

        fork_debug("Get: $list") if $opt_d ;

        unlink $list if -e $list ;
        #
        # download list of files now
        #
        eval{
            $file_fetch = File::Fetch->new(
                uri => "http://$ip:$pf{http_port}/$list") ;
        } ;
        fork_complain("File::Fetch($list) => $@") if $@ ;

        eval {
            $where = $file_fetch->fetch( to => "./" ) ;
        } ;
        fork_complain("File::Fetch(".$file_fetch->uri.") => $@") if $@ ;

        unless ( $where ) {
            eval {
                $where = $file_fetch->fetch( to => "./" ) ;
            } ;
            fork_complain("File::Fetch(".$file_fetch->uri.") => $@") if $@ ;
        }

        unless ( $where ) {
            eval {
                $where = $file_fetch->fetch( to => "./" ) ;
            } ;
            fork_complain("File::Fetch(".$file_fetch->uri.") => $@") if $@ ;
        }

        unless ( $where ) {
            fork_complain("Error fetching:  http://$ip:$pf{http_port}/$list") ;
            next ;
        }

        fork_complain("ERROR after download of: $list")
            unless -e $list ;

        open $input, "<$list" ;
        $files = new IO::Uncompress::AnyUncompress $input
            or fork_complain("IO::Uncompress::AnyUncompress "
                    ."failed: $AnyUncompressError") ;

        @temp = () ;
        while ( <$files> ) {
            s/\n// ;
            push(@temp,$_) ;
        }
        $input->close() ;
        $files->close() ;

        foreach $test ( @temp ) {
            #
            # Parse results and get size
            #
            @temp_dir = split(/\//,$test) ;
            $name = pop(@temp_dir) ;
            next unless  $name =~ /$pf{regex_for_files}/ ;
            next unless $name =~ /.*(${sta}|EXMP).*/ ;
            unshift(@temp_dir, $list =~ /active/ ? 'WDIR' : 'WDIR2' ) ;

            $list{$name} = join('/',@temp_dir) ;
            fork_debug("$name => $list{$name}") if $opt_d ;
        }

    }

    return %list ;

}

sub get_md5 {
    my $sta  = shift ;
    my $file  = shift ;
    my $ip    = shift ;
    my $lists    = shift ;
    my ($old,$md5_lib,$f,$d,$digest,$md5,$local_path,$folder) ;
    my ($where) ;
    my @md5_raw = () ;

    $local_path = prepare_path($sta) . '/md5/' ;

    $old = $file ;

    $file .= '.md5' unless $file =~ /\.md5/ ;

    fork_notify("Get MD5 $file") if $opt_d ;

    chdir $local_path
        or fork_die("Cannot change to directory ($local_path)") ;

    unless ( -s "$local_path/$file" ) {

        unlink "$local_path/$file" ;
        fork_notify("Lets download MD5 file $file -> $local_path")
            if $opt_d ;

        foreach $f (@$lists) {

            next unless $f =~ m/$pf{md5_folder}/ ;
            fork_notify("Now with directory $f") if $opt_d ;
            $d = ( $f =~ /active/ ? 'WDIR' : 'WDIR2' ) ;
            $f =~ m/list\.\w+\.($pf{md5_folder}\S*)\.gz/ ;
            next unless $1 ;

            fork_notify("attempt download of MD5: $d/$1/$file")
                if $opt_d ;

            $where = download_file("$d/$1/$file",$local_path,$ip) ;
            last if $where ;
            $where = download_file("$d/$1/$file",$local_path,$ip) ;
            last if $where ;

        }

    }

    fork_complain("Error downloading: $file")
        unless -e "$local_path/$file" ;

    return 'missing' unless -e "$local_path/$file" ;

    open(DAT, '<', "$local_path/$file")
        or fork_die("Cannot open $local_path/$file!") ;

    while (<DAT>) {
        chomp ;
        fork_debug("MD5 file:: $_") if $opt_v ;
        push @md5_raw, split ;
    }

    close(DAT) ;

    unless ( scalar(@md5_raw) ) {

        fork_complain("Error in scalar value of "
            ."md5 in file $local_path/$file @md5_raw") ;

        move("$local_path/$file",
            "$local_path/../trash/$file")
            or fork_complain("Can't move $file to $local_path/../trash") ;

        #unlink "$local_path/$file" ;
        return 'error' ;

    }


    if ( $md5_raw[0] !~ /(\S{32})/ ) {

        fork_complain("Error in md5 regex for value in "
            ."file $local_path/$file @md5_raw") ;

        move("$local_path/$file",
            "$local_path/../trash/$file")
            or fork_complain("Can't move $file to $local_path/../trash") ;

        #unlink "$local_path/$file" ;
        return 'error' ;

    }

    if ( $md5_raw[1] !~ /($old)/ ) {

        fork_complain("Error in md5 in filename in "
            ."file $local_path/$file @md5_raw") ;

        move("$local_path/$file",
            "$local_path/../trash/$file")
            or fork_complain("Can't move $file to $local_path/../trash") ;

        #unlink "$local_path/$file" ;
        return 'error' ;

    }

    $md5 = $md5_raw[0] ;


    #
    # Open file and get local md5
    #
    $file  =~ s/\.md5//g ;
    $local_path = prepare_path($sta) ;

    open(FILE,"$local_path/$file")
        or fork_die("Cannot open $local_path/$file for md5 calc.") ;

    $md5_lib = Digest::MD5->new ;
    $md5_lib->addfile(FILE) ;
    $digest = $md5_lib->hexdigest || 0 ;
    close FILE ;

    fork_complain("Cannot produce MD5 for ($file)")
        unless $digest ;

    return $md5 if $digest eq $md5 ;

    fork_complain("$file MD5 problem:: reported:$md5 calc:$digest ") ;

    return 'error' ;

}

sub get_medias_and_lists {
    my $sta = shift ;
    my $ip = shift ;
    my (@text,$line,$browser, $resp) ;
    my $active = '' ;
    my $reserve = '' ;
    my @dir = () ;

    $browser = LWP::UserAgent->new ;

    $resp = $browser->timeout(120) ;

    fork_debug("$sta:\tLWP::UserAgent->get("
        ."http://$ip:$pf{http_port}/stats.html)")
        if $opt_d ;

    $resp = $browser->get("http://$ip:$pf{http_port}/stats.html") ;

    unless ( $resp->is_success ) {

        fork_debug("2nd time.... $sta:"
            ."\tLWP::UserAgent->get("
            ."http://$ip:$pf{http_port}/stats.html)")
            if $opt_d ;

        $resp = $browser->get("http://$ip:$pf{http_port}/stats.html") ;

        fork_complain("Missing http://$ip:$pf{http_port}/stats.html")
            unless $resp ;

        return unless $resp ;
    }

    if ( $resp->is_success ) {
        @text =  split '\n', $resp->content ;

        for ($line=0; $line < scalar @text; $line++){

            $text[$line] =~ m/MEDIA site \d crc=(\S+) IN USE/ ;
            $active = $1 ;
            last if $active ;

        }

        for ($line=0; $line < scalar @text; $line++){

            $text[$line] =~ m/MEDIA site \d crc=(\S+) RESERVE/ ;
            $reserve = $1 ;
            last if $reserve ;

        }

        #
        # Look for file lists
        #
        for (my $line=0; $line < scalar @text; $line++){

            push(@dir,"$1") if
                $text[$line] =~ m/>(list\.(active|reserve)\.($pf{folder_with_files}|$pf{md5_folder}).*\.gz)</ ;

            next unless $1 ;
            fork_debug("Data Folder: $1") if $opt_v ;


        }
    }
    else {

        fork_complain("problem reading http://$ip:$pf{http_port}/stats.html") ;
        return ;

    }

    fork_complain("Cannot find MEDIA 1 in http://$ip:$pf{http_port}/stats.html")
        unless $active ;

    fork_complain("Cannot find MEDIA 2 in http://$ip:$pf{http_port}/stats.html")
        unless $reserve ;

    $active  ||= '' ;
    $reserve ||= '' ;

    fork_debug("get_medias_and_lists("
        ."http://$ip:$pf{http_port}/stats.html) "
        ."=> ($active,$reserve)")
        if $opt_v ;

    return ($active,$reserve,@dir) ;

}

sub prepare_path {
    my ($station,$status)  = @_ ;
    my $path = '' ;

    #
    # Default value for status is Active
    #
    $status ||= 'Active';

    fork_die("prepare_path(). Cannot produce path! "
        ."We need a station name...")
        unless $station ;

    $path = File::Spec->rel2abs( "$pf{local_data_dir}/$station" ) ;


    #
    # Exit here if this is a DECOM station.
    # Old stations are removed from the archive
    # and should not get recreated by this tool.
    #
    return $path unless $status eq 'Active' ;


    mkdir($path) unless -e $path ;
    fork_die("Cannot create folder $path") unless -e $path ;

    mkdir "$path/trash" unless -d "$path/trash" ;
    fork_die("Cannot make directory ($path/trash/)") unless -d "$path/trash" ;

    mkdir "$path/md5" unless -d "$path/md5" ;
    fork_die("Cannot make directory ($path/md5/)") unless -d "$path/md5" ;

    mkdir "$path/lists" unless -d "$path/lists" ;
    fork_die("Cannot make directory ($path/lists/)") unless -d "$path/lists" ;


    return $path ;

}

sub dblock { # $lock_status = &dblock ( $db, $lock_duration ) ;
    my ( $db, $lock_duration ) = @_ ;
    my ( $Pf, $dbloc_pf_file, $host, $pid ) ;
    my ( %pf ) ;

    chop ($host = `uname -n` ) ;
    $pid = $$ ;

    $Pf            = $db . "_LOCK" ;
    $dbloc_pf_file = $db . "_LOCK.pf" ;
    fork_debug ( "Pf    $Pf     dbloc_pf_file   "
        ."$dbloc_pf_file  pid $pid" )
        if $opt_V ;

    if ( ! -f $dbloc_pf_file ) {

        fork_debug (
            sprintf("$db new lock set to %s",
                strydtime ( now() + $lock_duration ))
            ) if $opt_V ;

        &write_dblock ( $dbloc_pf_file, $0,
            $host, $pid, &now(), &now() + $lock_duration ) ;

        return 0 ;

    } else {

        %pf = getparam( $Pf ) ;
        if ( $pf{unlock_time} > &now() && $pf{pid} != $pid ) {

            fork_complain (
                sprintf ("$db is locked until %s",
                    strydtime ( $pf{unlock_time} )
                    ) ) ;

                #prettyprint ( \%pf ) ;
            return 1 ;

        } elsif  ( $pf{unlock_time} > &now() && $pf{pid} == $pid ) {

            fork_debug (
                sprintf ("$db lock is extended to %s",
                    strydtime ( now() + $lock_duration )
                ) ) if $opt_V ;

            &write_dblock ( $dbloc_pf_file, $0,
                $host, $pid, $pf{lock_time},
                now() + $lock_duration ) ;

            %pf = () ;
            return 0 ;

        } else {

            fork_debug (
                sprintf ("$db lock set to %s",
                    strydtime ( now() + $lock_duration )
                ) ) if $opt_V ;

            &write_dblock ( $dbloc_pf_file, $0,
                $host, $pid, &now(), &now() + $lock_duration ) ;

            %pf = () ;
            return 0 ;

        }
    }

}

sub dbunlock { # $lock_status = &dbunlock ( $db ) ;
    my ( $db ) = @_ ;
    my ( $Pf, $dbloc_pf_file, $host, $host1 ) ;
    my ( $lock_time1, $pid, $pid1, $program1, $unlock_time1 ) ;
    my ( %pf ) ;

    chop ($host = `uname -n` ) ;
    $pid = $$ ;

    $Pf            = $db . "_LOCK" ;
    $dbloc_pf_file = $db . "_LOCK.pf" ;
    fork_debug ( "Pf    $Pf     "
        ."dbloc_pf_file   $dbloc_pf_file" ) if $opt_V ;

    if ( ! -f $dbloc_pf_file ) {

        fork_complain ( "dbunlock:      $dbloc_pf_file does not exist!" ) ;
        return 1 ;

    } else {

        pfupdate ( $Pf ) ;
        %pf = getparam( $Pf ) ;
        if ($0 ne $pf{program} || $pid != int($pf{pid}) || $host ne $pf{host}) {

            fork_complain ( "unable to unlock $db" ) ;
            fork_complain ( "program    $0      $pf{program}" ) ;
            fork_complain ( "pid        $pid    $pf{pid}" ) ;
            fork_complain ( "host       $host   $pf{host}" ) ;
            return 1 ;

        }
        if ( $pf{unlock_time} < &now() ) {

            fork_complain (
                sprintf ("$db was already unlocked at %s",
                    strydtime ( $pf{unlock_time} )
                ) ) ;
            return 1 ;

        }

        &write_dblock ( $dbloc_pf_file, $0,
            $host, $pid, $pf{lock_time}, &now() ) ;

        return 0 ;
    }

}

sub write_dblock {
    # &write_dblock ( $dbloc_pf_file, $program,
    #       $host, $pid, $lock_time, $unlock_time ) ;
    my ( $dbloc_pf_file, $program, $host,
        $pid, $lock_time, $unlock_time ) = @_ ;
    open( LOCK,   ">$dbloc_pf_file" ) ;
    print LOCK    "program      $program\n" ;
    print LOCK    "host         $host\n" ;
    print LOCK    "pid          $pid\n" ;
    printf ( LOCK "lock_time    %d\n", $lock_time ) ;
    printf ( LOCK "unlock_time  %d\n", $unlock_time ) ;
    close LOCK ;
    return ;

}

sub fork_log { # &fork_log ( $parent, $line ) ;
    my $line = shift;

    unless ( $parent ) {
        print STDOUT "[LOG:$line]\n";
        return;
    }

    elog_log( $line );

    return;
}

sub fork_notify { # &fork_notify ( $parent, $line ) ;
    my $line = shift;

    unless ( $parent ) {
        print STDOUT "[NOTIFY:$line]\n";
        return;
    }

    elog_notify( $line );

    return;
}

sub fork_debug { # &fork_debug ( $parent, $line ) ;
    my $line = shift;

    unless ( $parent ) {
        print STDOUT "[DEBUG:$line]\n";
        return;
    }

    elog_debug( $line );

    return;
}

sub fork_complain { # &fork_complain ( $parent, $line ) ;
    my $line = shift;

    unless ( $parent ) {
        print STDOUT "[PROBLEM:$line]\n" ;
        return;
    }

    elog_complain( $line );

    return  ;
}

sub fork_die { # &fork_die ( $parent, $line ) ;
    my $line = shift;

    if ( $parent ) {

        elog_die( $line ) ;

    } else {

        fork_complain( $line );
        fork_notify("finished processing station") ;
        exit ;

    }
}

__END__
#{{{
=pod

=head1 NAME

rsync_baler - Sync a remote baler directory to a local copy

=head1 SYNOPSIS

rsync_baler [-h] [-x] [-v] [-d] [-s sta_regex] [-r sta_regex] [-p pf] [-m email,email]

=head1 ARGUMENTS

Recognized flags:

=over 2

=item B<-h>

Help. Produce this documentation

=item B<-x>

Ignore all database flags for max attempts on files.

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
The script will fail if used outside ANF-TA installation.
Depends on tables [deployment, stabaler, staq330].

=head1 AUTHOR

Juan C. Reyes <reyes@ucsd.edu>

=head1 SEE ALSO

Perl(1).

=cut
#}}}
