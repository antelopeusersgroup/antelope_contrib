#
#  *** No BRTT support ***
#
#   rsync_baler: script to create a local copy of remote baler
#
#  The software will connect to any active baler44 station and will
#  download all missing miniseed files on the local archive. The
#  code will get the list of stations and the ips from a web API
#  that we populate from a database query to a join on several
#  tables [deployment, stabaler, staq330] 
#
#  The process will clean all databases and then subset for
#  active stations only. Each of those will start with a ping to each
#  station for connectivity and then will download a compressed list
#  of files present on the remote media. The list is correlated with
#  the local list of files and any missing file is flagged for
#  download. Each file should  have a md5 checksum present on the
#  remote baler. The checksum is also downloaded and verified with
#  the locally calculated checksum. Each step of the process is
#  archived on a rsyncbaler table in the local directory.
#
#   author: Juan C. Reyes
#   email:  reyes@ucsd.edu
#

# PERL PRAGMAS
# we want to create variables on the fly. Cannot use strict subs.
#use strict "subs" ;
use strict "vars" ;
use warnings ;

# Need to include some files from Antelope
use lib "$ENV{ANTELOPE}/contrib/data/perl" ;

use LWP ;
use Fcntl ;
use POSIX ;
use archive ;
use subnetMatch ;
use utilfunct qw[getparam check_procs];
use sysinfo ;
use Net::Ping ;
use Datascope ;
use Pod::Usage ;
use IO::Handle ;
use File::Spec ;
use File::Copy qw[move];
use Getopt::Std ;
use LWP::Simple ;
use File::Fetch ;
use JSON::PP;
use Digest::MD5 qw[md5_hex] ;
use IO::Uncompress::AnyUncompress qw[anyuncompress $AnyUncompressError] ;


our(%pf) ;
our(%logs,%errors) ;
our($to_parent,$nstas,$get_sta,$parent,$host) ;
our($problems,$nchild,$file_fetch) ;
our($force_include,$avoid_ips,$subject,$start,$end,$run_time_str) ;
our($opt_n,$opt_x,$opt_r,$opt_s,$opt_h,$opt_w,$opt_v,$opt_m,$opt_p,$opt_d) ;


use constant false => 0 ;
use constant true  => 1 ;


#
# Set umask for process
#
umask 002 ;


# Helpful on the non-block reads from threads
STDOUT->autoflush(1) ;


$ENV{'ELOG_MAXMSG'} = 0 ;
$ENV{'ELOG_TAG'} = 'rsync_baler *%s*' ;
$parent = $$ ;
$start = now() ;
$host = my_hostname() ;

elog_init($0,@ARGV) ;



# Command line arguments
unless ( &getopts('nxhdvm:p:s:r:') || @ARGV > 1 ) {
    pod2usage({-exitval => 99, -verbose => 2}) ;
}


#
# Print help and exit
#
pod2usage({-exitval => 9, -verbose => 2}) if $opt_h ;


#
# Implicit flags
#
# rewrite opt_v with opt_w to avoid printing getparam() logs
# it's unfortunate that the library got written with global
# variables hard-coded in the logic.
$opt_w = $opt_d ? $opt_d : $opt_v ;  
$opt_v = $opt_d ; # unless we are in debug, then we let the lib do it's logging


# Some feedback to the user that we are working
fork_log("$0 @ARGV") ;


#
# If we want to fork the process this will take
# the argument  for the station that needs processing.
# If this is the mater process then this will be NULL.
#
$get_sta = $ARGV[0] || '' ;


#
# Get parameters from config file (PF file)
#
$opt_p ||= "rsync_baler.pf" ;
%pf = getparam($opt_p) ;


#
# Initialize  mail
#
if ($opt_m){
    savemail() ;
    fork_log("Initialize mail") ;
}


if ( $get_sta ) {
    # run as child...
    $parent = 0 ;
    fork_log("Starting $get_sta at ".strydtime($start)." on $host") ;
    fork_debug("Running as child process for station: $get_sta") ;

} else {
    fork_notify("Starting at ".strydtime($start)." on $host") ;
    # run as parent (daemon mode)...
    fork_debug("Running as parent process.\n") ;
}


#
# Verify reject station subset
# Note:
#   If we have a reject subset from command-line
#   then we use that string. If we don't then
#   we look into the parameter file for a possible
#   definition set on days of the week. This is
#   related to a different process that we run
#   over the files and parses the data. We want
#   to avoid access collision with that process.
#   If you just want the data then you can avoid this.
#
if ( $pf{avoid_on_day_of_week} ) {
    $opt_r ||= $pf{avoid_on_day_of_week}{epoch2str( now(), "%A" )} ;
}


#
# Load reject IPS from parameter file.
# Should be fine with NULL input from the PF file.
#
$avoid_ips = subnet_match( @{$pf{avoid_ips}} ) ;
$force_include = subnet_match( @{$pf{force_include}} ) ;



#
# Configure object used to download files from the sites. This
# part will specify the system calls deep down on the library
# and can be useful for troubleshooting strange performance
# issues. We also try to match the verbosity to the one we
# got from the user for this process.
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
# Verify metadata URL
# We don't query the dbmaster directly anymore so we need
# a valid URL to get our information from the API. At this
# time we have a NoSQL server delivering JSON data but this
# will be trivial to modify.
#
fork_die("Can't work without (JSON) url pf[json_url] => $pf{json_url}")
    unless $pf{json_url} ;



#
# ************************
# **    MAIN LOOP       **
# ************************
# Now we start with the actual processing and work.
# Two options here...
# 1) Parent process loop for thread spawning.
# 2) Child process function call to get data.
#

#
# CHILD PROCESS...  Get data from the stations
#
if ( $get_sta ) {
    get_data($get_sta) ; # CHILD PROCESS WILL END HERE!!!!
    exit 0;
}


###########################################
# ONLY PARENT PROC SHOULD CONTINUE!!!!
###########################################

$nstas = run_in_threads() ;


#
# Print error logs
#
( $nchild, $problems ) = &problem_print ( \%errors ) ;

#
# Print logs
#
&log_print ( \%logs ) ;


fork_log("started at ".strydtime($start)." on $host") ;
fork_log ("completed at ".strydtime(now())." \n\n" ) ;

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
sub get_json {
    # Get informaiton from URL in JSON format
    my $url = shift ;

    my $json        = JSON::PP->new->utf8 ;

    fork_log("Get URL: $url");

    my $network = LWP::UserAgent->new ;
    $network->timeout( 120 ) ;
    my $resp = $network->get( $url ) ;

    fork_die( "No response from server for $url" ) unless ( $resp->is_success ) ;

    return $json->decode( $resp->content ) ;
}

sub get_stations_from_url {
    my ($ip,$dlsta,$net,$sta) ;
    my %sta_hash ;

    my $avoid_today = $pf{avoid_on_day_of_week}{epoch2str( now(), "%A" )} || '';

    my $json_data = get_json( $pf{json_url} ) ;

    for my $data_hash ( @$json_data ) {

        my $sta = $data_hash->{'sta'};

        fork_debug( "Test response: $sta" ) ;

        # Filter out station if needed
        next if $opt_s and $sta !~ /$opt_s/ ;
        next if $opt_r and $sta =~ /$opt_r/ ;
        next if $avoid_today and $sta =~ /$avoid_today/ ;

        $sta_hash{$sta}{'dlsta'} = $data_hash->{'id'};
        $sta_hash{$sta}{'snet'} = $data_hash->{'snet'};
        $sta_hash{$sta}{'sta'} = $data_hash->{'sta'};

        fork_log( "Add $sta_hash{$sta}{'dlsta'} to list." ) ;


    }

    fork_die("NO STATIONS SELECTED FROM DATABASE.") unless %sta_hash ;

    return %sta_hash ;
}


sub get_info_for_sta {
    # We got a list of sites on the parent process. Now we need
    # to get details on this particular station that we are processing
    # in a different thread.

    my $sta = shift ;
    my ($ip,$dlsta,$net) ;
    my %sta_hash ;

    my $url  = "$pf{json_url}&sta=$sta" ;
    my $json_data = get_json( $url ) ;

    for my $data_hash ( @$json_data ) {

        fork_log( "Got metadata for station: $data_hash->{id}" ) ;

        next if $data_hash->{sta} !~ /$sta/ ;

        $sta_hash{dlsta} = $data_hash->{'id'};
        $sta_hash{net} = $data_hash->{'snet'};
        $sta_hash{snet} = $data_hash->{'snet'};
        $sta_hash{sta} = $data_hash->{'sta'};
        $sta_hash{time} = $data_hash->{'time'};
        $sta_hash{endtime} = $data_hash->{'endtime'};
        $sta_hash{ip} = 0 ;

        $sta_hash{status} = 'Decom' ;
        if ($sta_hash{endtime} eq '-') {
            $sta_hash{status} = 'Active' ;
        }


        if ( $data_hash->{'orbcomms'} ) {
            $sta_hash{ip} = $data_hash->{'orbcomms'}->{'inp'};
            fork_debug( "\tip: $sta_hash{ip}" ) ;

            #
            # Use this regex to clean the IP string...
            #
            if ( $sta_hash{ip} =~ /([\d]{1,3}\.[\d]{1,3}\.[\d]{1,3}\.[\d]{1,3})/ ) {
                $sta_hash{ip} = $1 ;
            }
            else {
                fork_complain("Failed grep on IP (ip'$sta_hash{ip}',dlsta'$sta_hash{dlsta}')") ;
                $sta_hash{ip} = 0 ;
            }

        }

        fork_log( "\ttime: $sta_hash{time}" ) ;
        fork_log( "\tendtime: $sta_hash{endtime}" ) ;
        fork_log( "\tstatus: $sta_hash{status}" ) ;
        fork_log( "\tip: $sta_hash{ip}" ) ;


    }



    #
    # Verify if IP is in range of restiction list
    #
    # Some sites need a different configuration for the download
    # of the files. Most likely because the low bandwidth satlinks.
    # If we match then we should stop here.
    # There is a way to keep a sub-group of sites within the bigger
    # rejected group. Just make sure that you set the correct netmasks
    # on the parameter file.
    if ( $sta_hash{ip} && $avoid_ips->($sta_hash{ip}) ) {
        unless ( $force_include->($sta_hash{ip}) ) {
            fork_notify("\t$sta_hash{dlsta} $sta_hash{ip} matches entry in AVOID IP LIST')") ;
            exit ;
        }
    }

    return %sta_hash ;
}

sub run_in_threads {
    my @procs ;
    my $station ;
    my $pid ;
    my $cmd ;
    my $max_out = $pf{max_procs} ;
    #my %archive = get_stations_from_db() ;
    my %archive = get_stations_from_url() ;

    $nstas = scalar keys %archive ;

    STATION: foreach $station (sort keys %archive) {

        my $sta = $archive{$station}{'sta'} ;
        fork_debug("run_in_threads($sta)") ;

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
        nonblock_read(\%archive,\%logs,\%errors) ;

        #
        # Stop if we are at max procs
        #
        redo STATION if scalar(@procs) >= $max_out ;


        #
        # Fork the script (not using &fork)
        #
        $cmd   =  "$0 " ;
        $cmd  .=  "-n " if $opt_n ;
        $cmd  .=  "-x " if $opt_x ;
        $cmd  .=  "-v " if $opt_w ;
        $cmd  .=  "-d " if $opt_d ;
        $cmd  .=  "-p $opt_p " if $opt_p ;
        $cmd  .=  "$sta " ;


        fork_debug("Now: ".@procs." procs") ;
        fork_log("Starting $sta: $cmd ") ;

        ##################
        #   OLD METHOD   #
        ##################
        #pipe($archive{$station}{fh}, $to_parent)
        #   or fork_die('Cannot create pipe()') ;
        #$pid = fork() ;
        #exec( $cmd ) unless $pid ;

        undef $pid ;
        $pid = open( $archive{$station}{fh}, "-|" ) ;

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

            fork_debug("Got pid $pid for $sta") ;
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

    fork_log("Initiated all stations." ) ;

    #
    # wait for all children to finish
    #
    #nonblock_read(\%archive,\%logs,\%errors) while check_procs(@procs) ;
    while ( 1 ){

        nonblock_read(\%archive,\%logs,\%errors) ;

        last unless scalar check_procs(@procs) ;

        #
        # throttle the reading engine
        #
        sleep (5) ;

    }
    #while (<$to_parent>) { fork_notify($_) ; }

    fork_log("All stations completed." ) ;

    return $nstas ;

}

sub problem_print { # ( $nchild, $problems ) = &problem_print ( \%errors ) ;
    my $errors = shift ;
    my ( @total ) ;
    my ( $nchild, $nerr, $nprob ) ;

    $nchild = $nerr = $nprob = 0 ;


    elog_complain('') ;
    elog_complain('') ;
    elog_complain("-------- Problems: --------") ;
    elog_complain('') ;


    for my $k ( sort keys %$errors) {

        next if ( $errors->{$k}->{problems} == 0 ) ;
        $nchild++ ;
        $nprob++ ;
        elog_complain("   $k:") ;

        @total = () ;
        for my $j ( keys %{$errors->{$k}} ) {
            next if ( $j =~ /problems/ ) ;
            push( @total, int($j) ) ;
        }

        for my $j ( sort {$a <=> $b} @total ) {
            elog_complain("   $j) $errors->{$k}->{$j}") ;
            $nerr++ ;
        }

        elog_complain('') ;
    }

    elog_complain("No problems in script.") unless $nprob ;

    elog_complain("-------- End of problems: --------") ;
    elog_complain('') ;

    return ( $nchild, $nerr ) ;
}

sub log_print { # &log_print ( \%logs ) ;
    my $logs = shift ;
    my ( @total ) ;

    elog_log('') ;
    elog_log('') ;
    elog_log("-------- Logs: --------") ;
    elog_log('') ;

    for my $k ( sort keys %$logs) {

        elog_log("On child $k:") ;

        @total = () ;
        for my $j ( keys %{$logs->{$k}} ) {
            next if ( $j =~ /lines/ ) ;
            push( @total, int($j) ) ;
        }

        for my $j ( sort {$a <=> $b} @total ) {
            elog_log("   $j) $logs->{$k}->{$j}") ;
        }

        elog_log('') ;
    }

    elog_log("-------- End of logs: --------") ;
    elog_log('') ;
}

sub nonblock_read { # &nonblock_read ( \%stas, \%logs, \%errors ) ;
    my ( $stas, $logs, $errors ) = @_ ;
    my ( $fh, $fileline, $line )  ;

    fork_debug('nonblock_read()') ;
    foreach my $sta (sort keys %$stas) {
        fork_debug("nonblock_read($sta)") ;

        next unless $fh = $stas->{$sta}->{fh} ;
        fork_debug( $parent, "nonblock_read $sta    $stas->{$sta}->{fh}" );

        while ( $fileline = <$fh> ) {

            while ( $fileline =~ /\[LOG:(.*?)\]$/g )     {
                $line = $1 ;
                $logs->{$sta}->{lines}++ ;
                $logs->{$sta}->{ $logs->{$sta}->{lines} } = $line ;
                fork_debug( "$sta - $1" ) ;
            }
            while ( $fileline =~ /\[NOTIFY:(.*?)\]$/g )     {
                $line = $1 ;
                $logs->{$sta}->{lines}++ ;
                $logs->{$sta}->{ $logs->{$sta}->{lines} } = $line ;
                fork_debug( "$sta - $1" ) ;
            }
            while ( $fileline =~ /\[DEBUG:(.*?)\]$/g )     {
                $line = $1 ;
                $logs->{$sta}->{lines}++ ;
                $logs->{$sta}->{ $logs->{$sta}->{lines} } = $line ;
                fork_debug( "$sta - $1" ) ;
            }
            while ( $fileline =~ /\[PROBLEM:(.*?)\]$/g )     {
                $line = $1 ;
                $logs->{$sta}->{lines}++ ;
                $logs->{$sta}->{ $logs->{$sta}->{lines} } = $line ;
                $errors->{$sta}->{problems}++ ;
                $errors->{$sta}->{ $errors->{$sta}->{problems} } = $line ;
                fork_complain( "$sta - $1" ) ;
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

    #
    # CHILD PROCESS ONLY...
    # We start by cleaning the databases first. Verify that
    # all the files on disk are listed on the database and that
    # every entry on the database is present on disk or correctly
    # tagged as missing. If the station is too old then we are going
    # to kill the process early. This is done by looking at the
    # "endtime" of the station.
    #

    my $sta = shift ;

    fork_die("No value for station in child.") unless $sta ;

    my (%active_media_files,$media,@rem_file) ;
    my (%remote,$size,@temp_download,@dbwr,@dbr,@dbr_sub) ;
    my (%clean,$local_path_file,%avoid,$replace,$file,$speed,$run_time) ;
    my ($bytes,$d_data,$start_file,$where,$attempts,@missing) ;
    my ($p,$rem_s,$loc_s,@diff,$results,$run_time_str,$dbout) ;
    my ($msdtime,$end_file,$record,$time,$dir,$dfile) ;
    my ($k,$m,$g,$total_size,%temp_hash,@total_downloads,@download) ;
    my ($digest,$hexd,$md5,$remote_file_content, $remote_file_handle) ;
    my ($stat,$mode,$f,$http_folder,$md5_lib,@original_downloads) ;
    my (@lists,@dbscr,@recs,@data,@db_r,%remove,%flagged,@db,@db_t) ;
    my (@download_list, $media_active, $media_reserve,%downloaded) ;
    my ($mlimit, $days);
    my ($start_of_report) ;

    my %table = get_info_for_sta($sta) ;

    #
    # Prepare Variables and Folders
    #
    my $type    = '' ;
    my $resp    = 0 ;
    my $ip     = $table{ip} or 0;
    my $dlsta  = $table{dlsta} ;
    my $net    = $table{net} ;
    my $status = $table{status} ;
    my $endtime = $table{endtime} ;
    my $path = prepare_path($sta,$status) ;
    my $start_sta = now() ;


    #
    # Stop if the enditme is larger than the
    # value limit_innactive_age in parameter
    # file.
    #
    if ( $pf{limit_innactive_age}  and int($endtime) > 0) {
        if ( $pf{limit_innactive_age} < (now() - $endtime)  ) {
            fork_notify("STOP $sta, more than $pf{limit_innactive_age} seconds old.") ;
            exit ;
        }
    }

    #
    # Stop if this is a DECOM station
    # and the folder is not present
    # in the archive.
    #
    unless ( -e $path ) {
        fork_notify("STOP $sta, DECOMMISSIONED and no folder.") ;
        exit ;
    }

    #
    # Try to lock baler database.
    #
    if ( dblock("${path}/${sta}_baler",$pf{max_child_run_time}) ) {
        fork_die("Cannot lock database ${path}/${sta}_baler") ;
    }


    #
    # Read local files and fix errors on database
    #
    fix_local( $sta,$net,$dlsta ) ;


    #
    # For DECOM ( non-Active so we catch all options ) stations stop here
    #
    unless ( $status eq 'Active') {
        dbunlock("${path}/${sta}_baler") ;
        fork_notify("$sta is under DECOMMISSION status.") ;
        exit ;
    }

    #
    # No more to do in this case.
    #
    unless ( $ip ) {
        dbunlock("${path}/${sta}_baler") ;
        fork_die("$sta has no IP") ;
    }

    #
    # Limit the downloads to some Megabytes in some days
    #
    while ( ($days, $mlimit) = each $pf{bandwidth_limits} ) {
        $d_data = total_data_downloaded($sta,$days) || 0.0 ;
        fork_log("$sta downloaded $d_data Mbyts in last $days days.") ;
        if ( $d_data > $mlimit ) {
            dbunlock("${path}/${sta}_baler") ;
            fork_die("$sta downloaded ( $d_data ) Mbts in the last $days days.") ;
        }

    }

    #
    # Ping the station
    #
    $p = Net::Ping->new("tcp", 5) ;
    $p->port_number($pf{http_port}) ;
    $record=0 ;

    # Need the infinite loop so you can easily control this from
    # the parameter file. There are multiple ways on how to do this...
    while ( 1 ) {
        $record++;

        last if $p->ping($ip) ;

        sleep $pf{connect_pause} ;

        if ( $record == $pf{max_attempts} ) {
            dbunlock("${path}/${sta}_baler") ;
            fork_die("$sta on http://$ip:$pf{http_port} NOT RESPONDING!") ;
        }
    }
    fork_log("$sta responded after $record attempts and $pf{connect_pause} secs wait time." )
                if $record > 1 ;

    # Destroy that Net object
    undef($p) ;


    #
    # Get medias ID's
    # Read that status page on the baler and scrape the values from the text
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
            fork_log("$dfile status set to skip") ;
            next LINE ;
        }

        if ( $msdtime > 1 ) {
            # file already processed
            fork_debug("$dfile already processed msdtime=>$msdtime") ;
            next LINE ;
        }

        # Maybe we want to avoid old files.
        # We don't get the wfdisc present on the Baler medias
        # with some of the metadata for the files but the names
        # of the files are a great way to extract the most important
        # part... the age of the file.
        if ( yesno( $pf{limit_history} ) ) {

            # Match name to get time of data.
            $dfile =~ m/^\S*-(\d{4})(\d{2})(\d{2})(\d{6})$/ ;
            fork_debug( "$dfile date is $2/$3/$1" );

            if ( $1 and $2 and $3 ) {
                if ( (now() - str2epoch("$2/$3/$1")) > int($pf{limit_history}) ) {
                    fork_debug( "$dfile is too old: " . strtdelta(now() - str2epoch("$2/$3/$1")) );
                    next LINE;
                }

            }

        }


        if ($attempts > 5 ) {
            # Too many attempts, or add file
            # if we are using the opt_x flag at runtime
            if ( $opt_x ) {

                fork_notify("$dfile will NOT be rejected based on -x flag") ;

            } else {

                fork_log("$dfile *AVOID - MAX ATTEMPTS* attempts=>$attempts status=>$stat md5=$md5") ;
                next LINE ;

            }

        }



        #
        # Make list of files to download from Baler
        #
        if ($stat !~ /downloaded/) {
            #
            # We already have an entry that IS NOT downloaded.
            # Get file one more time.
            #
            fork_log("$dfile listed in database but not done") ;

            $flagged{$dfile} = '' ;
            next LINE ;

        }


        #
        # This is a typical filie with an error
        # on the download that creates a html
        # only file with some logs in it.
        # ALWAYS SAME SIZE OF 591
        #
        if ( -s "$path/$dfile" == 591 ) {
            fork_complain("$dfile error in file size == 591") ;
            fork_complain("$dfile add to download list. From-DB") ;
            $flagged{$dfile} = '' ;
            next LINE ;
        }


        #
        # Use/Avoid md5 verification
        #
        if ( yesno( $pf{use_md5} ) ) {

            #
            # Verify for valid checksum
            #
            if ($md5 =~ /(\S{32})/ or $md5 =~ /ignore|skip/ ) {

                if ( $opt_d ) {
                    fork_debug("$dfile verified md5=>$md5") ;
                }
                next LINE ;

            }

            fork_log("$dfile fixing md5=>$md5") ;

            #
            # If missing checksum then connect to station
            #

            if ( $ip and not $opt_n) {

                fork_debug("Get md5 for $dfile") ;

                # Keep track of attemtps.
                dbputv(@db,'attempts',int(dbgetv(@db,'attempts'))+1,"lddate",dbgetv(@db,"lddate")) ;

                # Get the md5 file
                dbputv(@db, "md5", get_md5($sta,$dfile,$ip,\@lists),
                        "lddate",dbgetv(@db,"lddate") ) ;

                # Verify.
                $md5 = dbgetv(@db,'md5') ;

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

            fork_complain("$dfile Problem with md5. Add to download list.") ;
            $flagged{$dfile} = '' ;

        }


    }


    dbclose(@db) ;

    fork_log("From-DB: $_") foreach ( sort keys %flagged ) ;

    # Done with the local database.

    # Now read the Baler...
    %remote = read_baler( $sta, $ip, \@lists, $media_active, $media_reserve) ;

    unless ( keys %remote ) {
        dbunlock("${path}/${sta}_baler") ;
        fork_die("Can't get list of files: $ip:$pf{http_port})") ;
    }

    # There is a parameter to set the max amount of time
    # that we have for each process. Verify this now.
    unless ( check_time($start_sta) ) {
        dbunlock("${path}/${sta}_baler") ;
        fork_die("No more time to complete the downloads. EXIT!!!!") ;
    }


    #
    # Compare remote list to local archive
    #
    @db = open_db($sta) ;
    unless ( @db  ) {
        dbunlock("${path}/${sta}_baler") ;
        fork_die("$sta Problems on db open!") ;
    }
    foreach $f ( sort keys %remote ) {
        # Don't verify checksum files
        next if $f =~ /\.md5/ ;

        # Maybe we want to avoid old files.
        if ( yesno( $pf{limit_history} ) ) {

            # Match name to get time of data.
            $f =~ m/^\S*-(\d{4})(\d{2})(\d{2})(\d{6})$/ ;
            fork_debug( "$f date is $2/$3/$1" );

            if ( $1 and $2 and $3 ) {
                if ( (now() - str2epoch("$2/$3/$1")) > int($pf{limit_history}) ) {
                    fork_log( "$f is too old: " . strtdelta(now() - str2epoch("$2/$3/$1")) );
                    next;
                }

            }

        }

        #
        # Check if we have the file
        #
        $db[3] = dbfind(@db, "dfile =~ /$f/", -1) ;

        if ($db[3] >= 0 ){

            # Skip file if "downloaded", "skip" or "avoid" flags
            next if dbgetv(@db,'status') =~ /downloaded/ ;
            next if dbgetv(@db,'status') =~ /avoid/ ;
            next if dbgetv(@db,'status') =~ /skip/ ;

            # Don't do anything is we got to 5 attempts
            unless ( $opt_x ) {
                next if dbgetv(@db,'attempts') > 5 ;
            }

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

        } else {


            fork_log("Add to flagged list. From-Baler: $f") ;
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


    dbclose(@db) ;


    unless (keys %flagged) {
        fork_log("No new files. http://$ip:$pf{http_port}") ;
        dbunlock("${path}/${sta}_baler") ;
        return ;
    }


    #
    # Download the missing files. A simple natural
    # sort will get the files in order. Except when
    # the system changed names and the files have
    # mixed station names. That will only be an issue
    # if the other name is EXMP because all others
    # are filtered out.
    #
    if ( $pf{newest_first} ) {
        # Start at newest.
        @download_list = sort {$b cmp $a} keys %flagged ;
    } else {
        # Start at oldest.
        @download_list = sort {$a cmp $b} keys %flagged ;
    }

    #
    # Log list of files
    #
    foreach $file ( @download_list ) {
        fork_debug("$file => $flagged{$file}") ;
    }

    fork_log('Files to download: ' . join(' ' ,@download_list)) ;

    FILE: foreach $file ( @download_list ) {

        $dir = $flagged{$file} ;
        $where = '' ;
        last unless check_time($start_sta) ;


        # Maybe we want to avoid old files.
        if ( yesno( $pf{limit_history} ) ) {

            # Match name to get time of data.
            $file =~ m/^\S*-(\d{4})(\d{2})(\d{2})(\d{6})$/ ;
            fork_debug( "$file date is $2/$3/$1" );

            if ( $1 and $2 and $3 ) {
                if ( (now() - str2epoch("$2/$3/$1")) > int($pf{limit_history}) ) {
                    fork_complain( "$file is too old: " . strtdelta(now() - str2epoch("$2/$3/$1")) );
                    next;
                }

            }

        }


        #
        # Limit the downloads to some Megabytes in some time (days)
        #
        while ( ($days, $mlimit) = each $pf{bandwidth_limits} ) {
            $d_data = total_data_downloaded($sta,$days) || 0.0 ;
            fork_debug("$sta downloaded $d_data Mbyts in last $days days.") ;
            if ( $d_data > $mlimit ) {
                fork_die("$sta downloaded ( $d_data ) Mbts in the last $days days.") ;
            }

        }


        # Notify if we are working with EXMP files
        fork_complain("Donwloading EXMP file: $file") if $file =~ /.*EXMP.*/ ;


        fork_log("Start download: $dir/$file") ;

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
            fork_log("move $path/$file to $path/trash/") ;

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

            fork_log("download_file($dir/$file,$path,$ip)") ;

            $start_file = now() ;
            $where = download_file("$dir/$file",$path,$ip) || '' ;
            $end_file = now() ;

        } else {

            #
            # We don't know the correct URL for
            # the file. Lets loop over all alternatives.
            #
            foreach $f (@lists) {

                fork_log("Now with directory $f") ;

                $dir = ( $f =~ /active/ ? 'WDIR' : 'WDIR2' ) ;

                $f =~ m/list\.\w+\.(s?data\S*)\.gz/ ;
                next unless $1 ;

                $start_file = now() ;
                fork_log("attempt download: $dir/$1/$file") ;
                $where = download_file("$dir/$1/$file",$path,$ip) || '' ;
                $end_file = now() ;
                last if $where ;

            }

        }

        $run_time = $end_file-$start_file ;
        $run_time_str = strtdelta($run_time) ;
        if ( -f $where ) {
            fork_log("Success download $file $run_time_str") ;
        }

        #
        # Verify downloaded file
        #
        $md5 = 'error' ;
        $status = 'downloaded' ;
        if ( -f $where) {

            if ( yesno( $pf{use_md5} ) ) {

                $md5 = get_md5($sta,$file,$ip,\@lists) || 'error' ;

                if ( $md5 =~ /(\S{32})/ ) {
                    fork_log("$file verified with md5: $md5") ;
                }
                else {
                    fork_complain("$file => status:$status md5:$md5") ;
                }

            } else {
                $md5 = 'avoid' ;
            }

            push @total_downloads, $file ;

        } elsif ( -f "$path/trash/$file") {

            #
            # Resurrect the file if we have previous copy
            #
            fork_complain("Resurrect file from $path/trash/$file") ;
            move("$path/trash/$file","$path/$file")
                or fork_complain("Can't move $file to $path") ;
            if ( yesno( $pf{use_md5} ) ) {
                $md5 = get_md5($sta,$file,$ip,\@lists) || 'error' ;
            }

        } else {

            fork_complain("$file => Not present after download") ;
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

        fork_die("$sta Problems on db open!") unless @db ;

        $db[3] = dbfind(@db, "dfile =~ /$file/", -1) ;

        #
        # Update attempts
        #
        $attempts = 1 ;
        if ($db[3] >= 0) {

            $attempts = int( dbgetv(@db,'attempts') )+1 ;
            $attempts = 1 if dbgetv(@db,'status') !~ $status ;

        }

        fork_log("$file | $dlsta | $start_file | $end_file "
            ."| $status | $attempts | $size | $speed | $md5") ;

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

        fork_debug("Next file") ;

    }

    unless ( scalar @total_downloads ) {
        fork_die("NO DOWNLOADS!!!! http://$ip:$pf{http_port}");
    }

    delete $flagged{$_} foreach @total_downloads ;

    if ( scalar keys %flagged > 0 ) {
        fork_complain('Missing: '.scalar keys %flagged . ' files') ;
        fork_debug('Missing: '.join(' ',sort keys %flagged)) ;
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

    fork_notify( "$sta with ". scalar @total_downloads." files "
        ."($m Mb) from $ip in $run_time_str") ;

    dbunlock("${path}/${sta}_baler") ;

    return 0;

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
            return ;
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
    my ($start,$r,$record) ;
    my (@db_temp,@db,@dbscr,@recs) ;
    my $total_bytes = 0.0 ;

    #
    # Verify Database
    #
    fork_debug("Get data downloaded in last $days for $sta") ;

    @db = open_db($sta) ;

    unless ( @db  ) {
        return $total_bytes ;
    }

    eval { dbquery(@db,dbTABLE_PRESENT) ; } ;
    if ( $@ ) {
        dbclose(@db) ;
        return $total_bytes ;
    }

    if (dbquery(@db, dbRECORD_COUNT) < 1) {
        dbclose(@db) ;
        return $total_bytes ;
    }

    $start = str2epoch("-${days}days") ;
    @db= dbsubset ( @db,
        "status =~ /downloaded/ && time >= $start ") ;

    unless ( dbquery(@db, dbRECORD_COUNT) ){
        dbclose(@db) ;
        return $total_bytes ;
    }

    $record  =  dbquery(@db, dbRECORD_COUNT) ;
    for ( $db[3] = 0 ; $db[3] < $record ; $db[3]++ ) {
        $total_bytes += (dbgetv (@db, 'filebytes')) ;
    }

    dbclose(@db) ;

    fork_debug("$total_bytes downloaded") ;

    # for Kbytes
    $total_bytes = sprintf("%0.2f", $total_bytes/1024) ;
    # for Mbytes
    $total_bytes = sprintf("%0.2f", $total_bytes/1024) ;

    fork_debug("Total Megabytes downloaded: $total_bytes") ;

    return $total_bytes ;

}

sub download_file {
    my $file = shift ;
    my $path = shift ;
    my $ip = shift ;
    my ($file_fetch,$where,$url) ;
    my $type = 'error' ;
    my $size = 0 ;
    my @temp_new ;


    #foreach ( split(/[\s|\/]/,$file) ) {
    #    next if /^\/$/ ;
    #    push(@temp_new,$_) if /^\S+$/ ;
    #}

    #$file = join('/',@temp_new) ;

    $url = "http://$ip:$pf{http_port}/$file" ;

    fork_debug( "Build File::Fetch object: $url") ;

    eval{
        $file_fetch = File::Fetch->new( uri => $url) ;
    } ;
    fork_complain("File::Fetch -> $@") if $@ ;

    fork_complain("ERROR in build of File::Fetch -> $url" )
        unless $file_fetch ;

    return unless $file_fetch ;

    fork_debug("Download: ".$file_fetch->uri) ;

    #eval {  $where = $file_fetch->fetch( to => "$path/" ) ; } ;
    #fork_complain("File::Fetch ".$file_fetch->uri." $@") if $@ ;

    $where = $file_fetch->fetch( to => "$path/" ) || '' ;

    #fork_complain(
    #   "ERROR on download of http://$ip:$pf{http_port}/$file")
    #   unless -f $where ;

    return unless -f $where ;


    # Verify size of file
    ($type, $size) = head( $url ) or fork_complain("ERROR $url: $!") ;

    if ( $size ){

        fork_debug("New downloaded file $where:   type:$type    size:$size") ;

        # If size of file is not the expected
        if ( -s $where != $size ) {
            fork_complain("Problem with file size of $where Reported:$size") ;
            move($where,"$path/trash/")
                or fork_complain("Can't move $where to $path/trash/") ;
            return ;
        }

    } else {

        fork_complain("Can't get size of $url on HTTP call.") ;

    }

    # Size 591 is a text page of HTTP errors.
    if ( -s $where == 591 ) {
        fork_complain("Problem with the file. $where from $url") ;
        move($where,"$path/trash/")
            or fork_complain("Can't move $where to $path/trash/") ;
        return ;
    }

    #fork_complain("ERROR on $file after download. Problem with name")
    #   unless $where =~ /$temp_new[-1]/ ;

    fork_complain("Cannot see $where after download") unless -f $where ;

    return $where if -f $where ;

    return ;

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

    fork_debug("Opening database ($dbout).") ;

    #
    # Create descriptor file if missing
    #
    unless ( -e $dbout) {
        fork_debug("$sta Creating new database ($dbout).") ;

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
    fork_debug("$sta Openning database table  ($dbout)") ;

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

    fork_debug("Reading and fixing of local directory") ;

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
        fork_debug("Clean table for dfile =~/$file/ ") ;

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

        fork_debug("local file: $f") ;

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

        fork_debug("Got subset for $f: (@recs)") ;

        if (scalar @recs == 1 ) {

            #
            # Verify the file is set to the proper status
            #
            $db[3] = $recs[0] ;

            if ( dbgetv(@db, 'status') =~ /downloaded/) {

                fork_debug("$f already in database as downloaded") ;

            } else {

                fork_notify("$f updated to 'downloaded'") ;
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
            fork_notify("$f adding as 'downloaded'") ;
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
    fork_log("Crunch table.") if $nulls ;
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
    my ($path,$name,$test) ;
    my %list ;
    my ($input,$list,$where,$files) ;
    my @temp_dir = () ;
    my (@temp,@db,@n,@queries) ;
    my $attempt = 1 ;

    $path = prepare_path($sta) ;

    $path .= '/lists/' ;

    fork_die("Cannot make directory ($path)") unless -d $path ;

    chdir $path or fork_die("Cannot go to directory ($path)") ;

    fork_debug("Full array for directories: @$dir") ;

    #
    # For each of the folders
    #
    foreach $list ( @$dir ) {

        #$list = "list.$dir.data.gz" ;

        next unless $list =~ /$pf{folder_with_files}/ ;

        fork_debug("Get: $list") ;

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
            #fork_debug("Split $test") ;
            @temp_dir = split(/\//,$test) ;
            $name = pop(@temp_dir) ;
            next unless $name ;
            #fork_debug("passed name test") ;
            #fork_debug("Test $name => $pf{regex_for_files}")  ;
            next unless  $name =~ /($pf{regex_for_files})/ ;
            #fork_debug("passed regex") ;
            next unless $name =~ /.*(${sta}|EXMP).*/ ;
            #fork_debug("passed ${sta}|EXMP regex") ;
            unshift(@temp_dir, $list =~ /active/ ? 'WDIR' : 'WDIR2' ) ;

            $list{$name} = join('/',@temp_dir) ;
            fork_debug("$name => $list{$name}") ;
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

    return 'ignore' unless $pf{md5_folder} =~ /\w{1,}/ ;

    $local_path = prepare_path($sta) . '/md5/' ;

    $old = $file ;

    $file .= '.md5' unless $file =~ /\.md5/ ;

    fork_debug("Get MD5 $file") ;

    chdir $local_path
        or fork_die("Cannot change to directory ($local_path)") ;

    unless ( -s "$local_path/$file" ) {

        unlink "$local_path/$file" ;
        fork_notify("Lets download MD5 file $file -> $local_path") ;

        foreach $f (@$lists) {

            next unless $f =~ m/$pf{md5_folder}/ ;
            fork_debug("Now with directory $f") ;
            $d = ( $f =~ /active/ ? 'WDIR' : 'WDIR2' ) ;
            $f =~ m/list\.\w+\.($pf{md5_folder}\S*)\.gz/ ;
            next unless $1 ;

            fork_debug("attempt download of MD5: $d/$1/$file") ;

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
        fork_log("MD5 file:: $_") ;
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
    # Connect to the Baler and parse the text
    # that we get on the status page. This page is
    # continuously updating and kept in memory. Returns
    # relatively fast without timeouts.
    # We try 2 times anyway....
    my $sta = shift ;
    my $ip = shift ;
    my (@text,$line,$browser, $resp) ;
    my $active = '' ;
    my $reserve = '' ;
    my @dir = () ;

    $browser = LWP::UserAgent->new ;

    $resp = $browser->timeout(120) ;

    fork_debug("$sta:\tLWP::UserAgent->get("
        ."http://$ip:$pf{http_port}/stats.html)") ;

    $resp = $browser->get("http://$ip:$pf{http_port}/stats.html") ;

    unless ( $resp->is_success ) {

        fork_debug("2nd time.... $sta:"
            ."\tLWP::UserAgent->get("
            ."http://$ip:$pf{http_port}/stats.html)") ;

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
                $text[$line] =~ m/>(list\.(active|reserve)\.$pf{folder_with_files}.*\.gz)</ ;

            fork_debug("Got data Folder: $1") if $1 ;

            next if $1 ;

            push(@dir,"$1") if $pf{md5_folder} =~ /\w{1,}/ and
                $text[$line] =~ m/>(list\.(active|reserve)\.$pf{md5_folder}.*\.gz)</ ;

            fork_debug("Got md5 Folder: $1") if $1 ;

        }
    }
    else {

        fork_complain("problem reading http://$ip:$pf{http_port}/stats.html") ;
        return ;

    }

    fork_complain("Cannot find MEDIA 1 in http://$ip:$pf{http_port}/stats.html")
        unless $active ;

    if ( $pf{media2_warning} ) {
        fork_complain("Cannot find MEDIA 2 in http://$ip:$pf{http_port}/stats.html")
            unless $reserve ;
    }

    $active  ||= '' ;
    $reserve ||= '' ;

    fork_log("get_medias_and_lists(http://$ip:$pf{http_port}/stats.html) => ($active,$reserve)") ;

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
    fork_log ( "Pf    $Pf     dbloc_pf_file   "
        ."$dbloc_pf_file  pid $pid" ) ;

    if ( ! -f $dbloc_pf_file ) {

        fork_log (
            sprintf("$db new lock set to %s",
                strydtime ( now() + $lock_duration ))
            ) ;

        &write_dblock ( $dbloc_pf_file, $0,
            $host, $pid, &now(), &now() + $lock_duration ) ;

        return ;

    } else {

        %pf = getparam( $Pf ) ;
        if ( $pf{unlock_time} > &now() && $pf{pid} != $pid ) {

            fork_complain (
                sprintf ("$db is locked until %s",
                    strydtime ( $pf{unlock_time} )
                    ) ) ;

            return 1 ;

        } elsif  ( $pf{unlock_time} > &now() && $pf{pid} == $pid ) {

            fork_log (
                sprintf ("$db lock is extended to %s",
                    strydtime ( now() + $lock_duration )
                ) ) ;

            &write_dblock ( $dbloc_pf_file, $0,
                $host, $pid, $pf{lock_time},
                now() + $lock_duration ) ;

            %pf = () ;
            return ;

        } else {

            fork_log (
                sprintf ("$db lock set to %s",
                    strydtime ( now() + $lock_duration )
                ) ) ;

            &write_dblock ( $dbloc_pf_file, $0,
                $host, $pid, &now(), &now() + $lock_duration ) ;

            %pf = () ;
            return ;

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
    fork_log ( "Pf    $Pf     "
        ."dbloc_pf_file   $dbloc_pf_file" ) ;

    if ( ! -f $dbloc_pf_file ) {

        fork_complain ( "dbunlock:      $dbloc_pf_file does not exist!" ) ;
        exit 1 ;

    } else {

        pfupdate ( $Pf ) ;
        %pf = getparam( $Pf ) ;
        if ($0 ne $pf{program} || $pid != int($pf{pid}) || $host ne $pf{host}) {

            fork_complain ( "unable to unlock $db" ) ;
            fork_complain ( "program    $0      $pf{program}" ) ;
            fork_complain ( "pid        $pid    $pf{pid}" ) ;
            fork_complain ( "host       $host   $pf{host}" ) ;
            exit 1 ;

        }
        if ( $pf{unlock_time} < &now() ) {

            fork_complain (
                sprintf ("$db was already unlocked at %s",
                    strydtime ( $pf{unlock_time} )
                ) ) ;
            exit 1 ;

        }

        &write_dblock ( $dbloc_pf_file, $0,
            $host, $pid, $pf{lock_time}, &now() ) ;

        return ;
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
    return if not $opt_w ;
    my $line = shift;

    unless ( $parent ) {
        print STDOUT "[LOG:$line]\n";
        return;
    }

    elog_notify( $line ) if $opt_v;

    return;
}

sub fork_notify { # &fork_notify ( $parent, $line ) ;
    my $line = shift;

    unless ( $parent ) {
        print STDOUT "[NOTIFY:$line]\n";
        return;
    }

    elog_notify( $line ) if $opt_v;

    return;
}

sub fork_debug { # &fork_debug ( $parent, $line ) ;
    return if not $opt_d ;
    my $line = shift;

    unless ( $parent ) {
        print STDOUT "[DEBUG:$line]\n";
        return;
    }

    elog_debug( $line ) if $opt_d ;

    return;
}

sub fork_complain { # &fork_complain ( $parent, $line ) ;
    my $line = shift;

    unless ( $parent ) {
        print STDOUT "[PROBLEM:$line]\n" ;
        return;
    }

    elog_complain( $line ) if $opt_v;

    return  ;
}

sub fork_die { # &fork_die ( $parent, $line ) ;
    my $line = shift;

    if ( $parent ) {

        elog_die( $line ) ;

    } else {

        # Maybe we need to unlock the database before
        # we exit. Try to create the path just from the
        # station name. The name comes from the command-
        # line argument.

        my $path = prepare_path($get_sta) ;
        my $full_path = "${path}/${get_sta}_baler";
        dbunlock( $full_path ) if -f $full_path ;

        fork_complain( "$get_sta:DIED: $line" );
        exit 9;

    }
}

__END__
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
