#
#   rsync_baler2json: create json file from rsync_baler directory
#   author: Juan C. Reyes
#   email:  reyes@ucsd.edu
#   No BRTT support
#

#
#  Program setup
#
#{{{

use sysinfo;
use Datascope;
use Pod::Usage;
use Getopt::Std;
use List::Util qw[max min];


    $parent = $$;
    $start = now();
    $host = my_hostname();



    unless ( &getopts('s:r:vh') ) { 
        elog_complain("Problem with flags.");
        pod2usage({-verbose => 2});
    }

    if ( 3 < @ARGV or @ARGV < 2 ) { 
        elog_complain("Problem with arguments.");
        pod2usage({-verbose => 2});
    }

    #
    # Print help and exit
    #
    pod2usage({-verbose => 2}) if $opt_h;

    if ( $opt_v ) {
        elog_notify('');
        elog_notify("$0 @ARGV");
        elog_notify("Starting at ".strydtime($start)." on $host");
        elog_notify('');
        elog_notify('');
    }

    $database = $ARGV[0];
    elog_complain("We need to have a database with [deployment|stabaler|staq330] tables") unless $database;
    pod2usage({-verbose => 1}) unless $database;
    elog_notify("Database: $database") if $opt_v;

    $dir = $ARGV[1];
    elog_complain("We need to have a directory with the folders of the stations") unless $dir;
    pod2usage({-verbose => 1}) unless $dir;
    elog_notify("Directory: $dir") if $opt_v;

    $json = $ARGV[2] if scalar(@ARGV) == 3;
    elog_notify("JSON: $json") if $opt_v;
    if ( $json ) {
        $json = File::Spec->rel2abs( $json ); 
        elog_notify("Write table in json file: $json") if $opt_v;
        unlink($json) if -e $json;
    }

    #
    # Verify Database
    #
    @db = dbopen ( $database, "r" ) or elog_die("Can't open DB: $database"); 

    # Open table for list of valid stations 
    @db_on = dblookup(@db, "", "deployment" , "", "");

    # Open table for list of station types ie 'PacketBaler44'
    @db_sta = dblookup(@db, "", "stabaler", "", "");

    # Open table for list of current ips
    @db_ip = dblookup(@db, "", "staq330" , "", "");

    #
    # Verify access to directory
    #
    elog_die("Can't access dir => $dir.") unless -e $dir;



    elog_notify('Get list of stations:') if $opt_v;
    $stations = get_stations_from_db(); 

    #
    # Report and/or JSON file export
    #
    build_json($stations);

    #
    # Calc total time for script
    #
    $end = now();
    $run_time = $end - $start;
    $start = strydtime($start);
    $end = strydtime($end);
    $run_time_str = strtdelta($run_time);

    elog_notify("Start: $start End: $end") if $opt_v;
    elog_notify("Runtime: $run_time_str") if $opt_v;

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
    elog_notify("dbsubset ( stablaler.model =~ /Packet Baler44/)") if $opt_v;
    @db_1 = dbsubset ( @db_sta, "stabaler.model =~ /PacketBaler44/ ");

    elog_notify("dbsubset ( sta =~ /$opt_s/)") if $opt_v && $opt_s;
    @db_1 = dbsubset ( @db_1, "sta =~ /$opt_s/") if $opt_s;

    elog_notify("dbsubset ( sta !~ /$opt_r/)") if $opt_v && $opt_r;
    @db_1 = dbsubset ( @db_1, "sta !~ /$opt_r/") if $opt_r;

    $nrecords = dbquery(@db_1,dbRECORD_COUNT) or elog_die("No records to work with after dbsubset()"); 
    elog_notify("dbsubset => nrecords = $nrecords") if $opt_v;


    for ( $db_1[3] = 0 ; $db_1[3] < $nrecords ; $db_1[3]++ ) { 

        ($dlsta,$net,$sta,$time,$endtime) = dbgetv(@db_1, qw/dlsta net sta time endtime/); 

        elog_notify("[$sta] [$net] [$dlsta] [$time] [$endtime]") if $opt_v;

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
            problem("Failed grep on IP stabaler{inp}->(ip'$ip',dlsta'$dlsta')") unless $1;
            $sta_hash{$sta}{ip} = $1 if $1; 

        }

        elog_notify("$dlsta $sta_hash{$sta}{status} $sta_hash{$sta}{ip}") if $opt_v; 

        foreach (sort @{$sta_hash{$sta}{dates}}) { elog_notify("\t\t@$_") if $opt_v; }

    }

    eval { dbclose(@db_sta); };
    eval { dbclose(@db_ip);  };
    eval { dbclose(@db_on);  };

    return \%sta_hash;
#}}}
}

sub build_json {
#{{{
    $stations = shift;
    my (@bw);
    my ($dfile);
    my (@flagged,@downloaded,@missing);
    my ($text,$time);
    my (@dbr,@dbr_temp,@queries);
    my (@dbr_grouped);
    my ($average,$median);
    my ($bandwidth_low,$bandwidth_high,$start_of_report);
    my ($total_30,$total_7,$last_30,$last_7);
    my ($md5_missing,$md5_error,$last_file,$last_time);



    open ( JSON, ">$json") if $json;

    print JSON "{" if $json;

    foreach $temp_sta ( sort keys %$stations ) {

        elog_notify("Now report on $temp_sta.") if $opt_v;

        $text = "\"$temp_sta\": {";
        @flagged = ();
        @downloaded = ();
        @missing = ();
        @bw = ();
        $d = -1;
        $f = -1;
        $last_time = 0;
        $last_file = 0;
        $md5_error = 0;
        $md5_missing = 0;
        $last_7 = str2epoch("-7days");
        $last_30 = str2epoch("-30days");
        $total_7 = 0;
        $total_30 = 0;

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
        unless ( @dbr ) {
            $text   .= ",\n\t\"error\": \"No Database!\" },\n";
            export($text);
            next;
        }

        eval { dbquery(@dbr,"dbTABLE_PRESENT"); };
        if ( $@ ) {
            $text   .= ",\n\t\"error\": \"No dbTABLE_PRESENT!\" },\n";
            export($text);
            dbclose(@dbr);
            next;
        }

        if (dbquery(@dbr, 'dbRECORD_COUNT') < 1) {
            $text   .= ",\n\t\"error\": \"Database empty!\" },\n";
            export($text);
            dbclose(@dbr);
            next;
        }

        @dbr_sorted = dbsort(@dbr,'status');
        @dbr_grouped = dbgroup(@dbr_sorted,'status');


        #elog_notify("FIELD: ".$_) foreach dbquery(@dbr_grouped,'dbTABLE_FIELDS');
        #$recs =  dbquery(@dbr_grouped,'dbRECORD_COUNT');
        #for ( $dbr_grouped[3] = 0 ; $dbr_grouped[3] < $recs; @dbr_grouped[3]++ ) {
        #elog_notify( $_ ) foreach dbgetv(@dbr_grouped,qw/status bundle/);
        #}



        $f = dbfind(@dbr_grouped, 'status =~ /flagged/', -1);
        $e = dbfind(@dbr_grouped, 'status =~ /error-download/', -1);
        $d = dbfind(@dbr_grouped, 'status =~ /downloaded/', -1);

        #elog_notify("d: $d f: $f e: $e");

        #
        # Get list of flagged files
        #
        if ( $f >= 0 ) {
            @dbr_grouped[3] = $f;
            @dbr_temp= split(" ",dbgetv(@dbr_grouped,"bundle"));
            #elog_notify("flagged dbr_temp: @dbr_temp");
            for ( $t = $dbr_temp[3] ; $t < $dbr_temp[2] ; $t++ ) {
                $dbr_temp[3] = $t;
                push @flagged, dbgetv (@dbr_temp, 'dfile');
            }
        }

        #
        # Get list of downloaded files
        #
        if ( $d >= 0 ) {
            @dbr_grouped[3] = $d;
            @dbr_temp= split(" ",dbgetv(@dbr_grouped,"bundle"));
            #elog_notify("downloaded dbr_temp: @dbr_temp");

            for ( $t = $dbr_temp[3] ; $t < $dbr_temp[2] ; $t++ ) {
                $dbr_temp[3] = $t;

                ($file, $time, $bytes, $bandwidth, $md5) = dbgetv(@dbr_temp, qw/dfile time filebytes bandwidth md5/);
                #elog_notify("$file, $time, $status, $bytes, $bandwidth, $md5");

                push @downloaded, $file; 

                $bandwidth += 0;
                push @bw, $bandwidth if $bandwidth > 0; 

                $last_time = $time unless $last_time;
                $last_file = $file unless $last_file;

                if ( $last_time < $time) { 
                    $last_time = $time;
                    $last_file = $file;
                }

                $md5_error += 1 if  $md5 =~ /.*error.*/;
                $md5_missing += 1 if  $md5 =~ /.*missing.*/;

                $total_7 += $bytes if $time > $last_7;
                $total_30 += $bytes if $time > $last_30;

                #elog_notify("$time ?> $last_7");
                #elog_notify("true") if $endtime > $last_7;
                #elog_notify("total_7 = $total_7");
                #elog_notify("$endtime ?> $last_30");
                #elog_notify("true") if $endtime > $last_30;
                #elog_notify("total_30 = $total_30");

            }
        }
        #elog_die('end');

        #
        # Check for missing files
        #
        @missing = unique_array(\@flagged,\@downloaded);

        if ( scalar @missing ) {

            @missing =  grep { $_ = "\"$_\"" } @missing;
            $text .= ",\n\t\"missing_files\": [" . join(',',@missing) . "]";
            $text .= ",\n\t\"error\": \"Missing ".@missing." files. \"";
            elog_notify("Station $temp_sta missing files:[@missing]") if $opt_v;

        }

        $text .= ",\n\t\"missing\": " . scalar(@missing);
        $text .= ",\n\t\"downloaded\": " . scalar(@downloaded);

        if ( $last_file  and $last_time ) {
            $last_time = epoch2str($last_time,"%Y-%m-%d");
            $last_time =~ s/\s*//g;
            $text .= ",\n\t\"last\": \"$last_file\"";
            $text .= ",\n\t\"last_time\": \"$last_time\"";
        }
        else {
            $text .= ",\n\t\"last\": \"UNKNOWN\"";
            $text .= ",\n\t\"last_time\": \"UNKNOWN\"";
        }

        if ( scalar @bw ) {
            $bandwidth_low  = min @bw;
            $bandwidth_high = max @bw;
            $median = median(\@bw);
            $average = average(\@bw);

            $median = sprintf("%0.1f", $median);
            $average = sprintf("%0.1f", $average);
            $bandwidth_high = sprintf("%0.1f", $bandwidth_high);
            $bandwidth_low = sprintf("%0.1f", $bandwidth_low);

        }
        else{
            $bandwidth_high = '"-"';
            $bandwidth_low  = '"-"';
            $median = '"-"';
            $average  = '"-"';
        }

        $text .= ",\n\t\"low_b\": $bandwidth_low";
        $text .= ",\n\t\"high_b\": $bandwidth_high";
        $text .= ",\n\t\"median\": $median";
        $text .= ",\n\t\"average\": $average";

#
        # Get md5 errors
        #
        $text .= ",\n\t\"error-verify\": \"$md5_error\"";
        $text .= ",\n\t\"md5-missing\": \"$md5_missing\"";

        #
        # Get list of flagged files
        #
        if ( $e >= 0 ) {
            $e = $dbr_temp[2]-$dbr_temp[3];
            $text .= ",\n\t\"error-download\": \"$e\"";
        }
        else {
            $text .= ",\n\t\"error-download\": \"UNKNOWN\"";
        }

        #
        # Get list by month
        #
        #$text .= ",\n\t\"files\": {";

        #@queries = build_time_regex($temp_sta,$stations->{$temp_sta}->{dates});

        #foreach (@queries) {

        #    #
        #    # Open db and search for files here
        #    #
        #    @dbr_temp= dbsubset ( @dbr, "dfile =~ /($_)/ && status == 'downloaded'");
        #    $nrecords = dbquery(@dbr_temp, 'dbRECORD_COUNT') ;

        #    $text .= " \"$_\": \"$nrecords\",";
        #    dbfree(@dbr_temp);

        #}

        #dbclose(@dbr);


        #$text .= "{" if (chop($text) eq '{');

        #$text .= "}\n";

        # for Kbytes
        $total_7 = sprintf("%0.2f", $total_7/1024);
        $total_30 = sprintf("%0.2f", $total_30/1024);
        # for Mbytes
        $total_7 = sprintf("%0.2f", $total_7/1024);
        $total_30 = sprintf("%0.2f", $total_30/1024);

        $text .= ",\n\t\"30Mbytes\": " . ($total_30);
        $text .= ",\n\t\"7Mbytes\": " . ($total_7);

        $text   .= "\n\t},\n";

        export($text);
        dbfree( @dbr_sorted );
        dbfree( @dbr_grouped );
        dbclose( @dbr);

    }

    seek JSON,-2,2 if $json;
    export("\n}");
    close( JSON ) if $json;

#}}}
}

sub export {
#{{{
    $text = shift;

    if ( $json ) {
        elog_notify($text) if $opt_v;
        print JSON $text;
    }
    else {
        elog_notify($text);
    }
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


                elog_notify("Create regex for:".strtime($start)."=>".strtime($end)) if $opt_v;

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

    foreach (sort keys %queries) { elog_notify("Regex=$_") if $opt_v};

    return sort keys %queries;

#}}}
}

sub open_db {
#{{{
    my $sta = shift;
    my @db;

    #
    # Prepare Folder Name
    #
    my $path = prepare_path($sta);

    return unless -e $path;

    #
    # Build station database name
    #
    $dbout = "$path/$sta";
    $dbout .= "_baler";

    #
    # Fix path
    #
    $dbout = File::Spec->rel2abs( $dbout ); 

    #
    # Open table
    #
    elog_notify("$sta Openning database table  ($dbout.rsyncbaler)") if $opt_v;
    eval { @db  = dbopen_table("$dbout.rsyncbaler","r+") or elog_complain("Can't open DB: $dbout.rsyncbaler",$sta) };
    #elog_notify("$sta open_db() $path => @db");
    return unless @db; 
    return @db; 

#}}}
}

sub read_local {
#{{{
    my $sta = shift;
    my %list;
    my $file;
    my $f;

    elog_notify("Reading local directory") if $opt_v;

    my $path = prepare_path($sta);

    opendir(DIR,$path) or elog_die("Failed to open $path: $!");

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

    foreach (sort keys %list) { elog_notify("LOCAL: $_") if $opt_v; }

    return sort keys %list;

#}}}
}

sub prepare_path {
#{{{
    my $station  = shift;

    elog_die("prepare_path(). Cannot produce path! We need a station name...") unless $station;

    my $path = File::Spec->rel2abs( "$dir/$station" ); 

    elog_notify("$sta path: $path ") if $opt_v;
    return $path;
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

__END__
#{{{
=pod

=head1 NAME

rsync_baler2json - Read Baler44 rsyncbaler table(directory) and create JSON output

=head1 SYNOPSIS

rsync_baler2json [-v] [-h] [-s sta_regex] [-r sta_regex] dbmaster baler44_dir [json.output] 

=head1 ARGUMENTS

=over 4

=item dbmaster

Table with tables deployment, stabaler and staq330.

=item baler44_dir

Path to the base directory with all stations. Local archive of balers.

=item [json.output]

File to update with all the JSON output from the script. If empty then print to stdout.
You can then pipe the output to a second script. 

=back

=head1 OPTIONS 

=over 4

=item B<-h> 

Print this help message

=item B<-v> 

Produce verbose output while running

=item B<-s regex>

Select station regex. ('STA1|STA2' or 'A...|B.*')

=item B<-r regex>

Reject station regex. ('STA1|STA2' or 'A...|B.*')

=back

=head1 DESCRIPTION

This script will read a local directory that archives rsync_baler 
folders for Baler44 stations and will output the status of the 
tables in JSON format. This is the fastest way of creating a 
website that will present this information to the users. 
Maybe not to use in realtime to export data to http calls but to
update json file for the ajax calls from the clients. 
The script is simple and may fail if used outside ANF-TA installation. 

=head1 AUTHOR

Juan C. Reyes <reyes@ucsd.edu>

=cut
#}}}
