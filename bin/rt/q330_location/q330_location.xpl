#
#  Checks q330 locations from tadata/pf/st orb packet
#
#  Need to account for q330hr
#
#   use diagnostics ;
    use warnings;
    use strict ;
    use Datascope ;
    use sysinfo;
    use orb ;
    use archive ;
    use Getopt::Std ;

    our($pgm,$host,$Problems,$opt_v,$opt_d,$opt_c,$opt_m,$opt_n,$opt_f,$opt_p,$opt_s,$opt_t);
    my (@dbcomm,@dbcommscr,@dbcommnull,@dbsitesub,@dbsnet,@dbstage,@dbstagescr,@dbdls);
    my (%offline,$subset,$nbytes,$result,$pkt,$subcode,$desc,$type,$suffix,$pf);
    my ($orbname,$cmd_orbname,$dbname,$cmd_orb,$orb,$pktid,$srcname,$sta,$chan,$loc);
    my ($fer,$when,$src,$pkttime,$start,$nettmp,$statmp,$chantmp);
    my ($pfsource,$subject,$cmd,$usage,$match);
    my ($long,$ssident,$lat,$lon,$endtime,$now,$stime);
    my (@db_temp,@temp_dls,@sources,@stas,@matches);
    my ($commtype,$provider,$nsta);
    my (%site,$ref,$tbl,%dls_hash,@dls_array,$dls,$log_file);
    my (@keys,@db,@dbsite,@dbdlsite);
    my ($km,$tolerance);

#  Set up mail  
            
    $pgm = $0 ;
    $pgm =~ s".*/"" ;
    elog_init($pgm, @ARGV);
    $cmd = "\n$0 @ARGV" ;

    if ( !  getopts('ndvfc:m:p:s:t:') || @ARGV < 2 || @ARGV > 3) { 
        $usage  =  "\n\n\nUsage: $0 [-d] [-v] [-n] [-f] [-m mail_to] [-c chan_code] " ;
        $usage .=  "[-p pf_sourcename] [-s sta_regex] [-t max_offset_meters] status_orb [cmd_orb] db\n"  ;
        
        elog_notify($cmd) ; 
        problem($usage);
        email_and_end(1);
    }

    $opt_v = defined($opt_d) ? $opt_d : $opt_v;

    &savemail() if $opt_m ; 
    elog_notify($cmd) if $opt_v; 

    $start = strydtime(now());
    $now   = now();
        
    $host = my_hostname();
    elog_notify ("\nstarting execution on   $host   $start\n\n");

#  Set up intial parameters

    ($orbname,$cmd_orbname,$dbname) = @ARGV if (@ARGV == 3);
    ($orbname,$dbname) = @ARGV if (@ARGV == 2);

    $chan      = $opt_c || "BHZ" ;
    $pfsource  = $opt_p || ".*/pf/st" ;
    $tolerance = $opt_t ? ($opt_t/1000) : 0.02;

    elog_notify ("pfsource  $pfsource")   if $opt_d;
    elog_notify ("status_orb$orbname")    if $opt_d;
    elog_notify ("cmd_orb   $cmd_orbname")if $opt_d;
    elog_notify ("db        $dbname\n")   if $opt_d;
    

    #
    #  Open databases
    #
    @db         = dbopen($dbname,"r+"); 
    @dbsite     = dblookup(@db,"","site","","");
    @dbdlsite   = dblookup(@db,"","dlsite","","");
    @dbsnet     = dblookup(@db,"","snetsta","","");

    @dbcomm     = dblookup(@db,"","comm","","");
    @dbcommscr  = dblookup(@dbcomm,"","","","dbSCRATCH");
    @dbcommnull = dblookup(@dbcomm,"","","","dbNULL");

    @dbstage    = dblookup(@db,"","stage","","");
    @dbstagescr = dblookup(@dbstage,"","","","dbSCRATCH");

    #
    # Verify databases
    #
    table_check(\@dbsite);
    table_check(\@dbcomm);
    table_check(\@dbsnet);
    table_check(\@dbstage); 

    if ($Problems) { email_and_end(1); }

    #
    # Subset site table
    #
    $subset     = "(offdate == NULL || offdate > ".yearday($now).")";
    $subset    .= "&& sta =~ /$opt_s/" if $opt_s;
    elog_notify ("$dbname.site subset $subset") if $opt_d;
    @dbsitesub = dbjoin(@dbsite,@dbsnet);
    @dbsitesub = dbsubset(@dbsitesub,$subset);
    @dbsitesub = dbsort(@dbsitesub,"sta");


    #
    # Subset dlsite table
    #
    $subset   = "endtime == NULL ";
    $subset  .= "&& dlname =~ /.*_$opt_s/" if $opt_s;
    if ( dbquery(@dbdlsite,"dbRECORD_COUNT") > 0 ) {
        elog_notify ("$dbname.dlsite subset $subset") if $opt_d;
        @dbdls   = dbsubset(@dbdlsite,$subset);
        @dbdls   = dbsort(@dbdls,"dlname");
    }
    else {
        elog_notify ("$dbname.dlsite is empty!") if $opt_d;
        @dbdls = @dbdlsite;
    }

#
#  open input orb
#
    if ( defined $cmd_orbname ){
        $cmd_orb = orbopen($cmd_orbname,"r");
        if ( $cmd_orb < 0 ) { 
            problem("Failed to open orb $cmd_orbname for reading");
            email_and_end(1);
        }
        orbclose($cmd_orb);
    }
    else { $cmd_orbname = $orbname; }

    $orb = orbopen($orbname,"r");
    if ( $orb < 0 )     { 
        problem("Failed to open orb $orbname for reading");
        email_and_end(1);
    }

    orbselect( $orb, $pfsource);
    ($when, @sources) = orbsources ( $orb );

#
#  Look for sources in orb
#
    if (!@sources) { 
        problem("No orbsorces inside $orbname"); 
        email_and_end(1);
    }

    foreach $src (@sources) {
        $srcname = $src->srcname() ;
        orbselect ( $orb, $srcname ) ;
        ($pktid, $srcname, $pkttime, $pkt, $nbytes) = orbget ( $orb, "ORBNEWEST" ) ;
        elog_notify (sprintf ("\n%s %s \n\n", $srcname, strydtime($pkttime))) if $opt_d;

        if (!defined $pktid || $nbytes == 0) {
            next;
        }

        ($result, $pkt) = unstuffPkt ( $srcname, $pkttime, $pkt, $nbytes ) ;

        if ( $result ne "Pkt_pf" ) {
            if( $opt_d ) {
                elog_notify ("Received a $result, skipping") ;
            }
            next;
        }
    
        ($nettmp, $statmp, $chantmp, $loc, $suffix, $subcode) = $pkt->parts() ;
        ($type, $desc) = $pkt->PacketType() ;

        $pf = $pkt->pf ;
#
#  process pf files looking for stations
#
        if ( defined $pf ) {
            $ref = pfget($pf, "");
            @temp_dls = keys %{$ref->{dls}};
            @temp_dls = grep { /.*_$opt_s/ } @temp_dls if ($opt_s);
            @dls_array = sort get_unique( (@dls_array,@temp_dls) );
        }

#
#  process each net_station using dlcmd 
#
        foreach $dls (@temp_dls) {

            if (! $dls) { next; }

            $log_file = "/tmp/dlcmd_${dls}_$$.pf";

#
# Use -f option to refresh data from station
#
            if ($opt_f) { $cmd = "dlcmd $cmd_orbname $ref->{target} - $dls getstatus -force 2>&1 >$log_file";  }
            else { $cmd = "dlcmd $cmd_orbname $ref->{target} - $dls getstatus 2>&1 >$log_file";  }

            elog_notify("Running: $cmd") if $opt_d;
            $result = run($cmd);
            $tbl = pfget($log_file,'');
            unlink $log_file;

            if ($opt_d) {
                elog_notify("$dls table from dlcmd getstatus");
                print_pfe($tbl,1);
                elog_notify("$dls table from orbpckt");
                print_pfe($ref,1);
            }

#
# Get all data into hashes
#
            $fer = $ref->{dls}{$dls};
            @keys = keys %$tbl;
            $tbl = $tbl->{$keys[0]}{status}{gps};


            if ( $tbl->{number_of_satellites_used} ) {;
                $dls_hash{$dls}{gps}         = $tbl->{gps_fix_string};
                $dls_hash{$dls}{lat}         = $tbl->{lat};
                $dls_hash{$dls}{lon}         = $tbl->{lon};
                $dls_hash{$dls}{elev}            = ($tbl->{elev})/1000;
                $dls_hash{$dls}{source}      = 'dlcmd getstatus';
                $dls_hash{$dls}{sat_used}        = $tbl->{number_of_satellites_used};
                $dls_hash{$dls}{sat_in_view} = $tbl->{number_of_satellites_in_view};
            }
            else {
                $dls_hash{$dls}{lat}     = $fer->{lat};
                $dls_hash{$dls}{lon}     = $fer->{lon};
                $dls_hash{$dls}{gps}     = $fer->{gps};
                $dls_hash{$dls}{elev}        = $fer->{elev};
                $dls_hash{$dls}{source}  = 'orb Pkt_pf';
            }

            $dls_hash{$dls}{inp}     = $fer->{inp};
            $dls_hash{$dls}{thr}     = $fer->{thr};
            $dls_hash{$dls}{model}   = $fer->{type};
            $dls_hash{$dls}{idtag}   = $fer->{pt};
            $dls_hash{$dls}{status}  = 'on-line';
            $dls_hash{$dls}{pkttime} = $pkttime;
            $dls_hash{$dls}{ssident} = uc($fer->{sn});

            if ($opt_v) {
                $fer = $dls_hash{$dls};
                elog_notify("$dls:");
                elog_notify( "\t$dls_hash{$dls}{ssident} $dls_hash{$dls}{idtag} $dls_hash{$dls}{thr}");
                elog_notify( "\t$dls_hash{$dls}{lat} $dls_hash{$dls}{lon} $dls_hash{$dls}{elev}");
                elog_notify( "\t$dls_hash{$dls}{gps} $dls_hash{$dls}{source} ");
            }

            next if ( ! $dls_hash{$dls}{idtag} || $dls_hash{$dls}{idtag} == 0 );
#
# hash $site needed to check site table
#
            $sta = $dls ;
            $sta =~ s/.*_// ;
            $site{$sta} = $dls; 
        }
    } 
    
    orbclose($orb);


    if (! %dls_hash) { 
        problem("No stations in orb $orbname") if ! $opt_s; 
        problem("No stations in orb $orbname matching regex /$opt_s/") if $opt_s; 
        email_and_end(1);
    }

#
#  Check Site table 
#
    $nsta = dbquery(@dbsitesub,"dbRECORD_COUNT");
    if ( $opt_d ) {
        elog_notify("Checking site table");
        elog_notify("\t$nsta records in table.");
    }
    for ($dbsitesub[3] = 0; $dbsitesub[3] < $nsta; $dbsitesub[3]++) {
        $sta = dbgetv(@dbsitesub,'sta');

        if ( $site{$sta} ) { 
            $long = $site{$sta};
            check_gps(\@dbsitesub,$dls_hash{$long},'sta','site'); 
        } 
        else { 
            $offline{$sta} = 1;
            problem("For site table: $sta off-line (not present in orb)"); 
        }

        delete( $site{$sta} );
    }

    @stas = sort keys %site;
    
    if ($#stas >= 0 && $opt_v) {
        elog_notify("\n");
        elog_notify("Remaining site stations to process -    @stas");
    }
    foreach $sta (@stas) { problem("$sta not in site table"); }


#
#  Check dlsite table
#
    $nsta = dbquery(@dbdls,"dbRECORD_COUNT");
    if ( $opt_d ) {
        elog_notify("\nChecking dlsite table\n");
        elog_notify("$nsta stations to process");
    }

    for ($dbdls[3] = 0; $dbdls[3] < $nsta; $dbdls[3]++) {
        ($dls,$lat,$lon,$ssident,$commtype,$provider) = 
            dbgetv(@dbdls,"dlname","lat","lon","ssident","commtype","provider");
            
        $dls_hash{$dls}{commtype} = $commtype;
        $dls_hash{$dls}{provider} = $provider;
        
        elog_notify("$dls   $dls_hash{$dls}{commtype}    $dls_hash{$dls}{provider}") if $opt_d;
        
        $sta = $dls ;
        $sta =~ s/.*_// ;

        ($commtype,$provider) = comm_type($dls);

        if ($opt_d) {
            elog_notify("$sta:");
            elog_notify("\tdlsite: $dls_hash{$dls}{commtype} $dls_hash{$dls}{provider}");
            elog_notify("\tcomm  : $commtype $provider");
        }


        if ( defined $dls_hash{$dls}{idtag} ) {
            if ($dls_hash{$dls}{idtag} eq 0 ) {
                if (! defined $offline{$sta} ) {
                    problem("$sta off-line");
                }
            }
            elsif ($ssident ne $dls_hash{$dls}{ssident} ) {
# do we want to use stage table change?
                elog_notify("$dls changed datalogger, closing old record");
                dbputv(@dbdls,"endtime",($now - 1)) unless $opt_n;
            }
            elsif ($commtype !~ /$dls_hash{$dls}{commtype}/ || ($provider !~ /$dls_hash{$dls}{provider}/ ) ) {
                elog_notify("$dls changed commtype or provider, closing old record");
                dbputv(@dbdls,"endtime",($now - 1)) unless $opt_n;
            }
            elsif ( check_gps(\@dbdls,$dls_hash{$dls},'dlname','dlsite')) {
                elog_notify("Adding new values (lat,lon)=($lat,$lon) to table dlsite");
                dbputv(@dbdls,"lat",$lat,"lon",$lon) unless $opt_n;
            }
        } 
        else {
            dbputv(@dbstagescr,"sta",$sta,"chan",$chan,"gtype","digitizer");
            @matches = dbmatches(@dbstagescr,@dbstage,"stage","sta","chan","gtype");
            $endtime = 0;
            foreach $match (@matches) {
                $dbstage[3] = $match;
                $endtime = dbgetv(@dbstage,"endtime") if dbgetv(@dbstage,"endtime") > $endtime;
            }
            if ($endtime != 0 && $endtime < $now) {
                problem(sprintf("$sta now decomissioned, closing old record at %s",strydtime($endtime))) ;
                dbputv(@dbdls,"endtime",$endtime) unless $opt_n;
            } else {
                problem(sprintf("$sta apparently decomissioned, record in stage table still active."),
                        "Uncomment $sta from q3302orb.pf if this station is supposed to be on line.",
                        "Otherwise, please update metadata.") ;
            }
        }
        elog_notify("delete $dls    $dls_hash{$dls}") if $opt_d;
        delete($dls_hash{$dls});
    }
#
#  New stations not in dlsite table
#
    @dls_array = sort keys %dls_hash;

    if ($#dls_array >= 0 ) {
        elog_notify("\n");
        elog_notify("Remaining dlsite stations to process -     @dls_array") if $opt_v;

        foreach $dls (@dls_array) {
            elog_notify("$dls not in dlsite table, adding!") if $opt_v;
            
            $sta = $dls ;
            $sta =~ s/.*_// ;

            ($commtype,$provider) = comm_type($dls);

            dbputv(@dbstagescr,"sta",$sta,"chan",$chan,"time",$now,"gtype","digitizer");
            @matches = dbmatches(@dbstagescr,@dbstage,"stage","sta","chan","gtype");
            
            if ($#matches == 0) {
                elog_notify("\t$dls new record added to dlsite table");
                dbaddv(@dbdlsite, "model",  $dls_hash{$dls}{model},
                                "ssident",  $dls_hash{$dls}{ssident},
                                "time",  $now,
                                "dlname",   $dls, 
                                "idtag",    $dls_hash{$dls}{idtag},
                                "lat",    $dls_hash{$dls}{lat},
                                "lon",    $dls_hash{$dls}{lon},
                                "elev",  $dls_hash{$dls}{elev},
                                "commtype", $commtype,
                                "provider", $provider) unless $opt_n;
            }  else {
                problem("$sta not in stage table");
            }               
        }
    }

    email_and_end(0);

#
# Email and end script
#
sub email_and_end {
    my $status = shift;
    my $title;
    my $host = my_hostname();

    $stime = strydtime(now());

    if (! defined $Problems ) {
        elog_notify ("Completed with SUCCESS status  $stime\n\n");
        $subject = sprintf("Success  $pgm  $host");
    } else { 
        elog_notify ("Completed with $Problems problems  $stime\n\n");
        $subject = "Problems - $pgm $host - $Problems problems" ;
    }
    &sendmail($subject, $opt_m) if $opt_m ; 
  
    exit $status;
}


#
# Check reported gps values to archived values
#
sub check_gps {
    my ($db,$rf,$flag,$table) = @_ ;
    my @text = ();
    

    my ($st,$lat,$lon) = dbgetv(@$db,$flag,"lat","lon");

    if ( $rf->{lat} !~ /\d+\.?\d?/ ){ problem("$st lat not a number: $rf->{lat}"); return 0; };
    if ( $rf->{lat} !~ /\d+\.?\d?/ ){ problem("$st lon not a number: $rf->{lon}"); return 0; };
    if ( abs($rf->{lat}) > 90  || abs($rf->{lat}) < 0.01 ){ problem("$st lat out of range: $rf->{lat}"); return 0; };
    if ( abs($rf->{lon}) > 180 || abs($rf->{lon}) < 0.01 ){ problem("$st lon out of range: $rf->{lon}"); return 0; };

    if ($opt_d) {
        elog_notify("Checking gps location for station $st");
        elog_notify("\ttable $lat $lon");
        elog_notify("\tdlcmd $rf->{lat},$rf->{lon}");
    }

    my $km = (111. * dbex_eval(@$db,"distance($rf->{lat},$rf->{lon},lat,lon)"));
    if ( $km > $tolerance) { 
        push(@text, sprintf "\t$st   %7.3f km offset", $km);
        push(@text, sprintf "\tdatabase     lat:%9.5f     lon:%9.5f", $lat, $lon);
        push(@text, sprintf "\treported     lat:%9.5f     lon:%9.5f", $rf->{lat},$rf->{lon});
        push(@text, sprintf "\t             quality:%s     satellites:%d out of %d",
                        $rf->{gps},$rf->{sat_used},$rf->{sat_in_view});
        push(@text, sprintf "\t             source: %s", $rf->{source});
        push(@text, sprintf "\t             db.table: %s.%s", $dbname,$table);
        problem(@text);
        if( $rf->{gps} eq '3-D' ) { return 1; } 
    }
    return 0;
}


sub table_check {  #  $Problems = &table_check($Problems,@db);
    my $db = shift;

    if (! dbquery(@$db,"dbTABLE_PRESENT")) {
        problem( dbquery(@$db,"dbTABLE_NAME")." table is not available.");  
    }
}


sub print_pfe { #use print_pfe($ref,0);
    my ($r,$level) = @_;
    my $tab;
    my $nexttab;

    #
    # Nested Sub
    #
    local *print_val = sub {
        my ($tab,$k,$v) = @_;
        my $line = '';

        my $length = length($k);

        if ($length > 30) {
            elog_notify("${tab} $k --> $v");
        }
        else{
            for (my $n=0; $n < 30-$length; $n++){ $line .= '-'; }
            elog_notify("$tab $k$line> $v");
        }
    };

    foreach (0 .. ($level-1)){ $tab .= "    "; }
    $level= $level + 1;
    foreach (0 .. ($level-1)){ $nexttab .= "    "; }

    while( my ($k1,$v1) = each %$r ){
        if (ref($v1) eq "ARRAY") {
            elog_notify("${tab} $k1@ >>");
            for my $i (0 .. (@$v1-1)){
                if ( ref(@$v1[$i]) eq "ARRAY" ) {
                    elog_notify("${nexttab} $i@ >>");
                    print_pfe(@$v1[$i],$level+1);
                }
                elsif ( ref(@$v1[$i]) eq "HASH" ) {
                    elog_notify("${nexttab} $i% >>");
                    print_pfe(@$v1[$i],$level+1);
                }
                else { print_val(${nexttab}, $i, @$v1[$i]); }
            }
        }
        elsif (ref($v1) eq "HASH") {
            elog_notify("${tab} $k1% >>");
            print_pfe($v1,$level);
        }
        else{
            print_val(${tab}, $k1, $v1);
        }
    }
}

sub problem { # use problem("log of problem");
    my @text = @_;

    $Problems++ ;
    elog_notify("\n");
    elog_complain("Problem #$Problems") ;
    foreach (@text) {
        elog_complain("    $_");
    }
    elog_notify("\n");

}



sub comm_type {
    my $logger = shift;
    my ($type,$provider);

    $logger =~ s/^.*_// ;

    dbputv(@dbcommscr,"sta",$logger,"time",$now);
    my @matches = dbmatches(@dbcommscr,@dbcomm,"comm");
    
    if ($#matches == -1) {
        ($type,$provider) = dbgetv(@dbcommnull,"commtype","provider");
        problem("$logger no open record in comm table");
    } elsif ($#matches == 0) {
        $dbcomm[3] = $matches[0];
        ($type,$provider) = dbgetv(@dbcomm,"commtype","provider");
    }  else {
        ($type,$provider) = dbgetv(@dbcommnull,"commtype","provider");
        problem("$logger $#matches records in comm table");
    }       
    return ($type,$provider);
}
