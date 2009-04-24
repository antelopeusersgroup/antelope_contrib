#
#  Checks q330 locations from tadata/pf/st orb packet
#
#  Need to account for q330hr
#
#    use diagnostics ;
    use strict ;
    use Datascope ;
    use orb ;
    use archive ;
    require "getopts.pl" ;

    our ( $opt_v, $opt_c, $opt_m, $opt_n, $opt_p, $opt_s, $opt_t );
    
    my ($pgm,$host,$Problems,$subject,$cmd,$usage,$subset,$match);
    my ($pfsource,$orbname,$dbname,$orb,$pktid,$srcname,$net,$sta,$chan,$loc);
    my ($nbytes,$result,$pkt,$subcode,$desc,$type,$suffix,$pf,$ref);
    my ($when,$src,$pkttime,$start, $nettmp, $statmp, $chantmp);
    my ($ssident,$lat,$lon,$endtime,$now,$stime);
    my ($km,$delta,$tolerance);
    my ($commtype,$provider,$model,$nsta);
    my ($dls);
    my (@db,@dbsite,@dbdlsite);
    my (@dbcomm,@dbcommscr,@dbcommnull,@dbsitesub,@dbsnet,@dbstage,@dbstagescr,@dbdls);
    my (@sources,@dls,@stas,@matches);
    my (%dls,%sta);

#  Set up mail    
            
    $pgm = $0 ;
    $pgm =~ s".*/"" ;
    elog_init($pgm, @ARGV);
    $cmd = "\n$0 @ARGV" ;

    if ( !  &Getopts('vc:m:p:s:t:') || @ARGV != 2 ) { 
        $usage  =  "\n\n\nUsage: $0 [-v] [-m mail_to] [-n net_code] [-c chan_code] " ;
        $usage .=  "[-p pf_sourcename] [-s sta_regex] [-t max_offset_meters] orb db\n"  ;
        
        elog_notify($cmd) ; 
        elog_die ( $usage ) ; 
    }

    &savemail() if $opt_m ; 
    elog_notify($cmd) ; 
    
    $start = strydtime(now());
        
    chop ($host = `uname -n` ) ;
    elog_notify ("\nstarting execution on	$host	$start\n\n");

#  Set up intial parameters

    $orbname   = $ARGV[0];
    $dbname    = $ARGV[1];

    $net       = $opt_n || "TA" ;
    $chan      = $opt_c || "BHZ" ;
    $pfsource  = $opt_p || ".*/pf/st" ;
    $tolerance = ( $opt_t/1000 ) || 0.02 ;

    elog_notify ("net		$pfsource") if $opt_v;
    elog_notify ("pfsource	$pfsource") if $opt_v;
    elog_notify ("orb		$orbname") if $opt_v;
    elog_notify ("db		$dbname\n") if $opt_v;
    
#  Set up default model.  Will need to change this when more dataloggers get put into this system.

    $model = "q330";
#
#  open db
#
    @db         = dbopen($dbname,"r+");    
    @dbsite     = dblookup(@db,"","site","","");
    @dbdlsite   = dblookup(@db,"","dlsite","","");
    @dbcomm     = dblookup(@db,"","comm","","");
    @dbcommscr  = dblookup(@dbcomm,"","","","dbSCRATCH");
    @dbcommnull = dblookup(@dbcomm,"","","","dbNULL");
    @dbsnet     = dblookup(@db,"","snetsta","","");
    @dbstage    = dblookup(@db,"","stage","","");
    @dbstagescr = dblookup(@dbstage,"","","","dbSCRATCH");
    
    $subset    = "snet =~ /$net/ && (offdate == NULL || offdate > " . yearday(now()) . " )";
    $subset   .= "&& sta =~ /$opt_s/" if $opt_s;
    elog_notify ("subset	$subset") if $opt_v;
    @dbsitesub = dbjoin(@dbsite,@dbsnet);
    @dbsitesub = dbsubset(@dbsitesub,$subset);
    @dbsitesub = dbsort(@dbsitesub,"sta");
    
    $Problems = &table_check($Problems,@dbsite);
    $Problems = &table_check($Problems,@dbcomm);
    $Problems = &table_check($Problems,@dbsnet);
    $Problems = &table_check($Problems,@dbstage);    

    if ($Problems) { 
        $subject = "Problems - $pgm $host	$Problems database problems" ;
        &sendmail($subject, $opt_m) if $opt_m ; 
        elog_die("\n$subject") ;
    }    
#
#  open input orb
#
    $orb = orbopen($orbname,"r");

    if( $orb < 0 ) {
        $subject = "Problems - $pgm $host	Failed to open orb '$orbname' for reading";
        &sendmail($subject, $opt_m) if $opt_m ; 
        elog_die( $subject );
    }

    orbselect( $orb, $pfsource);
    
    ($when, @sources) = orbsources ( $orb );
#
#  Look over source names
#
    foreach $src (@sources) {
        $srcname = $src->srcname() ;
        orbselect ( $orb, $srcname ) ;
        ($pktid, $srcname, $pkttime, $pkt, $nbytes) = orbget ( $orb, "ORBNEWEST" ) ;
        elog_notify (sprintf ("\n%s	%s \n\n", $srcname, strydtime($pkttime))) if $opt_v;
        if (!defined $pktid) {
            next ;
        }
        if ( $nbytes == 0 ) {
            next ;
        }
        ($result, $pkt) = unstuffPkt ( $srcname, $pkttime, $pkt, $nbytes ) ;

        if ( $result ne "Pkt_pf" ) {
            if( $opt_v ) {
                elog_notify ("Received a $result, skipping") ;
            }
            next;
        }
    
        ($nettmp, $statmp, $chantmp, $loc, $suffix, $subcode) = $pkt->parts() ;
        ($type, $desc) = $pkt->PacketType() ;

        $pf = $pkt->pf ;
#
#  process pf files and build hashes
#

        if ( defined $pf ) {
            $ref = pfget($pf, "");
            @dls = sort keys %{$ref->{dls}};
            if ($opt_s) {
                @dls = grep { /.*_$opt_s/ } @dls;
            }
#
#  process each net_station
#
            foreach $dls (@dls) {

                if ($opt_n) {
                    next if ($dls !~ /^$opt_n/);
                }
                $dls{$dls}{model}     = "q330";


                $dls{$dls}{inp}     = $ref->{dls}{$dls}{inp};
                $dls{$dls}{ssident} = $ref->{dls}{$dls}{sn};
                $dls{$dls}{idtag}   = $ref->{dls}{$dls}{pt};
                $dls{$dls}{lat}     = $ref->{dls}{$dls}{lat}; 
                $dls{$dls}{lon}     = $ref->{dls}{$dls}{lon};
                $dls{$dls}{elev}    = $ref->{dls}{$dls}{elev};
                $dls{$dls}{thr}     = $ref->{dls}{$dls}{thr};
                $dls{$dls}{gps}     = $ref->{dls}{$dls}{gps};
                $dls{$dls}{pkttime} = $pkttime;
                                
                if ( $dls{$dls}{ssident} =~ /.*[a-f].*/ ) {
                    elog_notify ("$srcname	$dls	$ssident	" . strydtime($pkttime) . "	pktid  $pktid");
                }
                $dls{$dls}{ssident}  = uc($dls{$dls}{ssident});

                elog_notify( sprintf "%s	%s	%s	%s	%s	%s	%s	%s	%s", 
                    $dls, $dls{$dls}{inp}, $dls{$dls}{ssident},$dls{$dls}{idtag},
                    $dls{$dls}{lat},$dls{$dls}{lon},$dls{$dls}{elev},$dls{$dls}{thr},$dls{$dls}{gps}) if $opt_v;
                    
                next if ( $dls{$dls}{idtag} == 0 );
                next if ( $dls{$dls}{lat}  eq "-" || abs($dls{$dls}{lat}) < 0.01 );
                next if ( $dls{$dls}{lon}  eq "-" || abs($dls{$dls}{lon}) < 0.01 );
                
                $sta = $dls ;
                $sta =~ s/.*_// ;
                
                $sta{$sta}{lat}     = $ref->{dls}{$dls}{lat}; 
                $sta{$sta}{lon}     = $ref->{dls}{$dls}{lon};
                $sta{$sta}{elev}    = $ref->{dls}{$dls}{elev};
                $sta{$sta}{gps}     = $ref->{dls}{$dls}{gps};
                
#                elog_notify( sprintf "%s	%s	%s	%s", 
#                    $sta, $sta{$sta}{lat},$sta{$sta}{lon},$sta{$sta}{elev}) if $opt_v;
                    
            }
        }
    }
    

#
#  Check Site table to see if coordinates match gps
#
    elog_notify("\nChecking site table\n");
    $nsta = dbquery(@dbsitesub,"dbRECORD_COUNT");
    for ($dbsitesub[3] = 0; $dbsitesub[3] < $nsta; $dbsitesub[3]++) {
        ($sta,$lat,$lon) = dbgetv(@dbsitesub,"sta","lat","lon");
                            
        if (exists($sta{$sta})) {
            $delta = dbex_eval(@dbsitesub,"distance($sta{$sta}{lat},$sta{$sta}{lon},lat,lon)");
            $km = $delta * 111.;
            if ($km > $tolerance) {
                $Problems++ ;
                elog_complain("\nProblem #$Problems") ;
                elog_complain(sprintf "$sta           %7.3f km offset\n", $km);
                elog_complain(sprintf "    gps          lat  %9.5f		lon  %9.5f		status	%s\n", 
                              $sta{$sta}{lat}, $sta{$sta}{lon}, $sta{$sta}{gps});
                elog_complain(sprintf "    site table   lat  %9.5f		lon  %9.5f  \n", $lat, $lon);
#
#  put in GPS tests here
#
            }
        } else {
            $Problems++  if $opt_v;
            elog_complain("\nProblem #$Problems") if $opt_v;
            elog_complain("$sta not on-line") if $opt_v;
        }
        delete($sta{$sta});
    }

#
#  New stations not in site table
#
    @stas = sort keys %sta;
    
    if ($#stas >= 0) {
        elog_notify("\n");
        elog_notify("Remaining stations to process - 	@stas") if $opt_v;

        foreach $sta (@stas) {
            $Problems++ ;
            elog_complain("\nProblem #$Problems") ;
            elog_complain("$sta not in site table");
        }
    }

#
#  Process dlsite table
#
    $now = now();
    
    elog_notify("\nChecking dlsite table\n");
    
    $subset    = "endtime == NULL ";
    $subset   .= "&& dlname =~ /.*_$opt_s/" if $opt_s;
    elog_notify ("subset	$subset") if $opt_v;
    @dbdls     = dbsubset(@dbdlsite,$subset);
    @dbdls     = dbsort(@dbdls,"dlname");
    
    $nsta = dbquery(@dbdls,"dbRECORD_COUNT");
    
#    elog_notify("$nsta stations to process");
    for ($dbdls[3] = 0; $dbdls[3] < $nsta; $dbdls[3]++) {
        ($dls,$lat,$lon,$ssident,$commtype,$provider,$model) = 
              dbgetv(@dbdls,"dlname","lat","lon","ssident","commtype","provider","model");
              
        $dls{$dls}{commtype} = $commtype;
        $dls{$dls}{provider} = $provider;
        
#        elog_notify("$dls	$dls{$dls}{commtype}	$dls{$dls}{provider}");
        
        $sta = $dls ;
        $sta =~ s/.*_// ;
        
#        elog_notify("$dls	$sta");
        dbputv(@dbcommscr,"sta",$sta,"time",$now);
        @matches = dbmatches(@dbcommscr,@dbcomm,"comm");
        
        if ($#matches == -1) {
            ($commtype,$provider) = dbgetv(@dbcommnull,"commtype","provider");
            $Problems++ ;
            elog_complain("\nProblem #$Problems") ;
            elog_complain("$dls	$sta	no open record in comm table");
        } elsif ($#matches == 0) {
            $dbcomm[3] = $matches[0];
            ($commtype,$provider) = dbgetv(@dbcomm,"commtype","provider");
        }  else {
            ($commtype,$provider) = dbgetv(@dbcommnull,"commtype","provider");
            $Problems++ ;
            elog_complain("\nProblem #$Problems") ;
            elog_complain("$dls	$sta	$#matches records in comm table");
        }        

#        elog_notify("$dls	$dls{$dls}{commtype}	$dls{$dls}{provider}	$commtype	$provider");


        if (exists($dls{$dls})) {
#            elog_notify("exists	$dls");
            if ($dls{$dls}{idtag} == 0 ) {
                $Problems++  if $opt_v;
                elog_complain("\nProblem #$Problems") if $opt_v;
                elog_complain("$sta not on-line") if $opt_v;
                delete($dls{$dls});
                next;
            }
            if ($ssident != $dls{$dls}{ssident} ) {
# do we want to use stage table change?
                elog_notify("$dls changed datalogger, closing old record");
                dbputv(@dbdls,"endtime",($now - 1));
                next;
            }
            if ($commtype !~ /$dls{$dls}{commtype}/ || ($provider !~ /$dls{$dls}{provider}/ ) ) {
                elog_notify("$dls changed commtype or provider, closing old record");
                dbputv(@dbdls,"endtime",($now - 1));
                next;
            }
            $delta = dbex_eval(@dbdls,"distance($dls{$dls}{lat},$dls{$dls}{lon},lat,lon)");
            $km = $delta * 111.;
            if ($km > $tolerance) {
                $Problems++ ;
                elog_complain("\nProblem #$Problems") ;
                elog_complain(sprintf "$dls           %7.3f km offset, updating row\n", $km);
                elog_complain(sprintf "    gps          lat  %9.5f		lon  %9.5f		status	%s\n", 
                              $dls{$dls}{lat}, $dls{$dls}{lon}, $dls{$dls}{gps});
                elog_complain(sprintf "    site table   lat  %9.5f		lon  %9.5f  \n", $lat, $lon);
#
#  put in GPS tests here
#
                dbputv(@dbdls,"lat",$lat,"lon",$lon);
            }
        } else {
            $sta = $dls ;
            $sta =~ s/.*_// ;
            dbputv(@dbstagescr,"sta",$sta,"chan",$chan,"gtype","digitizer");
            @matches = dbmatches(@dbstagescr,@dbstage,"stage","sta","chan","gtype");
            $endtime = 0;
            foreach $match (@matches) {
                $dbstage[3] = $match;
                $endtime = dbgetv(@dbstage,"endtime") if dbgetv(@dbstage,"endtime") > $endtime;
            }
            if ($endtime < $now) {
                $Problems++ ;
                elog_complain("\nProblem #$Problems") ;
                elog_complain(sprintf("$sta now decomissioned, closing old record at %s",strydtime($endtime))) ;
                dbputv(@dbdls,"endtime",$endtime) unless ($endtime > $now);
            } else {
                $Problems++ ;
                elog_complain("\nProblem #$Problems") ;
                elog_complain(sprintf("$sta apparently decomissioned, record in stage table still active.")) ;
                elog_complain("      Uncomment $sta from q3302orb.pf if this station is supposed to be on line.") ;
                elog_complain("      Otherwise, please update metadata.") ;
            }
        }
#        elog_notify("delete	$dls	$dls{$dls}");
        delete($dls{$dls});
    }

#
#  New stations not in dlsite table
#
    @dls = sort keys %dls;

    if ($#stas >= 0) {
        elog_notify("\n");
        elog_notify("Remaining dlsite stations to process - 	@dls") if $opt_v;
    }

    foreach $dls (@dls) {
        $Problems++ ;
        elog_complain("\nProblem #$Problems") ;
        elog_complain("$dls not in dlsite table, adding $dls to table");
        
        $sta = $dls ;
        $sta =~ s/.*_// ;

        dbputv(@dbcommscr,"sta",$sta,"time",$now);
        @matches = dbmatches(@dbcommscr,@dbcomm,"new_comm");

        if ($#matches == 0) {
            $dbcomm[3] = $matches[0];
            ($commtype,$provider) = dbgetv(@dbcomm,"commtype","provider");
        }  else {
            ($commtype,$provider) = dbgetv(@dbcommnull,"commtype","provider");
        }        

        dbputv(@dbstagescr,"sta",$sta,"chan",$chan,"time",$now,"gtype","digitizer");
        @matches = dbmatches(@dbstagescr,@dbstage,"stage","sta","chan","gtype");
        
        if ($#matches == 0) {
            elog_notify("$dls	new record added to dlsite table");
            dbaddv(@dbdlsite, "model",    $dls{$dls}{model},
                              "ssident",  $dls{$dls}{ssident},
                              "time",     $now,
                              "dlname",   $dls, 
                              "idtag",    $dls{$dls}{idtag},
                              "lat",      $dls{$dls}{lat},
                              "lon",      $dls{$dls}{lon},
                              "elev",     $dls{$dls}{elev},
                              "commtype", $commtype,
                              "provider", $provider);
        }  else {
            $Problems++ ;
            elog_complain("\nProblem #$Problems") ;
            elog_complain("$sta not in stage table");
        }                
    }

    if ($Problems == 0 ) {
        $stime = strydtime(now());
        elog_notify ("completed successfully	$stime\n\n");

        $subject = sprintf("Success  $pgm  $host");
        elog_notify ("\n$subject");
        &sendmail ( $subject, $opt_m ) if $opt_m ;
    } else { 
        $subject = "Problems - $pgm $host	$Problems problems" ;
        &sendmail($subject, $opt_m) if $opt_m ; 
        elog_die("\n$subject") ;
    }
  
    exit(0);


sub table_check {  #  $Problems = &table_check($Problems,@db);
    my ($Problems,@db) = @_;
    my ($table);
    if (!dbquery(@db,"dbTABLE_PRESENT")) {
        $table = dbquery(@db,"dbTABLE_NAME");
        $Problems++ ;
        printf "\nProblem #$Problems\n" ;
        print "$table table is not available for $dbname \n";    
    }
    return $Problems;
}
