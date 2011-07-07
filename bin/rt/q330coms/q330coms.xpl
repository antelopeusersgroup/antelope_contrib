#
#  Builds and updates q330comms table from tadata/pf/st orb packet
#
#    use diagnostics ;
    use strict ;
    use Datascope ;
    use orb ;
    use archive ;
    use Getopt::Std ;

    our ( $opt_v, $opt_m, $opt_n, $opt_p );
    
    my ($pfsource,$orbname,$dbname,$orb,$pktid,$srcname,$net,$sta,$chan,$loc);
    my ($nbytes,$result,$pkt,$subcode,$desc,$type,$suffix,$pf,$ref);
    my ($when,$src,$pkttime,$start,$end);
    my ($ssident,$lat,$lon,$elev,$now);
    my ($latnull,$latdb,$lonnull,$londb,$elevnull,$elevdb,$endnull,$ssidentdb,$idtagdb,$timedb);
    my ($stadb,$inpdb,$nsta);
    my ($pgm,$host,$subject,$cmd,$usage,$rec);
    my (@db,@dbq330,@dbq330r,@dbscratch,@dbnull);
    my (@sources,@stas);
    my (%dls);

#  Set up mail    
            
    $pgm = $0 ;
    $pgm =~ s".*/"" ;
    elog_init($pgm, @ARGV);
    $cmd = "\n$0 @ARGV" ;

    if ( !  getopts('vm:n:p:') || @ARGV != 2 ) { 
        $usage  =  "\n\n\nUsage: $0 [-v] [-m mail_to] [-n net] [-p pf_sourcename] orb db\n"  ;
        
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

    $pfsource  = $opt_p || ".*/pf/st" ;
    
    elog_notify ("pfsource	$pfsource") if $opt_v;
    elog_notify ("orb		$orbname") if $opt_v;
    elog_notify ("db		$dbname\n") if $opt_v;
    
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
    
#        printf "%s	%s\n", $srcname, strydtime($pkttime);

        ($net, $sta, $chan, $loc, $suffix, $subcode) = $pkt->parts() ;
        ($type, $desc) = $pkt->PacketType() ;

        $pf = $pkt->pf ;

        if ( defined $pf ) {
            $ref = pfget($pf, "");
            @stas = sort keys %{$ref->{dls}};
            foreach $sta (@stas) {

                if ($opt_n) {
                    next if ($sta !~ /^$opt_n/);
                }

                $dls{$sta}{inp}     = $ref->{dls}{$sta}{inp};
                $dls{$sta}{ssident} = $ref->{dls}{$sta}{sn};
                $dls{$sta}{idtag}   = $ref->{dls}{$sta}{pt};
                $dls{$sta}{lat}     = $ref->{dls}{$sta}{lat}; 
                $dls{$sta}{lon}     = $ref->{dls}{$sta}{lon};
                $dls{$sta}{elev}    = $ref->{dls}{$sta}{elev};
                $dls{$sta}{thr}     = $ref->{dls}{$sta}{thr};
                $dls{$sta}{pkttime} = $pkttime;
                                
                if ( $dls{$sta}{ssident} =~ /.*[a-f].*/ ) {
                    elog_notify ("$srcname	$sta	$ssident	" . strydtime($pkttime) . "	pktid  $pktid");
                }
                $dls{$sta}{ssident}  = uc($dls{$sta}{ssident});

                elog_notify( sprintf "%s	%s	%s	%s	%s	%s	%s	%s", 
                    $sta, $dls{$sta}{inp}, $dls{$sta}{ssident},$dls{$sta}{idtag},
                    $dls{$sta}{lat},$dls{$sta}{lon},$dls{$sta}{elev},$dls{$sta}{thr}) if $opt_v;

            }
        }
    }
    
#
#  open output db
#
    @db        = dbopen($dbname,"r+");
    @dbq330    = dblookup(@db,"","q330comm","","");
    @dbscratch = dblookup(@dbq330,"","","","dbSCRATCH");
    @dbnull    = dblookup(@dbq330,"","","","dbNULL");
    $endnull   = dbgetv(@dbnull,"endtime");
    
    $nsta = dbquery(@dbq330,"dbRECORD_COUNT");
    elog_notify("	Number of database stations:	$nsta") if $opt_v;
    
    $now = now();
    @dbq330r    = dbsubset(@dbq330,"endtime > $now");
    @dbq330r    = dbsort(@dbq330r,"dlsta");
    
    $nsta = dbquery(@dbq330r,"dbRECORD_COUNT");
    elog_notify("	Number of active database stations:	$nsta") if $opt_v;
    
    @stas = sort keys %dls;
    elog_notify("	Number of active real time stations:	$#stas") if $opt_v;
    elog_notify("\n	Stations to process - 	@stas");
#
#  Loop over existing q330coms table records
#                
    for ($dbq330r[3] = 0; $dbq330r[3] < $nsta; $dbq330r[3]++) {
        ($stadb, $timedb, $inpdb, $ssidentdb, $idtagdb, $latdb, $londb, $elevdb) = 
            dbgetv(@dbq330r,"dlsta","time","inp","ssident", "idtag","lat","lon","elev");
#
#  Station expected to be connected
#                
        if (exists($dls{$stadb})) {
            elog_notify(sprintf("%s active as of:		%s",$stadb,strydtime($timedb))) if $opt_v;
#
#  Station not currently connected
#                
            if ($dls{$stadb}{idtag} == 0 ) {
                elog_notify("	$dls{$stadb}{ssident}	$dls{$stadb}{inp}	not connected") if $opt_v;
                delete($dls{$stadb});
                next;
            }
#
#  Station has same Q330 serial number and ip connection
#                
            if ( $ssidentdb =~ /$dls{$stadb}{ssident}/ && $inpdb =~ /$dls{$stadb}{inp}/  ) {
                elog_notify("	$ssidentdb	$inpdb	did not change") if $opt_v;
#
#  Updated lat and lon
#                
                ($latnull,$lonnull,$elevnull) = dbgetv(@dbnull,"lat","lon","elev");

                if (($latdb == $latnull)||($londb==$lonnull)||($elevdb==$elevnull)|| abs($latdb) < 0.01 || abs($londb) < 0.01) {
                    elog_notify("		updating location	$dls{$stadb}{lat}	$dls{$stadb}{lon}	$dls{$stadb}{elev}") if $opt_v;
                    
                    if ($lat eq "-" || $lon eq "-" || $elev eq "-") {
                        dbputv(@dbq330r,"lat",$latnull,"lon",$lonnull,"elev",$elevnull);
                    } else {
                        dbputv(@dbq330r,"lat",$dls{$stadb}{lat},"lon",$dls{$stadb}{lon},"elev",$dls{$stadb}{elev});
                    }
                }
#
#  Station has new Q330 serial number or ip connection
#                
            } else {
                elog_notify("$stadb	$ssidentdb	$inpdb	CHANGED	putting endtime in view");
                dbputv(@dbq330r,"endtime",($now-1));
                elog_notify("		$dls{$stadb}{ssident}	$dls{$stadb}{inp}	adding new record to table");
                next;
            }
        }  else {
#
#  Stations to close in table
#
            elog_notify(sprintf("%s is closed as of:	%s",$stadb,strydtime(now())));
            dbputv(@dbq330r,"endtime",($now-1));
        }
        delete($dls{$stadb});
    }
#
#  New stations or data to add to table
#
    @stas = sort keys %dls;
    elog_notify("\n");
    elog_notify("Remaining stations to process - 	@stas") if $opt_v;

    foreach $sta (@stas) {
#
#  Station not currently connected
#                
        if ($dls{$sta}{idtag} == 0 ) {
            elog_notify("$sta	$dls{$sta}{ssident}	$dls{$sta}{inp}	not connected") if $opt_v;
            next;
        }
        elog_notify("Adding station	$sta");
        dbaddv(@dbq330,"dlsta",   $sta,
                       "time",    $now,
                       "inp",     $dls{$sta}{inp},
                       "ssident", $dls{$sta}{ssident},
                       "idtag",   $dls{$sta}{idtag},
                       "lat",     $dls{$sta}{lat},
                       "lon",     $dls{$sta}{lon},
                       "elev",    $dls{$sta}{elev},
                       "thr",     $dls{$sta}{thr});
    }

    $subject = "Completion - $pgm $host $orbname ";
    &sendmail($subject, $opt_m) if $opt_m ; 
    print "\n$subject \n\n";
    
    $end = strydtime(now());
    
    elog_notify("$pgm	started $start	ended $end");
exit ;
    
    
