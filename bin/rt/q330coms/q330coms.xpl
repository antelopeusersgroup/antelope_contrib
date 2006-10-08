#
#  Builds and updates q330comms table from tadata/pf/st orb packet
#
#    use diagnostics ;
    use strict ;
    use Datascope ;
    use orb ;
    require "getopts.pl" ;

    our ( $opt_v, $opt_n, $opt_p );
    my ($pfsource,$orbname,$dbname,$orb,$pktid,$srcname,$time,$net,$sta,$chan,$loc);
    my ($nbytes,$result,$pkt,$packet,$subcode,$desc,$type,$suffix,$pf,$ref,$value,$liststa);
    my ($listpar,$par,$nsta,$line);
    my ($when,$src,$pkttime);
    my (%pf,%dls,%par);
    my ($inp,$ssident,$idtag,$lat,$lon,$elev,$thr,$endtime);
    my ($latnull,$latdb,$lonnull,$londb,$elevnull,$elevdb);
    my (@db,@dbq330,@dbq330r,@dbscratch,@dbnull,@row,@sources);

    if ( !  &Getopts('vn:p:') || @ARGV != 2 )
        { die ( "Usage: $0 [-v] [-n net] [-p pf_sourcename] orb db\n" ) ; }

    $orbname   = $ARGV[0];
    $dbname    = $ARGV[1];

    elog_init( $0, @ARGV );

    $pfsource  = $opt_p || ".*/pf/st" ;

    elog_notify ("pfsource	$pfsource") if $opt_v ;
    elog_notify ("orb		$orbname")  if $opt_v ;
    elog_notify ("db		$dbname")   if $opt_v ;
#
#  open output db
#
    @db        = dbopen($dbname,"r+");
    @dbq330    = dblookup(@db,"","q330comm","","");
    @dbq330r   = @dbq330;
    @dbscratch = dblookup(@dbq330,"","","","dbSCRATCH");
    @dbnull    = dblookup(@dbq330,"","","","dbNULL");
    $endtime   = dbgetv(@dbnull,"endtime");

#
#  open input orb
#
    $orb = orbopen($orbname,"r");

    if( $orb < 0 ) {
        die( "Failed to open orb '$orbname' for reading\n" );
    }

    orbselect( $orb, $pfsource);
    
    ($when, @sources) = orbsources ( $orb );

    foreach $src (@sources) {
        $srcname = $src->srcname() ;
        orbselect ( $orb, $srcname ) ;
        ($pktid, $srcname, $pkttime, $pkt, $nbytes) = orbget ( $orb, "ORBNEWEST" ) ;
        elog_notify (sprintf "\n%s	%s \n", $srcname, strydtime($pkttime)) if $opt_v;
        if (!defined $pktid) {
            next ;
        }
        if ( $nbytes == 0 ) {
            next ;
        }
        ($result, $pkt) = unstuffPkt ( $srcname, $pkttime, $pkt, $nbytes ) ;

        if ( $result ne "Pkt_pf" ) {
            if( $opt_v ) {
                elog_notify( "Received a $result, skipping\n" );
            }
            next;
        }
    
#        printf "%s	%s\n", $srcname, strydtime($pkttime);

        ($net, $sta, $chan, $loc, $suffix, $subcode) = $pkt->parts() ;
        ($type, $desc) = $pkt->PacketType() ;

        $pf = $pkt->pf ;

        if ( defined $pf ) {
            $ref = pfget($pf, "");
            my @stas = sort keys %{$ref->{dls}};
            foreach my $sta (@stas) {

                if ($opt_n) {
                    next if ($sta !~ /^$opt_n/);
                }

                my @pars = sort keys %{$ref->{dls}{$sta}};
                $inp     = $ref->{dls}{$sta}{"inp"};
                $ssident = $ref->{dls}{$sta}{"sn"};
                $idtag   = $ref->{dls}{$sta}{"pt"};
                $lat     = $ref->{dls}{$sta}{"lat"}; 
                $lon     = $ref->{dls}{$sta}{"lon"};
                $elev    = $ref->{dls}{$sta}{"elev"};
                $thr     =  $ref->{dls}{$sta}{"thr"};
                
                if ( $ssident =~ /.*[a-f].*/ ) {
                    $line = "\n	$srcname	$sta	$ssident	" . strydtime($pkttime) . "	pktid  $pktid \n";
                    elog_notify($line);
                }
                $ssident     = uc($ssident);

                elog_notify (sprintf "%s	%s	%s	%s	%s	%s	%s	%s", $sta, $inp, $ssident,$idtag,$lat,$lon,$elev,$thr) if $opt_v;

                dbputv(@dbscratch, "dlsta",   $sta,
                                   "time",    $pkttime,
                                   "endtime", $endtime,
                                   "inp",     $inp,
                                   "ssident", $ssident,
                                   "idtag",   $idtag,
                                   "thr",     $thr);

                if ($lat eq "-" || $lon eq "-" || $elev eq "-") {
                   ($lat,$lon,$elev) = dbgetv(@dbnull,"lat","lon","elev");
                }

                dbputv(@dbscratch, "lat", $lat, "lon", $lon, "elev", $elev);

#  Check if data changed when compared to database

                @row     = dbmatches(@dbscratch,@dbq330,"no_change","dlsta","inp","ssident","endtime");
                if ($#row == 0) {
                    if ($idtag == 0) {
                        next;
                    }
                    $dbq330r[3] = $row[0];
                    ($latnull,$lonnull,$elevnull) = dbgetv(@dbnull,"lat","lon","elev");
                    ($latdb, $londb, $elevdb) = dbgetv(@dbq330r,"lat","lon","elev");
                    if (($latdb == $latnull)||($londb==$lonnull)||($elevdb==$elevnull)) {
                        dbputv(@dbq330r,"lat",$lat,"lon",$lon,"elev",$elev);
                    }
                    next;
                }

#  Check to see if ip changes
                
                @row     = dbmatches(@dbscratch,@dbq330,"ip_change","dlsta","ssident","endtime");
                if ($#row == 0) {
                    if ($idtag == 0) {
                        next;
                    }
                    $dbq330r[3] = $row[0];
                    dbputv(@dbq330r,"endtime",($pkttime-1)); 
                    dbadd(@dbq330);
                    next;
                }  elsif ($#row != -1){
                    elog_die ("PF data ip_change $sta	$time	$inp	$ssident	$endtime \n matches q330comms rows $#row @row\n");           
                }
                
#  Check to see if sn changes
                
                @row     = dbmatches(@dbscratch,@dbq330,"sn_change","dlsta","inp","endtime");
                if ($#row == 0) {
                    $dbq330r[3] = $row[0];
                    dbputv(@dbq330r,"endtime",($pkttime-1)); 
                    dbadd(@dbq330);
                    next;
                }  elsif ($#row != -1){
                    elog_die ("PF data sn_change $sta	$time	$inp	$ssident	$endtime \n matches q330comms rows @row\n");           
                }
                
#  Check to see if sta changes
                
                @row     = dbmatches(@dbscratch,@dbq330,"sta_change","ssident","inp","endtime");
                if ($#row == 0) {
                    $dbq330r[3] = $row[0];
                    dbputv(@dbq330r,"endtime",($pkttime-1)); 
                    dbadd(@dbq330);
                    next;
                }  elsif ($#row != -1){
                    elog_die ("PF data sta_change $sta	$time	$inp	$ssident	$endtime \n matches q330comms rows @row\n");           
                }

                dbadd(@dbq330);
            }
        }
#        exit;
    }
exit ;
