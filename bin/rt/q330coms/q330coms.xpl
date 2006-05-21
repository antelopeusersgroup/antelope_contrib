#
#  Builds and updates q330comms table from tadata/pf/st orb packet
#
#    use diagnostics ;
    use strict ;
    use Datascope ;
    use orb ;
    require "getopts.pl" ;

    our ( $opt_v, $opt_p );
    my ($pfsource,$orbname,$dbname,$orb,$pktid,$srcname,$time,$net,$sta,$chan,$loc);
    my ($nbytes,$result,$pkt,$packet,$subcode,$desc,$type,$suffix,$pf,$ref,$value,$liststa);
    my ($listpar,$par,$nsta);
    my (%pf,%dls,%par);
    my ($inp,$ssident,$idtag,$lat,$lon,$elev,$thr,$endtime);
    my ($latnull,$latdb,$lonnull,$londb,$elevnull,$elevdb);
    my (@db,@dbq330,@dbq330r,@dbscratch,@dbnull,@row);

    if ( !  &Getopts('vp:') || @ARGV != 2 )
        { die ( "Usage: $0 [-v] [-p pf_sourcename] orb db\n" ) ; }

    $orbname   = $ARGV[0];
    $dbname    = $ARGV[1];

    elog_init( $0, @ARGV );

    $pfsource  = $opt_p || "tadata/pf/st" ;

    print STDOUT "pfsource	$pfsource\n\n" if $opt_v ;
    print STDOUT "orb		$orbname\n\n" if $opt_v ;
    print STDOUT "db		$dbname\n\n" if $opt_v ;
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

    for (;;) {
        ( $pktid, $srcname, $time, $packet, $nbytes ) = orbreap( $orb );
	($result, $pkt) = unstuffPkt( $srcname, $time, $packet, $nbytes ); 
        if ( $result ne "Pkt_pf" ) {
            if( $opt_v ) {
                elog_notify( "Received a $result, skipping\n" );
            }
            next;
        }
    
#        printf "%s	%s\n", $srcname, strydtime($time);

        ($net, $sta, $chan, $loc, $suffix, $subcode) = $pkt->parts() ;
        ($type, $desc) = $pkt->PacketType() ;

        $pf = $pkt->pf ;

        if ( defined $pf ) {
            $ref = pfget($pf, "");
            my @stas = sort keys %{$ref->{dls}};
            foreach my $sta (@stas) {

                printf "%s	%s", $sta, strydtime($time) if $opt_v;

                my @pars = sort keys %{$ref->{dls}{$sta}};
                $inp     = $ref->{dls}{$sta}{"inp"};
                $ssident = $ref->{dls}{$sta}{"sn"};
                $idtag   = $ref->{dls}{$sta}{"pt"};
                $lat     = $ref->{dls}{$sta}{"lat"}; 
                $lon     = $ref->{dls}{$sta}{"lon"};
                $elev    = $ref->{dls}{$sta}{"elev"};
                $thr     =  $ref->{dls}{$sta}{"thr"};

                printf "	%s	%s	%s	%s	%s	%s	%s", $inp, $ssident,$idtag,$lat,$lon,$elev,$thr if $opt_v;
                printf "\n" if $opt_v;

                dbputv(@dbscratch, "dlsta",   $sta,
                                   "time",    $time,
                                   "endtime", $endtime,
                                   "inp",     $inp,
                                   "ssident", $ssident,
                                   "idtag",   $idtag,
                                   "thr",     $thr);

                if ($lat == "-" || $lon == "-" || $elev == "-") {
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
                    dbputv(@dbq330r,"endtime",($time-1)); 
                    dbadd(@dbq330);
                    next;
                }  elsif ($#row != -1){
                    elog_die ("PF data ip_change $sta	$time	$inp	$ssident	$endtime \n matches q330comms rows $#row @row\n");           
                }
                
#  Check to see if sn changes
                
                @row     = dbmatches(@dbscratch,@dbq330,"sn_change","dlsta","inp","endtime");
                if ($#row == 0) {
                    $dbq330r[3] = $row[0];
                    dbputv(@dbq330r,"endtime",($time-1)); 
                    dbadd(@dbq330);
                    next;
                }  elsif ($#row != -1){
                    elog_die ("PF data sn_change $sta	$time	$inp	$ssident	$endtime \n matches q330comms rows @row\n");           
                }
                
#  Check to see if sta changes
                
                @row     = dbmatches(@dbscratch,@dbq330,"sta_change","ssident","inp","endtime");
                if ($#row == 0) {
                    $dbq330r[3] = $row[0];
                    dbputv(@dbq330r,"endtime",($time-1)); 
                    dbadd(@dbq330);
                    next;
                }  elsif ($#row != -1){
                    elog_die ("PF data sta_change $sta	$time	$inp	$ssident	$endtime \n matches q330comms rows @row\n");           
                }

                dbadd(@dbq330);
            }
        }
        exit;
    }
exit ;
