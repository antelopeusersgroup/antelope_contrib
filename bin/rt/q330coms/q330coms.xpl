#
#  Builds and updates q330comms table from tadata/pf/st orb packet
#
#    use diagnostics ;
    use strict ;
    use Datascope ;
    use orb ;
    use archive ;
    require "getopts.pl" ;

    our ( $opt_v, $opt_m, $opt_n, $opt_p );
    
    my ($pfsource,$orbname,$dbname,$orb,$pktid,$srcname,$net,$sta,$chan,$loc);
    my ($nbytes,$result,$pkt,$packet,$subcode,$desc,$type,$suffix,$pf,$ref);
    my ($when,$src,$pkttime,$start,$end);
    my ($inp,$ssident,$idtag,$lat,$lon,$elev,$thr,$endtime);
    my ($latnull,$latdb,$lonnull,$londb,$elevnull,$elevdb);
    my ($r,$row,$stadb,$inpdb,$ssold,$nsta,$nstau,$nsn);
    my ($prog_name,$mailtmp,$host,$Notes,$Problems,$subject);
    my (@db,@dbq330,@dbq330r,@dbscratch,@dbnull,@dbuniq);
    my (@row,@sources,@sta);

    if ( !  &Getopts('vm:n:p:') || @ARGV != 2 )
        { die ( "Usage: $0 [-v] [-m mail_to] [-n net] [-p pf_sourcename] orb db\n" ) ; }

#  Set up mail    
    
    $start = strydtime(now());
    elog_init ( $0, @ARGV );
        
    $prog_name = $0 ;
    $prog_name =~ s".*/"" ;

    $mailtmp = "/tmp/#$prog_name.$$";
    &savemail($mailtmp) if $opt_m ; 
    
    chop ($host = `uname -n` ) ;
    
    $Notes     = 0;
    $Problems  = 0;

#  Set up intial parameters

    $orbname   = $ARGV[0];
    $dbname    = $ARGV[1];

    $pfsource  = $opt_p || ".*/pf/st" ;
    
    print "pfsource	$pfsource \n";
    print "orb		$orbname \n";
    print "db		$dbname \n\n";
    

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
        printf "\n%s	%s \n\n", $srcname, strydtime($pkttime) if $opt_v;
        if (!defined $pktid) {
            next ;
        }
        if ( $nbytes == 0 ) {
            next ;
        }
        ($result, $pkt) = unstuffPkt ( $srcname, $pkttime, $pkt, $nbytes ) ;

        if ( $result ne "Pkt_pf" ) {
            if( $opt_v ) {
                print "Received a $result, skipping\n" ;
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
                
                next if ( $idtag == 0 );
                
                if ( $ssident =~ /.*[a-f].*/ ) {
                    print "$srcname	$sta	$ssident	" . strydtime($pkttime) . "	pktid  $pktid \n";
                }
                $ssident     = uc($ssident);

                printf "%s	%s	%s	%s	%s	%s	%s	%s \n", $sta, $inp, $ssident,$idtag,$lat,$lon,$elev,$thr if $opt_v;

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

                if (abs($lat) < 0.01 || abs($lon) < 0.01 ) {
                    ($lat,$lon,$elev) = dbgetv(@dbnull,"lat","lon","elev");
                }

                dbputv(@dbscratch, "lat", $lat, "lon", $lon, "elev", $elev);


#  Check if data are new

                @row = dbmatches(@dbscratch,@dbq330,"new_install","ssident","endtime");
                if ($#row == -1) {
                    @sta = dbmatches(@dbscratch,@dbq330,"dup_sta","dlsta","endtime");
                    foreach $r (@sta) {
                        $dbq330r[3] = $r;
                        ($ssold) = dbgetv(@dbq330r,"ssident");
                        dbputv(@dbq330r,"endtime",($pkttime-1));
                        $Notes++ ;
                        printf "\nNotification #$Notes\n" ;
                        printf "$sta datalogger $ssold replaced with $ssident \n";
                    }
                    dbadd(@dbq330);
                    next;
                }
                
#  Find most recent row
                $row = pop @row ;
                
#  Clean up any leftover rows w/o endtimes

                foreach $r (@row) {
                    $dbq330r[3] = $r;
                    dbputv(@dbq330r,"endtime",($pkttime-1));
                }
                
#  Use most recent row

                $dbq330r[3] = $row;
                ($stadb, $inpdb, $latdb, $londb, $elevdb) = dbgetv(@dbq330r,"dlsta","inp","lat","lon","elev");
                
#  Changed station
                
                if ($sta ne $stadb ) {
                    dbputv(@dbq330r,"endtime",($pkttime-1)); 
                    dbadd(@dbq330);
                    $Notes++ ;
                    printf "\nNotification #$Notes\n" ;
                    printf " $stadb datalogger $ssident moved to $sta \n";
                    next;
                }
                
#  Updated lat and lon
                
                ($latnull,$lonnull,$elevnull) = dbgetv(@dbnull,"lat","lon","elev");
                if (($latdb == $latnull)||($londb==$lonnull)||($elevdb==$elevnull)) {
                    dbputv(@dbq330r,"lat",$lat,"lon",$lon,"elev",$elev);
                }
                
#  Changed ip information
                
                if ($inp ne $inpdb ) {
                    dbputv(@dbq330r,"endtime",($pkttime-1)); 
                    dbadd(@dbq330);
                    printf "$stadb datalogger moved from $inpdb to $inp \n" if $opt_v;
                    next;
                }                
            }
        }
    }
    
    @dbq330r = dbsubset(@dbq330r,"endtime > _$start\_");
    $nsta    = dbquery(@dbq330r,"dbRECORD_COUNT");
    @dbuniq  = dbsort(@dbq330r,"dlsta","-u");
    $nstau   = dbquery(@dbuniq,"dbRECORD_COUNT");
    @dbuniq  = dbsort(@dbq330r,"ssident","-u");
    $nsn     = dbquery(@dbuniq,"dbRECORD_COUNT");
    
    if (($nsta != $nstau)||($nsn != $nstau)) {
        print "\nNumber of unique stations	$nstau\n";
        print "Number of stations	$nsta\n";
        print "Number of unique serial numbers	$nsn\n";
        $subject = "Problems and Notifications - $prog_name $host $orbname ";
        &sendmail($subject, $opt_m, $mailtmp) if $opt_m ; 
        print "\n$subject \n\n";
        exit (1);
    }

    if ($Notes) {
        $subject = "Notifications - $prog_name $host $orbname ";
        &sendmail($subject, $opt_m, $mailtmp) if $opt_m ; 
        print "\n$subject \n\n";
    }
    
    $end = strydtime(now());
    
    elog_notify("$prog_name	started $start	ended $end");
exit ;
