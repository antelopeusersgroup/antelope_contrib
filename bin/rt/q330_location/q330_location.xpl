#
#  Checks q330 locations from tadata/pf/st orb packet
#
#    use diagnostics ;
    use strict ;
    use Datascope ;
    use orb ;
    use archive ;
    require "getopts.pl" ;

    our ( $opt_v, $opt_m, $opt_p, $opt_t );
    
    my ($pfsource,$orbname,$dbname,$orb,$pktid,$srcname,$net,$sta,$chan,$loc);
    my ($nbytes,$result,$pkt,$packet,$subcode,$desc,$type,$suffix,$pf,$ref);
    my ($when,$src,$pkttime,$start,$end);
    my ($inp,$ssident,$idtag,$lat,$lon,$elev,$thr,$endtime);
    my ($r,$got_pkt);
    my ($prog_name,$mailtmp,$host,$Problems,$subject);
    my ($stacode,$km,$delta,$latchk,$lonchk,$tolerance,$dlstack,$ssidentck);
    my ($commtype,$provider,$model);
    my (@db,@dbsite,@dbsitescr,@dbdlsite,@dbdlscr,@dbdlnull);
    my (@dbcomm,@dbcommscr);
    my (@row,@sources,@dlstas,@crow);

    if ( !  &Getopts('vm:p:t:') || @ARGV != 2 )
        { die ( "Usage: $0 [-v] [-m mail_to] [-p pf_sourcename] [-t max_offset_meters] orb db\n" ) ; }

#  Set up mail    
    
    $start = strydtime(now());
        
    $prog_name = $0 ;
    $prog_name =~ s".*/"" ;

    $mailtmp = "/tmp/#$prog_name.$$";
    &savemail($mailtmp) if $opt_m ; 
    
    chop ($host = `uname -n` ) ;
    
    $Problems  = 0;

#  Set up intial parameters

    $orbname   = $ARGV[0];
    $dbname    = $ARGV[1];

    $pfsource  = $opt_p || ".*/pf/st" ;
    $tolerance = ( $opt_t/1000 ) || 0.02 ;
    
    print "pfsource	$pfsource \n";
    print "orb		$orbname \n";
    print "db		$dbname \n\n";
    

#  Set up default model.  Will need to change this when more dataloggers get put into this system.

    $model = "q330";
    
#
#  open db
#
    @db        = dbopen($dbname,"r+");    
    @dbsite    = dblookup(@db,"","site","","");
    @dbsitescr = dblookup(@dbsite,"","","","dbSCRATCH");
    @dbdlsite  = dblookup(@db,"","dlsite","","");
    @dbdlscr   = dblookup(@dbdlsite,"","","","dbSCRATCH");
    @dbdlnull  = dblookup(@dbdlsite,"","","","dbNULL");
    @dbcomm    = dblookup(@db,"","comm","","");
    @dbcommscr = dblookup(@dbcomm,"","","","dbSCRATCH");
    
    $Problems = &table_check($Problems,@dbsite);
    $Problems = &table_check($Problems,@dbdlsite);
    $Problems = &table_check($Problems,@dbcomm);

#
#  open input orb
#
    $orb = orbopen($orbname,"r");

    if( $orb < 0 ) {
        die( "Failed to open orb '$orbname' for reading\n" );
    }

    orbselect( $orb, $pfsource);
    
    ($when, @sources) = orbsources ( $orb );
 
    print "Checking site table ------------------------------------------------\n";

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
    
        ($net, $sta, $chan, $loc, $suffix, $subcode) = $pkt->parts() ;
        ($type, $desc) = $pkt->PacketType() ;

        $pf = $pkt->pf ;

        if ( defined $pf ) {
            $ref = pfget($pf, "");
            my @dlstas = sort keys %{$ref->{dls}};
            foreach my $dlsta (@dlstas) {

                my @pars = sort keys %{$ref->{dls}{$dlsta}};
                $inp     = $ref->{dls}{$dlsta}{"inp"};
                $ssident = $ref->{dls}{$dlsta}{"sn"};
                $idtag   = $ref->{dls}{$dlsta}{"pt"};
                $lat     = $ref->{dls}{$dlsta}{"lat"}; 
                $lon     = $ref->{dls}{$dlsta}{"lon"};
                $elev    = $ref->{dls}{$dlsta}{"elev"};
                $thr     = $ref->{dls}{$dlsta}{"thr"};
                
                next if ( $idtag == 0 );
                next if ( $lat   eq "-" || abs($lat) < 0.01 );
                next if ( $lon   eq "-" || abs($lat) < 0.01 );
                
                $ssident     = uc($ssident);

                printf "%s	%s	%s	%s	%s	%s	%s	%s \n", $dlsta, $inp, $ssident,$idtag,$lat,$lon,$elev,$thr if $opt_v;

#  Check site station locations

                if (dbquery(@dbsite,"dbTABLE_PRESENT")) {
                    $sta = $dlsta ;
                    $sta =~ s/.*_// ;

                    dbputv(@dbsitescr, "sta", $sta);
                    @row = dbmatches(@dbsitescr,@dbsite,"$sta","sta");
                    if ($#row == -1) {
                        $Problems++ ;
                        printf "\nProblem #$Problems\n" ;
                        printf "$sta does not exist in site table\n", $km;
                    }
                    foreach $r (@row) {
                        $dbsite[3] = $r;
                        ($latchk,$lonchk) = dbgetv(@dbsite,"lat","lon");
                        $delta = dbex_eval(@dbsite,"distance($lat,$lon,lat,lon)");
                        $km = $delta * 111.;
                        if ($km > $tolerance) {
                            $Problems++ ;
                            printf "\nProblem #$Problems\n" ;
                            printf "$dlsta  %7.3f km offset\n", $km;
                            printf "	$dlsta gps	lat  %9.5f		lon  %9.5f 	\n", $lat, $lon;
                            printf "	site table	lat  %9.5f		lon  %9.5f 	row	$r\n", $latchk, $lonchk ;
                        }
                    }
                }
            }
        }
    }
    
    print "\n\nChecking dlsite table ------------------------------------------------\n";
    
    $endtime   = dbgetv(@dbdlnull,"endtime");

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
    
        ($net, $sta, $chan, $loc, $suffix, $subcode) = $pkt->parts() ;
        ($type, $desc) = $pkt->PacketType() ;

        $pf = $pkt->pf ;

        if ( defined $pf ) {
            $ref = pfget($pf, "");
            my @dlsta = sort keys %{$ref->{dls}};
            foreach my $dlsta (@dlsta) {
                $sta = $dlsta ;
                $sta =~ s/.*_// ;            
                ($commtype,$provider) = dbgetv(@dbdlnull,"commtype","provider");

                my @pars = sort keys %{$ref->{dls}{$dlsta}};
                $inp     = $ref->{dls}{$dlsta}{"inp"};
                $ssident = $ref->{dls}{$dlsta}{"sn"};
                $idtag   = $ref->{dls}{$dlsta}{"pt"};
                $lat     = $ref->{dls}{$dlsta}{"lat"}; 
                $lon     = $ref->{dls}{$dlsta}{"lon"};
                $elev    = $ref->{dls}{$dlsta}{"elev"};
                $thr     = $ref->{dls}{$dlsta}{"thr"};
                
                next if ( $idtag == 0 );
                next if ( $lat   eq "-" || abs($lat) < 0.01 );
                next if ( $lon   eq "-" || abs($lat) < 0.01 );
                                
                $ssident     = uc($ssident);
                
                printf "%s	%s	%s	%s	%s	%s	%s	%s \n", $dlsta, $inp, $ssident,$idtag,$lat,$lon,$elev,$thr if $opt_v;

#  Check dlsite station locations

                if (dbquery(@dbdlsite,"dbTABLE_PRESENT")) {
                    dbputv(@dbdlscr, "model",   $model,
                                     "ssident", $ssident,
                                     "dlname",  $dlsta, 
                                     "time",    $pkttime, 
                                     "endtime", $endtime);
                    @row = dbmatches(@dbdlscr,@dbdlsite,"$dlsta");
                    if ($#row == -1) {
                        $Problems++ ;
                        printf "\nProblem #$Problems\n" ;
                        printf "$dlsta does not exist in dlsite table, adding $dlsta to table\n", $km;
                        if (dbquery(@dbcomm,"dbTABLE_PRESENT")) {
                            dbputv(@dbcommscr,"sta",$sta,"time",$pkttime,"endtime",$endtime);
                            @crow = dbmatches(@dbcommscr,@dbcomm,"comm_$sta");
                            if ($#crow == 0) {
                                $dbcomm[3] = $crow[0];
                                ($commtype,$provider) = dbgetv(@dbcomm,"commtype","provider");
                            }
                        }
                        dbaddv(@dbdlsite,"model",$model,
                                         "ssident",$ssident,
                                         "time", $pkttime,
                                         "dlname",$dlsta,
                                         "idtag",$idtag,
                                         "lat",$lat,
                                         "lon",$lon,
                                         "elev",$elev,
                                         "commtype",$commtype,
                                         "provider",$provider);
                        next;
                    }
                    foreach $r (@row) {
                        $dbdlsite[3] = $r;
                        ($dlstack,$ssidentck,$latchk,$lonchk) = dbgetv(@dbdlsite,"dlname","ssident","lat","lon");
                        if ($dlstack ne $dlsta ) {
                            $Problems++ ;
                            printf "\nProblem #$Problems\n" ;
                            printf "dlsta  $dlsta in pf file has ssident $ssident\n";
                            printf "dlsta  $dlstack in dlsite table has ssident $ssidentck\n";
                            printf "Human intervention required";
                            last;
                        }
                        $delta = dbex_eval(@dbdlsite,"distance($lat,$lon,lat,lon)");
                        $km = $delta * 111.;
                        if ($km > $tolerance) {
                            $Problems++ ;
                            printf "\nProblem #$Problems\n" ;
                            printf "$dlsta  %7.3f km offset, updating row\n", $km;
                            printf "	$dlsta gps	lat  %9.5f		lon  %9.5f 	\n", $lat, $lon;
                            printf "	dlsite table	lat  %9.5f		lon  %9.5f 	row	$r\n", $latchk, $lonchk ;
                            dbputv(@dbdlsite,"lat",$lat,"lon",$lon);
                        }
                    }
                }
            }
        }
    }
    
    if ($Problems) {
        $subject = "Problems - $prog_name $host $orbname ";
        &sendmail($subject, $opt_m, $mailtmp) if $opt_m ; 
        print "\n$subject \n\n";
        exit (1);
    }

exit ;

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
