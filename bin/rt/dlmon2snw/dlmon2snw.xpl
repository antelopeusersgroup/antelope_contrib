#
#  Builds input file for SeisNetWatch from dlmon.pf 
#
#    use diagnostics ; 
    use strict ;
    use Datascope ;
    use orb;
    require "getopts.pl" ;

    our ( $opt_v, $opt_p );
    my ($snw_file,$pfsource,$orbname,$orb,$pktid,$srcname,$time,$net,$sta,$chan,$loc);
    my ($nbytes,$result,$pkt,$packet,$subcode,$desc,$type,$suffix,$pf,$ref,$value,$liststa);
    my ($listpar,$par,$nsta);
    my (%pf,%dls,%par);

    if ( !  &Getopts('vp:') || @ARGV != 1 )
        { die ( "Usage: $0 [-v] [-p pf_sourcename] orb \n" ) ; }

    $orbname   = $ARGV[0];

    elog_init( $0, @ARGV );

    $pfsource  = $opt_p || "tadata/pf/st" ;

    print STDOUT "pfsource	$pfsource\n\n" if $opt_v ;
    print STDOUT "orb		$orbname\n\n" if $opt_v ;
#
#  open input orb
#
    $orb = orbopen($orbname,"r");

    if( $orb < 0 ) {
        die( "Failed to open orb $orbname for reading\n" );
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

        ($net, $sta, $chan, $loc, $suffix, $subcode) = $pkt->parts() ;
        ($type, $desc) = $pkt->PacketType() ;
        print "$net\_$sta\_$chan\_$loc/$suffix/$subcode $type $desc\n" ;

        $pf = $pkt->pf ;

        if ( defined $pf ) {
            $ref = pfget($pf, "");
            my @stas = sort keys %{$ref->{dls}};
            foreach my $sta (@stas) {
                $nsta = $sta;
                $nsta =~ s/_/-/ ;
                printf "%s:", $nsta;
                my @pars = sort keys %{$ref->{dls}{$sta}};
                printf "%d:",($#pars + 1);
                foreach my $par (@pars) {
                    $value = $ref->{dls}{$sta}{$par};
                    printf "%s=%s;", $par, $value;
                }
                printf "\n";
            }
        }
        exit;
    }


exit ;

