#
#  Builds input file for SeisNetWatch from dlmon.pf 
#
#    use diagnostics ; 
    use strict ;
    use Datascope ;
    use orb;
    use Getopt::Std ;

    our ( $opt_v, $opt_p, $opt_i );
    my ($pfsource,$orbname,$orb,$pktid,$srcname,$pkttime,$net,$sta,$chan,$loc);
    my ($nbytes,$result,$pkt,$packet,$subcode,$desc,$type,$suffix,$pf,$ref,$value);
    my ($par,$nsta,$when,$src);
    my (@sources);
    my (%pf,%dls,%par);

    if ( !  getopts('vip:') || @ARGV != 1 )
        { die ( "Usage: $0 [-v] [-i] [-p pf_sourcename] orb \n" ) ; }

    $orbname   = $ARGV[0];

    elog_init ( $0, @ARGV );

    $pfsource  = $opt_p || ".*/pf/st" ;

    elog_notify ("pfsource	$pfsource\n\n") if $opt_v ;
    elog_notify ("orb		$orbname\n\n")  if $opt_v ;
#
#  open input orb
#
    $orb = orbopen($orbname,"r");

    if( $orb < 0 ) {
        die( "Failed to open orb $orbname for reading\n" );
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
        ($net, $sta, $chan, $loc, $suffix, $subcode) = $pkt->parts() ;
        ($type, $desc) = $pkt->PacketType() ;
        elog_notify ("$net\_$sta\_$chan\_$loc/$suffix/$subcode $type $desc\n")  if $opt_v;

        $pf = $pkt->pf ;

        if ( defined $pf ) {
            $ref = pfget($pf, "");
            my @stas = sort keys %{$ref->{dls}};
            foreach my $sta (@stas) {
                $nsta = $sta;
                $nsta =~ s/_/-/ ;
                printf "%s:", $nsta;
                my @pars = sort keys %{$ref->{dls}{$sta}};
                if ($opt_i) {
                    printf "    ";
                } else {
                    printf "%d:",($#pars + 1);
                }
                foreach my $par (@pars) {
                    $value = $ref->{dls}{$sta}{$par};
                    if ($opt_i) {
                        next unless ($par =~ /inp/ );
                    }
                    printf "%s=%s;", $par, $value;
                }
                printf "\n";
            }
        }
    }


exit ;

