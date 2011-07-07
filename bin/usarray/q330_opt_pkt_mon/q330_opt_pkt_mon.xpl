#
#   program needs:
#       open status orb
#
#  Liberally copied from pktmon
#
    use Getopt::Std ;
    use strict ;
    use Datascope ;
    use archive;
    use utilfunct ; 
    use orb ;
    
    our ( $Pgm, $Host );
    our ( $opt_0, $opt_V, $opt_d, $opt_n, $opt_p, $opt_v );
    our ( $Lastmem, $Pktbytes, $Pktcnt ) ; 
    our ( %Pf, %Opt ) ;
    
{    #  Main program

    my ( $usage, $cmd, $subject, $verbose, $debug, $Pf, $problems, $problem_check );
    my ( $nbytes, $orb, $orbname, $packet, $pkt, $pktid, $reject, $select, $source, $srcname, $stime, $time, $type, $when, $bit0, $bit1, $bit2, $bit6, $bit7, $value );
    my ( @sources );

    $Pgm = $0 ; 
    $Pgm =~ s".*/"" ;
    elog_init($Pgm, @ARGV);
    $cmd = "\n$0 @ARGV" ;
    
    if (  ! getopts('vV0dn:p:') || @ARGV != 1 ) { 
        $usage  =  "\n\n\nUsage: $0  \n	[-v] [-V] [-d] [-0] \n" ;
        $usage .=  "	[-p pf] [-n npkts]   \n" ;
        $usage .=  "	status_orb \n\n"  ; 
        
        elog_notify($cmd) ; 
        elog_die ( $usage ) ; 
    }
    
    elog_notify($cmd) ; 
    $stime = strydtime(now());
    chop ($Host = `uname -n` ) ;
    elog_notify ("\nstarting execution on	$Host	$stime");

    $Pf         = $opt_p || $Pgm ;

    $orbname   = shift @ARGV;
    
    $opt_v      = defined($opt_V) ? $opt_V : $opt_v ;    
    $verbose    = $opt_v;
    $debug      = $opt_V;

    %Pf = getparam($Pf, $verbose, $debug);
    $select = $Pf{select_packets} ; 
    $reject = $Pf{reject_packets} ; 

    elog_notify("select    $select" ) ;
    elog_notify("reject    $reject" ) ;

#
#  open orb
#
    $orb = orbopen($orbname,"r+");
    orbposition($orb, "oldest") if $opt_0 ; 

    orbselect($orb, $select) if $select ne "" ; 
    orbreject($orb, $reject) if $reject ne "" ;
    
    orbstashselect($orb, "NO_STASH") ;

    ($when, @sources) = orbsources($orb) ;
    
#    prettyprint(\@sources);
    
    foreach $source (@sources) {
        elog_notify(sprintf ("%-15s    %8d    %s    %s\n",
                $source->srcname, $source->npkts, 
                strtdelta($when-$source->slatest_time) ) ) ; # if $opt_V;
    }

    for (;;) {
        ($pktid, $srcname, $time, $packet, $nbytes) = orbreap($orb) ;
        next if ($pktid == -16) ; # skip stash packets
        # &show_mem(" * after orbreap") if $opt_d ;
        last if (! defined $pktid || $pktid < 0 || ($opt_n && ($opt_n < $Pktcnt))) ;
        $Pktcnt++ ; 
        $Pktbytes += $nbytes ; 
        &showPkt($pktid, $srcname, $time, $packet, $nbytes, 4) if $opt_d ;
        # &show_mem(" * after showPkt") if $opt_d ;
        ($type, $pkt) = unstuffPkt($srcname, $time, $packet, $nbytes) ;
        # &show_mem(" * after unstuffPkt") if $opt_d ;
        if ( $type eq "Pkt_pf" ) { 
            my ($net, $sta, $chan, $loc, $suffix, $subcode) = $pkt->parts() ; 
            # &show_mem(" * after pkt->parts") if $opt_d ;
            process_packet ( $orb, $srcname, $time, $pkt) ; 
            &show_mem(" * after process_packet") if $opt_d ;
        } else { 
            warn ( "skipping packet $srcname   type  $type\n" ) ; 
        }
        # &show_mem(sprintf("%8d %-20s %4d", $pktid, $srcname, $nbytes)) if $opt_d ;
    }


#
#  close up
#
print STDERR "processed $Pktcnt packets, with $Pktbytes bytes\n" ; 
    
    orbclose($orb);
    
#     @unique_vals = sort { $a <=> $b } (get_unique(@unique_vals));
#     
#     elog_notify("unique vals    @unique_vals");
#     
#     foreach $value (@unique_vals) {
# 	    $bit0     = int($value)  & 0x1 ;
# 	    $bit1     = (int($value) >> 1)  & 0x1 ;
# 	    $bit2     = (int($value) >> 2)  & 0x1 ;
# 	    $bit6     = (int($value) >> 6)  & 0x1 ;
# 	    $bit7     = (int($value) >> 7)  & 0x1 ;
# 	    elog_notify (sprintf("    %d    %d    %d    %d    %d    %d", $value, $bit0, $bit1, $bit2, $bit6, $bit7)) if $opt_v;
#     }
        
    $stime = strydtime(now());
    elog_notify ("completed successfully	$stime\n\n");
  
    exit(0);
}

sub process_packet { 
    my ( $orb, $srcname, $time, $pkt ) = @_ ; 
    my ( $acok, $api, $ins1, $ins2, $pf, $sta, $pftarget, $ti );        
    my ( %pf, %pfout );        
    my ( $chan, $loc, $net, $packet, $parts, $pfout, $pktout, $pkttime, $srcname_new, $subcode, $suffix ) ;

    $pftarget = $srcname ;
    $pftarget =~ s/\/pf\/st/.pf/ ;
    elog_notify("pftarget    $pftarget") ;

    $pf = $pkt->pf ;
    %pf = getparam( $pf, $opt_v, $opt_V );
    
    foreach $sta ( sort keys %{$pf{dls}} ) {

        if ( $pf{dls}{$sta}{opt} =~ /-/ ) {
            $acok = $api = $ins1 = $ins2 = $ti = "-" ;
        } else {
            $acok = $api = $ins1 = $ins2 = $ti = 0 ;
        }
        
        $acok = 1 if ( $pf{dls}{$sta}{opt} =~ /.*acok.*/) ;
        $api  = 1 if ( $pf{dls}{$sta}{opt} =~ /.*api.*/) ;
        $ins1 = 1 if ( $pf{dls}{$sta}{opt} =~ /.*isp1.*/) ;
        $ins2 = 1 if ( $pf{dls}{$sta}{opt} =~ /.*isp2.*/) ;
        $ti   = 1 if ( $pf{dls}{$sta}{opt} =~ /.*ti.*/)   ;
        
        $pfout{$sta}{acok} = $acok ;
        $pfout{$sta}{api}  = $api ;
        $pfout{$sta}{ins1} = $ins1 ;
        $pfout{$sta}{ins2} = $ins2 ;
        $pfout{$sta}{ti}   = $ti ;
    }
   
    prettyprint(\%pfout) if $opt_V;
    
    pfput("dls",\%pfout,$pftarget ) ;
    
    ($net, $sta, $chan, $loc, $suffix, $subcode) = $pkt->parts() ;
    $parts  = Srcname->new(src_net=>$net,src_sta =>$sta,src_chan=>$chan,src_loc=>$loc) ;
    $pktout = Packet->new(subcode=>'vtw', suffix=>'pf', parts=>$parts, pf=>$pftarget) ;
    ($srcname_new, $pkttime, $packet) = stuffPkt($pktout) ;

    $srcname_new = join_srcname($net, $sta, $chan, $loc, $suffix, "vtw") ;
    orbput($orb, $srcname_new, $time, $packet, length($packet)) ;

#     ($net, $sta, $chan, $loc, $suffix, $subcode) = $pkt->parts() ;
#     $parts  = Srcname->new(src_net=>$net,src_sta =>$sta,src_chan=>$chan,src_loc=>$loc) ;
#     $pktout = Packet->new(subcode=>'vtw', suffix=>'pf', parts=>$parts, pf=>$pftarget) ;
#     ($srcname_new, $pkttime, $packet) = stuffPkt($pktout) ;
#     orbput($orb, $srcname_new, $time, $packet, length($packet)) ;
    
    %pf    = ();
    %pfout = ();
    return;
    
}

sub getmem {
    my ($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,
        $atime,$mtime,$ctime,$blksize,$blocks)
            = stat("/proc/self/as");
    return $size ;
}
        
sub show_mem {   
    # my %psinfo  = psinfo($$) ;
    # my $mem = $psinfo{mem} ;
    my $mem = getmem() ;
    printf STDOUT "%-35s  %10d  %6d\n", 
	$_[0], $mem, $mem-$Lastmem if $mem > $Lastmem ;
    $Lastmem = $mem ;
}
