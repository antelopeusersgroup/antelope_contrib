#
#   Original Author:
#       Frank Vernon <flvernon@ucsd.edu>
#
#   Update: 3/14
#       Juan Reyes <reyes@ucsd.edu>
#       - Add "-w" option to make
#           orb packets with the
#           information as waveforms
#

use Getopt::Std ;
use sysinfo ;
use strict ;
use Datascope ;
use archive;
use utilfunct ;
use orb ;

our ( $Pgm, $Host );
our ( $opt_d, $opt_0, $opt_w, $opt_V, $opt_V, $opt_n, $opt_p, $opt_v );
our ( $max_buffer_time, $Lastmem, $Pktbytes, $Pktcnt ) ;
our ( %pfout_buffer, %Pf, %Opt ) ;
our @opt_channels = qw/acok api ins1 ins2 ti/ ;

my ( $usage, $cmd, $subject, $Pf, $problems, $problem_check );
my ( $nbytes, $orb, $orbname, $packet, $pkt, $pktid, $reject );
my ( $select, $source, $srcname, $stime, $time, $type, $when );
my ( $return, $bit0, $bit1, $bit2, $bit6, $bit7, $value );
my ( @sources );

$Pgm = $0 ;
$Pgm =~ s".*/"" ;
elog_init($Pgm, @ARGV);
$cmd = "\n$0 @ARGV" ;

elog_notify($cmd) ;
$stime = strydtime(now());
chop ($Host = `uname -n` ) ;

if (  ! getopts('dwvV0n:p:') || @ARGV != 1 ) {
    $usage  =  "\n\n\nUsage: $0  \n [-d] [-w] [-v] [-V] [-0] \n" ;
    $usage .=  "    [-p pf] [-n npkts]   \n" ;
    $usage .=  "    status_orb \n\n"  ;

    elog_notify( $cmd ) ;
    elog_die( $usage ) ;
}

elog_notify ("\nstarting execution on   $Host   $stime");


$orbname = shift @ARGV;

$opt_v   = defined( $opt_V ) ? $opt_V : $opt_v ;

#$Pf = $opt_p || $Pgm ;
#%Pf = getparam($Pf);
%Pf = getparam( $opt_p || $Pgm );
$select = $Pf{select_packets} ;
$reject = $Pf{reject_packets} ;
$max_buffer_time = $Pf{buffer_time_window} || 600 ;

elog_notify( "select => ( $select )" ) ;
elog_notify( "reject => ( $reject )" ) ;

# Open the ORB and output info
$orb = open_orb() ;

# Reap packets and extract data from ORB
$return = main() ;

orbclose( $orb );
elog_notify( "Close orb connection." ) ;

exit $return ;


sub main {

    while ( 1 ) {

        elog_notify( "Go to packet: $pktid" ) if $pktid > 0 and $opt_V ;
        orbseek($orb, $pktid ) if $pktid > 0;

        ($pktid, $srcname, $time, $packet, $nbytes) = orbreap($orb) ;
        elog_notify( "Got packet: $pktid $srcname ".epoch2str($time,"%Y%j-%T") ) if $opt_V ;


        unless ( $pktid ) {
            &show_mem( " * after orbreap" ) ;
            elog_complain( "Problem on orbreap() pkid:$pktid srcname:$srcname" ) ;
            return 1 ;
        }

        next if $pktid < 0 ;

        &show_mem(" * after orbreap") if $opt_V ;

        $Pktcnt++ if $opt_n;
        #$Pktbytes += $nbytes ;

        &showPkt($pktid, $srcname, $time, $packet, $nbytes, 4) if $opt_V ;

        ($type, $pkt) = unstuffPkt($srcname, $time, $packet, $nbytes) ;

        if ( $type eq "Pkt_pf" ) {
            my ($net, $sta, $chan, $loc, $suffix, $subcode) = $pkt->parts() ; 

            process_packet ( $orb, $srcname, $time, $pkt) ;
            &show_mem(" * after process_packet") if $opt_V ;

        } else {
            warn ( "skipping packet $srcname   type  $type" ) if $opt_v;
        }

        if ( $opt_n and ($opt_n < $Pktcnt) ) {
            elog_notify( "Processed $Pktcnt packets" ) ;
            return 0;
        }

        elog_notify( "Last packet: $pktid" ) if $opt_V ;

    }


    $stime = strydtime( now() ) ;
    elog_notify ("completed at $stime\n\n") ;

    return 0 ;
}

sub process_packet {
    my ( $orb, $srcname, $time, $pkt ) = @_ ;
    my ( $interval, $pf, $sta, $pftarget );
    my ( @temp_stash, $chan, $ref, %pf, %pfout );
    my ( $chan, $loc, $net, $packet, $parts, $pktout ) ;
    my ( $pktid, $pkttime, $srcname_new, $subcode, $suffix ) ;
    my ( $first_time, $last_time, $e, $n, $s, $c, $l, $sf) ;
    my ( $temp_pkt, $parts ) ;
    my ( $newsrcname, $pkttime, $packet) ;
    # IMPORTANT:
    # %pfout is to be use for exporting vtw pf packets
    # for waveforms we want to use the global variable
    # %pfout_buffer that we defined at the init part.

    $pftarget = $srcname ;
    $pftarget =~ s/\/pf\/st/.pf/ ;
    elog_notify("pf packet: $pftarget") if $opt_v ;

    $pf = $pkt->pf ;
    $ref = pfget( $pf, "");
    %pf = %$ref ;
    $interval =  int($pf{itvl}) ;

    foreach $sta ( sort keys %{$pf{dls}} ) {

        elog_notify("\tgot: $sta $pf{dls}{$sta}{opt}") if $opt_V ;

        if ( $pf{dls}{$sta}{opt} =~ /-/ ) {
            for $chan ( @opt_channels ) {
                $pfout{$sta}{$chan} = '-' ;
            }
        } else {
            for $chan ( @opt_channels ) {
                $pfout{$sta}{$chan} = $pf{dls}{$sta}{opt} =~ /.*($chan).*/ ? 1 : 0 ;
                buffer_data( $sta, $chan, $pf{dls}{$sta}{opt},
                            $time, $interval) if $opt_w ;
            }
        }

    }

    #prettyprint(\%pfout) if $opt_V;

    if ( $opt_w ) {
        foreach $sta ( sort keys %pfout_buffer ) {

            ($n,$s,$c,$l,$sf) = split_srcname($sta);

            for $chan ( @opt_channels ) {
                $first_time = $pfout_buffer{$sta}{$chan}{first_time} ;
                $last_time  = $pfout_buffer{$sta}{$chan}{last_time} ;

                next unless $first_time and $last_time ;
                next if (( $last_time - $first_time ) < $max_buffer_time) ;

                elog_notify( "\t\tBuffer Full for ${sta}_${chan} with: "
                    . strtdelta($last_time - $first_time) ) if $opt_V ;

                # waveform packet
                my $calib = 1 ;
                my $calper = 1 ;
                my $segtype = "c"  ; # counts = dimensionless integer

                $e = PktChannel->new(net=>$n, sta=>$s, chan=>uc($chan),
                            time=>$first_time, samprate=>(1/$interval),
                            calib=>$calib, calper=>$calper, segtype=>$segtype,
                            data=>\@{$pfout_buffer{$sta}{$chan}{data}} ) ;

                push(@temp_stash, $e) ;

                # Cleanup buffer
                $pfout_buffer{$sta}{$chan}{data} = () ;
                $pfout_buffer{$sta}{$chan}{first_time} = 0 ;
                $pfout_buffer{$sta}{$chan}{last_time} = 0 ;
            }

            next unless scalar @temp_stash ;

            $parts = Srcname->new(src_net=>$n, src_sta=>$s, src_subcode=>'MSTV') ;

            $temp_pkt = Packet->new(suffix=>'MGENC', parts=>$parts,
                            channels=>[@temp_stash]) ;

            ($newsrcname, $pkttime, $packet) = stuffPkt($temp_pkt) ;

            if ( $opt_d ) {
                elog_notify( "***dry-run***: orbputx: -NULL- $newsrcname time: "
                        . epoch2str($pkttime,"%Y%j-%T") ) ;
            } else {
                $pkttime = now() ;
                $pktid = orbputx($orb, $newsrcname, $pkttime, $packet, length($packet)) ; 
                elog_notify( "\t\torbputx:$pktid $newsrcname time: "
                        . epoch2str($pkttime,"%Y%j-%T") ) if $opt_v ;
            }

            # Clean buffer
            @temp_stash = ();

        }

    } else {

        pfput("dls",\%pfout,$pftarget ) ;

        ($net, $sta, $chan, $loc, $suffix, $subcode) = $pkt->parts() ;
        $parts  = Srcname->new(src_net=>$net,src_sta =>$sta,src_chan=>$chan,src_loc=>$loc) ;
        $pktout = Packet->new(subcode=>'vtw', suffix=>'pf', parts=>$parts, pf=>$pftarget) ;
        ($srcname_new, $pkttime, $packet) = stuffPkt($pktout) ;

        $srcname_new = join_srcname($net, $sta, $chan, $loc, $suffix, "vtw") ;

        if ( $opt_d ) {
            elog_notify( "***dry-run***: orbputx: -NULL- $newsrcname time: "
                    . epoch2str($time,"%Y%j-%T") ) ;
        } else {
            $pktid = orbputx($orb, $srcname_new, $time, $packet, length($packet)) ;
            elog_notify( "\t\torbputx:$pktid $srcname_new time: "
                . epoch2str($time,"%Y%j-%T") ) if $opt_v ;
        }

    }

    %pf    = ();
    %pfout = () ;

}

sub buffer_data {
    my ( $sta, $chan, $string, $time, $interval ) = @_ ;
    my ( $log );

    my $new_data   = $string =~ /.*($chan).*/ ? 1 : 0 ;

    my $first_time = $pfout_buffer{$sta}{$chan}{first_time} || 0;
    my $last_time  = $pfout_buffer{$sta}{$chan}{last_time}  || 0;

    if ( $opt_V ) {
        $log = "\tprev: ${sta}_${chan}" ;
        $log .= ":".epoch2str($pfout_buffer{$sta}{$chan}{first_time},"%Y%j-%T") if $first_time;
        $log .= ":".epoch2str($pfout_buffer{$sta}{$chan}{last_time},"%Y%j-%T") if $last_time;
        $log .= ":[".join(',', @{$pfout_buffer{$sta}{$chan}{data}})."]" if $pfout_buffer{$sta}{$chan}{data};
        elog_notify($log) ;
    }

    if ( $last_time and $time gt ( $last_time+($interval*1.5) ) ) {
        # Clean buffer...
        $log  = "\n\n\t*** Gap in the data for ${sta}_${chan}\n" ;
        $log .= "\t*** Last data from $last_time ".epoch2str($last_time,"%Y%j-%T")."\n" ;
        $log .= "\t*** New data is $time ".epoch2str($time,"%Y%j-%T")."\n" ;
        $log .= "\t*** Diff: " . strtdelta($time - $last_time) . " \n" ;
        $log .= "\t*** Dump previous segment: [" ;
        $log .= join( ',', @{$pfout_buffer{$sta}{$chan}{data}} ) . "]\n" ;
        elog_complain( $log ) ;

        $pfout_buffer{$sta}{$chan}{first_time} = 0 ;
        $pfout_buffer{$sta}{$chan}{data} = () ;

    }

    $pfout_buffer{$sta}{$chan}{data} ||= () ;
    $pfout_buffer{$sta}{$chan}{first_time} ||= $time ;
    $pfout_buffer{$sta}{$chan}{last_time} = $time ;

    push( @{$pfout_buffer{$sta}{$chan}{data}}, $new_data ) ;


    if ( $opt_V ) {
        $log = "\tadd [$new_data] to ${sta}_${chan}" ;
        $log .= ":".epoch2str($pfout_buffer{$sta}{$chan}{first_time},"%Y%j-%T") ;
        $log .= ":".epoch2str($pfout_buffer{$sta}{$chan}{last_time},"%Y%j-%T") ;
        $log .= ":[".join(',', @{$pfout_buffer{$sta}{$chan}{data}})."]" ;
        elog_notify($log) ;
    }

}

sub show_mem {
    my %vminfo = sysmem() ;
    my $output ;
    $output .= " physmem:" . sprintf("%.1f Gb", $vminfo{physmem}/1024) ;
    $output .= " used:" . sprintf("%.1f Mb", $vminfo{used}/1024) ;
    elog_notify( "Memory:  $output" ) ;

}

sub open_orb {
    my ( $pktid ) ;

    $orb = orbopen($orbname,"r+");

    orbstashselect($orb, "NO_STASH") ;

    $pktid = orbposition($orb, "oldest") if $opt_0 ;
    elog_notify( "orbposition pktid: $pktid" ) if $opt_V ;

    orbselect($orb, $select) if $select ;
    orbreject($orb, $reject) if $reject ;


    ($when, @sources) = orbsources($orb) ;

    prettyprint(\@sources) if $opt_V ;

    foreach $source (@sources) {
        elog_notify(sprintf ("%-15s    %8d    %s    %s\n",
                $source->srcname, $source->npkts,
                strtdelta($when-$source->slatest_time) ) ) if $opt_v;
    }

    elog_notify( "open_orb: at packet => " . orbtell($orb) ) if $opt_V ;

    return $orb ;

}
