# Script to put origin info into a datascope origin table row.
# 2010 Jun 27 - T. Mulder

# TO DO:
# Put /pf packet into orb, this program only puts /db/origin packets into orb.

use Getopt::Std ;
use strict ; 
use Datascope ;
use orb ;
use IO::Socket ;

our ( $opt_pf, $opt_v, $opt_V ) ;
 
{    #  Main program

    my ( $Pf, $cmd, $debug, $hostname, $idname, $idnumber, $new, $orb, $orbclient, $orbname, $orbsize, $packet, $parts, $pgm, $pkt, $pkttime, $port, $problems, $sock, $srcname, $subject, $usage, $verbose ) ;
    my ( $lat, $lon, $depth, $time, $orid, $evid, $jdate, $nass, $ndef, $ndp, $grn, $srn, $etype, $review, $depdp, $dtype, $mb, $mbid, $ms, $msid, $ml, $mlid, $algorithm, $auth, $commid, $ldddate );
    my ( @db, @dbnetmag, @dborigin, @ids );
    my ( %pf );
#
#  Program setup
#
    $pgm = $0 ; 
    $pgm =~ s".*/"" ;
    elog_init($pgm, @ARGV);
    $cmd = "\n$0 @ARGV" ;
    
    if ( ! getopts('vVp:') || @ARGV != 2 ) { 
        $usage  =  "\n\n\nUsage: $0 [-v] [-pf pf] orb idserver"  ;         
        elog_notify( $cmd ) ; 
        elog_die   ( $usage ) ; 
    }

    $Pf         = $opt_pf || $pgm ;

    $opt_v      = defined($opt_V) ? $opt_V : $opt_v ;    
    $verbose    = $opt_v;
    $debug      = $opt_V;
    
    %pf = &getparam( $Pf, $verbose, $debug );
    
    $orbname    = $ARGV[0] ;
    $port	= $ARGV[1] ;
    $port       =~ s/.*:// ;
    $hostname	= "localhost";

    if ( $verbose ) { 
        elog_notify( "\nOrbname = $ARGV[0]\n" ) ;
        elog_notify( "Idserver(port) = $port\n" ) ;
        elog_notify(  "hostname = $hostname\n" ) ;
    }

    $evid = &get_id( "evid", $idnumber, $hostname, $port );
    $orid = &get_id( "orid", $idnumber, $hostname, $port );

    if ( $opt_v || $opt_V ) {
	elog_notify( "\nRecieved ids: evid=$evid, orid=$orid\n" );
    }

#
# Put parameter file data into orb as db packet.
#

    $orb = orbopen( $orbname, "r+" );

    @db       = dbopen   ( "/tmp/tmp_$$", "r+" ) ;
    @dborigin = dblookup ( @db, 0, "origin", 0, 0 ) ;
    @dbnetmag = dblookup ( @db, 0, "netmag", 0, 0 ) ;
    
    if ($pf{mag} !~ /mb/ || $pf{mag} !~ /Ms/) {
		$ml = $pf{mag} ;
                $mb = 0 ;
                $ms = 0 ;
    }

    if ($pf{mag} =~ /mb/) {
                $ml = 0 ;
                $mb = $pf{mag} ;
                $ms = 0 ;
    }

    if ($pf{mag} =~ /Ms/) {
                $ml = 0 ;
                $mb = 0 ;
                $ms = $pf{mag} ;
    }

	
    $new = dbaddv(@dborigin, "time"  , str2epoch($pf{time}) ,
                             "lat"   , $pf{lat}             ,
                             "lon"   , $pf{lon}             ,
                             "depth" , $pf{depth}           ,
                             "nass"  , 0                    ,
                             "ndef"  , 0                    ,
                             "review", $pf{review}          ,
                             "ml"    , $ml                  ,
                             "mb"    , $mb                  ,
                             "ms"    , $ms                  ,
			     "orid"  , $orid                ,
			     "evid"  , $evid                ,
			     "jdate" , -1                   ,
			     "ndp"   , -1                   ,
			     "grn"   , -1                   ,
			     "srn"   , -1                   ,
			     "etype" , "-"                  ,
			     "depdp" , -999.0000            ,
			     "dtype" , "-"                  ,
			     "mlid"  , -1                   ,
			     "mbid"  , -1                   ,
			     "msid"  , -1                   ,
			     "algorithm"  , "pr_origin"     ,
			     "auth"  , $auth                ,
			     "commid", -1                  );
                            
    $dborigin[3] = $new ;
    
    $parts = Srcname->new(src_net=>"", src_sta =>"") ;
    $pkt   = Packet->new(suffix=>'db', parts=>$parts, db=>\@dborigin) ;
    ( $srcname, $pkttime, $packet ) = stuffPkt($pkt) ;
    orbput( $orb, $srcname, $time, $packet, length($packet) ) ;

    elog_notify("/db/origin packet evid=$evid, orid=$orid added to orb $orbname");

    dbclose ( @db ) ;
    exit(0);
}

sub get_id {	# $idname, $idnumber, $hostname, $port 
	my ($idname, $idnumber, $hostname, $port) = @_ ;
        my ($sock);

#   Idserver default port is :2499
    $sock = new IO::Socket::INET ( PeerAddr => $hostname,
				   PeerPort => $port,
				   Proto    => 'tcp',
				   Reuse    => 1 );
    die "Could not connect to idserver $port: $!\n" unless $sock ;

    print $sock "$idname" ;
    $idnumber = <$sock>; 
    chomp( $idnumber );
#	print "IDnumber = $idnumber, $idname\n" ;


    close( $sock );

    if ( $opt_V ) {
        elog_notify( "\nSubroutine get_id: idname = $idname $idnumber\n" ) ;
        elog_notify( "Subroutine get_id: Connection to dbidserver $port closed\n" ) ;
    }

    return( $idnumber );
}

sub prettyprint {
	my $val = shift;
	my $prefix = "";
	if (@_) { $prefix = shift ; }

	if (ref($val) eq "HASH") {
		my @keys = sort ( keys  %$val );
		my %hash = %$val;
		foreach my $key (@keys) {
			my $newprefix = $prefix . "{". $key . "}" ;
			prettyprint ($hash{$key}, $newprefix) ;
		}
	} elsif (ref($val) eq "ARRAY") {
		my $i = 0;
		my @arr = @$val;
		foreach my $entry ( @$val ) {
			my $newprefix = $prefix . "[". $i . "]" ;
			prettyprint ($arr[$i], $newprefix) ;
			$i++;
		}
	} else {
        elog_notify("	$prefix  =  $val");
	}
}

sub getparam { # %pf = getparam($Pf, $verbose, $debug);
    my ($Pf, $verbose, $debug) = @_ ;
    my ( $ref );
    my ( %pf ) ;
    
    $ref = pfget( $Pf, "" ) ;
    %pf = %$ref ;

    elog_notify("\n Parameter file	$Pf\n") if $verbose;
    elog_notify("$Pf	ref($ref)") if $debug;
    
    &prettyprint(\%pf) if $verbose;
        
    elog_notify("\n ") if $verbose;
            
    return (%pf) ;
}

