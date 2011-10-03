#
#   Copyright (c) 2007 Lindquist Consulting, Inc.
#   All rights reserved. 
#                                                                     
#   Written by Dr. Kent Lindquist, Lindquist Consulting, Inc. 
#
#   This software is licensed under the New BSD license: 
#
#   Redistribution and use in source and binary forms,
#   with or without modification, are permitted provided
#   that the following conditions are met:
#   
#   * Redistributions of source code must retain the above
#   copyright notice, this list of conditions and the
#   following disclaimer.
#   
#   * Redistributions in binary form must reproduce the
#   above copyright notice, this list of conditions and
#   the following disclaimer in the documentation and/or
#   other materials provided with the distribution.
#   
#   * Neither the name of Lindquist Consulting, Inc. nor
#   the names of its contributors may be used to endorse
#   or promote products derived from this software without
#   specific prior written permission.
#
#   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
#   CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED
#   WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
#   WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
#   PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL
#   THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY
#   DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
#   CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
#   PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
#   USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
#   HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
#   IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
#   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
#   USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
#   POSSIBILITY OF SUCH DAMAGE.
#

use Getopt::Std;

use Datascope;
use Datascope::pf2xml;
use sysinfo;
use orb;

sub inform {
	my( $msg ) = @_;

	if( $opt_v ) {

		elog_notify( "$msg\n" );
	}

	return;
}

sub pfmorph {
	my( $pfname ) = @_;

	my( %dls ) = %{pfget( $pfname, "dls" )};

	foreach my $sta ( keys( %dls ) ) {

		if( defined( $dls{$sta}{opt} ) && $dls{$sta}{opt} ne "-" ) {

			$dls{$sta}{acok} = $dls{$sta}{opt} =~ /acok/ ? 1 : 0;
			$dls{$sta}{api}  = $dls{$sta}{opt} =~ /api/  ? 1 : 0;
			$dls{$sta}{isp1} = $dls{$sta}{opt} =~ /isp1/ ? 1 : 0;
			$dls{$sta}{isp2} = $dls{$sta}{opt} =~ /isp2/ ? 1 : 0;
			$dls{$sta}{ti}   = $dls{$sta}{opt} =~ /ti/   ? 1 : 0;
			
		} else {
			
			$dls{$sta}{acok} = "-";
			$dls{$sta}{api}  = "-";
			$dls{$sta}{isp1} = "-";
			$dls{$sta}{isp2} = "-";
			$dls{$sta}{ti}   = "-";
		}
	}

	pfput( "dls", \%dls, $pfname );

	return;
}

if ( ! getopts('a:m:v') || @ARGV != 2 ) { 
	my $pgm = $0 ; 
	$pgm =~ s".*/"" ;
	die ( "Usage: $pgm [-v] [-a after] [-m match] orb dir\n" ) ; 
}

elog_init( $pgm, @ARGV );

inform( "Starting orbdlstat2xml at " . strtime( now() ) . " UTC" );

$orbname = $ARGV[0];
$dir = $ARGV[1];

if( makedir( $dir ) < 0 ) {
	
	die( "Failed to make directory '$dir'. Bye.\n" );
}

$orb = orbopen( $orbname, "r&" );

if( $opt_a ) {

	$after_time = str2epoch( $opt_a );

	orbafter( $orb, $after_time );

	inform( "Rewound orb to " . strtime( $after_time ) . " UTC" );
}

if( $orb < 0 ) {
	
	die( "Failed to open $orbname\n" );
}

if( $opt_m ) {

	$match_regex = $opt_m;

} else {

	$match_regex = ".*/pf/(st|vtw)";
}

$nsources = orbselect( $orb, $match_regex );

inform( "Selected $nsources packet streams matching '$match_regex'" );

for( ;; ) {
	
	($pktid, $srcname, $time, $packet, $nbytes)  = orbreap( $orb );

	++$npkts;

	if( ! defined( $pktid ) ) {
		
		next;
	}

        ($result, $pkt) = unstuffPkt($srcname, $time, $packet, $nbytes) ;

	$header = "<?xml version=\"1.0\" encoding=\"iso-8859-1\"?>\n";

	if( $result eq "Pkt_st" || 
	    $result eq "Pkt_pf" ||
	    $result eq "Pkt_stash" ) {

		$pfstring = $pkt->string();

		if( defined( $pfstring ) && $pfstring ne "" ) {

			$file = $srcname;
			$file =~ s@/pf/(st|vtw)@@;
			$file .= "_stash.xml";
		
			$file = "$dir/$file";
		
			$pfname = "apf";

			pfnew( $pfname );

			pfcompile( $pfstring, $pfname );

			pfmorph( $pfname );

			$xmlstring = pf2xml( "-n", $pfname, "", $header );

			pffree( $pfname );

			$backbuffer_file = "$file+";

			$rc = open( F, ">$backbuffer_file" );

			if( $rc == 0 ) {

				die( "Failed to open '$backbuffer_file' " .
				     "for writing\n" );
			}
	
			print F $xmlstring;
	
			close( F );

			system( "mv $backbuffer_file $file" );

			inform( "Wrote packet type $result from $srcname, " .
				strtime( $time ) . " UTC to $file" );

		} else {

			$file = $srcname;
			$file =~ s@/pf/(st|vtw)@@;
			$file .= ".xml";
		
			$file = "$dir/$file";
		
			$pkt->pf();

			$pfname = "Packet::pf";

			pfmorph( $pfname );

			$xmlstring = pf2xml( "-n", $pfname, "", $header );

			$backbuffer_file = "$file+";

			$rc = open( F, ">$backbuffer_file" );
	
			if( $rc == 0 ) {

				die( "Failed to open '$backbuffer_file' " .
				     "for writing\n" );
			}
	
			print F $xmlstring;
	
			close( F );

			system( "mv $backbuffer_file $file" );

			inform( "Wrote packet type $result from $srcname, " .
				strtime( $time ) . " UTC to $file" );

		}
	}
}

inform( "Stopping orbdlstat2xml at " . strtime( now() ) . " UTC" );
