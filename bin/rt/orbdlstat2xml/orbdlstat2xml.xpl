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

if ( @ARGV != 2 ) { 
	my $pgm = $0 ; 
	$pgm =~ s".*/"" ;
	die ( "Usage: $pgm orb dir\n" ) ; 
}

use Datascope;
use orb;

$orbname = $ARGV[0];
$dir = $ARGV[1];

$orb = orbopen( $orbname, "r" );

orbafter( $orb, 0 );

if( $orb < 0 ) {
	
	die( "Failed to open $orbname\n" );
}

orbselect( $orb, ".*/pf/st" );

for( ;; ) {

	($pktid, $srcname, $time, $packet, $nbytes)  = orbreap( $orb );

	if( ! defined( $pktid ) ) {
		
		next;
	}

        ($result, $pkt) = unstuffPkt($srcname, $time, $packet, $nbytes) ;

	if( $result == Pkt_st ) {

		$file = $srcname;
		$file =~ s@/pf/st@@;
		$file .= ".xml";
		
		$file = concatpaths( $dir, $file );
		
		$pftmp = "/tmp/orbdlstat2xml_$<_$$\_tmp_$time.pf";
		$xmltmp = "$file+";

		open( F, ">$pftmp" );

		$string = $pkt->string();

		if( defined( $string ) ) {

			print F $pkt->string();

		} else {

			print F pf2string( $pkt->pf() );
		}

		close( F );

		system( "echo '<?xml version=\"1.0\" encoding=\"iso-8859-1\"?>' | pf2xml -n -h - $pftmp > $xmltmp" );

		system( "mv $xmltmp $file" );

		unlink( $pftmp );
	}
}
