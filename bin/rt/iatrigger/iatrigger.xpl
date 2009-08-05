
#
#   Copyright (c) 2009 Lindquist Consulting, Inc.
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

use Datascope;
use orb;
use Getopt::Std;

sub inform {
	my( $msg ) = @_;

	if( $opt_v ) {

		elog_notify( $msg );
	}

	return;
}

elog_init( "iatrigger", @ARGV );

if( ! &getopts( 'a:c:p:t:v' ) || @ARGV != 1 ) { 

	elog_die( "Usage: iatrigger [-v] [-a time_string] [-p pfname] [-c cmdorbname] [-t target] orbname\n" ) ; 

} else {

	$eqorbname = pop( @ARGV );
}

if( $opt_p ) { 

	$Pfname = $opt_p;

} else {

	$Pfname = "iatrigger";
}

if( $opt_c ) {

	$cmdorbname = $opt_c;

} else {

	$cmdorbname = pfget( $Pfname, "cmdorbname" );
}

# SCAFFOLD need state file

if( $opt_t ) {
	
	$target = $opt_t;

} else {

	$target = pfget( $Pfname, "target" );
}

$select = pfget( $Pfname, "select" );
$trigger_expression = pfget( $Pfname, "trigger_expression" );
$start_expr = pfget( $Pfname, "start_expr" );
$end_expr = pfget( $Pfname, "end_expr" );

if( ( $cmdorb = orbopen( $cmdorbname, "w" ) ) < 0 ) {
	
	elog_die( "Failed to open command-orb '$cmdorbname' for writing. Bye.\n" );

} else {

	inform( "Opened command-orb '$cmdorbname' for writing.\n" );
}

if( ( $eqorb = orbopen( $eqorbname, "r&" ) ) < 0 ) {
	
	elog_die( "Failed to open earthquake-orb '$eqorbname' for reading. Bye.\n" );

} else {

	inform( "Opened earthquake-orb '$eqorbname' for reading.\n" );
}

if( defined( $select ) && $select ne "" ) {

	orbselect( $eqorb, $select );
}

if( $opt_a ) {

	inform( "Positioning earthquake-orb to '$opt_a'.\n" );

	orbposition( $eqorb, $opt_a );
}

@dbtmp = dbtmp( "css3.0" );

while( 1 ) {
	
	inform( "Waiting for an earthquake...\n" );

	( $pktid, $srcname, $time, $packet, $nbytes ) = orbreap( $eqorb );

	( $result, $pkt ) = unstuffPkt( $srcname, $time, $packet, $nbytes );

	if( $result eq "Pkt_db" ) {

		inform( "Received a database packet '$srcname' at '" . strtime( $time ) . "'\n" );
		
		@db = $pkt->db;

	} elsif( $result eq "Pkt_pf" ) {

		inform( "Received a parameter-file packet '$srcname' at '" . strtime( $time ) . "'\n" );

		@db = dblookup( @dbtmp, "", "origin", "", "dbSCRATCH" );

		$raw = pfget( $pkt->pf, "origin" );

		inform( "$raw\n" );

		dbput( @db, $raw );

	} else {

		next;
	}

	if( ! dbex_eval( @db, $trigger_expression ) ) {
		
		inform( "No match to trigger expression\n" );
		inform( "Done processing this earthquake\n\n" );

		next;

	} else {

		inform( "Origin row matched trigger expression '$trigger_expression'\n" );
	}

	$reqstart = dbex_eval( @db, $start_expr );
	$reqend = dbex_eval( @db, $end_expr );

	$cmdpf = "cmd.pf";

	pfnew( $cmdpf );

	$reqcmd = "acqrange"; 

	pfput( "command", $reqcmd, $cmdpf );
	pfput( "start", $reqstart, $cmdpf );
	pfput( "end", $reqend, $cmdpf );

	inform( "Issuing request\n" . pf2string( $cmdpf ) );

	$reqparts = Srcname->new(src_net=>$target, src_suffix=>'pf');
	
	$reqpkt = Packet->new(suffix=>'pf', parts=>$reqparts, pf=>$cmdpf) ;

	($reqsrcname, $reqpkttime, $reqpacket) = stuffPkt($reqpkt) ;

	$reqsrcname .= "/dlcm";

	orbput($cmdorb, $reqsrcname, $reqtime, $reqpacket, length($reqpacket)) ;

	inform( "Done processing this earthquake\n\n" );
}

