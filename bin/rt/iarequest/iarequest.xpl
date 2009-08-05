
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
use Tk;
use Tk::ROText;
use Tk::LabEntry;
use elog_gui;

sub deliver_response {
	my( $msg ) = @_;

	if( ! defined( $Windows{"Main"} ) ) {

		elog_notify( $msg );

	} else {

		$Windows{"responses"}->insert( 'end', $msg );
		$Windows{"responses"}->yview( 'end' );
	}

	return;
}

sub iarequest_elog_gui {
	
	$nextreap->cancel();

	my( $result ) = ::elog_gui( @_ );

	schedule_nextreap();

	return $result;
}

sub schedule_nextreap {

	$nextreap = $Windows{"Main"}->after( 100, \&reap_response );
}

sub normalize_times {
	
	if( $end_string eq "" ) {
		
		$end = undef;

	} else {

		$end = str2epoch( $end_string );

		$end_string = epoch2str( $end, "%D %T %Z" );
	}

	$start = str2epoch( $start_string );

	$start_string = epoch2str( $start, "%D %T %Z" );

	return;
}

sub change_mode {

	if( $mode eq "point" ) {

		$Windows{"endentry"}->packForget();

		$end_string = "";

		normalize_times();

	} elsif( $mode eq "span" ) {

		$Windows{"endentry"}->pack( -side => "top", 
				      	    -expand => 1,
					    -fill => 'x',
					    -anchor => 'w' );

	} else {

		elog_complain( "Unexpected request to change mode to '$mode'\n" );
	}

	return;
}

sub init_menubar {
	my( $w ) = @_;
	
	my( $menubar, $menubutton, $filemenu );

	$menubar = $w->Frame( -relief => 'raised',
			      -borderwidth => 2 );

	$menubutton = $menubar->Menubutton(
				-text => 'File', 
				-pady => 0,
				-anchor => 'w',
				)->pack( -side => "left" );

	$filemenu = $menubutton->Menu( -tearoff => 0 );				

	$filemenu->add( "command", -label => "Quit", -command => \&exit );

	$menubutton->configure( -menu => $filemenu );

	return $menubar;
}

sub init_windows {
	
	$Windows{"Main"} = MainWindow->new();

	elog_gui_init( MW => $Windows{"Main"} );
	elog_callback( "::iarequest_elog_gui" );

	$Windows{"Main"}->resizable( 0, 0 );

	$Windows{"Main"}->bind( "<Control-KeyPress-c>", \&exit );
	$Windows{"Main"}->bind( "<Control-KeyPress-C>", \&exit );

	$Windows{"Main"}->title( "iarequest: ia2orb target '$target' via command-orb '$cmdorbname'" );

	$Windows{"menubar"} = init_menubar( $Windows{"Main"} );

	$Windows{"menubar"}->grid( -row => 0,
				   -column => 0,
				   -sticky => 'new',
				   );

	$Windows{"Main"}->gridColumnconfigure( 0, -weight => 1 );

	$Windows{"modeframe"} = $Windows{"Main"}->Frame( -relief => 'raised', 
				 		    -borderwidth => 2 );

	$Windows{"pointbutton"} = $Windows{"modeframe"}->Radiobutton( -variable => \$mode, 
							   -value => "point", 
							   -text => "Point",
							   -command => \&change_mode )
							 ->pack( -side => "left", -expand => 1 );

	$Windows{"spanbutton"} = $Windows{"modeframe"}->Radiobutton( -variable => \$mode, 
							  -value => "span",
							  -text => "Span",
							  -command => \&change_mode )
							->pack( -side => "left", -expand => 1 );

	$Windows{"modeframe"}->grid( -row => 1,
				     -column => 0,
				     -sticky => 'new',
				     );

	$Windows{"timeframe"} = $Windows{"Main"}->Frame( -relief => 'raised', 
				 		    	 -borderwidth => 2 );

	$Windows{"startentry"} = $Windows{"timeframe"}->LabEntry(-label => "Start:", 
								 -labelPack => [qw/-side left/],
								 -textvariable => \$start_string )
							      ->pack( -side => "top", 
							      	      -expand => 1, 
								      -fill => 'x',
								      -anchor => 'w');

	$Windows{"endentry"} = $Windows{"timeframe"}->LabEntry(-label => "  End:", 
								 -labelPack => [qw/-side left/],
								 -textvariable => \$end_string )
							      ->pack( -side => "top", 
							      	      -expand => 1,
								      -fill => 'x',
								      -anchor => 'w' );

	if( $mode eq "point" ) {
		
		$Windows{"endentry"}->packForget();
	}

	$Windows{"startentry"}->bind( "<KeyPress-Return>", \&normalize_times );
	$Windows{"endentry"}->bind( "<KeyPress-Return>", \&normalize_times );

	$Windows{"startentry"}->bind( "<KeyPress-Tab>", \&normalize_times );
	$Windows{"endentry"}->bind( "<KeyPress-Tab>", \&normalize_times );

	$Windows{"startentry"}->bind( "<Leave>", \&normalize_times );
	$Windows{"endentry"}->bind( "<Leave>", \&normalize_times );

	$Windows{"timeframe"}->grid( -row => 2,
				     -column => 0,
				     -sticky => 'new',
				     );

	my( $send_button ) = $Windows{"Main"}->Button( -text => "Send", 
						       -bg => "green",
						       -command => \&make_request );

	$send_button->grid( -row => 3,
			    -column => 0,
			    -sticky => 'new' );

	$Windows{"responses"} = $Windows{"Main"}->Scrolled( "ROText",
							-width => 80, -height => 25, 
							-wrap => 'word' );

	$Windows{"responses"}->grid( -row => 4,
				     -column => 0,
				     -sticky => 'new' );
	return;
}

sub make_request {

	if( $mode eq "span" ) {

		if( ! defined( $end ) ) {

			elog_notify( "You must set an end time when the 'span' option is selected; " .
				     "ignoring request\n" );

			return;
		}

		if( $end < $start ) {

			elog_notify( "Start time '" . epoch2str( $start, "%D %T %Z" ) . 
				     "' is later than End time '" .
				     epoch2str( $end, "%D %T %Z" ) .
				     "'; ignoring request\n" );

			return;
		}
	}

	$cmdpf = "cmd.pf";

	pfnew( $cmdpf );

	if( defined( $end ) ) {

		pfput( "command", "acqrange", $cmdpf );
		pfput( "start", $start, $cmdpf );
		pfput( "end", $end, $cmdpf );

	} else {

		pfput( "command", "acqnet", $cmdpf );
		pfput( "time", $start, $cmdpf );
	}

	$parts = Srcname->new(src_net=>$target, src_suffix=>'pf');

	$pkt = Packet->new(suffix=>'pf', parts=>$parts, pf=>$cmdpf) ;

	($srcname, $pkttime, $packet) = stuffPkt($pkt) ;

	$srcname .= "/dlcm";

	orbput($orbw, $srcname, now(), $packet, length($packet)) ;

	if( ! defined( $Windows{"Main"} ) ) {

		reap_response();
	}

	return;
}

sub reap_response {

	if( ! defined( $Windows{"Main"} ) ) {

		( $pktid, $srcname, $time, $packet, $nbytes ) = orbreap( $orbr );

	} else {

		( $pktid, $srcname, $time, $packet, $nbytes ) = orbreap_timeout( $orbr, 0 );

		if( ! defined( $pktid ) ) {

			schedule_nextreap();

			return;
		}
	}


	( $result, $pkt ) = unstuffPkt( $srcname, $time, $packet, $nbytes );

	( $type, $desc ) = $pkt->PacketType();

	$pf = $pkt->pf;

	if( defined( $pf ) ) {

		$ret_command = pfget( $pf, "command" );
		$ret_response = pfget( $pf, "response" );

		deliver_response( "Received response from ia2orb:\n\t$ret_response\n\n" );

	} else {

		elog_complain( "Received unexpected response (not a pf) on command orb\n" );
	}

	if( defined( $Windows{"Main"} ) ) {

		schedule_nextreap();
	}

	return;
}

elog_init( "iarequest", @ARGV );

if( ! &getopts( 'c:p:t:' ) || @ARGV > 2 ) { 

	elog_die( "Usage: iarequest [-p pfname] [-c cmdorbname] [-t target] [time [endtime]]\n" ) ; 
}

if( $opt_p ) { 

	$Pfname = $opt_p;

} else {

	$Pfname = "iarequest";
}

if( $opt_c ) {

	$cmdorbname = $opt_c;

} else {

	$cmdorbname = pfget( $Pfname, "cmdorbname" );
}

if( $opt_t ) {
	
	$target = $opt_t;

} else {

	$target = pfget( $Pfname, "target" );
}

$mode = pfget( $Pfname, "default_gui_mode" );

if( $mode ne "point" && $mode ne "span" ) {
	
	elog_die( "Parameter 'mode' in '$Pfname' must be 'point' or 'span'. Bye.\n" );
}

if( ( $orbw = orbopen( $cmdorbname, "w" ) ) < 0 ) {
	
	elog_die( "Failed to open command-orb '$cmdorbname' for writing. Bye.\n" );
}

if( ( $orbr = orbopen( $cmdorbname, "r" ) ) < 0 ) {
	
	elog_die( "Failed to open command-orb '$cmdorbname' for reading. Bye.\n" );
}

orbselect( $orbr, "/pf/dlcmr" );

if( @ARGV <= 0 ) {

	init_windows();

	$start_string = pfget( $Pfname, "default_request_start" );
	$end_string = "";

	normalize_times();

	schedule_nextreap();

	MainLoop;

} else {

	if( @ARGV == 2 ) {

		$end_string = pop( @ARGV );

	} else {

		$end_string = undef;
	}

	$start_string = pop( @ARGV );

	normalize_times();

	make_request();
}

exit 0;
