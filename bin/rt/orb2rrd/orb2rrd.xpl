#
#   Copyright (c) 2006 Lindquist Consulting, Inc.
#   All rights reserved. 
#                                                                     
#   Written by Dr. Kent Lindquist, Lindquist Consulting, Inc. 
#
#   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY
#   KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
#   WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR 
#   PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS
#   OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR 
#   OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
#   OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE 
#   SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
#
#   This software may be used freely in any way as long as 
#   the copyright statement above is not removed. 

require "getopts.pl" ;
use Datascope;
use orb;
use RRDs;
 
sub inform {
	my( $msg ) = @_;
	
	if( $opt_v ) {
		
		elog_notify( "$msg\n" );
	}

	return;
}

sub archive_dlsvar {
	my( $net, $sta, $dls_var, $time, $val ) = @_;

	my( $varname ) = "$net\_$sta\_$dls_var";

	my( $myrrd ) = concatpaths( $Dir, $varname . ".rrd" );

	my( $datasource ) = "DS:$dls_var:$Dls_vars{$dls_var}{'dsparams'}";

	if( ! -e "$myrrd" ) {

		my( $start_time ) = $time - $Stepsize_sec;

		inform( "Creating rrdfile $myrrd\n" ); 

		RRDs::create( "$myrrd", 
				"-b", "$start_time", 
				"-s", "$Stepsize_sec",
				"$datasource", @{$Dls_vars{$dls_var}{'rras'}} ); 
	}

	RRDs::update( $myrrd, "$time:$var" );

	return;
}

$Pf = "orb2rrd.pf";
$match = ".*/pf/st";
$pktid = 0;
$time = -9999999999.999;

if ( ! &Getopts('s:f:p:m:vV') || @ARGV != 1 ) { 

    	die ( "Usage: orb2rrd [-vV] [-s statefile] [-p pffile] " .
	      "[-m match] [-f from] orb\n" ) ; 

} else {
	
	$orbname = $ARGV[0];
}

elog_init( $0, @ARGV );

if( $opt_V ) {
	
	$opt_v++;
}

if( $opt_p ) {
	
	$Pf = $opt_p;
}

if( $opt_m ) {
	
	$match = $opt_m;
}

$orb = orbopen( $orbname, "r&" );

if( $orb < 0 ) {

	die( "Failed to open orb '$orbname' for reading\n" );
}

orbselect( $orb, $match );

if( $opt_f && ( ! $opt_s || ! -e "$opt_s" ) ) {
	
	$pktid = orbposition( $orb, $opt_f );

	inform( "Positioned to packet $pktid" );

} elsif( $opt_f ) {

	elog_complain( "Ignoring -f in favor of existing state-file\n" );
}

if( $opt_s ) {

	$stop = 0;
	exhume( $opt_s, \$stop, 15 );
	orbresurrect( $orb, \$pktid, \$time  );
	orbseek( $orb, "$pktid" );
}

$Dir = pfget( $Pf, "dir" );
$Stepsize_sec = pfget( $Pf, "stepsize_sec" );
@lines = @{pfget( $Pf, "dls_vars" )};

foreach $line ( @lines ) {

	my( $dls_var, $dsparams, @myrras ) = split( /\s+/, $line );

	$Dls_vars{$dls_var}{'dsparams'} = $dsparams;
	$Dls_vars{$dls_var}{'rras'} = \@myrras;
}

for( ; $stop == 0 ; ) {

	($pktid, $srcname, $time, $packet, $nbytes) = orbreap( $orb );

	if( $opt_s ) {

		bury();
	}

	($result, $pkt) = unstuffPkt( $srcname, $time, $packet, $nbytes ); 

	if( $result ne "Pkt_pf" ) {

		inform( "Received a $result, skipping\n" );
		next;
	}

	$msg = "Received a parameter-file '$srcname' at " . strtime( $time );

	if( $opt_V ) {
		$msg .= ":\n" . pf2string( $pkt->pf ) . "\n\n";
	} else {
		$msg .= "\n";
	}

	inform( $msg );

	%mypktpf = %{pfget( $pkt->pf(), "dls" )};

	$time = int( $time );

	$dls_var = "br24"; #SCAFFOLD

	foreach $element ( keys %mypktpf ) {
	  foreach $dls_var ( keys %Dls_vars ) {

		( $net, $sta ) = split( '_', $element );

		$val =  $mypktpf{$element}{$dls_var};

		archive_dlsvar( $net, $sta, $dls_var, $time, $val );
	   }
	}
}
