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
 
sub inform {
	my( $msg ) = @_;
	
	if( $opt_v ) {
		
		elog_notify( "$msg\n" );
	}

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

$have_RRDs = 0;
$i = 0;

while( ( $d = $INC[$i++] ) && ! $have_RRDs ) {

	if( -e "$d/RRDs.pm" ) {

		$have_RRDs++;
		break;
	}
}

if( ! $have_RRDs ) { 
	
	die( "orb2rrd requires the perl RRD module for rrdtool " .
	     "(available from http://people.ee.ethz.ch/oetiker/webtools/rrdtool/)" .
	     ". Bye.\n" );
} else {

	eval( "use RRDs" );
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

$dir = pfget( $Pf, "dir" );
$line = pfget( $Pf, "vars[0]" );
( $varname, $pfsourcekey, $datasource, $myrra ) = split( /\s+/, $line );

$myrrd = concatpaths( $dir, $varname . ".rrd" );
$datasource = "DS:$varname:$datasource";

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

	$time = int( $time );
	$interval = 1;

	if( ! -e "$myrrd" ) {

		$start_time = $time - $interval;

		inform( "Creating rrdfile $myrrd\n" ); 

		system( "rrdtool create $myrrd -b $start_time -s $interval $datasource $myrra" );

		# RRDs::create( "$myrrd", 
		#		"-b $start_time", 
		#		"-s $interval",
		#		"$datasource $myrra" ); 
	}

	$msg = "Received a parameter-file '$srcname' at " . strtime( $time );

	if( $opt_V ) {
		$msg .= ":\n" . pf2string( $pkt->pf ) . "\n\n";
	} else {
		$msg .= "\n";
	}

	inform( $msg );

	$var =  pfget( $pkt->pf(), $pfsourcekey );

	print "SCAFFOLD $var\n";

	RRDs::update( $myrrd, "$time:$var" );
}
