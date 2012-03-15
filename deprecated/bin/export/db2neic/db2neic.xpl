# db2neic
#
# Submit database to NEIC in their format
#
# Kent Lindquist
# Geophysical Institute
# University of Alaska, Fairbanks
# February, 2000

use Getopt::Std ;
use Datascope ;
 
sub day_start {
	my( $myepoch ) = @_;

	return str2epoch( strdate( $myepoch ) );
}

$program = `basename $0`;
chomp( $program );

if ( ! getopts('sc:a:') || @ARGV != 1 ) {
	die ( "Usage: $program [-s] [-c mail_address] [-a mail_address] database\n" );
} else {
	$dbname = pop( @ARGV );
}

$subject = pfget( $program, "subject" );
$ml_min = pfget( $program, "ml_min" );

@db = dbopen( $dbname, "r" );

@db = dblookup( @db, "", "event", "", "" );
if( dbquery( @db, "dbTABLE_PRESENT" ) ) {
	@dbt = dblookup( @db, "", "origin", "", "" );
	@db = dbjoin( @db, @dbt );
	@db = dbsubset( @db, "orid == prefor" );
	if( dbquery( @db, "dbRECORD_COUNT" ) <= 0 ) {
		die( "$program: no hypocenters for events in $dbname\n" );
	} 
} else {
	@db = dblookup( @db, "", "origin", "", "" );
	if( dbquery( @db, "dbRECORD_COUNT" ) <= 0 ) {
		die( "$program: no hypocenters in $dbname\n" );
	} 
} 

$expr = "ml >= $ml_min";
@db = dbsubset( @db, $expr );

if( dbquery( @db, "dbRECORD_COUNT" ) <= 0 ) {
	print "$program: no qualifying hypocenters in $dbname\n";
	exit( 0 );
} 

@dbt = dblookup( @db, "", "assoc", "", "" );
@db = dbjoin( @db, @dbt );
if( dbquery( @db, "dbRECORD_COUNT" ) <= 0 ) {
	die( "$program: no arrival associations for hypocenters in $dbname\n" );
} 

@dbt = dblookup( @db, "", "arrival", "", "" );
@db = dbjoin( @db, @dbt );
if( dbquery( @db, "dbRECORD_COUNT" ) <= 0 ) {
	die( "$program: no arrivals for hypocenters in $dbname\n" );
} 

@db = dbsort( @db, "origin.time", "sta", "iphase" );

$nrecs = dbquery( @db, "dbRECORD_COUNT" );

$last_orid = -1;
$last_day_start = -999999999.999;

if( defined( $opt_c ) ) {
	$cc = ",$opt_c";
} else {
	$cc = "";
}

if( $opt_s ) {
	$recipient = pfget( $program, "neic_address" );
	open( P, "|mailx -s \"$subject\" $recipient $cc" );
	$FH = *P;
} elsif( defined( $opt_a ) && $opt_a ne "" ) {
	$recipient = $opt_a;
	open( P, "|mailx -s \"$subject\" $recipient $cc" );
	$FH = *P;
} else {
	$FH = *STDOUT;
}

print $FH "AEIC EARTHQUAKES\n";

for( $db[3] = 0; $db[3] < $nrecs; $db[3]++ ) {
	( $orid ) = dbgetv( @db, "orid" );
	( $origintime ) = dbgetv( @db, "origin.time" );
	$day_start = &day_start( $origintime );

	if( $day_start != $last_day_start ) {
		$last_day_start = $day_start;
		print $FH "\nSEISMO " . uc( epoch2str( $day_start, "%b %d, %Y" ) ) . "\n";
	}

	if( $orid != $last_orid ) {
		$last_orid = $orid;
		undef %Used;
		( $lat, $lon, $depth, $ml ) = dbgetv( @db, "lat", "lon", "depth", "ml" );
		print $FH "((HYPO ";
		print $FH epoch2str( $origintime, "%m/%d/%Y %H:%M:%S.%s " );
		print $FH sprintf( "LAT %.3f", abs( $lat ) );
		print $FH abs( $lat ) == $lat ? "N " : "S ";
		print $FH sprintf( "LONG %.3f", abs( $lon ) );
		print $FH abs( $lon ) == $lon ? "E " : "W ";
		print $FH sprintf( "DEPTH %.1f ", $depth );
		print $FH sprintf( "MAG %.1f ML.))\n", $ml );
	}

	( $sta, $chan, $iphase, $arrtime ) = dbgetv( @db,
		"sta", "chan", "iphase", "arrival.time" );
	next if( defined ( $Used{"$sta:$iphase"} ) );
	$Used{"$sta:$iphase"}++;
	print $FH sprintf( "%-5s %-5s %-5s ",
		uc( $sta ), uc( $chan ), uc( $iphase ) ) . 
		epoch2str( $arrtime, "%m/%d/%Y %H:%M:%S.%s\n" );
}

if( defined( $opt_s ) || defined( $opt_a ) ) {
	close( $FH );
	print "NEIC-format submission mailed to $recipient\n";
}

exit( 0 );
