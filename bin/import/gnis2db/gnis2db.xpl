use Datascope;

#require "flush.pl";

if ( @ARGV < 2 || @ARGV > 3 ) {

	die ("USAGE: $0 file database [State]\n");

} else {

	$file	= $ARGV[0];
	$database = $ARGV[1];
	if( defined( $ARGV[2] ) ) {
		$fix_state = $ARGV[2];
	}
}

if (!-e $file) {
	print STDERR "File to convert, $file, does not exist!\n";
	exit(1);
}

@db = dbopen ( $database, "r+" ); 
print STDERR "opened database: $database \n";
@db_place = dblookup(@db, "", "places" , "" , "");
if( $db_place[1] < 0 ) {
	die( "Couldn't lookup $database.places. " .
	     "Schema wrong or not specified??\n" ); 
}

open (FILE, "< $file")  || die "Can't open $file for reading: $! \n";

while(<FILE>) {

	$_ =~ s/^[\'\"]//;
	#split each line using a quote-comma-quote as the delimiter
	if( defined( $fix_state ) ) {

		$state = $fix_state;

		($fname, $ftype, $county, $county_id, $state_id, $lat_dms,
	 	$lon_dms, $lat, $lon, $slat1, $slon1, $slat2, $slon2,
	 	$elev, $pop, $cell) = split(/[\'\"],[\'\"]/,$_);
	} else {
		($state, $fname, $ftype, $county, $county_id, $state_id,
		$lat_dms, $lon_dms, $lat, $lon, $slat1, $slon1, $slat2,
		$slon2, $elev, $pop, $cell) = split(/[\'\"],[\'\"]/,$_);
	 }

	if( defined( $elev ) && $elev ne "" ) { 

		# convert elev from feet to km
		$elev = $elev*0.0003048;

	} else {

		$elev = -999;
	}

	# GNIS files have a few repetitions. It's easier to 
	# let them live: garbage-in, garbage-out
	$db_place[3] = dbaddnull( @db_place );
	dbputv( @db_place, 	
		"lat", 		$lat, 
 		"lon", 		$lon, 
		"elev", 	$elev, 
		"place",	$fname, 
		"placetype", 	$ftype, 
		"county",	$county,
		"state",	$state );
}
dbclose( @db );
close( FILE );

