# capitalize_station
#
# Kent Lindquist
# Geophysical Institute
# University of Alaska, Fairbanks
# January, 1999

use Datascope;

if( $#ARGV != 0 ) {
	die( "Usage: capitalize_station dbname\n" );
} else {
	$dbname = $ARGV[0];
}

@db = dbopen( $dbname, "r+" );
@db = dblookup( @db, 0, "site", "sta", 0 );
@tables = dbquery( @db, "dbFIELD_TABLES" );

foreach $table ( @tables ) {
	@db = dblookup( @db, 0, $table, "dbALL", 0 );
	$yes = dbquery( @db, "dbTABLE_PRESENT" );
	next unless $yes;
	
	$nrecs = dbquery( @db, "dbRECORD_COUNT" );
	
	for( $db[3] = 0; $db[3] < $nrecs; $db[3]++ ) {
		$sta = dbgetv( @db, "sta" );
		$sta =~ tr/[a-z]/[A-Z]/;
		dbputv( @db, "sta", $sta );
	}
}
