use Datascope;

if( $#ARGV != 0 ) { die("Usage: $0 dbname\n" );}

@db = dbopen($ARGV[0], "r+" );

@db = dblookup( @db, "", "origin", "", "" );

$nrecs = dbquery( @db, "dbRECORD_COUNT" );

$jdate_old = -1;
$count = 0;

for( $db[3]=0; $db[3]<$nrecs; $db[3]++ ) {
	
	($jdate) = dbgetv( @db, "jdate" );

	if( $jdate != $jdate_old ) {
		$jdate_old = $jdate;
		$count = 1;
	} else {
		++$count;
	}
	
	$orid = ($jdate - 1900000)*1000 + $count;
	print "$jdate $count $orid\n";

	dbputv( @db, "orid", $orid );
}
