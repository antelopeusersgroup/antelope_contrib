use Datascope;

if( $#ARGV != 0 ) {
	die( "Usage: dbfix_calib dbname\n" );
} else {
	$dbname = $ARGV[0];
}

@db = dbopen( $dbname, "r+" );
if( $db[0] < 0 ) {
	die( "dbfix_calib: failed to open $dbname. Bye.\n" );
}

@db = dblookup( @db, "", "calibration", "", "" );
if( ! dbquery( @db, "dbTABLE_PRESENT" ) ) {
	die( "dbfix_calib: no calibration table in $dbname. Bye.\n" );
}

# prepare for future loop over multiple tables 
$table = "wfdisc"; 
@db = dblookup( @db, "", "$table", "", "" );
if( ! dbquery( @db, "dbTABLE_PRESENT" ) ) {
	die( "dbfix_calib: no $table table in $dbname. Bye.\n" );
}

system( "dbjoin $dbname.$table calibration | dbset -v - $table.calib 'calib == NULL' calibration.calib" );
