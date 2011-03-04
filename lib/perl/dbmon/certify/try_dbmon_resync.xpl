
use Datascope;
use Datascope::dbmon;

sub newrow { 
	my( @db ) = splice( @_, 0, 4 );
	my( $table, $irecord, $sync, $fhref ) = @_;

	$row = dbget( @db );
	
	printf $fhref "New Row $irecord in '$table' [sync '$sync']: $row\n";

	return;
}

sub delrow { 
	my( @db ) = splice( @_, 0, 4 );
	my( $table, $sync, $fhref ) = @_;

	printf $fhref "Deleted row from '$table' with sync '$sync'\n";

	return;
}

sub querysyncs {
	my( @db ) = splice( @_, 0, 4 );
	my( $table, $fhref ) = @_;

	my( $sync );
	my( @syncs ) = ();

	print $fhref "Resynchronizing table '$table'\n";

	if( dbquery( @db, dbTABLE_PRESENT ) ) {

		$db[3] = 0;

		$sync = dbmon_compute_row_sync( @db );

		push( @syncs, $sync );
	}

	return @syncs;
}

$Program_name = $0;
$Program_name =~ s".*/"";

elog_init( $Program_name, @ARGV );

$hookname = "ahook";

if( scalar( @ARGV ) != 2 ) {

	elog_die( 0, "Usage: $Program_name dbname statusfile\n" );

} else {
		
	$dbname = $ARGV[0];
	$statusfile = $ARGV[1];
}

@db = dbopen_database( $dbname, "r" );

$fh = \*STDOUT;

@tables = ( "affiliation", "arrival" );

dbmon_init( @db, $hookname, \&newrow, \&delrow, \&querysyncs, @tables );

dbmon_resync( $hookname, $fh );

dbmon_update( $hookname, $fh );

printf STDOUT "Adding new arrival table:\n";

system( "cp data/mod.new.demo.arrival results/dbmon_sync/demo.arrival" );

sleep( 1 );

dbmon_update( $hookname, $fh );

printf STDOUT "Shortening arrival table:\n";

system( "cp data/mod.shorter.demo.arrival results/dbmon_sync/demo.arrival" );

sleep( 1 );

dbmon_update( $hookname, $fh );

printf STDOUT "Lengthening arrival table:\n";

system( "cp data/mod.longer.demo.arrival results/dbmon_sync/demo.arrival" );

sleep( 1 );

dbmon_update( $hookname, $fh );

dbmon_status( $statusfile, $hookname );

elog_flush( 1, 0 );

dbclose( @db );

exit( 0 );
