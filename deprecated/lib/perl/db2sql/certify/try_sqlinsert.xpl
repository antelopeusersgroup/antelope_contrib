use Datascope;
use Datascope::db2sql;
use Datascope::dbmon;

$dbname = "/opt/antelope/data/db/demo/demo";
$flags = 0;
	
@db = dbopen( $dbname, "r" );

@sqlinsert = db2sqlinsert( @db, \&dbmon_compute_row_sync, $flags );

printf "Conversion results:\n" . join( "\n", @sqlinsert );

exit( 0 );
