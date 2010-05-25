
use Datascope;
use Datascope::db2sql;
use Datascope::dbmon;

$dbname = "/opt/antelope/data/db/demo/demo";
$flags = 0;
	
@db = dbopen( $dbname, "r" );

@db = dblookup( @db, "", "origin", "", "" );

$db[3] = 0;

@sqlcommands = db2sqlinsert( @db, \&dbmon_compute_row_sync, $flags );

$sync = dbmon_compute_row_sync( @db );

push( @sqlcommands, db2sqldelete( @db, $sync, $flags ) );		

printf "Conversion results:\n" . join( "\n", @sqlcommands );

exit( 0 );
