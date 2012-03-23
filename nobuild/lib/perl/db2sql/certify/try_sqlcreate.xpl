use Datascope;
use Datascope::db2sql;

$dbname = "/opt/antelope/data/db/demo/demo";
$flags = 0;
	
@db = dbopen( $dbname, "r" );

@sqlcreate = dbschema2sqlcreate( @db, $flags );

printf "Conversion results:\n" . join( "\n", @sqlcreate );

exit( 0 );
