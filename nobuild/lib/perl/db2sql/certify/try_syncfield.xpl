
use Datascope;
use Datascope::db2sql;
use Datascope::dbmon;

$dbname = "/opt/antelope/data/db/demo/demo";
$flags = 0;
	
print STDOUT sprintf( "Check of sync-field name before modification: %s\n", db2sql_get_syncfield_name() );

db2sql_set_syncfield_name( "mytestsyncfield" );

print STDOUT sprintf( "Check of sync-field name after modification: %s\n", db2sql_get_syncfield_name() );

@db = dbopen( $dbname, "r" );

@db = dblookup( @db, "", "lastid", "", "" );

@sqlcommands = dbschema2sqlcreate( @db, $flags );

$db[3] = 0;

push( @sqlcommands, db2sqlinsert( @db, \&dbmon_compute_row_sync, $flags ) );

$sync = dbmon_compute_row_sync( @db );

push( @sqlcommands, db2sqldelete( @db, $sync, $flags ) );		

printf "Conversion results:\n" . join( "\n", @sqlcommands );

exit( 0 );
