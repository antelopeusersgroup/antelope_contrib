
use Datascope;
require "getopts.pl";

elog_init( $0, @ARGV );

if( ! Getopts( 'l:pns:' ) || $#ARGV < 1 ) {
	die( "Usage: $0 [-l seconds] [-n] " .
		"[-s catalog_subset_expression] dbcat dbname\n" );
} else {
	$lag_seconds = defined( $opt_l ) ? $opt_l : 0;
	defined( $opt_n ) && $nowait++;
	if( defined( $opt_s ) ) { $subset_expr = $opt_s; }
	$dbname = pop( @ARGV );
	$dbcat = pop( @ARGV );
}

@dbout = dbopen( $dbname, "r+" );
if( $dbout[0] < 0 ) {
	die( "Failed to open $dbout\n" );
}
@dbout = dblookup( @dbout, "", "origin", "", "" );
@dbscratch = dblookup( @dbout, "", "origin", "", "dbSCRATCH" );
@dbevent = dblookup( @dbout, "", "event", "", "" );

@dbcat = dbopen( $dbcat, "r" );
if( $dbcat[0] < 0 ) {
	die( "Failed to open $dbcat\n" );
}

@dbcat = dblookup( @dbcat, "", "origin", "", "" );
$r0 = 0;
$nextr0 = $r0;

$tablefname = dbquery( @dbcat, "dbTABLE_FILENAME" );

$lastmtime = 0;
@cache = ();
for( ;; ) {

	if( $#cache >= 0 ) {

		elog_notify( "Checking $#cache cached records\n" );

		@oldcache = @cache;
		$cache = ();
		foreach $recnum ( @oldcache ) {
		
			$dbcat[3] = $recnum;
	
			push( @cache, add_if_appropriate( @dbcat ) );
		}
	}

	$mtime = (stat("$tablefname"))[9];

	if( $mtime != $lastmtime ) {

		$lastmtime = $mtime;
		
		$nrows = dbquery( @dbcat, "dbRECORD_COUNT" );
		$nnew = $nrows - $nextr0;
		elog_notify( "$nnew new rows in catalog database\n" );

		for( $dbcat[3]=$nextr0; $dbcat[3]<$nrows; $dbcat[3]++ ) {

			next if( defined( $subset_expr ) &&
				 ! dbex_eval( @dbcat, $subset_expr ) );

			push( @cache, add_if_appropriate( @dbcat ) );
		}

		$nextr0 = $nrows;
	}

	last if $nowait;

	sleep( 20 );
}

sub add_if_appropriate {
	my( @db ) = @_;

	my( $time, $auth ) = dbgetv( @db, "time", "auth" );

	my( $timenow ) = str2epoch( "now" );

	my( $record ) = dbget( @db );
	dbput( @dbscratch, $record );

	my( @records ) = dbmatches( @dbscratch, @dbout, "originhook",
			"lat", "lon", "depth", "time", "auth" );

	if( $#records >= 0 ) { 

		return; # Catalog hypocenter is already present

	} elsif( $timenow - $time < $lag_seconds ) {

		elog_notify( "Caching record $db[3]: Too early to add\n" );
		return $db[3]; # Cache record for later checking

	} else {
		my( $orid ) = dbnextid( @dbout, "orid" );
		my( $evid ) = dbnextid( @dbout, "evid" );

		elog_notify( "Adding new record as orid $orid, evid $evid\n" );

		dbputv( @dbscratch, "orid", $orid, "evid", $evid );
		dbadd( @dbout );
		dbaddv( @dbevent, "evid", $evid,
				  "prefor", $orid, 
				  "auth", $auth );

		return;
	}
}
