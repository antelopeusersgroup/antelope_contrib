
use Datascope;
use Getopt::Std;

elog_init( $0, @ARGV );

if( ! getopts( 'nl:p:s:' ) || $#ARGV < 1 ) {
	die( "Usage: $0 [-l seconds] [-n] " .
		"[-s catalog_subset_expression] dbcat dbname\n" );
} else {
	$lag_seconds = defined( $opt_l ) ? $opt_l : 0;
	defined( $opt_n ) && $nowait++;
	if( defined( $opt_s ) ) { $subset_expr = $opt_s; }
	if( defined( $opt_p ) ) { $add_phases = $opt_p; }
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
@dbassoc = dblookup( @dbout, "", "assoc", "", "" );
@dbarrival = dblookup( @dbout, "", "arrival", "", "" );

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
		
		if( defined( $add_phases ) ) {
			add_phases( @db, $orid );
		}

		return;
	}
}

sub add_phases {
	my( $orid ) = pop( @_ );
	my( @dbcatrow ) = @_;

	my( $cat_orid ) = dbgetv( @dbcatrow, "orid" );
	my( @dbcatphases ) = dbprocess( @dbcatrow, 
					"dbopen origin",
					"dbsubset orid == $cat_orid",
					"dbjoin assoc",
					"dbsort delta",
					"dbjoin arrival",
					"dbseparate arrival" );

	$nphases = dbquery( @dbcatphases, "dbRECORD_COUNT" );

	if( $nphases > 0 ) {	# Add real phases

		my( $nmax ) = $add_phases eq "all" ? $nphases :
			( $nphases >= $add_phases ? $add_phases : $nphases );

		for($dbcatphases[3]=0; $dbcatphases[3]<$nmax; $dbcatphases[3]++) {

			my( $arid ) = dbnextid( @dbout, "arid" );

			my( $record ) = dbget( @dbcatphases );

			$dbarrival[3] = dbaddnull( @dbarrival );
			foreach $field (dbquery(@dbcatphases,"dbTABLE_FIELDS")) {
				( $val ) = dbgetv( @dbcatphases, "$field" );
				dbputv( @dbarrival, "$field", $val );
			}
			dbputv( @dbarrival, "arid", $arid );

			my( $sta, $phase ) = dbgetv( @dbcatphases, 	
						     "sta", "iphase" );

			dbaddv( @dbassoc, 
				"arid", $arid,
				"orid", $orid,
				"sta", $sta,
				"phase", $phase );
		}

	} else {		# Add predicted phases

		my( @db ) = dbprocess( @dbout, 
				"dbopen origin",
				"dbsubset orid == $orid", 
				"dbjoin site",
				"dbsubset offdate == NULL",
		"dbsort distance(origin.lat,origin.lon,site.lat,site.lon)" );

		my( $nstas ) = dbquery( @db, "dbRECORD_COUNT" );
		my( $nmax ) = $add_phases eq "all" ? $nstas :
			( $nstas >= $add_phases ? $add_phases : $nstas );

		for( $db[3]=0; $db[3]<$nmax; $db[3]++ ) {

			my( $arid ) = dbnextid( @dbout, "arid" );

			my( $sta ) = dbgetv( @db, "sta" );

			my( $time ) = dbex_eval( @db, "parrival()" );

			dbaddv( @dbarrival,
				"arid", $arid,
				"time", $time,
				"sta", $sta,
				"iphase", "+P",
				"auth", "dbadd_unassoc" );

			dbaddv( @dbassoc, 
				"arid", $arid,
				"orid", $orid,
				"sta", $sta,
				"phase", "+P" );
		}
	}
}
