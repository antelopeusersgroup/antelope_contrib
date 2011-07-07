# translate_stachan_names
#
# Kent Lindquist
# Geophysical Institute
# University of Alaska, Fairbanks
# January, 1999

use Datascope;
use Getopt::Std;

if( ! getopts( 'vn' ) || $#ARGV < 1 ) {
	$Program = `basename $0`;
	chop( $Program );
	die( "Usage: $Program [-v] [-n] translation_db dbname [dbname...]\n" );
} else {
	$transdbname = shift( @ARGV );
}

if( ! defined( pfget( "translate_stachan_names", "chantrans" ) ) ) {

	%chantrans = ();

} else {
	my( $hashref ) = pfget( "translate_stachan_names", "chantrans" );
	%chantrans = %$hashref;
}

@dbtrans = dbopen( $transdbname, "r+" );
@dbtrans = dblookup( @dbtrans, 0, "statrans", 0, 0 );
$nrecs = dbquery( @dbtrans, "dbRECORD_COUNT" );

for( $dbtrans[3]=0; $dbtrans[3]<$nrecs; $dbtrans[3]++ ) {
	
	( $oldsta, $newsta, $newchan, $defaultchan ) =
		dbgetv( @dbtrans, "oldsta", "newsta",
				  "newchan", "defaultchan" );

	$Newstas{$oldsta} = $newsta;
	$Newchans{$oldsta} = $newchan;
	$Defaultchans{$oldsta} = $defaultchan;
}

dbclose( @dbtrans );

$first = 1;

while( $dbname = shift( @ARGV ) ) {

	if( $opt_v ) { print "\nProcessing $dbname:\n\n"; }

	@db = dbopen( $dbname, "r+" );

	if( $first ) {

		@db = dblookup( @db, 0, "sitechan", "sta", 0 );
		@Sta_tables = dbquery( @db, "dbFIELD_TABLES" );
	
		@db = dblookup( @db, 0, "sitechan", "chan", 0 );
		@Chan_tables = dbquery( @db, "dbFIELD_TABLES" );
	 
		foreach $sta_table ( @Sta_tables ) {
 		
			push( @Stachan_tables,
			      grep( /^$sta_table$/, @Chan_tables ) );
			
		}

		$first = 0;
	}

	foreach $table ( @Stachan_tables ) {
	
		@db = dblookup( @db, 0, $table, "dbALL", 0 );
		$nrecs = dbquery( @db, "dbRECORD_COUNT" );
	
		if( $opt_v ) { print "$nrecs in $table\n"; }
	
		for( $db[3] = 0; $db[3] < $nrecs; $db[3]++ ) {
		
			( $sta, $chan ) = dbgetv( @db, "sta", "chan" );
	
			if( ! defined( $Newstas{$sta} ) ) {
				print( 
				"No translation for $sta in $dbname.$table\n" );
				next;
			}

			if( $Newchans{$sta} ne "-" ) {

				$newchan = $Newchans{$sta};

			} elsif( defined( $chantrans{$chan} ) ) {

				$newchan = $chantrans{$chan};

			} elsif( $chan ne "-" ) {

				$newchan = $chan;

			} else {
				
				$newchan = $Defaultchans{$sta};

			}

			$Translations{"$sta\_$chan"} =
				$Newstas{"$sta"} . "_" . $newchan;

			unless( $opt_n ) {

				dbputv( @db, "sta", $Newstas{$sta},
					     "chan", $newchan );
			}
		}

		$Done{$table}++;
	}

	foreach $table ( @Sta_tables ) {

		if( $Done{$table} ) {
			next;
		}

		@db = dblookup( @db, 0, $table, "dbALL", 0 );
		$nrecs = dbquery( @db, "dbRECORD_COUNT" );

		if( $opt_v ) { print "$nrecs in $table\n"; }

		for( $db[3] = 0; $db[3] < $nrecs; $db[3]++ ) {

			$sta = dbgetv( @db, "sta" );

			if( ! defined( $Newstas{$sta} ) ) {
				print( 
				"No translation for $sta in $dbname.$table\n" );
				next;
			}
		
			unless( $opt_n ) {

				dbputv( @db, "sta", $Newstas{$sta} );
			}
		}	

	}

	dbclose( @db );
}

foreach $key ( sort( keys( %Translations ) ) ) {

	if( $opt_n ) {

		print "Planning $key\t=> $Translations{$key}\n";

	} elsif( $opt_v ) {

		print "Translated $key\t=> $Translations{$key}\n";
	}
}
