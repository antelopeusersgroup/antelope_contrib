# tabulate_missed_stations
#
# Kent Lindquist
# Geophysical Institute
# University of Alaska, Fairbanks
# January, 1999

use Datascope;

if( $#ARGV < 1 ) {
	die( "Usage: $0 transdb dbname [dbname...]\n" );
}

@dbsta = dbopen( shift(@ARGV) , "r" );
@dbsta = dblookup( @dbsta, 0, "statrans", 0, 0 );

$n = dbquery( @dbsta, "dbRECORD_COUNT" );
for( $dbsta[3]=0;$dbsta[3]<$n;$dbsta[3]++) {
	$oldsta = dbgetv( @dbsta, "oldsta" );
	$Oldstations{$oldsta}++;
}

print "Oldstations are\n",join(" ",keys( %Oldstations ) ), "\n\n";

foreach $dbname (@ARGV) {
	%Missed = ();
	@db=dbopen("$dbname", "r" );
	foreach $table ( "arrival", "wfdisc" ) {

		@db = dblookup( @db, 0, "$table", 0, 0 );

		$nrecs = dbquery( @db, "dbRECORD_COUNT" );
		print "$nrecs rows in $dbname.$table\n";

		for( $db[3]=0; $db[3]<$nrecs; $db[3]++ ) {

			( $oldsta, $oldchan, $time ) =
			  dbgetv( @db, "sta", "chan", "time" );

			if( ! $Oldstations{$oldsta} ) {

				$Missed{$oldsta}++;
				$TotalMissed{$oldsta}++;

				$Oldchans->{$oldsta}->{$oldchan}++;

				if( ! defined( $firsttime{$oldsta} ) ||
				    $firsttime{$oldsta} > $time ) {
					$firsttime{$oldsta} = $time;
				}
			}
		}
	}
	dbclose(@db);

	if( scalar(keys(%Missed)) > 0 ) {
		print "Database $dbname has missed\n\t",
			join(" ",keys(%Missed)),"\n\n";
	} else {
		print "No Missed stations in $dbname\n\n";
	}
}

$number_missed = scalar( keys( %TotalMissed ) );

if( $number_missed > 0 ) {
	print "\n\n\nTotal of $number_missed stachans missed:\n";

	open( D, ">dbmissed" );
	print D "statrans1.1\n\n";
	close( D );
	
	@dbmissed = dbopen( "dbmissed", "r+" );
	@dbmissed = dblookup( @dbmissed, 0, "statrans", 0, 0 );
	foreach $oldsta ( sort( keys( %TotalMissed ) ) ) {
		print "$oldsta:\t",
		      strdate( $firsttime{$oldsta} ), "\t",
		      join( ", ", sort( keys( %{$Oldchans->{$oldsta}} ) ) ),
		      "\n";
		dbaddv( @dbmissed, "oldsta", $oldsta );
	}
}
