#
# Script to clean out database of archived continuous data
#
# K. Lindquist
# Geophysical Institute
# U. of Alaska
# August, 1998
#

use Datascope;
use Getopt::Std;

sub set_globals {
	
	@Clear_tables = ( "gap", "changed", "ratechange", "retransmit" );
	@Save_tables = ( "wfdisc" );

	$Max_time_in_database = -9999999999.999;
	$Min_time_in_database = 9999999999.999;
	$Sec_Per_Day = 86400;
	$opt_v = 0;
	$opt_o = "";

	$full_epoch_format = "%E (%j) %G  %T %Z %A\n";

	if( ! defined( $ENV{'PFPATH'} ) || length( $ENV{'PFPATH'} ) == 0 ) {

		$ENV{'PFPATH'} = "/tmp";

	} else {

		$ENV{'PFPATH'} .= ":/tmp";
	}
}

sub decrement_jdate {

	my( $yearday, $ndays ) = @_;

	return yearday( epoch( $yearday ) - $ndays * $Sec_Per_Day );
}

sub crunch_and_find_timespan {

	my( $dbname ) = @_;

	my( @db, $table, $nrecs );
	my( $max_time, $min_time );

	@db = dbopen( $dbname, "r+" );

	foreach $table ( @Clear_tables ) {

		@db = dblookup( @db, "", $table, "", "" );

		if( $db[1] < 0 ) {
			print STDERR "table $table not available\n";
			next;

		}
			
		$nrecs = dbquery( @db, "dbRECORD_COUNT" );

		if( $opt_v ) {
			print STDERR 
			   "truncating $table from $nrecs to zero records\n"; 
		}

		dbtruncate( @db, 0 );
	
	}

	foreach $table ( @Save_tables ) {

		@db = dblookup( @db, "", $table, "", "" );
	
		$nrecs = dbquery( @db, "dbRECORD_COUNT" );

		if( $nrecs > 0 ) {
	
			if( $opt_v ) { print STDERR "crunching $table\n"; }

			dbcrunch( @db );
	
			@db = dbsort( @db, "time" );

			$nrecs = dbquery( @db, "dbRECORD_COUNT" );

			$db[3] = 0;
			$min_time = dbgetv( @db, "time" );

			$db[3] = $nrecs - 1;
			$max_time = dbgetv( @db, "time" );

			$Max_time_in_database = $max_time > $Max_time_in_database ?
					 $max_time : $Max_time_in_database;

			$Min_time_in_database = $min_time < $Min_time_in_database ? 
					 $min_time : $Min_time_in_database;
		} else {

			if( $opt_v ) { print STDERR "table $dbname.$table not present\n"; }

		}
	}

	if( $opt_v ) { print STDERR "Min time in $Dbname: " .
			epoch2str( $Min_time_in_database, $full_epoch_format ); }
	if( $opt_v ) { print STDERR "Max time in $Dbname: " . 
			epoch2str( $Max_time_in_database, $full_epoch_format ); }

	dbclose( @db );
}

sub name_output_database {
	my( $dbname, $target_yearday ) = @_;

	substr( $target_yearday, 4, 0 ) = "-";

	my( $day_suffix ) = epoch2str( str2epoch( $target_yearday ), "_%Y_%m_%d" );

	return $dbname . $day_suffix;
}

sub split_day {

	my( $dbname, $target_yearday ) = @_;

	my( $dbout_name ) = name_output_database( $dbname, $target_yearday );

	if( -l $dbout_name ) {
		if( $opt_v ) { print STDERR "Removing link $dbout_name\n"; }
		unlink( $dbout_name );
	}

	if( -l "$dbout_name.wfdisc" ) {
		if( $opt_v ) { print STDERR "Removing link $dbout_name.wfdisc\n"; }
		unlink( "$dbout_name.wfdisc" );
	}

	if( -e $dbname ) {

		my( $cmd ) = "/bin/cp $dbname $dbout_name";	
		system( $cmd );
	}

	if( $opt_v ) { print STDERR "Splitting to $dbout_name\n"; }

	my( $v );
	if( $opt_v ) {
		$v = "-v";
	} else {
		$v = "";
	}

	my( $cmd ) = "dbsplit $v -d -f -s \'yearday( time ) == $target_yearday\'";
	$cmd .= " $dbname $dbout_name";

	system( "$cmd" );

	if( -e "$dbout_name" && ! -e "$dbout_name.wfdisc" ) {
		system( "/bin/rm -f $dbout_name" );
	}
}

sub write_dbsplit_pffile {
	
	$Pfname = "/tmp/dbsplit.pf";
	my( $table );

	if( $opt_v ) {
		print STDERR "Handling tables: ",
				join( " ", @Save_tables ), "\n"; 
	}

	if( -e $Pfname && ! -w $Pfname ) {
		die( "$Pfname exists but is not writable. Bye!\n" );
	}

	open( P, ">$Pfname" );

	print P "views   &Tbl{\n";

	foreach $table ( @Save_tables ) {

		print P "\t$table\n";

	}
	print P "}\n";

	close( P );
}

&set_globals();

if( ! getopts('vo:') || $#ARGV != 1 ) {

	die( "Usage: split_archive_database [-v] dbname Leave_N_days\n" );

} else {

	$Dbname = $ARGV[0];

	# Leave_N_days includes the last partial day present 
	#   (i.e. setting this to 2 leaves 1.5 days in the database)

	$Leave_N_days = $ARGV[1];
}

if( $opt_v ) {
	print STDERR "\nStarting at:", 
		epoch2str( str2epoch( "now" ), $full_epoch_format );
}

if( -e "$Dbname.MSGFILE" ) {

	if( $opt_v ) { print STDERR "Pausing orb2db from pid $$\n"; }

	my( $cmd ) = "orb2db_msg $Dbname pause";
	system( "$cmd" );
}

&crunch_and_find_timespan( $Dbname );

&write_dbsplit_pffile( );

$target_yearday = decrement_jdate( yearday( $Max_time_in_database ), $Leave_N_days );

while( $target_yearday >= yearday( $Min_time_in_database ) ) {
	
	split_day( $Dbname, $target_yearday  );

	$target_yearday = decrement_jdate( $target_yearday, 1 );
}

if( -e "$Dbname.MSGFILE" ) {

	if( $opt_v ) { print STDERR "Continuing orb2db from pid $$\n"; }

	my( $cmd ) = "orb2db_msg $Dbname continue";

	system( "$cmd" );
}

if( -e $Pfname ) {

	my( $cmd ) = "/bin/rm $Pfname";
	system( $cmd );

}	

if( $opt_v ) {
	print STDERR "\nDone at:", 
		epoch2str( str2epoch( "now" ), $full_epoch_format ), "\n";
}

