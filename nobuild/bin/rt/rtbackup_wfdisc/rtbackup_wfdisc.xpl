# Script to copy waveforms from realtime antelope machine to a large
# archival disk partition so that the realtime database can be cleaned up. 

# Taimi Mulder, Geological Survey of Canada, Sidney, B.C., Dec 2004
# Modified Feb. 2005 by Taimi Mulder and Kent Lindquist, Lindquist Consulting Inc.

use Datascope;
use Fcntl ':flock';

use Getopt::Std;

sub my_system {
	my( $command ) = @_;

	if( $opt_v || $opt_n ) {

		elog_notify( "Running command: $command\n" );
	}

	if( ! $opt_n ) {

		$rc = system( "$command" );

	} else {
		elog_notify( "\t(skipping)\n" );
	}

	return $rc;
}

sub check_lock { 
	my( $prog_name ) = @_ ;

	my( $lockfile ) = ".$prog_name" ;

	if( $opt_v ) {

		elog_notify( "Locking $lockfile\n" );
	}

	open ( LOCK, ">$lockfile" ) ;

	if ( flock( LOCK, LOCK_EX | LOCK_NB ) != 1 ) {

        	elog_die( "Can't lock file '$lockfile'. Quitting to avoid potential collision with other programs.\n" );
	}

    	print LOCK "$$\n" ;
}

sub release_lock {
	my( $prog_name ) = @_;

	my( $lockfile ) = ".$prog_name";

	if( $opt_v ) {

		elog_notify( "Unlocking $lockfile\n" );
	}

	flock( LOCK, LOCK_UN );
	
	close( LOCK );

	unlink( $lockfile );
}

sub oldest_realtime {

	my( $oldest_realtime_timestamp );

	if( $opt_v ) {
		
		elog_notify( "Checking real-time database..." );
	} 

	my( @jdates ) = `dbsubset $real_time_wf_dbname.wfdisc 'jdate != NULL && jdate != 1970001' | dbsort - jdate | dbselect - jdate | uniq`;
	chomp( @jdates );

	if( @jdates < 1 ) {

		elog_die( "Can't find any jdates in $real_time_wf_dbname.wfdisc. Bye!\n" );
	}

	my( $ndays_in_rt_db ) = scalar( @jdates ) - 1;

	my( $oldest_jdate ) = $jdates[0];

	my( %rtcrontab ) = %{pfget( "rtexec", "crontab" )};
	
	if( grep( /rtdbclean/, values( %rtcrontab ) ) ) {		
		
		my( $rtdbclean_max_days_db ) = pfget( "rtdbclean", "max_days_db" );

		my( $ndays_at_cleanup_risk ) = $ndays_in_rt_db - $rtdbclean_max_days_db;

		if( $ndays_at_cleanup_risk > 0 && $ndays_at_cleanup_risk <= $ndays_in_rt_db ) {

			$oldest_jdate = $jdates[$ndays_at_cleanup_risk-1];

			if( $opt_v ) {

				elog_notify( "rtdbclean appears to be enabled, with max_days_db set to " .
				             "$rtdbclean_max_days_db days. There are currently $ndays_in_rt_db complete days in the " .
					     "real-time database. Consider oldest day in real-time db to be " .
					     "$oldest_jdate instead of $jdates[0], adjusting for pending rtdbclean operation." );
			}
		}
	}

	$oldest_realtime_timestamp = str2epoch( "$oldest_jdate" );

	if( $opt_v ) {
		
		elog_notify( "Oldest data in real-time database (excluding data scheduled for cleanup) starts at " .
			      strtime( $oldest_realtime_timestamp ) );
	} 

	return $oldest_realtime_timestamp;
}

sub latest_archived {

	my( $latest_jdate, $latest_archive_wfdisc );

	my( $archive_wf_base_dbname ) = $archive_wf_dbname;

	# Assume everything after any percent-escapes is used 
	# to label individual archive volumes:

	$archive_wf_base_dbname =~ s/%.*//;

	my( @archive_wfdiscs ) = glob( "$archive_wf_base_dbname*\.wfdisc" );
	
	if( scalar( @archive_wfdiscs ) < 1 ) {
	
		elog_die( "No pre-existing archives; can't continue. Bye!\n" );
	}

	# Rely on glob's default sort in ascending order 
	# (as documented in File::Glob(3), on which the 
	# perl glob() built-in command is based):

	$latest_archive_wfdisc = pop( @archive_wfdiscs );

	if( $opt_v ) {
	
		elog_notify( "Examining $latest_archive_wfdisc for latest jdate..." );
	}

	chomp( $latest_jdate = `dbsort $latest_archive_wfdisc jdate | dbselect - jdate | tail -1` );

	if( $opt_v ) {
	
		elog_notify( "...latest archived jdate is $latest_jdate\n" );
	}

	return ( $latest_jdate, $latest_archive_wfdisc );
}

sub increment_jdate {
	my( $start ) = @_;

	return yearday( str2epoch( "$start" ) + 86400 );
}

elog_init( $0, @ARGV );

if( ! getopts( 'j:vn' ) || @ARGV != 0 ) {
	
	elog_die( "Usage: rtbackup_wfdisc [-j jdate] [-v] [-n]\n" );
}

if( $opt_v ) {

	elog_notify( "Starting rtbackup_wfdisc.\n" );
}

check_lock( "rtdbclean" );

$Pf = "rtbackup_wfdisc";

$real_time_wfdir = pfget( $Pf, "real_time_wfdir" );
$real_time_wf_dbname = pfget( $Pf, "real_time_wf_dbname" );

$wf_subdir_pattern = pfget( $Pf, "wf_subdir_pattern" );

$archive_wfdir = pfget( $Pf, "archive_wfdir" );
$archive_wf_dbname = pfget( $Pf, "archive_wf_dbname" );

$copy = pfget( $Pf, "copy" );
$cat = pfget( $Pf, "cat" );

$ignore_most_recent_ndays = pfget( $Pf, "ignore_most_recent_ndays" );
$minimum_database_overlap_ndays = pfget( $Pf, "minimum_database_overlap_ndays" );
$warning_email = pfget( $Pf, "warning_email" );

( $latest_archived_jdate, $latest_archive_wfdisc ) = latest_archived();


if( $opt_j ) {

	$active_jdate = $opt_j;

} else {

	$active_jdate = increment_jdate( $latest_archived_jdate );
}

$active_timestamp = str2epoch( "$active_jdate" );
$subdir = epoch2str( $active_timestamp, $wf_subdir_pattern );

$movedir = "$real_time_wfdir/$subdir";

if( $active_timestamp > ( str2epoch( yearday( str2epoch( "now" ) ) ) - $ignore_most_recent_ndays * 86400 ) ) {

	release_lock( "rtdbclean" );

	elog_die( "Ignoring data for jdate $active_jdate: more recent than $ignore_most_recent_ndays days. Bye!\n" );
}

if( ! -d $movedir ) {
	
	release_lock( "rtdbclean" );

	elog_die( "Intending to copy subdirectory '$movedir' but '$movedir' does not exist. Bye!\n" );
}

$archive_wfdir = epoch2str( $active_timestamp, "$archive_wfdir" );
$archive_wf_dbname = epoch2str( $active_timestamp, "$archive_wf_dbname" );

$tempdb = "/tmp/rtbackup_wfdisc_$<_$$";

my_system( "dbsubset $real_time_wf_dbname.wfdisc 'jdate == $active_jdate' | dbselect - > $tempdb.wfdisc" );
my_system( "dbset $tempdb.wfdisc dir '*' \"$archive_wfdir/$subdir\"" );

if( ! $opt_n ) {

	if( ! -e "$tempdb.wfdisc" || -z "$tempdb.wfdisc" ) {

		release_lock( "rtdbclean" );

		elog_die( "CRITICAL ERROR (RISK OF DATA-LOSS): Failed to extract rows for jdate " .
		          "$active_jdate from $real_time_wf_dbname.wfdisc. No waveforms copied. Bye!\n" );

	} else {

		chomp( $nrows = `wc -l $tempdb.wfdisc` );
		$nrows =~ s/^\s*//;
		$nrows = (split( /\s+/, $nrows ))[0];
		
		if( ! defined( $nrows ) || $nrows <= 0 ) {
			
			release_lock( "rtdbclean" );

			unlink( "$tempdb.wfdisc" );

			elog_die( "CRITICAL ERROR (RISK OF DATA-LOSS): No rows in extracted " .
				  "wfdisc for $active_jdate. Bye!\n" );
		}
	}
}

if( ( ! -d "$archive_wfdir" ) && ( ! $opt_n ) &&  ( mkdir( "$archive_wfdir" ) == 0 ) ) {
	
	release_lock( "rtdbclean" );

	unlink( "$tempdb.wfdisc" );

	elog_die( "Directory '$archive_wfdir' doesn't exist; Failed to create it (error: $!)\n" );
}

if( $opt_v ) {

	elog_notify( "Copy waveform directory $movedir into $archive_wfdir:\n" );
}

my_system( "$copy $movedir $archive_wfdir" );

if( $opt_v ) {

	elog_notify( "Concatenate wfdisc rows for jdate $active_jdate onto $archive_wf_dbname.wfdisc:\n" );
}

my_system( "$cat $tempdb.wfdisc >> $archive_wf_dbname.wfdisc" );

unlink( "$tempdb.wfdisc" );

$oldest_realtime_timestamp = oldest_realtime();

$actual_overlap_ndays = ( $active_timestamp - $oldest_realtime_timestamp ) / 86400;

if( $actual_overlap_ndays <= $minimum_database_overlap_ndays ) {

	if( $actual_overlap_ndays <= 0 ) {

		$warning_message = "No overlap between real-time and archive " .
		       	"databases (want $minimum_database_overlap_ndays days); " .
			"there's a " . -$actual_overlap_ndays . "-day gap. Is rtbackup_wfdisc failing?";

	} else {

		$warning_message = "Only $actual_overlap_ndays days overlapping between real-time and archive " .
		       	"databases (want $minimum_database_overlap_ndays days). Is rtbackup_wfdisc failing?";
	} 

	$warning_subject = "rtbackup_wfdisc warning: suspect risk of upcoming data loss";

	elog_complain( "$warning_message...sending email to $warning_email.\n" );

	if( $opt_v ) {

		$v = "-v";

	} else {

		$v = ""; 
	}	

	$message_tempfile = "/tmp/rtbackup_wfdisc_$<_$$.warning";
	open( M, ">$message_tempfile" );
	print M $warning_message;
	close M;

	my_system( "$cat $message_tempfile | rtmail $v -s '$warning_subject' $warning_email" );

	unlink( $message_tempfile );

} elsif( $opt_v ) {
	
	elog_notify( "Overlap between real-time and archive databases is $actual_overlap_ndays days " .
		     "(want $minimum_database_overlap_ndays days)...OK\n" );
}

release_lock( "rtdbclean" );

if( $opt_v ) {

	elog_notify( "Done with rtbackup_wfdisc.\n" );
}
